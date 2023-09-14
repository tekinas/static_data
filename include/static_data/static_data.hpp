#ifndef TEK_STATIC_DATA
#define TEK_STATIC_DATA

#include <bit>
#include <expected>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace tek {
struct recursive_base {};

template<typename>
constexpr bool is_recursive = false;
}// namespace tek

namespace tek::detail {
template<size_t size>
constexpr decltype(auto) applyIdxSeq(auto &&func) {
    return [&]<auto... I>(std::index_sequence<I...>) -> decltype(auto) {
        return func.template operator()<I...>();
    }(std::make_index_sequence<size>{});
}

template<size_t N>
constexpr void unroll(auto &&expr) {
    applyIdxSeq<N>([&]<size_t... I> { (static_cast<void>(expr.template operator()<I>()), ...); });
}

template<typename T>
concept Tuple = std::is_class_v<T> and requires {
    { std::tuple_size<T>::value } -> std::convertible_to<size_t>;
    requires applyIdxSeq<std::tuple_size_v<T>>([]<auto... I> {
        return ((requires {
                    typename std::tuple_element<I, T>::type;
                } and (requires(T &t) { t.template get<I>(); } or requires(T &t) { get<I>(t); })) and ...);
    });
};

template<size_t I, typename T>
    requires(Tuple<std::remove_cvref_t<T>> and I < std::tuple_size_v<std::remove_cvref_t<T>>)
constexpr decltype(auto) getElm(T &&tuple) {
    if constexpr (requires { tuple.template get<I>(); }) return tuple.template get<I>();
    else
        return get<I>(tuple);
}

template<typename T>
constexpr decltype(auto) applyTuple(auto &&func, T &&tuple)
    requires Tuple<std::remove_cvref_t<T>>
{
    return applyIdxSeq<std::tuple_size_v<std::remove_cvref_t<T>>>(
            [&]<auto... I> -> decltype(auto) { return func(getElm<I>(tuple)...); });
}

template<Tuple tuple>
constexpr decltype(auto) applyTypeSeq(auto &&func) {
    return applyIdxSeq<std::tuple_size_v<tuple>>(
            [&]<auto... I> -> decltype(auto) { return func.template operator()<std::tuple_element_t<I, tuple>...>(); });
}

namespace stdr = std::ranges;

template<typename T, template<typename...> class Ref>
constexpr bool is_specialization = false;

template<template<typename...> class Ref, typename... Args>
constexpr bool is_specialization<Ref<Args...>, Ref> = true;

template<typename T>
concept ToStaticDataMemFun = requires(T &value) { std::move(value).to_static_data(); };

template<typename T>
concept ToStaticDataFreeFun = requires(T &value) { to_static_data(std::move(value)); };

template<typename T>
concept CValue =
        std::is_trivially_copyable_v<T> and
        not(stdr::range<T> or Tuple<T> or is_specialization<T, std::variant> or is_specialization<T, std::optional> or
            is_specialization<T, std::expected> or std::is_pointer_v<T> or is_specialization<T, std::unique_ptr> or
            ToStaticDataMemFun<T> or ToStaticDataFreeFun<T>);

template<auto...>
constexpr int int_v{};

template<CValue T>
constexpr size_t writeToBytes(T value, std::byte *ptr) {
    using Bytes = std::array<std::byte, sizeof(T)>;
    unroll<sizeof(T)>([ptr, bytes = std::bit_cast<Bytes>(value)]<size_t i> {
        if constexpr (requires { int_v<std::bit_cast<Bytes>(std::bit_cast<T>(Bytes{}))[i]>; }) ptr[i] = bytes[i];
    });
    return sizeof value;
}

template<CValue Value>
constexpr auto readFromBytes(std::byte const *ptr) {
    std::array<std::byte, sizeof(Value)> bytes;
    for (auto &b : bytes) b = *ptr++;
    return std::bit_cast<Value>(bytes);
}

template<CValue Value, size_t N>
    requires(N != 0)
constexpr auto readFromBytes(std::byte const *ptr) {
    std::array<std::byte, sizeof(Value) * N> bytes;
    for (auto &b : bytes) b = *ptr++;
    return std::bit_cast<std::array<Value, N>>(bytes);
}

struct SpanOff {
    size_t base;
    size_t size;
};

template<typename T>
constexpr void writeAt(T &&value, std::byte *&ptr);

template<stdr::input_range Rng>
constexpr void writeNestedRange(Rng &&rng, std::byte *&ptr) {
    using value_t = stdr::range_value_t<stdr::range_reference_t<Rng>>;
    std::vector<SpanOff> offset_vec;
    std::vector<value_t> value_vec;
    for (auto &&r : rng) {
        auto const prev_size = value_vec.size();
        for (auto &&re : r) value_vec.push_back(std::move(re));
        offset_vec.push_back({prev_size, value_vec.size() - prev_size});
    }
    if constexpr (stdr::input_range<value_t>) writeNestedRange(value_vec, ptr);
    else
        writeAt(value_vec, ptr);
    writeAt(offset_vec, ptr);
}

template<typename T>
constexpr void writeAt(T &&value, std::byte *&ptr) {
    using DT = std::remove_cvref_t<T>;
    if constexpr (ToStaticDataMemFun<T>) writeAt(std::move(value).to_static_data(), ptr);
    else if constexpr (ToStaticDataFreeFun<T>)
        writeAt(to_static_data(std::move(value)), ptr);
    else if constexpr (stdr::input_range<T>) {
        if constexpr (using value_t = stdr::range_value_t<T>; stdr::input_range<value_t>) writeNestedRange(value, ptr);
        else if constexpr (stdr::sized_range<T> and CValue<value_t>) {
            ptr += writeToBytes<size_t>(stdr::size(value), ptr);
            for (auto &&e : value) ptr += writeToBytes(e, ptr);
        } else {
            auto start = ptr;
            size_t size = 0;
            for (ptr += sizeof(size_t); auto &&e : value) {
                if constexpr (CValue<value_t>) ptr += writeToBytes(e, ptr);
                else
                    writeAt(e, ptr);
                ++size;
            }
            writeToBytes(size, start);
        }
    } else if constexpr (Tuple<DT>)
        applyTuple([&](auto &&...e) { (writeAt(e, ptr), ...); }, value);
    else if constexpr (is_specialization<DT, std::variant>) {
        ptr += writeToBytes<size_t>(value.index(), ptr);
        std::visit([&](auto &&alt) { writeAt(alt, ptr); }, value);
    } else if constexpr (is_specialization<DT, std::optional>) {
        ptr += writeToBytes<bool>(value.has_value(), ptr);
        if (value) writeAt(*value, ptr);
    } else if constexpr (is_specialization<DT, std::expected>) {
        ptr += writeToBytes<bool>(value.has_value(), ptr);
        if (value) {
            if constexpr (not std::is_void_v<typename DT::value_type>) writeAt(*value, ptr);
        } else
            writeAt(value.error(), ptr);
    } else if constexpr (std::is_pointer_v<DT> or is_specialization<DT, std::unique_ptr>) {
        ptr += writeToBytes<bool>(value != nullptr, ptr);
        if (value) writeAt(*value, ptr);
    } else {
        static_assert(CValue<DT>);
        ptr += writeToBytes(value, ptr);
    }
}

template<typename T>
struct Type {};

template<typename T>
struct Array {};

template<size_t N, typename T>
struct NestedArray {};

template<typename... T>
struct Tup {};

template<typename... T>
struct Variant {};

template<typename T>
struct Optional {};

template<typename T, typename E>
struct Expected {};

template<typename T>
struct Pointer {};

template<typename T, typename WriteT>
struct CustomData {};

template<typename T, typename WriteT>
struct Recursive {};

template<typename>
constexpr auto writef();

template<stdr::input_range Rng, size_t N = 0>
constexpr auto writeNestedRngf() {
    using value_t = stdr::range_value_t<stdr::range_reference_t<Rng>>;
    if constexpr (stdr::input_range<value_t>) return writeNestedRngf<stdr::range_reference_t<Rng>, N + 1>();
    else
        return NestedArray<N, decltype(writef<value_t>())>{};
}

template<typename T>
constexpr auto writef() {
    using DT = std::remove_cvref_t<T>;
    if constexpr (ToStaticDataMemFun<T> or ToStaticDataFreeFun<T>) {
        using static_t = decltype([](auto &&value) {
            if constexpr (ToStaticDataMemFun<T>) return std::move(value).to_static_data();
            else
                return to_static_data(std::move(value));
        }(std::declval<T>()));
        if constexpr (std::is_base_of_v<recursive_base, DT> or is_recursive<DT>) return Recursive<DT, static_t>{};
        else
            return CustomData<DT, decltype(writef<static_t>())>{};
    } else if constexpr (stdr::input_range<T>) {
        if constexpr (using value_t = stdr::range_value_t<T>; stdr::input_range<value_t>) return writeNestedRngf<DT>();
        else
            return Array<decltype(writef<value_t>())>{};
    } else if constexpr (Tuple<DT>)
        return applyTypeSeq<DT>([]<typename... TE> { return Tup<decltype(writef<TE>())...>{}; });
    else if constexpr (is_specialization<DT, std::variant>)
        return applyIdxSeq<std::variant_size_v<DT>>(
                []<size_t... i> { return Variant<decltype(writef<std::variant_alternative_t<i, DT>>())...>{}; });
    else if constexpr (is_specialization<DT, std::optional>)
        return Optional<decltype(writef<typename DT::value_type>())>{};
    else if constexpr (is_specialization<DT, std::expected>)
        return Expected<decltype(writef<typename DT::value_type>()), decltype(writef<typename DT::error_type>())>{};
    else if constexpr (std::is_pointer_v<DT>)
        return Pointer<decltype(writef<std::remove_pointer_t<DT>>())>{};
    else if constexpr (is_specialization<DT, std::unique_ptr>)
        return Pointer<decltype(writef<typename DT::element_type>())>{};
    else {
        static_assert(CValue<DT>);
        return Type<DT>{};
    }
}

template<typename T>
auto readf(T);

template<typename T, typename WT>
class StaticPtr {
public:
    constexpr auto &operator*() const {
        using static_t = decltype([](auto static_v) {
            if constexpr (requires { T::from_static_data(static_v); }) return T::from_static_data(static_v);
            else if constexpr (requires { from_static_data(std::type_identity<T>{}, static_v); })
                return from_static_data(std::type_identity<T>{}, static_v);
            else
                return static_v;
        }(readf(writef<WT>())));
        return *static_cast<static_t const *>(data);
    }

    constexpr auto const *operator->() const { return &**this; }

private:
    template<auto &>
    friend class Reader;

    consteval explicit StaticPtr(void const *data) : data{data} {}

    void const *data;
};

template<typename T>
auto readf(Type<T>) -> T;

template<typename T>
auto readf(Array<T>) -> std::span<decltype(readf(T{})) const>;

template<size_t N, typename T>
auto readf(NestedArray<N, T>) {
    if constexpr (N) return std::span<decltype(readf(NestedArray<N - 1, T>{})) const>{};
    else
        return std::span<std::span<decltype(readf(T{})) const> const>{};
}

template<typename... T>
auto readf(Tup<T...>) -> std::tuple<decltype(readf(T{}))...>;

template<typename... T>
auto readf(Variant<T...>) -> std::variant<decltype(readf(T{}))...>;

template<typename T>
auto readf(Optional<T>) -> std::optional<decltype(readf(T{}))>;

template<typename T, typename E>
auto readf(Expected<T, E>) -> std::expected<decltype(readf(T{})), decltype(readf(E{}))>;

template<typename E>
auto readf(Expected<void, E>) -> std::expected<void, decltype(readf(E{}))>;

template<typename T>
auto readf(Pointer<T>) -> decltype(readf(T{})) const *;

template<typename T, typename WT>
auto readf(CustomData<T, WT>) -> decltype([](auto read_v) {
    if constexpr (requires { T::from_static_data(read_v); }) return T::from_static_data(read_v);
    else if constexpr (requires { from_static_data(std::type_identity<T>{}, read_v); })
        return from_static_data(std::type_identity<T>{}, read_v);
    else
        return read_v;
}(readf(WT{})));

template<typename T, typename WT>
auto readf(Recursive<T, WT>) -> StaticPtr<T, WT>;

template<size_t pos, typename value_type>
struct Result {
    static constexpr auto next_pos = pos;
    value_type value;
};

template<size_t pos, typename T>
constexpr auto result(T &&value) {
    return Result<pos, std::remove_cvref_t<T>>{value};
}

template<auto &buffer>
class Reader {
public:
    template<typename T>
    using read_t = decltype(readf(T{}));

    template<size_t pos>
    static constexpr auto readAt(auto) = delete;

    template<size_t pos, typename T>
    static constexpr auto readAt(Type<T>) {
        return result<pos + sizeof(T)>(readFromBytes<T>(ptr(pos)));
    }

    template<size_t pos, typename T>
    static constexpr auto readAt(Array<Type<T>>) {
        constexpr auto data_pos = pos + sizeof(size_t);
        if constexpr (constexpr auto count = readFromBytes<size_t>(ptr(pos))) {
            static constexpr auto array = readFromBytes<T, count>(ptr(data_pos));
            return result<data_pos + sizeof array>(std::span<T const>{array});
        } else
            return result<data_pos>(read_t<Array<Type<T>>>{});
    }

    template<size_t pos, typename T>
    static constexpr auto readAt(Array<T>) {
        constexpr auto data_pos = pos + sizeof(size_t);
        if constexpr (constexpr auto count = readFromBytes<size_t>(ptr(pos))) {
            static constexpr auto res = readNTArray<data_pos, count, T>();
            return result<res.next_pos>(std::span{&res.value[0].value, count});
        } else
            return result<data_pos>(read_t<Array<T>>{});
    }

    template<size_t pos, size_t N, typename T>
    static constexpr auto readAt(NestedArray<N, Type<T>>) {
        constexpr auto data_pos = pos + sizeof(size_t);
        if constexpr (constexpr auto count = readFromBytes<size_t>(ptr(pos))) {
            static constexpr auto array = readFromBytes<T, count>(ptr(data_pos));
            return readRngs<data_pos + sizeof array, N>([](size_t i) { return &array[i]; });
        } else
            return readRngs<data_pos, N>([](size_t) -> read_t<Type<T>> const * { return nullptr; });
    }

    template<size_t pos, size_t N, typename T>
    static constexpr auto readAt(NestedArray<N, T>) {
        constexpr auto data_pos = pos + sizeof(size_t);
        if constexpr (constexpr auto count = readFromBytes<size_t>(ptr(pos))) {
            static constexpr auto res = readNTArray<data_pos, count, T>();
            return readRngs<res.next_pos, N>([](size_t i) { return &res.value[i].value; });
        } else
            return readRngs<data_pos, N>([](size_t) -> read_t<T> const * { return nullptr; });
    }

    template<size_t pos, typename... T>
    static constexpr auto readAt(Tup<T...>) {
        if constexpr (sizeof...(T)) return readTuple<pos, 0, sizeof...(T)>(std::tuple<T...>{});
        else
            return result<pos>(std::tuple{});
    }

    template<size_t pos, typename... T>
    static constexpr auto readAt(Variant<T...>) {
        constexpr auto index = readFromBytes<size_t>(ptr(pos));
        constexpr auto res = readAt<pos + sizeof(size_t)>(std::tuple_element_t<index, std::tuple<T...>>{});
        return result<res.next_pos>(read_t<Variant<T...>>{std::in_place_index<index>, res.value});
    }

    template<size_t pos, typename T>
    static constexpr auto readAt(Optional<T>) {
        constexpr auto data_pos = pos + sizeof(bool);
        if constexpr (using ROptional = read_t<Optional<T>>; constexpr auto has_value = readFromBytes<bool>(ptr(pos))) {
            constexpr auto res = readAt<data_pos>(T{});
            return result<res.next_pos>(ROptional{res.value});
        } else
            return result<data_pos>(ROptional{});
    }

    template<size_t pos, typename T, typename E>
    static constexpr auto readAt(Expected<T, E>) {
        using RExpected = read_t<Expected<T, E>>;
        constexpr auto data_pos = pos + sizeof(bool);
        if constexpr (readFromBytes<bool>(ptr(pos))) {
            constexpr auto res = readAt<data_pos>(T{});
            return result<res.next_pos>(RExpected{res.value});
        } else {
            constexpr auto res = readAt<data_pos>(E{});
            return result<res.next_pos>(RExpected{std::unexpect, res.value});
        }
    }

    template<size_t pos, typename E>
    static constexpr auto readAt(Expected<void, E>) {
        using RExpected = read_t<Expected<void, E>>;
        constexpr auto data_pos = pos + sizeof(bool);
        if constexpr (readFromBytes<bool>(ptr(pos))) return result<data_pos>(RExpected{});
        else {
            constexpr auto res = readAt<data_pos>(E{});
            return result<res.next_pos>(RExpected{std::unexpect, res.value});
        }
    }

    template<size_t pos, typename T>
    static constexpr auto readAt(Pointer<T>) {
        constexpr auto data_pos = pos + sizeof(bool);
        if constexpr (constexpr auto has_value = readFromBytes<bool>(ptr(pos))) {
            static constexpr auto res = readAt<data_pos>(T{});
            return result<res.next_pos>(&res.value);
        } else
            return result<data_pos>(read_t<Pointer<T>>{});
    }

    template<size_t pos, typename T, typename WT>
    static constexpr auto readAt(CustomData<T, WT>) {
        if constexpr (constexpr auto res = readAt<pos>(WT{}); requires { T::from_static_data(res.value); })
            return result<res.next_pos>(T::from_static_data(res.value));
        else if constexpr (requires { from_static_data(std::type_identity<T>{}, res.value); })
            return result<res.next_pos>(from_static_data(std::type_identity<T>{}, res.value));
        else
            return res;
    }

    template<size_t pos, typename T, typename WT>
    static constexpr auto readAt(Recursive<T, WT>) {
        constexpr auto res = readAt<pos>(writef<WT>());
        static constexpr auto rcrsv_value = [&] {
            if constexpr (requires { T::from_static_data(res.value); }) return T::from_static_data(res.value);
            else if constexpr (requires { from_static_data(std::type_identity<T>{}, res.value); })
                return from_static_data(std::type_identity<T>{}, res.value);
            else
                return res.value;
        }();
        return result<res.next_pos>(StaticPtr<T, WT>{&rcrsv_value});
    }

private:
    static constexpr auto ptr(size_t pos) { return buffer.data() + pos; }

    template<size_t pos, typename T, size_t index, size_t count>
    static constexpr auto readRcrsv(auto &array) {
        constexpr auto res = readAt<pos>(T{});
        std::construct_at(&array[index].value, res.value);
        if constexpr ((index + 1) != count) return readRcrsv<res.next_pos, T, index + 1, count>(array);
        else
            return result<res.next_pos>(0);
    }

    template<size_t pos, size_t count, typename T>
    static constexpr auto readNTArray() {
        using value_t = read_t<T>;
        union Storage {
            value_t value;
            constexpr Storage() {}
            constexpr ~Storage() {}
        };
        std::array<Storage, count> array;
        auto const res = readRcrsv<pos, T, 0, count>(array);
        return result<res.next_pos>(array);
    }

    template<size_t pos, size_t depth>
    static constexpr auto readRngs(auto prev_addr) {
        constexpr auto count = readFromBytes<size_t>(ptr(pos));
        constexpr auto next_pos = pos + sizeof(size_t) + sizeof(SpanOff) * count;
        using prev_t = std::remove_cvref_t<decltype(*prev_addr(0))>;
        using span_t = std::span<std::span<prev_t const> const>;
        if constexpr (count) {
            static constexpr auto span_arr = [&] {
                auto const off_arr = readFromBytes<SpanOff, count>(ptr(pos + sizeof(size_t)));
                std::array<std::span<prev_t const>, count> span_arr;
                for (size_t i = 0; i != count; ++i)
                    span_arr[i] = std::span{prev_addr(off_arr[i].base), off_arr[i].size};
                return span_arr;
            }();
            if constexpr (depth) return readRngs<next_pos, depth - 1>([](size_t i) { return &span_arr[i]; });
            else
                return result<next_pos>(span_t{span_arr});
        } else if constexpr (depth)
            return readRngs<next_pos, depth - 1>([](size_t) { return span_t{}.data(); });
        else
            return result<next_pos>(span_t{});
    }

    template<size_t pos, size_t index, size_t size>
    static constexpr auto readTuple(auto tup, auto &...elems) {
        constexpr auto res = readAt<pos>(get<index>(tup));
        if constexpr ((index + 1) != size) return readTuple<res.next_pos, index + 1, size>(tup, elems..., res.value);
        else
            return result<res.next_pos>(std::tuple{elems..., res.value});
    }
};
}// namespace tek::detail

namespace tek {
template<size_t buffer_size = 1 * 1024 * 1024>
consteval auto static_data(std::invocable auto func) {
    static constexpr auto buffer = [&] {
        std::array<std::byte, buffer_size> buffer{};
        auto ptr = buffer.data();
        detail::writeAt(func(), ptr);
        return buffer;
    }();
    return detail::Reader<buffer>::template readAt<0>(detail::writef<decltype(func())>()).value;
}

template<typename T>
using static_data_t = decltype(detail::readf(detail::writef<T>()));
}// namespace tek

#endif
