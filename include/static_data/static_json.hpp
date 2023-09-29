#ifndef TEK_STATIC_JSON
#define TEK_STATIC_JSON

#include "static_data.hpp"
#include <algorithm>
#include <charconv>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <ranges>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace tek {
namespace detail {
namespace stdr = std::ranges;
namespace vws = std::views;

template<typename Value>
class ValueMap {
public:
    template<bool const_value>
    struct key_value {
        std::string_view key;
        std::conditional_t<const_value, Value const &, Value &> value;
    };

    constexpr ValueMap() = default;

    constexpr ValueMap(std::initializer_list<std::pair<std::string_view, Value>> init) {
        for (auto [str, val] : init) emplace(str, std::move(val));
    }

    constexpr bool operator==(ValueMap const &other) const {
        if (size() != other.size()) return false;
        return stdr::is_permutation(keys, other.keys) and stdr::is_permutation(values, other.values);
    }

    constexpr auto begin() { return get_view(*this).begin(); }
    constexpr auto begin() const { return get_view(*this).begin(); }

    constexpr auto end() { return get_view(*this).end(); }
    constexpr auto end() const { return get_view(*this).end(); }

    constexpr auto size() const { return keys.size(); }

    constexpr bool empty() const { return keys.empty(); }

    constexpr void clear() {
        keys.clear();
        values.clear();
    }

    constexpr auto capacity() const { return keys.capacity(); }

    constexpr void reserve(size_t sz) {
        keys.reserve(sz);
        values.reserve(sz);
    }

    constexpr bool contains(std::string_view key) const { return stdr::contains(keys, key); }

    constexpr auto find(std::string_view key) { return find_impl(*this, key); }
    constexpr auto find(std::string_view key) const { return find_impl(*this, key); }

    template<std::random_access_iterator Itr>
        requires(std::same_as<key_value<true>, std::iter_value_t<Itr>> or
                 std::same_as<key_value<false>, std::iter_value_t<Itr>>)
    constexpr void erase(Itr itr) {
        auto const dist = [&] {
            if constexpr (std::same_as<key_value<true>, std::iter_value_t<Itr>>)
                return stdr::distance(std::as_const(*this).begin(), itr);
            else
                return stdr::distance(begin(), itr);
        }();
        keys.erase(stdr::next(keys.begin(), dist));
        values.erase(stdr::next(values.begin(), dist));
    }

    constexpr bool erase(std::string_view key) {
        if (auto const itr = find(key); itr != end()) {
            erase(itr);
            return true;
        }
        return false;
    }

    constexpr Value &operator[](std::string_view key) {
        if (auto const itr = find(key); itr != end()) return (*itr).value;
        keys.emplace_back(key);
        return values.emplace_back();
    }

    template<std::convertible_to<Value> Arg>
    constexpr auto emplace(std::string_view key, Arg &&arg) {
        if (auto const itr = find(key); itr != end()) return std::pair{itr, false};
        keys.emplace_back(key);
        values.emplace_back(std::forward<Arg>(arg));
        return std::pair{stdr::prev(end()), true};
    }

    constexpr auto to_static_data() const { return std::forward_as_tuple(keys, values); }

private:
    template<typename T>
    static constexpr auto get_view(T &&this_) {
        return vws::zip(this_.keys, this_.values) | vws::transform([](auto &&elm) {
                   return key_value<std::is_const_v<std::remove_reference_t<T>>>{std::get<0>(elm), std::get<1>(elm)};
               });
    }

    template<typename T>
    static constexpr auto find_impl(T &&this_, std::string_view key) {
        if (auto const itr = stdr::find(this_.keys, key); itr != this_.keys.end())
            return stdr::next(this_.begin(), stdr::distance(this_.keys.begin(), itr));
        return this_.end();
    }

    std::vector<std::string> keys;
    std::vector<Value> values;
};

template<typename char_t, size_t N>
struct fixed_string {
    using value_type = char_t;
    using str_view = std::basic_string_view<value_type>;

    constexpr fixed_string(str_view str) {
        if constexpr (N) stdr::copy_n(str.data(), N, m_Value.data());
    }

    constexpr fixed_string(value_type const (&str)[N + 1]) : fixed_string{str_view{str, N}} {}

    constexpr str_view value() const {
        if constexpr (N) return str_view{m_Value};
        else
            return {};
    }

    constexpr bool operator==(str_view str) const { return value() == str; }

    constexpr operator str_view() const { return value(); }

    std::array<value_type, N> m_Value;
};

template<typename char_t, size_t N>
fixed_string(const char_t (&str)[N]) -> fixed_string<char_t, N - 1>;

template<auto value>
class ConstValue {};

template<typename... Funcs>
struct overload : Funcs... {
    using Funcs::operator()...;
};
}// namespace detail

namespace json {
enum class kind : uint8_t { Null, Bool, Int64, UInt64, Double, String, Array, Object };

class value;
using array = std::vector<value>;
using object = detail::ValueMap<value>;

class value : tek::recursive_base {
public:
    constexpr value() = default;

    constexpr value(std::nullptr_t) : m_Value{std::in_place_index<0>} {}

    constexpr value(bool b) : m_Value{std::in_place_index<1>, b} {}

    template<std::integral I>
        requires(not std::same_as<bool, I> and std::is_signed_v<I>)
    constexpr value(I i) : m_Value{std::in_place_index<2>, i} {}

    template<std::integral I>
        requires(not std::same_as<bool, I> and std::is_unsigned_v<I>)
    constexpr value(I i) : m_Value{std::in_place_index<3>, i} {}

    constexpr value(std::floating_point auto f) : m_Value{std::in_place_index<4>, f} {}

    template<std::convertible_to<std::string_view> Arg>
    constexpr value(Arg &&sargs) : m_Value{std::in_place_index<5>, std::forward<Arg>(sargs)} {}

    constexpr value(array arr) : m_Value{std::in_place_index<6>, std::move(arr)} {}

    constexpr value(object objs) : m_Value{std::in_place_index<7>, std::move(objs)} {}

    constexpr bool operator==(value const &) const = default;

    constexpr auto kind() const { return json::kind{static_cast<std::underlying_type_t<json::kind>>(m_Value.index())}; }

    template<typename... Func>
    constexpr auto visit(Func &&...func) {
        return std::visit(detail::overload{std::forward<Func>(func)...}, m_Value);
    }

    template<typename... Func>
    constexpr auto visit(Func &&...func) const {
        return std::visit(detail::overload{std::forward<Func>(func)...}, m_Value);
    }

    constexpr auto &to_static_data() const { return m_Value; }

private:
    std::variant<std::nullptr_t, bool, int64_t, uint64_t, double, std::string, array, object> m_Value;
};

template<auto &jv>
class const_value {
private:
    static constexpr auto value_ = std::get<jv.index()>(jv);

    template<size_t i>
    struct ObjElem {
        static constexpr std::string_view key{std::get<0>(value_)[i]};
        static constexpr auto value = const_value<*std::get<1>(value_)[i]>{};
    };

public:
    static constexpr auto kind = json::kind{static_cast<std::underlying_type_t<json::kind>>(jv.index())};

    constexpr auto operator*() const
        requires(not(kind == json::kind::Array or kind == json::kind::Object))
    {
        if constexpr (kind == json::kind::String) return std::string_view{value_};
        else
            return value_;
    }

    static constexpr auto size()
        requires(kind == json::kind::Object)
    {
        return std::get<0>(value_).size();
    }

    static constexpr auto size()
        requires(kind == json::kind::Array)
    {
        return value_.size();
    }

    template<std::integral auto i>
        requires(kind == json::kind::Array and i < size())
    static constexpr auto operator[](detail::ConstValue<i>) {
        return const_value<*value_[i]>{};
    }

    template<std::integral auto i>
        requires(kind == json::kind::Object and i < size())
    static constexpr auto operator[](detail::ConstValue<i>) {
        return ObjElem<i>{};
    }

    template<detail::fixed_string key>
        requires(kind == json::kind::Object)
    static constexpr auto operator[](detail::ConstValue<key>) {
        constexpr auto i = [] {
            auto const keys = std::get<0>(value_);
            for (size_t i{}; i != size(); ++i)
                if (std::string_view{keys[i]} == key) return i;
        }();
        return const_value<*std::get<1>(value_)[i]>{};
    }
};

namespace literals {
template<char... ch>
consteval auto operator""_i() {
    constexpr auto value = [] -> std::optional<unsigned long long> {
        std::array const str{ch...};
        auto start = str.data();
        auto base = 10;
        if (str.size() >= 2) {
            if (auto const d = str[1]; str[0] == '0') {
                if (d == 'b' or d == 'B') {
                    base = 2;
                    start += 2;
                } else if (d == 'x' or d == 'X') {
                    base = 16;
                    start += 2;
                } else {
                    base = 8;
                    ++start;
                }
            }
        }
        auto const last = str.data() + str.size();
        unsigned long long value;
        if (auto const [ptr, err] = std::from_chars(start, last, value, base); err == std::errc{} and ptr == last)
            return value;
        return {};
    }();
    if constexpr (value) return detail::ConstValue<*value>{};
    else
        static_assert(false, "invalid integer literal");
}

template<detail::fixed_string str>
consteval auto operator""_k() {
    return detail::ConstValue<str>{};
}
}// namespace literals
}// namespace json

template<auto value>
inline constexpr detail::ConstValue<value> cv{};

template<typename Func>
    requires std::is_invocable_r_v<json::value, Func>
consteval auto static_json(Func func) {
    return json::const_value<*tek::static_data([=] { return std::invoke_r<json::value>(func); })>{};
}
}// namespace tek

#endif
