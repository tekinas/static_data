#include <static_data/static_data.hpp>

#include <algorithm>
#include <charconv>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <functional>
#include <initializer_list>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace tek {
namespace detail {
class CString {
public:
    constexpr CString() = default;
    template<std::convertible_to<std::string_view> S>
    constexpr CString(S &&str) : str{std::forward<S>(str)} {}
    constexpr bool operator==(CString const &) const = default;
    constexpr bool operator==(std::string_view s) const { return str == s; }
    constexpr auto &to_static_data() const { return str; }
    static constexpr auto from_static_data(std::span<char const> span) { return std::string_view{span}; }

private:
    std::string str;
};

template<typename Value>
class ValueMap {
public:
    constexpr ValueMap() = default;

    constexpr ValueMap(std::initializer_list<std::pair<std::string_view, Value>> init) {
        for (auto [str, val] : init)
            if (std::ranges::find(keys, str) == keys.end()) {
                keys.push_back(str);
                values.push_back(std::move(val));
            }
    }

    constexpr auto to_static_data() const { return std::forward_as_tuple(keys, values); }

private:
    std::vector<CString> keys;
    std::vector<Value> values;
};

template<typename char_t, size_t N>
struct fixed_string {
    using value_type = char_t;
    using str_view = std::basic_string_view<value_type>;

    constexpr fixed_string(str_view str) {
        if constexpr (N) std::ranges::copy_n(str.data(), N, m_Value.data());
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
}// namespace detail

template<auto value>
inline constexpr detail::ConstValue<value> cv{};

namespace literals {
template<char... ch>
consteval auto operator""_i() {
    constexpr auto value = [] -> std::optional<size_t> {
        std::array const str{ch...};
        size_t value;
        auto const last = str.data() + str.size();
        if (auto const [ptr, err] = std::from_chars(str.data(), last, value); err == std::errc{} and ptr == last)
            return value;
        return std::nullopt;
    }();
    if constexpr (value) return detail::ConstValue<*value>{};
    else
        static_assert(false, "invalid literal value");
}

template<detail::fixed_string str>
consteval auto operator""_k() {
    return detail::ConstValue<str>{};
}
}// namespace literals

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

    constexpr auto &to_static_data() const { return m_Value; }

private:
    std::variant<std::nullptr_t, bool, int64_t, uint64_t, double, detail::CString, array, object> m_Value;
};

template<auto &jv>
class const_value {
private:
    static constexpr auto value_ = std::get<jv.index()>(jv);

    template<size_t i>
    struct ObjElem {
        static constexpr auto key = std::get<0>(value_)[i];
        static constexpr auto value = const_value<*std::get<1>(value_)[i]>{};
    };

public:
    static constexpr auto kind = json::kind{static_cast<std::underlying_type_t<json::kind>>(jv.index())};

    constexpr auto operator*() const
        requires(not(kind == json::kind::Array or kind == json::kind::Object))
    {
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

    template<size_t i>
        requires(kind == json::kind::Array and i < size())
    static constexpr auto operator[](detail::ConstValue<i>) {
        return const_value<*value_[i]>{};
    }

    template<size_t i>
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
                if (keys[i] == key) return i;
        }();
        return const_value<*std::get<1>(value_)[i]>{};
    }
};
}// namespace json

template<typename Func>
    requires std::is_invocable_r_v<json::value, Func>
consteval auto static_json(Func func) {
    return json::const_value<*tek::static_data([=] { return std::invoke_r<json::value>(func); })>{};
}
}// namespace tek
