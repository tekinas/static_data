#include <static_data/static_data.hpp>

#include <cstdint>
#include <fmt/compile.h>
#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/std.h>
#include <lexy/action/base.hpp>
#include <lexy/action/parse.hpp>
#include <lexy/callback.hpp>
#include <lexy/callback/container.hpp>
#include <lexy/dsl.hpp>
#include <lexy/dsl/base.hpp>
#include <lexy/dsl/production.hpp>
#include <lexy/grammar.hpp>
#include <lexy/input/string_input.hpp>
#include <optional>
#include <ranges>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace {
template<typename Key, typename Value>
class UnorderedMap {
public:
    constexpr UnorderedMap() = default;
    constexpr auto to_static_data() { return std::views::zip(keys, values); }
    constexpr auto to_static_data() const { return std::views::zip(keys, values); }
    constexpr bool empty() const { return keys.empty(); }
    constexpr size_t size() const { return keys.size(); }
    constexpr void reserve(size_t s) {
        keys.reserve(s);
        values.reserve(s);
    }
    constexpr void clear() {
        keys.clear();
        values.clear();
    }
    constexpr void emplace(auto &&...args) {
        std::pair<Key, Value> ele{args...};
        if (auto const itr = std::ranges::find(keys, ele.first); itr == keys.end()) {
            keys.push_back(std::move(ele.first));
            values.push_back(std::move(ele.second));
        } else
            values[itr - keys.begin()] = std::move(ele.second);
    }
    constexpr void insert(auto &&value) { emplace(value); }

private:
    std::vector<Key> keys;
    std::vector<Value> values;
};

template<typename Char>
class BasicString : public std::vector<char> {
public:
    constexpr BasicString() = default;
    constexpr BasicString(auto b, auto e) : std::vector<char>(b, e) {}
    constexpr BasicString &append(auto b, auto e) {
        insert(end(), b, e);
        return *this;
    }
    constexpr operator std::basic_string_view<Char>() const { return std::basic_string_view<Char>{data(), size()}; }
    constexpr auto to_static_data() const { return std::span{*this}; }
    static constexpr auto from_static_data(auto span) { return std::string_view{span}; }
};

using String = BasicString<char>;

namespace ast {
class json_value;
using json_null = std::nullptr_t;
using json_bool = bool;
struct json_number {
    int64_t integer;
    std::optional<String> fraction;
    std::optional<int16_t> exponent;
};
using json_string = String;
using json_array = std::vector<json_value>;
using json_object = UnorderedMap<String, json_value>;
class json_value {
public:
    template<typename T>
    explicit constexpr json_value(T t) : v(std::move(t)) {}
    std::variant<json_null, json_bool, json_number, json_string, json_array, json_object> v;
};

constexpr auto to_static_data(json_number const &jn) {
    return std::forward_as_tuple(jn.integer, jn.fraction, jn.exponent);
}
constexpr auto &to_static_data(json_value const &jv) { return jv.v; }
constexpr auto from_static_data(std::type_identity<json_number>, auto jn) {
    struct json_number {
        int64_t integer;
        std::optional<std::string_view> fraction;
        std::optional<int16_t> exponent;
    };
    return json_number{std::get<0>(jn), std::get<1>(jn), std::get<2>(jn)};
}
}// namespace ast
}// namespace

template<>
constexpr bool tek::is_recursive<ast::json_value> = true;

namespace {
namespace grammar {
namespace dsl = lexy::dsl;

struct json_value;

struct null : lexy::token_production {
    static constexpr auto rule = dsl::lit<"null">;
    static constexpr auto value = lexy::construct<ast::json_null>;
};

struct boolean : lexy::token_production {
    struct true_ : lexy::transparent_production {
        static constexpr auto rule = dsl::lit<"true">;
        static constexpr auto value = lexy::constant(true);
    };
    struct false_ : lexy::transparent_production {
        static constexpr auto rule = dsl::lit<"false">;
        static constexpr auto value = lexy::constant(false);
    };
    static constexpr auto rule = dsl::p<true_> | dsl::p<false_>;
    static constexpr auto value = lexy::forward<ast::json_bool>;
};

struct number : lexy::token_production {
    struct integer : lexy::transparent_production {
        static constexpr auto rule = dsl::minus_sign + dsl::integer<int64_t>(dsl::digits<>.no_leading_zero());
        static constexpr auto value = lexy::as_integer<int64_t>;
    };
    struct fraction : lexy::transparent_production {
        static constexpr auto rule = dsl::lit_c<'.'> >> dsl::capture(dsl::digits<>);
        static constexpr auto value = lexy::as_string<String>;
    };
    struct exponent : lexy::transparent_production {
        static constexpr auto rule = (dsl::lit_c<'e'> | dsl::lit_c<'E'>) >> (dsl::sign + dsl::integer<int16_t>);
        static constexpr auto value = lexy::as_integer<int16_t>;
    };
    static constexpr auto rule = dsl::peek(dsl::lit_c<'-'> / dsl::digit<>) >>
                                 (dsl::p<integer> + dsl::opt(dsl::p<fraction>) + dsl::opt(dsl::p<exponent>));
    static constexpr auto value = lexy::construct<ast::json_number>;
};

struct string : lexy::token_production {
    struct invalid_char {
        static constexpr auto name = "invalid character in string literal";
    };
    static constexpr auto escaped_symbols = lexy::symbol_table<char>//
                                                    .map<'"'>('"')
                                                    .map<'\\'>('\\')
                                                    .map<'/'>('/')
                                                    .map<'b'>('\b')
                                                    .map<'f'>('\f')
                                                    .map<'n'>('\n')
                                                    .map<'r'>('\r')
                                                    .map<'t'>('\t');
    struct code_point_id {
        static constexpr auto rule = dsl::lit<"u"> >> dsl::code_unit_id<lexy::utf16_encoding, 4>;
        static constexpr auto value = lexy::construct<lexy::code_point>;
    };
    static constexpr auto rule = dsl::quoted.limit(dsl::ascii::newline)(
            (-dsl::unicode::control).error<invalid_char>,
            dsl::backslash_escape.symbol<escaped_symbols>().rule(dsl::p<code_point_id>));
    static constexpr auto value = lexy::as_string<ast::json_string, lexy::utf8_encoding>;
};

struct unexpected_trailing_comma {
    static constexpr auto name = "unexpected trailing comma";
};

constexpr auto sep = dsl::sep(dsl::comma).trailing_error<unexpected_trailing_comma>;

struct array {
    static constexpr auto rule = dsl::square_bracketed.opt_list(dsl::recurse<json_value>, sep);
    static constexpr auto value = lexy::as_list<ast::json_array>;
};

struct object {
    static constexpr auto rule =
            dsl::curly_bracketed.opt_list(dsl::p<string> + dsl::try_(dsl::colon) + dsl::recurse<json_value>, sep);
    static constexpr auto value = lexy::as_collection<ast::json_object>;
};

struct json_value : lexy::transparent_production {
    static constexpr auto name = "json value";
    struct expected_json_value {
        static constexpr auto name = "expected json value";
    };
    static constexpr auto rule = dsl::p<null> | dsl::p<boolean> | dsl::p<number> | dsl::p<string> | dsl::p<object> |
                                 dsl::p<array> | dsl::error<expected_json_value>;
    static constexpr auto value = lexy::construct<ast::json_value>;
};

struct json {
    static constexpr auto max_recursion_depth = 19;
    static constexpr auto whitespace = dsl::ascii::blank / dsl::ascii::newline;
    static constexpr auto rule = dsl::p<json_value> + dsl::eof;
    static constexpr auto value = lexy::forward<ast::json_value>;
};

struct report_error {
    struct _sink {
        using return_type = std::size_t;
        template<typename Input, typename Reader, typename Tag>
        constexpr void operator()(const lexy::error_context<Input> &, const lexy::error<Reader, Tag> &) {
            std::unreachable();
        }
        constexpr std::size_t finish() && { return 0; }
    };
    constexpr auto sink() const { return _sink{}; }
};
}// namespace grammar
}// namespace

template<typename... Funcs>
struct overload : Funcs... {
    using Funcs::operator()...;
};

template<char ch, size_t max_n = 100>
constexpr auto times_ch(size_t n) {
    static constexpr std::string_view str{tek::static_data([] { return std::views::repeat(ch, max_n); })};
    assert(n <= max_n);
    return str.substr(0, n);
}

void print_json(tek::static_data_t<ast::json_value> json, size_t level = 0) {
    constexpr size_t lsize = 2;
    std::visit(overload{[](tek::static_data_t<ast::json_null>) { fmt::print("null"); },
                        [](tek::static_data_t<ast::json_bool> jb) { fmt::print("{}", jb); },
                        [](tek::static_data_t<ast::json_number> jn) {
                            fmt::print("{}", jn.integer);
                            if (jn.fraction) fmt::print(".{}", *jn.fraction);
                            if (jn.exponent) fmt::print("e{}", *jn.exponent);
                        },
                        [](tek::static_data_t<ast::json_string> js) {
                            fmt::print("\"");
                            for (auto c : js)
                                if (c == '"') fmt::print(R"(\")");
                                else if (c == '\\')
                                    fmt::print(R"(\\)");
                                else if (std::iscntrl(c) != 0)
                                    fmt::print("\\x{:02x}", static_cast<unsigned char>(c));
                                else
                                    fmt::print("{}", c);
                            fmt::print("\"");
                        },
                        [=](tek::static_data_t<ast::json_array> ja) {
                            fmt::print("[\n");
                            auto first = true;
                            for (auto jvalue : ja) {
                                if (first) first = false;
                                else
                                    fmt::print(",\n");
                                fmt::print("{}", times_ch<' '>(lsize * (level + 1)));
                                print_json(jvalue, level + 1);
                            }
                            fmt::print("\n{}]", times_ch<' '>(lsize * level));
                        },
                        [=](tek::static_data_t<ast::json_object> jo) {
                            fmt::print("{{\n");
                            auto first = true;
                            for (auto &&[key, value] : jo) {
                                if (first) first = false;
                                else
                                    fmt::print(",\n");
                                fmt::print("{}{} : ", times_ch<' '>(lsize * (level + 1)), key);
                                print_json(value, level + 1);
                            }
                            fmt::print("\n{}}}", times_ch<' '>(lsize * level));
                        }},
               *json);
}

int main() {
    constexpr auto res = tek::static_data([] {
        constexpr std::basic_string_view json_str =
                u8R"%([true, false, null, 14124.2414E12, { "name" : "tekinas"}, [0, 1, 2, 3, true], {"1" : true, "2" : false, "3" : null, "4" : 14124.2414E12, "5" : { "name" : "tekinas"}, "6" : [0, 1, 2, 3, true]}])%";
        auto json = lexy::parse<grammar::json>(lexy::string_input(json_str), grammar::report_error{});
        if (json.is_error() or not json.has_value()) std::unreachable();
        return json.value();
    });
    print_json(res);
}
