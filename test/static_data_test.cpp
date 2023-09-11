#include <static_data/static_data.hpp>

#include <cassert>
#include <fmt/compile.h>
#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/std.h>
#include <functional>
#include <numeric>
#include <string_view>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

template<typename T, typename E, typename Char>
struct fmt::formatter<std::expected<T, E>, Char> {
public:
    constexpr auto parse(auto &ctx) const { return ctx.begin(); }

    constexpr auto format(std::expected<T, E> const &exp, auto &ctx) const {
        if (exp) {
            if constexpr (std::is_void_v<T>) return fmt::format_to(ctx.out(), "expected()");
            else
                return fmt::format_to(ctx.out(), "expected({})", *exp);
        }
        return fmt::format_to(ctx.out(), "unexpected({})", exp.error());
    }
};

namespace {
namespace rngs = std::ranges;
namespace vws = std::views;

constexpr auto to_string = [](auto &&e) {
    std::vector<char> buffer;
    fmt::format_to(std::back_inserter(buffer), FMT_COMPILE("{}"), e);
    return buffer;
};

constexpr auto deref = [](auto p) { return *p; };

template<typename Variant, size_t index, typename... Args>
    requires std::is_constructible_v<std::variant_alternative_t<index, Variant>, Args...>
constexpr auto make(Args &&...args) {
    return Variant{std::in_place_index<index>, std::forward<Args>(args)...};
}

template<typename Func, rngs::input_range... Rngs>
    requires(std::invocable<Func &, rngs::range_reference_t<Rngs>...> and sizeof...(Rngs) > 0)
constexpr void cartesian_product(Func &&func, Rngs &&...rngs) {
    auto helper = [](auto self, auto &&func, auto &r1, auto &...rn) {
        for (auto &&e : r1)
            if constexpr (sizeof...(rn)) self(self, [&](auto &&...en) { std::invoke(func, e, en...); }, rn...);
            else
                std::invoke(func, e);
    };
    helper(helper, func, rngs...);
}

class Vec : public tek::recursive_base {
public:
    constexpr Vec(double val) : value{val} {}
    constexpr Vec(double val, size_t n) : value{val} {
        for (auto i : vws::iota(1uz, n + 1))
            if (i % 2 == 0) rng.emplace_back(9012.214 * static_cast<double>(i) * val);
            else
                rng.emplace_back(1224.124 * static_cast<double>(i) * val, i / 2);
    }
    constexpr auto to_static_data() const { return std::pair{value, rng}; }
    static constexpr auto from_static_data(auto val) {
        struct CVec {
            double value;
            std::span<tek::static_data_t<Vec> const> rng;
        };
        return CVec{std::get<0>(val), std::get<1>(val)};
    }

private:
    double value;
    std::vector<Vec> rng;
};

template<char ch, size_t max_n = 500>
constexpr auto times_ch(size_t n) {
    static constexpr std::string_view str{tek::static_data([] { return vws::repeat(ch, max_n); })};
    assert(n <= max_n);
    return str.substr(0, n);
}

constexpr void print_vec(tek::static_data_t<Vec> sv, size_t level = 0) {
    fmt::print("{}value : {}, range size : {}\n", times_ch<' '>(level * 2), sv->value, sv->rng.size());
    for (auto &esv : sv->rng) print_vec(esv, level + 1);
}
}// namespace

int main() {
    using namespace std::literals;
    constexpr auto fmt_str = "{}\n"sv;
    using tek::static_data;
    fmt::print(fmt_str, static_data([] { return '#'; }));
    fmt::print(fmt_str, static_data([] { return true; }));
    fmt::print(fmt_str, static_data([] { return -14124897899812321z; }));
    fmt::print(fmt_str, static_data([] { return 89718936127247788uz; }));
    fmt::print(fmt_str, static_data([] { return -1.123f; }));
    fmt::print(fmt_str, static_data([] { return 34.998162831423; }));
    fmt::print(fmt_str, static_data([] {
                   return std::tuple{'#', true, -14124897899812321z, 89718936127247788uz, -1.123f, 34.998162831423};
               }));
    fmt::print(fmt_str, static_data([] {
                   using Tup = std::tuple<double, std::string, int64_t>;
                   return std::vector<std::vector<std::vector<Tup>>>{
                           {{Tup{98139182.124124, "STRING1", -134978124}, Tup{777.123132, "STRING2", 27319848489},
                             Tup{1.22333444455555, "STRING3", -66666666666}},
                            {Tup{-999999.88888, "STRING4", 0}, Tup{-55555.4444333221, "STRING5", 7777777777}},
                            {Tup{42322.14344, "STRING6", 0}}}};
               }));
    fmt::print(fmt_str, static_data([] {
                   using Tup = std::tuple<double, std::string, int64_t>;
                   return std::vector<std::vector<std::vector<Tup>>>{};
               }));
    fmt::print(fmt_str, static_data([] {
                   return std::array{std::tuple{"1"sv, "22"sv}, std::tuple{"333"sv, "4444"sv},
                                     std::tuple{"55555"sv, "666666"sv}};
               }));
    fmt::print(fmt_str, static_data([] {
                   return std::array{std::tuple{vws::iota(-10, 10), vws::iota(-20, 20)},
                                     std::tuple{vws::iota(-30, 30), vws::iota(-40, 40)},
                                     std::tuple{vws::iota(-50, 50), vws::iota(-60, 60)}};
               }));
    fmt::print(fmt_str, static_data([] {
                   return std::array{std::tuple{std::array{898, 136, 391}, std::array{6, 3296, 92368}},
                                     std::tuple{std::array{78, 136, 913}, std::array{8688721, -137319, -3187}}};
               }));
    fmt::print(fmt_str, static_data([] {
                   return std::tuple{std::array<std::tuple<std::string, double, int64_t>, 0>{}, vws::iota(-81, 81),
                                     vws::iota(0uz, 100uz),
                                     vws::iota(0, 1000) | vws::filter([](auto e) { return e % 2 == 0; }),
                                     std::tuple{1, 2.134, 90.9f, 'o', "tekinas"s, std::array<int, 0>{}, std::tuple{}}};
               }));
    fmt::print(fmt_str, static_data([] {
                   return std::vector{"Chandragupta Maurya"sv, "Samraat Chakravartin"sv,
                                      "Chhatrapati Shivaji Maharaj"sv, "Ranjit Singh"sv};
               }));
    fmt::print(fmt_str, static_data([] {
                   return std::vector{48273.923, 12983.98216, 76183.981, 0909.123, 7163.123, 98878.9123} |
                          vws::transform(to_string);
               }));
    fmt::print(fmt_str, static_data([] {
                   return std::array{vws::iota(90, 100), vws::iota(10, 20), vws::iota(200, 220)};
               }));
    fmt::print(fmt_str, static_data([] {
                   using Var = std::variant<int64_t, uint64_t, double, std::string_view, float>;
                   return std::array{make<Var, 0>(-1873175),   make<Var, 2>(16871313.13385), make<Var, 1>(6187637),
                                     make<Var, 1>(9981898931), make<Var, 2>(98.1761378678),  make<Var, 3>("Salvation"),
                                     make<Var, 3>("Nirava"),   make<Var, 4>(123.13398f),     make<Var, 4>(78.13f),
                                     make<Var, 2>(9111.11111)};
               }));
    fmt::print(fmt_str, static_data([] {
                   return vws::iota(0, 200) |
                          vws::transform([](auto e) { return e % 2 == 0 ? std::optional{e} : std::nullopt; }) |
                          vws::transform([](auto &&e) { return e.transform(to_string); });
               }));
    fmt::print(fmt_str, static_data([] {
                   using Exp = std::expected<int64_t, std::string_view>;
                   static constexpr std::unexpected odd{"odd"sv};
                   return vws::iota(0, 200) | vws::transform([](auto e) { return e % 2 == 0 ? Exp{e} : odd; }) |
                          vws::transform([](auto &&e) { return e.transform(to_string); });
               }));
    fmt::print(fmt_str, static_data([] {
                   return vws::iota(0, 10) | vws::transform([](auto e) { return vws::iota(-e, e + 1); }) |
                          vws::transform([](auto iv) {
                              return iv | vws::transform([](auto e) {
                                         return to_string(e) | vws::transform([](auto c) {
                                                    return std::pair{c * 1010101.010101, vws::repeat(c, 5)};
                                                });
                                     });
                          });
               }));
    fmt::print(fmt_str, static_data([] {
                   std::array letters{'!', '@', '#', '$', '%', '^', '&', '*', '-', '=', '_', '+'};
                   std::array strs{"forest"sv, "valley"sv, "ski"sv, "mountain"sv};
                   std::vector cars{"Mercedes"sv, "BMW"sv, "Porsche"sv};
                   std::vector<std::tuple<char, std::string_view, std::string_view>> vec;
                   cartesian_product([&](auto c, auto str, auto car) { vec.emplace_back(c, str, car); }, letters, strs,
                                     cars);
                   return vec;
               }));
    fmt::print(fmt_str, static_data([] {
                            static constexpr double d1 = 8913.231, d2 = -2130924.12, d3 = 99324.1412, d4 = -7777.666,
                                                    d5 = 0.2134;
                            return std::array{&d1, &d2, &d3, &d4, &d5};
                        }) | vws::transform(deref));
    fmt::print(fmt_str, static_data([] {
                            using Ptr = std::unique_ptr<size_t>;
                            return vws::iota(0uz, 500uz) |
                                   vws::transform([](auto i) { return i % 7 == 0 ? Ptr{new size_t{i}} : Ptr{}; });
                        }) | vws::filter([](auto ptr) { return ptr != nullptr; }) |
                                vws::transform(deref));
    fmt::print(fmt_str, static_data([] {
                   std::array const colors{"Blue"sv, "Red"sv, "Green"sv, "Yellow"sv, "Black"sv, "White"sv};
                   return vws::zip(auto{colors}, vws::iota(0uz));
               }));
    fmt::print(fmt_str, static_data([] {
                   struct Val {
                       constexpr auto to_static_data() const { return vws::iota(-50z, 50z); }
                   };
                   return Val{};
               }));
    fmt::print(fmt_str, static_data([] {
                   struct Val {
                       constexpr auto to_static_data() const { return vws::iota(0uz, 5000uz); }
                       static constexpr auto from_static_data(std::span<size_t const> arr) {
                           return std::accumulate(arr.begin(), arr.end(), 0uz);
                       }
                   };
                   return Val{};
               }));
    fmt::print(fmt_str, static_data([] {
                   struct PData {
                       constexpr explicit PData(int16_t d0, double d1, char d2) : d0{d0}, d1{d1}, d2{d2} {}
                       int16_t d0;
                       double d1;
                       char d2;
                   };
                   struct Val {
                       constexpr auto to_static_data() const {
                           static constexpr auto chars = std::array{'!', '@', '#', '$', '%', '^', '&', '*', '[', ']'};
                           return vws::iota(0uz, 40uz) | vws::transform([](auto i) {
                                      return PData{static_cast<int16_t>(i), static_cast<double>(i) * 98765.516,
                                                   chars[i % sizeof(chars)]};
                                  });
                       }
                       static constexpr auto from_static_data(std::span<PData const> span) {
                           return span | vws::transform([](PData v) { return std::tuple{v.d0, v.d1, v.d2}; });
                       }
                   };
                   return Val{};
               }));
    fmt::print(fmt_str, static_data([] {
                   struct NDCValue {
                       constexpr explicit NDCValue(double d0) : d0{d0} {}
                       double d0;
                   };
                   struct Val {
                       static constexpr auto from_static_data(std::span<std::tuple<int32_t, NDCValue> const> span) {
                           return span | vws::transform([](auto v) {
                                      return std::pair{std::get<0>(v), std::get<1>(v).d0};
                                  });
                       }
                       constexpr auto to_static_data() const {
                           return vws::zip(vws::iota(0), vws::iota(-200, 200) | vws::transform([](auto i) {
                                                             return NDCValue{i * 98765.516};
                                                         }));
                       }
                   };
                   return Val{};
               }));
    fmt::print(fmt_str, static_data([] {
                            using View = decltype(vws::iota(0, 10));
                            return std::array{std::make_unique<View>(0, 10),  std::make_unique<View>(-20, -1),
                                              std::make_unique<View>(-4, 30), std::make_unique<View>(323, 350),
                                              std::make_unique<View>(-9, 9),  std::make_unique<View>(100, 130)};
                        }) | vws::transform(deref));
    print_vec(static_data([] { return Vec{12.124, 15}; }));
}
