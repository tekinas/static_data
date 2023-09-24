#include <static_data/static_json.hpp>

#include <cassert>
#include <fmt/format.h>

namespace {
template<size_t N>
constexpr void template_for(auto &&func) {
    [&]<size_t... I>(std::index_sequence<I...>) {
        (func.template operator()<I>(), ...);
    }(std::make_index_sequence<N>{});
}

template<char ch, size_t max_n = 100>
constexpr auto times_ch(size_t n) {
    static constexpr std::string_view str{tek::static_data([] { return std::views::repeat(ch, max_n); })};
    assert(n <= max_n);
    return str.substr(0, n);
}

template<typename... Funcs>
struct overload : Funcs... {
    using Funcs::operator()...;
};

template<auto json>
void print_json(size_t level = 0) {
    constexpr size_t lsize = 2;
    if constexpr (json.kind == tek::json::kind::Object)
        template_for<json.size()>([&]<size_t i> {
            if constexpr (not i) fmt::print("{{\n");
            else
                fmt::print(",\n");
            fmt::print("{}{} : ", times_ch<' '>(lsize * (level + 1)), json[tek::cv<i>].key);
            print_json<json[tek::cv<i>].value>(level + 1);
            if constexpr (i + 1 == json.size()) fmt::print("\n{}}}", times_ch<' '>(lsize * level));
        });
    else if constexpr (json.kind == tek::json::kind::Array)
        template_for<json.size()>([&]<size_t i> {
            if constexpr (not i) fmt::print("{{\n");
            else
                fmt::print(",\n");
            fmt::print("{}", times_ch<' '>(lsize * (level + 1)));
            print_json<json[tek::cv<i>]>(level + 1);
            if constexpr (i + 1 == json.size()) fmt::print("\n{}}}", times_ch<' '>(lsize * level));
        });
    else
        overload{[](std::nullptr_t) { fmt::print("null"); }, [](bool jb) { fmt::print("{}", jb); },
                 [](int64_t ji) { fmt::print("{}", ji); },   [](uint64_t ji) { fmt::print("{}", ji); },
                 [](double jf) { fmt::print("{}", jf); },    [](std::string_view js) { fmt::print("{}", js); }}(*json);
}

constexpr tek::json::value get_json() {
    using namespace std::string_view_literals;
    using namespace tek::json;
    value v1 = nullptr;
    value v2 = true;
    value v3 = -124124;
    value v4 = 1961824uz;
    value v5 = 90121.2414f;
    value v6 = 8912489124.87861274;
    value v7 = "Hello"sv;
    value v8{{1, 2, 4, 5, "124124"sv, 12412.1424, {{true, 5, 6}}, nullptr, true, false}};
    value v9{{{"arg1", -9971837.1331},
              {"arg2", 66612.21f},
              {"arg3", 248001uz},
              {"arg4", -5},
              {"arg5", "STRING"},
              {"arg6", -33333.1424},
              {"arg7", nullptr},
              {"arg8", true},
              {"arg9", false},
              {"arg10", {{1, 2, 4, 5, "124124", 12412.1424, nullptr, true, false}}},
              {"arg11",
               {{
                       {"aa", 45},
                       {"bb", -999868172},
                       {"cc", 8888888111111111111uz},
                       {"dd", 333333.42526446898998},
                       {"ee", "JSON"},
                       {"ff", 12412.1424},
                       {"gg", nullptr},
                       {"hh", true},
                       {"ii", false},
                       {"jj", {{1, 2, 4, 5, "EaRtH", 12412.1424, nullptr, true, false}}},
               }}}}};
    array v10{-1363187678, 16871389311uz, true, false, 90.13f, 2223.2424};
    object v11{{"A", 0XDEADBEEF},
               {"B", 077273147136z},
               {"C", 4},
               {"D", 5},
               {"E", "FLOW"sv},
               {"F", 12412.1424},
               {"G", nullptr},
               {"H", true},
               {"I", false},
               {"J", {{-111111111z, 22222222z, 99999999z, -666666z, "GRAPH", -8776676.31, nullptr, true, false}}},
               {"K",
                {{
                        {"L", 1},
                        {"M", 2},
                        {"N", 4},
                        {"O", 5},
                        {"P", "124124"},
                        {"Q", 12412.1424},
                        {"R", nullptr},
                        {"S", true},
                        {"T", false},
                        {"U", {{1, 2, 4, 5, "124124", 12412.1424, nullptr, true, false}}},
                }}}};
    array v12{true, false, true, false, nullptr, false, true};
    return {{{"DATA",
              {{v1,
                v2,
                v3,
                v4,
                v5,
                v6,
                v7,
                v8,
                v9,
                v10,
                v11,
                v12,
                {{v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12}}}}},
             {"PLANE", -78517843136z}}};
}
}// namespace

int main() {
    using namespace tek;
    using namespace tek::literals;
    print_json<static_json([] { return get_json(); })>(), fmt::print("\n");

    fmt::print("{}\n", *static_json([] { return nullptr; }));
    fmt::print("{}\n", *static_json([] { return true; }));
    fmt::print("{}\n", *static_json([] { return false; }));
    fmt::print("{}\n", *static_json([] { return -1324348798961387z; }));
    fmt::print("{}\n", *static_json([] { return 9918936136781383uz; }));
    fmt::print("{}\n", *static_json([] { return 666699999.76451613; }));
    fmt::print("{}\n", *static_json([] { return "TEKINAS IS GENIUS"; }));
    fmt::print("{}\n", *static_json([] { return json::array{0, 1, true, "FILTH"}; })[3_i]);
    fmt::print("{}\n", *static_json([] { return json::object{{"FILTH", "TRUE FILTH"}}; })["FILTH"_k]);
    fmt::print("{}\n", *static_json([] { return json::object{{"KEY", "VALUE"}}; })[0_i].value);

    constexpr auto cj1 = static_json([] { return json::array{1, 2, true, false, "TEKINAS", nullptr}; });
    static_assert(*cj1[0_i] == 1);
    static_assert(*cj1[1_i] == 2);
    static_assert(*cj1[2_i] == true);
    static_assert(*cj1[3_i] == false);
    static_assert(*cj1[4_i] == "TEKINAS");
    static_assert(*cj1[5_i] == nullptr);

    constexpr auto cj2 = static_json([] {
        return json::object{{"A", 1}, {"B", 2}, {"C", true}, {"D", false}, {"E", "TEKINAS"}, {"F", nullptr}};
    });
    static_assert(*cj2["A"_k] == 1);
    static_assert(*cj2["B"_k] == 2);
    static_assert(*cj2["C"_k] == true);
    static_assert(*cj2["D"_k] == false);
    static_assert(*cj2["E"_k] == "TEKINAS");
    static_assert(*cj2["F"_k] == nullptr);

    static_assert(cj2[0_i].key == "A" and *cj2[0_i].value == 1);
    static_assert(cj2[1_i].key == "B" and *cj2[1_i].value == 2);
    static_assert(cj2[2_i].key == "C" and *cj2[2_i].value == true);
    static_assert(cj2[3_i].key == "D" and *cj2[3_i].value == false);
    static_assert(cj2[4_i].key == "E" and *cj2[4_i].value == "TEKINAS");
    static_assert(cj2[5_i].key == "F" and *cj2[5_i].value == nullptr);

    constexpr auto cj3 = static_json([] {
        return json::array{json::array{
                json::array{json::array{"TEKINAS", true, nullptr, 56565455122.1313, -664615376z, 11111119929121uz,
                                        json::object{{"MANA", true}, {"NIRVANA", 987654321.123456789}}}}}};
    });
    fmt::print("{}\n", *cj3[0_i][0_i][0_i][0_i]);
    fmt::print("{}\n", *cj3[0_i][0_i][0_i][1_i]);
    fmt::print("{}\n", *cj3[0_i][0_i][0_i][2_i]);
    fmt::print("{}\n", *cj3[0_i][0_i][0_i][3_i]);
    fmt::print("{}\n", *cj3[0_i][0_i][0_i][4_i]);
    fmt::print("{}\n", *cj3[0_i][0_i][0_i][5_i]);
    auto const obj = cj3[0_i][0_i][0_i][6_i];
    fmt::print("{} : {}\n", obj[0_i].key, *obj[0_i].value);
    fmt::print("{} : {}\n", obj[1_i].key, *obj[1_i].value);
}
