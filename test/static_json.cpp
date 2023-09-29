#include <concepts>
#include <static_data/static_json.hpp>

#include <cassert>
#include <fmt/format.h>
#include <utility>

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

template<tek::detail::fixed_string fmt_str>
[[gnu::noinline]] void printF(auto... args) {
    fmt::print(fmt_str.value(), args...);
}

template<tek::json::const_value json>
[[gnu::flatten]] void print_json(size_t level = 0) {
    constexpr size_t lsize = 2;
    if constexpr (json.kind == tek::json::kind::Object) {
        printF<"{{\n">();
        template_for<json.size()>([&]<size_t i> {
            if constexpr (i) printF<",\n">();
            printF<"{}{} : ">(times_ch<' '>(lsize * (level + 1)), json[tek::cv<i>].key);
            print_json<json[tek::cv<i>].value>(level + 1);
        });
        printF<"\n{}}}">(times_ch<' '>(lsize * level));
    } else if constexpr (json.kind == tek::json::kind::Array) {
        printF<"{{\n">();
        template_for<json.size()>([&]<size_t i> {
            if constexpr (i) printF<",\n">();
            printF<"{}">(times_ch<' '>(lsize * (level + 1)));
            print_json<json[tek::cv<i>]>(level + 1);
        });
        printF<"\n{}}}">(times_ch<' '>(lsize * level));
    } else
        overload{[](std::nullptr_t) { printF<"null">(); }, [](bool jb) { printF<"{}">(jb); },
                 [](int64_t ji) { printF<"{}">(ji); },     [](uint64_t ji) { printF<"{}">(ji); },
                 [](double jf) { printF<"{}">(jf); },      [](std::string_view js) { printF<"{}">(js); }}(*json);
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
    v8.visit(overload{[](auto &&) {}, [](array &a) { a.push_back("ELViS"); }});
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

constexpr auto get_json1() {
    using namespace tek::json;
    object obj;
    obj["PLANET"] = "Earth";
    obj["Sun"] = -90.133189f;
    obj["Gamma"] = -7625165189783168z;
    obj["Alpha"] = 444444712625712671uz;
    obj["Theta"] = true;
    obj["Omega"] = nullptr;
    obj["Data"] = {{1, true, false, "InFo", 0.21398681275}};
    obj["InFo"] = auto{obj};
    (*obj.find("Sun")).value = false;
    assert(obj.erase("Omega"));
    assert(obj.find("Omega") == obj.end());
    assert(not obj.erase("FFFF"));
    auto data_value = obj["Data"];
    obj.erase(obj.find("Data"));
    assert(obj.find("Data") == obj.end());
    obj.emplace("Text", array{{"UTF-8", nullptr}});
    {
        auto [itr, succ] = obj.emplace("Alpha", 45.13);
        assert(not succ);
        assert((*itr).key == "Alpha");
        assert((*itr).value == 444444712625712671uz);
    }
    {
        auto [itr, succ] = obj.emplace("Gott Mit Uns", true);
        assert(succ);
        assert((*itr).key == "Gott Mit Uns");
        assert((*itr).value == true);
    }
    obj["Theta"] = {{"TeKiNaS", true, nullptr, false, 88931.133, {{-21875, 781981uz}}}};
    obj["CHARS"] = {{'@', '!', '$', '#'}};
    obj["Text"].visit(overload{[](auto &&) {},
                               [](array &arr) {
                                   arr.insert(arr.end(), {true, false, nullptr, 612.9824f});
                               }});
    array arr1{(*obj.find("Alpha")).key,
               (*obj.find("Alpha")).value,
               (*obj.find("Text")).value,
               obj["CHARS"],
               obj["InFo"],
               data_value};
    for (auto kv : obj) arr1.push_back(kv.key), arr1.push_back(kv.value);
    obj["Array"] = std::move(arr1);
    obj.erase(std::as_const(obj).find("PLANET"));
    assert(obj.find("PLANET") == obj.end());
    obj.erase(obj.find("Gamma"));
    assert(obj.find("Gamma") == obj.end());
    obj["Gama"] = auto{obj["Theta"]};
    return obj;
}
}// namespace

int main() {
    using namespace tek;
    using namespace tek::json::literals;

    static_assert(std::regular<json::value> and std::regular<json::array> and std::regular<json::object>);

    constexpr auto cj0 = static_json([] { return get_json(); });
    static_assert(cj0.kind == json::kind::Object);
    print_json<cj0>(), printF<"\n">();

    printF<"{}\n">(*static_json([] { return nullptr; }));
    printF<"{}\n">(*static_json([] { return true; }));
    printF<"{}\n">(*static_json([] { return false; }));
    printF<"{}\n">(*static_json([] { return -1324348798961387z; }));
    printF<"{}\n">(*static_json([] { return 9918936136781383uz; }));
    printF<"{}\n">(*static_json([] { return 666699999.76451613; }));
    printF<"{}\n">(*static_json([] { return "TEKINAS IS GENIUS"; }));
    printF<"{}\n">(*static_json([] { return json::array{0, 1, true, "FILTH"}; })[3_i]);
    printF<"{}\n">(*static_json([] { return json::object{{"FILTH", "TRUE FILTH"}}; })["FILTH"_k]);
    printF<"{}\n">(*static_json([] { return json::object{{"KEY", "VALUE"}}; })[0_i].value);

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
    printF<"{}\n">(*cj3[0_i][0_i][0_i][0_i]);
    printF<"{}\n">(*cj3[0_i][0_i][0_i][1_i]);
    printF<"{}\n">(*cj3[0_i][0_i][0_i][2_i]);
    printF<"{}\n">(*cj3[0_i][0_i][0_i][3_i]);
    printF<"{}\n">(*cj3[0_i][0_i][0_i][4_i]);
    printF<"{}\n">(*cj3[0_i][0_i][0_i][5_i]);
    auto const obj = cj3[0_i][0_i][0_i][6_i];
    printF<"{} : {}\n">(obj[0_i].key, *obj[0_i].value);
    printF<"{} : {}\n">(obj[1_i].key, *obj[1_i].value);


    constexpr auto cj4 = static_json([] { return get_json1(); });
    print_json<cj4>(), printF<"\n">();
    static_assert(*cj4["CHARS"_k][0_i] == '@');
    static_assert(*cj4["CHARS"_k][1_i] == '!');
    static_assert(*cj4["CHARS"_k][2_i] == '$');
    static_assert(*cj4["CHARS"_k][3_i] == '#');
}
