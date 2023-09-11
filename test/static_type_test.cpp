#include <static_data/static_data.hpp>

#include <fmt/compile.h>
#include <fmt/format.h>
#include <string_view>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

namespace vws = std::views;

template<class T>
constexpr auto type_name() {
    static constexpr std::string_view pf = __PRETTY_FUNCTION__;
    return std::string_view{tek::static_data([] {
#ifdef __clang__
        return pf.substr(22, pf.size() - 22 - 1);
#elif defined(__GNUC__)
        return pf.substr(37, pf.size() - 37 - 1);
#endif
    })};
}

class Vec : public tek::recursive_base {
public:
    constexpr Vec(double val);
    constexpr Vec(double val, size_t n);
    constexpr auto to_static_data() const { return std::pair{value, rng}; }

private:
    double value;
    std::vector<Vec> rng;
};

template<typename T>
void static_type_of() {
    fmt::print(FMT_COMPILE("{} => {}\n"), type_name<T>(), type_name<tek::static_data_t<T>>());
}

int main() {
    static_type_of<std::unique_ptr<std::string>>();
    static_type_of<std::vector<std::string>>();
    static_type_of<decltype(vws::iota(0, 10))>();
    static_type_of<std::vector<Vec>>();
    static_type_of<std::optional<std::tuple<double, std::string>>>();
    static_type_of<std::vector<std::vector<std::tuple<double, std::string>>>>();
}
