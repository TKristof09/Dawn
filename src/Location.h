#pragma once
#include <cstdint>
#include <print>

struct Location
{
    std::string_view filename;
    size_t line;
    size_t col;
};

template<>
struct std::formatter<Location>
{
    constexpr auto parse(std::format_parse_context& ctx)
    {
        return ctx.begin();
    }

    template<typename FormatContext>
    auto format(Location loc, FormatContext& ctx) const
    {
        return std::format_to(ctx.out(), "{}:{}:{}", loc.filename, loc.line, loc.col);
    }
};
