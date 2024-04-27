#pragma once
#include <format>
#include <variant>
#include <vector>
#include <stack>
#include <string_view>
#include <unordered_map>

#include "Location.h"

namespace Types
{
struct NoneType;
struct Int;
struct String;
struct Bool;
struct Array;
struct Struct;
struct Function;

using Type = std::variant<NoneType, Int, String, Bool, Array, Struct, Function>;

struct NoneType
{
};
struct Int
{
};
struct String
{
};
struct Bool
{
};
struct Array
{
    std::vector<Type> type;  // TODO
};
struct Struct
{
    std::vector<Type> types;
};
struct Function
{
    std::vector<Type> parameters;
    std::vector<Type> returnType;  // TODO
};
};  // namespace Types

using Type = Types::Type;


class TypeStack
{
public:
    // cutoff will be used for functions, as they shouldn't be able to access the outer scope i think, to be seen for closures and stuff
    void PushFrame(bool cutoff = false)
    {
        size_t offset = m_frames.empty() ? m_globalFrame.EndOffset() : m_frames.front().EndOffset();
        m_frames.push_front({cutoff, cutoff ? 0 : offset});
    }

    void Push(const std::string& name, Type t)
    {
        return m_frames.empty() ? m_globalFrame.Push(name, t) : m_frames.front().Push(name, t);
    }

    Type Find(const std::string& name, Location loc)
    {
        for(auto& frame : m_frames)
        {
            auto t = frame.Find(name);
            if(t)
                return t.value();
            if(frame.IsCutoff())
                break;
        }

        auto var = m_globalFrame.Find(name);
        if(var)
            return var.value();

        std::println(stderr, "{}: Use of undeclared symbol `{}`", loc, name);
        exit(1);
    }

    void PopFrame()
    {
        m_frames.pop_front();
    }

private:
    class StackFrame
    {
    public:
        StackFrame(bool cutoff, uint64_t offset) : m_currentOffset(offset), m_isCutoff(cutoff) {}
        void Push(const std::string& name, Type t)
        {
            m_types[name] = t;
        }
        std::optional<Type> Find(const std::string& name)
        {
            auto it = m_types.find(name);
            if(it == m_types.end())
                return std::nullopt;
            return it->second;
        }

        uint64_t EndOffset() const
        {
            return m_currentOffset;
        }
        bool IsCutoff() const
        {
            return m_isCutoff;
        }

    private:
        std::unordered_map<std::string, Type> m_types;
        uint64_t m_currentOffset = 0;
        bool m_isCutoff          = false;
    };

    std::deque<StackFrame> m_frames;
    StackFrame m_globalFrame{true, 0};  // TODO: figure out how I want to deal with globals
};


struct Printer
{
    constexpr std::string operator()(const Types::NoneType&) const
    {
        return "NoneType";
    }
    constexpr std::string operator()(const Types::Int&) const
    {
        return "int";
    }
    constexpr std::string operator()(const Types::String&) const
    {
        return "string";
    }
    constexpr std::string operator()(const Types::Bool&) const
    {
        return "bool";
    }
    constexpr std::string operator()(const Types::Array& a) const
    {
        return "Array {" + std::visit(*this, a.type[0]) + "}";
    }
    constexpr std::string operator()(const Types::Struct& s) const
    {
        std::string result = "Struct {";
        for(auto& type : s.types)
            result += std::visit(*this, type) + ", ";
        result.pop_back();
        result.pop_back();
        result += "}";
        return result;
    }
    constexpr std::string operator()(const Types::Function& f) const
    {
        std::string result = "Function {";
        for(auto& type : f.parameters)
            result += std::visit(*this, type) + ", ";
        result.pop_back();
        result.pop_back();
        result += "} -> {";
        result += std::visit(*this, f.returnType[0]);
        result += "}";
        return result;
    }
};

template<>
struct std::formatter<Type>
{
    constexpr auto parse(std::format_parse_context& ctx)
    {
        return ctx.begin();
    }

    template<typename FormatContext>
    constexpr auto format(const Type& t, FormatContext& ctx) const
    {
        return std::format_to(ctx.out(), "{}", std::visit(Printer{}, t));
    }
};
