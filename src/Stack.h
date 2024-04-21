#pragma once

#include "Location.h"
#include <print>
#include <stack>
#include <string_view>
#include <unordered_map>
#include <vector>

struct Variable
{
    std::string_view name;
    uint64_t size;
    uint64_t baseSize;

    uint64_t baseOffset = 0;  // offset from rbp, set by the stack

    Variable(std::string_view name, uint64_t size, uint64_t baseSize) : name(name), size(size), baseSize(baseSize) {}
};


class Stack
{
public:
    // cutoff will be used for functions, as they shouldn't be able to access the outer scope i think, to be seen for closures and stuff
    void PushFrame(bool cutoff = false)
    {
        m_frames.push_back({cutoff || m_frames.empty() ? 0 : m_frames.back().EndOffset()});
    }
    Variable PushVariable(Variable var)
    {
        return m_frames.back().Push(var);
    }

    Variable Find(std::string_view name, Location loc)
    {
        for(auto& frame : m_frames)
        {
            auto var = frame.Find(name);
            if(var)
                return var.value();
        }

        std::println(stderr, "{}: Use of undeclared symbol `{}`", loc, name);
        exit(1);
    }

    void PopFrame()
    {
        m_frames.pop_back();
    }

private:
    class StackFrame
    {
    public:
        StackFrame(uint64_t offset) : m_currentOffset(offset) {}
        Variable Push(Variable var)
        {
            m_currentOffset += var.size;
            var.baseOffset   = m_currentOffset;
            m_variables.push_back(var);
            m_lookup[var.name] = m_variables.size() - 1;
            return var;
        }
        std::optional<Variable> Find(std::string_view name)
        {
            auto it = m_lookup.find(name);
            if(it == m_lookup.end())
                return std::nullopt;
            return m_variables[it->second];
        }

        uint64_t EndOffset() const
        {
            return m_currentOffset;
        }

    private:
        std::unordered_map<std::string_view, uint32_t> m_lookup;  // the ast owns the variable name strings, so view is safe
        std::vector<Variable> m_variables;
        uint64_t m_currentOffset = 0;
    };
    std::deque<StackFrame> m_frames;
};
