#pragma once

#include "Location.h"
#include <print>
#include <stack>
#include <string_view>
#include <unordered_map>
#include <vector>

struct Variable
{
    std::string name;
    uint64_t size;
    uint64_t baseSize;

    uint64_t baseOffset = 0;  // offset from rbp, set by the stack

    Variable(std::string name, uint64_t size, uint64_t baseSize) : name(name), size(size), baseSize(baseSize) {}
};


class Stack
{
public:
    // cutoff will be used for functions, as they shouldn't be able to access the outer scope i think, to be seen for closures and stuff
    void PushFrame(bool cutoff = false)
    {
        size_t offset = m_frames.empty() ? m_globalFrame.EndOffset() : m_frames.front().EndOffset();
        m_frames.push_front({cutoff, cutoff ? 0 : offset});
    }

    Variable PushVariable(Variable var)
    {
        return m_frames.empty() ? m_globalFrame.Push(var) : m_frames.front().Push(var);
    }

    Variable Find(const std::string& name, Location loc)
    {
        for(auto& frame : m_frames)
        {
            auto var = frame.Find(name);
            if(var)
                return var.value();
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
        Variable Push(Variable var)
        {
            m_currentOffset += var.size;
            var.baseOffset   = m_currentOffset;
            m_variables.push_back(var);
            m_lookup[var.name] = m_variables.size() - 1;
            return var;
        }
        std::optional<Variable> Find(const std::string& name)
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
        bool IsCutoff() const
        {
            return m_isCutoff;
        }

    private:
        std::unordered_map<std::string, uint32_t> m_lookup;  // the ast owns the variable name strings, so view is safe
        std::vector<Variable> m_variables;
        uint64_t m_currentOffset = 0;
        bool m_isCutoff          = false;
    };

    std::deque<StackFrame> m_frames;
    StackFrame m_globalFrame{true, 0};  // TODO: figure out how I want to deal with globals
};
