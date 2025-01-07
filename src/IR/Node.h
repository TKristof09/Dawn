#pragma once

#include <cassert>
#include <deque>
#include <unordered_map>
#include <vector>
#include <print>

#include "Types.h"

enum class NodeType
{
#define NODE_OPCODE_DEFINE_ALL
#define NODE_OPCODE_DEFINE(opcode) opcode,
#include "NodeOpCodes.h"
#undef NODE_OPCODE_DEFINE_ALL
#undef NODE_OPCODE_DEFINE
    Unknown,
};

template<>
struct std::formatter<NodeType>
{
    constexpr auto parse(auto& ctx) { return ctx.begin(); }

    template<typename FormatContext>
    auto format(NodeType t, FormatContext& ctx) const
    {
        switch(t)
        {
#define NODE_OPCODE_DEFINE_ALL
#define NODE_OPCODE_DEFINE(opcode) \
    case NodeType::opcode:         \
        return std::format_to(ctx.out(), #opcode);
#include "NodeOpCodes.h"
#undef NODE_OPCODE_DEFINE_ALL
#undef NODE_OPCODE_DEFINE
        case NodeType::Unknown:
            return std::format_to(ctx.out(), "Unknown");
        }
    };
};


static uint32_t GetNextId()
{
    static uint32_t id = 0;
    return id++;
}
class Node
{
public:
    const Type& GetType() const { return m_valueType; }

    void Print()
    {
        if(m_nodeType == NodeType::Scope)
            return;

        if(m_visited)
            return;

        std::println("n{}[label=\"{}\"]", m_id, m_nodeType);
        m_visited = true;
        for(auto output : m_uses)
        {
            if(output->m_nodeType == NodeType::Scope)
                continue;
            std::println("n{} -> n{}", output->m_id, m_id);
        }

        for(auto output : m_uses)
            output->Print();
    }

    uint32_t GetId() const { return m_id; }

protected:
    Node(NodeType nodeType, Type valueType, uint32_t numInputs = 0) : m_nodeType(nodeType), m_valueType(valueType), m_inputs(numInputs) {}

    void SetInput(uint32_t index, Node* input)
    {
        m_inputs[index] = input;
        input->AddOutput(this);
    }
    void AddOutput(Node* output) { m_uses.push_back(output); }
    void AddInput(Node* input)
    {
        m_inputs.push_back(input);
        input->AddOutput(this);
    }

    Node* GetInput(uint32_t index) const { return m_inputs[index]; }
    uint32_t GetNumInputs() const { return m_inputs.size(); }


private:
    NodeType m_nodeType = NodeType::Unknown;
    Type m_valueType;
    std::vector<Node*> m_inputs;
    std::vector<Node*> m_uses;  // might be worth to use a set instead if we don't care about order
    uint32_t m_id = GetNextId();

    bool m_visited = false;  // TODO: temporary for printing
};
class ConstantNode : public Node
{
public:
    ConstantNode(Node* startNode, Type valueType) : Node(NodeType::Constant, valueType, 1)
    {
        SetInput(0, startNode);
    }
};

#define NODE_OPCODE_DEFINE_BIN
#define NODE_OPCODE_DEFINE(opcode)                                                         \
    class opcode##Node : public Node                                                       \
    {                                                                                      \
    public:                                                                                \
        opcode##Node(Node* left, Node* right) : Node(NodeType::opcode, left->GetType(), 2) \
        {                                                                                  \
            assert(left->GetType().Get() == right->GetType().Get());                       \
            SetInput(0, left);                                                             \
            SetInput(1, right);                                                            \
        }                                                                                  \
    };
#include "NodeOpCodes.h"
#undef NODE_OPCODE_DEFINE_BIN
#undef NODE_OPCODE_DEFINE


#define NODE_OPCODE_DEFINE_CMP
#define NODE_OPCODE_DEFINE(opcode)                                                   \
    class opcode##Node : public Node                                                 \
    {                                                                                \
    public:                                                                          \
        opcode##Node(Node* left, Node* right) : Node(NodeType::opcode, Type::Bot, 2) \
        {                                                                            \
            assert(left->GetType().Get() == right->GetType().Get());                 \
            SetInput(0, left);                                                       \
            SetInput(1, right);                                                      \
        }                                                                            \
    };
#include "NodeOpCodes.h"
#undef NODE_OPCODE_DEFINE_CMP
#undef NODE_OPCODE_DEFINE
//
class StartNode : public Node
{
public:
    StartNode() : Node(NodeType::Start, Type::Top) {}
};

class ReturnNode : public Node
{
public:
    ReturnNode(Node* control) : Node(NodeType::Return, Type::Top, 1) { SetInput(0, control); }
    ReturnNode(Node* control, Node* value) : Node(NodeType::Return, value->GetType(), 2)
    {
        SetInput(0, control);
        SetInput(1, value);
    }
};


class StopNode : public Node
{
public:
    StopNode() : Node(NodeType::Stop, Type::Top) {}
    using Node::AddInput;
};

class ScopeNode : public Node
{
public:
    ScopeNode() : Node(NodeType::Scope, Type::Top)
    {
        m_frames.push_front({});  // global frame
    }

    void PushFrame()
    {
        m_frames.push_front({});
    }

    void PushNode(std::string_view name, Node* node)
    {
        if(node == nullptr)
        {
            m_frames.front()[name] = -1;
        }
        else
        {
            m_frames.front()[name] = GetNumInputs();
            AddInput(node);
        }
    }

    Node* Find(std::string_view name)
    {
        for(auto& frame : m_frames)
        {
            if(auto node = frame.find(name); node != frame.end())
            {
                if(node->second == -1)
                {
                    std::println(stderr, "Use of uninitialized symbol `{}`", name);
                    exit(1);
                }
                return GetInput(node->second);
            }
        }

        std::println(stderr, "Use of undeclared symbol `{}`", name);
        exit(1);
    }

    void Update(std::string_view name, Node* node)
    {
        for(auto& frame : m_frames)
        {
            if(auto it = frame.find(name); it != frame.end())
            {
                if(it->second == -1)
                {
                    it->second = GetNumInputs();
                    AddInput(node);
                }
                else
                {
                    SetInput(it->second, node);
                }
                return;
            }
        }

        std::println(stderr, "Use of undeclared symbol `{}`", name);
        exit(1);
    }

    void PopFrame()
    {
        m_frames.pop_front();
        assert(!m_frames.empty() && "Popping global frame is not allowed!");
    }

    void Print()
    {
        std::println("s{}[label=\"ScopeNode\"]", GetId());
        int level = m_frames.size() - 1;
        for(auto& frame : m_frames)
        {
            for(auto& [name, index] : frame)
            {
                std::println("var{}[label=\"{}: {}\" shape=box]", index, level, name);
                std::println("s{} -> var{}", GetId(), index);
                if(index != -1)
                    std::println("var{} -> n{}[style=dashed]", index, GetInput(index)->GetId());
            }
            --level;
        }
    }

private:
    std::deque<std::unordered_map<std::string_view, int32_t>> m_frames;
};

class ProjectionNode : public Node
{
public:
    ProjectionNode(Node* input, uint32_t index) : Node(NodeType::Projection, input->GetType().Get(index), 1)
    {
        SetInput(0, input);
        m_index = index;
    }

private:
    uint32_t m_index;
};
