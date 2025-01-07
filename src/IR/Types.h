#pragma once

#include <cassert>
#include <cstdint>
#include <span>
#include <vector>

struct Type
{
    enum class Kind
    {
        Bot,      // Bottom (not a constant)
        Top,      // Top (can be constant or non-constant)
        Control,  // Control flow bottom
        Int,      // All integers
        Tuple,    // Tuple type
        Bool,
    };

    Type(Type::Kind kind) : m_kind(kind) {}

    virtual Type::Kind Get(uint32_t idx = 0) const { return Type::Kind::Bot; }
    virtual Type Meet(const Type& other) const
    {
        return Type::Bot;
    }

    static const Type Bot;
    static const Type Top;

protected:
    Type::Kind m_kind;
};

struct ControlType : public Type
{
    Type::Kind Get(uint32_t idx) const override { return Type::Kind::Control; }
};

struct IntType : public Type
{
    static const IntType Top;
    static const IntType Bot;

    // Constructor for constant integers
    IntType(uint64_t value) : Type(Type::Kind::Int), m_value(value), m_isConstant(true) {}

    Type::Kind Get(uint32_t idx = 0) const override { return Type::Kind::Int; }
    Type Meet(const Type& other) const override
    {
        if(other.Get(0) == Type::Kind::Int)
        {
            const auto& otherInt = static_cast<const IntType&>(other);

            if(IsBot())
                return *this;
            if(otherInt.IsBot())
                return otherInt;

            if(otherInt.IsTop())
                return *this;
            if(IsTop())
                return otherInt;

            // Both are constants, if they are unrelated return int bottom
            if(m_value == otherInt.m_value)
                return *this;
            return IntType::Bot;
        }

        return Type::Meet(other);
    }

    bool IsTop() const
    {
        return m_isConstant && m_value == 0;
    }
    bool IsBot() const { return m_isConstant && m_value == 1; }

private:
    // Constructor for non-constant integers
    IntType(bool isTop) : Type(Type::Kind::Int), m_value(isTop ? 0 : 1), m_isConstant(false) {}

    uint64_t m_value;  // For now we only support 64-bit integers, but in the future we'll probably store all as 64bit and just have a flag for the actual size
    bool m_isConstant;
};


struct TupleType : public Type
{
    TupleType(const std::vector<Type>& types) : Type(Type::Kind::Tuple), m_types(types) {}
    Type::Kind Get(uint32_t idx = 0) const override { return m_types[idx].Get(); }
    Type Meet(const Type& other) const override
    {
        assert(false);  // Not implemented
        if(other.Get(0) == Type::Kind::Tuple)
            return *this;
        return Type::Meet(other);
    }

private:
    std::vector<Type> m_types;
};
