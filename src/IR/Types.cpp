#include "Types.h"


const Type Type::Bot = Type(Type::Kind::Bot);
const Type Type::Top = Type(Type::Kind::Top);

const IntType IntType::Top = IntType(true);
const IntType IntType::Bot = IntType(false);
