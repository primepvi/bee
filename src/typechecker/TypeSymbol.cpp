#include "bee/typechecker/TypeSymbol.hpp"

namespace bee::typechecker {

  VariableSymbol::VariableSymbol(std::string_view name, bool constant, bool lit, bool hasLitValue, Type type)
    : m_name(name), m_constant(constant), m_lit(lit), m_hasLitValue(hasLitValue), m_type(std::move(type)) {}

FunctionSymbol::FunctionSymbol(std::string_view name, std::size_t arity,
                               Type type)
    : m_name(name), m_arity(arity), m_type(std::move(type)) {}

} // namespace bee::typechecker
