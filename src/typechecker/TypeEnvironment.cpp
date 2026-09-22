#include "bee/typechecker/TypeEnvironment.hpp"
#include "bee/typechecker/TypeSymbol.hpp"
#include <memory>
#include <stdexcept>

namespace bee::typechecker {

TypeEnvironment::TypeEnvironment(TypeScopeKind scopeKind,
                                 std::shared_ptr<TypeEnvironment> parent)
    : m_scopeKind(scopeKind), m_parent(std::move(parent)) {}

bool TypeEnvironment::hasSymbol(std::string_view key) const {
  if (m_symbols.contains(key))
    return true;

  return m_parent == nullptr ? false : m_parent->hasSymbol(key);
}

bool TypeEnvironment::scopeHasSymbol(std::string_view key) const {
  return m_symbols.contains(key);
}

std::shared_ptr<TypeSymbol>
TypeEnvironment::getSymbol(std::string_view key) const {
  if (m_symbols.contains(key))
    return m_symbols.at(key);

  if (m_parent == nullptr)
    throw std::runtime_error(
        "error: attempt to get an invalid key in type environment.");

  return m_parent->getSymbol(key);
}

std::shared_ptr<TypeSymbol>
TypeEnvironment::scopeGetSymbol(std::string_view key) const {
  if (!m_symbols.contains(key))
    throw std::runtime_error(
        "error: attempt to get an invalid key in type environment.");

  return m_symbols.at(key);
}

  void TypeEnvironment::putSymbol(std::unique_ptr<TypeSymbol> symbol) {
  m_symbols.insert_or_assign(symbol->name(), std::move(symbol));
}

} // namespace bee::typechecker
