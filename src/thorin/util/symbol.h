#ifndef ANYDSL2_SYMBOL_H
#define ANYDSL2_SYMBOL_H

#include <cstring>
#include <string>
#include <unordered_set>

#include "anydsl2/util/hash.h"

namespace anydsl2 {

struct StrHash { size_t operator () (const char* s) const; };
struct StrEqual { bool operator () (const char* s1, const char* s2) const { return std::strcmp(s1, s2) == 0; } };

class Symbol {
public:
    Symbol() { insert(""); }
    Symbol(const char* str) { insert(str); }
    Symbol(const std::string& str) { insert(str.c_str()); }

    const char* str() const { return str_; }
    bool operator == (Symbol symbol) const { return str() == symbol.str(); }
    bool operator != (Symbol symbol) const { return str() != symbol.str(); }

    static void destroy();

private:
    void insert(const char* str);

    const char* str_;
    typedef std::unordered_set<const char*, StrHash, StrEqual> Table;
    static Table table_;
};

inline std::ostream& operator << (std::ostream& o, Symbol s) { return o << s.str(); }

} // namespace anydsl2

namespace std {

template<>
struct hash<anydsl2::Symbol> {
    size_t operator () (anydsl2::Symbol symbol) const { return anydsl2::hash_value(symbol.str()); }
};

}

#endif