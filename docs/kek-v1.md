# Kek v1 Language Specification (Frozen)

Status: Frozen
Version: `1.0.1`
Frozen on: `2026-02-11`
Last amended: `2026-02-12`

This document is the canonical grammar and compatibility contract for Kek v1.
If there is any conflict between implementation and this document, this document wins.

## 1. Scope
Kek v1 is a statically typed, imperative language with modules and enough expressiveness to implement a compiler frontend.

## 2. Canonical Syntax Rules
- Statements terminate with `;`
- Blocks use `{ ... }`
- Canonical function params use `name: Type`
- Canonical function return types use `-> Type`
- Canonical struct method placement is inline inside `struct { ... }`
- Separate `impl Type { ... }` blocks are still accepted in v1 for compatibility

Examples:
```kek
fun parse(input: String) -> Num {
    return 1;
}
```

## 3. Compatibility Rules (Frozen)
Compatibility rules below are part of the v1 freeze and are intentionally explicit.

### 3.1 Legacy syntax accepted during compatibility window
- Legacy parameter syntax: `Type name`
- Legacy function return syntax: `fun x(...): Type { ... }`

Compatibility window:
- Start date: `2026-02-11`
- End date: `2026-09-30`

Behavior during compatibility window:
- Legacy syntax is accepted.
- Compiler should emit warning diagnostics and suggest canonical replacements.

Behavior after `2026-09-30`:
- Legacy syntax moves behind an explicit compatibility flag (planned: `--compat v0`).
- Canonical syntax remains default.

## 4. Lexicon Source of Truth
The reserved tokens for Kek v1 are defined in:

- `docs/lexicon-v1.toml`

That file is normative for:
- Keywords
- Operators
- Punctuation
- Tokenization priority for multi-character operators
- Literal and identifier regex contracts

## 5. Grammar (EBNF)

```ebnf
program      = { item } EOF ;

item         = import_decl
             | const_decl
             | var_decl
             | fun_decl
             | struct_decl
             | enum_decl
             | impl_decl
             | class_decl ;

import_decl  = "import" IDENT [ "as" IDENT ] "from" STRING ";" ;

const_decl   = [ "pub" ] "const" IDENT ":" type "=" expr ";" ;
var_decl     = "var" IDENT ":" type [ "=" expr ] ";" ;

fun_decl     = [ "pub" ] "fun" IDENT "(" [ params ] ")" [ "->" type ] block ;
params       = param { "," param } ;
param        = IDENT ":" type ;
# compatibility mode MAY accept legacy `Type name` params

struct_decl  = [ "pub" ] "struct" IDENT "{" { struct_member } "}" ;
struct_member= field_decl | method_decl ;
field_decl   = IDENT ":" type ";" ;
method_decl  = [ "pub" ] "fun" IDENT "(" [ params ] ")" [ "->" type ] block ;

enum_decl    = [ "pub" ] "enum" IDENT "{" enum_variant { "," enum_variant } [ "," ] "}" ;
enum_variant = IDENT [ "(" type_list ")" ] ;
type_list    = type { "," type } ;

impl_decl    = [ "pub" ] "impl" IDENT "{" { method_decl } "}" ;

class_decl   = [ "pub" ] "class" IDENT "{" { var_decl | fun_decl } "}" ;

block        = "{" { stmt } "}" ;

stmt         = var_decl
             | const_decl
             | if_stmt
             | while_stmt
             | for_stmt
             | match_stmt
             | return_stmt
             | break_stmt
             | continue_stmt
             | expr_stmt
             | block ;

if_stmt      = "if" expr block [ "else" ( if_stmt | block ) ] ;
while_stmt   = "while" expr block ;
for_stmt     = "for" IDENT [ "," IDENT ] "in" expr block ;
match_stmt   = "match" expr "{" { match_arm } "}" ;
match_arm    = pattern "=>" ( block | expr ";" ) ;
pattern      = "_"
             | literal
             | IDENT
             | IDENT "(" [ pattern { "," pattern } ] ")" ;

return_stmt  = "return" [ expr ] ";" ;
break_stmt   = "break" ";" ;
continue_stmt= "continue" ";" ;
expr_stmt    = expr ";" ;

expr         = assign ;
assign       = logic_or [ assign_op assign ] ;
assign_op    = "=" | "+=" | "-=" | "*=" | "/=" | "%=" ;

logic_or     = logic_and { "||" logic_and } ;
logic_and    = bit_or { "&&" bit_or } ;
bit_or       = bit_xor { "|" bit_xor } ;
bit_xor      = bit_and { "^" bit_and } ;
bit_and      = equality { "&" equality } ;
equality     = compare { ( "==" | "!=" ) compare } ;
compare      = shift { ( "<" | "<=" | ">" | ">=" ) shift } ;
shift        = term { ( "<<" | ">>" ) term } ;
term         = factor { ( "+" | "-" ) factor } ;
factor       = unary { ( "*" | "/" | "%" ) unary } ;
unary        = ( "!" | "-" ) unary | postfix ;
postfix      = primary { call | member | index | try_op } ;
call         = "(" [ args ] ")" ;
args         = expr { "," expr } ;
member       = "." IDENT ;
index        = "[" expr "]" ;
try_op       = "?" ;

primary      = literal
             | IDENT
             | "this"
             | "None"
             | "(" expr ")"
             | array_lit ;

array_lit    = "[" [ expr { "," expr } ] "]" ;

type         = base_type { "[]" } ;
base_type    = "Num" | "Bool" | "String" | "Char" | "Byte" | "Void"
             | IDENT
             | IDENT "<" type { "," type } ">" ;
```

## 6. Frozen Semantics
- Conditions must be `Bool`
- `&&` and `||` short-circuit
- `for item, index in expr` binds element + zero-based index
- `Array<T>` is a built-in generic container type; `T[]` is compatibility syntax for the same runtime type
- `String` is represented as `Array<Char>` at runtime and supports the same index/iteration model
- Array indexing requires numeric index expressions
- Built-in array methods are available on both `Array<T>` and `T[]`: `len()`, `is_empty()`, `push(T)`, `pop()`
- `return;` is legal only in functions returning `Void` (or compatibility mode if not yet type-checked)
- `this` is valid inside class methods, struct methods, and `impl` methods

## 7. Reserved-Word Policy
All entries listed in `docs/lexicon-v1.toml` under `keywords` are reserved in v1 and cannot be used as identifiers.

## 8. Change Control
Any change to this file or `docs/lexicon-v1.toml` is a language change and must:
1. bump version in both files,
2. include migration notes,
3. include parser/lexer test updates.
