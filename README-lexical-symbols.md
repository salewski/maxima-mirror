## Implementation of lexical symbols in Maxima
### Robert Dodier
### February 2024

To get an overview, take a look at:
```
demo ("lexical_symbols.demo");
```

To see the test cases, try:
```
run_testsuite (tests = rtest_lexical_symbols, display_all = true);
```

### Outline
 
 LEXICAL SYMBOLS

 * A lexical symbol is a symbol defined in a lexical extent.
 * A lexical extent is one of the following operators and its arguments:
     * `block`
     * `lambda`
     * named function
     * array function (both `f[x]` and `f[x](y)`)
     * `for` loop
     * macro (defined by `::=`)
 * Lexical symbols defined in different lexical extents are distinct, even if they have the same name.
 * Every symbol which is not a lexical symbol is a nonlexical symbol.
     * There is a unique nonlexical symbol for any given name.
     * Dynamic binding is applied to nonlexical symbols.
 * In addition, a symbol may be declared nonlexical so that it is a nonlexical symbol
   even if it is defined in a lexical extent.
     * A nonlexical declaration applies to any lexical extents parsed after the declaration is evaluated.
     * `declare(x, nonlexical)` declares the symbol `x` as nonlexical,
     and `remove(x, nonlexical)` removes the declaration.

 IMPLEMENTATION

 * Lexical symbols are created by the parser
 * Lexical extents are created with nonlexical symbols, then lexical symbols are substituted as needed
     * Since quote-quote is applied by the parser, lexical symbol substitution is applied
       to an expression containing a quote-quote substitution
     * Since evaluation occurs after the parser, lexical symbol substitution is not applied
       to expressions created by evaluation, e.g. `define(f(x), x)`, `funmake(lambda, [[x], x])`.
 * Lexical symbol substitution is not applied to symbols declared nonlexical
     * Whether or not a symbol has been declared nonlexical is determined by the declarations
       in effect at the time an expression is parsed;
       a nonlexical declaration has no effect on expressions which have already been parsed
 * Lexical symbols are displayed with the name stated when an expression is read,
   e.g. the lexical symbol in `f(x) := x` is displayed as `x`
 * Lexical symbols are implemented as uninterned Lisp symbols which have names
   constructed from the display name and a number (i.e., a Lisp gensym)

### NOT IMPLEMENTED YET BUT UNDER CONSIDERATION

 * Apply lexical symbol substitution to expressions created by evaluation?
   e.g. `define(f(x), x)`, `funmake(lambda, [[x], x])`
 * Additional lexical extents?
     * `buildq`
     * `makelist` / `create_list`/ `makeset` 
     * `sum`
     * `integrate`
     * `diff`
 * Lexical environments, a.k.a. closures
     * There exists a proof of concept implementation at: https://github.com/maxima-packages/robert-dodier/lexical\_symbols
