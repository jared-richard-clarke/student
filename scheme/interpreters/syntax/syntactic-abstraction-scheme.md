# Syntactic Abstraction in Scheme

by R. Kent Dybvig, Robert Hieb, and Carl Bruggeman

## About

The argument to a macro transformer is a **syntax object**. A syntax object
contains contextual information about an expression in addition to its structure.
This contextual information is used by the expander to maintain hygiene and
referential transparency.

Traditional Lisp macro systems user ordinary list-structured data to represent
syntax. Although these list structures are easy to manipulate, syntactic
information is lost -- like the distinction between different identifiers
that share the same name. Syntax objects allow transformers to compare identifiers
according to their intended use as free identifiers, bound identifiers, or symbolic
data.

Transformers decompose their input using **syntax-case** and rebuild their output
using **syntax**.

A **syntax** form returns a Scheme object in much the same way as **quote** and
**quasiquote**. However, the values of pattern variables appearing within template
are inserted into template, and contextual syntactic information within the template
is retained.

A pattern datum is any non-list, non-symbol datum.

## Hygienic Macros

Hygienic macros prevent free identifiers inserted by macro application from
being captured by program bindings. They also prevent bindings introduced by
macros from capturing free identifiers.

In the λ-calculus, alpha conversion is used to circumvent hygiene problems
caused by program transformations.

## Macro Calls

```text
(keyword subform ...)
```

## Macro Definition and Binding

```text
(define-syntax keyword transformer-expression)

(let-syntax    ([keyword transformer-expression] ...) body)

(letrec-syntax ([keyword transformer-expression] ...) body)
```

## Destructuring and Restructuring

```text
(syntax-case input-expression (literal? ...)
  (pattern fender? expression)
  ...)

(syntax template)

(syntax-object->datum syntax-object)

(datum->syntax-object identifier datum)
```

## Predicates

```text
(identifier? object)

(free-identifier=?  identifier-1 identifier-2)

(bound-identifier=? identifier-1 identifier-2)
```

## Syntax Rules

```scheme
(define-syntax syntax-rules
  (lambda (x)
    (syntax-case x ()
      [(_ (i ...) ((keyword . pattern) template) ...)
       (syntax (lambda (x)
                 (syntax-case x (i ...)
                   ((dummy . pattern) (syntax template))
                   ...)))])))
```

## With Syntax

```scheme
(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      [(_ ([p e0] ...) e1 e2 ...)
       (syntax (syntax-case (list e0 ...) ()
                 [(p ...) (begin e1 e2 ...)]))])))
```

## Traditional Macro Expansion Algorithm

Pattern matching is used to hide the details of accessing the expression parts.

```text
Abstract Data Type: Exp (expression)

   Sym ⊂ Exp
 Const ⊂ Exp
cons   : Exp x Exp -> Exp
car    : Exp -> Exp
cdr    : Exp -> Exp
pair?  : Exp -> Bool
sym?   : Exp -> Bool

expand : Exp x Env -> ExpandedExp

expand(e, r) =
    case parse(e, r) of:
                    constant(c) -> symbolic-data(c),
	            variable(s) -> variable(s),
	    application(e1, e2) -> application(expand(e1, r), expand(e2, r))
	       symbolic-data(e) -> symbolic-dat(e),
	         function(s, e) -> function(s, expand(e, r[s := Variable])),
	macro-application(s, e) -> expand(t(e), r) where t = r(s)

parse : Exp x Env -> ParsedExp

           parse([c], r) = constant(c)
           parse([s], r) = variable(s) if r(s) = Variable
     parse([(e1 e2), r]) = application(e1, e2) if e1 (not ∈) Sym
       parse([(s e)], r) = application(s, e) if r(s) = Variable
       parse([(s e)], r) = macro-application(s, e) if r(s) ∈ Transformer
   parse([(quote e)], r) = symbolic-data(e) if r([quote]) = Special
parse([(lambda s e)], r) = function(s, e) if r([lambda]) = Special
```

## A Substitution-Based Macro-Expansion Algorithm

```text
expand : Exp x Env -> ExpandExp

expand(e, r) =
    case parse(e, r) of:
                          variable(i) -> variable(resolve(i)),
  	          application(e1, e2) -> application(expand(e1, r), expand(e2, r)),
	             symbolic-data(e) -> symbolic-data(strip(e)),
	               syntax-data(e) -> symbolic-data(e),
	               function(i, e) -> function(s, expand(subst(e, i, s), r'))
	                                 where r' = r[s := Variable] and s is fresh,
                      pfunction(i, e) -> function(s, expand(subst(e, i, s), r'))
	                                 where r' = r[s := PVariable] and s is fresh,
	      macro-application(i, e) -> expand(mark(t(mark(e, m)), m), r)
	                                 where t = r(resolve(i)) and m is fresh,
	    syntax-binding(i, e1, e2) -> expand(subst(e2, i, s), r[s := t])
	                                 where t = eval(expand(e1, r)) and s is fresh,
	rec-syntax-binding(i, e1, e2) -> expand(subst(e2, i, s), r[s := t])
	                                 where t = eval(expand(subst(e1, i, s), r))
					     and s is fresh
```
