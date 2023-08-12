# Parser Combinators in Factor

Original code by Chris Double

## Simple Parsers

```factor
USING: kernel math.parser parser-combinators strings unicode ;
IN: parser-combinators.simple

: digit-parser ( -- parser )
  [ digit? ] satisfy [ digit> ] <@ ;

: integer-parser ( -- parser )
  [ digit? ] satisfy <*> [ string>number ] <@ ;

: string-parser ( -- parser )
  [ CHAR: \" = ] satisfy
  [ CHAR: \" = not ] satisfy <*> &>
  [ CHAR: \" = ] satisfy <& [ >string ] <@  ;

: bold-parser ( -- parser )
  "*" token
  [ CHAR: * = not  ] satisfy <*> [ >string ] <@ &>
  "*" token <& ;

: italic-parser ( -- parser )
  "_" token
  [ CHAR: _ = not ] satisfy <*> [ >string ] <@ &>
  "_" token <& ;

: comma-list ( element -- parser )
  "," token list-of ;
```

## Arithmetic Evaluator

```factor
: operator ( -- parser )
  "/" token 
  "*" token <|>
  "+" token <|>
  "-" token <|>
  [ "\\ " swap cat2 eval unit ] <@ ;

: expression ( -- parser )
  natural-parser 
  operator sp       <&>  
  natural-parser sp <&> 
  [ uncons swap uncons -rot append append reverse call ] <@ ;

"40+2" expression call lcar . ! => [[ "" 42 ]]
```
