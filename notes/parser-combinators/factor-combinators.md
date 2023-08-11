# Parser Combinators in Factor

Original code by Chris Double

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
