# Combinator Parsers

`.>>.` (`andThen`): applies two parsers in sequence and returns a tuple.

`<|>` (`orElse`): applies the first parser, and if that fails, applies the second parser.

`choice`: extends `<|>` to choose from a list of parsers.

`bind`: chains the result of a parser to another parser-producing function.

`map`: transforms the value inside a parser.

`return`: transforms a value into a parser.

`apply`: applies a parser containing a function to a parser containing a value.

`lift2`: transforms two-parameter functions into functions that work on parsers.

`sequence`: converts a list of parsers into a parser containing a list.

`many`: matches zero or more specified parsers.

`many1`: matches one or more specified parsers.

`opt`: matches an optional specified parser.

`.>>`: matches two values, keeps the first.

`>>.`: matches two values, keeps the second.

`between`: matches three values, keeps the second.

`sep`: parses zero or more parsers with a separator.

`sep1`: parses one or more parsers with a separator.
