/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

// FIXME: the groupdescs for duplicate groups need reworking, or the groups split
/**
  *
  * @since 5.0.0
  *
  * @groupprio cond 25
  * @groupname cond Conditional Combinators
  * @groupdesc cond
  *     These combinators will decide which branch to take next based on the result of another parser.
  *     This differs from combinators like `<|>` which make decisions based on the success/failure of
  *     a parser: here the result of a ''successful'' parse will direct which option is done. These
  *     are sometimes known as "selective" combinators.
  *
  * @groupprio prim 0
  * @groupname prim Primitive Combinators
  * @groupdesc prim
  *     These combinators are specific to parser combinators. In one way or another, they influence how a
  *     parser consumes input, or under what conditions a parser does or does not fail. These are really
  *     important for most practical parsing considerations, although `lookAhead` is much less well used.
  *
  * @groupprio basic 5
  * @groupname basic Consumptionless Parsers
  * @groupdesc basic
  *     These combinators and parsers do not consume input: they are the most primitive ways of producing
  *     successes and failures with the minimal possible effect on the parse. They are, however, reasonably
  *     useful; in particular, `pure` and `unit` can be put to good use in injecting results into a parser
  *     without needing to consume anything, or mapping another parser.
  *
  * @groupprio iter 10
  * @groupname iter Iterative Combinators
  * @groupdesc iter
  *     These combinators all execute a given parser an unbounded number of times, until either it fails, or another
  *     parser succeeds, depending on the combinator. All of the results produced by the
  *     repeated execution of the parser are returned in a `List`. These are almost essential for any practical parsing
  *     task.
  *
  * @groupprio item 15
  * @groupname item Input Query Combinators
  * @groupdesc item
  *     These combinators do not consume input, but they allow for querying of the input stream - specifically checking
  *     whether or not there is more input that can be consumed or not. In particular, most parsers should be making
  *     use of `eof` to ensure that the parser consumes all the input available at the end of the parse.
  *
  * @groupprio iter 0
  * @groupname iter Iterative Combinators
  * @groupdesc iter
  *     These combinators all execute a given parser an unbounded number of times, until either it fails, or another
  *     parser succeeds, depending on the combinator. Depending on the combinator, all of the results produced by the
  *     repeated execution of the parser may be returned in a `List`. These are almost essential for any practical parsing
  *     task.
  *
  * @groupprio item 10
  * @groupname item Input Query Combinators
  * @groupdesc item
  *     These combinators do not consume input, but they allow for querying of the input stream - specifically checking
  *     whether or not there is more input that can be consumed or not. In particular, most parsers should be making
  *     use of `eof` to ensure that the parser consumes all the input available at the end of the parse.
  *
  * @groupprio opt 20
  * @groupname opt Optional Parsing Combinators
  * @groupdesc opt
  *     These combinators allow for the ''possible'' parsing of some parser. If the parser succeeds, that is ok
  *     so long as it '''did not consume input'''. Be aware that the result of the success may be replaced with
  *     these combinators, with the exception of [[option `option`]], which still preserves the result.
  *
  * @groupprio sep 25
  * @groupname sep Separated Values Combinators
  * @groupdesc sep
  *     These combinators are concerned with delimited parsing, where one parser is repeated but delimited by another one.
  *     In each of these cases `p` is the parser of interest and `sep` is the delimeter. These combinators mainly differ
  *     in either the number of `p`s they require, or exactly where the delimeters are allowed (only between, always
  *     trailing, or either). In all cases, they return the list of results generated by the repeated parses of `p`.
  *
  * @groupprio multi 50
  * @groupname multi Multiple Branching/Sequencing Combinators
  * @groupdesc multi
  *     These combinators allow for testing or sequencing a large number of parsers in one go. Be careful, however, these are
  *     variadic combinators and are necessarily (for compatibility with Scala 2) '''not lazy'''.
  *
  *     In such a case where laziness is desired without resorting to the other lazier combinators, there
  *     is a neat trick: unroll the first iteration of the combinator, and use the corresponding regular combinator
  *     to do that (i.e. `<::>` or `*>`): since these will have a lazy
  *     right-hand side, the remaining variadic arguments will be kept lazily suspended until later. Alternatively,
  *     it is possible to use the [[parsley.Parsley.LazyParsley.unary_~ prefix `~`]] combinator to make any individual
  *     arguments lazy as required, for example `skip(p, ~q, r)`.
  *
  * @groupprio range 65
  * @groupname range Range Combinators
  * @groupdesc range
  *     These combinators allow for the parsing of a specific parser either a specific number of times, or between a certain
  *     amount of times.
  *
  * @groupprio cond 75
  * @groupname cond Conditional Combinators
  * @groupdesc cond
  *     These combinators allow for the conditional extraction of a result, or the execution of a parser
  *     based on another. They are morally related to [[Parsley.branch `branch`]] and [[Parsley.select `select`]] but are
  *     less fundamental.
  *
  * @groupprio pred 100
  * @groupname pred Character Predicates
  * @groupdesc pred
  *     These are useful for providing to the sub-descriptions of a [[token.descriptions.LexicalDesc]] to specify behaviour for the lexer.
  *     Other than that, they aren't ''particularly'' useful.
  *
  * @groupprio core 0
  * @groupname core Core Combinators and Parsers
  * @groupdesc core
  *     These are the most primitive combinators for consuming input capable of any input reading tasks.
  *
  * @groupprio skip 75
  * @groupname skip Whitespace Skipping Parsers
  * @groupdesc skip
  *     These parsers are designed to skip chunks of whitespace, for very rudimentary lexing tasks. It
  *     is probably better to use the functionality of [[parsley.token]].
  *
  * @groupprio class 20
  * @groupname class Character Class Combinators
  * @groupdesc class
  *     These combinators allow for working with ''character classes''. This means that a set, or range, of
  *     characters can be specified, and the combinator will return a parser that matches one of those characters
  *     (or conversely, any character that is ''not'' in that set). The parsed character is always returned.
  *
  * @groupprio spec 25
  * @groupname spec Specific Character Parsers
  * @groupdesc spec
  *     These parsers are special cases of [[satisfy `satisfy`]] or [[char `char`]]. They are worth using, as they are given special error labelling,
  *     producing nicer error messages than their primitive counterparts.
  *
  *     This documentation assumes JDK 17.
  *     JDK 17 is compliant with [[https://www.unicode.org/versions/Unicode13.0.0/UnicodeStandard-13.0.pdf Unicode® Specification 13.0]].
  *     As such, the descriptions of the parsers in this section are accurate with respect to Unicode® Specification 13.0:
  *     using a different JDK may affect the ''precise'' definitions of the parsers below. If in doubt, check the documentation
  *     for `java.lang.Character` to see which Unicode version is supported by your JVM. A table of the Unicode versions
  *     up to JDK 17 can be found [[https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Character.html here]].
  *
  *     These parsers are only able to parse unicode characters in the range `'\u0000'` to `'\uffff'`, known as
  *     the ''Basic Multilingual Plane (BMP)''. Unicode characters wider than a single 16-bit character should be
  *     parsed using multi-character combinators such as `string`, or, alternatively, combinators found in [[unicode `unicode`]].
  *
  * @groupprio string 22
  * @groupname string String Combinators
  * @groupdesc string
  *     These combinators allow for working with, or building, strings. This means that they can
  *     parse specific strings, specific sets of strings, or can read characters repeatedly to
  *     generate strings. They are united in all returning `String` as their result.
  *
  * @define strict be aware that all of the arguments to this combinator are in '''strict''' positions.
  *
  * @define oneOf
  *     This combinator tries to parse any character from supplied set of characters `cs`, returning it if successful.
  * @define noneOf
  *     This combinator tries to parse any character '''not''' from supplied set of characters `cs`, returning it if successful.
  *
  * @define categories
  *     ''The full list of codepoints found in a category can be found in the
  *     [[https://www.unicode.org/Public/13.0.0/ucd/extracted/DerivedGeneralCategory.txt Unicode Character Database]]''.
  *
  * @define bodyLift
  *     This combinator applies the given parsers in sequence and then applies the given function `f` of to all of the results.
  *
  *     Firstly, each parser is parsed in turn, each producing a result. So long as all of the parsers succeeded,
  *     the combinator can succeed by returning the application of the function `f` to all the arguments. If any
  *     of the parsers fails, the entire combinator fails.
  *
  * @define paramLift a function to apply to the results of the parsers with arity
  * @define returnLift a parser that parses all of the given parsers in order, and then combines their results with `f`.
  * @define bodyAp
  *     This combinator applies the given parsers in sequence and then applies the function returned by `pf` of to all of the results of the other parsers.
  *
  *     Firstly, each parser is parsed in turn, each producing a result (and the first, a function `f`). So long as all of the parsers succeeded,
  *     the combinator can succeed by returning the application of the function `f` to all the arguments. If any
  *     of the parsers fails, the entire combinator fails.
  *
  * @define paramAp a parser that returns a function to apply to the results of the parsers with arity
  * @define returnAp a parser that parses all of the given parsers in order, and then combines their results with `f`.
  */
object quickstart extends ParsleyImpl with combinator with character with position with lift with ap
