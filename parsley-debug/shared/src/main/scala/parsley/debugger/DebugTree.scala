/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

/** The tree representing a parser's parse tree (its parser execution path).
  *
  * Initially unpopulated, it will be populated with information regarding the parser, such as
  * what it is (if it is a primitive such as [[parsley.internal.deepembedding.singletons.Pure]],
  * or a user-defined named parser if names are collected) as the parser itself runs on some input.
  *
  * Any tree node will store the input it has parsed (or attempted to parse) as well as its
  * success state as an optional [[ParseAttempt]] instance.
  *
  * Although this trait is unsealed, it is not useful to make a subtype of this trait, as this
  * trait's sole purpose is to provide safe methods into handling the frozen trees produced by
  * the debugger.
  *
  * @since 4.5.0
  */
trait DebugTree extends Iterable[DebugTree] {
    /** The name of the parser that made this node. */
    def parserName: String

    /** The type name of the parser that formed this node. */
    def internalName: String

    /** The numerical identifier of this child, which is defined if this is a child parser that consumes input. */
    def childNumber: Option[Long]

    /** Get the full input that was attempted to be parsed by the debugged parser.
      *
      * This is the whole input, unaltered, even parts where the parser did not attempt to parse.
      * To see only the input a particular node or child node has viewed, look inside [[parseResults]].
      */
    def fullInput: String

    /** Get the potential parse attempt recorded for this particular parser. */
    def parseResults: Option[ParseAttempt]

    /** What are the child debug nodes for this node?
      *
      * The map provided by the implementation should be a linked map in order to preserve the
      * order of child parser occurrences within each parser.
      *
      * Internally, child nodes are given an arbitrary numeric suffix to disambiguate them in the map
      * if multiple child nodes have the same parser name. This also allows one to traverse the tree
      * down specific nodes via keys.
      *
      * Those internal names are not represented if checking [[parserName]].
      */
    def nodeChildren: Map[String, DebugTree]

// $COVERAGE-OFF$
    /** Provides a depth-first view of the tree as an iterator. */
    override def iterator: Iterator[DebugTree] = Iterator(this) ++ nodeChildren.values.flatMap(_.iterator)

    override def toString: String = {
        val possibleChildNumber = childNumber.map(", " + _.toString).getOrElse("")
        val hasSuccess          = parseResults.exists(_.success)
        val keys                = nodeChildren.keys

        s"DebugTree { name: $parserName ($internalName$possibleChildNumber), success: $hasSuccess, children: $keys }"
    }
}

object DebugTree {
    def unapply(dt: DebugTree): Some[(String, String, Option[Long], String, Option[ParseAttempt], Map[String, DebugTree])] = {
        Some((dt.parserName, dt.internalName, dt.childNumber, dt.fullInput, dt.parseResults, dt.nodeChildren))
    }
}
// $COVERAGE-ON$
