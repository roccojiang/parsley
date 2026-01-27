# Lint rules for Parsley: `parsley-garnish`

A static analysis tool for `parsley`, providing lint diagnostics and automated refactoring (auto-fixes) implemented as Scalafix rules.

Based on thesis work in [`parsley-garnish`: A linter for the `parsley` parser combinator library](https://www.imperial.ac.uk/media/imperial-college/faculty-of-engineering/computing/public/distinguished-projects/2324-ug-projects/parsley-garnish-report-rocco-jiang-final.pdf).

## Rules
* `FactorLeftRecursion`: Detects and rewrites unproductive left-recursive parsers into idiomatic `chain` parsers. Emits a warning when the parser cannot be fixed automatically and would otherwise result in an infinite loop at runtime.
* Implicit conversions:
  * `AmbiguousImplicitConversions`: Warns when multiple conflicting implicit parser conversions are in scope.
  * `UseImplicitConversions`: Rewrites explicit `char`/`string` combinators to use implicit conversion methods instead (style improvement).
  * `NoExplicitImplicitConversions`: Removes unnecessary explicit calls to `parsley`'s implicit conversion methods (code smell).
* General style rules:
  * `SimplifyParser`: Rewrites overly complex parsers into their most simple equivalent form.
  * `AvoidParserRedefinition`: Detects and rewrites cases where the user has manually defined higher-level combinators that `parsley` provides out-of-the-box.
* Experimental rules:
  * `UseGenericBridges`: Converts `map`/`lift`/`zipped` parsers to use [generic template bridges](https://j-mie6.github.io/parsley/5.0/api-guide/templates.html). Type inference is best-effort only, so users may have to manually adjust types provided to the generated bridge companion objects. On Scala 3, [macro bridges](https://j-mie6.github.io/parsley/5.0/api-guide/macros.html) provide a more powerful and ergonomic alternative.

## Development
### Directory structure
* [`rules/`](rules/) contains rule implementations.
  * [`scalafix.v1.Rule`](rules/src/main/resources/META-INF/services/scalafix.v1.Rule) should be updated when adding a new rule, so that Scalafix knows to load it by name.
* [`input/`](input/) and [`output/`](output/) contain integration tests using `scalafix-testkit`.
* [`tests/`](tests/) contains unit tests using ScalaTest.

### Package structure
* [`parsley.garnish.rules`](rules/src/main/scala/parsley/garnish/rules/) contains the implemenation of the rules described [above](#rules).
* [`parsley.garnish.parser`](rules/src/main/scala/parsley/garnish/parser/) defines the `Parser` AST, which models the `parsley` combinator tree at a high level, allowing users to directly manipulate `parsley` tree terms.
* [`parsley.garnish.expr`](rules/src/main/scala/parsley/garnish/expr/) defines the `Expr` intermediate AST, enabling simple Scala expressions within `Parser`s to be manipulated and normalised.
* [`parsley.garnish.utils`](rules/src/main/scala/parsley/garnish/utils/) contains general utilities.

### Useful commands
```shell
# Run all (integration + unit) tests
sbt ~garnishTests/test

# Run a specific integration test
sbt ~garnishTests/testOnly *RuleSuite -- -z <TestName>

# Run main method in integration tests (opens a selector to choose which, if there are multiple main classes)
sbt ~garnishInput/run
```
