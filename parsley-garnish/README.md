# Lint rules for Parsley: `parsley-garnish`

Scalafix lint diagnostics and auto-fix rules for `parsley`.

## Development
### Directory structure
* `rules/` contains rule implementations
  * `src/main/resources/META-INF/services/scalafix.v1.Rule` should be updated when adding a new rule
* `input/` and `output/` contain integration tests using `scalafix-testkit`
* `tests/` contains unit tests using ScalaTest

### Package structure
* `parsley.garnish.rules` contains the rules themselves
* `parsley.garnish.model` holds the `Parser` and `Expr` intermediate AST implementations
* `parsley.garnish.analysis` includes various utilities for analysing the Scalameta AST to lift sections to `Parser` and/or `Expr`
* `parsley.garnish.util` consists of general utilities

### Useful commands
```shell
# Run all (integration + unit) tests
sbt ~garnishTests/test

# Run a specific integration test
sbt ~garnishTests/testOnly *RuleSuite -- -z <TestName>

# Run main method in integration tests (opens a selector to choose which, if there are multiple main classes)
sbt ~garnishInput/run
```
