# parsley-garnish: Scalafix rules for Parsley

(Within the root directory)

To develop rules:
```
sbt ~garnishTests/test
# edit rules in parsley-garnish/rules/src/main/scala/fix/*.scala
```

To run the test inputs:
```
sbt ~garnishInput/run
```
