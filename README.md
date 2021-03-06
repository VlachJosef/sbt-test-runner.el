# Sbt test runner

Convenient functions for quickly selecting Scala test file to run.

## Goal of this project

Goal of this project is to run Scala test files with minimal setup. Only tool necessary is to have sbt 1.4.x (or newer) project and to have enabled [sbt-tests-metadata](https://github.com/VlachJosef/sbt-tests-metadata) sbt plugin.

## Dependencies

For following commands to work you need to have [sbt-tests-metadata](https://github.com/VlachJosef/sbt-tests-metadata) sbt plugin enabled and you have to be using **sbt 1.4.x** or newer.

Read [sbt-tests-metadata instalation instruction](https://github.com/VlachJosef/sbt-tests-metadata#installation) for details.

## Available commands

All following commands ultimately runs `testOnly` sbt command.

```
project/testOnly foo.bar.BazSpec
```

### Select from all

```elisp
(sbt-test-select-all-tests)
```

Choose from all available tests across **all projects** to select the one to run.

![sbt-test-select-all-tests in action](gifs/sbt-test-select-all-tests.gif)


### Select from current project

```elisp
(sbt-test-select-project-tests)
```

Choose from all available tests for **current projects** to select the one to run.

Current project in this context is the project to which current buffer's file belongs to.


### Run current test file

```elisp
(sbt-test-run-current)
```

Directly runs current Scala test file.


## Special munit support

Since [munit](https://scalameta.org/munit/) allows only one way to define tests, it is easy to detect all runnable test names by parsing current scala file for `test(...)` occurrences. User then can easily choose what test to run.

When in test file inheriting from `munit.FunSuite` two additional commands are available:

### Select test to run in current test file

Choose single test to run from all available tests in current test file.
Only available if your test file is **munit**.

```elisp
(sbt-test-munit-select-test)
```

### Select current test only

Like `(sbt-test-munit-select-test)` but puts current test as initial input.

Note. Current test is closest preceeding test relative to the `(point)` or the first test if `(point)` is before first test.

```elisp
(sbt-test-munit-prefer-current)
```

## How it works

todo
