# Sbt test runner

Convenient functions for quickly selecting Scala test file to run.

## Available commands:

### Select from all

Choose from all available tests across **all projects** to select the one to run:

```elisp
(sbt-test-select-all-tests)
```

### Select from current project

Choose from all available tests for **current projects** to select the one to run:
Current project in this context is the project to which current buffer's file belongs to.

```elisp
(sbt-test-select-project-tests)
```

### Run current test file

```elisp
(sbt-test-run-current)
```

### Select test to run in current test file.

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
