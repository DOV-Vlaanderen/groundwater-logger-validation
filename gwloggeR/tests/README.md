# Introduction

There are various testing procedures of `gwloggeR`. Each testing type is placed in its own directory.

## Strict

These are classical test that are enforced by the [`testthat`](http://r-pkgs.had.co.nz/tests.html) package. See [here](https://r-pkgs.org/tests.html) for a full user manual. These test can be executed from Rstudio with `Ctrl` + `Shift` + `T`.

Mainly the hydrostatic pressure tests are important since they test the detection function based on a predefined small timeseries that has clear AO, LS and TC events.

Some takeaways:

* Whenever you are tempted to type something into a print statement or a debugger expression, write it as a test instead.
* Test files live in `tests/testthat/` and their file names must start with `test`.
* Start each file with `testthat::context()` that describes what is tested in the file.
* Group various expected results into one unit testing `testhat::test_that()` function. These units have a name, so when something goes wrong, you know where to look at. Each test is run in its own environment and is self-contained, but the global options are not reset.

## Qualitative

Algorithms in `gwloggeR` are the result of iterative development. This development was mainly based on various analyses of supplied data. Once a new version is released, we want to guarantee the consistency with previous versions. And if there is a difference, we want this difference to be apparent before releasing it in production. In this section we have tests that make those differences apparent.

Tests in this section are not enforced. They are used in a more "qualitative" sense: the differences are made apparent with git. These test-functions are run on previous data that was used for development of `gwloggeR`. The results are then exported to comprehensive files like CSVs and PNGs. Git will make the differences apparent to the user, and it is then up to the user to choose whether to accept them (i.e. commit), or consider them as new bugs introduced by the fix. 

Testing whether two images (PNGs) are the same is not straightforward. R Studio has no decent tool for this. One can use [tortoisegit](https://tortoisegit.org/) which has a built-in image comparison tool that allows overlaying and blending.

# Extras

* Test coverage: `devtools::test_coverage()` 
* Comprehensive `R CMD check` explanation: https://r-pkgs.org/r-cmd-check.html. Mostly stuff one needs to take into consideration when building a package.
