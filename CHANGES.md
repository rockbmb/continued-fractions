0.10.0.2
=====
* Update link Travis CI build information in README from `travis-ci.org` to
  `travis-ci.com` because of [this recent change](https://blog.travis-ci.com/2018-05-02-open-source-projects-on-travis-ci-com-with-github-apps).

0.10.0.1
=====
* Remove unqualified imports from test module.
* Add README and CHANGES files to `extra-source-files` field in `.cabal` file.
* Add `bug-reports` field to `.cabal` file.

0.10.0.0
=====

* Temporarily move project's repository from [mokus0](https://github.com/mokus0/continued-fractions) to
  [rockbmb](https://github.com/rockbmb/continued-fractions). This was a duplication, not a fork, so
  users can create issues in the latter repository as well.
* Enable Travis CI to run tests for each build for the project's new repository.
* Increase lower bound for the Cabal version to be used when compiling this
  project from `>=1.6` to `>=1.10`.
* Change the supported GHC versions from the interval [7.0.4 .. 7.11] to
  [8.0.2 .. 8.6.1].
