Avoid testing in Latin-1 locale environment causing current errors.

## Test environments
- local macOS (Mojave) install, R 3.6.0
- ubuntu 16.04.6 (on travis-ci), oldrel, release, devel
- Debian GNU/Linux 9 (on Docker)
- win-builder

## R CMD check results

macOS (rcmdcheck::rcmdcheck())
Duration: 44.8s

❯ checking data for non-ASCII characters ... NOTE
    Note: found 188 marked UTF-8 strings

0 errors ✔ | 0 warnings ✔ | 1 note ✖

