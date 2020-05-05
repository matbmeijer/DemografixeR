## Test environments
* local OS X install, R 3.6.3
* ubuntu 14.04 (on travis-ci), R 3.6.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* Maintainer: 'Matthias Brenninkmeijer <matthias@brennotten.net>' - Days since last update: 6
* On win server sometimes openssl gives problems (curl dependency). Yet tests with win-builder worked perfectly.
* Solved issue in regards to topic ofs "Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error)."
