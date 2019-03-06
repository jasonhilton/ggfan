## Test environments
* Windows 7 Enterprise desktop machine
* ubuntu 14.04 on travis-ci
* Ubuntu 16.04 desktop

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs on local checks or on travis CI

## R HUB check results

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
The following note was received:

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2019-03-01 as check problems were not
    corrected in time.

* Fedora Linux, R-devel, clang, gfortran
There was a PREPERROR while installing packages.
This seems to be a problem for a few users, and I'm not really sure what it relates to, but it doesn't seem to be to do with ggfan.
https://github.com/r-hub/rhub/issues/141


*	Ubuntu Linux 16.04 LTS, R-release, GCC
A NOTE on about a (very slightly) long-running example.
* checking examples ... NOTE
Examples with CPU or elapsed time > 5s
         user system elapsed
geom_fan 5.14  0.016   5.361



## Downstream dependencies
There are currently no downstream dependencies for this package
