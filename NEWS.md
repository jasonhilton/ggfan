# ggfan 0.1.4
* Ensures vignette compatibility with new version of `rstan` (which now no longer imports ggplot2 on which I was stupidly relying). Thanks to Ben Goodrich for highlighting this (and for all the cool work done by the stan devs in general). 
* Fixes notes regarding unneeded imports.

# ggfan 0.1.3
* Ensures compatibility with updates of dplyr and tibble (thanks to Francois Romain for adding the relevant import for dplyr.)

# ggfan 0.1.2
* Ensures compatibility with ggplot 2.2.1.9000 and hopefully therefore 2.3.0.
* Adding a default linetype to GeomIntervalPath prevented an error in the call to draw_key_path in the new version of ggplot2.
* Fix documentation links to external packages

# ggfan 0.1.1

* Make `rstan` a strong dependency to avoid warnings when running vignette or on load of dataset if this package is not installed on test platform.

# ggfan 0.1.0

Initial submission to CRAN
