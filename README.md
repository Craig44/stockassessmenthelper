# An R package with miscellaneous functions that I have found useful for my work
An R library with useful functions I have collected over time for plotting stock assessment and statistical stuff.
There is also a few functions for interrogating CASAL models.

## Installation
```r
devtools::install_github("Craig44/stockassessmenthelper")
```
An issue you might find when installing this, is it has dependencies with R packages that are not cran i.e. `library(casal)` and `library(DataWeighting)`. To get these packages email casal2@niwa.co.nz, the casal package can be found [here](https://casal2.github.io/casal/).

## Query Functionality
Once the library is installed you can query the functionality `library(help="stockassessmenthelper")` also see the vignette with the command `browseVignettes("stockassessmenthelper")`
