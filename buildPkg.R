# Build package
library(roxygen2)
library(devtools)

document()
install(dependencies = FALSE, build_vignettes = FALSE)

library(stockassessmenthelper)

?VAlignPlots
