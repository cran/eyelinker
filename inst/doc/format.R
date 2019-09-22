## ----message=FALSE-------------------------------------------------------
require(eyelinker)

# Load in an example file, look at structure
ascfile <- system.file("extdata/mono2000.asc.gz", package = "eyelinker")
asc <- read.asc(ascfile)
str(asc, max.level = 1)

