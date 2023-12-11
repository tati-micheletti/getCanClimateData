# Getting DEM for ClimateNA
library("reproducible")
options(reproducible.gdalwarp = TRUE)

source("makeClimateDEM_boreal.R")

DEM <- makeClimateDEM_boreal(destinationPath = file.path(getwd(), "outputs/"))
