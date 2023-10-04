rm(list = ls())

######################################################################################################
# Step 0) to do, only once

# These packages need to be installed first:
# install.packages(c("abind", "agricolae", "akima", "beanplot", "boot", "callr", "car",
#                    "caTools", "chron", "cluster", "compiler", "data.table", "devtools",
#                    "FAdist", "fields", "gbm", "gdalUtils", "geoR", "gpclib", "grDevices",
#                    "gstat", "Hmisc", "klaR", "kriging", "leaps", "maps", "mapdata",
#                    "maptools", "MASS", "MCMCpack", "nlme", "numDeriv", "onls", "PBSmapping",
#                    "plotrix", "pls", "proto", "raster", "rgdal", "rgeos", "rlas", "robustbase",
#                    "rworldmap", "RSEIS", "R.utils","smatr"))
#
# rhdf5 is a specific package that needs to be installed separately
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("rhdf5")

# Some default r files also need to be edited:
# load.everything.r (line 247)
# loaded.package[["hdf5"       ]] = discreet.require(hdf5        )
# must become:
# loaded.package[["rhdf5"       ]] = discreet.require(rhdf5        )

# functions magma.r and inferno.r: replace <<- by <- 

# read.q.files:    soilcp     = rep(datum$soil.prop$soilcp,nzg)

# ED2/R-utils/monthly.template.r, line 14 (mymont    = hdf5load(file=h5first,load=FALSE,verbosity=0,tidy=TRUE)) needs to be replaced with:
# mymont    = lapply(h5read_opt(h5first),FUN=aperm)
# names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

# ED2/R-utils/read.q.files.r, line 113 ( mymont    = hdf5load(file=h5file,load=FALSE,verbosity=0,tidy=TRUE)) needs to be replaced with:
# mymont    = lapply(h5read_opt(h5file),FUN=aperm)
# names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

# Then we read the outputs files with the read_and_plot_ED2.2_all_tspft function

source("/data/gent/vo/000/gvo00074/felicien/R/h5read_opt.r")
source("/data/gent/vo/000/gvo00074/felicien/R/read_and_save_ED2.2.R")

read_and_save_ED2.2(there = '/kyukon/scratch/gent/vo/000/gvo00074/felicien/BCI/analy/', # path to the analy outputs (Q files)
                              place = 'analysis',                                              # output name
                              yeara = '1901/01/01',                                                 # first year/month to process
                              yearz = '1903/01/01',                                                 # last year/month (+ 1) to process
                              ED2srcdir = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2.2model/ED2/R-utils")

# ED2srcdir is the location of the R-utils library from the github ED2 repository
# read_and_plot_ED2.2_all_tspft function generates a .RData file + figures

# We then load the output files
#load("./outputs/Japan_default.RData")

# Plot something
#matplot(datum$szpft$gpp[,12,c(2,3,4,18)],type = "l")

