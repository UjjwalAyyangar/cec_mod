load_libraries <- function(){
  library(sp)
  library(raster)
  library(rgdal)
  library(parallel)
  library(maptools)
  library(foreign)
}

root_dir = "/gpfs/data1/cmongp/ujjwal/cec/cec_mod"
data_dir = "/gpfs/data1/cmongp/ujjwal/cec/Forest Data/"

setup <- function(num_clusters=20){
  load_libraries()
  rasterOptions(maxmemory = 10e+9, chunksize = 10e+08) # comment this out if the machine is not powerful enough
  setwd("/gpfs/data1/cmongp/ujjwal/cec/Forest Data/") # sets the directory path
}


