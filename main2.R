source ("./cec_utils.R")
source("./config.R")
source("./forest_type.R")
source("./data.R")
source("./cluster_results.R")

load_forest_data <- function(){
  # loading forest data rasters
  log("loading forest data")
  
  fl <<- list.files(pattern=glob2rx("Total*.tif$"))
  
  fl_1 <<- raster(fl[1])
  
  # All the rasters have fl_l's projection  
  fl.ele <<- load_preproc_raster("ele_raster.tif") # elevation
  fl.slp <<- load_preproc_raster("slp_raster.tif") # slope
  fl.asp <<- load_preproc_raster("asp_raster.tif") # aspect
  fl.sit <<- load_preproc_raster("sit_raster.tif") # sit
  
  log("Making a raster brick")
  fl.stack <<- stack(fl)
  fl.stack <<- stack(fl.stack, fl.sit)
  fl.stack <<- shorten_fname(fl.stack)
}

load_shapefile <- function(){
  
  log("Loading Counties shapefile")
  shp <<- readOGR("CA_Counties/CA_Counties_TIGER2016.shp")
  fl.fhz.raster <<- load_preproc_raster("fl_fhz_raster.tif")
}


load_data <- function(){
  # CDL
  cdl.proj.mask <<- load_cdl()
  
  # Wilderness
  wild.prj <<- readOGR("wilderness_proj.shp")
  
  # Ownership
  own.prj <<- load_preproc_raster("own_proj.tif")
  
  # NPS excluded from ownership
  own_nps_ex <<- load_preproc_raster("own_nps_ex.tif")
  
  # Only NPS land in ownership
  own_nps_in <<- load_preproc_raster("own_nps_in.tif")
  
  # NPS excluded from fl.ele
  fl.ele.nps_ex <<- load_preproc_raster("fl_ele_nps_ex.tif") 
  
  log("Loading raster for NPS and WILD exlcuded from fl.ele")
  # NPS AND WILD Excluded from fl.ele
  fl.ele.wild.nps_ex.mask <<- load_preproc_raster("fl_ele_wild_nps_ex_mask.tif")
  
  # Main data frame 
  log("Converting the raster into points")
  #df.ele <<- readOGR("DF_ELE","cec_layer")
  #writeOGR(df.ele,"DF_ELE","cec_layer","ESRI Shapefile")
  
  # load counties shapefiles
  load_shapefile()
  
}

write_county_elevation = function(county_name){
  log("Writing for county",county_name)
  df.ele <<- rasterToPoints(fl.ele.wild.nps_ex.mask, spatial=T)
  
  shp_filt = shp[shp$NAME==county_name,]
  shp.curr_county.prj = load_county_shapefile(shp_filt, county_name)
  
  
  log("Getting county's elevation data")
  df.ele.curr_county <- over(df.ele, shp.curr_county.prj)
  df.ele.curr_county.na.omit <- na.omit(cbind(df.ele@data,df.ele@coords,df.ele.curr_county$NAME))
  
  log("Writing into csv file")
  county_dir <- paste("counties/",county_name,sep="")
  write.csv(df.ele.curr_county.na.omit, paste(county_dir,"/F3_data_ele_v3.csv",sep = ""))
}

write_counties_ele = function(){
  counties = c("Inyo","Tulare","Madera","Mariposa","Mono","Tuolumne","Nevada","Placer")
  for(county_name in counties){
    write_county_elevation(county_name)
  }
  
}



get_county_df <- function(county_name){
  county_dir <- paste("counties/",county_name,sep="")
  ele_file_path <- paste(county_dir,"/F3_data_ele_v3.csv",sep = "")
  
  if(file.exists(ele_file_path)){
    df.ele.curr_county.na.omit <- read.csv(ele_file_path)
  } else {
    log("Generating ele file")
    df.ele <<- rasterToPoints(fl.ele.wild.nps_ex.mask, spatial=T)
    shp_filt = shp[shp$NAME==county_name,]
    shp.curr_county.prj = load_county_shapefile(shp_filt, county_name)
    
    
    log("Getting county's elevation data")
    df.ele.curr_county <- over(df.ele, shp.curr_county.prj)
    df.ele.curr_county.na.omit <- na.omit(cbind(df.ele@data,df.ele@coords,df.ele.curr_county$NAME))
  }
  
  log("Transforming coordinates")
  coordinates(df.ele.curr_county.na.omit) <- ~x+y
  proj4string(df.ele.curr_county.na.omit) <- proj4string(fl.ele)
  df.data.curr_county <- extract(fl.stack, df.ele.curr_county.na.omit, sp=T)
  
  df.data.curr_county.latlon <- spTransform(df.data.curr_county, CRS("+proj=longlat +datum=WGS84"))
  #fname = paste0(county_name,"_lat_long")
  #writeOGR(df.data.curr_county.latlon,fname,paste0("COUNTY-",county_name),"ESRI Shapefile")
  return(df.data.curr_county.latlon)	
}


run_clustering = function(county_name, num_clusters=20){
  num_clusters=30
  
  county_df = get_county_df(county_name)
  write_complete_cluster(county_name)
  endCluster()
  beginCluster(num_clusters)
  write_complete_cluster_df(county_name, county_df)
  endCluster()
  beginCluster(num_clusters)
  generate_output(county_name, county_df, shp)
  endCluster()
  
}

main = function() {
  setup(30)
  load_forest_data()
  load_data()
  load_shapefile()
  # write_counties_ele()
  counties = c("Inyo","Tulare","Madera","Mariposa","Mono","Tuolumne","Nevada","Placer")
  for(county_name in counties){
    log("Running for county",county_name)
    run_clustering(county_name)
  }
  
  
}

main()
