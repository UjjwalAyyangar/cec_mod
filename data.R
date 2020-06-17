load_cdl <- function(){
  log("loading cdl ")
  cdl.prj.mask <- load_preproc_raster("cdl_prj_mask.tif")
  return (cdl.prj.mask)  
 
}

load_wilderness <- function(){
  log("loading wilderness")
  wild.prj <<- readOGR("wilderness_proj.shp") # wilderness areas shapefile 
}

load_ownership_nps <- function(){
  log("loading ownership data")
  own.prj <<- load_preproc_raster("own_proj.tif")
  
  # get nps only land from ownership
  own_nps_ex <<- load_preproc_raster("own_nps_ex.tif")
  #own_nps_in <<- mask(own.prj,own_nps_ex,inverse=T) # only nps
  
  #writeRaster(own_nps_in,"own_nps_in.tif")
  own_nps_in = raster("own_nps_in.tif")
  #load_preproc_raster(own_nps_in)
  
  #fl.ele.nps_ex <<- mask(fl.ele, own_nps_in, inverse=T) # all fl.ele without nps
  #writeRaster(fl.ele.nps_ex,"fl_ele_nps_ex.tif",overwrite=T)
  fl.ele.nps_ex <<- load_preproc_raster("fl_ele_nps_ex.tif")
  #own_stack = stack(own_nps_ex,own_nps_in,own.prj)
  #fl_ele_stack = stack(fl.ele,fl.ele.nps_ex)
  #plot(fl_ele_stack)
  #plot(own_stack)
  
  
  
}

load_county_shapefile = function(shp.curr_county, county_name){
   
  shapes_dir = "/gpfs/data1/cmongp/ujjwal/cec/shapefiles/"
  shape_fname = paste0(county_name,"_proj.shp") 
  shape_file_path = paste0(shapes_dir,shape_fname)

  if(! dir.exists(shapes_dir)){
  	dir.create(shapes_dir, recursive=TRUE)
  }

  if(! file.exists(shape_file_path)){
	log("Generating projected shapefile")
  
  	log("Transforming the projection")
 	 shp.curr_county.prj <- spTransform(shp.curr_county, proj4string(fl.ele))
  	
	log("Writing shapefile into memory")
	shapefile(shp.curr_county.prj, shape_file_path)

	return (shp.curr_county.prj)

  } else {

	return (shapefile(shape_file_path))

  }
}


