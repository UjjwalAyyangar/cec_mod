dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

load_county_combined_df = function(county_name){
  county_dir <- paste0("counties/",county_name)
  csv_file_path <- paste(county_dir,"/",county_name,"_private_forest2.csv",sep="")
  return (read.csv(csv_file_path))
}


write_complete_cluster <- function(county_name){
  
  log("Preparing kmeans raster file for",county_name)
  county_dir <- paste("counties/",county_name,sep="")
  log("Countery directory is",county_dir)
  
  plot_file <- paste("/plot/",county_name,"_all_nps_wild_ex.tif",sep="")
  log("Plot file location is",plot_file)
  
  
  f3_data_file <- paste(county_dir,"/F3_data_kmeans100_v7.csv",sep="")
  log("kmeans csv location is",f3_data_file)
  
  plot.kmeans.fhz <- read.csv(f3_data_file)
  raster.kmeans100.fhz <- rasterFromXYZ(plot.kmeans.fhz[c(4,5,8)])
  
  proj4string(raster.kmeans100.fhz) <- proj4string(raster(fl[1]))
  
  log("Writing kmeans raster for",county_name,"in to memory.","File name is : ",plot_file)
  writeRaster(raster.kmeans100.fhz, paste(county_dir,plot_file,sep=""), overwrite=T)
  
}

get_cluster_raster = function(county_name){
  county_dir <- paste("counties/",county_name,sep="")
  plot_file <- paste("/plot/",county_name,"_all_nps_wild_ex.tif",sep="")
  raster_path <- paste0(county_dir,plot_file)
  return (raster(raster_path))
  
  
}

write_complete_cluster_df <- function(county_name, county_df){
  log("Preparing kmeans data frame for county,",county_name)
  county_dir <- paste("counties/",county_name,sep="")
  
  # reading kmeans csv
  
  test.kmeans100 <- read.csv(paste(county_dir,"/F3_data_kmeans100_v7.csv",sep=""))
  test.kmeans100.filt <- test.kmeans100[,c('Cluster1','Cluster2')]
  test.kmeans100.filt<-spCbind(county_df,test.kmeans100.filt) #Error
  
  
  # convert into data frame
  
  test.kmeans100.filt.df <- as.data.frame(test.kmeans100.filt)
  test.kmeans100.filt.df[is.na(test.kmeans100.filt.df)]<-0
  
  colnames(test.kmeans100.filt.df)<-dbSafeNames(colnames(test.kmeans100.filt.df))
  test.kmeans100.filt.df$cluster_no <- test.kmeans100.filt.df$cluster1 * 2500 + test.kmeans100.filt.df$cluster2
  
  headers <- paste0(colnames(test.kmeans100.filt.df), " ", "REAL", " ", "NOT NULL")
  f = file("table_headers.txt")
  writeLines(headers, f)
  close(f)
  write.csv(test.kmeans100.filt.df, paste(county_dir,"/",county_name,"_all_nps_wild_ex.csv",sep=""), row.names = F)
  

}

round_lat_long <- function(df){
  df$xi <- round(df$xi,2)
  df$yi <- round(df$yi,2)
  return(df)
}

regularize <- function(a,b){
  vals = !is.na(a[])
  a[vals] = b[vals]
  return (a)
}



generate_output <- function(county_name, county_df, shp){
  log("Getting county shapefile")
  shp_filt = shp[shp$NAME==county_name,]
	shp.curr_county.prj <- load_county_shapefile(shp_filt,county_name)
	log("Getting complete result")
	county_dir = paste("counties/",county_name,sep="")
  	plot_file = paste("/plot/",county_name,"_all_nps_wild_ex.tif",sep="")
  	f3_data_file = paste(county_dir,"/F3_data_kmeans100_v7.csv",sep="")
  	kmeans_raster_path = paste0(county_dir,plot_file)
  	
	own_nps_ex = load_preproc_raster("own_nps_ex.tif")

	kmeans_raster = raster(paste0(county_dir,plot_file))
	
  
	plot.kmeans.fhz = read.csv(f3_data_file)
  
  	plot_csv = plot.kmeans.fhz[,c('Cluster1','Cluster2','x','y')]
  	colnames(plot_csv)[3] = "xi"
  	colnames(plot_csv)[4] = "yi"
  	plot_csv_bind = spCbind(county_df,plot_csv)

	county_own_crop = crop(own_nps_ex, extent(shp.curr_county.prj))
  
  	county_own = mask(county_own_crop, shp.curr_county.prj)
  
  	own_extent = extent(county_own)
  	kmeans_extent = extent(kmeans_raster)
  	

	log("Getting common extent")
  	xmin = max(own_extent[1], kmeans_extent[1])
  	xmax = min(own_extent[2], kmeans_extent[2])
  	ymin = max(own_extent[3], kmeans_extent[3])
  	ymax = min(own_extent[4], kmeans_extent[4])
  
		
  	kmeans_own_extent = extent(xmin,xmax,ymin,ymax)
  
 	kmeans_raster_crop <<- crop(kmeans_raster, extent(kmeans_own_extent))
  	county_own = crop(county_own, extent(kmeans_own_extent))
  
	log("Getting county forest land")
  	county_forest_land <<- mask(kmeans_raster_crop, county_own)
  
  
  	county_forest_land = overlay(county_forest_land,kmeans_raster_crop, fun=regularize)
  
  
  	plot_file_forest = paste0("/plot/",county_name,"_forest_land.tif")  
  	forest_raster_path = paste0(county_dir, plot_file_forest)
	writeRaster(county_forest_land,forest_raster_path, overwrite=T)
  
  	forest_land_df = rasterToPoints(county_forest_land)
  	forest_land_normal_df = as.data.frame(forest_land_df)
  	colnames(forest_land_normal_df)[1]="xi"
  	colnames(forest_land_normal_df)[2]="yi"
  	colnames(forest_land_normal_df)[3]="land"
  	forest_land_normal_df$land_use = rep("Forest",nrow(forest_land_normal_df))
  
  	forest_land_normal_df$xi <- round(forest_land_normal_df$xi,2)
  	forest_land_normal_df$yi <- round(forest_land_normal_df$yi,2)
  
  	plot_csv_bind$xi <- round(plot_csv_bind$xi,2)
  	plot_csv_bind$yi <- round(plot_csv_bind$yi,2)

	# PRIVATE 

	log("Getting private land")
  	county_private_land <<- mask(kmeans_raster_crop,county_forest_land,inverse=T)
  	county_private_land <<- overlay(county_private_land,kmeans_raster_crop, fun=regularize)
  
  	plot_file_private <<- paste0("/plot/",county_name,"_private_land.tif")  
  	private_raster_path = paste0(county_dir, plot_file_private)
  	writeRaster(county_private_land, private_raster_path, overwrite=T)
  
  	private_land_df = rasterToPoints(county_private_land)
  	private_land_normal_df = as.data.frame(private_land_df)
  
  
  	colnames(private_land_normal_df)[1]="xi"
  	colnames(private_land_normal_df)[2]="yi"
  	colnames(private_land_normal_df)[3]="land"
  	private_land_normal_df$land_use = rep("Private",nrow(private_land_normal_df))

	# COMBINE 

	log("combining dataframes")
	  combined_df = rbind(private_land_normal_df,forest_land_normal_df)
  	combined_res = spCbind(plot_csv_bind,combined_df)
  	combined_res <<- as.data.frame(combined_res)
  	#combined_res[is.na(combined_res)]<-0
  
	log("Getting db safe names")
  	colnames(combined_res)<-dbSafeNames(colnames(combined_res))
  	colnames(combined_res)[3] <- "county_name"
  	combined_res$cluster_no <- combined_res$cluster1 * 2500 + combined_res$cluster2
  	
  	# Adding forest type information
  	north_sierra = c("Plumas","Butte","Yuba","Nevada","Sierra")
  	south_sierra = c("Amador","Alpine","Calaveras","Tuolumne","Mono","Mariposa","Madera","Fresno",
  	                 "Inyo","Tulare","Kern","Placer")
  	
	log("Getting forest type information")
  	region = "north"
  	if(county_name %in% north_sierra){
  	  ftype_ras = get_ftype_raster()
  	} else {
  	  region = "south"
  	  ftype_ras = get_south_ftype_raster()
  	}
  	
  	ftype_cropped = crop(ftype_ras, extent(kmeans_raster_crop))
  	ftype_stack = stack(kmeans_raster_crop,ftype_cropped)
  	ftype_ras_df = as.data.frame(ftype_stack, xy=TRUE)
  	colnames(ftype_ras_df) = c("xi","yi","county_val","value")
  	
  	ftype_ras_df = map_to_regional(ftype_ras_df, region)
  	ftype_ras_df = ftype_ras_df[,!names(ftype_ras_df) %in% c("county_val","value"),drop=F]
  	
  	ftype_ras_df$xi = round(ftype_ras_df$xi,-1)
  	ftype_ras_df$yi = round(ftype_ras_df$yi,-1)
  	
  	combined_res$xi = round(combined_res$xi,-1)
  	combined_res$yi = round(combined_res$yi,-1)
  	
  	combined_res = merge(x=combined_res, y=ftype_ras_df,by=c("xi","yi"),all.x = TRUE)
  	cols.dont.want <- c("land","xi","yi","xi_1","yi_1") # if you want to remove multiple columns
  	combined_res_out <- combined_res[, ! names(combined_res) %in% cols.dont.want, drop = F]
  
  	#headers <<- paste0(colnames(combined_res), " ", "REAL", " ", "NOT NULL")
  	#writeLines(headers, file("table_headers.txt"))
    
  	write.csv(combined_res_out, paste(county_dir,"/",county_name,"_private_forest2.csv",sep=""),na="0", row.names = F)
 }
  

