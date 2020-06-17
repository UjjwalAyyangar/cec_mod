set_clusters <- function(n){
  num_clusters <<- n
}

log <- function(...){
  arguments <- paste(list(...),collapse= ' ')
  print(arguments)
  
}

load_preproc_raster <- function(raster_name){
  # This function returns rasters
  raster_path <<- paste('preproc_rasters/',raster_name,sep='')
  log("loading",raster_path)
  return (raster(raster_path))
}

shorten_fname <- function(ras){
  
  fnames = names(ras)
  short_names = c()
  
  for (name in fnames){
    if (grepl("NoMGT",name,fixed=TRUE)){
      temp = strsplit(name,"NoMGT")[[1]][1]
      temp = substr(temp,11,nchar(temp)-1)
      short_names = c(short_names,temp)
    }else{
      short_names = c(short_names,name)
    }
  }
  
  names(ras)= short_names
  return (ras)

}

euclid_dist = function(row){
    dists <- (row[["x"]] - df2$x)^2 + (row[["y"]]- df2$y)^2
    return(cbind(df2[which.min(dists),], distance = min(dists)))
  
}


