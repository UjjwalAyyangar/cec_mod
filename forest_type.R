# FOREST TYPE 

mconifer_mask <- function(df){
  mixed_conifer = c("MB","MD","MF","MH","MP","RF","WF")
  mask = is.na(df$REG_D)
  
  for (val in mixed_conifer){
    #log(val)
    new_mask = df$REG_D==val
    mask = mask | new_mask
  }
  
  return (mask)
}

map_to_regional = function(ftype_ras_df, region){
  
  ftype_dbf = get_ftype_dbf(region)
  
  new_col = "reg_d"
  
  for(row in 1:length(ftype_ras_df)){
    val = ftype_ras_df$value[row]
    if (is.na(val)){
      ftype = "other"
    } else{
      ftype = ftype_dbf[ftype_dbf$Value==val,]$REGIONAL_D
    }
    ftype_ras_df$reg_d[row] = ftype
    
  }
  
  return (ftype_ras_df)
  
}

pine_mask <- function(df){
  pine = c("EP","JP","KP","LP","PP","PD","PL","WW","WB","PW")
  mask = is.na(df$REG_D)
  
  for (val in pine){
    #log(val)
    new_mask = df$REG_D==val
    mask = mask | new_mask
  }
  
  return (mask)
}


gen_ftype_df <- function(df, mask, ftype){
  l = length(df)
  df = df[mask,]
  forest_type = rep(ftype,l)
  df$forest_type = forest_type
  
  return(df)
}

get_ftype_df <- function(df){
  mconifer_mask = mconifer_mask(df)
  pine_mask = pine_mask(df)
  other_mask = !(mconifer_mask | pine_mask)
  
  mconifer = gen_ftype_df(df,mconifer_mask,"mixed_conifer")
  pine = gen_ftype_df(df,pine_mask,"pine")
  other = gen_ftype_df(df,other_mask,"other")
  
  
  reg_df = rbind(mconifer, pine)
  reg_df = rbind(reg_df,other)
  
  return(reg_df)
}

get_ftype_raster = function(){
  carmen_data = "/gpfs/data1/cmongp/ujjwal/cec/Carmen_data/"
  ftype_files = list.files(carmen_data,pattern=glob2rx("*.tif"),full.names = T)
  ns =  raster(ftype_files[1])
  return (projectRaster(ns,fl_1,method="ngb"))
  #return (disaggregate(raster(ftype_files[1]),fact=16))
}

get_south_ftype_raster = function(){
  carmen_data = "/gpfs/data1/cmongp/ujjwal/cec/Carmen_data/New"
  ftype_files = list.files(carmen_data,pattern=glob2rx("*.tif"),full.names = T)
  ss =  raster(ftype_files[1])
  return (projectRaster(ss,fl_1,method="ngb"))
  #return (disaggregate(raster(ftype_files[1]),fact=16))
}


classify = function(reg_d){
  if (reg_d %in% c("EP","JP","KP","LP","PP","PD","PL","WW","WB","PW")){
    return("pine")
  } else if (reg_d %in% c("MB","MD","MF","MH","MP","RF","WF")){
    return ("mixed_conifer")
  } else{
    return ("other")
  }
}

get_ftype_dbf = function(region){
  carmen_data = "/gpfs/data1/cmongp/ujjwal/cec/Carmen_data/"
  if(region =="north"){
    
    dbf_files = list.files(carmen_data,pattern=glob2rx("*.dbf"),full.names = T)
    dbf_file = read.dbf(dbf_files[7], as.is=TRUE)
    dbf_file = dbf_file[,c(1,3)]
  } else {
    carmen_data_new = paste0(carmen_data,"/New")
    dbf_files = list.files(carmen_data_new,pattern=glob2rx("*.dbf"),full.names = T)
    dbf_file = read.dbf(dbf_files[1], as.is=TRUE)
    names(dbf_file)[1] = "Value"
    dbf_file = dbf_file[,c(1,2)]
    
  }
  
  for(row in 1:nrow(dbf_file)){
    reg_d = dbf_file$REGIONAL_D[row]
    new_reg_d = classify(reg_d)
    dbf_file$REGIONAL_D[row] = new_reg_d
  }
  
  return (dbf_file)
}

load_forest_type <- function(){
  
 ftype_dir = "Forest_Type/"
 carmen_data = "/gpfs/data1/cmongp/ujjwal/cec/Carmen_data/"
 north_df_path = paste0(carmen_data,"north_df.csv")
 
 if(file.exists(north_df_path)){
   log("Dataframe exists. Reading it.")
   return (read.csv(north_df_path))
 }
 
 log("Generating dataframe.")
 
 ftype_files = list.files(carmen_data,pattern=glob2rx("*.shp"))
 
 log("loading north shapefile")
 north_shape = shapefile(paste0(carmen_data,ftype_files[1]))
 north_shape_proj = spTransform(north_shape,crs(fl.ele))
 north_shape_ras = rasterize(north_shape_proj, fl.ele)
 
 log("Extracting Regional dominance information")
 north_df = data.frame(coordinates(north_shape),north_shape$REGIONAL_D)
 names(north_df)[3] = "REG_D"
 
 log("Classifying regional dominance types into mixed conifer, pine & other")
 north_reg_df = get_ftype_df(north_df)
 
 log("Storing north dataframe into memory")
 names(north_reg_df)[1] = "x"
 names(north_reg_df)[2] = "y"
 write.csv(north_reg_df, north_df_path)
 
 #south_shp_dir = paste0(carmen_data,ftype_files[2])
 #south_shape = shapefile(south_shp_dir)
 #south_df = data.frame(coordinates(south_shape),south_shape$REGIONAL_D)
 #names(south_df)[3] = "REG_D"
 #south_reg_df = get_ftype_df(south_df)
 
 return(north_reg_df)
 
}


