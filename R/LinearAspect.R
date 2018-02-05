# Linear Aspect ----------------------------
linearAspect <- function(aspect = aspect, size = size, filename = filename, writeRaster = writeRaster)
{
  if(quiet == FALSE) process.time.start.LinearAspect <- proc.time()

  if(quiet == FALSE) cat("Running Linear Aspect ...\n")

  # remove negative values and convert to radiant
  tmp.aspect <- raster::calc(aspect, fun = function(x){ifelse(x < 0, NA, (450.0 - x)/57.296)}) # convert to radiant

  if(quiet == FALSE) cat("... calculate cos and sin from aspect\n")
  # calculate sinus and cosinus from aspect
  tmp.sin <- raster::calc(tmp.aspect, sin)
  tmp.cos <- raster::calc(tmp.aspect, cos)

  # calculate sums in focal statistics for sin and cos
  # RQGIS::find_algorithms("neighbor")
  # RQGIS::get_args_man(alg = "grass7:r.neighbors")
  # RQGIS::get_usage(alg = "grass7:r.neighbors")
  # tmp.sin <- RQGIS::run_qgis(alg = "grass7:r.neighbors", input = tmp.sin, method = "6", size = as.character(size), output = "tmpSin.tif", load_output = TRUE, show_output_paths = FALSE)
  # tmp.cos <- RQGIS::run_qgis(alg = "grass7:r.neighbors", input = RQGIS::dem, method = "6", size = "3", output = "tmpCos.tif", load_output = TRUE, show_output_paths = FALSE)

  # tmp.sin <- raster::focal(tmp.sin, w = matrix(1, size, size), fun = sum, na.rm = TRUE, NAonly=TRUE, pad=TRUE)
  # tmp.cos <- raster::focal(tmp.cos, w = matrix(1, size, size), fun = sum, na.rm = TRUE, NAonly=TRUE, pad=TRUE)

  # load data into GRASS
  rgrass7::writeRAST(as(tmp.sin, 'SpatialGridDataFrame'), "tmp.sin",
                     zcol = names(tmp.sin), useGDAL = TRUE, flags = c("overwrite"))

  rgrass7::writeRAST(as(tmp.cos, 'SpatialGridDataFrame'), "tmp.cos",
                     zcol = names(tmp.cos), useGDAL = TRUE, flags = c("overwrite"))

  # perform focal statistics
  # print(rgrass7::parseGRASS("r.neighbors"))
  rgrass7::execGRASS("r.neighbors", flags = c("quiet", "overwrite"), parameters = list(
    input = "tmp.sin", output = "tmp.sin.sum", method = "sum", size = size))

  rgrass7::execGRASS("r.neighbors", flags = c("quiet", "overwrite"), parameters = list(
    input = "tmp.cos", output = "tmp.cos.sum", method = "sum", size = size))


  # get data from GRASS GIS
  tmp.sin <- raster::raster(rgrass7::readRAST("tmp.sin.sum", close_OK = FALSE))
  tmp.cos <- raster::raster(rgrass7::readRAST("tmp.cos.sum", close_OK = FALSE))

  # start final calculations
  tmp.Mod <- raster::overlay(tmp.sin, tmp.cos, fun = function(x, y){return((((450-(atan2(x, y) * 57.296)) * 100) %% 36000)/100)})

  if(quiet == FALSE) cat("... final calculation\n")
  outRaster.linearAspect <- raster::overlay(tmp.sin, tmp.cos, tmp.Mod, fun = function(x, y, z) {ifelse((x == 0) & (y == 0), -1, z)})

  projection(outRaster.linearAspect) <- projection(aspect)
  if(!is.null(mask)){outRaster.linearAspect <- raster::overlay(outRaster.linearAspect, mask, fun = function(x, y){ifelse(is.na(y), NA, x)})}
  if(!is.null(mask.by.thres)){outRaster.linearAspect <- raster::calc(outRaster.linearAspect, fun = function(x){ifelse(x >= mask.by.thres, NA, x)})}



  if(writeRaster == TRUE)
  {
    # write data
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster.linearAspect, filename = paste0(output.path, '/', filename), overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Linear Aspect: " , (proc.time() - process.time.start.LinearAspect)["elapsed"][[1]]/60, " Minutes ------\n"))

  names(outRaster.linearAspect) <- "Linear Aspect"
  return(outRaster.linearAspect)
} # end function Linear Aspect
