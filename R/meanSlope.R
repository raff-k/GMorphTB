meanSlope <- function(slope = NULL, size.MS = "3", filename = "slp_mean.tif", writeRaster = FALSE)
{

  if(quiet == FALSE) process.time.start.MeanSlope <- proc.time()

  if(quiet == FALSE) cat("Running Mean Slope ...\n")

  if(quiet == FALSE) cat("... calculate mean slope\n")

  # outRaster.meanSlope <- raster::focal(slope.Fun, w = matrix(1, size, size), fun = mean, na.rm = TRUE)
  # rgrass7::writeRAST(as(slope.Fun, 'SpatialGridDataFrame'), "tmp.slope",
  #                    zcol = names(slope.Fun), useGDAL = TRUE, flags = c("overwrite"))
  #
  # rgrass7::execGRASS("r.neighbors", flags = c("quiet", "overwrite"), parameters = list(
  #   input = "tmp.slope", output = "tmp.slope.mean", method = "average", size = size))



  outRaster.meanSlope <- RQGIS::run_qgis(alg = "grass7:r.neighbors", input = slope.Fun, method = "0", size = size, output = "tmpMeanSlp.tif",
                                         load_output = TRUE, show_output_paths = FALSE)
  # remove value smaller 0
  outRaster.meanSlope <- raster::calc(outRaster.meanSlope, fun = function(x){ifelse(x < 0, 0, x)})


  if(!is.null(mask)){outRaster.meanSlope <- raster::overlay(outRaster.meanSlope, mask, fun = function(x, y){ifelse(is.na(y), NA, x)})}
  if(!is.null(mask.by.thres)){outRaster.meanSlope  <- raster::calc(outRaster.meanSlope, fun = function(x){ifelse(x >= mask.by.thres, NA, x)})}


  # get data from GRASS GIS
  # outRaster.meanSlope  <- raster::raster(rgrass7::readRAST("tmp.slope.mean", close_OK = FALSE))
  # projection(outRaster.meanSlope) <- projection(slope.Fun)


  if(writeRaster == TRUE)
  {
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster.meanSlope, filename = paste0(output.path, '/', filename), overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Mean Slope: " , (proc.time() - process.time.start.MeanSlope)["elapsed"][[1]]/60, " Minutes ------\n"))
  if(quiet == FALSE) cat("-------------------------\n")
  if(quiet == FALSE) cat("-------------------------\n")
  if(quiet == FALSE) cat("-------------------------\n")

  names(outRaster.meanSlope) <- "MeanSlope"
  return(outRaster.meanSlope)

} # end function mean slope
