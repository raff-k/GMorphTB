landform <- function(elevation = elevation, size = params.Landform$size, filename = params.Landform$filename, writeRaster = params.Landform$writeRaster)
{
  if(quiet == FALSE) process.time.start.Landform <- proc.time()

  if(quiet == FALSE) cat("Running Landform: concavity/convexity landform index (Bolstads variant) ...\n")


  # calculate focal statistics: mean
  # mean.Tmp <- raster::focal(elevation, w = matrix(1, size, size), fun = mean, na.rm = TRUE)
  # load data into GRASS
  # rgrass7::writeRAST(as(elevation, 'SpatialGridDataFrame'), "mean.Tmp",
  #                    zcol = names(mean.Tmp), useGDAL = TRUE, flags = c("overwrite"))

  # rgrass7::execGRASS("r.neighbors", flags = c("quiet", "overwrite"), parameters = list(
  #   input = "elevation", output = "tmp.elevation.mean", method = "average", size = size))

  # RQGIS::get_usage(alg = "grass7:r.neighbors")
  mean.Tmp <- RQGIS::run_qgis(alg = "grass7:r.neighbors", input = elevation, method = "0", size = size, output = "tmpMeanElev.tif",
                              load_output = TRUE, show_output_paths = FALSE)

  # get data from GRASS GIS
  # mean.Tmp <- raster::raster(rgrass7::readRAST("tmp.elevation.mean", close_OK = FALSE))
  # projection(mean.Tmp) <- projection(elevation)


  # calculate landform
  if(quiet == FALSE) cat("... calculate landform\n")
  outRaster.landfrom <- raster::overlay(elevation, mean.Tmp, fun = function(x, y) {return(10000 * ((x - y)/1000/36.2))})

  if(!is.null(mask)){outRaster.landfrom <- raster::overlay(outRaster.landfrom, mask, fun = function(x, y){ifelse(is.na(y), NA, x)})}
  if(!is.null(mask.by.thres)){outRaster.landfrom <- raster::calc(outRaster.landfrom, fun = function(x){ifelse(x >= mask.by.thres, NA, x)})}




  if(writeRaster == TRUE)
  {
    # write data
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster.landfrom, filename = paste0(output.path, '/', filename), overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Landform: " , (proc.time() - process.time.start.Landform)["elapsed"][[1]]/60, " Minutes ------\n"))
  if(quiet == FALSE) cat("-------------------------\n")
  if(quiet == FALSE) cat("-------------------------\n")
  if(quiet == FALSE) cat("-------------------------\n")

  names(outRaster.landfrom) <- "Landform"
  return(outRaster.landfrom)

} # end function Landform
