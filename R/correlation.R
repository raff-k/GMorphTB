#' @title Correlation (Statistics)
#'
#' @description Calculates corrlation or covariance between two rasters.
#'
#'
#' @param rast1 first \linkS4class{RasterLayer}
#' @param rast2 second \linkS4class{RasterLayer}
#' @param measure  Transformation type. Must be "correlation" or "covariance". Default: "correlation"
#' @param output (optional) file name to save output. Default: NULL
#' @param load_output If TRUE, function output file will be loaded into R. Default: TRUE
#' @param quiet IF FALSE, console messages are shown. Default: TRUE
#' @param ... optional parameter for [RQGIS::run_qgis()]
#'
#' @return
#' \linkS4class{RasterLayer} correlation
#'
#'
#'
#' @keywords correlation
#'
#'
#' @export
#'

correlation <- function(rast1 = NULL, rast2 = NULL, size = 3, measure = "correlation", output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Correlation ...\n")
  }


  if(quiet == FALSE) cat("... calculate statistics \n")

  tmpXY <- raster::overlay(x = rast1, y = rast2, fun = function(x, y){return(x * y)})

  xBar <- RQGIS::run_qgis(alg = "grass7:r.neighbors", input = rast1, method = "average", size = size, output = file.path(tempdir(), "rast1_m.tif"), load_output = TRUE, show_output_paths = FALSE)
  yBar <- RQGIS::run_qgis(alg = "grass7:r.neighbors", input = rast2, method = "average", size = size, output = file.path(tempdir(), "rast2_m.tif"), load_output = TRUE, show_output_paths = FALSE)

  xyBar <- RQGIS::run_qgis(alg = "grass7:r.neighbors", input = tmpXY, method = "average", size = size, output = file.path(tempdir(), "rast12_m.tif"), load_output = TRUE, show_output_paths = FALSE)

  coVar <- raster::overlay(x = xBar, y = yBar, z = xyBar, fun = function(x, y, z){return(z - (x*y))})



  if(measure == "covariance")
  {
    outRaster <- coVar
  }


  if(measure == "correlation")
  {
    xStd <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                                     input = rast1, method = "stddev", size = size, output = file.path(tempdir(), "rast1_sd.tif"))

    yStd <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                            input = rast2, method = "stddev", size = size, output = file.path(tempdir(), "rast2_sd.tif"))

    xyStd <- raster::overlay(x = xStd, y = yStd, fun = function(x, y){return(x * y)})
    corr <-  raster::overlay(x = coVar, y = xyStd, fun = function(x, y){return(x / y)})
    # corr <- raster::calc(x = corr, fun = function(x){return(ifelse(x > 1, 1, x))})

    outRaster <- corr


  }

  names(outRaster) <- measure


  if(!is.null(output))
  {
    # write raster
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Correlation/Covariance: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))



  if(load_output)
  {
    return(outRaster)
  }

} # end of function correlation
