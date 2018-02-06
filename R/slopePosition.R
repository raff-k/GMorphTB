#' @title Slope Position
#'
#' @description [slopePosition()] calculates scalable slope position by subtracting a focalmean raster
#' from the original elevation raster.
#'
#'
#' @param elevation \linkS4class{RasterLayer} containing elevation values
#' @param size analysis window (rectangle). Default: 3
#' @param output (optional) file name to save output. Default: NULL
#' @param load_output If TRUE, function output file will be loaded into R. Default: TRUE
#' @param quiet IF FALSE, console messages are shown. Default: TRUE
#' @param ... optional parameter for [RQGIS::run_qgis()]
#'
#' @return
#' \linkS4class{RasterLayer} slopePosition
#'
#'
#'
#' @keywords slope position
#'
#'
#' @export

slopePosition <- function(elevation = NULL, size = 3, output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Slope Position ...\n")
  }

  meanTmp <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                           input = elevation, method = "average", size = size, output = file.path(tempdir(), "tmp_m.tif"), ...)

  outRaster <- raster::overlay(x = elevation, y = meanTmp, fun = function(x, y){return(x-y)})

  names(outRaster) <- "slopePosition"

  if(!is.null(output))
  {
    # write data
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Slope Position: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))


  if(load_output)
  {
    return(outRaster)
  }

} # end of function slopePosition
