#' @title 2nd Derivative Slope (Temperature and Moisture)
#'
#' @description Calculates 2nd derivative of slope.
#'
#'
#' @param elevation \linkS4class{RasterLayer} containing elevation values
#' @param slope (optional) \linkS4class{RasterLayer} containing slope values. Default: NULL
#' @param zscale Multiplicative factor to convert elevation units to horizontal units. Default: "1.0"
#' @param output (optional) file name to save output. Default: NULL
#' @param load_output If TRUE, function output file will be loaded into R. Default: TRUE
#' @param quiet IF FALSE, console messages are shown. Default: TRUE
#' @param ... optional parameter for [RQGIS::run_qgis()]
#'
#' @return
#' \linkS4class{RasterLayer} SecondDerivativeSlope
#'
#'
#'
#' @keywords second derivative slope
#'
#'
#' @export
#'
SecondDerivativeSlope <- function(elevation = NULL, slope = NULL, aspect = NULL, zscale = "1.0", output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running 2nd Derivative Slope ...\n")
  }


  # get slope
  if(is.null(slope))
  {
    slope <- RQGIS::run_qgis(alg = "grass7:r.slope.aspect",  load_output = TRUE, show_output_paths = FALSE,
                             elevation = elevation, zscale = zscale, slope = file.path(tempdir(), "tmp_slp.tif"), ...)
  }


  if(quiet == FALSE) cat("... calculate 2nd Derivative Slope\n")

  outRaster <- RQGIS::run_qgis(alg = "grass7:r.slope.aspect",  load_output = TRUE, show_output_paths = FALSE,
                           elevation = slope, zscale = zscale, slope = file.path(tempdir(), "tmp_slp_2nd.tif"), ...)


  names(outRaster) <- "SecondDerivativeSlope"


  if(!is.null(output))
  {
    # write raster
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of 2nd Derivative Slope: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))



  if(load_output)
  {
    return(outRaster)
  }

} # end of function SecondDerivativeSlope
