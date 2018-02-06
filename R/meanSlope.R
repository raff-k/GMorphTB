#' @title Mean Slope (Directionality)
#'
#' @description Mean of slope within a defined window.
#'
#'
#' @param elevation \linkS4class{RasterLayer} containing elevation values
#' @param slope (optional) \linkS4class{RasterLayer} containing slope values. Default: NULL
#' @param size analysis window (rectangle). Default: 3
#' @param zscale Multiplicative factor to convert elevation units to horizontal units. Default: "1.0"
#' @param output (optional) file name to save output. Default: NULL
#' @param load_output If TRUE, function output file will be loaded into R. Default: TRUE
#' @param quiet IF FALSE, console messages are shown. Default: TRUE
#' @param ... optional parameter for [RQGIS::run_qgis()]
#'
#' @return
#' \linkS4class{RasterLayer} mean slope
#'
#'
#'
#' @keywords mean slope
#'
#'
#' @export

meanSlope <- function(elevation = NULL, slope = NULL, size = 3, zscale = "1.0", output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Mean Slope ...\n")
  }


  # get slope
  if(is.null(slope))
  {
    slope <- RQGIS::run_qgis(alg = "grass7:r.slope.aspect",  load_output = TRUE, show_output_paths = FALSE,
                            elevation = elevation, zscale = zscale, slope = file.path(tempdir(), "tmp_slp.tif"), ...)
  }

  # mean slope
  outRaster <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                               input = slope, method = "0", size = size, output = file.path(tempdir(), "tmp_m_slp.tif"), ...)

  # remove value smaller 0
  outRaster <- raster::calc(outRaster, fun = function(x){ifelse(x < 0, 0, x)})

  names(outRaster) <- "meanSlope"

  if(!is.null(output))
  {
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Mean Slope: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))


  if(load_output)
  {
    return(outRaster)
  }

} # end function meanSlope
