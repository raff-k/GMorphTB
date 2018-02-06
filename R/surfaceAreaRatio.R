#' @title Surface Area Ratio (Surface Texture)
#'
#' @description [surfaceAreaRatio()] calculates scalable slope position by subtracting a focalmean raster
#' from the original elevation raster.
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
#' \linkS4class{RasterLayer} surfaceAreaRatio
#'
#'
#'
#' @keywords surface area ratio
#'
#'
#' @export

surfaceAreaRatio <- function(elevation = NULL, slope = NULL, size = 3, zscale = "1.0", output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Surface Area Ratio ...\n")
  }

  # get slope
  if(is.null(slope))
  {
    slope <- RQGIS::run_qgis(alg = "grass7:r.slope.aspect",  load_output = TRUE, show_output_paths = FALSE,
                            elevation = elevation, zscale = zscale, slope = file.path(tempdir(), "tmp_slp.tif"), ...)
  }

  c <- prod(raster::res(slope))
  v <- pi/180

  tmp1 <- raster::calc(x = slope, fun = function(x){return(x*v)})

  outRaster <- raster::calc(x = tmp1, fun = function(x){return(c/cos(x))})

  names(outRaster) <- "surfaceAreaRatio"

  if(!is.null(output))
  {
    # write data
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Surface Area Ratio: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))


  if(load_output)
  {
    return(outRaster)
  }

} # end of function surfaceAreaRatio
