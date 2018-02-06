#' @title Surface Relief Ratio (Surface Texture)
#'
#' @description [surfaceReliefRatio()] describes rugosity in an continuous raster surface within rectangular
#'  or circular window. The implementation of SRR can be shown as: srr = (z(mean) - z(min))/(z(max) - z(min)).
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
#' \linkS4class{RasterLayer} surfaceReliefRatio
#'
#'
#'
#' @keywords surface relief ratio
#'
#'
#' @export

surfaceReliefRatio <- function(elevation = NULL, size = 3, output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Surface Relief Ratio ...\n")
  }


  tmp1 <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                             input = elevation, method = "minimum", size = size, output = file.path(tempdir(), "tmp_min.tif"), ...)

  tmp2 <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                          input = elevation, method = "maximum", size = size, output = file.path(tempdir(), "tmp_max.tif"), ...)

  tmp3 <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                          input = elevation, method = "average", size = size, output = file.path(tempdir(), "tmp_m.tif"), ...)


  conTmp <- raster::overlay(x = tmp2, y = tmp1, fun = function(x, y){return(x-y)})
  outVal <- raster::overlay(x = conTmp, y = tmp1, z = tmp3, fun = function(x, y, z){return((z-y)/x)})

  outRaster <- raster::overlay(x = outVal, y = conTmp, fun = function(x, y){return(ifelse(y == 0, 0, x))})

  names(outRaster) <- "surfaceReliefRatio"

  if(!is.null(output))
  {
    # write data
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Surface Relief Ratio: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))


  if(load_output)
  {
    return(outRaster)
  }

} # end of function surfaceReliefRatio
