#' @title Linear Aspect (Directionality)
#'
#' @description Transforms the circular aspect to a linear variable.
#'
#'
#' @param elevation \linkS4class{RasterLayer} containing elevation values
#' @param aspect (optional) \linkS4class{RasterLayer} containing aspect values. Note: North must be 0, and East must be 90. Default: NULL
#' @param size analysis window (rectangle). Default: 3
#' @param zscale Multiplicative factor to convert elevation units to horizontal units. Default: "1.0"
#' @param output (optional) file name to save output. Default: NULL
#' @param load_output If TRUE, function output file will be loaded into R. Default: TRUE
#' @param quiet IF FALSE, console messages are shown. Default: TRUE
#' @param ... optional parameter for [RQGIS::run_qgis()]
#'
#' @return
#' \linkS4class{RasterLayer} linear aspect
#'
#'
#'
#' @keywords linear aspect
#'
#'
#' @export
#'

linearAspect <- function(elevation = NULL, aspect = NULL, size = 3, zscale = "1.0", output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Linear Aspect ...\n")
  }


  # get aspect
  if(is.null(aspect))
  {
    aspect <- RQGIS::run_qgis(alg = "grass7:r.slope.aspect",  load_output = TRUE, show_output_paths = FALSE,
                           elevation = elevation, zscale = zscale, aspect = file.path(tempdir(), "tmp_asp.tif"), ...)

    # change direction, so that North is 0 Degree
    aspect <- raster::calc(aspect, fun = function(x) { return((450 - x) %% 360)})
  }


  # remove negative values and convert to radiant
  tmp.aspect <- raster::calc(aspect, fun = function(x){ifelse(x < 0, NA, (450.0 - x)/57.296)}) # convert to radiant

  if(quiet == FALSE) cat("... calculate cos and sin from aspect\n")

  # calculate sinus and cosinus from aspect
  tmp.sin <- raster::calc(tmp.aspect, sin)
  tmp.cos <- raster::calc(tmp.aspect, cos)

  # calculate sums in focal statistics for sin and cos
  tmp.sin <- RQGIS::run_qgis(alg = "grass7:r.neighbors", input = tmp.sin, method = "6", size = size, output = file.path(tempdir(), "tmpSin.tif"), load_output = TRUE, show_output_paths = FALSE)
  tmp.cos <- RQGIS::run_qgis(alg = "grass7:r.neighbors", input = tmp.cos, method = "6", size = size, output = file.path(tempdir(), "tmpCos.tif"), load_output = TRUE, show_output_paths = FALSE)

  tmp.sin <- raster::focal(tmp.sin, w = matrix(1, size, size), fun = sum, na.rm = TRUE, NAonly=TRUE, pad=TRUE)
  tmp.cos <- raster::focal(tmp.cos, w = matrix(1, size, size), fun = sum, na.rm = TRUE, NAonly=TRUE, pad=TRUE)

  # start final calculations
  tmp.Mod <- raster::overlay(tmp.sin, tmp.cos, fun = function(x, y){return((((450-(atan2(x, y) * 57.296)) * 100) %% 36000)/100)})

  if(quiet == FALSE) cat("... final calculation\n")
  outRaster <- raster::overlay(tmp.sin, tmp.cos, tmp.Mod, fun = function(x, y, z) {ifelse((x == 0) & (y == 0), -1, z)})

  names(outRaster) <- "linearAspect"

  if(!is.null(output))
  {
    # write data
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Linear Aspect: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))


  if(load_output)
  {
    return(outRaster)
  }

} # end function linearAspect
