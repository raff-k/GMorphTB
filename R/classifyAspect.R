#' @title Classify Aspect (Directionality)
#'
#' @description Classifies aspect into discrete classes.
#'
#'
#' @param elevation \linkS4class{RasterLayer} containing elevation values
#' @param aspect (optional) \linkS4class{RasterLayer} containing aspect values. Note: North must be 0, and East must be 90. Default: NULL
#' @param zscale Multiplicative factor to convert elevation units to horizontal units. Default: "1.0"
#' @param output (optional) file name to save output. Default: NULL
#' @param load_output If TRUE, function output file will be loaded into R. Default: TRUE
#' @param quiet IF FALSE, console messages are shown. Default: TRUE
#' @param ... optional parameter for [RQGIS::run_qgis()]
#'
#' @return
#' \linkS4class{RasterLayer} classifyAspect
#'
#'
#'
#' @keywords classify aspect
#'
#'
#' @export
#'

linearAspect <- function(elevation = NULL, aspect = NULL, zscale = "1.0", output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Classify Aspect ...\n")
  }


  # get aspect
  if(is.null(aspect))
  {
    aspect <- RQGIS::run_qgis(alg = "grass7:r.slope.aspect",  load_output = TRUE, show_output_paths = FALSE,
                              elevation = elevation, zscale = zscale, aspect = file.path(tempdir(), "tmp_asp.tif"), ...)

    # change direction, so that North is 0 Degree
    aspect <- raster::calc(aspect, fun = function(x) { return((450 - x) %% 360)})
  }


  offset <- 22.5

  remap <- c(0, offset, 64,   offset, 45 + offset, 128,   45 + offset , 90 + offset, 1,   90 + offset, 135 + offset, 2,    135 + offset, 180 + offset ,4,
             180 + offset, 225 + offset, 8,   225 + offset, 270 + offset, 16,     270 + offset, 315 + offset, 32,    315 + offset, 360, 64)

  remap <- matrix(remap, ncol = 3, byrow = TRUE)

  if(quiet == FALSE) cat("... classifying aspect\n")
  outRaster <- raster::reclassify(x = aspect, rcl = remap)

   names(outRaster) <- "classifyAspect"

  if(!is.null(output))
  {
    # write data
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Classify Aspect: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))


  if(load_output)
  {
    return(outRaster)
  }

} # end function classifyAspect
