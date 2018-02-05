#' @title Site Exposure Index
#'
#' @description Calculates a site exposure index (Balice et al. 2000)
#'
#'
#' @param elevation \linkS4class{RasterLayer} containing elevation values
#' @param slope (optional) \linkS4class{RasterLayer} containing slope values. Default: NULL
#' @param aspect (optional) \linkS4class{RasterLayer} containing aspect values. Note: North must be 0, and East must be 90. Default: NULL
#' @param zscale Multiplicative factor to convert elevation units to horizontal units. Default: "1.0"
#' @param output (optional) file name to save output. Default: NULL
#' @param load_output If TRUE, function output file will be loaded into R. Default: TRUE
#' @param quiet IF FALSE, console messages are shown. Default: TRUE
#' @param ... optional parameter for [RQGIS::run_qgis()]
#'
#' @return
#' \linkS4class{RasterLayer} site exposure index
#'
#'
#'
#' @keywords site exposure index
#'
#'
#' @export
#'
siteExposure <- function(elevation = NULL, slope = NULL, aspect = NULL, zscale = "1.0", output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Site Exposure Index ...\n")
  }


  # get slope and/or aspect
  if(is.null(slope) || is.null(aspect))
  {
    tmp <- RQGIS::run_qgis(alg = "grass7:r.slope.aspect",  load_output = TRUE, show_output_paths = FALSE,
                            elevation = elevation, zscale = zscale, slope = file.path(tempdir(), "tmp_slp.tif"),
                           aspect = file.path(tempdir(), "tmp_asp.tif"), ...)
    if(is.null(slope))
    {
      slope <- tmp[[1]]
    }

    if(is.null(aspect))
    {
      aspect <- tmp[[2]]

      # change direction, so that North is 0 Degree
      aspect <- raster::calc(aspect, fun = function(x) { return((450 - x) %% 360)})
    }
  }


  tmp.cosResult <- raster::calc(aspect, fun = function(x) { return(cos((3.142 * (x - 180))/180))})


  if(quiet == FALSE) cat("... calculate Site Exposure Index\n")
  outRaster <- raster::overlay(slope, tmp.cosResult, fun = function(x, y) {return(x * y)})

  names(outRaster) <- "SEI"


  if(!is.null(output))
  {
    # write raster
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Site Exposure Index: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))



  if(load_output)
  {
    return(outRaster)
  }

} # end of function siteExposure
