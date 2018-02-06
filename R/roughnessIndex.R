#' @title Roughness Index
#'
#' @description [roughnessIndex()] as standard deviation of residual topography (Cavalli et al. 2008).
#'
#'
#' @param elevation \linkS4class{RasterLayer} containing elevation values
#' @param output (optional) file name to save output. If return_weighted is TRUE, output should be vector containing two file names. Default: NULL
#' @param load_output If TRUE, function output file will be loaded into R. Default: TRUE
#' @param return_weighted If TRUE roughness index is normalized by max-value. Default: FALSE
#' @param quiet IF FALSE, console messages are shown. Default: TRUE
#' @param ... optional parameter for [RQGIS::run_qgis()]
#'
#' @return
#' \linkS4class{RasterLayer} roughness index. If return_weighted is TRUE, a list is returned containing roughness index [[1]] and normalized roughness index [[2]]
#'
#'
#'
#' @keywords mean slope
#'
#'
#' @export
roughnessIndex <- function(elevation = NULL, size = 3, output = NULL, load_output = TRUE, return_weighted = FALSE, quiet = TRUE, ...)
{


  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Roughness Index as standard deviation of residual topography (Cavalli et al. 2008) ...\n")
  }

  # moving window mean of input
  if(quiet == FALSE) cat("... moving window mean\n")

  tmp.mean <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                              input = elevation , method = "0", size = size, output = file.path(tempdir(), "tmp_m_elev.tif"), ...)


  # difference beteen original and smoothed: residual topography
  if(quiet == FALSE) cat("... calculate residual topography\n")
  tmp.dif <- raster::overlay(elevation, tmp.mean, fun = function(x, y) {return(x - y)}, na.rm = TRUE)


  if(quiet == FALSE) cat("... calculate Roughness Index\n")
  outRaster  <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                                   input = tmp.dif, method = "stddev", size = size, output = file.path(tempdir(), "tmp_RI.tif"), ...)

  names(outRaster) <- "roughnessIndex"


  if(return_weighted)
  {
    outRaster.max <- raster::cellStats(outRaster, 'max')

    if(quiet == FALSE) cat("... calculate normalized Roughness Index\n")
    outRaster.w <- raster::calc(outRaster, fun = function(x){return(1 - (x/outRaster.max))})
    names(outRaster.w) <- "roughnessIndexW"
  }


  if(!is.null(output))
  {
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output[1], overwrite = TRUE)

    if(return_weighted)
    {
      raster::writeRaster(x = outRaster.w, filename = output[2], overwrite = TRUE)
    }

  }


  if(quiet == FALSE) cat(paste0("------ Run of Roughness Index: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))


  if(load_output)
  {
    if(return_weighted)
    {
      return(list(outRaster, outRaster.w))
    } else {
      return(outRaster)
      }
  }

} # end of function roughnessIndex
