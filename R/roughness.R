#' @title Roughness
#'
#' @description [roughness()] represents the variance in a continuous raster within a specified window and scale.
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
#' \linkS4class{RasterLayer} roughness
#'
#'
#'
#' @keywords roughness
#'
#'
#' @export

roughness <- function(elevation = NULL, size = 3, output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Roughness ...\n")
  }

  # standard deviation
  tmp1  <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                                input = tmp.dif, method = "stddev", size = size, output = file.path(tempdir(), "tmp_roughness.tif"), ...)


  # square
  outRaster <- raster::calc(x = tmp1, fun = function(x){return(x*x)})

  names(outRaster) <- "roughness"

  if(!is.null(output))
  {
    # write data
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Roughness: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))


  if(load_output)
  {
    return(outRaster)
  }

} # end of function roughness
