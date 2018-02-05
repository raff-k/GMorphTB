#' @title Landform
#'
#' @description Concavity/convexity landform index (Bolstad’s variant).
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
#' \linkS4class{RasterLayer} landform
#'
#'
#'
#' @keywords landform
#'
#'
#' @export
#'

landform <- function(elevation = NULL, size = 3, output = NULL, load_output = TRUE, quiet = TRUE, ...)
{


  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Landform: concavity/convexity landform index (Bolstads variant) ...\n")
  }


  # calculate focal statistics: mean
  mean.Tmp <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                              input = elevation, method = "0", size = size, output = file.path(tempdir(), "tmp_m_elev.tif"), ...)

    # calculate landform
  if(quiet == FALSE) cat("... calculate landform\n")
  outRaster <- raster::overlay(elevation, mean.Tmp, fun = function(x, y) {return(10000 * ((x - y)/1000/36.2))})

  names(outRaster) <- "Landform"

  if(!is.null(output))
  {
    # write data
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Landform: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))

  if(load_output)
  {
    return(outRaster)
  }

} # end function Landform
