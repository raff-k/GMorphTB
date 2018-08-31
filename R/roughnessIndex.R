#' @title Roughness Index (Surface Texture)
#'
#' @description [roughnessIndex()] as standard deviation of residual topography (Cavalli et al. 2008).
#'
#'
#' @param elevation \linkS4class{RasterLayer} containing elevation values
#' @param size window size (using SAGA GIS it is the radius!) in cell units. Default: 3 (3x3 window, or SAGA 6x6)
#' @param output (optional) file name to save output. If return_weighted is TRUE, output should be vector containing two file names. Default: NULL
#' @param load_output If TRUE, function output file will be loaded into R. Default: TRUE
#' @param return_weighted If TRUE roughness index is normalized by max-value. Default: FALSE
#' @param quiet IF FALSE, console messages are shown. Default: TRUE
#' @param use.SAGA If TRUE, SAGA GIS is used for computing. Moreover, parameter env.rsaga must be set. Default: FALSE
#' @param env.rsaga SAGA GIS environment created by RSAGA::rsaga.env(). Default: NULL
#' @param ... optional parameter for [RQGIS::run_qgis()]
#'
#' @return
#' \linkS4class{RasterLayer} roughness index. If return_weighted is TRUE, a list is returned containing roughness index [[1]] and normalized roughness index [[2]]
#'
#'
#'
#' @keywords roughness index
#'
#'
#' @export
roughnessIndex <- function(elevation = NULL, size = 3, output = NULL, load_output = TRUE, return_weighted = FALSE, use.SAGA = FALSE, env.rsaga = NULL, quiet = TRUE, ...)
{
  if(use.SAGA && is.null(env.rsaga))
  {
    stop('If "use.SAGA" is TRUE then "env.rsaga" must be set!')
  }


  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Roughness Index as standard deviation of residual topography (Cavalli et al. 2008) ...\n")
  }

  # moving window mean of input
  if(quiet == FALSE) cat("... moving window mean\n")

  if(use.SAGA)
  {
    path.tmp.elev <- file.path(tempdir(), "tmp_elev.sgrd")
    path.tmp.elev.mean <- file.path(tempdir(), "tmp_m_elev.sgrd")
    path.tmp.elev.dif <- file.path(tempdir(), "tmp_d_elev.sgrd")
    path.tmp.out <- file.path(tempdir(), "tmp_out.sgrd")

    raster::writeRaster(x = elevation, filename = paste0(tools::file_path_sans_ext(x = path.tmp.elev), ".sdat"), overwrite = TRUE, NAflag = -99999)

    # RSAGA::rsaga.get.usage(lib = "statistics_grid", module = 1, env = env.rsaga)
    RSAGA::rsaga.geoprocessor(lib = "statistics_grid", module = 1, env = env.rsaga, show.output.on.console = FALSE, param = list(
      GRID = path.tmp.elev, MEAN = path.tmp.elev.mean, RADIUS = size, MODE = "0"))

    # difference beteen original and smoothed: residual topography
    if(quiet == FALSE) cat("... calculate residual topography\n")
    # RSAGA::rsaga.get.usage(lib = "grid_calculus", module = 1, env = env.rsaga)
    RSAGA::rsaga.geoprocessor(lib = "grid_calculus", module = 1, env = env.rsaga, show.output.on.console = FALSE, param = list(
      GRIDS = paste0(c(path.tmp.elev, path.tmp.elev.mean), collapse = ";"), RESULT = path.tmp.elev.dif, FORMULA = "a-b",
      FNAME = "1", NAME = "grid_residual"))

    if(quiet == FALSE) cat("... calculate Roughness Index\n")
    # RSAGA::rsaga.get.usage(lib = "statistics_grid", module = 1, env = env.rsaga)
    RSAGA::rsaga.geoprocessor(lib = "statistics_grid", module = 1, env = env.rsaga, show.output.on.console = FALSE, param = list(
      GRID = path.tmp.elev.dif, STDDEV = path.tmp.out, RADIUS = size, MODE = "0"))

    # ... load data
    outRaster <- raster::raster(paste0(tools::file_path_sans_ext(x = path.tmp.out), ".sdat"))
    names(outRaster) <- "roughnessIndex"

  } else {
    tmp.mean <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                                input = elevation , method = "0", size = size, output = file.path(tempdir(), "tmp_m_elev.tif"), ...)

    # difference beteen original and smoothed: residual topography
    if(quiet == FALSE) cat("... calculate residual topography\n")
    tmp.dif <- raster::overlay(elevation, tmp.mean, fun = function(x, y) {return(x - y)}, na.rm = TRUE)


    if(quiet == FALSE) cat("... calculate Roughness Index\n")
    outRaster  <- RQGIS::run_qgis(alg = "grass7:r.neighbors", load_output = TRUE, show_output_paths = FALSE,
                                  input = tmp.dif, method = "stddev", size = size, output = file.path(tempdir(), "tmp_RI.tif"), ...)

    names(outRaster) <- "roughnessIndex"
  }





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
