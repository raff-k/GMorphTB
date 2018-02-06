#' @title Slope/Aspect Transformation (Temperature and Moisture)
#'
#' @description Options are Stage’s (1976) COS, SIN; or Roberts & Cooper (1989) TRASP (topographic radiation aspect index).
#' COS AND SIN - An a priori assumption of a maximum in the NW quadrant (45 azimuth) and a minimum in the SW quadrant can be
#' replaced by an empirically determined location of the optimum (Stage, 1976). For slopes from 0% - 100%, the functions are
#' linearized and bounded from -1 to 1. Greater than 100% slopes are treated out of the -1 to 1 range and the model sets all
#' values greater than 100% to 101% and flat areas (-1) to nodata. TRASP - Circular aspect is transformed to assign a value of
#' zero to land oriented in a north- northeast direction, (typically the coolest and wettest orientation), and a value of one on
#' the hotter, dryer south-southwesterly slopes. The result is a continuous variable between 0 - 1 (Roberts and Cooper 1989).
#'
#'
#' @param elevation \linkS4class{RasterLayer} containing elevation values
#' @param slope (optional) \linkS4class{RasterLayer} containing slope values. Value format must be percent. Default: NULL
#' @param aspect (optional) \linkS4class{RasterLayer} containing aspect values. Note: North must be 0, and East must be 90. Default: NULL
#' @param zscale Multiplicative factor to convert elevation units to horizontal units. Default: "1.0"
#' @param transType Transformation type. Must be "COS", "SIN" or "TRASP". Default: "COS"
#' @param output (optional) file name to save output. Default: NULL
#' @param load_output If TRUE, function output file will be loaded into R. Default: TRUE
#' @param quiet IF FALSE, console messages are shown. Default: TRUE
#' @param ... optional parameter for [RQGIS::run_qgis()]
#'
#' @return
#' \linkS4class{RasterLayer} saTransformation
#'
#'
#'
#' @keywords slope/aspect transformation
#'
#'
#' @export
#'

saTransformation <- function(elevation = NULL, slope = NULL, aspect = NULL, zscale = "1.0", transType = "COS", output = NULL, load_output = TRUE, quiet = TRUE, ...)
{

  if(quiet == FALSE)
  {
    process.time.start <- proc.time()
    cat("Running Slope/Aspect Transformation ...\n")
  }


  # get slope
  if(is.null(slope))
  {
    if(transType == "COS" | transType == "SIN")
    {
      slope <- RQGIS::run_qgis(alg = "grass7:r.slope.aspect",  load_output = TRUE, show_output_paths = FALSE,
                             elevation = elevation, zscale = zscale, format = "percent", slope = file.path(tempdir(), "tmp_slp.tif"), ...)
    }
  }

  # get aspect
  if(is.null(aspect))
  {
    aspect <- RQGIS::run_qgis(alg = "grass7:r.slope.aspect",  load_output = TRUE, show_output_paths = FALSE,
                              elevation = elevation, zscale = zscale, aspect = file.path(tempdir(), "tmp_asp.tif"), ...)

    # change direction, so that North is 0 Degree
    aspect <- raster::calc(aspect, fun = function(x) { return((450 - x) %% 360)})
  }


  if(quiet == FALSE) cat("... calculate Slope/Aspect Transformation\n")

  if(transType == "COS" | transType == "SIN")
  {
    # nullSet <- raster::calc(x = aspect, fun = function(x){return(ifelse(aspect == -1, NA, aspect))})
    nullSet <- aspect
    values(nullSet)[values( nullSet) == -1] <- NA
    con <- raster::calc(x = slope, fun = function(x){return(ifelse(x > 100, 101, x))})
    tmp4 <- raster::calc(x = con, fun = function(x){return(x/100)})

    if(transType == "COS")
    {
      outRaster <- raster::overlay(x = nullSet, y = tmp4, fun = function(x, y){return(cos(x/57.296)*y)})
    } else{ # sin selected
      outRaster <- raster::overlay(x = nullSet, y = tmp4, fun = function(x, y){return(sin(x/57.296)*y)})
      }
  }

  if(transType == "TRASP")
  {
    tmp2 <- raster::calc(x = aspect, fun = function(x){return((1 - cos((3.142/180)*(x - 30))))})
    tmp3 <- raster::calc(x = tmp2, fun = function(x){return(x/2)})

    outRaster <- raster::overlay(x = tmp3, y = aspect, fun = function(x, y){return(ifelse(y < 0, 0.5, x))})
  }

    names(outRaster) <- "saTransformation"


  if(!is.null(output))
  {
    # write raster
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outRaster, filename = output, overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Slope/Aspect Transformation: " , (proc.time() - process.time.start)["elapsed"][[1]]/60, " Minutes ------\n"))



  if(load_output)
  {
    return(outRaster)
  }

} # end of function saTransformation
