RI <- function(elevation = NULL, size = 3, filename.RI = "RI.tif", filename.RIw = "RIw.tif", writeRaster = FALSE)
{


  if(quiet == FALSE) process.time.start.RI <- proc.time()

  if(quiet == FALSE) cat("Running Roughness Index as standard deviation of residual topography (Cavalli et al. 2008) ...\n")

  # moving window mean of input
  if(quiet == FALSE) cat("... moving window mean\n")
  # tmp.mean <- raster::focal(elevation, w = matrix(1, size, size), fun = mean, na.rm = TRUE)

  rgrass7::execGRASS("r.neighbors", flags = c("quiet", "overwrite"), parameters = list(
    input = "elevation", output = "tmp.elevation.mean", method = "average", size = size))

  tmp.mean <- raster::raster(rgrass7::readRAST("tmp.elevation.mean", close_OK = FALSE))  # get data from GRASS GIS
  # projection(tmp.mean) <- projection(elevation)

  # tmp.mean <- RQGIS::run_qgis(alg = "grass7:r.neighbors", input = elevation , method = "0", size = size, output = "tmpMeanElev.tif",
  #                                        load_output = TRUE, show_output_paths = FALSE)

  if(!is.null(mask)){tmp.mean <- raster::overlay(tmp.mean, mask, fun = function(x, y){ifelse(is.na(y), NA, x)})}



  # difference beteen original and smoothed: residual topography
  if(quiet == FALSE) cat("... calculate residual topography\n")
  tmp.dif <- raster::overlay(elevation, tmp.mean, fun = function(x, y) {return(x - y)}, na.rm = TRUE)

  # roughness index
  # too slow...?
  # outraster.RI <- raster::focal(tmp.dif, w = matrix(1, size, size), fun = function(x) {return(sd(x, na.rm = TRUE))})
  # outraster.RI <- raster::focal(tmp.dif, w = matrix(1, size, size), fun = function(x) {return( sqrt(sum(abs(x-mean(x))^2)/size) )})


  if(writeRaster == TRUE)
  {
    # RQGIS::get_args_man(alg = "grass7:r.neighbors")
    if(quiet == FALSE) cat("... calculate Roughness Index\n")

    # outraster.RI  <- RQGIS::run_qgis(alg = "grass7:r.neighbors",
    #                                  input = tmp.dif , method = "stddev", size = size, output = paste0(output.path, '/', filename.RI), load_output = TRUE, show_output_paths = FALSE)
    #
    # if(!is.null(mask)){outraster.RI  <- raster::overlay(outraster.RI, mask, fun = function(x, y){ifelse(is.na(y), NA, x)})}
    # if(!is.null(mask.by.thres)){outraster.RI  <- raster::calc(outraster.RI, fun = function(x){ifelse(x >= mask.by.thres, NA, x)})}


    rgrass7::writeRAST(as(tmp.dif, 'SpatialGridDataFrame'), "tmp.dif",
                       zcol = names(tmp.dif), useGDAL = TRUE, flags = c("overwrite"))

    rgrass7::execGRASS("r.neighbors", flags = c("quiet", "overwrite"), parameters = list(
      input = "tmp.dif", output = "tmp.dif.stddev", method = "stddev", size = size))

    outraster.RI  <- raster::raster(rgrass7::readRAST("tmp.dif.stddev", close_OK = FALSE))  # get data from GRASS GIS
    # projection(outraster.RI) <- projection(elevation)

    if(!is.null(mask)){outraster.RI  <- raster::overlay(outraster.RI, mask, fun = function(x, y){ifelse(is.na(y), NA, x)})}
    if(!is.null(mask.by.thres)){outraster.RI  <- raster::calc(outraster.RI, fun = function(x){ifelse(x >= mask.by.thres, NA, x)})}


    raster::writeRaster(x = outraster.RI, filename = paste0(output.path, '/', filename.RI), overwrite = TRUE)



    # weighted
    # RI.max <- maxValue(outraster.RI[!is.na(outraster.RI)])
    RI.max <- cellStats(outraster.RI,'max')

    if(quiet == FALSE) cat("... calculate normalized Roughness Index\n")
    outraster.RI.w <- raster::calc(outraster.RI, fun = function(x){return(1 - (x/RI.max))})

    # write weighted raster
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outraster.RI.w, filename = paste0(output.path, '/', filename.RIw), overwrite = TRUE)

  } else {

    if(quiet == FALSE) cat("... calculate Roughness Index\n")

    # outraster.RI  <- RQGIS::run_qgis(alg = "grass7:r.neighbors",
    #                                  input = tmp.dif , method = "stddev", size = size, output = paste0(output.path, '/', filename.RI), load_output = TRUE)

    if(!is.null(mask)){outraster.RI  <- raster::overlay(outraster.RI, mask, fun = function(x, y){ifelse(is.na(y), NA, x)})}
    if(!is.null(mask.by.thres)){outraster.RI  <- raster::calc(outraster.RI, fun = function(x){ifelse(x >= mask.by.thres, NA, x)})}


    rgrass7::writeRAST(as(tmp.dif, 'SpatialGridDataFrame'), "tmp.dif",
                       zcol = names(tmp.dif), useGDAL = TRUE, flags = c("overwrite"))

    rgrass7::execGRASS("r.neighbors", flags = c("quiet", "overwrite"), parameters = list(
      input = "tmp.dif", output = "tmp.dif.stddev", method = "stddev", size = size))

    # outraster.RI  <- raster::raster(rgrass7::readRAST("tmp.dif.stddev", close_OK = FALSE))  # get data from GRASS GIS
    # projection(outraster.RI) <- projection(elevation)


    RI.max <- cellStats(outraster.RI,'max')
    if(quiet == FALSE) cat("... calculate normalized Roughness Index\n")
    outraster.RI.w <- raster::calc(outraster.RI, fun = function(x){return(1 - (x/RI.max))})

  } # enf if write == TRUE

  if(quiet == FALSE) cat(paste0("------ Run of Roughness Index: " , (proc.time() - process.time.start.RI)["elapsed"][[1]]/60, " Minutes ------\n"))
  if(quiet == FALSE) cat("-------------------------\n")
  if(quiet == FALSE) cat("-------------------------\n")
  if(quiet == FALSE) cat("-------------------------\n")

  names(outraster.RI) <- "RI"
  names(outraster.RI.w) <- "RIw"
  return(list(outraster.RI, outraster.RI.w))

} # end of function RI
