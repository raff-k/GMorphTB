# Site Exposure Index ---------------------------
SEI <- function(slope = NULL, aspect = NULL, size = 3, filename = "SEI.tif", writeRaster = FALSE)
{

  if(quiet == FALSE) process.time.start.SEI <- proc.time()
  if(quiet == FALSE) cat("Running Site Exposure Index ...\n")


  tmp.cosResult <- raster::calc(aspect, fun = function(x) { return(cos((3.142 * (x - 180))/180))})


  if(quiet == FALSE) cat("... calculate Site Exposure Index\n")
  outraster.SEI <- raster::overlay(slope, tmp.cosResult, fun = function(x, y) {return(x * y)})
  if(!is.null(mask)){outraster.SEI <- raster::overlay(outraster.SEI, mask, fun = function(x, y){ifelse(is.na(y), NA, x)})}
  if(!is.null(mask.by.thres)){outraster.SEI  <- raster::calc(outraster.SEI, fun = function(x){ifelse(x >= mask.by.thres, NA, x)})}


  if(writeRaster == TRUE)
  {
    # write raster
    if(quiet == FALSE) cat("... write raster\n")
    raster::writeRaster(x = outraster.SEI, filename = paste0(output.path, '/', filename), overwrite = TRUE)
  }

  if(quiet == FALSE) cat(paste0("------ Run of Site Exposure Index: " , (proc.time() - process.time.start.SEI)["elapsed"][[1]]/60, " Minutes ------\n"))
  if(quiet == FALSE) cat("-------------------------\n")
  if(quiet == FALSE) cat("-------------------------\n")
  if(quiet == FALSE) cat("-------------------------\n")


  names(outraster.SEI) <- "SEI"
  return(outraster.SEI)

} # end of function SEI
