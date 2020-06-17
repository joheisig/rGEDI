#'Get GEDI Elevation and Height Metrics (GEDI Level2A)
#'
#'@description This function extracts Elevation and Relative Height (RH) metrics from GEDI Level2A data.
#'
#'@usage getLevel2AM(level2a, land_cover_data = FALSE)
#'
#'@param level2a A GEDI Level2A object (output of \code{\link[rGEDI:readLevel2A]{readLevel2A}} function).
#'An S4 object of class "gedi.level2a".
#'@param land_cover_data logical. If TRUE, land cover information on percent tree cover and percent non-vegetated
#'from Landsat and MODIS are retrieved.
#'
#'@return Returns an S4 object of class \code{\link[data.table:data.table]{data.table-class}}
#'containing the elevation and relative heights metrics.
#'
#'@seealso https://lpdaac.usgs.gov/products/gedi02_av001/
#'
#'@details Characteristics. Flag indicating likely invalid waveform (1=valid, 0=invalid).
#'\itemize{
#'\item \emph{beam} Beam identifie
#'\item \emph{shot_number} Shot number
#'\item \emph{degrade_flag} Flag indicating degraded state of pointing and/or positioning information
#'\item \emph{quality_flag} Flag simplifying selection of most useful data
#'\item \emph{delta_time} Transmit time of the shot since Jan 1 00:00 2018
#'\item \emph{sensitivity} Maxmimum canopy cover that can be penetrated
#'\item \emph{solar_elevation} Solar elevation
#'\item \emph{lat_lowestmode} Latitude of center of lowest mode
#'\item \emph{lon_lowestmode} Longitude of center of lowest mode
#'\item \emph{elev_highestreturn} Elevation of highest detected return relative to reference ellipsoid Meters
#'\item \emph{elev_lowestmode} Elevation of center of lowest mode relative to reference ellipsoid
#'\item \emph{rh} Relative height metrics at 1\% interval
#'}
#'Additionally, if land_cover_data = TRUE:
#'\itemize{
#'\item \emph{landsat_treecover} Tree cover in 2010, defined as canopy closure for all vegetation taller than 5 m
#'\item \emph{modis_treecover} Percent tree cover from MODIS MOD44B V6 data
#'\item \emph{modis_treecover_sd} Percent tree cover standard deviation from MODIS MOD44B V6 data
#'\item \emph{modis_nonvegetated} Percent non-vegetated from MODIS MOD44B V6 data
#'\item \emph{modis_nonvegetated_sd} Percent non-vegetated standard deviation from MODIS MOD44B V6 data
#'}
#'@examples
#'
#'# Specifying the path to GEDI level2A data (zip file)
#'outdir = tempdir()
#'level2A_fp_zip <- system.file("extdata",
#'                   "GEDI02_A_2019108080338_O01964_T05337_02_001_01_sub.zip",
#'                   package="rGEDI")
#'
#'# Unzipping GEDI level2A data
#'level2Apath <- unzip(level2A_fp_zip,exdir = outdir)
#'
#'# Reading GEDI level2A data (h5 file)
#'level2a<-readLevel2A(level2Apath=level2Apath)
#'
#'# Extracting GEDI Elevation and Height Metrics
#'level2AM<-getLevel2AM(level2a)
#'head(level2AM)
#'
#'close(level2a)
#'@export
getLevel2AM<-function (level2a, land_cover_data = FALSE) {
  level2a <- level2a@h5
  groups_id <- grep("BEAM\\d{4}$", gsub("/", "", hdf5r::list.groups(level2a, 
                                                                    recursive = F)), value = T)
  rh.dt <- data.table::data.table()
  pb <- utils::txtProgressBar(min = 0, max = length(groups_id), 
                              style = 3)
  i.s = 0
  for (i in groups_id) {
    i.s <- i.s + 1
    utils::setTxtProgressBar(pb, i.s)
    level2a_i <- level2a[[i]]
    if (any(hdf5r::list.datasets(level2a_i) == "shot_number")) {
      if (length(level2a_i[["rh"]]$dims) == 2) {
        rh = t(level2a_i[["rh"]][, ])
      }
      else {
        rh = t(level2a_i[["rh"]][])
      }
      rhs <- data.table::data.table(beam <- rep(i, length(level2a_i[["shot_number"]][])), 
                                    shot_number = level2a_i[["shot_number"]][], 
                                    degrade_flag = level2a_i[["degrade_flag"]][], 
                                    quality_flag = level2a_i[["quality_flag"]][], 
                                    quality_flag = level2a_i[["delta_time"]][], 
                                    lat_lowestmode = level2a_i[["lat_lowestmode"]][], 
                                    lon_lowestmode = level2a_i[["lon_lowestmode"]][], 
                                    elev_highestreturn = level2a_i[["elev_highestreturn"]][], 
                                    elev_lowestmode = level2a_i[["elev_lowestmode"]][], 
                                    rh)  
    }
    if (land_cover_data){
      lcd <- data.table::data.table(landsat_treecover = level2a_i[["land_cover_data/landsat_treecover"]][], 
                                    modis_nonvegetated = level2a_i[["land_cover_data/modis_nonvegetated"]][],
                                    modis_nonvegetated_sd = level2a_i[["land_cover_data/modis_nonvegetated_sd"]][],
                                    modis_treecover = level2a_i[["land_cover_data/modis_treecover"]][],
                                    modis_treecover_sd = level2a_i[["land_cover_data/modis_treecover_sd"]][])
      rhs <- cbind(rhs, lcd)
    }
    rh.dt <- rbind(rh.dt, rhs)
  }
  clmn_names <- c("beam", "shot_number", "degrade_flag", 
                  "quality_flag", "delta_time", "lat_lowestmode", "lon_lowestmode", 
                  "elev_highestreturn", "elev_lowestmode", paste0("rh", seq(0, 100)))
  if (land_cover_data){
    colnames(rh.dt) <- c(clmn_names, colnames(lcd))
  } else {colnames(rh.dt) <- clmn_names}

  close(pb)
  return(rh.dt)
}


