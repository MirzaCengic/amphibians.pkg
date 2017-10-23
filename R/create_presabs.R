

#### Function to create PAs for given resolution ####

# Input so far is the rasterized species range, and the processing resolution
# Default resolution is 10 arc-minutes, and the continents and realms data
# will be leaded according to the spatial resolution.

# If there are fewer than 30 points for given resolution, the resolution is
# lowered. Add check for resolution before this function

#' Create presence/pseudoabsence dataframe.
#'
#' @param input_species_range Simple feature of the species range.
#' @param target_resolution Output of check_processing_resolution()
#' @param write_output If TRUE, stores the presence-absence dataframe to disk. Default is FALSE.
#'
#' @return Returns nothing, writes csv with presence and pseudo-absence coordinates.
#' @export
#'
#' @examples

create_presabs <- function(input_species_range, target_resolution, write_output = "FALSE")
{

  stopifnot(target_resolution %in% c("10m", "5m", "2.5m", "30s", "Insufficient"))

  if (target_resolution == "Insufficient") {break("Resolution insufficient")}

  # stopifnot()

  ## Get continents and realms rasters

  realm_raster <- raster(paste0(milkunize("Amphibians_project_folder/Data/Raster/Realm_", "m5"), target_resolution, ".tif"))

  # Continents rasters
  continent_raster <- raster(paste0(milkunize("Amphibians_project_folder/Data/Raster/Continent_", "m5"), target_resolution, ".tif"))

  ####
  outfile <- paste0(milkunize("Amphibians_project_folder/Data/Species_data/PAs/", "m5"), unique(input_species_range$binomial), ".csv")


  range_sp <- fasterize(input_species_range, continent_raster)


  range_vals <- getValues(range_sp)
  range_vals[range_vals >= 1] <- 1
  range_sp <- setValues(range_sp, range_vals)
  # Extract points
  range_pts <- rasterToPoints(range_sp)
  occ_pts <- as.data.frame(coordinates(range_pts))
  names(occ_pts)[3] <- "PA"


  #### Create pseudoabsences ####
  # print("Calculating species mask")
  sp_mask <- mask(continent_raster, range_sp) #select continent code overlaping with range
  mask_v <- unique(sp_mask)
  #codes of continents for species range
  # print("Calculating continents")
  continent_raster2 <- Which(continent_raster %in% mask_v)
  conti_vals <- getValues(continent_raster2)
  conti_vals[conti_vals == 0] <- NA
  continent_raster2 <- setValues(continent_raster2, conti_vals)
  # #extract raster for pseudo-absences (realm in which the species is present)
  # print("Calculating realms")
  mask2 <- mask(realm_raster, range_sp) #select realm code overlaping with range
  mask2_v <- unique(mask2)
  realm2 <- Which(realm_raster %in% mask2_v)

  realm2_vals <- getValues(realm2)
  realm2_vals[realm2_vals == 0] <- NA
  realm2 <- setValues(realm2, realm2_vals)
  # #combination realm-continent where species is present
  realmcont <- mask(realm2, continent_raster2)
  realmcont[realmcont == 1] <- 0
  # Resample for testing
  # print("Calculating PAs")
  PA_ras <- merge(range_sp, realmcont) #raster with 1 for cells outside range and 0 for within range
  #
  PA_vals <- getValues(PA_ras)
  PA_vals[PA_vals == 1] <- NA
  PA_vals[PA_vals == 0] <- 1
  PA_ras <- setValues(PA_ras, PA_vals)
  # Points might fall outside of the land area
  # print("Sampling")
  pas <- sampleRandom(PA_ras, 1000, na.rm = TRUE, sp = TRUE)
  pas <- as.data.frame(coordinates(pas))
  pas$PA <- rep(0, nrow(pas))
  gc()
  ##


  sp_presabs <- rbind(occ_pts, pas)

if (isTRUE(write_output))
  {
  write_csv(sp_presabs, outfile)
  }

  return(sp_presabs)
}
