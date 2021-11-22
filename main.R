# Project Etiopian Wolf (Canis simensis)
# Species Distribution Modelling
# Wildlife Ecology and Conservation (REG32806)
# Wageningen University
# Group 12

# Install required packages
if(!"raster" %in% rownames(installed.packages())){install.packages("raster")}
if(!"rgdal" %in% rownames(installed.packages())){install.packages("rgdal")}
if(!"sf" %in% rownames(installed.packages())){install.packages("sf")}
if(!"dplyr" %in% rownames(installed.packages())){install.packages("dplyr")}

# Load required packages
library(raster)
library(rgdal)
library(sf)
library(dplyr)

# Create data directory
dir <- "data"
if(!dir.exists(dir)){
  dir.create(dir)
}

# Add points from GBIF
occurenceGBIF <- read.csv("data/SpeciesCoordinates.csv")

# Download Ethiopian administrative shape
Ethiopia <- raster::getData(name = "GADM", path = dir, country = "ETH", level = 0)

# Import habitat patches created from QGIS
HabitatPatches <- readOGR("data/", "HabitatPatches")
HabitatPatchesSf <- st_as_sf(HabitatPatches)

# Make separate polygons of different habitats #

# First for the multipolygon landscapes
BaleMountains <- st_union(HabitatPatchesSf[1,"LEGEND"], HabitatPatchesSf[7,"LEGEND"])
BaleMountains <- BaleMountains[,-2]

ArsiMountains <- st_union(HabitatPatchesSf[4,"LEGEND"], HabitatPatchesSf[5,"LEGEND"])
ArsiMountains <- ArsiMountains[,-2]

NorthWollo <- st_union(HabitatPatchesSf[9,"LEGEND"], HabitatPatchesSf[10,"LEGEND"],
                       HabitatPatchesSf[11,"LEGEND"], HabitatPatchesSf[12,"LEGEND"])
NorthWollo <- NorthWollo[,-2]

# Then for the single polygon landscapes
SouthWollo <- HabitatPatchesSf[2,"LEGEND"]

SimienNP <- HabitatPatchesSf[3,"LEGEND"]

MtGuna <- HabitatPatchesSf[6,"LEGEND"]

MtChoke <- HabitatPatchesSf[13,"LEGEND"]

MtGosh <- HabitatPatchesSf[8,"LEGEND"]

MtMenz <- HabitatPatchesSf[14,"LEGEND"]

# Visualize different occurence locations in Ethiopia
plot(Ethiopia)
plot(BaleMountains, col = "red", add = T)
plot(ArsiMountains, col = "blue", add = T)
plot(SimienNP, col = "yellow", add = T)
plot(NorthWollo, col = "cyan", add = T)
plot(SouthWollo, col = "black", add = T)
plot(MtGuna, col = "grey", add = T)
plot(MtChoke, col = "lightgreen", add = T)
plot(MtGosh, col = "darkorange", add = T)
plot(MtMenz, col = "darkolivegreen", add = T)

# Add points form GBIF to the map
points(occurenceGBIF$Longitude, occurenceGBIF$Latitude)

# A