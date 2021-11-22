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
BaleMountains <- as_Spatial(BaleMountains)

ArsiMountains <- st_union(HabitatPatchesSf[4,"LEGEND"], HabitatPatchesSf[5,"LEGEND"])
ArsiMountains <- ArsiMountains[,-2]
ArsiMountains <- as_Spatial(ArsiMountains)

NorthWollo <- st_union(HabitatPatchesSf[9,"LEGEND"], HabitatPatchesSf[10,"LEGEND"],
                       HabitatPatchesSf[11,"LEGEND"], HabitatPatchesSf[12,"LEGEND"])
NorthWollo <- NorthWollo[,-2]
NorthWollo <- as_Spatial(NorthWollo)

# Then for the single polygon landscapes
SouthWollo <- HabitatPatchesSf[2,"LEGEND"]
SouthWollo <- as_Spatial(SouthWollo)

SimienNP <- HabitatPatchesSf[3,"LEGEND"]
SimienNP <- as_Spatial(SimienNP)

MtGuna <- HabitatPatchesSf[6,"LEGEND"]
MtGuna <- as_Spatial(MtGuna)

MtChoke <- HabitatPatchesSf[13,"LEGEND"]
MtChoke <- as_Spatial(MtChoke)

MtGosh <- HabitatPatchesSf[8,"LEGEND"]
MtGosh <- as_Spatial(MtGosh)

MtMenz <- HabitatPatchesSf[14,"LEGEND"]
MtMenz <- as_Spatial(MtMenz)

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

# We want to generate more occurrence points of the Ethiopian wolf to create a
# better representation of its current range. Therefore we want to generate
# random occurrence points in the landscapes where the wolf has little to no
# GBIF points. We will proportionally assign points to these landscapes with 
# occurence data from Marino (2003). We have 48 occurences from GBIF in Bale
# Mountains. According to Marino (2003), there are 250 Ethiopian wolfs in
# this mountain range. This means that the ratio GBIF occurences : population
# is equal to 48 / 250 = 0.192 in Bale national park. We will use this ratio
# to determine the number of points we want to generate in the other habitat patches.

# Step 1: Create a dataframe with the different landscapes, their population
# sizes according to Marino (2003) (we will take the avarage value of the 
# population estimate range), and the number of points that are in that landscape.

Landscape <- c("BaleMountains", "ArsiMountains", "SimienNP", "NorthWollo",
                  "SouthWollo", "MtGuna", "MtChoke", "MtGosh", "MtMenz")
Population <- c(250, 100, 47, 21, 17, 8, 0, 0, 20)
OccurencesGBIF <- c(48, 0, 3, 0, 0, 0, 0, 0, 0)
OccurenceRatio <- c(rep(0.192, 9))
PopulationPatchDF <- data.frame(Population, OccurencesGBIF,
                                OccurenceRatio)
PopulationPatchDF["NewPointsToGenerate"] <- 
  (PopulationPatchDF["Population"] * PopulationPatchDF["OccurenceRatio"]) - PopulationPatchDF["OccurencesGBIF"]
rownames(PopulationPatchDF) <- Landscape

# Now we know how many points we want to generate per habitat patch, column 5 of PopulationPatchDF

# Step 2: Generate random points in the landscapes according to the data in PopulationPatchDF

# Bale Mountains
if(floor(PopulationPatchDF["BaleMountains", "NewPointsToGenerate"]) > 0){
  occurencesBaleMountaints <- sp::spsample(BaleMountains, 
               floor(PopulationPatchDF["BaleMountains", "NewPointsToGenerate"]),
               type = "random",
               iter = 25)
}

# ArsiMountains
if(floor(PopulationPatchDF["ArsiMountains", "NewPointsToGenerate"]) > 0){
  occurencesArsiMountains <- sp::spsample(ArsiMountains, 
                                           floor(PopulationPatchDF["ArsiMountains", "NewPointsToGenerate"]),
                                           type = "random",
                                           iter = 25)
}

# SimienNP
if(floor(PopulationPatchDF["SimienNP", "NewPointsToGenerate"]) > 0){
  occurencesSimienNP <- sp::spsample(SimienNP, 
                                          floor(PopulationPatchDF["SimienNP", "NewPointsToGenerate"]),
                                          type = "random",
                                          iter = 25)
}


#newOccurences <- occurenceGBIF[]





