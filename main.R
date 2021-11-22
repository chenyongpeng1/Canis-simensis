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
if(round(PopulationPatchDF["BaleMountains", "NewPointsToGenerate"], 0) > 0){
  occurencesBaleMountaints <- sp::spsample(BaleMountains, 
               round(PopulationPatchDF["BaleMountains", "NewPointsToGenerate"], 0),
               type = "random",
               iter = 25)
}

# ArsiMountains
if(round(PopulationPatchDF["ArsiMountains", "NewPointsToGenerate"],0) > 0){
  occurencesArsiMountains <- sp::spsample(ArsiMountains, 
              round(PopulationPatchDF["ArsiMountains", "NewPointsToGenerate"],0),
              type = "random",
              iter = 25)
}

# SimienNP
if(round(PopulationPatchDF["SimienNP", "NewPointsToGenerate"],0) > 0){
  occurencesSimienNP <- sp::spsample(SimienNP, 
              round(PopulationPatchDF["SimienNP", "NewPointsToGenerate"],0),
              type = "random",
              iter = 25)
}

# NorthWollo
if(round(PopulationPatchDF["NorthWollo", "NewPointsToGenerate"],0) > 0){
  occurencesNorthWollo <- sp::spsample(NorthWollo, 
              round(PopulationPatchDF["NorthWollo", "NewPointsToGenerate"],0),
              type = "random",
              iter = 25)
}

# SouthWollo
if(round(PopulationPatchDF["SouthWollo", "NewPointsToGenerate"],0) > 0){
  occurencesSouthWollo <- sp::spsample(SouthWollo, 
              round(PopulationPatchDF["SouthWollo", "NewPointsToGenerate"],0),
              type = "random",
              iter = 25)
}

# MtGuna
if(round(PopulationPatchDF["MtGuna", "NewPointsToGenerate"],0) > 0){
  occurencesMtGuna <- sp::spsample(MtGuna, 
              round(PopulationPatchDF["MtGuna", "NewPointsToGenerate"],0),
              type = "random",
              iter = 25)
}

# MtChoke
if(round(PopulationPatchDF["MtChoke", "NewPointsToGenerate"],0) > 0){
  occurencesMtChoke <- sp::spsample(MtChoke, 
              round(PopulationPatchDF["MtChoke", "NewPointsToGenerate"],0),
              type = "random",
              iter = 25)
}

# MtGosh
if(round(PopulationPatchDF["MtGosh", "NewPointsToGenerate"],0) > 0){
  occurencesMtGosh <- sp::spsample(MtGosh, 
              round(PopulationPatchDF["MtGosh", "NewPointsToGenerate"],0),
              type = "random",
              iter = 25)
}

# MtMenz
if(round(PopulationPatchDF["MtMenz", "NewPointsToGenerate"],0) > 0){
  occurencesMtMenz <- sp::spsample(MtMenz, 
                                    round(PopulationPatchDF["MtMenz", "NewPointsToGenerate"],0),
                                    type = "random",
                                    iter = 25)
}

# Now we have to write the generated points to the species occurences csv

# Create point csv directory
if(!dir.exists("data/points")){
  dir.create("data/points")
}

# BaleMountains
if(exists(x = "occurencesBaleMountains")){
  write.table(occurencesBaleMountains, file = "data/points/BaleMountains.csv", row.names = F, sep = ",")
  BaleMountainsFile <- read.csv("data/points/BaleMountains.csv")
  BaleMountainsFile$species <- "Canis simensis"
  colnames(BaleMountainsFile) <- c("Longitude", "Latitude","species")
  BaleMountainsFile <- BaleMountainsFile[,c(3,1,2)] 
  OccurencesPoints <- rbind(occurenceGBIF, BaleMountainsFile)
}

# ArsiMountains
if(exists(x = "occurencesArsiMountains")){
  write.table(occurencesArsiMountains, file = "data/points/ArtsiMountains.csv", row.names = F, sep = ",")
  ArsiMountainsFile <- read.csv("data/points/ArtsiMountains.csv")
  ArsiMountainsFile$species <- "Canis simensis"
  colnames(ArsiMountainsFile) <- c("Longitude", "Latitude","species")
  ArsiMountainsFile <- ArsiMountainsFile[,c(3,1,2)] 
  OccurencesPoints <- rbind(occurenceGBIF, ArsiMountainsFile)
}


# SimienNP
if(exists(x = "occurencesSimienNP")){
  write.table(occurencesSimienNP, file = "data/points/SimienNP.csv", row.names = F, sep = ",")
  SimienNPFile <- read.csv("data/points/SimienNP.csv")
  SimienNPFile$species <- "Canis simensis"
  colnames(SimienNPFile) <- c("Longitude", "Latitude","species")
  SimienNPFile <- SimienNPFile[,c(3,1,2)] 
  OccurencesPoints <- rbind(OccurencesPoints, SimienNPFile)
}

# SimienNP
if(exists(x = "occurencesSimienNP")){
  write.table(occurencesArsiMountains, file = "data/points/SimienNP.csv", row.names = F, sep = ",")
  SimienNPFile <- read.csv("data/points/SimienNP.csv")
  SimienNPFile$species <- "Canis simensis"
  colnames(SimienNPFile) <- c("Longitude", "Latitude","species")
  SimienNPFile <- SimienNPFile[,c(3,1,2)] 
  OccurencesPoints <- rbind(OccurencesPoints, SimienNPFile)
}

# NorthWollo
if(exists(x = "occurencesNorthWollo")){
  write.table(occurencesNorthWollo, file = "data/points/NorthWollo.csv", row.names = F, sep = ",")
  NorthWolloFile <- read.csv("data/points/NorthWollo.csv")
  NorthWolloFile$species <- "Canis simensis"
  colnames(NorthWolloFile) <- c("Longitude", "Latitude","species")
  NorthWolloFile <- NorthWolloFile[,c(3,1,2)] 
  OccurencesPoints <- rbind(OccurencesPoints, NorthWolloFile)
}

# SouthWollo
if(exists(x = "occurencesSouthWollo")){
  write.table(occurencesSouthWollo, file = "data/points/SouthWollo.csv", row.names = F, sep = ",")
  SouthWolloFile <- read.csv("data/points/SouthWollo.csv")
  SouthWolloFile$species <- "Canis simensis"
  colnames(SouthWolloFile) <- c("Longitude", "Latitude","species")
  SouthWolloFile <- SouthWolloFile[,c(3,1,2)] 
  OccurencesPoints <- rbind(OccurencesPoints, SouthWolloFile)
}

# MtGuna
if(exists(x = "occurencesMtGuna")){
  write.table(occurencesMtGuna, file = "data/points/MtGuna.csv", row.names = F, sep = ",")
  MtGunaFile <- read.csv("data/points/MtGuna.csv")
  MtGunaFile$species <- "Canis simensis"
  colnames(MtGunaFile) <- c("Longitude", "Latitude","species")
  MtGunaFile <- MtGunaFile[,c(3,1,2)] 
  OccurencesPoints <- rbind(OccurencesPoints, MtGunaFile)
}

# MtChoke
if(exists(x = "occurencesMtChoke")){
  write.table(occurencesMtChoke, file = "data/points/MtChoke.csv", row.names = F, sep = ",")
  MtChokeFile <- read.csv("data/points/MtChoke.csv")
  MtChokeFile$species <- "Canis simensis"
  colnames(MtChokeFile) <- c("Longitude", "Latitude","species")
  MtChokeFile <- MtChokeFile[,c(3,1,2)] 
  OccurencesPoints <- rbind(OccurencesPoints, MtChokeFile)
}

# MtGosh
if(exists(x = "occurencesMtGosh")){
  write.table(occurencesMtGosh, file = "data/points/MtGosh.csv", row.names = F, sep = ",")
  MtGoshFile <- read.csv("data/points/MtGosh.csv")
  MtGoshFile$species <- "Canis simensis"
  colnames(MtGoshFile) <- c("Longitude", "Latitude","species")
  MtGoshFile <- MtGoshFile[,c(3,1,2)] 
  OccurencesPoints <- rbind(OccurencesPoints, MtGoshFile)
}

# MtMenz
if(exists(x = "occurencesMtMenz")){
  write.table(occurencesMtMenz, file = "data/points/MtMenz.csv", row.names = F, sep = ",")
  MtMenzFile <- read.csv("data/points/MtMenz.csv")
  MtMenzFile$species <- "Canis simensis"
  colnames(MtMenzFile) <- c("Longitude", "Latitude","species")
  MtMenzFile <- MtMenzFile[,c(3,1,2)] 
  OccurencesPoints <- rbind(OccurencesPoints, MtMenzFile)
}

# Check OccurencesPoints dataset
plot(Ethiopia)
points(OccurencesPoints$Longitude, OccurencesPoints$Latitude)

