###############################################
##### Script for "Data manipulation in R" #####
###############################################

##### Preliminary 1: Set up workspace ###########

# Set up a new project in R Studio
# Where is your working directory?
# Are the files you need in your working directory?

##### Preliminary 2: Data objects in R ###############

# define a vector of the numerical values 1 to 27 and assign to an object called x
x <- 1:27

# define a matrix (assign to y) with 9 rows and 3 columns using the data in x
y <- matrix(x, 9, 3)

# extract a vector from y using the numerical indices in x
y[2,3]
y[x]
# define a 3-dimensional array (assign to z) with 3 rows, 3 columns and 3 layers, using the data in x
z <- array(x, dim = c(3,3,3))

# browse the datasets available in R, find Edgar Anderson's Iris data and load it as a dataframe 
data()
iris <- data.frame(iris)

# convert the iris dataset to a matrix and check the data types
iris2 <- as.matrix(iris)

# using matrices instead of data frames 
# when data is all numbers (except for row and column names), I normally use matrices rather than dataframes

##### Exercise 1: Reading data ##################

# Use the read.table function to read:
# observations.txt, elevation.txt, species.txt and birdlife.txt
birdlife <- read.table("birdlife.txt", sep = "\t", stringsAsFactors = F, header = T)
elev <- read.delim("elevation.txt", stringsAsFactors = F, header = T)
species <- read.delim("species.txt", stringsAsFactors = F, header = T)
observation <- read.delim("observations.txt", stringsAsFactors = F, header = T)
## a brief discussion on normalisation

##### Exercise 2: Subsetting data #############

# How many observations have an abundance >5?
nrow(observation[observation$Abundance > 5,])

length(which(observation$Abundance > 5))

# What is the average elevation of Gloucester Tops sites? Gloucester Tops sites are prefaced by "G".

# We could just use the numeric indices
mean(elev$Elevation[15:29])
# a more robust solution using grep
mean(elev$Elevation[grep(pattern = "G", x = elev$Site)])

# What is the maximum bill length of insectivores?
exp(max(species$Bill.length[which(species$Guild == "Insectivore")]))

##### Exercise 3: Aggregating data ############

# produce a table of mean values for each trait for each feeding guild
aggregate(species[, 5:11], by =  list(species$Guild), FUN = mean)
# produce a table of mean values for each trait for each Family
aggregate(species[, 5:11], by =  list(species$Family), FUN = mean)
# produce a species list from the observation data
unique(observation$Species)

# assign sites to 250m elevation bands (using modulo arithmetic)
elev$Elevation%/%250
elev$Elevation%%250

##### Exercise 4: Tabulating data #############

# produce an incidence (presence/absence) matrix of all bird species for all sites
incidence <- table(observation$Site, observation$Species)
incidence[incidence >1] <- 1

# produce an abundance matrix of all bird species for all sites
abundance <- tapply(observation$Abundance, INDEX = list(observation$Site, observation$Species), FUN = sum)
abundance[is.na(abundance)] <- 0

#proportional abundance matrix
p <- abundance[1,]/sum(abundance[1,])

# using an apply function on the abundance table
apply(abundance, 1, sum, na.rm = T)

# first let's define an inverse simpson function

##### Exercise 5: Matching data ###############

# check the species list against the observation data
species$Common.Name%in%observation$Species
unique(observation$Species)%in%species$Common.Name

species$Common.Name[!species$Common.Name%in%observation$Species]
unique(observation$Species[!observation$Species%in%species$Common.Name])

observation$Species[observation$Species=="Australian King Parrot"] <- "Australian King-Parrot"
observation$Species[observation$Species=="Brown Cuckoo-dove"] <- "Brown Cuckoo-Dove"
observation$Species[observation$Species=="Glossy Black-cockatoo"] <- "Glossy Black-Cockatoo"
observation$Species[observation$Species=="Wompoo Fruit-dove"] <- "Wompoo Fruit-Dove"

# check the species list against the Birdlife International official list
species$Scientific.Name%in%birdlife$Scientific.name
species$Scientific.Name[!species$Scientific.Name%in%birdlife$Scientific.name]
species$Scientific.Name[species$Scientific.Name=="Leucosarcia picata"] <- "Leucosarcia melanoleuca" # we can fix this name


##### Exercise 6: Sorting data ################

# sort the species data by feeding guild
sort(species$Guild)
order(species$Guild)

species[order(species$Guild),]

##### Exercise 7: Merging data ################

# produce a table of all observations with columns for elevation, scientific name, taxonomic status (Birdlife), family and feeding guild
obs <- merge(observation, elev)
# produce an abundance matrix of all feeding guilds for all sites for those species where taxonomic status is "R".
obs <- merge(obs, species[,1:4], by.x = "Species", by.y = "Common.Name")
# produce an incidence matrix of all bird families for all 250m elevation bands

table(obs$Elevation%/%250, obs$Family)

##### Exercise 8: Random sampling of data ############

# produce a 5x5 submatrix from the abundance table using random sampling
abundance[sample(1:nrow(abundance), 5), sample(1:ncol(abundance), 5)]

