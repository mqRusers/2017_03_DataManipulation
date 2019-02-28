###############################################
##### Script for "Data manipulation in R" #####
###############################################

# originally prepared by David Nipperess

##### Preliminary 1: Set up workspace ###########

# Set up a new project in R Studio
# Where is your working directory?
# Are the files you need in your working directory?

##### Preliminary 2: Data objects in R ###############

# define a vector of the numerical values 1 to 27 and assign to an object called x

x <- 1:27
x

# define a matrix (assign to y) with 9 rows and 3 columns using the data in x

y <- matrix(data=x,nrow=9,ncol=3)
y

# extract a vector from y using the numerical indices in x

y[x]  # referencing an object with another object

# define a 3-dimensional array (assign to z) with 3 rows, 3 columns and 3 layers, using the data in x

z <- array(data=x,dim=c(3,3,3))
z
z[x]
z[rev(x)] # What does the function rev() do? How can you find out?

# browse the datasets available in R, find Edgar Anderson's Iris data and load it as a dataframe 

data()
data(iris)
head(iris)
dim(iris)
#view(iris)

# convert the iris dataset to a matrix and check the data types

iris2 <- as.matrix(iris)
head(iris2)
mode(iris2)

iris3 <- as.matrix(iris[,1:4])
head(iris3)
mode(iris3)

## using matrices instead of data frames 

# when data is all numbers (except for row and column names), I normally use matrices rather than dataframes
# if you have row names in your data file, and you want to convert to a matrix, you will need to specify which column has the row names (see the row.names argument in the read.table function)

##### Exercise 1: Reading data ##################

# Use the read.table function to read:
# observations.txt, elevation.txt, species.txt and birdlife.txt

### inspect your data BEFORE attempting to import into R 

observations <- read.table("./data/observations.txt",header=TRUE,sep="\t",as.is=TRUE) # using the generic function

observations <- read.delim("./data/observations.txt",header=TRUE,as.is=TRUE) # using the function specifically for tab-delimited files
elevation <- read.delim("./data/elevation.txt",header=TRUE,as.is=TRUE)
species <- read.delim("./data/species.txt",header=TRUE,as.is=TRUE)
birdlife <- read.delim("./data/birdlife.txt",header=TRUE,as.is=TRUE)

## a brief discussion on normalisation

##### Exercise 2: Subsetting data #############

# How many observations have an abundance >5?

observations[(observations$Abundance>5),]
length(observations$Abundance>5) # don't work
length(which(observations$Abundance>5))
sum(which(observations$Abundance>5))

# What is the average elevation of Gloucester Tops sites? Gloucester Tops sites are prefaced by "G".

# We could just use the numeric indices

elevation
elevation[15:29,] # are the sites in rows 15 through 29
mean(elevation[15:29,2])

# a more robust solution
?grep
grep("G",elevation$Site)
mean(elevation$Elevation[grep("G",elevation$Site)])

# What is the maximum bill length of insectivores?

max(species$Bill.length.log)
species[species$Guild=="Insectivore",]
max(species$Bill.length.log[species$Guild=="Insectivore"])

##### Exercise 3: Aggregating data ############

# produce a table of mean values for each trait for each feeding guild

?aggregate
aggregate(species,by=list(species$Guild),FUN=mean) # produces warnings because we are taking the mean of character vectors
warnings() # why is there 20 warnings?
aggregate(species[,6:11],by=list(species$Guild),FUN=mean)

# produce a table of mean values for each trait for each Family

aggregate(species[,6:11],by=list(species$Family),FUN=mean)

# produce a species list from the observation data

?unique
unique(observations$Species)

# assign sites to 250m elevation bands (using modulo arithmetic)

?Arithmetic
elevation$Elevation%/%250
plot(elevation$Elevation%/%250,elevation$Elevation)

##### Exercise 4: Tabulating data #############

# produce an incidence (presence/absence) matrix of all bird species for all sites

?table
incidence_table <- table(observations$Site,observations$Species)
incidence_table[1:5,1:5]
max(incidence_table)

# observations[(observations$Species == 'Yellow-throated Scrubwren'),] # examples of why max is 2

incidence_table[incidence_table>1]
incidence_table[incidence_table>1] <- 1
# ifelse(incidence_table>0,yes=1,no=0) # an alternative way to do find and replace
max(incidence_table)

# produce an abundance matrix of all bird species for all sites

?tapply
abundance_table <- tapply(observations$Abundance,list(observations$Site,observations$Species),FUN="sum")
abundance_table[1:5,1:5]
is.na(abundance_table)
abundance_table[is.na(abundance_table)] <- 0
abundance_table[1:5,1:5] # now an absence is what we want it to be (0) instead of NA.

# using an apply function on the abundance table


# first let's define an inverse simpson function
# sum of the proportional abundances squared
# https://en.wikipedia.org/wiki/Diversity_index#Inverse_Simpson_index

# convert the matrix to proportional abundances
p <- abundance_table/sum(abundance_table) # this doesn't work
sum(p) # this is why it doesn't work
p <- abundance_table[1,]/sum(abundance_table[1,])
p
sum(p)
simp <- sum(p^2)
simp
isimp <- 1/sum(p^2)
isimp

# lets wrap that as a function
# needs: x a list of abundances
# returns: inverse simpson index
invsimp <- function(x) {
  p <- x/sum(x)
  invsimp <- 1/sum(p^2)
}

# double check that this works
isimp2 <- invsimp(abundance_table[1,])
isimp2

# get site diversity for every site with just one line of code!
site_diversity <- apply(abundance_table,MARGIN=1,FUN=invsimp)
site_diversity

##### Exercise 5: Matching data ###############

# check the species list against the observation data

species$Common.Name%in%observations$Species
observations$Species%in%species$Common.Name
!species$Common.Name%in%observations$Species
!observations$Species%in%species$Common.Name
species$Common.Name[!species$Common.Name%in%observations$Species]
observations$Species[!observations$Species%in%species$Common.Name]

observations$Species[observations$Species=="Australian King Parrot"] <- "Australian King-Parrot"
observations$Species[observations$Species=="Brown Cuckoo-dove"] <- "Brown Cuckoo-Dove"
observations$Species[observations$Species=="Glossy Black-cockatoo"] <- "Glossy Black-Cockatoo"
observations$Species[observations$Species=="Wompoo Fruit-dove"] <- "Wompoo Fruit-Dove"

# check the species list against the Birdlife International official list

species$Scientific.Name%in%birdlife$Scientific.name
species$Scientific.Name[!species$Scientific.Name%in%birdlife$Scientific.name]
species$Scientific.Name%in%birdlife$Scientific.name[birdlife$Status=="R"]
species$Scientific.Name[!species$Scientific.Name%in%birdlife$Scientific.name[birdlife$Status=="R"]]

species$Scientific.Name[species$Scientific.Name=="Leucosarcia picata"] <- "Leucosarcia melanoleuca" # we can fix this name
species$Scientific.Name[!species$Scientific.Name%in%birdlife$Scientific.name] # should return 0 if no problems
species$Scientific.Name[!species$Scientific.Name%in%birdlife$Scientific.name[birdlife$Status=="R"]] # we will leave this one as is

##### Exercise 6: Sorting data ################

# sort the species data by feeding guild

?sort
sort(species$Guild) # simple option for vectors
order(species$Guild) # this returns index numbers instead of values. This is useful if you need to sort a dataframe or matrix by the values in one column.
species[order(species$Guild),]
species$Common.Name[order(species$Guild)]

##### Exercise 7: Merging data ################

# produce a table of all observations with columns for elevation, scientific name, taxonomic status (Birdlife), family and feeding guild

?merge
new_obs <- merge(observations,elevation) # don't need "by" argument
new_obs <- merge(new_obs,species[,1:4],by.x="Species",by.y="Common.Name")
new_obs <- merge(new_obs,birdlife,by.x="Scientific.Name",by.y="Scientific.name")

head(new_obs) # produce an abundance matrix of all feeding guilds for all sites for those species where taxonomic status is "R".

guild_abundance_table <- tapply(new_obs$Abundance,list(new_obs$Site,new_obs$Guild),FUN="sum")
guild_abundance_table[is.na(guild_abundance_table)] <- 0

# produce an incidence matrix of all bird families for all 250m elevation bands

bands <- new_obs$Elevation%/%250
family_incidence_table <- table(bands,new_obs$Family)
family_incidence_table[family_incidence_table>1] <- 1

##### Exercise 8: Random sampling of data ############

?sample
sample(1:100,size=5,replace=FALSE)

# produce a 5x5 submatrix from the abundance table using random sampling

?nrow
submat <- abundance_table[sample(1:nrow(abundance_table),size=5),sample(1:ncol(abundance_table),size=5)]
submat