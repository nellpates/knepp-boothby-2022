# Knepp and Boothby 2022 write up
# Nell Pates
# Set up and data clean-up ready for analyses
# 14 Dec 2023 - FINAL

# -----------------------------------------------------------------------------#
#### SETUP: ####
# -----------------------------------------------------------------------------#

# set working directory, then:
source("src/headers.R") # load all required packages

# Load data
knepp <- read.csv("raw-data/k-plants.csv") # raw data from Knepp surveys
boothby <- read.csv("raw-data/b-plants.csv") # raw data from Boothby surveys
tree <- read.tree("raw-data/Vascular_Plants_rooted.dated.tre") # Zanne et al (2014) phylogeny 

# Initial format and get rid of  rows that contain *s
# KNEPP
knepp$Species <- stringi::stri_trans_general(knepp$Species, "latin-ascii")
knepp$Species <- gsub(" ", "_", knepp$Species, fixed=TRUE) # replaces spaces with _ to match across data sets
knepp[is.na(knepp)] <- 0
knepp <- knepp %>% 
  filter(! grepl('\\*', Plot))

# BOOTHBY
boothby$Species <- stringi::stri_trans_general(boothby$Species, "latin-ascii")
boothby$Species <-gsub(" ", "_", boothby$Species, fixed=TRUE)
boothby[is.na(boothby)] <- 0
boothby <- boothby %>% 
  filter(! grepl('\\*', Plot))

# "Build" phylogenies
ktree <- congeneric.merge(tree, unique(knepp$Species)) # adds missing species to phylogeny
btree <- congeneric.merge(tree, unique(boothby$Species))

# Format for pez etc. - creates comparative data objects
#KNEPP
k.comm <- with(knepp, tapply(Cover, list(paste(Site,Block,Group,Plot), Species), mean, rm.na=TRUE))
k.comm[is.na(k.comm)] <- 0
k.c.data <- comparative.comm(ktree, k.comm)

#BOOTHBY
b.comm <- with(boothby, tapply(Cover, list(paste(Site,Block,Plot), Species), mean, rm.na=TRUE))
b.comm[is.na(b.comm)] <- 0
b.c.data <- comparative.comm(btree, b.comm)

# Make hierarchical groupings for each site
# Grouping sites into nested arrangements (block, major, minor etc)

#KNEPP
raw.groups <- strsplit(sites(k.c.data), " ")
site.no <- sapply(raw.groups, function(x) x[4])
env <- data.frame(
  block = sapply(raw.groups, function(x) x[2]),
  fractal = sapply(raw.groups, function(x) x[3]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)

env$fractal[env$fractal=="NA"] <- "A" # R is seeing "NA" and sometimes coercing to the concept of an NA
rownames(env) <- sites(k.c.data)
write.csv(env, file = "kneppfile.csv")

# A manual way to identify which sites 'belong' to which groups in the southern block
# where fractals X, Y, and Z overlap (1 indicates inclusion in the grouping) 
# see section below: Dealing with nesting at Knepp
env$X <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0)
env$Y <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,1,0)
env$Z <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1)

# Uncomment below to check the site names, species dropped, and species included
#sites(k.c.data) 
#k.c.data$dropped$comm.sp.lost
#species(k.c.data)

# Subsetting to update object 'knepp' - losing 'sticks', 'bare soil' etc
k_wanted_spp <- species(k.c.data)
knepp <- subset(knepp, Species %in% k_wanted_spp)
write.csv(knepp, file = "clean-data/knepp_clean.csv")
knepp <- read.csv("clean-data/knepp_clean.csv")

# Merge everything in a comparative.comm dataset 
k.c.data <- comparative.comm(tree(k.c.data), comm(k.c.data), env=env)
saveRDS(k.c.data, "clean-data/k.c.data.RDS")

# BOOTHBY
raw.groups <- strsplit(sites(b.c.data), " ")
site.no <- sapply(raw.groups, function(x) x[3])
env <- data.frame(
  fractal = sapply(raw.groups, function(x) x[2]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)
env$fractal[env$fractal=="NA"] <- "A" 
rownames(env) <- sites(b.c.data)

#sites(b.c.data) 
#b.c.data$dropped$comm.sp.lost
#species(b.c.data)

b_wanted_spp <- species(b.c.data)
boothby <- subset(boothby, Species %in% b_wanted_spp)

write.csv(boothby, file = "clean-data/boothby_clean.csv")
boothby <- read.csv("clean-data/boothby_clean.csv")
# Merge everything in a compative.comm dataset 
b.c.data <- comparative.comm(tree(b.c.data), comm(b.c.data), env=env)
saveRDS(b.c.data, "clean-data/b.c.data.RDS")


# -----------------------------------------------------------------------------#
#### DEALING WITH NESTING AT KNEPP: ####
# -----------------------------------------------------------------------------#

# Starting with the clean k.c.data from above
k.c.data <- readRDS("clean-data/k.c.data.RDS")

# Duplicate the sites 
k.c.data$comm <- rbind(k.c.data$comm, k.c.data$comm["Knepp S X 222",])
# changing row name:
row.names(k.c.data$comm)[33] <- "Knepp S Y 111" 
# Duplicate the environmental entries
k.c.data$env <- rbind(k.c.data$env, k.c.data$env["Knepp S X 222",])
# Set the new duplicate to have the right entries in the environmental dataset
k.c.data$env[nrow(k.c.data$env),] <- c("S", "Y", "1", "1", "1", "1", "0")
# changing row name:
row.names(k.c.data$env)[33] <- "Knepp S Y 111" 

# Repeat for the other 2 duplicate sites:
k.c.data$comm <- rbind(k.c.data$comm, k.c.data$comm["Knepp S X 333",])
row.names(k.c.data$comm)[34] <- "Knepp S Z 111" 
k.c.data$env <- rbind(k.c.data$env, k.c.data$env["Knepp S X 333",])
k.c.data$env[nrow(k.c.data$env),] <- c("S", "Z", "1", "1", "1", "0", "1")
row.names(k.c.data$env)[34] <- "Knepp S Z 111" 

k.c.data$comm <- rbind(k.c.data$comm, k.c.data$comm["Knepp S Z 222",])
row.names(k.c.data$comm)[35] <- "Knepp S Y 333" 
k.c.data$env <- rbind(k.c.data$env, k.c.data$env["Knepp S Z 222",])
k.c.data$env[nrow(k.c.data$env),] <- c("S", "Y", "3", "3", "0", "1", "1")
row.names(k.c.data$env)[35] <- "Knepp S Y 333"

k.c.data <- comparative.comm(tree(k.c.data), comm(k.c.data), env=k.c.data$env) 
saveRDS(k.c.data, "clean-data/k.c.data.RDS")
