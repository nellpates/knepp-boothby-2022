# Knepp and Boothby write up
# Nell Pates
# Set up and data clean-up ready for analyses
# 2 September 2024 - FINAL
# -----------------------------------------------------------------------------#
#### SETUP: ####
# -----------------------------------------------------------------------------#

#setwd("kb all data") # set working directory
source("src/headers.R") # load all required packages

# Load data from Knepp and Boothby surveys: 2022, 2023, 2024
knepp2022 <- read.csv("raw-data/knepp2022.csv") 
boothby2022 <- read.csv("raw-data/boothby2022.csv")

knepp2023 <- read.csv("raw-data/knepp2023.csv") 
boothby2023 <- read.csv("raw-data/boothby2023.csv")

knepp2024 <- read.csv("raw-data/knepp2024.csv") 
boothby2024 <- read.csv("raw-data/boothby2024.csv")
tree <- read.tree("raw-data/Vascular_Plants_rooted.dated.tre") # Zanne et al (2014) phylogeny 

# Initial format and get rid of  rows that contain *s

# KNEPP 2022

knepp2022$Species <- stringi::stri_trans_general(knepp2022$Species, "latin-ascii")
knepp2022$Species <- gsub(" ", "_", knepp2022$Species, fixed=TRUE) # replaces spaces with _ to match across data sets
knepp2022[is.na(knepp2022)] <- 0
knepp2022 <- knepp2022 %>% 
  filter(! grepl('\\*', Plot))

# KNEPP 2023

knepp2023$Species <- stringi::stri_trans_general(knepp2023$Species, "latin-ascii")
knepp2023$Species <- gsub(" ", "_", knepp2023$Species, fixed=TRUE) # replaces spaces with _ to match across data sets
knepp2023[is.na(knepp2023)] <- 0
knepp2023 <- knepp2023 %>% 
  filter(! grepl('\\*', Plot))

# KNEPP 2024

knepp2024$Species <- stringi::stri_trans_general(knepp2024$Species, "latin-ascii")
knepp2024$Species <- gsub(" ", "_", knepp2024$Species, fixed=TRUE) # replaces spaces with _ to match across data sets
knepp2024[is.na(knepp2024)] <- 0
knepp2024 <- knepp2024 %>% 
  filter(! grepl('\\*', Plot))

# BOOTHBY 2022

boothby2022$Species <- stringi::stri_trans_general(boothby2022$Species, "latin-ascii")
boothby2022$Species <-gsub(" ", "_", boothby2022$Species, fixed=TRUE)
boothby2022[is.na(boothby2022)] <- 0
boothby2022 <- boothby2022 %>% 
  filter(! grepl('\\*', Plot))

# BOOTHBY 2023

boothby2023$Species <- stringi::stri_trans_general(boothby2023$Species, "latin-ascii")
boothby2023$Species <-gsub(" ", "_", boothby2023$Species, fixed=TRUE)
boothby2023[is.na(boothby2023)] <- 0
boothby2023 <- boothby2023 %>% 
  filter(! grepl('\\*', Plot))

# BOOTHBY 2024

boothby2024$Species <- stringi::stri_trans_general(boothby2024$Species, "latin-ascii")
boothby2024$Species <-gsub(" ", "_", boothby2024$Species, fixed=TRUE)
boothby2024[is.na(boothby2024)] <- 0
boothby2024 <- boothby2024 %>% 
  filter(! grepl('\\*', Plot))

# "Build" phylogenies
ktree2022 <- congeneric.merge(tree, unique(knepp2022$Species)) # adds missing species to phylogeny
ktree2023 <- congeneric.merge(tree, unique(knepp2023$Species)) 
ktree2024 <- congeneric.merge(tree, unique(knepp2024$Species)) 
btree2022 <- congeneric.merge(tree, unique(boothby2022$Species))
btree2023 <- congeneric.merge(tree, unique(boothby2023$Species))
btree2024 <- congeneric.merge(tree, unique(boothby2024$Species))

# Format for pez etc. - creates comparative data objects
#KNEPP 2022

k.comm2022 <- with(knepp2022, tapply(Cover, list(paste(Site,Block,Group,Plot), Species), mean, rm.na=TRUE))
k.comm2022[is.na(k.comm2022)] <- 0
k.c.data2022 <- comparative.comm(ktree2022, k.comm2022)

#KNEPP 2023

k.comm2023 <- with(knepp2023, tapply(Cover, list(paste(Site,Block,Group,Plot), Species), mean, rm.na=TRUE))
k.comm2023[is.na(k.comm2023)] <- 0
k.c.data2023 <- comparative.comm(ktree2023, k.comm2023)

#KNEPP 2024

k.comm2024 <- with(knepp2024, tapply(Cover, list(paste(Site,Block,Group,Plot), Species), mean, rm.na=TRUE))
k.comm2024[is.na(k.comm2024)] <- 0
k.c.data2024 <- comparative.comm(ktree2024, k.comm2024)

#BOOTHBY 2022

b.comm2022 <- with(boothby2022, tapply(Cover, list(paste(Site,Block,Plot), Species), mean, rm.na=TRUE))
b.comm2022[is.na(b.comm2022)] <- 0
b.c.data2022 <- comparative.comm(btree2022, b.comm2022)

#BOOTHBY 2023

b.comm2023 <- with(boothby2023, tapply(Cover, list(paste(Site,Block,Plot), Species), mean, rm.na=TRUE))
b.comm2023[is.na(b.comm2023)] <- 0
b.c.data2023 <- comparative.comm(btree2023, b.comm2023)

#BOOTHBY 2024

b.comm2024 <- with(boothby2024, tapply(Cover, list(paste(Site,Block,Plot), Species), mean, rm.na=TRUE))
b.comm2024[is.na(b.comm2024)] <- 0
b.c.data2024 <- comparative.comm(btree2024, b.comm2024)

# Uncomment below to check the site names, species dropped, and species included
sites(k.c.data2022) 
k.c.data2022$dropped$comm.sp.lost
species(k.c.data2022)

sites(k.c.data2023) 
k.c.data2023$dropped$comm.sp.lost
species(k.c.data2023)

sites(k.c.data2024) 
k.c.data2024$dropped$comm.sp.lost
species(k.c.data2024)

sites(b.c.data2022) 
b.c.data2022$dropped$comm.sp.lost
species(b.c.data2022)

sites(b.c.data2023) 
b.c.data2023$dropped$comm.sp.lost
species(b.c.data2023)

sites(b.c.data2024) 
b.c.data2024$dropped$comm.sp.lost
species(b.c.data2024)

# Make hierarchical groupings for each site
# Grouping sites into nested arrangements (block, major, minor etc)

#KNEPP 2022

raw.groups <- strsplit(sites(k.c.data2022), " ")
site.no <- sapply(raw.groups, function(x) x[4])
env <- data.frame(
  block = sapply(raw.groups, function(x) x[2]),
  fractal = sapply(raw.groups, function(x) x[3]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)

env$fractal[env$fractal=="NA"] <- "A" # R is seeing "NA" and sometimes coercing to the concept of an NA
rownames(env) <- sites(k.c.data2022)

# A manual way to identify which sites 'belong' to which groups in the southern block
# where fractals X, Y, and Z overlap (1 indicates inclusion in the grouping) 
# see section below: Dealing with nesting at Knepp2022
env$X <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0)
env$Y <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,1,0)
env$Z <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1)

# Subsetting to update 'knepp' objects - losing 'sticks', 'bare soil' etc
k_wanted_spp2022 <- species(k.c.data2022)
knepp2022 <- subset(knepp2022, Species %in% k_wanted_spp2022)
write.csv(knepp2022, file = "clean-data/knepp_clean2022.csv")
knepp2022 <- read.csv("clean-data/knepp_clean2022.csv")

# Merge everything in a comparative.comm datasets

k.c.data2022 <- comparative.comm(tree(k.c.data2022), comm(k.c.data2022), env=env)
saveRDS(k.c.data2022, "clean-data/k.c.data2022.RDS")

#KNEPP 2023

raw.groups <- strsplit(sites(k.c.data2023), " ")
site.no <- sapply(raw.groups, function(x) x[4])
env <- data.frame(
  block = sapply(raw.groups, function(x) x[2]),
  fractal = sapply(raw.groups, function(x) x[3]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)

env$fractal[env$fractal=="NA"] <- "A" # R is seeing "NA" and sometimes coercing to the concept of an NA
rownames(env) <- sites(k.c.data2023)

env$X <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
           1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0)
env$Y <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,
            0,0,1,0,0,0,0,0,0,0)
env$Z <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,1,
            1,1,1,1,1,1,1,1,1,1)

k_wanted_spp2023 <- species(k.c.data2023)
knepp2023 <- subset(knepp2023, Species %in% k_wanted_spp2023)
write.csv(knepp2023, file = "clean-data/knepp_clean2023.csv")
knepp2023 <- read.csv("clean-data/knepp_clean2023.csv")

k.c.data2023 <- comparative.comm(tree(k.c.data2023), comm(k.c.data2023), env=env)
saveRDS(k.c.data2023, "clean-data/k.c.data2023.RDS")

#KNEPP 2024

raw.groups <- strsplit(sites(k.c.data2024), " ")
site.no <- sapply(raw.groups, function(x) x[4])
env <- data.frame(
  block = sapply(raw.groups, function(x) x[2]),
  fractal = sapply(raw.groups, function(x) x[3]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)

env$fractal[env$fractal=="NA"] <- "A" # R is seeing "NA" and sometimes coercing to the concept of an NA
rownames(env) <- sites(k.c.data2024)

env$X <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
           1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0)
env$Y <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,
           0,0,0,0,1,0,0,0,0,0,0)
env$Z <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,
           1,1,1,1,1,1,1,1,1,1,1)

k_wanted_spp2024 <- species(k.c.data2024)
knepp2024 <- subset(knepp2024, Species %in% k_wanted_spp2024)
write.csv(knepp2024, file = "clean-data/knepp_clean2024.csv")
knepp2024 <- read.csv("clean-data/knepp_clean2024.csv")

k.c.data2024 <- comparative.comm(tree(k.c.data2024), comm(k.c.data2024), env=env)
saveRDS(k.c.data2024, "clean-data/k.c.data2024.RDS")

# BOOTHBY 2022

raw.groups <- strsplit(sites(b.c.data2022), " ")
site.no <- sapply(raw.groups, function(x) x[3])
env <- data.frame(
  fractal = sapply(raw.groups, function(x) x[2]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)
env$fractal[env$fractal=="NA"] <- "A" 
rownames(env) <- sites(b.c.data2022)

b_wanted_spp2022 <- species(b.c.data2022)
boothby2022 <- subset(boothby2022, Species %in% b_wanted_spp2022)

write.csv(boothby2022, file = "clean-data/boothby_clean2022.csv")
boothby <- read.csv("clean-data/boothby_clean2022.csv")

# Merge everything in a compative.comm dataset 
b.c.data2022 <- comparative.comm(tree(b.c.data2022), comm(b.c.data2022), env=env)
saveRDS(b.c.data2022, "clean-data/b.c.data2022.RDS")

# BOOTHBY 2023

raw.groups <- strsplit(sites(b.c.data2023), " ")
site.no <- sapply(raw.groups, function(x) x[3])
env <- data.frame(
  fractal = sapply(raw.groups, function(x) x[2]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)
env$fractal[env$fractal=="NA"] <- "A" 
rownames(env) <- sites(b.c.data2023)

b_wanted_spp2023 <- species(b.c.data2023)
boothby2023 <- subset(boothby2023, Species %in% b_wanted_spp2023)

write.csv(boothby2023, file = "clean-data/boothby_clean2023.csv")
boothby <- read.csv("clean-data/boothby_clean2023.csv")

b.c.data2023 <- comparative.comm(tree(b.c.data2023), comm(b.c.data2023), env=env)
saveRDS(b.c.data2023, "clean-data/b.c.data2023.RDS")

# BOOTHBY 2024

raw.groups <- strsplit(sites(b.c.data2024), " ")
site.no <- sapply(raw.groups, function(x) x[3])
env <- data.frame(
  fractal = sapply(raw.groups, function(x) x[2]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)
env$fractal[env$fractal=="NA"] <- "A" 
rownames(env) <- sites(b.c.data2024)

b_wanted_spp2024 <- species(b.c.data2024)
boothby2024 <- subset(boothby2024, Species %in% b_wanted_spp2024)

write.csv(boothby2024, file = "clean-data/boothby_clean2024.csv")
boothby <- read.csv("clean-data/boothby_clean2024.csv")

b.c.data2024 <- comparative.comm(tree(b.c.data2024), comm(b.c.data2024), env=env)
saveRDS(b.c.data2024, "clean-data/b.c.data2024.RDS")

# -----------------------------------------------------------------------------#
#### DEALING WITH NESTING AT KNEPP: ####
# -----------------------------------------------------------------------------#

# 2022

# Starting with the clean k.c.data from above
k.c.data2022 <- readRDS("clean-data/k.c.data2022.RDS")
k.c.data2022$comm

# Duplicate the sites 
k.c.data2022$comm <- rbind(k.c.data2022$comm, k.c.data2022$comm["Knepp S X 222",])
# changing row name:
row.names(k.c.data2022$comm)[33] <- "Knepp S Y 111" 
# Duplicate the environmental entries
k.c.data2022$env <- rbind(k.c.data2022$env, k.c.data2022$env["Knepp S X 222",])
# Set the new duplicate to have the right entries in the environmental dataset
k.c.data2022$env[nrow(k.c.data2022$env),] <- c("S", "Y", "1", "1", "1", "1", "0")
# changing row name:
row.names(k.c.data2022$env)[33] <- "Knepp S Y 111" 

# Repeat for the other 2 duplicate sites:
k.c.data2022$comm <- rbind(k.c.data2022$comm, k.c.data2022$comm["Knepp S X 333",])
row.names(k.c.data2022$comm)[34] <- "Knepp S Z 111" 
k.c.data2022$env <- rbind(k.c.data2022$env, k.c.data2022$env["Knepp S X 333",])
k.c.data2022$env[nrow(k.c.data2022$env),] <- c("S", "Z", "1", "1", "1", "0", "1")
row.names(k.c.data2022$env)[34] <- "Knepp S Z 111" 

k.c.data2022$comm <- rbind(k.c.data2022$comm, k.c.data2022$comm["Knepp S Z 222",])
row.names(k.c.data2022$comm)[35] <- "Knepp S Y 333" 
k.c.data2022$env <- rbind(k.c.data2022$env, k.c.data2022$env["Knepp S Z 222",])
k.c.data2022$env[nrow(k.c.data2022$env),] <- c("S", "Y", "3", "3", "0", "1", "1")
row.names(k.c.data2022$env)[35] <- "Knepp S Y 333"

k.c.data2022 <- comparative.comm(tree(k.c.data2022), comm(k.c.data2022), env=k.c.data2022$env) 
saveRDS(k.c.data2022, "clean-data/k.c.data2022.RDS")

# 2023

# Starting with the clean k.c.data from above
k.c.data2023 <- readRDS("clean-data/k.c.data2023.RDS")

# Duplicate the sites 
k.c.data2023$comm <- rbind(k.c.data2023$comm, k.c.data2023$comm["Knepp S X 222",])
# changing row name:
row.names(k.c.data2023$comm)[71] <- "Knepp S Y 111" 
# Duplicate the environmental entries
k.c.data2023$env <- rbind(k.c.data2023$env, k.c.data2023$env["Knepp S X 222",])
# Set the new duplicate to have the right entries in the environmental dataset
k.c.data2023$env[nrow(k.c.data2023$env),] <- c("S", "Y", "1", "1", "1", "1", "0")
# changing row name:
row.names(k.c.data2023$env)[71] <- "Knepp S Y 111" 

# Repeat for the other 2 duplicate sites:
k.c.data2023$comm <- rbind(k.c.data2023$comm, k.c.data2023$comm["Knepp S X 333",])
row.names(k.c.data2023$comm)[72] <- "Knepp S Z 111" 
k.c.data2023$env <- rbind(k.c.data2023$env, k.c.data2023$env["Knepp S X 333",])
k.c.data2023$env[nrow(k.c.data2023$env),] <- c("S", "Z", "1", "1", "1", "0", "1")
row.names(k.c.data2023$env)[72] <- "Knepp S Z 111" 

k.c.data2023$comm <- rbind(k.c.data2023$comm, k.c.data2023$comm["Knepp S Z 222",])
row.names(k.c.data2023$comm)[73] <- "Knepp S Y 333" 
k.c.data2023$env <- rbind(k.c.data2023$env, k.c.data2023$env["Knepp S Z 222",])
k.c.data2023$env[nrow(k.c.data2023$env),] <- c("S", "Y", "3", "3", "0", "1", "1")
row.names(k.c.data2023$env)[73] <- "Knepp S Y 333"
check<- as.data.frame(k.c.data2023$comm)
k.c.data2023 <- comparative.comm(tree(k.c.data2023), comm(k.c.data2023), env=k.c.data2023$env) 
saveRDS(k.c.data2023, "clean-data/k.c.data2023.RDS")

# 2024

# Starting with the clean k.c.data from above
k.c.data2024 <- readRDS("clean-data/k.c.data2024.RDS")

# Duplicate the sites 
k.c.data2024$comm <- rbind(k.c.data2024$comm, k.c.data2024$comm["Knepp S X 222",])
# changing row name:
row.names(k.c.data2024$comm)[72] <- "Knepp S Y 111" 
# Duplicate the environmental entries
k.c.data2024$env <- rbind(k.c.data2024$env, k.c.data2024$env["Knepp S X 222",])
# Set the new duplicate to have the right entries in the environmental dataset
k.c.data2024$env[nrow(k.c.data2024$env),] <- c("S", "Y", "1", "1", "1", "1", "0")
# changing row name:
row.names(k.c.data2024$env)[72] <- "Knepp S Y 111" 

# Repeat for the other 2 duplicate sites:
k.c.data2024$comm <- rbind(k.c.data2024$comm, k.c.data2024$comm["Knepp S X 333",])
row.names(k.c.data2024$comm)[73] <- "Knepp S Z 111" 
k.c.data2024$env <- rbind(k.c.data2024$env, k.c.data2024$env["Knepp S X 333",])
k.c.data2024$env[nrow(k.c.data2024$env),] <- c("S", "Z", "1", "1", "1", "0", "1")
row.names(k.c.data2024$env)[73] <- "Knepp S Z 111" 

k.c.data2024$comm <- rbind(k.c.data2024$comm, k.c.data2024$comm["Knepp S Z 222",])
row.names(k.c.data2024$comm)[74] <- "Knepp S Y 333" 
k.c.data2024$env <- rbind(k.c.data2024$env, k.c.data2024$env["Knepp S Z 222",])
k.c.data2024$env[nrow(k.c.data2024$env),] <- c("S", "Y", "3", "3", "0", "1", "1")
row.names(k.c.data2024$env)[74] <- "Knepp S Y 333"
check<- as.data.frame(k.c.data2024$comm)
k.c.data2024 <- comparative.comm(tree(k.c.data2024), comm(k.c.data2024), env=k.c.data2024$env) 
saveRDS(k.c.data2024, "clean-data/k.c.data2024.RDS")
