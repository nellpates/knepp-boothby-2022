# Knepp and Boothby write up
# Nell Pates
# ALPHA DIVERSITY METRICS FOR KNEPP 2022 BY BLOCK
# 217 eptember 2024 - FINAL

# This breaks down 2022 plant data by block to interrogate diversity metrics

# -----------------------------------------------------------------------------#
#### SETUP: ####
# -----------------------------------------------------------------------------#

source("src/headers.R")

# Load data
kneppall <- read.csv("raw-data/knepp2022.csv")
north <- subset(kneppall, Block == "N")
middle <- subset(kneppall, Block == "M")
south <- subset(kneppall, Block == "S")

tree <- read.tree("raw-data/Vascular_Plants_rooted.dated.tre")

# Initial format and get rid of  rows that contain *s
# KNEPP
kneppall$Species <- stringi::stri_trans_general(kneppall$Species, "latin-ascii")
kneppall$Species <- gsub(" ", "_", kneppall$Species, fixed=TRUE)
kneppall[is.na(kneppall)] <- 0
kneppall <- kneppall %>% 
  filter(! grepl('\\*', Plot))

north$Species <- stringi::stri_trans_general(north$Species, "latin-ascii")
north$Species <- gsub(" ", "_", north$Species, fixed=TRUE)
north[is.na(north)] <- 0
north <- north %>% 
  filter(! grepl('\\*', Plot))

middle$Species <- stringi::stri_trans_general(middle$Species, "latin-ascii")
middle$Species <- gsub(" ", "_", middle$Species, fixed=TRUE)
middle[is.na(middle)] <- 0
middle <- middle %>% 
  filter(! grepl('\\*', Plot))

south$Species <- stringi::stri_trans_general(south$Species, "latin-ascii")
south$Species <- gsub(" ", "_", south$Species, fixed=TRUE)
south[is.na(south)] <- 0
south <- south %>% 
  filter(! grepl('\\*', Plot))

# "Build" phylogenies
alltree <- congeneric.merge(tree, unique(kneppall$Species))
northtree <- congeneric.merge(tree, unique(north$Species))
middletree <- congeneric.merge(tree, unique(middle$Species))
southtree <- congeneric.merge(tree, unique(south$Species))

# Format for pez etc.
#KNEPP
all.comm <- with(kneppall, tapply(Cover, list(paste(Site,Block,Group,Plot), Species), mean, rm.na=TRUE))
all.comm[is.na(all.comm)] <- 0
all.c.data <- comparative.comm(alltree, all.comm)

#KNEPP
raw.groups <- strsplit(sites(all.c.data), " ")
site.no <- sapply(raw.groups, function(x) x[4])
env <- data.frame(
  block = sapply(raw.groups, function(x) x[2]),
  fractal = sapply(raw.groups, function(x) x[3]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)

env$fractal[env$fractal=="NA"] <- "A" # R is seeing "NA" and sometimes coercing to the concept of an NA
rownames(env) <- sites(all.c.data)

env$X <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0)
env$Y <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,1,0)
env$Z <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1)

all.c.data <- comparative.comm(tree(all.c.data), comm(all.c.data), env=env)

# -----------------------------------------------------------------------------#
#### DEALING WITH NESTING: ####
# -----------------------------------------------------------------------------#

# Duplicate the sites 
all.c.data$comm <- rbind(all.c.data$comm, all.c.data$comm["Knepp S X 222",])
# changing row name:
row.names(all.c.data$comm)[33] <- "Knepp S Y 111" 
# Duplicate the environmental entries
all.c.data$env <- rbind(all.c.data$env, all.c.data$env["Knepp S X 222",])
# Set the new duplicate to have the right entries in the environmental dataset
all.c.data$env[nrow(all.c.data$env),] <- c("S", "Y", "1", "1", "1", "1", "0")
# changing row name:
row.names(all.c.data$env)[33] <- "Knepp S Y 111" 

# Repeat for the other 2 duplicate sites:
all.c.data$comm <- rbind(all.c.data$comm, all.c.data$comm["Knepp S X 333",])
row.names(all.c.data$comm)[34] <- "Knepp S Z 111" 
all.c.data$env <- rbind(all.c.data$env, all.c.data$env["Knepp S X 333",])
all.c.data$env[nrow(all.c.data$env),] <- c("S", "Z", "1", "1", "1", "0", "1")
row.names(all.c.data$env)[34] <- "Knepp S Z 111" 

all.c.data$comm <- rbind(all.c.data$comm, all.c.data$comm["Knepp S Z 222",])
row.names(all.c.data$comm)[35] <- "Knepp S Y 333" 
all.c.data$env <- rbind(all.c.data$env, all.c.data$env["Knepp S Z 222",])
all.c.data$env[nrow(all.c.data$env),] <- c("S", "Y", "3", "3", "0", "1", "1")
row.names(all.c.data$env)[35] <- "Knepp S Y 333"

all.c.data <- comparative.comm(tree(all.c.data), comm(all.c.data), env=all.c.data$env) 


north.comm <- with(north, tapply(Cover, list(paste(Site,Block,Group,Plot), Species), mean, rm.na=TRUE))
north.comm[is.na(north.comm)] <- 0
north.c.data <- comparative.comm(northtree, north.comm)

middle.comm <- with(middle, tapply(Cover, list(paste(Site,Block,Group,Plot), Species), mean, rm.na=TRUE))
middle.comm[is.na(middle.comm)] <- 0
middle.c.data <- comparative.comm(middletree, middle.comm)

south.comm <- with(south, tapply(Cover, list(paste(Site,Block,Group,Plot), Species), mean, rm.na=TRUE))
south.comm[is.na(south.comm)] <- 0
south.c.data <- comparative.comm(southtree, south.comm)


all.rich <- .ses.mntd(all.c.data)$ntaxa
median(all.rich) # 9
IQR(all.rich) # 5
all.pd <- .pd(all.c.data)[,"pd"]
median(all.pd) # 908
IQR(all.pd) # 403
all.ses.mpd <- .ses.mpd(all.c.data)$mpd.obs.z
all.ses.mpd <- na.omit(all.ses.mpd)
median(all.ses.mpd) # 0.179
IQR(all.ses.mpd) # 1.58
all.ses.mntd <- .ses.mntd(all.c.data)$mntd.obs.z
all.ses.mntd <- na.omit(all.ses.mntd)
median(all.ses.mntd) # -1.132886
IQR(all.ses.mntd) # 1.704072

north.rich <- .ses.mntd(north.c.data)$ntaxa
median(north.rich) # 10
IQR(north.rich) # 5
north.pd <- .pd(north.c.data)[,"pd"]
median(north.pd) # 1034.155
IQR(north.pd) # 318.6445
north.ses.mpd <- .ses.mpd(north.c.data)$mpd.obs.z
north.ses.mpd <- na.omit(north.ses.mpd)
median(north.ses.mpd) # 0.639
IQR(north.ses.mpd) # 0.710
north.ses.mntd <- .ses.mntd(north.c.data)$mntd.obs.z
north.ses.mntd <- na.omit(north.ses.mntd)
median(north.ses.mntd) # -1.13
IQR(north.ses.mntd) # # 1.84

middle.rich <- .ses.mntd(middle.c.data)$ntaxa
median(middle.rich) # 5
IQR(middle.rich) # 2
middle.pd <- .pd(middle.c.data)[,"pd"]
median(middle.pd) # 507.5995
IQR(middle.pd) # 224
middle.ses.mpd <- .ses.mpd(middle.c.data)$mpd.obs.z
middle.ses.mpd <- na.omit(middle.ses.mpd)
median(middle.ses.mpd) # -0.817
IQR(middle.ses.mpd) # 1.61
middle.ses.mntd <- .ses.mntd(middle.c.data)$mntd.obs.z
middle.ses.mntd <- na.omit(middle.ses.mntd)
median(middle.ses.mntd) # -0.821
IQR(middle.ses.mntd) # 1.23

south.rich <- .ses.mntd(south.c.data)$ntaxa
median(south.rich) # 9.5
IQR(south.rich) # 4.5
south.pd <- .pd(south.c.data)[,"pd"]
median(south.pd) # 950.2141
IQR(south.pd) # 185
south.ses.mpd <- .ses.mpd(south.c.data)$mpd.obs.z
south.ses.mpd <- na.omit(south.ses.mpd)
median(south.ses.mpd) # 0.193
IQR(south.ses.mpd) # 1.304
south.ses.mntd <- .ses.mntd(south.c.data)$mntd.obs.z
south.ses.mntd <- na.omit(south.ses.mntd)
median(south.ses.mntd) # -0.566
IQR(south.ses.mntd) # 1.611