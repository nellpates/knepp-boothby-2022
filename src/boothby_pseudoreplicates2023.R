# Knepp and Boothby write up
# Nell Pates
# SUPPLEMENTARY INFORMATION - Boothby pseudoreplicates 2023
# 2 September 2024 - FINAL

# The raw data for Boothby (boothby2023.csv) has a column 'survey' with two categories: A or B
# A is only the sites that were fully surveyed
# B is sites that were photographed to confirm monocultures
# (there is no option C here (see boothby_pseudoreplicates2022), 
# all sites were either surveyed in full, or photographed)

# This script runs the same analysis as that for comparing Knepp and Boothby, 
# but on smaller subsets of data from Boothby to validate our results

# -----------------------------------------------------------------------------#
#### SET UP: ####
# -----------------------------------------------------------------------------#

source("src/headers.R")

# Load data
boothby <- read.csv("raw-data/boothby2023.csv")
tree <- read.tree("raw-data/Vascular_Plants_rooted.dated.tre")

# Initial format and get rid of  rows that contain *s
# BOOTHBY
boothby$Species <- stringi::stri_trans_general(boothby$Species, "latin-ascii")
boothby$Species <-gsub(" ", "_", boothby$Species, fixed=TRUE)
boothby[is.na(boothby)] <- 0
boothby <- boothby %>% 
  filter(! grepl('\\*', Plot))

# Boothby pseurdoreplicates - full surveys only
boothby.full <-subset(boothby, survey == "A") 
# Boothby pseudoreplicates - full surveys + photos
boothby.photos <-subset(boothby, survey == "A" | survey == "B") 

# "Build" phylogenies

btreefull<- congeneric.merge(tree, unique(boothby.full$Species))
btreephotos <- congeneric.merge(tree, unique(boothby.photos$Species))

# Format for pez etc.

#BOOTHBY - Full surveys only
b.comm.full <- with(boothby.full, tapply(Cover, list(paste(Site,Block,Plot), Species), mean, rm.na=TRUE))
b.comm.full[is.na(b.comm.full)] <- 0
b.c.data.full <- comparative.comm(btreefull, b.comm.full)

#BOOTHBY - Full + photographed sites
b.comm.photos <- with(boothby.photos, tapply(Cover, list(paste(Site,Block,Plot), Species), mean, rm.na=TRUE))
b.comm.photos[is.na(b.comm.photos)] <- 0
b.c.data.photos <- comparative.comm(btreephotos, b.comm.photos)

# Make hierarchical groupings for each site

#BOOTHBY - Full surveys only
raw.groups <- strsplit(sites(b.c.data.full), " ")
site.no <- sapply(raw.groups, function(x) x[3])
env <- data.frame(
  fractal = sapply(raw.groups, function(x) x[2]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)
env$fractal[env$fractal=="NA"] <- "A" 
rownames(env) <- sites(b.c.data.full)

b_wanted_spp_full <- species(b.c.data.full)
boothby.full <- subset(boothby.full, Species %in% b_wanted_spp_full)
write.csv(boothby.full, file = "clean-data/boothby_clean_full2023.csv")
boothby.full <- read.csv("clean-data/boothby_clean_full2023.csv")

b.c.data.full <- comparative.comm(tree(b.c.data.full), comm(b.c.data.full), env=env)

#BOOTHBY - Full + photographed sites
raw.groups <- strsplit(sites(b.c.data.photos), " ")
site.no <- sapply(raw.groups, function(x) x[3])
env <- data.frame(
  fractal = sapply(raw.groups, function(x) x[2]),
  major = substr(site.no, 0, 1),
  minor = substr(site.no, 2, 2)
)
env$fractal[env$fractal=="NA"] <- "A" 
rownames(env) <- sites(b.c.data.photos)

b_wanted_spp_photos <- species(b.c.data.photos)
boothby.photos <- subset(boothby.photos, Species %in% b_wanted_spp_photos)
write.csv(boothby.photos, file = "clean-data/boothby_clean_photos2023.csv")
boothby.photos <- read.csv("clean-data/boothby_clean_photos2023.csv")

b.c.data.photos <- comparative.comm(tree(b.c.data.photos), comm(b.c.data.photos), env=env)

# -----------------------------------------------------------------------------#
#### WRAPPER FUNCTIONS: ####
# -----------------------------------------------------------------------------#

fit.model.photos <- function(response){
  par(mar=c(5.1,5.5,4.1,2.1))
  model <- stan_lmer(response ~ (1|fractal/major/minor), data=env(b.c.data.photos), iter=10000, warmup=8000, adapt_delta=.999)
  coefs <- as.data.frame(summary(model))
  variances <- coefs[c("sigma","Sigma[minor:(major:fractal):(Intercept),(Intercept)]","Sigma[major:fractal:(Intercept),(Intercept)]","Sigma[fractal:(Intercept),(Intercept)]"),"50%"]
  names(variances) <- c("residual","minor","major","fractal")
  variances <- variances[c("fractal","major","minor","residual")]
  barplot(variances/sum(variances), ylim=c(0,1), ylab = "", names.arg = " ", border="black", col="white", cex.axis = 1.5, cex.names=1.5, cex.lab = 2)
}


fit.model.full <- function(response){
  par(mar=c(5.1,5.5,4.1,2.1))
  model <- stan_lmer(response ~ (1|fractal/major/minor), data=env(b.c.data.full), iter=10000, warmup=8000, adapt_delta=.999)
  coefs <- as.data.frame(summary(model))
  variances <- coefs[c("sigma","Sigma[minor:(major:fractal):(Intercept),(Intercept)]","Sigma[major:fractal:(Intercept),(Intercept)]","Sigma[fractal:(Intercept),(Intercept)]"),"50%"]
  names(variances) <- c("residual","minor","major","fractal")
  variances <- variances[c("fractal","major","minor","residual")]
  barplot(variances/sum(variances), ylim=c(0,1), ylab = "", names.arg = " ", cborder="black", col="white", cex.axis = 1.5, cex.names=1.5, cex.lab = 2)
}


# ----------------------------------------------------------------------------#
#### Species richness ####
# ----------------------------------------------------------------------------#

barnamesboothby <- c("Fractal", "Major", "Minor", "Residual")
#b.rich <- .ses.mntd(b.c.data)$ntaxa
b.rich.full <- .ses.mntd(b.c.data.full)$ntaxa
b.rich.photos <- .ses.mntd(b.c.data.photos)$ntaxa


par(mar=c(5.1,5.5,4.1,2.1), mfrow = c(1,2))
# All sites
#fit.model.all(b.rich)
# Full & photographed sites
fit.model.photos(b.rich.photos)
# Full surveys only
fit.model.full(b.rich.full)

# ----------------------------------------------------------------------------#
#### Faith's PD ####
# ----------------------------------------------------------------------------#

par(mar=c(5.1,5.5,4.1,2.1), mfrow = c(1,2))
#b.pd <- .pd(b.c.data)[,"pd"]
b.pd.photos <- .pd(b.c.data.photos)[,"pd"]
b.pd.full <- .pd(b.c.data.full)[,"pd"]

# All sites
#fit.model.all(b.pd)
# Full & photographed sites
fit.model.photos(b.pd.photos)
# Full surveys only
fit.model.full(b.pd.full)

# ----------------------------------------------------------------------------#
#### SES MPD ####
# ----------------------------------------------------------------------------#

par(mar=c(5.1,5.5,4.1,2.1), mfrow = c(1,2))

#b.ses.mpd <- .ses.mpd(b.c.data)$mpd.obs.z
b.ses.mpd.photos <- .ses.mpd(b.c.data.photos)$mpd.obs.z
b.ses.mpd.full <- .ses.mpd(b.c.data.full)$mpd.obs.z

# All sites
#fit.model.all(b.ses.mpd)
# Full & photographed sites
fit.model.photos(b.ses.mpd.photos)
# Full surveys only
fit.model.full(b.ses.mpd.full)

# ----------------------------------------------------------------------------#
#### SES MNTD ####
# ----------------------------------------------------------------------------#

par(mar=c(5.1,5.5,4.1,2.1), mfrow = c(1,2))

#b.ses.mntd <- .ses.mntd(b.c.data)$mntd.obs.z
b.ses.mntd.photos <- .ses.mntd(b.c.data.photos)$mntd.obs.z
b.ses.mntd.full <- .ses.mntd(b.c.data.full)$mntd.obs.z

# All sites
#fit.model.all(b.ses.mntd)
# Full & photographed sites
fit.model.photos(b.ses.mntd.photos)
# Full surveys only
fit.model.full(b.ses.mntd.full)

