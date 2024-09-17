# Knepp and Boothby write up
# Nell Pates
# NMDS plotting - 2023 data
# Bind together the two community matrices then run NMDS 
# 2 September 2024 - FINAL
# -----------------------------------------------------------------------------#
#### SETUP: ####
# -----------------------------------------------------------------------------#

source("src/headers.R") # load all required packages

# Load data
plants <- read.csv("raw-data/all-plants2023.csv") # all data from Knepp and Boothby surveys
tree <- read.tree("raw-data/Vascular_Plants_rooted.dated.tre") # Zanne et al (2014) phylogeny 

# Initial format and get rid of  rows that contain *s

plants$Species <- stringi::stri_trans_general(plants$Species, "latin-ascii")
plants$Species <- gsub(" ", "_", plants$Species, fixed=TRUE) # replaces spaces with _ to match across data sets
plants[is.na(plants)] <- 0
plants <- plants %>% 
  filter(! grepl('\\*', Plot))

# "Build" phylogenies
plantstree <- congeneric.merge(tree, unique(plants$Species)) # adds missing species to phylogeny

betaplants <- with(plants, tapply(Cover, list(paste(Site,Block,Group,Plot, sep=""), Species), mean, rm.na=TRUE))
betaplants[is.na(betaplants)] <- 0
rownames(betaplants) <- gsub("0", "", rownames(betaplants), fixed = TRUE)
betaplants <- as.data.frame(betaplants)
saveRDS(betaplants, "clean-data/betaplants2023.RDS")

betaplants <- readRDS("clean-data/betaplants2023.RDS")

# 1: convert to a matrix
plants.comm <- as.matrix(betaplants)

# 2: Turn the NAs into 0s (absences)
plants.comm[is.na(plants.comm)] <- 0

# PLOT 
plants_NMDS <- metaMDS(plants.comm, distance = "bray", k=2)

samples <- as.data.frame(plants_NMDS$points)
plots <- rownames(plants.comm)
sites <- ifelse(str_detect(plots, "Boothby"), "Boothby", "Knepp")
nmds_data <- cbind(samples,plots, sites)

tocheck1 <- subset(nmds_data, nmds_data$MDS1 <= -1)
tocheck2 <- subset(nmds_data, nmds_data$MDS2 <= -0.09)

ggplot(nmds_data, aes(x=MDS1, y=MDS2, shape=sites))+
  geom_point(size = 3.5)+
  labs(x="NMS 1", y="NMS 2")+
  stat_ellipse(data=subset(nmds_data, sites=="Boothby"), aes(x = MDS1, y = MDS2),linetype="dashed",level = 0.95)+
  stat_ellipse(data=subset(nmds_data, sites=="Knepp"), aes(x = MDS1, y = MDS2),linetype="solid",level = 0.95)+
  scale_shape_manual(values = c("Boothby" = 1, "Knepp" = 16), name="", labels=c("Boothby", "Knepp"))+
  theme_classic()+
  theme(text = element_text(size = 20))


plot(plants_NMDS, display = "sites", labels = "sites")
stressplot(plants_NMDS)

orditorp(plants_NMDS, "sites")
