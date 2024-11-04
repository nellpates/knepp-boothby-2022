# NMDS - All Years

# Knepp and Boothby write up
# Nell Pates
# NMDS plotting - all years of data combined 

# 26 September 2024 - FINAL
# -----------------------------------------------------------------------------#
#### SETUP: ####
# -----------------------------------------------------------------------------#
setwd("knepp-boothby")
source("src/headers.R") # load all required packages

# Load data
plants <- read.csv("raw-data/all-plants-all-years.csv") # all data from Knepp and Boothby surveys
tree <- read.tree("raw-data/Vascular_Plants_rooted.dated.tre") # Zanne et al (2014) phylogeny 

# Initial format and get rid of  rows that contain *s

plants$Species <- stringi::stri_trans_general(plants$Species, "latin-ascii")
plants$Species <- gsub(" ", "_", plants$Species, fixed=TRUE) # replaces spaces with _ to match across data sets
plants[is.na(plants)] <- 0
plants <- plants %>% 
  filter(! grepl('\\*', Plot))

# "Build" phylogenies
plantstree <- congeneric.merge(tree, unique(plants$Species)) # adds missing species to phylogeny

betaplants <- with(plants, tapply(Cover, list(paste(Site,Year,Block,Group,Plot, sep=""), Species), mean, rm.na=TRUE))
betaplants[is.na(betaplants)] <- 0
rownames(betaplants) <- gsub("0", "", rownames(betaplants), fixed = TRUE)
betaplants <- as.data.frame(betaplants)
saveRDS(betaplants, "clean-data/betaplants-allyears.RDS")

betaplants <- readRDS("clean-data/betaplants-allyears.RDS")

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
nmds_data$year <- 1:412
nmds_data$year[1:78] <- 2022
nmds_data$year[79:163] <- 2023
nmds_data$year[164:240] <- 2024 
nmds_data$year[241:271] <- 2022
nmds_data$year[272:341] <- 2023 
nmds_data$year[342:412] <- 2024 
nmds_data$year <- as.factor(nmds_data$year)


stressplot(plants_NMDS)
orditorp(plants_NMDS, "sites")

# Custom colours for NMDS:
custom_colors <- c(
  "Knepp_2022" = "red4",
  "Knepp_2023" = "red3",
  "Knepp_2024" = "red",
  "Boothby_2022" = "royalblue4",
  "Boothby_2023" = "royalblue2",
  "Boothby_2024" = "lightblue"
)

nmds_data$colour <- paste(nmds_data$sites, nmds_data$year, sep="_")

ggplot(nmds_data, aes(x=MDS1, y=MDS2, colour = colour))+
  geom_point(size = 3.5)+
  labs(x="NMS 1", y="NMS 2")+
  stat_ellipse(data=subset(nmds_data, sites=="Boothby"), aes(x = MDS1, y = MDS2),level = 0.95)+
  stat_ellipse(data=subset(nmds_data, sites=="Knepp"), aes(x = MDS1, y = MDS2),level = 0.95)+
  scale_color_manual(values = custom_colors)+
  theme_classic()+
  theme(text = element_text(size = 24))


#ggplot(nmds_data, aes(x=MDS1, y=MDS2, shape=year, colour = sites, linetype = year))+
#  geom_point(size = 3.5)+
#  labs(x="NMS 1", y="NMS 2")+
#  stat_ellipse(data=subset(nmds_data, sites=="Boothby"), aes(x = MDS1, y = MDS2),level = 0.95)+
#  stat_ellipse(data=subset(nmds_data, sites=="Knepp"), aes(x = MDS1, y = MDS2),level = 0.95)+
#  scale_shape_manual(values = c("2022" = 21, "2023" = 22, "2024" = 23), name="", labels=c("2022", "2023", "2024"))+
#  scale_linetype_manual(values = c("solid", "longdash", "dotted"))+
#  scale_color_manual(values=c("royalblue4","red3"))+
#  theme_classic()+
#  theme(text = element_text(size = 20))

#ggplot(nmds_data, aes(x=MDS1, y=MDS2, colour = sites, linetype = year))+
#  geom_point(shape = 21, size = 3.5)+
#  labs(x="NMS 1", y="NMS 2")+
#  stat_ellipse(data=subset(nmds_data, sites=="Boothby"), aes(x = MDS1, y = MDS2),level = 0.95)+
#  stat_ellipse(data=subset(nmds_data, sites=="Knepp"), aes(x = MDS1, y = MDS2),level = 0.95)+
#  scale_linetype_manual(values = c("solid", "longdash", "dotted"), name = "")+
#  scale_color_manual(values=c("royalblue4","red3"), name = "")+
#  theme_classic()+
#  theme(text = element_text(size = 24))
