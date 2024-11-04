# I wonder: can you combine these by rows so that you have dots for Knepp 
# (still in red) and Boothby (still in blue) in the same plot, and have the colours
# with open/filled showing the differences? That way we would have only four panels here, 
# as the eight here seems like not a lot of information density to be honest. 
# Then you could lay out the plots as you do with the conceptual figure, 
# which would be a nice symmetry

# Knepp and Boothby write up
# Nell Pates
# Figure plotting - 2022, 2023 and 2024 data combined
# 5 September 2024 - FINAL

# Plotting the observed values for 2022, 2023, and 2024 - coded for significance -
# in a single plot

# -----------------------------------------------------------------------------#
#### SET UP: ####
# -----------------------------------------------------------------------------#
setwd("knepp-boothby")
source("src/headers.R")
library(gridExtra)

# read in workspace image: "violinplots.RData" and update to make different variables for each year
load("clean-data/violinplots2022.RData")

mv(from = "k.c.data", to = "k.c.data2022")

mv(from = "k.richness.red", to = "k.richness.red2022")
mv(from = "k.phylo.red", to = "k.phylo.red2022")
mv(from = "k.pairwise.red", to = "k.pairwise.red2022")
mv(from = "k.nearest.red", to = "k.nearest.red2022")

mv(from = "b.c.data", to = "b.c.data2022")

mv(from = "b.richness.red", to = "b.richness.red2022")
mv(from = "b.phylo.red", to = "b.phylo.red2022")
mv(from = "b.pairwise.red", to = "b.pairwise.red2022")
mv(from = "b.nearest.red", to = "b.nearest.red2022")

load("clean-data/violinplots2023.RData")

mv(from = "k.c.data", to = "k.c.data2023")

mv(from = "k.richness.red", to = "k.richness.red2023")
mv(from = "k.phylo.red", to = "k.phylo.red2023")
mv(from = "k.pairwise.red", to = "k.pairwise.red2023")
mv(from = "k.nearest.red", to = "k.nearest.red2023")

mv(from = "b.c.data", to = "b.c.data2023")

mv(from = "b.richness.red", to = "b.richness.red2023")
mv(from = "b.phylo.red", to = "b.phylo.red2023")
mv(from = "b.pairwise.red", to = "b.pairwise.red2023")
mv(from = "b.nearest.red", to = "b.nearest.red2023")

load("clean-data/violinplots2024.RData")

mv(from = "k.c.data", to = "k.c.data2024")

mv(from = "k.richness.red", to = "k.richness.red2024")
mv(from = "k.phylo.red", to = "k.phylo.red2024")
mv(from = "k.pairwise.red", to = "k.pairwise.red2024")
mv(from = "k.nearest.red", to = "k.nearest.red2024")

mv(from = "b.c.data", to = "b.c.data2024")

mv(from = "b.richness.red", to = "b.richness.red2024")
mv(from = "b.phylo.red", to = "b.phylo.red2024")
mv(from = "b.pairwise.red", to = "b.pairwise.red2024")
mv(from = "b.nearest.red", to = "b.nearest.red2024")

# -----------------------------------------------------------------------------#
#### Manual input of significant results: ####
# -----------------------------------------------------------------------------#

#### KNEPP 2022 ####

# create a column 'p', with 0 = not significant, 1 = significant.
k.richness.red2022$p <- c(1,0,1,0,0)
# subset data into significant and non-sig for plotting
k.richness.1_2022 <- subset(k.richness.red2022, k.richness.red2022$p == 1) # = significant 
k.richness.2_2022 <- subset(k.richness.red2022, k.richness.red2022$p == 0) # = not significant 
# repeat for all data:

k.phylo.red2022$p <- c(1,0,1,0,1)
k.phylo.1_2022 <- subset(k.phylo.red2022, k.phylo.red2022$p == 1)
k.phylo.2_2022 <- subset(k.phylo.red2022, k.phylo.red2022$p == 0)

k.pairwise.red2022$p <- c(1,0,0,0,0)
k.pairwise.1_2022 <- subset(k.pairwise.red2022, k.pairwise.red2022$p == 1)
k.pairwise.2_2022 <- subset(k.pairwise.red2022, k.pairwise.red2022$p == 0)

k.nearest.red2022$p <- c(0,0,0,0,1)
k.nearest.1_2022 <- subset(k.nearest.red2022, k.nearest.red2022$p == 1)
k.nearest.2_2022 <- subset(k.nearest.red2022, k.nearest.red2022$p == 0)

#### BOOTHBY 2022 #### 

b.richness.red2022$p <- c(0,1,0,0)
b.richness.1_2022 <- subset(b.richness.red2022, b.richness.red2022$p == 1) # = significant results
b.richness.2_2022 <- subset(b.richness.red2022, b.richness.red2022$p == 0) # = not significant
b.phylo.red2022$p <- c(0,0,0,1)
b.phylo.1_2022 <- subset(b.phylo.red2022, b.phylo.red2022$p == 1)
b.phylo.2_2022 <- subset(b.phylo.red2022, b.phylo.red2022$p == 0)
b.pairwise.red2022$p <- c(0,0,0,0)
b.pairwise.1_2022 <- subset(b.pairwise.red2022, b.pairwise.red2022$p == 1)
b.pairwise.2_2022 <- subset(b.pairwise.red2022, b.pairwise.red2022$p == 0)
b.nearest.red2022$p <- c(0,0,0,0)
b.nearest.1_2022 <- subset(b.nearest.red2022, b.nearest.red2022$p == 1)
b.nearest.2_2022 <- subset(b.nearest.red2022, b.nearest.red2022$p == 0)

# ----------------------------------------------------------------------------#
#### PLOTTING ####
# ----------------------------------------------------------------------------#
# ----------------------------------------------------------------------------#
#### SR ####
# ----------------------------------------------------------------------------#

richness <- ggplot(k.richness.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="red3", color="black", size = 5, stroke = 1,
    data = k.richness.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.15)
  ) +
  geom_point( # plot other results with a different colour
    shape = 21, fill="white", color="red", size = 5,
    data = k.richness.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.15)
  )+
  geom_point( # add Boothby SR points in different colour
    shape = 21, fill="royalblue4", color="black", size = 5, stroke = 1,
    data = b.richness.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=0.15)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="royalblue4", size = 5,
    data = b.richness.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=0.15)
  ) +
  theme_classic()+
  theme(text = element_text(size = 24))# increase font sizes

# ----------------------------------------------------------------------------#
#### PD ####
# ----------------------------------------------------------------------------#
# 2022, 2023, 2024 at Knepp

faithpd <- ggplot(k.phylo.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="red3", color="black", size = 5, stroke = 1,
    data = k.phylo.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.15)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="red", size = 5,
    data = k.phylo.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.15)
  ) +      
  geom_point( # add Boothby SR points in different colour
    shape = 21, fill="royalblue4", color="black", size = 5, stroke = 1,
    data = b.phylo.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=0.15)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="royalblue4", size = 5,
    data = b.phylo.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=0.15)
  ) +
  theme_classic()+
  theme(text = element_text(size = 24))# increase font sizes

# ----------------------------------------------------------------------------#
#### SES MPD ####
# ----------------------------------------------------------------------------#
# 2022, 2023, 2024 at Knepp

sesMPD <- ggplot(k.pairwise.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="red3", color="black", size = 5, stroke = 1,
    data = k.pairwise.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.15)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="red", size = 5,
    data = k.pairwise.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.15)
  ) +      
  geom_point( # add Boothby SR points in different colour
    shape = 21, fill="royalblue4", color="black", size = 5, stroke = 1,
    data = b.pairwise.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=0.15)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="royalblue4", size = 5,
    data = b.pairwise.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=0.15)
  ) +
  theme_classic()+
  theme(text = element_text(size = 24))# increase font sizes
  
# ----------------------------------------------------------------------------#
#### SES MNTD ####
# ----------------------------------------------------------------------------#
# 2022, 2023, 2024 at Knepp

sesMNTD <- ggplot(k.nearest.red2022, aes(x = name, y = value, fill = name))+
    xlab("")+ # Manage x and y labs depending on layout of final Fig
    ylab("Proportion of variance")+
    scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
    geom_point( # plot the significant results with one colour
      shape = 21, fill="red3", color="black", size = 5, stroke = 1,
      data = k.nearest.1_2022, 
      show.legend = FALSE,
      position = position_nudge(x=-0.15)
    ) +
    geom_point( # plot  other results with a different colour
      shape = 21, fill="white", color="red", size = 5,
      data = k.nearest.2_2022, 
      show.legend = FALSE,
      position = position_nudge(x=-0.15)
    ) +      
    geom_point( # add Boothby SR points in different colour
      shape = 21, fill="royalblue4", color="black", size = 5, stroke = 1,
      data = b.nearest.1_2022, 
      show.legend = FALSE,
      position = position_nudge(x=0.15)
    ) +
    geom_point( # plot  other results with a different colour
      shape = 21, fill="white", color="royalblue4", size = 5,
      data = b.nearest.2_2022, 
      show.legend = FALSE,
      position = position_nudge(x=0.15)
    ) +
    theme_classic()+
    theme(text = element_text(size = 24))# increase font sizes


grid.arrange(richness, sesMPD, faithpd, sesMNTD, nrow = 2)

