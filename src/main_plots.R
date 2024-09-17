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

#### KNEPP 2023 ####

k.richness.red2023$p <- c(0,0,0,0,0)
k.richness.1_2023 <- subset(k.richness.red2023, k.richness.red2023$p == 1) # = significant 
k.richness.2_2023 <- subset(k.richness.red2023, k.richness.red2023$p == 0) # = not significant 
k.phylo.red2023$p <- c(1,0,1,1,1)
k.phylo.1_2023 <- subset(k.phylo.red2023, k.phylo.red2023$p == 1)
k.phylo.2_2023 <- subset(k.phylo.red2023, k.phylo.red2023$p == 0)
k.pairwise.red2023$p <- c(1,0,0,0,0)
k.pairwise.1_2023 <- subset(k.pairwise.red2023, k.pairwise.red2023$p == 1)
k.pairwise.2_2023 <- subset(k.pairwise.red2023, k.pairwise.red2023$p == 0)
k.nearest.red2023$p <- c(1,0,0,0,0)
k.nearest.1_2023 <- subset(k.nearest.red2023, k.nearest.red2023$p == 1)
k.nearest.2_2023 <- subset(k.nearest.red2023, k.nearest.red2023$p == 0)

#### BOOTHBY 2023 #### 

b.richness.red2023$p <- c(1,0,1,1)
b.richness.1_2023 <- subset(b.richness.red2023, b.richness.red2023$p == 1) # = significant results
b.richness.2_2023 <- subset(b.richness.red2023, b.richness.red2023$p == 0) # = not significant
b.phylo.red2023$p <- c(1,1,1,1)
b.phylo.1_2023 <- subset(b.phylo.red2023, b.phylo.red2023$p == 1)
b.phylo.2_2023 <- subset(b.phylo.red2023, b.phylo.red2023$p == 0)
b.pairwise.red2023$p <- c(0,0,0,0)
b.pairwise.1_2023 <- subset(b.pairwise.red2023, b.pairwise.red2023$p == 1)
b.pairwise.2_2023 <- subset(b.pairwise.red2023, b.pairwise.red2023$p == 0)
b.nearest.red2023$p <- c(0,0,0,0)
b.nearest.1_2023 <- subset(b.nearest.red2023, b.nearest.red2023$p == 1)
b.nearest.2_2023 <- subset(b.nearest.red2023, b.nearest.red2023$p == 0)

#### KNEPP 2024 ####

k.richness.red2024$p <- c(0,0,1,0,0)
k.richness.1_2024 <- subset(k.richness.red2024, k.richness.red2024$p == 1) # = significant 
k.richness.2_2024 <- subset(k.richness.red2024, k.richness.red2024$p == 0) # = not significant 
k.phylo.red2024$p <- c(1,0,1,0,1)
k.phylo.1_2024 <- subset(k.phylo.red2024, k.phylo.red2024$p == 1)
k.phylo.2_2024 <- subset(k.phylo.red2024, k.phylo.red2024$p == 0)
k.pairwise.red2024$p <- c(1,0,1,0,1)
k.pairwise.1_2024 <- subset(k.pairwise.red2024, k.pairwise.red2024$p == 1)
k.pairwise.2_2024 <- subset(k.pairwise.red2024, k.pairwise.red2024$p == 0)
k.nearest.red2024$p <- c(1,0,0,0,0)
k.nearest.1_2024 <- subset(k.nearest.red2024, k.nearest.red2024$p == 1)
k.nearest.2_2024 <- subset(k.nearest.red2024, k.nearest.red2024$p == 0)

#### BOOTHBY 2024 #### 

b.richness.red2024$p <- c(0,0,0,0)
b.richness.1_2024 <- subset(b.richness.red2024, b.richness.red2024$p == 1) # = significant results
b.richness.2_2024 <- subset(b.richness.red2024, b.richness.red2024$p == 0) # = not significant
b.phylo.red2024$p <- c(0,0,0,0)
b.phylo.1_2024 <- subset(b.phylo.red2024, b.phylo.red2024$p == 1)
b.phylo.2_2024 <- subset(b.phylo.red2024, b.phylo.red2024$p == 0)
b.pairwise.red2024$p <- c(0,0,0,0)
b.pairwise.1_2024 <- subset(b.pairwise.red2024, b.pairwise.red2024$p == 1)
b.pairwise.2_2024 <- subset(b.pairwise.red2024, b.pairwise.red2024$p == 0)
b.nearest.red2024$p <- c(0,0,0,0)
b.nearest.1_2024 <- subset(b.nearest.red2024, b.nearest.red2024$p == 1)
b.nearest.2_2024 <- subset(b.nearest.red2024, b.nearest.red2024$p == 0)

# ----------------------------------------------------------------------------#
#### PLOTTING ####
# ----------------------------------------------------------------------------#
# ----------------------------------------------------------------------------#
#### SR ####
# ----------------------------------------------------------------------------#
# 2022, 2023, 2024 at Knepp

kneppSR <- ggplot(k.richness.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="red3", color="black", size=4, stroke = 1,
    data = k.richness.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="black", size=4,
    data = k.richness.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot the significant results with one colour
    shape = 22, fill="red3", color="black", size=4, stroke = 1,
    data = k.richness.1_2023, 
    show.legend = FALSE
  ) +
  geom_point( # plot  other results with a different colour
    shape = 22, fill="white", color="black", size=4,
    data = k.richness.2_2023, 
    show.legend = FALSE
  )+
  geom_point( # plot the significant results with one colour
    shape = 23, fill="red3", color="black", size=4, stroke = 1,
    data = k.richness.1_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 23, fill="white", color="black", size=4,
    data = k.richness.2_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  )+
  theme_classic()+
  theme(text = element_text(size = 17))# increase font sizes


# Plot SR 2022, 2023, 2024 at Boothby

boothbySR <- ggplot(b.richness.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.richness.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="black", size=4,
    data = b.richness.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot the significant results with one colour
    shape = 22, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.richness.1_2023, 
    show.legend = FALSE
  ) +
  geom_point( # plot  other results with a different colour
    shape = 22, fill="white", color="black", size=4,
    data = b.richness.2_2023, 
    show.legend = FALSE
  )+
  geom_point( # plot the significant results with one colour
    shape = 23, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.richness.1_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 23, fill="white", color="black", size=4,
    data = b.richness.2_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  )+
  theme_classic()+
  theme(text = element_text(size = 17))# increase font sizes


# ----------------------------------------------------------------------------#
#### PD ####
# ----------------------------------------------------------------------------#
# 2022, 2023, 2024 at Knepp

kneppPD <- ggplot(k.phylo.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="red3", color="black", size=4, stroke = 1,
    data = k.phylo.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="black", size=4,
    data = k.phylo.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot the significant results with one colour
    shape = 22, fill="red3", color="black", size=4, stroke = 1,
    data = k.phylo.1_2023, 
    show.legend = FALSE
  ) +
  geom_point( # plot  other results with a different colour
    shape = 22, fill="white", color="black", size=4,
    data = k.phylo.2_2023, 
    show.legend = FALSE
  )+
  geom_point( # plot the significant results with one colour
    shape = 23, fill="red3", color="black", size=4, stroke = 1,
    data = k.phylo.1_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 23, fill="white", color="black", size=4,
    data = k.phylo.2_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  )+
  theme_classic()+
  theme(text = element_text(size = 17))# increase font sizes


# Plot PD 2022, 2023, 2024 at Boothby

boothbyPD <- ggplot(b.phylo.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.phylo.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="black", size=4,
    data = b.phylo.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot the significant results with one colour
    shape = 22, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.phylo.1_2023, 
    show.legend = FALSE
  ) +
  geom_point( # plot  other results with a different colour
    shape = 22, fill="white", color="black", size=4,
    data = b.phylo.2_2023, 
    show.legend = FALSE
  )+
  geom_point( # plot the significant results with one colour
    shape = 23, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.phylo.1_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 23, fill="white", color="black", size=4,
    data = b.phylo.2_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  )+
  theme_classic()+
  theme(text = element_text(size = 17))# increase font sizes


# ----------------------------------------------------------------------------#
#### SES MPD ####
# ----------------------------------------------------------------------------#
# 2022, 2023, 2024 at Knepp

kneppMPD <- ggplot(k.pairwise.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="red3", color="black", size=4, stroke = 1,
    data = k.pairwise.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="black", size=4,
    data = k.pairwise.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot the significant results with one colour
    shape = 22, fill="red3", color="black", size=4, stroke = 1,
    data = k.pairwise.1_2023, 
    show.legend = FALSE
  ) +
  geom_point( # plot  other results with a different colour
    shape = 22, fill="white", color="black", size=4,
    data = k.pairwise.2_2023, 
    show.legend = FALSE
  )+
  geom_point( # plot the significant results with one colour
    shape = 23, fill="red3", color="black", size=4, stroke = 1,
    data = k.pairwise.1_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 23, fill="white", color="black", size=4,
    data = k.pairwise.2_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  )+
  theme_classic()+
  theme(text = element_text(size = 17))# increase font sizes


# Plot SES MPD 2022, 2023, 2024 at Boothby

boothbyMPD <- ggplot(b.pairwise.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="red", color="black", size=4, stroke = 1,
    data = b.pairwise.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="black", size=4,
    data = b.pairwise.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot the significant results with one colour
    shape = 22, fill="red", color="black", size=4, stroke = 1,
    data = b.pairwise.1_2023, 
    show.legend = FALSE
  ) +
  geom_point( # plot  other results with a different colour
    shape = 22, fill="white", color="black", size=4,
    data = b.pairwise.2_2023, 
    show.legend = FALSE
  )+
  geom_point( # plot the significant results with one colour
    shape = 23, fill="red", color="black", size=4, stroke = 1,
    data = b.pairwise.1_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 23, fill="white", color="black", size=4,
    data = b.pairwise.2_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  )+
  theme_classic()+
  theme(text = element_text(size = 17))# increase font sizes

# ----------------------------------------------------------------------------#
#### SES MNTD ####
# ----------------------------------------------------------------------------#
# 2022, 2023, 2024 at Knepp

kneppMNTD <- ggplot(k.nearest.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="red3", color="black", size=4, stroke = 1,
    data = k.nearest.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="black", size=4,
    data = k.nearest.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot the significant results with one colour
    shape = 22, fill="red3", color="black", size=4, stroke = 1,
    data = k.nearest.1_2023, 
    show.legend = FALSE
  ) +
  geom_point( # plot  other results with a different colour
    shape = 22, fill="white", color="black", size=4,
    data = k.nearest.2_2023, 
    show.legend = FALSE
  )+
  geom_point( # plot the significant results with one colour
    shape = 23, fill="red3", color="black", size=4, stroke = 1,
    data = k.nearest.1_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 23, fill="white", color="black", size=4,
    data = k.nearest.2_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  )+
  theme_classic()+
  theme(text = element_text(size = 17))# increase font sizes


# Plot SES MNTD 2022, 2023, 2024 at Boothby

boothbyMNTD <- ggplot(b.nearest.red2022, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2))+ # to make all figures comparable and neat
  geom_point( # plot the significant results with one colour
    shape = 21, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.nearest.1_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="black", size=4,
    data = b.nearest.2_2022, 
    show.legend = FALSE,
    position = position_nudge(x=-0.2)
  ) +
  geom_point( # plot the significant results with one colour
    shape = 22, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.nearest.1_2023, 
    show.legend = FALSE
  ) +
  geom_point( # plot  other results with a different colour
    shape = 22, fill="white", color="black", size=4,
    data = b.nearest.2_2023, 
    show.legend = FALSE
  )+
  geom_point( # plot the significant results with one colour
    shape = 23, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.nearest.1_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  ) +
  geom_point( # plot  other results with a different colour
    shape = 23, fill="white", color="black", size=4,
    data = b.nearest.2_2024, 
    show.legend = FALSE,
    position = position_nudge(x=0.2)
  )+
  theme_classic()+
  theme(text = element_text(size = 17))# increase font sizes

grid.arrange(kneppSR, boothbySR, 
             kneppPD, boothbyPD, ncol=2)
library(gridExtra)
grid.arrange(kneppSR, boothbySR, 
             kneppPD, boothbyPD,
             kneppMPD, boothbyMPD,
             kneppMNTD, boothbyMNTD, ncol=2)

