# Knepp and Boothby 2022 write up
# Nell Pates
# Figure 4 plotting
# 14 Dec 2024 - FINAL

# Taking the data from null permutations (Figure_4_nulls) and Figure_4_set-up
# to create Figure 4 - violin plots with data points
# This means any edits to plots can be made without re-running the above set-up

# -----------------------------------------------------------------------------#
#### SET UP: ####
# -----------------------------------------------------------------------------#

source("src/headers.R")

# read in workspace image: "violinplots.RData":
load("clean-data/violinplots2024.RData")

# read in data from null permutations:
k.rich <- as.data.frame(readRDS("clean-data/nulls/k.rich.vars2024.RDS"))
k.pd <- as.data.frame(readRDS("clean-data/nulls/k.pd.vars2024.RDS"))
k.ses.mpd <- as.data.frame(readRDS("clean-data/nulls/k.ses.mpd.vars2024.RDS"))
k.ses.mntd <- as.data.frame(readRDS("clean-data/nulls/k.ses.mntd.vars2024.RDS"))

b.rich <- as.data.frame(readRDS("clean-data/nulls/b.rich.vars2024.RDS"))
b.pd <- as.data.frame(readRDS("clean-data/nulls/b.pd.vars2024.RDS"))
b.ses.mpd <- as.data.frame(readRDS("clean-data/nulls/b.ses.mpd.vars2024.RDS"))
b.ses.mntd <- as.data.frame(readRDS("clean-data/nulls/b.ses.mntd.vars2024.RDS"))

# -----------------------------------------------------------------------------#
#### Manual input of significant results: ####
# -----------------------------------------------------------------------------#

#### KNEPP ####

# create a column 'p', with 0 = not significant, 1 = significant.
k.richness.red$p <- c(0,0,1,0,0)
# subset data into significant and non-sig for plotting
k.richness.1 <- subset(k.richness.red, k.richness.red$p == 1) # = significant 
k.richness.2 <- subset(k.richness.red, k.richness.red$p == 0) # = not significant 
# repeat for all data:

k.phylo.red$p <- c(1,0,1,0,1)
k.phylo.1 <- subset(k.phylo.red, k.phylo.red$p == 1)
k.phylo.2 <- subset(k.phylo.red, k.phylo.red$p == 0)
k.pairwise.red$p <- c(1,0,1,0,1)
k.pairwise.1 <- subset(k.pairwise.red, k.pairwise.red$p == 1)
k.pairwise.2 <- subset(k.pairwise.red, k.pairwise.red$p == 0)
k.nearest.red$p <- c(1,0,0,0,0)
k.nearest.1 <- subset(k.nearest.red, k.nearest.red$p == 1)
k.nearest.2 <- subset(k.nearest.red, k.nearest.red$p == 0)

#### BOOTHBY #### 

b.richness.red$p <- c(0,0,0,0)
b.richness.1 <- subset(b.richness.red, b.richness.red$p == 1) # = significant results
b.richness.2 <- subset(b.richness.red, b.richness.red$p == 0) # = not significant
b.phylo.red$p <- c(0,0,0,0)
b.phylo.1 <- subset(b.phylo.red, b.phylo.red$p == 1)
b.phylo.2 <- subset(b.phylo.red, b.phylo.red$p == 0)
b.pairwise.red$p <- c(0,0,0,0)
b.pairwise.1 <- subset(b.pairwise.red, b.pairwise.red$p == 1)
b.pairwise.2 <- subset(b.pairwise.red, b.pairwise.red$p == 0)
b.nearest.red$p <- c(0,0,0,0)
b.nearest.1 <- subset(b.nearest.red, b.nearest.red$p == 1)
b.nearest.2 <- subset(b.nearest.red, b.nearest.red$p == 0)

# ----------------------------------------------------------------------------#
#### PLOTTING ####
# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
#### SR - KNEPP ####
# ----------------------------------------------------------------------------#
par(mfrow= c(1,2))
# change column names to more manageable 1:999
colnames(k.rich) <- 1:999
# transpose rows to columns
k.rich <- as.data.frame(t(k.rich)) 
# rename groupings with caps
names(k.rich)[1] <- "Residual"
names(k.rich)[2] <- "Minor"
names(k.rich)[3] <- "Major"
names(k.rich)[4] <- "Fractal"
names(k.rich)[5] <- "Block"

# Pivot table longer to input to geom_violin
k.rich <- pivot_longer(k.rich, cols = c("Block", "Fractal", "Major", "Minor", "Residual"))
library(gridExtra)

SR_knepp_2024 <- ggplot(k.rich, aes(x = name, y = value, fill = name))+
  xlab("")+ # Manage x and y labs depending on layout of final Fig
  ylab("Proportion of variance")+
  scale_y_continuous(breaks = seq(0, 1, by = .2), n.breaks = 6)+ # to make all figures comparable and neat
  geom_violin(
    trim = FALSE, 
    alpha = 0.2, 
    fill = "azure4") +
  geom_boxplot(width=0.05, color="black", fill="white", alpha=0.5)+
  geom_point( # plot the significant results with one colour
    shape = 21, fill="red3", color="black", size=4, stroke = 1,
    data = k.richness.1, 
    show.legend = FALSE
  ) +
  geom_point( # plot  other results with a different colour
    shape = 21, fill="white", color="black", size=4,
    data = k.richness.2, 
    show.legend = FALSE
  ) +
  theme_classic()+
  theme(text = element_text(size = 18)) # increase font sizes

# ----------------------------------------------------------------------------#
#### SR - BOOTHBY ####
# ----------------------------------------------------------------------------#

colnames(b.rich) <- 1:999
b.rich <- as.data.frame(t(b.rich)) # transpose row to columns
names(b.rich)[1] <- "Residual"
names(b.rich)[2] <- "Minor"
names(b.rich)[3] <- "Major"
names(b.rich)[4] <- "Fractal"

b.rich <- pivot_longer(b.rich, cols = c("Fractal", "Major", "Minor", "Residual"))

SR_boothby_2024 <- ggplot(b.rich, aes(x = name, y = value, fill = name))+
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), n.breaks= 6)+
  geom_violin(
    trim = FALSE, 
    alpha = 0.2, 
    fill = "azure4") +
  geom_boxplot(width=0.05, color="black", fill="white", alpha=0.5)+
  geom_point(
    shape = 21, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.richness.1, 
    show.legend = FALSE
  ) +
  geom_point(
    shape = 21, fill="white", color="black", size=4,
    data = b.richness.2, 
    show.legend = FALSE
  ) +
  theme_classic()+
  theme(text = element_text(size = 18))

# ----------------------------------------------------------------------------#
#### Faith's PD - KNEPP ####
# ----------------------------------------------------------------------------#

colnames(k.pd) <- 1:999
k.pd <- as.data.frame(t(k.pd)) # transpose row to columns
names(k.pd)[1] <- "Residual"
names(k.pd)[2] <- "Minor"
names(k.pd)[3] <- "Major"
names(k.pd)[4] <- "Fractal"
names(k.pd)[5] <- "Block"

k.pd <- pivot_longer(k.pd, cols = c("Block", "Fractal", "Major", "Minor", "Residual"))

PD_knepp_2024 <- ggplot(k.pd, aes(x = name, y = value, fill = name))+
  xlab("")+
  ylab("Proportion of variance")+
  scale_y_continuous(breaks = seq(0, 1, by = .2))+
  geom_violin(
    trim = FALSE, 
    alpha = 0.2, 
    fill = "azure4") +
  geom_boxplot(width=0.05, color="black", fill="white", alpha=0.5)+
  geom_point(
    shape = 21, fill="red3", color="black", size=4, stroke = 1,
    data = k.phylo.1, 
    show.legend = FALSE
  ) +
  geom_point(
    shape = 21, fill="white", color="black", size=4,
    data = k.phylo.2, 
    show.legend = FALSE
  ) +
  theme_classic()+
  theme(text = element_text(size = 18))


# ----------------------------------------------------------------------------#
#### Faith's PD - BOOTHBY ####
# ----------------------------------------------------------------------------#

colnames(b.pd) <- 1:999
b.pd <- as.data.frame(t(b.pd)) # transpose row to columns
names(b.pd)[1] <- "Residual"
names(b.pd)[2] <- "Minor"
names(b.pd)[3] <- "Major"
names(b.pd)[4] <- "Fractal"

b.pd <- pivot_longer(b.pd, cols = c("Fractal", "Major", "Minor", "Residual"))

PD_boothby_2024 <- ggplot(b.pd, aes(x = name, y = value, fill = name))+
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1, by = .2))+
  geom_violin(
    trim = FALSE, 
    alpha = 0.2, 
    fill = "azure4") +
  geom_boxplot(width=0.05, color="black", fill="white", alpha=0.5)+
  geom_point(
    shape = 21, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.phylo.1, 
    show.legend = FALSE
  ) +
  geom_point(
    shape = 21, fill="white", color="black", size=4,
    data = b.phylo.2, 
    show.legend = FALSE
  ) +
  theme_classic()+
  theme(text = element_text(size = 18))

# ----------------------------------------------------------------------------#
#### SES MPD - KNEPP ####
# ----------------------------------------------------------------------------#

colnames(k.ses.mpd) <- 1:999
k.ses.mpd <- as.data.frame(t(k.ses.mpd)) # transpose row to columns
names(k.ses.mpd)[1] <- "Residual"
names(k.ses.mpd)[2] <- "Minor"
names(k.ses.mpd)[3] <- "Major"
names(k.ses.mpd)[4] <- "Fractal"
names(k.ses.mpd)[5] <- "Block"

k.ses.mpd <- pivot_longer(k.ses.mpd, cols = c("Block", "Fractal", "Major", "Minor", "Residual"))

MPD_knepp_2024 <- ggplot(k.ses.mpd, aes(x = name, y = value, fill = name))+
  xlab("")+
  ylab("Proportion of variance")+
  scale_y_continuous(breaks = seq(0, 1, by = .2))+
  geom_violin(
    trim = FALSE, 
    alpha = 0.2, 
    fill = "azure4") +
  geom_boxplot(width=0.05, color="black", fill="white", alpha=0.5)+
  geom_point(
    shape = 21, fill="red3", color="black", size=4, stroke = 1,
    data = k.pairwise.1, 
    show.legend = FALSE
  ) +
  geom_point(
    shape = 21, fill="white", color="black", size=4,
    data = k.pairwise.2, 
    show.legend = FALSE
  ) +
  theme_classic()+
  theme(text = element_text(size = 18))


# ----------------------------------------------------------------------------#
#### SES MPD - BOOTHBY ####
# ----------------------------------------------------------------------------#

colnames(b.ses.mpd) <- 1:999
b.ses.mpd <- as.data.frame(t(b.ses.mpd)) # transpose row to columns
names(b.ses.mpd)[1] <- "Residual"
names(b.ses.mpd)[2] <- "Minor"
names(b.ses.mpd)[3] <- "Major"
names(b.ses.mpd)[4] <- "Fractal"

b.ses.mpd <- pivot_longer(b.ses.mpd, cols = c("Fractal", "Major", "Minor", "Residual"))

MPD_boothby_2024 <- ggplot(b.ses.mpd, aes(x = name, y = value, fill = name))+
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1, by = .2))+
  geom_violin(
    trim = FALSE, 
    alpha = 0.2, 
    fill = "azure4") +
  geom_boxplot(width=0.05, color="black", fill="white", alpha=0.5)+
  geom_point(
    shape = 21, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.pairwise.1, 
    show.legend = FALSE
  ) +
  geom_point(
    shape = 21, fill="white", color="black", size=4,
    data = b.pairwise.2, 
    show.legend = FALSE
  ) +
  theme_classic()+
  theme(text = element_text(size = 18))

# ----------------------------------------------------------------------------#
#### SES MNTD - KNEPP ####
# ----------------------------------------------------------------------------#

colnames(k.ses.mntd) <- 1:999
k.ses.mntd <- as.data.frame(t(k.ses.mntd)) # transpose row to columns
names(k.ses.mntd)[1] <- "Residual"
names(k.ses.mntd)[2] <- "Minor"
names(k.ses.mntd)[3] <- "Major"
names(k.ses.mntd)[4] <- "Fractal"
names(k.ses.mntd)[5] <- "Block"

k.ses.mntd <- pivot_longer(k.ses.mntd, cols = c("Block", "Fractal", "Major", "Minor", "Residual"))

MNTD_knepp_2024 <- ggplot(k.ses.mntd, aes(x = name, y = value, fill = name))+
  xlab("")+
  ylab("Proportion of variance")+
  scale_y_continuous(breaks = seq(0, 1, by = .2))+
  geom_violin(
    trim = FALSE, 
    alpha = 0.2, 
    fill = "azure4") +
  geom_boxplot(width=0.05, color="black", fill="white", alpha=0.5)+
  geom_point(
    shape = 21, fill="red3", color="black", size=4, stroke = 1,
    data = k.nearest.1, 
    show.legend = FALSE
  ) +
  geom_point(
    shape = 21, fill="white", color="black", size=4,
    data = k.nearest.2, 
    show.legend = FALSE
  ) +
  theme_classic()+
  theme(text = element_text(size = 18))
# ----------------------------------------------------------------------------#
#### SES MNTD - BOOTHBY ####
# ----------------------------------------------------------------------------#

colnames(b.ses.mntd) <- 1:999
b.ses.mntd <- as.data.frame(t(b.ses.mntd)) # transpose row to columns
names(b.ses.mntd)[1] <- "Residual"
names(b.ses.mntd)[2] <- "Minor"
names(b.ses.mntd)[3] <- "Major"
names(b.ses.mntd)[4] <- "Fractal"

b.ses.mntd <- pivot_longer(b.ses.mntd, cols = c("Fractal", "Major", "Minor", "Residual"))

MNTD_boothby_2024 <- ggplot(b.ses.mntd, aes(x = name, y = value, fill = name))+
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1, by = .2))+
  geom_violin(
    trim = FALSE, 
    alpha = 0.2, 
    fill = "azure4") +
  geom_boxplot(width=0.05, color="black", fill="white", alpha=0.5)+
  geom_point(
    shape = 21, fill="royalblue4", color="black", size=4, stroke = 1,
    data = b.nearest.1, 
    show.legend = FALSE
  ) +
  geom_point(
    shape = 21, fill="white", color="black", size=4,
    data = b.nearest.2, 
    show.legend = FALSE
  ) +
  theme_classic()+
  theme(text = element_text(size = 18))


grid.arrange(SR_knepp_2024, SR_boothby_2024,
             PD_knepp_2024, PD_boothby_2024,
             MPD_knepp_2024, MPD_boothby_2024,
             MNTD_knepp_2024, MNTD_boothby_2024, ncol=2)



