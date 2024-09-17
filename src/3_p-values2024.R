# Knepp and Boothby write up
# Nell Pates
# Figure plotting - 2024 data
# 2 September 2024 - FINAL

# Taking the data from null permutations (nulls.csv files) and set-up.csv files
# to create violin plots with data points
# This means any edits to plots can be made without re-running the above set-up

# -----------------------------------------------------------------------------#
#### SET UP: ####
# -----------------------------------------------------------------------------#

setwd("knepp-boothby")
source("src/headers.R")

# load data from set-up:
load("clean-data/violinplots2024.RData")

# read in data from null permutations:
k.rich.nulls <- as.data.frame(readRDS("clean-data/nulls/k.rich.vars2024.RDS"))
k.pd.nulls <- as.data.frame(readRDS("clean-data/nulls/k.pd.vars2024.RDS"))
k.ses.mpd.nulls <- as.data.frame(readRDS("clean-data/nulls/k.ses.mpd.vars2024.RDS"))
k.ses.mntd.nulls <- as.data.frame(readRDS("clean-data/nulls/k.ses.mntd.vars2024.RDS"))

b.rich.nulls <- as.data.frame(readRDS("clean-data/nulls/b.rich.vars2024.RDS"))
b.pd.nulls <- as.data.frame(readRDS("clean-data/nulls/b.pd.vars2024.RDS"))
b.ses.mpd.nulls <- as.data.frame(readRDS("clean-data/nulls/b.ses.mpd.vars2024.RDS"))
b.ses.mntd.nulls <- as.data.frame(readRDS("clean-data/nulls/b.ses.mntd.vars2024.RDS"))

# ----------------------------------------------------------------------------#
#### SR - KNEPP ####
# ----------------------------------------------------------------------------#

# change column names to more manageable 1:999
colnames(k.rich.nulls) <- 1:999
# transpose rows to columns
k.rich.nulls <- as.data.frame(t(k.rich.nulls)) 
# rename groupings with caps
names(k.rich.nulls)[1] <- "Residual"
names(k.rich.nulls)[2] <- "Minor"
names(k.rich.nulls)[3] <- "Major"
names(k.rich.nulls)[4] <- "Fractal"
names(k.rich.nulls)[5] <- "Block"

newdf <- as.data.frame(1:10)
# Use rank() to identify where our result ranks among the 999 null permutations
# Block - 
rank(c(k.richness.red[1,1], k.rich.nulls[,5]))[1] 
#hist(k.rich.nulls[,5])
#abline(v=k.richness.red[1,1], col="red")

# Fractal - 
rank(c(k.richness.red[2,1], k.rich.nulls[,4]))[1]
#hist(k.rich.nulls[,4])
#abline(v=k.richness.red[2,1], col="red")

# Major - 
rank(c(k.richness.red[3,1], k.rich.nulls[,3]))[1]
#abline(v=k.richness.red[3,1], col="red")
#hist(k.rich.nulls[,3])

# Minor - 
rank(c(k.richness.red[4,1], k.rich.nulls[,2]))[1]
#hist(k.rich.nulls[,2])
#abline(v=k.richness.red[4,1], col="red")

# Residual - 
rank(c(k.richness.red[5,1], k.rich.nulls[,1]))[1]
#hist(k.rich.nulls[,1])
#abline(v=k.richness.red[5,1], col="red")

# ----------------------------------------------------------------------------#
#### Faith's PD - KNEPP ####
# ----------------------------------------------------------------------------#

colnames(k.pd.nulls) <- 1:999
k.pd.nulls <- as.data.frame(t(k.pd.nulls)) # transpose row to columns
names(k.pd.nulls)[1] <- "Residual"
names(k.pd.nulls)[2] <- "Minor"
names(k.pd.nulls)[3] <- "Major"
names(k.pd.nulls)[4] <- "Fractal"
names(k.pd.nulls)[5] <- "Block"

# Block - 
rank(c(k.phylo.red[1,1], k.pd.nulls[,5]))[1]
#hist(k.pd.nulls[,5])
#abline(v=k.phylo.red[1,1], col="blue")

# Fractal - 
rank(c(k.phylo.red[2,1], k.pd.nulls[,4]))[1]
#hist(k.pd.nulls[,4])
#abline(v=k.phylo.red[2,1], col="blue")

# Major - 
rank(c(k.phylo.red[3,1], k.pd.nulls[,3]))[1]
#hist(k.pd.nulls[,3])
#abline(v=k.phylo.red[3,1], col="blue")

# Minor - 
rank(c(k.phylo.red[4,1], k.pd.nulls[,2]))[1]
#hist(k.pd.nulls[,2])
#abline(v=k.phylo.red[4,1], col="blue")

# Residual - 
rank(c(k.phylo.red[5,1], k.pd.nulls[,1]))[1]
#hist(k.pd.nulls[,1])
#abline(v=k.phylo.red[5,1], col="blue")


# ----------------------------------------------------------------------------#
#### SES MPD - KNEPP ####
# ----------------------------------------------------------------------------#

colnames(k.ses.mpd.nulls) <- 1:999
k.ses.mpd.nulls <- as.data.frame(t(k.ses.mpd.nulls)) # transpose row to columns
names(k.ses.mpd.nulls)[1] <- "Residual"
names(k.ses.mpd.nulls)[2] <- "Minor"
names(k.ses.mpd.nulls)[3] <- "Major"
names(k.ses.mpd.nulls)[4] <- "Fractal"
names(k.ses.mpd.nulls)[5] <- "Block"

# Block - 
rank(c(k.pairwise.red[1,1], k.ses.mpd.nulls[,5]))[1]
#hist(k.ses.mpd.nulls[,5])
#abline(v=k.pairwise.red[1,1], col="pink")

# Fractal - 
rank(c(k.pairwise.red[2,1], k.ses.mpd.nulls[,4]))[1]
#hist(k.ses.mpd.nulls[,4])
#abline(v=k.pairwise.red[2,1], col="pink")

# Major - 
rank(c(k.pairwise.red[3,1], k.ses.mpd.nulls[,3]))[1]
#hist(k.ses.mpd.nulls[,3])
#abline(v=k.pairwise.red[3,1], col="pink")

# Minor - 
rank(c(k.pairwise.red[4,1], k.ses.mpd.nulls[,2]))[1]
#hist(k.ses.mpd.nulls[,2])
#abline(v=k.pairwise.red[4,1], col="pink")

# Residual - 
rank(c(k.pairwise.red[5,1], k.ses.mpd.nulls[,1]))[1]
#hist(k.ses.mpd.nulls[,1])
#abline(v=k.pairwise.red[5,1], col="pink")

# ----------------------------------------------------------------------------#
#### SES MNTD - KNEPP ####
# ----------------------------------------------------------------------------#

colnames(k.ses.mntd.nulls) <- 1:999
k.ses.mntd.nulls <- as.data.frame(t(k.ses.mntd.nulls)) # transpose row to columns
names(k.ses.mntd.nulls)[1] <- "Residual"
names(k.ses.mntd.nulls)[2] <- "Minor"
names(k.ses.mntd.nulls)[3] <- "Major"
names(k.ses.mntd.nulls)[4] <- "Fractal"
names(k.ses.mntd.nulls)[5] <- "Block"

# Block - 
rank(c(k.nearest.red[1,1], k.ses.mntd.nulls[,5]))[1]
#hist(k.ses.mntd.nulls[,5])
#abline(v=k.nearest.red[1,1], col="purple")

# Fractal - 
rank(c(k.nearest.red[2,1], k.ses.mntd.nulls[,4]))[1]
#hist(k.ses.mntd.nulls[,4])
#abline(v=k.nearest.red[2,1], col="purple")

# Major - 
rank(c(k.nearest.red[3,1], k.ses.mntd.nulls[,3]))[1]
#hist(k.ses.mntd.nulls[,3])
#abline(v=k.nearest.red[3,1], col="purple")

# Minor - 
rank(c(k.nearest.red[4,1], k.ses.mntd.nulls[,2]))[1]
#hist(k.ses.mntd.nulls[,2])
#abline(v=k.nearest.red[4,1], col="purple")

# Residual - 
rank(c(k.nearest.red[5,1], k.ses.mntd.nulls[,1]))[1]
#hist(k.ses.mntd.nulls[,1])
#abline(v=k.nearest.red[5,1], col="purple")


# ----------------------------------------------------------------------------#
#### SR - BOOTHBY ####
# ----------------------------------------------------------------------------#

colnames(b.rich.nulls) <- 1:999
b.rich.nulls <- as.data.frame(t(b.rich.nulls)) # transpose row to columns
names(b.rich.nulls)[1] <- "Residual"
names(b.rich.nulls)[2] <- "Minor"
names(b.rich.nulls)[3] <- "Major"
names(b.rich.nulls)[4] <- "Fractal"

# Fractal - 
rank(c(b.richness.red[1,1], b.rich.nulls[,4]))[1]
#hist(b.rich.nulls[,4])
#abline(v=b.richness.red[1,1], col="red")

# Major - 
rank(c(b.richness.red[2,1], b.rich.nulls[,3]))[1]
#hist(b.rich.nulls[,3])
#abline(v=b.richness.red[2,1], col="red")

# Minor - 
rank(c(b.richness.red[3,1], b.rich.nulls[,2]))[1]
#hist(b.rich.nulls[,2])
#abline(v=b.richness.red[3,1], col="red")

# Residual - 
rank(c(b.richness.red[4,1], b.rich.nulls[,1]))[1]
#hist(b.rich.nulls[,1])
#abline(v=b.richness.red[4,1], col="red")

# ----------------------------------------------------------------------------#
#### Faith's PD - BOOTHBY ####
# ----------------------------------------------------------------------------#

colnames(b.pd.nulls) <- 1:999
b.pd.nulls <- as.data.frame(t(b.pd.nulls)) # transpose row to columns
names(b.pd.nulls)[1] <- "Residual"
names(b.pd.nulls)[2] <- "Minor"
names(b.pd.nulls)[3] <- "Major"
names(b.pd.nulls)[4] <- "Fractal"

# Fractal - 
rank(c(b.phylo.red[1,1], b.pd.nulls[,4]))[1]
#hist(b.pd.nulls[,4])
#abline(v=b.phylo.red[1,1], col="blue")

# Major - 
rank(c(b.phylo.red[2,1], b.pd.nulls[,3]))[1]
#hist(b.pd.nulls[,3])
#abline(v=b.phylo.red[2,1], col="blue")

# Minor - 
rank(c(b.phylo.red[3,1], b.pd.nulls[,2]))[1]
#hist(b.pd.nulls[,2])
#abline(v=b.phylo.red[3,1], col="blue")

# Residual - 
rank(c(b.phylo.red[4,1], b.pd.nulls[,1]))[1]
#hist(b.pd.nulls[,1])
#abline(v=b.phylo.red[4,1], col="blue")

# ----------------------------------------------------------------------------#
#### SES MPD - BOOTHBY ####
# ----------------------------------------------------------------------------#

colnames(b.ses.mpd.nulls) <- 1:999
b.ses.mpd.nulls <- as.data.frame(t(b.ses.mpd.nulls)) # transpose row to columns
names(b.ses.mpd.nulls)[1] <- "Residual"
names(b.ses.mpd.nulls)[2] <- "Minor"
names(b.ses.mpd.nulls)[3] <- "Major"
names(b.ses.mpd.nulls)[4] <- "Fractal"

# Fractal - 
rank(c(b.pairwise.red[1,1], b.ses.mpd.nulls[,4]))[1]
#hist(b.ses.mpd.nulls[,4])
#abline(v=b.pairwise.red[1,1], col="pink")

# Major - 
rank(c(b.pairwise.red[2,1], b.ses.mpd.nulls[,3]))[1]
#hist(b.ses.mpd.nulls[,3])
#abline(v=b.pairwise.red[2,1], col="pink")

# Minor - 
rank(c(b.pairwise.red[3,1], b.ses.mpd.nulls[,2]))[1]
#hist(b.ses.mpd.nulls[,2])
#abline(v=b.pairwise.red[3,1], col="pink")

# Residual - 
rank(c(b.pairwise.red[4,1], b.ses.mpd.nulls[,1]))[1]
#hist(b.ses.mpd.nulls[,1])
#abline(v=b.pairwise.red[4,1], col="pink")

# ----------------------------------------------------------------------------#
#### SES MNTD - BOOTHBY ####
# ----------------------------------------------------------------------------#

colnames(b.ses.mntd.nulls) <- 1:999
b.ses.mntd.nulls <- as.data.frame(t(b.ses.mntd.nulls)) # transpose row to columns
names(b.ses.mntd.nulls)[1] <- "Residual"
names(b.ses.mntd.nulls)[2] <- "Minor"
names(b.ses.mntd.nulls)[3] <- "Major"
names(b.ses.mntd.nulls)[4] <- "Fractal"

# Fractal - 
rank(c(b.nearest.red[1,1], b.ses.mntd.nulls[,4]))[1]
#hist(b.ses.mntd.nulls[,4])
#abline(v=b.nearest.red[1,1], col="purple")

# Major - 
rank(c(b.nearest.red[2,1], b.ses.mntd.nulls[,3]))[1]
#hist(b.ses.mntd.nulls[,3])
#abline(v=b.nearest.red[2,1], col="purple")

# Minor - 
rank(c(b.nearest.red[3,1], b.ses.mntd.nulls[,2]))[1]
#hist(b.ses.mntd.nulls[,2])
#abline(v=b.nearest.red[3,1], col="purple")

# Residual - 
rank(c(b.nearest.red[4,1], b.ses.mntd.nulls[,1]))[1]
#hist(b.ses.mntd.nulls[,1])
#abline(v=b.nearest.red[4,1], col="purple")

