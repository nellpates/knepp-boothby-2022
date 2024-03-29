# Knepp and Boothby 2022 write up
# Nell Pates
# p-values for Figure 4
# 14 Dec 2023 - FINAL

# This ranks our model's output against the 999 null permutations
# From this we determine the p-values
# uncomment to see #histograms which visualise the ranked results.

# -----------------------------------------------------------------------------#
#### SET UP: ####
# -----------------------------------------------------------------------------#

source("src/headers.R")

# load data from Figure_4_set-up:
load("clean-data/violinplots.RData")

# read in data from null permutations:
k.rich.nulls <- as.data.frame(readRDS("clean-data/nulls/k.rich.vars.RDS"))
k.pd.nulls <- as.data.frame(readRDS("clean-data/nulls/k.pd.vars.RDS"))
k.simp.nulls <- as.data.frame(readRDS("clean-data/nulls/k.simpson.vars.RDS"))
k.ses.mpd.nulls <- as.data.frame(readRDS("clean-data/nulls/k.ses.mpd.vars.RDS"))
k.ses.mntd.nulls <- as.data.frame(readRDS("clean-data/nulls/k.ses.mntd.vars.RDS"))

b.rich.nulls <- as.data.frame(readRDS("clean-data/nulls/b.rich.vars.RDS"))
b.pd.nulls <- as.data.frame(readRDS("clean-data/nulls/b.pd.vars.RDS"))
b.simpson.nulls <- as.data.frame(readRDS("clean-data/nulls/b.simpson.vars.RDS"))
b.ses.mpd.nulls <- as.data.frame(readRDS("clean-data/nulls/b.ses.mpd.vars.RDS"))
b.ses.mntd.nulls <- as.data.frame(readRDS("clean-data/nulls/b.ses.mntd.vars.RDS"))

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

# Use rank() to identify where our result ranks among the 999 null permutations
# Block - 998
rank(c(k.richness.red[1,1], k.rich.nulls[,5]))[1]
#hist(k.rich.nulls[,5])
#abline(v=k.richness.red[1,1], col="red")

# Fractal - 761
rank(c(k.richness.red[2,1], k.rich.nulls[,4]))[1]
#hist(k.rich.nulls[,4])
#abline(v=k.richness.red[2,1], col="red")

# Major - 17
rank(c(k.richness.red[3,1], k.rich.nulls[,3]))[1]
#hist(k.rich.nulls[,3])
#abline(v=k.richness.red[3,1], col="red")

# Minor - 180
rank(c(k.richness.red[4,1], k.rich.nulls[,2]))[1]
#hist(k.rich.nulls[,2])
#abline(v=k.richness.red[4,1], col="red")

# Residual - 95
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

# Block - 100
rank(c(k.phylo.red[1,1], k.pd.nulls[,5]))[1]
#hist(k.pd.nulls[,5])
#abline(v=k.phylo.red[1,1], col="blue")

# Fractal - 62
rank(c(k.phylo.red[2,1], k.pd.nulls[,4]))[1]
#hist(k.pd.nulls[,4])
#abline(v=k.phylo.red[2,1], col="blue")

# Major - 6
rank(c(k.phylo.red[3,1], k.pd.nulls[,3]))[1]
#hist(k.pd.nulls[,3])
#abline(v=k.phylo.red[3,1], col="blue")

# Minor - 52
rank(c(k.phylo.red[4,1], k.pd.nulls[,2]))[1]
#hist(k.pd.nulls[,2])
#abline(v=k.phylo.red[4,1], col="blue")

# Residual - 11
rank(c(k.phylo.red[5,1], k.pd.nulls[,1]))[1]
#hist(k.pd.nulls[,1])
#abline(v=k.phylo.red[5,1], col="blue")

# ----------------------------------------------------------------------------#
#### Simpson's Diversity Index - KNEPP ####
# ----------------------------------------------------------------------------#

colnames(k.simp.nulls) <- 1:999
k.simp.nulls <- as.data.frame(t(k.simp.nulls)) # transpose row to columns
names(k.simp.nulls)[1] <- "Residual"
names(k.simp.nulls)[2] <- "Minor"
names(k.simp.nulls)[3] <- "Major"
names(k.simp.nulls)[4] <- "Fractal"
names(k.simp.nulls)[5] <- "Block"

# Block - 971
rank(c(k.simpsons.red[1,1], k.simp.nulls[,5]))[1]
#hist(k.simp.nulls[,5])
#abline(v=k.simpsons.red[1,1], col="green")

# Fractal - 818
rank(c(k.simpsons.red[2,1], k.simp.nulls[,4]))[1]
#hist(k.simp.nulls[,4])
#abline(v=k.simpsons.red[2,1], col="green")

# Major - 762
rank(c(k.simpsons.red[3,1], k.simp.nulls[,3]))[1]
#hist(k.simp.nulls[,3])
#abline(v=k.simpsons.red[3,1], col="green")

# Minor - 670
rank(c(k.simpsons.red[4,1], k.simp.nulls[,2]))[1]
#hist(k.simp.nulls[,2])
#abline(v=k.simpsons.red[4,1], col="green")

# Residual - 334
rank(c(k.simpsons.red[5,1], k.simp.nulls[,1]))[1]
#hist(k.simp.nulls[,1])
#abline(v=k.simpsons.red[5,1], col="green")

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

# Block - 989
rank(c(k.pairwise.red[1,1], k.ses.mpd.nulls[,5]))[1]
#hist(k.ses.mpd.nulls[,5])
#abline(v=k.pairwise.red[1,1], col="pink")

# Fractal - 917
rank(c(k.pairwise.red[2,1], k.ses.mpd.nulls[,4]))[1]
#hist(k.ses.mpd.nulls[,4])
#abline(v=k.pairwise.red[2,1], col="pink")

# Major - 616
rank(c(k.pairwise.red[3,1], k.ses.mpd.nulls[,3]))[1]
#hist(k.ses.mpd.nulls[,3])
#abline(v=k.pairwise.red[3,1], col="pink")

# Minor - 723
rank(c(k.pairwise.red[4,1], k.ses.mpd.nulls[,2]))[1]
#hist(k.ses.mpd.nulls[,2])
#abline(v=k.pairwise.red[4,1], col="pink")

# Residual - 178
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

# Block - 966
rank(c(k.nearest.red[1,1], k.ses.mntd.nulls[,5]))[1]
#hist(k.ses.mntd.nulls[,5])
#abline(v=k.nearest.red[1,1], col="purple")

# Fractal - 487
rank(c(k.nearest.red[2,1], k.ses.mntd.nulls[,4]))[1]
#hist(k.ses.mntd.nulls[,4])
#abline(v=k.nearest.red[2,1], col="purple")

# Major - 191
rank(c(k.nearest.red[3,1], k.ses.mntd.nulls[,3]))[1]
#hist(k.ses.mntd.nulls[,3])
#abline(v=k.nearest.red[3,1], col="purple")

# Minor - 968
rank(c(k.nearest.red[4,1], k.ses.mntd.nulls[,2]))[1]
#hist(k.ses.mntd.nulls[,2])
#abline(v=k.nearest.red[4,1], col="purple")

# Residual - 6
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

# Fractal - 645
rank(c(b.richness.red[1,1], b.rich.nulls[,4]))[1]
#hist(b.rich.nulls[,4])
#abline(v=b.richness.red[1,1], col="red")

# Major - 988
rank(c(b.richness.red[2,1], b.rich.nulls[,3]))[1]
#hist(b.rich.nulls[,3])
#abline(v=b.richness.red[2,1], col="red")

# Minor - 554
rank(c(b.richness.red[3,1], b.rich.nulls[,2]))[1]
#hist(b.rich.nulls[,2])
#abline(v=b.richness.red[3,1], col="red")

# Residual - 58
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

# Fractal - 54
rank(c(b.phylo.red[1,1], b.pd.nulls[,4]))[1]
#hist(b.pd.nulls[,4])
#abline(v=b.phylo.red[1,1], col="blue")

# Major - 818
rank(c(b.phylo.red[2,1], b.pd.nulls[,3]))[1]
#hist(b.pd.nulls[,3])
#abline(v=b.phylo.red[2,1], col="blue")

# Minor - 791
rank(c(b.phylo.red[3,1], b.pd.nulls[,2]))[1]
#hist(b.pd.nulls[,2])
#abline(v=b.phylo.red[3,1], col="blue")

# Residual - 7
rank(c(b.phylo.red[4,1], b.pd.nulls[,1]))[1]
#hist(b.pd.nulls[,1])
#abline(v=b.phylo.red[4,1], col="blue")

# ----------------------------------------------------------------------------#
#### Simpson's Diversity Index - BOOTHBY ####
# ----------------------------------------------------------------------------#

colnames(b.simp.nulls) <- 1:999
b.simp.nulls <- as.data.frame(t(b.simp.nulls)) # transpose row to columns
names(b.simp.nulls)[1] <- "Residual"
names(b.simp.nulls)[2] <- "Minor"
names(b.simp.nulls)[3] <- "Major"
names(b.simp.nulls)[4] <- "Fractal"

# Fractal - 766
rank(c(b.simpsons.red[1,1], b.simp.nulls[,4]))[1]
#hist(b.simp.nulls[,4])
#abline(v=b.simpsons.red[1,1], col="green")

# Major - 987
rank(c(b.simpsons.red[2,1], b.simp.nulls[,3]))[1]
#hist(b.simp.nulls[,3])
#abline(v=b.simpsons.red[2,1], col="green")

# Minor - 727
rank(c(b.simpsons.red[3,1], b.simp.nulls[,2]))[1]
#hist(b.simp.nulls[,2])
#abline(v=b.simpsons.red[3,1], col="green")

# Residual - 80
rank(c(b.simpsons.red[4,1], b.simp.nulls[,1]))[1]
#hist(b.simp.nulls[,1])
#abline(v=b.simpsons.red[4,1], col="green")

# ----------------------------------------------------------------------------#
#### SES MPD - BOOTHBY ####
# ----------------------------------------------------------------------------#

colnames(b.ses.mpd.nulls) <- 1:999
b.ses.mpd.nulls <- as.data.frame(t(b.ses.mpd.nulls)) # transpose row to columns
names(b.ses.mpd.nulls)[1] <- "Residual"
names(b.ses.mpd.nulls)[2] <- "Minor"
names(b.ses.mpd.nulls)[3] <- "Major"
names(b.ses.mpd.nulls)[4] <- "Fractal"

# Fractal - 501
rank(c(b.pairwise.red[1,1], b.ses.mpd.nulls[,4]))[1]
#hist(b.ses.mpd.nulls[,4])
#abline(v=b.pairwise.red[1,1], col="pink")

# Major - 795
rank(c(b.pairwise.red[2,1], b.ses.mpd.nulls[,3]))[1]
#hist(b.ses.mpd.nulls[,3])
#abline(v=b.pairwise.red[2,1], col="pink")

# Minor - 340
rank(c(b.pairwise.red[3,1], b.ses.mpd.nulls[,2]))[1]
#hist(b.ses.mpd.nulls[,2])
#abline(v=b.pairwise.red[3,1], col="pink")

# Residual - 627
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

# Fractal - 319
rank(c(b.nearest.red[1,1], b.ses.mntd.nulls[,4]))[1]
#hist(b.ses.mntd.nulls[,4])
#abline(v=b.nearest.red[1,1], col="purple")

# Major - 735
rank(c(b.nearest.red[2,1], b.ses.mntd.nulls[,3]))[1]
#hist(b.ses.mntd.nulls[,3])
#abline(v=b.nearest.red[2,1], col="purple")

# Minor - 506
rank(c(b.nearest.red[3,1], b.ses.mntd.nulls[,2]))[1]
#hist(b.ses.mntd.nulls[,2])
#abline(v=b.nearest.red[3,1], col="purple")

# Residual - 599
rank(c(b.nearest.red[4,1], b.ses.mntd.nulls[,1]))[1]
#hist(b.ses.mntd.nulls[,1])
#abline(v=b.nearest.red[4,1], col="purple")