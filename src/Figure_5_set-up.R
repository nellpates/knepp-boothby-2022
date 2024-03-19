# Knepp and Boothby 2022 write up
# Nell Pates
# Setting up data point to add to violin plots
# 14 Dec 2023 - FINAL

# This runs the model for each metric, and stores the data ready for plotting
# This means any edits to plots can be made without re-running the above set-up

# -----------------------------------------------------------------------------#
#### SET UP: ####
# -----------------------------------------------------------------------------#

source("src/headers.R")

# load data - outputs from  "workspace.R"
k.c.data <- readRDS("clean-data/k.c.data.RDS")
b.c.data <- readRDS("clean-data/b.c.data.RDS")

# -----------------------------------------------------------------------------#
#### WRAPPER FUNCTIONS: ####
# -----------------------------------------------------------------------------#

# fits the model and returns variances as a proportion of total
# running 10,000 iterations with warmup = 8000 and adapt.delta = .999 to reduce
# divergent transitions

fit.model.knepp <- function(response){
  par(mar=c(5.1,5.5,4.1,2.1))
  model <- stan_lmer(response ~ (1|block/fractal/major/minor), data=env(k.c.data), iter=10000, warmup=8000, adapt_delta=.999)
  coefs <- as.data.frame(summary(model))
  variances <- coefs[c("sigma","Sigma[minor:(major:(fractal:block)):(Intercept),(Intercept)]", "Sigma[major:(fractal:block):(Intercept),(Intercept)]","Sigma[fractal:block:(Intercept),(Intercept)]","Sigma[block:(Intercept),(Intercept)]"),"50%"]
  names(variances) <- c("residual","minor","major","fractal","block")
  variances <- variances[c("block","fractal","major","minor","residual")]
  variances <- variances/sum(variances)
  return(variances)
}

fit.model.boothby <- function(response){
  par(mar=c(5.1,5.5,4.1,2.1))
  model <- stan_lmer(response ~ (1|fractal/major/minor), data=env(b.c.data), iter=10000, warmup=8000, adapt_delta=.999)
  coefs <- as.data.frame(summary(model))
  variances <- coefs[c("sigma","Sigma[minor:(major:fractal):(Intercept),(Intercept)]","Sigma[major:fractal:(Intercept),(Intercept)]","Sigma[fractal:(Intercept),(Intercept)]"),"50%"]
  names(variances) <- c("residual","minor","major","fractal")
  variances <- variances[c("fractal","major","minor","residual")]
  variances <- variances/sum(variances)
  return(variances)
}

# ----------------------------------------------------------------------------#
#### Getting results ####
# ----------------------------------------------------------------------------#
# ----------------------------------------------------------------------------#
#### KNEPP ####
# ----------------------------------------------------------------------------#

# Species richness (drawn from .ses.mntd)
k.richness <- .ses.mntd(k.c.data)$ntaxa 

# fit model using wrapper function
k.richness.red <- as.data.frame(fit.model.knepp(k.richness)) 
# call this "value" because it is what plotting expects to find
names(k.richness.red)[1] <- "value" 
# call this "name" because it is what plotting expects to find
k.richness.red$name <- c("Block", "Fractal", "Major", "Minor", "Residual")

# Faith's PD
k.phylo <- .pd(k.c.data)[,"pd"]
k.phylo.red <- as.data.frame(fit.model.knepp(k.phylo)) 
names(k.phylo.red)[1] <- "value"
k.phylo.red$name <- c("Block", "Fractal", "Major", "Minor", "Residual")

# Simpson's Diversity Index
k.simpsons <- diversity(k.c.data$comm, "simpson")
k.simpsons.red <- as.data.frame(fit.model.knepp(k.simpsons)) 
names(k.simpsons.red)[1] <- "value"
k.simpsons.red$name <- c("Block", "Fractal", "Major", "Minor", "Residual")

# SES MPD
k.pairwise <- .ses.mpd(k.c.data)$mpd.obs.z
k.pairwise.red <- as.data.frame(fit.model.knepp(k.pairwise)) 
names(k.pairwise.red)[1] <- "value"
k.pairwise.red$name <- c("Block", "Fractal", "Major", "Minor", "Residual")

# SES MNTD
k.nearest <- .ses.mntd(k.c.data)$mntd.obs.z
k.nearest.red <- as.data.frame(fit.model.knepp(k.nearest)) 
names(k.nearest.red)[1] <- "value"
k.nearest.red$name <- c("Block", "Fractal", "Major", "Minor", "Residual")


# ----------------------------------------------------------------------------#
#### BOOTHBY ####
# ----------------------------------------------------------------------------#

# Repeat all of the above for Boothby

b.richness <- .ses.mntd(b.c.data)$ntaxa

b.richness.red <- as.data.frame(fit.model.boothby(b.richness)) 
names(b.richness.red)[1] <- "value"
b.richness.red$name <- c("Fractal", "Major", "Minor", "Residual")

b.phylo <- .pd(b.c.data)[,"pd"]
b.phylo.red <- as.data.frame(fit.model.boothby(b.phylo)) 
names(b.phylo.red)[1] <- "value"
b.phylo.red$name <- c("Fractal", "Major", "Minor", "Residual")

b.simpsons <- diversity(b.c.data$comm, "simpson")
b.simpsons.red <- as.data.frame(fit.model.boothby(b.simpsons)) 
names(b.simpsons.red)[1] <- "value"
b.simpsons.red$name <- c("Fractal", "Major", "Minor", "Residual")

b.pairwise <- .ses.mpd(b.c.data)$mpd.obs.z
b.pairwise.red <- as.data.frame(fit.model.boothby(b.pairwise)) 
names(b.pairwise.red)[1] <- "value"
b.pairwise.red$name <- c("Fractal", "Major", "Minor", "Residual")
b.pairwise <- na.omit(b.pairwise)

b.nearest <- .ses.mntd(b.c.data)$mntd.obs.z
b.nearest.red <- as.data.frame(fit.model.boothby(b.nearest)) 
names(b.nearest.red)[1] <- "value"
b.nearest.red$name <- c("Fractal", "Major", "Minor", "Residual")
b.nearest <- na.omit(b.nearest)

# Save workspace image to clean-data
# This means any edits to plots can be made without re-running the above set-up
save.image("clean-data/violinplots.RData")

# Uncomment below for correlation tests for SR and PD
# KNEPP
# Shapiro-Wilk normality test for SR
#shapiro.test(k.rich) # => p = 0.6784
# Shapiro-Wilk normality test for PD
#shapiro.test(k.pd) # => p = 0.1855
#hist(k.rich)
#hist(k.pd)

#ggqqplot(k.rich, ylab = "k.rich")
#ggqqplot(k.pd, ylab = "k.pd")

#cor.test(k.rich, k.pd, method = "pearson")
# cor 0.8591997
# t = 9.1979, df = 30, p-value = 3.09e-10

# BOOTHBY = not normal data distribution, both heavily right-skewed

# Shapiro-Wilk normality test for SR
#shapiro.test(b.rich) # => p = 0.000...
# Shapiro-Wilk normality test for PD
#shapiro.test(b.pd) # => p = 0.000....
#hist(b.rich)
#hist(b.pd)

#ggqqplot(b.rich, ylab = "b.rich")
#ggqqplot(b.pd, ylab = "b.pd")

# if the data are not normally distributed, it’s recommended to use the non-parametric correlation, 
# including Spearman and Kendall rank-based correlation tests.
#cor.test(b.rich, b.pd, method = "spearman")
# correlation coefficient = rho 0.8643651 
# S = 11144, p-value < 2.2e-16
