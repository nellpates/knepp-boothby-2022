# Knepp and Boothby write up
# Nell Pates
# Variance components analysis and null permutations - 2023 data
# 2 September 2024 - FINAL

# This runs the model for each metric, and stores the data ready for plotting
# This means any edits to plots can be made without re-running the above set-up

# -----------------------------------------------------------------------------#
#### SET UP: ####
# -----------------------------------------------------------------------------#

source("src/headers.R")

# load data - outputs from  "workspace.R"
k.c.data <- readRDS("clean-data/k.c.data2023.RDS")
b.c.data <- readRDS("clean-data/b.c.data2023.RDS")

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
x <- as.data.frame(.ses.mntd(k.c.data)$ntaxa)
hist(x$`.ses.mntd(k.c.data)$ntaxa`)
median(x$`.ses.mntd(k.c.data)$ntaxa`) # 9
IQR(x$`.ses.mntd(k.c.data)$ntaxa`) # 6

# fit model using wrapper function
k.richness.red <- as.data.frame(fit.model.knepp(k.richness)) 
# call this "value" because it is what plotting expects to find
names(k.richness.red)[1] <- "value" 
# call this "name" because it is what plotting expects to find
k.richness.red$name <- c("Block", "Fractal", "Major", "Minor", "Residual")

# Faith's PD
k.phylo <- .pd(k.c.data)[,"pd"]
hist(k.phylo)
median(k.phylo) # 962
IQR(k.phylo) # 402

k.phylo.red <- as.data.frame(fit.model.knepp(k.phylo)) 
names(k.phylo.red)[1] <- "value"
k.phylo.red$name <- c("Block", "Fractal", "Major", "Minor", "Residual")

# SES MPD
k.pairwise <- .ses.mpd(k.c.data)$mpd.obs.z
hist(k.pairwise)
x <- as.data.frame(.ses.mpd(k.c.data)$mpd.obs.z)
x <- na.omit(x)
median(x$`.ses.mpd(k.c.data)$mpd.obs.z`) # -0.561
IQR(x$`.ses.mpd(k.c.data)$mpd.obs.z`) # 0.745

k.pairwise.red <- as.data.frame(fit.model.knepp(k.pairwise)) 
names(k.pairwise.red)[1] <- "value"
k.pairwise.red$name <- c("Block", "Fractal", "Major", "Minor", "Residual")

# SES MNTD
k.nearest <- .ses.mntd(k.c.data)$mntd.obs.z
hist(k.nearest)
x <- as.data.frame(.ses.mntd(k.c.data)$mntd.obs.z)
x <- na.omit(x)
median(x$`.ses.mntd(k.c.data)$mntd.obs.z`) # -1.23
IQR(x$`.ses.mntd(k.c.data)$mntd.obs.z`) # 1.09

k.nearest.red <- as.data.frame(fit.model.knepp(k.nearest)) 
names(k.nearest.red)[1] <- "value"
k.nearest.red$name <- c("Block", "Fractal", "Major", "Minor", "Residual")

# ----------------------------------------------------------------------------#
#### BOOTHBY ####
# ----------------------------------------------------------------------------#

# Repeat all of the above for Boothby

b.richness <- .ses.mntd(b.c.data)$ntaxa
y <- as.data.frame(.ses.mntd(b.c.data)$ntaxa)
hist(y$`.ses.mntd(b.c.data)$ntaxa`)
median(y$`.ses.mntd(b.c.data)$ntaxa`) # 2
IQR(y$`.ses.mntd(b.c.data)$ntaxa`) # 5

b.richness.red <- as.data.frame(fit.model.boothby(b.richness)) 
names(b.richness.red)[1] <- "value"
b.richness.red$name <- c("Fractal", "Major", "Minor", "Residual")

b.phylo <- .pd(b.c.data)[,"pd"]
hist(b.phylo)
median(b.phylo) # 192
IQR(b.phylo) # 414

b.phylo.red <- as.data.frame(fit.model.boothby(b.phylo)) 
names(b.phylo.red)[1] <- "value"
b.phylo.red$name <- c("Fractal", "Major", "Minor", "Residual")

b.pairwise <- .ses.mpd(b.c.data)$mpd.obs.z
hist(b.pairwise)
x <- as.data.frame(.ses.mpd(b.c.data)$mpd.obs.z)
x <- na.omit(x)
median(x$`.ses.mpd(b.c.data)$mpd.obs.z`) # -0.513
IQR(x$`.ses.mpd(b.c.data)$mpd.obs.z`) # 2.26

b.pairwise.red <- as.data.frame(fit.model.boothby(b.pairwise)) 
names(b.pairwise.red)[1] <- "value"
b.pairwise.red$name <- c("Fractal", "Major", "Minor", "Residual")

b.nearest <- .ses.mntd(b.c.data)$mntd.obs.z
hist(b.nearest)
x <- as.data.frame(.ses.mntd(b.c.data)$mntd.obs.z)
x <- na.omit(x)
median(x$`.ses.mntd(b.c.data)$mntd.obs.z`) # -0.474
IQR(x$`.ses.mntd(b.c.data)$mntd.obs.z`) # 1.64

b.nearest.red <- as.data.frame(fit.model.boothby(b.nearest)) 
names(b.nearest.red)[1] <- "value"
b.nearest.red$name <- c("Fractal", "Major", "Minor", "Residual")


# Uncomment below for correlation tests for SR and PD
# KNEPP
# Shapiro-Wilk normality test for SR
shapiro.test(k.richness) # => p = 3.859e-06
# Shapiro-Wilk normality test for PD
shapiro.test(k.phylo) # => p = 0.03519
hist(k.richness)
hist(k.phylo)

ggqqplot(k.richness, ylab = "k.richness")
ggqqplot(k.phylo, ylab = "k.phylo")

cor.test(k.richness, k.phylo, method = "spearman")
# rho 0.7746581 
# S = 14608, p-value = 8.938e-16

# BOOTHBY = not normal data distribution, both heavily right-skewed

# Shapiro-Wilk normality test for SR
shapiro.test(b.richness) # => p = 0.000...
# Shapiro-Wilk normality test for PD
shapiro.test(b.phylo) # => p = 0.000....
hist(b.richness)
hist(b.phylo)

ggqqplot(b.richness, ylab = "b.richness")
ggqqplot(b.phylo, ylab = "b.phlyo")

# if the data are not normally distributed, it’s recommended to use the non-parametric correlation, 
# including Spearman and Kendall rank-based correlation tests.
cor.test(b.richness, b.phylo, method = "spearman")
# correlation coefficient = rho 0.9365979 
# S = 6720.3, p-value = < 2.2e-16

