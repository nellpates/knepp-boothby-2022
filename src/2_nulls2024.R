# Knepp and Boothby write up
# Nell Pates
# Variance components analysis and null permutations - 2024 data
# 2 September 2024 - FINAL

# This runs the 999 x null permutations to create the violin plots in supplementary analyses
# and determine significance of results (see p-values.csv files)

# -----------------------------------------------------------------------------#
#### SETUP: ####
# -----------------------------------------------------------------------------#

#source("src/headers.R")
library("pez")
library("ape")
library("parallel")
library("rstanarm")
k.c.data <- readRDS("clean-data/k.c.data2024.RDS")
b.c.data <- readRDS("clean-data/b.c.data2024.RDS")

barnamesknepp <- c("Block", "Fractal", "Major", "Minor", "Residual")
barnamesboothby <- c("Fractal", "Major", "Minor", "Residual")


# -----------------------------------------------------------------------------#
#### WRAPPER FUNCTIONS: ####
# -----------------------------------------------------------------------------#

# fits the model and returns variances as a proportion of total
# running 10,000 iterations with warmup = 8000 and adapt.delta = .999 to reduce
# divergent transitions

# Wrapper function - null permutations KNEPP
calc.vars.knepp <- function(x){
  copy.to.permute <- global.data
  copy.to.permute$response <- sample(copy.to.permute$response)
  model <- stan_lmer(response ~ (1|block/fractal/major/minor), data=copy.to.permute, iter=10000, warmup=8000, adapt_delta=.999)
  coefs <- as.data.frame(summary(model))
  variances <- coefs[c("sigma","Sigma[minor:(major:(fractal:block)):(Intercept),(Intercept)]", "Sigma[major:(fractal:block):(Intercept),(Intercept)]","Sigma[fractal:block:(Intercept),(Intercept)]","Sigma[block:(Intercept),(Intercept)]"),"50%"]
  names(variances) <- c("residual","minor","major","fractal","block")
  variances[c("block","fractal","major","minor","residual")]
  variances <- variances/sum(variances)
  return(variances)
}

# Wrapper function - null permutations BOOTHBY
calc.vars.boothby <- function(x){
  copy.to.permute <- global.data
  copy.to.permute$response <- sample(copy.to.permute$response)
  model <- stan_lmer(response ~ (1|fractal/major/minor), data=copy.to.permute, iter=10000, warmup=8000, adapt_delta=.999)
  coefs <- as.data.frame(summary(model))
  variances <- coefs[c("sigma","Sigma[minor:(major:fractal):(Intercept),(Intercept)]","Sigma[major:fractal:(Intercept),(Intercept)]","Sigma[fractal:(Intercept),(Intercept)]"),"50%"]
  names(variances) <- c("residual","minor","major","fractal")
  variances[c("fractal","major","minor","residual")]
  variances <- variances/sum(variances)
  return(variances)
}

# ----------------------------------------------------------------------------#
#### Species richness ####
# ----------------------------------------------------------------------------#

k.rich <- .ses.mntd(k.c.data)$ntaxa
# create a global variable
global.data <- env(k.c.data)
# create a global variable for the function to draw from with sample()
global.data$response <- k.rich

# to calculate null permutations - run model 999 times
k.rich.vars <- mcMap(calc.vars.knepp, 1:999, mc.cores=50) # USING mcMAP to use multiple cores to speed run
saveRDS(k.rich.vars, "k.rich.vars2024.RDS") # save each output

# Repeat for Boothby 

b.rich <- .ses.mntd(b.c.data)$ntaxa
global.data <- env(b.c.data)
global.data$response <- b.rich

b.rich.vars <- mcMap(calc.vars.boothby, 1:999, mc.cores=50) 
saveRDS(b.rich.vars, "b.rich.vars2024.RDS")

# ----------------------------------------------------------------------------#
#### Faith's PD ####
# ----------------------------------------------------------------------------#

k.pd <- .pd(k.c.data)[,"pd"]
global.data <- env(k.c.data)
global.data$response <- k.pd

k.pd.vars <- mcMap(calc.vars.knepp, 1:999, mc.cores=50)
saveRDS(k.pd.vars, "k.pd.vars2024.RDS")

b.pd <- .pd(b.c.data)[,"pd"]
global.data <- env(b.c.data)
global.data$response <- b.pd

b.pd.vars <- mcMap(calc.vars.boothby, 1:999, mc.cores=50) 
saveRDS(b.pd.vars, "b.pd.vars2024.RDS")

# ----------------------------------------------------------------------------#
#### SES MPD ####
# ----------------------------------------------------------------------------#

k.ses.mpd <- .ses.mpd(k.c.data)$mpd.obs.z
global.data <- env(k.c.data)
global.data$response <- k.ses.mpd

k.ses.mpd.vars <- mcMap(calc.vars.knepp, 1:999, mc.cores=50)
saveRDS(k.ses.mpd.vars, "k.ses.mpd.vars2024.RDS")

b.ses.mpd <- .ses.mpd(b.c.data)$mpd.obs.z
global.data <- env(b.c.data)
global.data$response <- b.ses.mpd

b.ses.mpd.vars <- mcMap(calc.vars.boothby, 1:999, mc.cores=50) 
saveRDS(b.ses.mpd.vars, "b.ses.mpd.vars2024.RDS")

# ----------------------------------------------------------------------------#
#### SESmntd ####
# ----------------------------------------------------------------------------#

k.ses.mntd <- .ses.mntd(k.c.data)$mntd.obs.z
global.data <- env(k.c.data)
global.data$response <- k.ses.mntd

k.ses.mntd.vars <- mcMap(calc.vars.knepp, 1:999, mc.cores=50)
saveRDS(k.ses.mntd.vars, "k.ses.mntd.vars2024.RDS")

b.ses.mntd <- .ses.mntd(b.c.data)$mntd.obs.z
global.data <- env(b.c.data)
global.data$response <- b.ses.mntd

b.ses.mntd.vars <- mcMap(calc.vars.boothby, 1:999, mc.cores=50) 
saveRDS(b.ses.mntd.vars, "b.ses.mntd.vars2024.RDS")

#uncomment to save whole workspace
save.image("null_permutations2024.RData")