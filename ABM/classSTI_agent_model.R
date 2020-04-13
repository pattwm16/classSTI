##
## Simple SI Model with Vital Dynamics
##
## Author: William M. Patterson (Brown University)
## Date: August 2019
##

# Load EpiModel
library(EpiModel)
rm(list = ls())

# Vital Dynamics Setup ----------------------------------------------------
ages <- 15:74

# Rates per 100,000 for age groups: 15-24, 25-34, 35-44, 45-54, 55-64, 65-74
# Queried for (1) men who (2) live in Rhode Island.
# NB: These are crude rates
# Source: https://wonder.cdc.gov/controller/datarequest/D76
mortality_rate <- c(66.1, 145.9, 220.7, 463.0, 986.2, 2026.0)
# rate per person, per week
mr_pp_pw <- mortality_rate / 1e5 / 52

# Build out a mortality rate vector
age_spans <- rep(10, 6)
mr_vec <- rep(mr_pp_pw, times = age_spans)
data.frame(ages, mr_vec)

par(mar = c(3,3,2,1), mgp = c(2,1,0))
barplot(mr_vec, col = "steelblue1", xlab = "age", ylab = "Death Rate",
        ylim=c(0, 0.0004))

# Network Model Estimation ------------------------------------------------

# Initialize the network
n <- 300
nw <- network.initialize(n, directed = FALSE)

# Set up ages
ageVec <- sample(ages, n, replace = TRUE)
nw <- set.vertex.attribute(nw, "age", ageVec)

# Define the formation model: edges
formation <- ~edges + degree(0) + concurrent # + degree(2) +
              #degree(3) + absdiff("age")

# Input the appropriate target statistics for each term
mean_degree <- 2
edges <- mean_degree * (n/2)
avg.abs.age.diff <- 1.5
absdiff <- edges * avg.abs.age.diff
deg0 <- n*0.08
deg1 <- n*0.58
#deg2 <- n*0.16
#deg3 <- n*0.18


target.stats <- c(edges, deg0, deg1)#, deg2, deg3)

# Parameterize the dissolution model
coef.diss <- dissolution_coefs(~offset(edges), 60, mean(mr_vec))
coef.diss

# Fit the model
est <- netest(nw, formation, target.stats, coef.diss)

# Model diagnostics
dx <- netdx(est, nsims = 8, ncores = 8, nsteps = 500,
            nwstats.formula = ~edges + absdiff("age") + isolates + degree(0:5))
print(dx)
plot(dx, plots.joined = FALSE)


# Epidemic model simulation -----------------------------------------------

# Epidemic model parameters
param <- param.net(inf.prob = 0.15,
                   mortality.rates = mr_vec,
                   mortality.disease.mult = 2,
                   birth.rate = mean(mr_vec),
                   rec.rate = )

# Initial conditions
init <- init.net(i.num = 50)

# Read in the module functions // setwd first
source("d5-s4-module-fx.R", echo = TRUE)

# Control settings
control <- control.net(type = "SI",
                       nsims = 1,
                       ncores = parallel::detectCores(),
                       nsteps = 250,
                       aging.FUN = aging,
                       departures.FUN = dfunc,
                       arrivals.FUN = bfunc,
                       delete.nodes = TRUE,
                       depend = TRUE)

# Run the network model simulation with netsim
sim <- netsim(est, param, init, control)
print(sim)

# Plot outcomes
par(mfrow = c(1,2))
plot(sim, main = "State Prevalences")
plot(sim, main = "State Sizes", sim.lines = TRUE,
     qnts = FALSE, mean.smooth = FALSE)

par(mfrow = c(1, 2))
plot(sim, y = "num", main = "Population Size", qnts = 1, ylim = c(450, 550))
plot(sim, y = "meanAge", main = "Mean Age", qnts = 1, ylim = c(35, 50))

par(mfrow = c(1, 2))
plot(sim, y = "d.flow", mean.smooth = FALSE, qnts = 1, main = "Deaths")
plot(sim, y = "b.flow", mean.smooth = FALSE, qnts = 1, main = "Births")

# Examine the data
df <- as.data.frame(sim)
head(df, 25)

# Visualize network data ----
library("ndtv")

nw_viz <- get_network(sim)
nw_viz <- color_tea(nw_viz, verbose = FALSE)

slice.par <- list(start = 1, end = 25, interval = 1, 
                  aggregate.dur = 1, rule = "any")
render.par <- list(tween.frames = 10, show.time = FALSE)
plot.par <- list(mar = c(0, 0, 0, 0))
compute.animation(nw_viz, slice.par = slice.par, verbose = TRUE)

age <- get.vertex.attribute(nw_viz, "age")
age.size <- age/25

render.d3movie(
  nw_viz,
  render.par = render.par,
  plot.par = plot.par,
  vertex.cex = age.size,
  #vertex.sides = race.shape,
  vertex.col = "ndtvcol",
  edge.col = "darkgrey",
  vertex.border = "lightgrey",
  displaylabels = FALSE,
  filename = paste0(getwd(), "/movie.html"))




