# endogenous group confound example
#install.packages("remotes")
remotes::install_github("stepanjaburek/bayeswatch")
library(bayeswatch)
library(brms)


set.seed(11)

N_groups <- 30
N_id <- 200
a0 <- (-2)
bZY <- (-0.5)
g <- sample(1:N_groups, size = N_id, replace = TRUE) # sample into groups
Ug <- rnorm(N_groups, 1.5)                            # group confounds
X <- rnorm(N_id, Ug[g])                               # individual varying trait
Z <- rnorm(N_groups)                                   # group varying trait (observed)
Y <- rbinom(N_id, size = 1, prob = plogis(a0 + X + Ug[g] + bZY * Z[g]))


data <- data.frame(
  Y = Y,
  X = X,
  Z = Z[g],  
  g = g
)


mod <- brm_with_viz(Y ~ X + Z + (1 | g), data = data, family = bernoulli(),
                    chains = 4, cores = 4)
summary(mod)
