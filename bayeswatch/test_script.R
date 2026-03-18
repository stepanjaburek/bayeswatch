# endogenous group confound example
library(bayeswatch)
library(brms)
set.seed(8672)

N_groups <- 30
N_id <- 200
a0 <- (-2)
bZY <- (-0.5)
g <- sample(1:N_groups, size = N_id, replace = TRUE) # sample into groups
Ug <- rnorm(N_groups, 1.5)                            # group confounds
X <- rnorm(N_id, Ug[g])                               # individual varying trait
Z <- rnorm(N_groups)                                   # group varying trait (observed)
Y <- rbinom(N_id, size = 1, prob = plogis(a0 + X + Ug[g] + bZY * Z[g]))

# Map the group-level Z to the individuals in g
data <- data.frame(
  Y = Y,
  X = X,
  Z = Z[g],  # expands Z from 30 values to 200 based on group index
  g = g
)

# Run brms with the HMC animation in the Viewer pane
mod <- brm_with_viz(Y ~ X + Z + (1 | g), data = data, family = bernoulli(),
                    chains = 4, cores = 4)
summary(mod)
dat <- list(Y=Y,X=X,g=g,Ng=N_groups,Z=Z)
# naive model
m0 <- ulam_with_viz(
    alist(
        Y ~ bernoulli(p),
        logit(p) <- a + bxy*X + bzy*Z[g],
        a ~ dnorm(0,10),
        c(bxy,bzy) ~ dnorm(0,1)
    ) , data=dat , chains=4 , cores=4 )
precis(m0)




m0 <- ulam_with_viz(
+     alist(
+         Y ~ bernoulli(p),
+         logit(p) <- a + bxy*X + bzy*Z[g],
+         a ~ dnorm(0,10),
+         c(bxy,bzy) ~ dnorm(0,1)
+     ) , data=dat , chains=4 , cores=4 )
