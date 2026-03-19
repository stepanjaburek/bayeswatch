# bayeswatch  🏖️

> Watch some nice MCMC animations while your bayesian model samples.

This (mostly vibecoded) R package **bayeswatch** opens the amazing [Chi Feng's interactive MCMC demo](https://github.com/chi-feng/mcmc-demo) in your the RStudio/Positron viewer pane when your `brms` or `rethinking` model starts running, so you can kill time with some cool visuals while waiting for it to sample.

---

## Installation

```r
# Install from GitHub:
remotes::install_github("stepanjaburek/bayeswatch")
library(bayeswatch)
```

---

## How it works

Warning! - The animation runs **independently** of your actual chains — it's just a
pedagogical visualisation of how MCMC moves samples a target distribution, **it is not**
not a live replay of your actual samples under the hood. But you can pretend they are!

---

## Usage

Just add **_bayeswatch** after the ulam or brm function and you're good! See below for a use case in `rethinking` and `brms`.


### rethinking

```r
library(rethinking)
library(bayeswatch)

departments <- c("PoliSci", "Bio", "Math", "CS","Econ")

df <- data.frame(
  department = rep(departments, each = 50),
  beer_consumed  = rep(seq(50, 10, -10), each = 50) + rnorm(250, 0, 2),
  publications  = rpois(250,lambda = rep(5:1, each = 50))
)

d <- list(
    publications = as.integer(df$publications),
    beer_consumed = as.numeric(df$beer_consumed),
    department = as.integer(as.factor(df$department))
)


m.1 <- ulam_bayeswatch(
    alist(
        publications ~ dpois(lambda),
        log(lambda) <- a + beta_beer * beer_consumed + beta[department],
        a ~ dnorm(0, 1),
        beta_beer ~ dnorm(0, 1),
        beta[department] ~ dnorm(0, 1)
    ), data=d, chains=4
)
precis(m.1, depth=2)
plot(m.1)
trankplot(m.1)
traceplot(m.1)
```


### brms

```r
library(brms)
library(bayeswatch)

departments <- c("PoliSci", "Bio", "Math", "CS","Econ")

df <- data.frame(
  department = rep(departments, each = 50),
  beer_consumed  = rep(seq(50, 10, -10), each = 50) + rnorm(250, 0, 2),
  publications  = rpois(250,lambda = rep(5:1, each = 50))
)

m.1 <- brm_bayeswatch(
  formula = publications ~ beer_consumed + (1 | department), 
  data = df, 
  family = poisson(),
  prior = c(
    prior(normal(1, 0.5), class = "Intercept"),
    prior(normal(0, 0.2), class = "b", coef = "beer_consumed"),
    prior(exponential(1), class = "sd", group = "department")
  )
)

summary(m.1)
plot(m.1)
```

The Viewer pane (currently) shows a banana distribution sampled by:

- **HMC** — Hamiltonian Monte Carlo
- **NUTS** — No-U-Turn Sampler; dual averaging from Hoffman and Gelman (2014) 
- **Gibbs** — Gibbs Sampler
- **RWMH** — Random Walk Metropolis-Hastings a.k.a the Rosenbluth Algorithm

You can switch between algorithms using the buttons at the top of the viewer. Orange dot shows sampling is in progress; it turns green when done.

---



## Credits

- [Chi Feng](https://chi-feng.github.io/mcmc-demo/) for the MCMC visualisations
- Richard McElreath for `rethinking` and making me understand Bayesian stats a bit better
- Claude Code for building around 95% of the package

## License

MIT - do whatever you want with it. 

But probaby don't use it for any important modelling (because its vibecoded, you know)
