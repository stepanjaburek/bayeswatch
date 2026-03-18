# bayeswatch  🏖️

> Watch some nice MCMC animations while your bayesian model samples.

The (mostly vibecoded) R package **bayeswatch** opens the amazing [Chi Feng's interactive MCMC demo](https://github.com/chi-feng/mcmc-demo) in your the RStudio/Positron Viewer pane when your `brms` or `rethinking` model starts sampling and you can kill time with some cool visuals while waiting.

---

## Installation

```r
# Install from GitHub:
remotes::install_github("stepanjaburek/bayeswatch")
library(bayeswatch)
```

---

## Usage

### brms

```r
library(bayeswatch)
library(brms)

departments <- c( "Econ", "Bio", "Math",  "PoliSci", "Philosophy")

df <- data.frame(
  field = rep(departments, each = 20),
  beer_consumed  = rep(seq(10, 50, 10), each = 20) + rnorm(100, 0, 2),
  publications  = rpois(100, lambda = 5)
)

fit_spurious <- brm_with_viz(
  formula = publications ~ beer_consumed + (1 | field), 
  data = df, 
  family = poisson(),
  prior = c(
    prior(normal(1, 0.5), class = "Intercept"),
    prior(normal(0, 0.2), class = "b", coef = "beer_consumed"),
    prior(exponential(1), class = "sd", group = "field")
  )
)

summary(fit_spurious)
```


The Viewer pane (currently) shows:

- **HMC** — Hamiltonian Monte Carlo, matching what Stan/NUTS actually does
- **NUTS** — No-U-Turn Sampler (dual averaging)
- **Gibbs** — Gibbs Sampling for comparison
- **RWMH** — Random Walk Metropolis-Hastings for comparison

Switch between algorithms using the buttons at the top of the viewer. A pulsing amber dot shows sampling is in progress; it turns green when done.

---

## How it works

Warning! - The animation runs **independently** of your actual chains — it's just a
pedagogical visualisation of how MCMC moves through a target distribution,
not a live replay of your actual samples under the hood (but you can pretend they are!).

---


## Credits

- [Chi Feng](https://chi-feng.github.io/mcmc-demo/) for the cool MCMC visualisations
- Richard McElreath for `rethinking` and making me understand Bayesian stats accessible
- Claude Code for 95% of building the package

## License

Do whatever you want with it. 

Probaby don't use it for any important modelling (bc its vibecoded, you know)
