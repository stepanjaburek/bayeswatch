# bayeswatch  🏖️

> Watch some nice MCMC animations while your bayesian model samples.

The (mostly vibecoded) R package **bayeswatch** opens the amazing [Chi Feng's interactive MCMC demo](https://github.com/chi-feng/mcmc-demo) in your the RStudio/Positron Viewer pane when your `brms` or `rethinking` model starts sampling and you can kill time with some cool visuals while waiting.

---

## Installation

```r
# Install from GitHub:
remotes::install_github("stepanjaburek/bayeswatch")

```

---

## Usage

### brms

```r
library(bayeswatch)
library(brms)

fit <- brm_with_viz(
  bf(y ~ x + (1 | group)),
  data   = mydata,
  family = gaussian(),
  chains = 4,
  iter   = 2000
)
```

### rethinking 

```r
library(bayeswatch)
library(rethinking)

m <- ulam_with_viz(
  alist(
    y      ~ dnorm(mu, sigma),
    mu     <- a + b * x,
    a      ~ dnorm(0, 10),
    b      ~ dnorm(0, 1),
    sigma  ~ dexp(1)
  ),
  data = list(y = d$y, x = d$x)
)

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
