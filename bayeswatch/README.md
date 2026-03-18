# bayeswatch

> Watch your sampler sample.

**bayeswatch** opens [Chi Feng's interactive MCMC demo](https://chi-feng.github.io/mcmc-demo/) in the RStudio/Positron Viewer pane the moment your `brms` or `rethinking` model starts sampling — no extra setup, no new syntax to learn.

---

## Installation

```r
# Install from GitHub:
remotes::install_github("stepanjaburek/bayeswatch")

# Or from a local clone:
devtools::install("path/to/bayeswatch")
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

### rethinking (McElreath)

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

# Older map2stan interface
m2 <- map2stan_with_viz(flist, data = d)
```

The Viewer pane shows:

- **HMC** — Hamiltonian Monte Carlo, matching what Stan/NUTS actually does
- **NUTS** — No-U-Turn Sampler (dual averaging)
- **Gibbs** — Gibbs Sampling for comparison
- **RWMH** — Random Walk Metropolis-Hastings for comparison

Switch between algorithms using the buttons at the top of the viewer. A pulsing amber dot shows sampling is in progress; it turns green when done.

---

## How it works

```
brm_with_viz(...)
  │
  ├─ start_viewer()          # spin up httpuv on localhost:86xx
  │    └─ rstudioapi::viewer()  # open Viewer pane
  │
  ├─ callr::r_bg(brms::brm)  # sampling in background process
  │    └─ httpuv::service()   # 100ms service loop keeps viewer responsive
  │
  └─ mark_done()             # JS status poll → green badge
```

The local server serves `inst/www/index.html`, which bundles Chi Feng's
MCMC demo. A `/status` JSON endpoint is polled every 1.5 s by the page
so the "Done ✓" badge updates automatically.

The animation runs **independently** of your actual chains — it's a
pedagogical visualisation of how HMC moves through a target distribution,
not a live replay of your Stan samples.

---

## Roadmap

- [ ] Live trace plots from actual chain output (cmdstanr `$sample()` supports callbacks)
- [ ] Chain progress bar via Stan's `refresh` output parsing
- [ ] Target distribution selector (funnel, donut, banana — matching common Stan pain points)
- [ ] `shinystan`-style launch after sampling completes

---

## Credits

- [Chi Feng](https://chi-feng.github.io/mcmc-demo/) for the wonderful MCMC visualisation
- Richard McElreath for `rethinking` and making Bayesian stats accessible
- The `brms` team

## License

MIT
