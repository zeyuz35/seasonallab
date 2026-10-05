# seasonallab

A fork of Christoph Sax's experimental `seasonallab` package
from [christophsax/seasonallab](https://github.com/christophsax/seasonallab),
in order to provide some fixes with the `robust.seas` function, which is
intended to be a robust version of `seas` that *always* works.


## Installation

Using `remotes`:

```r
install.packages("remotes")
remotes::install_github("zeyuz35/seasonallab")
```

## Fixes

- fallback retries now override arguments supplied instead of failing or
  silently concatenating them
- `robust.seas` works inside functions and `lapply`/`parLapply` (the documented
  M3 workflow), and for non-symbol `x` arguments
- the start-year fix actually reaches `seas` instead of being lost
- `estimate.maxiter` is raised only when the error asks for it, matching the
  current X-13 error text
- non-time-series input rethrows the real `seas` error
