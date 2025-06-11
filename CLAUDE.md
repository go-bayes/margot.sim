- remember to update _pkgdown.yml after adding or deleting exported functions
- after each new function or revision, unit tests an update _pkdown.yml
- run pkgdown::build_articles() after each vingette is completed
- use nz english

# margot.sim Package Context

## Overview
margot.sim is an R package for simulating longitudinal data with realistic observational challenges and evaluating causal inference methods through Monte Carlo simulation. It implements a "shadowing" framework inspired by Plato's Allegory of the Cave, where observed data are distorted versions ("shadows") of true causal processes.

