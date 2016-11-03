# Optimal spraying and harvesting strategies to combat cbb: a dynamic approach

### Introduction

This repository provides the code for a forward-recursive dynamic programming model to estimate the optimal spraying and harvesting strategies to combat an invasive species to Hawaii coffee, the coffee berry borer (CBB).  To run the model follow the number order of each file name in the parent directory.  Here is a description of each file.

**Dynamic Model**

* `1-parameters.R`: initial parameter estimates to incorporate into the model

* `2-calibrated_markov_chains.R`: calibration of Markov chains based off of SHAC data for spraying and not spraying.

* `3-dynamic_model.R`: full dynamic model

**Functions in R/**

* `cherrygrowth.R`: logistic growth of green cherry based on acres \* projected cherry

* `cherrypricing.R`: dynamic cherry pricing based on Greenwell Farms current pricing 8/24/2016

* `decision.R`: decision to spray or not spray

* `markovcalibration.R`: returns calibrated Markov chains based on projected infestation when spraying or not spraying

* `maxnb.R`: dynamic function the maximize net benefit in $t$ period

* `trans.matrix.R`: standard maximum liklihood estimation of Markov process.

**Other files**

* `figures.R`: builds tables and plots from `3-dynamic_model.R`

* `doc/preliminary_results.Rmd`: document outlining initial results from baseline model

* `schac_data_calibration.R`: calibration of Markov chains from SHAC data.

