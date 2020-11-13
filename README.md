# covid19-ensembles
In this repo we implement several ensemble methods for quantile forecasts and evaluate them on COVID-19 forecasts from the US Forecast Hub.

**Overview:**
* `data_loading.R`  
Used to load and preprocess the forecast files as well as the ensemble forecasts and truth data.

* `data_exploration.R`  
Here we explore forecast availability depending on target_end_date and number of locations. This is used to determine which models will be included in the ensembles.

* `ensemble_methods.R`  
Here we implement the different combination methods.

* `ensemble_examples.R`  
This contains some examples on how to use the methods provided in `ensemble_methods.R`

* `compute_ensembles.R`  
Here we train the ensembles and export their forecasts.

* `scoring.R`  
Contains some metrics (WIS score, absolute error), as well as utility functions to score forecasts.

* `compute_scores.R`  
Here we evaluate the individual and ensemble models and export their scores.

* `evaluation_plots.R`  
A collection of functions to visualize the results.
