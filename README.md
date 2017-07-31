# autoBagging 
### Automatic Hyperparameter Optimization of Bagging Workflows
##### Authored by: Fábio Pinto and Vítor Cerqueira

autoBagging is an R package for automatically optimizing bagging workflows to solve classification predictive tasks.

### Installing

Currently, autoBagging is only available in Github. Soon it will be submitted to CRAN.

Install the package using **devtools**:

- `devtools::install_github("hadley/devtools")`

followed by:

- `devtools::install_github("fhpinto/autoBagging")`

In some OS, the installation might need manual installation of recursive dependencies (e.g. data.table).

### Guidelines

The core function is **autoBagging**. Its input is simply the formula for the predictive classification task and the dataset:

- `auto_model <- autoBagging(formula, train.data)`

For predicting new instances, the model uses the standard **predict** method:

- `preds <- predict(auto_model, test.data)`

#### Contact us at: \{fhpinto, vmac\}@inesctec.pt
