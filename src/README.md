The `src` contains the following: 

| Script                     | Description                                                                     |
|----------------------------|---------------------------------------------------------------------------------|
| `predictive_checks/plot.R` | Plots prior-posterior updates, prior & posterior predictive checks and trace plots        |
| `predictive_checks/run_model.R` | Runs predictive checks across the four scenarios in `data/games_for_pred_check.csv` |
| `recovery/plot_recovery.R` | Plots for parameter recovery                                                   |
| `recovery/run_recovery.R`  | Run parameter recovery across trials, priors                                    |
| `game_functions.R`         | Functions used in `simulate.R` to simulate data (e.g., RL agent, simple agent etc.) |
| `illustrate_priors.R`      | Script to illustrate different priors used + sigmoid curves across different values of tau |
| `simulate.R`               | Simulate data for predictive checks and for parameter recovery (across 60, 120 and 300 trials) |