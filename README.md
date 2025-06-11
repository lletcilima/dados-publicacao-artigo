# Replication Materials

## IDEB: A High-Stakes Educational Policy in Brazil?

**Francisca Letícia F. Lima**, **Francisca Zilania Mariano**, **Edward Martins Costa**

------------------------------------------------------------------------

### Introduction

The goal of `replication_ideb_results` is to provide transparent and reproducible materials for the replication of the results presented in the article *"IDEB: A High-Stakes Educational Policy in Brazil?"*. This repository includes the necessary data, R scripts, and outputs (figures and tables) to reproduce all empirical exercises developed in the paper.

------------------------------------------------------------------------

### Folder Structure

-   `data/`: contains the datasets required to run the analysis.\
-   `scripts/`: contains all R scripts used in the empirical exercises.\
-   `results/`: stores output tables, figures, and supplementary material.

### How to Run the Scripts

1.  Make sure the required data files are located in the `data/` folder.\
2.  Open the desired R script from the `scripts/` folder.\
3.  **Run only the code lines that are not commented out** (`#`).\
    These active lines perform the main estimation and analysis procedures.

Each script is self-contained and corresponds to a specific empirical component of the article (e.g., difference-in-differences estimations, placebo tests, heterogeneous effects, or robustness checks).

### Notes

-   All scripts were written in R. You may need to install packages such as `dplyr`, `DRDID`, `ggplot2`, `sf`, among others.
-   Set your working directory to the root of the project or use relative paths.

------------------------------------------------------------------------
