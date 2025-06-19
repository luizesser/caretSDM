# caretSDM

<!-- badges: start -->

[![R-CMD-check](https://github.com/luizesser/chooseGCM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/luizesser/chooseGCM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Luíz Fernando Esser

# caretSDM <a href="https://luizesser.github.io/caretSDM/"><img src="man/figures/logo.png" alt="caretSDM website" align="right" height="85"/></a>

`caretSDM` is a under development R package that uses the powerful `caret` package as the main engine to obtain Species Distribution Models. As `caret` is a packaged turned to build machine learning models, `caretSDM` has a strong focus on this approach.

## Installation

You can install the development version of caretSDM from
[GitHub](https://github.com/luizesser/caretSDM) with:

``` r
install.packages("devtools")
devtools::install_github("luizesser/caretSDM")
```

The package is also available on CRAN. Users are able to install it
using the following code:

``` r
install.packages("caretSDM")
```

## You need help?

caretSDM is vastly documented and has included some objects that can guide your data management. If some of your data or code seem to be wrong, try to take a look at those objects or the vignettes:

*Objects*

-   `bioc` Bioclimatic variables for current scenario in stars class.

-   `rivs` Hydrological variables for current scenario in sf class.

-   `occ` *Araucaria angustifolia* occurrence data as a dataframe.

-   `salm` *Salminus brasiliensis* occurrence data as a dataframe.

-   `parana` Shapefile to use in `sdm_area` in Simple Feature class.

-   `scen` Bioclimatic variables for future scenarios in stars class.

-   `algorithms` Dataframe with characteristics from every algorithm available in caretSDM.

*Vignettes*

-   `caretSDM Workflow for Species Distribution Modeling` is the main vignette for terrestrial species modeling, where we model the tree species *Araucaria angustifolia*.

-   `Modeling Species Distributions in Continental Water Bodies` is the main vignette for continental aquatic species modeling, where we model the fish species *Salminus brasiliensis*.

To access the vignettes in R, simply run:

``` r
vignette("Araucaria", "caretSDM")
vignette("Salminus", "caretSDM")
```
