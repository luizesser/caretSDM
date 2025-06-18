# caretSDM

Luíz Fernando Esser

# caretSDM <a href="https://luizesser.github.io/caretSDM/"><img src="man/figures/logo.png" alt="caretSDM website" align="right" height="85"/></a>

`caretSDM` is a under development R package that uses the powerful `caret` package as the main engine to obtain Species Distribution Models. As `caret` is a packaged turned to build machine learning models, `caretSDM` has a strong focus on this approach.

## Installing

First we will install the package from github. For that we will need to install the `devtools` package first and then install the `caretSDM` package.

``` r
install.packages(setdiff("devtools", rownames(installed.packages())))
devtools::install_github("luizesser/caretSDM", build_vignettes = TRUE) # This can take around 7 minutes to complete.
```

``` r
library(caretSDM)
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

-   `Araucaria` is the main vignette for terrestrial species modeling.

-   `Salminus` is the main vignette for continental aquatic species modeling.

To access the vignettes in R, simply run:

``` r
vignette("Araucaria", "caretSDM")
```
