# tSNE

This function calculates tSNE with presences and pseudoabsences data and
returns a list of plots.

## Usage

``` r
tsne_sdm(occ, pred = NULL, variables_selected = NULL)
```

## Arguments

- occ:

  A `occurrences` or `input_sdm` object.

- pred:

  A `predictors` object. If `occ` is of class `input_sdm`, then `pred`
  is retrieved from it.

- variables_selected:

  Variable to be used in t-SNE. It can also be 'vif', if previously
  calculated.

## Value

A list of plots, where each plot is a tSNE for a given pseudoabsence
dataset.

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com
