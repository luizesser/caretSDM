# Changelog

## caretSDM 1.1.0

CRAN release: 2025-07-09

- Initial CRAN submission.

## caretSDM 1.1.1

- Inclusion of Non-native Distribution using caretSDM with adaptations
  on sdm_area function to allow the use of scenarios that do not
  particularly match the modeling area.

## caretSDM 1.1.2

- Inclusion of methods to include new algorithms in caretSDM.

## caretSDM 1.1.3

- Inclusion of Ensemble of Small Models in caretSDM.

## caretSDM 1.1.4

CRAN release: 2025-08-29

- Inclusion of functions to add different classes together, as well as
  plots for niche and background. First attempts to include independent
  test data. tSNE is also working properly now. Inclusion of testing to
  improve code coverage.

## caretSDM 1.1.5

- Plotting ensemble predictions is working. Added blockCV methods for
  crossvalidation.

## caretSDM 1.2

- Inclusion of MaxEnt and Mahalanobis Distance directly on caretSDM.
- Pseudoabsences can now be retrieved outside a buffer.
- Inclusion of functions: correlate_sdm, prediction_change_sdm and
  tuneGrid_sdm.

## caretSDM 1.2.1

- A error on plots assertion was corrected to better describe assertion
  problems.

## caretSDM 1.2.2

- A CRS correction for add_scenarios function was missing for the
  crop_area parameter.

## caretSDM 1.2.3

CRAN release: 2025-11-06

- There was a problem with add_scenarios function when adding a stars
  with a crop_by.

## caretSDM 1.2.4

- input_sdm now checks if user passed occurrences to join_area.

## caretSDM 1.2.5

CRAN release: 2026-01-08

- buffer_sdm function now has a convex hull argument, allowing a buffer
  to be created around it.
- WorldClim_data function now uses httr2 package to gracefully handle
  errors.

## caretSDM 1.3

- caretSDM now does not allow to run maxent without previously run the
  background function.
