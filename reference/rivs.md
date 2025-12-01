# Hydrologic Variables

A `sf` LINESTRING object with hydrologic variables (LENGTH_KM and
DIST_DN_KM) for the Paraná state in Brazil. Data obtained from
HydroSHEDS for river flows \>= 10m3/s.

## Usage

``` r
rivs
```

## Format

\## \`rivs\` A `sf` with 1031 attributes and 2 fiels:

- LENGTH_KM:

  Length of the river reach segment, in kilometers.

- DIST_DN_KM:

  Distance from the reach outlet, i.e., the most downstream pixel of the
  reach, to the final downstream location along the river network, in
  kilometers. This downstream location is either the pour point into the
  ocean or an endorheic sink.

## Source

\<https://www.hydrosheds.org/\>
