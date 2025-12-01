# Download WorldClim v.2.1 bioclimatic data

This function allows to download data from WorldClim v.2.1
(https://www.worldclim.org/data/index.html) considering multiple GCMs,
time periods and SSPs.

## Usage

``` r
WorldClim_data(path = NULL,
               period = "current",
               variable = "bioc",
               year = "2090",
               gcm = "mi",
               ssp = "585",
               resolution = 10)
```

## Arguments

- path:

  Directory path to save downloads.

- period:

  Can be "current" or "future".

- variable:

  Allows to specify which variables you want to retrieve Possible
  entries are: "tmax","tmin","prec" and/or "bioc".

- year:

  Specify the year you want to retrieve data. Possible entries are:
  "2030", "2050", "2070" and/or "2090". You can use a vector to provide
  more than one entry.

- gcm:

  GCMs to be considered in future scenarios. You can use a vector to
  provide more than one entry.

  |          |                  |
  |----------|------------------|
  | **CODE** | **GCM**          |
  | ac       | ACCESS-CM2       |
  | ae       | ACCESS-ESM1-5    |
  | bc       | BCC-CSM2-MR      |
  | ca       | CanESM5          |
  | cc       | CanESM5-CanOE    |
  | ce       | CMCC-ESM2        |
  | cn       | CNRM-CM6-1       |
  | ch       | CNRM-CM6-1-HR    |
  | cr       | CNRM-ESM2-1      |
  | ec       | EC-Earth3-Veg    |
  | ev       | EC-Earth3-Veg-LR |
  | fi       | FIO-ESM-2-0      |
  | gf       | GFDL-ESM4        |
  | gg       | GISS-E2-1-G      |
  | gh       | GISS-E2-1-H      |
  | hg       | HadGEM3-GC31-LL  |
  | in       | INM-CM4-8        |
  | ic       | INM-CM5-0        |
  | ip       | IPSL-CM6A-LR     |
  | me       | MIROC-ES2L       |
  | mi       | MIROC6           |
  | mp       | MPI-ESM1-2-HR    |
  | ml       | MPI-ESM1-2-LR    |
  | mr       | MRI-ESM2-0       |
  | uk       | UKESM1-0-LL      |

- ssp:

  SSPs for future data. Possible entries are: "126", "245", "370" and/or
  "585". You can use a vector to provide more than one entry.

- resolution:

  You can select one resolution from the following alternatives: 10, 5,
  2.5 OR 30.

## Value

If data is not downloaded, the function downloads the data and has no
return value. If the data is downloaded, it imports the data as a
`stack`.

## Details

This function will create a folder entitled
"input_data/WorldClim_data_current" or
"input_data/WorldClim_data_future". All the data downloaded will be
stored in this folder. Note that, despite being possible to retrieve a
lot of data at once, it is not recommended to do so, since the data is
very heavy.

## References

https://www.worldclim.org/data/index.html

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# \donttest{
# download data from multiple periods:
year <- c("2050", "2090")
WorldClim_data(period = "future",
               variable = "bioc",
               year = year,
               gcm = "mi",
               ssp = "126",
               resolution = 10)
#> [1] "mi_ssp126_10_2050"
#> [1] "mi_ssp126_10_2090"
#> list()

# download data from one specific period
WorldClim_data(period = "future",
               variable = "bioc",
               year = "2070",
               gcm = "mi",
               ssp = "585",
               resolution = 10)
#> [1] "mi_ssp585_10_2070"
#> list()
# }
```
