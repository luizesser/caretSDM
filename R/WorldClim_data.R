#' Download WorldClim v.2.1 bioclimatic data
#'
#' This function allows to download data from WorldClim v.2.1
#' (https://www.worldclim.org/data/index.html) considering multiple GCMs, time periods and SSPs.
#'
#' @usage
#' WorldClim_data(path = NULL,
#'                period = "current",
#'                variable = "bioc",
#'                year = "2090",
#'                gcm = "mi",
#'                ssp = "585",
#'                resolution = 10)
#'
#' @param path Directory path to save downloads.
#' @param period Can be "current" or "future".
#' @param variable Allows to specify which variables you want to retrieve Possible entries are:
#' "tmax","tmin","prec" and/or "bioc".
#' @param year Specify the year you want to retrieve data. Possible entries are:
#' "2030", "2050", "2070" and/or "2090". You can  use a vector to provide more than one entry.
#' @param gcm GCMs to be considered in future scenarios. You can use a vector to provide more than
#' one entry.
#'  | **CODE** | **GCM** |
#'  | ---- | ---------------- |
#'  | ac  | ACCESS-CM2 |
#'  | ae  | ACCESS-ESM1-5 |
#'  | bc  | BCC-CSM2-MR |
#'  | ca  | CanESM5 |
#'  | cc  | CanESM5-CanOE |
#'  | ce  | CMCC-ESM2 |
#'  | cn  | CNRM-CM6-1 |
#'  | ch  | CNRM-CM6-1-HR |
#'  | cr  | CNRM-ESM2-1 |
#'  | ec  | EC-Earth3-Veg |
#'  | ev  | EC-Earth3-Veg-LR |
#'  | fi  | FIO-ESM-2-0 |
#'  | gf  | GFDL-ESM4 |
#'  | gg  | GISS-E2-1-G |
#'  | gh  | GISS-E2-1-H |
#'  | hg  | HadGEM3-GC31-LL |
#'  | in  | INM-CM4-8 |
#'  | ic  | INM-CM5-0 |
#'  | ip  | IPSL-CM6A-LR |
#'  | me  | MIROC-ES2L |
#'  | mi  | MIROC6 |
#'  | mp  | MPI-ESM1-2-HR |
#'  | ml  | MPI-ESM1-2-LR |
#'  | mr  | MRI-ESM2-0 |
#'  | uk  | UKESM1-0-LL |
#' @md
#' @param ssp SSPs for future data. Possible entries are: "126", "245", "370" and/or "585".
#' You can use a vector to provide more than one entry.
#' @param resolution You can select one resolution from the following alternatives: 10, 5, 2.5 OR 30.
#'
#' @details This function will create a folder entitled "input_data/WorldClim_data_current" or
#' "input_data/WorldClim_data_future". All the data downloaded will be stored in this folder.
#' Note that, despite being possible to retrieve a lot of data at once, it is not recommended to do
#' so, since the data is very heavy.
#'
#' @returns If data is not downloaded, the function downloads the data and has no return value. If
#' the data is downloaded, it imports the data as a \code{stack}.
#'
#' @references https://www.worldclim.org/data/index.html
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' \donttest{
#' # download data from multiple periods:
#' year <- c("2050", "2090")
#' WorldClim_data(period = "future",
#'                variable = "bioc",
#'                year = year,
#'                gcm = "mi",
#'                ssp = "126",
#'                resolution = 10)
#'
#' # download data from one specific period
#' WorldClim_data(period = "future",
#'                variable = "bioc",
#'                year = "2070",
#'                gcm = "mi",
#'                ssp = "585",
#'                resolution = 10)
#' }
#'
#' @importFrom httr GET write_disk http_error
#' @importFrom raster stack
#' @importFrom gtools mixedsort
#' @importFrom cli cli_alert_warning cli_abort
#' @importFrom utils unzip
#'
#' @export

WorldClim_data <- function(path = NULL, period = "current", variable = "bioc", year = "2090", gcm = "mi", ssp = "585", resolution = 10) {
  if (!all(period %in% c("current", "future"))) {
    cli::cli_abort(c(
      "x" = "Assertion on {.var period} failed. ",
      "i" = "{.var period} must be element of set ['current', 'future']."
    ))
  }
  if (!all(variable %in% c("bioc", "tmax", "tmin", "prec"))) {
    cli::cli_abort(c(
      "x" = "Assertion on {.var variable} failed. ",
      "i" = "{.var variable} must be element of set ['bioc', 'tmax','tmin','prec']."
    ))
  }
  if (!all(year %in% c("2030", "2050", "2070", "2090"))) {
    cli::cli_abort(c(
      "x" = "Assertion on {.var year} failed. ",
      "i" = "{.var year} must be element of set ['2030', '2050', '2070', '2090']."
    ))
  }
  if (!all(gcm %in% c(
    "ac", "ae", "bc", "ca", "cc", "ce", "cn", "ch", "cr", "ec", "ev", "fi",
    "gf", "gg", "gh", "hg", "in", "ic", "ip", "me", "mi", "mp", "ml",
    "mr", "uk", "all"
  ))) {
    cli::cli_abort(c(
      "x" = "Assertion on {.var gcm} failed. ",
      "i" = "{.var gcm} must be element of set ['ac','ae','bc','ca','cc','ce','cn','ch','cr','ec',
      'ev','fi','gf','gg','gh','hg','in','ic','ip','me','mi','mp','ml','mr','uk', 'all']."
    ))
  }
  if (!all(ssp %in% c("126", "245", "370", "585"))) {
    cli::cli_abort(c(
      "x" = "Assertion on {.var ssp} failed. ",
      "i" = "{.var ssp} must be element of set ['126', '245', '370', '585']."
    ))
  }
  if (!all(resolution %in% c(10, 5, 2.5, 30))) {
    cli::cli_abort(c(
      "x" = "Assertion on {.var resolution} failed. ",
      "i" = "{.var resolution} must be element of set [10, 5, 2.5, 30]."
    ))
  }

  assert_character_cli(period)
  assert_character_cli(variable)
  assert_character_cli(year)
  assert_character_cli(gcm)
  assert_character_cli(ssp)
  assert_numeric_cli(resolution)
  assert_character_cli(path, null.ok = TRUE, len = 1)

  res <- ifelse(resolution == 30, "s", "m")
  l <- list()

  if (period == "current") {
    if (is.null(path)) {
      path <- "input_data/WorldClim_data_current"
    }
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    if (length(list.files(path, pattern = ".tif$", full.names = T)) == 0) {
      print(paste0("current_", resolution, res))
      httr::GET(
        url = paste0(
          "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_",
          resolution,
          res, "_bio.zip"
        ),
        httr::write_disk(paste0(path, "/current_", resolution, res, ".zip"))
      )
      utils::unzip(
        zipfile = paste0(path, "/current_", resolution, res, ".zip"),
        exdir = path
      )
    } else {
      print(paste0("The file for current scenario is already downloaded."))
      print(paste0("Importing current from folder..."))
      l[["current"]] <- raster::stack(list.files(path, pattern = ".tif$", full.names = T))
      l[["current"]] <- l[["current"]][[gtools::mixedsort(names(l[["current"]]))]]
      names(l[["current"]]) <- paste0("bio", 1:19)
    }
  }

  if (period == "future") {
    if (is.null(path)) {
      path <- "input_data/WorldClim_data_future"
    }
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    all_gcm <- c(
      "ac", "ae", "bc", "ca", "cc", "ce", "cn", "ch", "cr", "ec", "ev", "fi",
      "gf", "gg", "gh", "hg", "in", "ic", "ip", "me", "mi", "mp", "ml",
      "mr", "uk"
    )
    gcm2 <- c(
      "ACCESS-CM2", "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CanESM5-CanOE", "CMCC-ESM2",
      "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg",
      "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G",
      "GISS-E2-1-H", "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0",
      "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR",
      "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL"
    )
    if ("all" %in% gcm) {
      gcm <- all_gcm
    }
    gcm3 <- gcm2[match(gcm, all_gcm)]
    all_year <- c("2030", "2050", "2070", "2090")
    year2 <- c("2021-2040", "2041-2060", "2061-2080", "2081-2100")
    year3 <- year2[match(year, all_year)]
    for (g in 1:length(gcm)) {
      for (s in 1:length(ssp)) {
        for (y in 1:length(year)) {
          if (!file.exists(paste0(path, "/", gcm[g], "_ssp", ssp[s], "_", resolution, "_", year[y], ".tif"))) {
            print(paste0(gcm[g], "_ssp", ssp[s], "_", resolution, "_", year[y]))
            if (!httr::http_error(paste0(
              "https://geodata.ucdavis.edu/cmip6/", resolution,
              res, "/", gcm3[g], "/ssp", ssp[s], "/wc2.1_", resolution,
              res, "_", variable, "_", gcm3[g], "_ssp", ssp[s], "_",
              year3[y], ".tif"
            ))) {
              try(httr::GET(
                url = paste0(
                  "https://geodata.ucdavis.edu/cmip6/", resolution,
                  res, "/", gcm3[g], "/ssp", ssp[s], "/wc2.1_", resolution,
                  res, "_", variable, "_", gcm3[g], "_ssp", ssp[s], "_",
                  year3[y], ".tif"
                ),
                httr::write_disk(paste0(
                  path, "/", gcm[g], "_ssp", ssp[s],
                  "_", resolution, "_", year[y], ".tif"
                ))
              ))
            }
          } else {
            print(paste0(
              "The file for future scenario (",
              paste0(path, "/", gcm[g], "_ssp", ssp[s], "_", resolution, "_", year[y], ".tif"),
              ") is already downloaded."
            ))
            nome <- paste0(gcm[g], "_ssp", ssp[s], "_", resolution, "_", year[y])
            print(paste0("Importing ", nome, " from folder..."))
            l[[nome]] <- raster::stack(list.files(path, pattern = paste0(nome, ".tif$"), full.names = T))
            l[[nome]] <- l[[nome]][[gtools::mixedsort(names(l[[nome]]))]]
            names(l[[nome]]) <- paste0("bio", 1:19)
          }
        }
      }
    }
  }
  return(l)
}
