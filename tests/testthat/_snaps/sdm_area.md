# sdm_area - sf/predictors no variables selected

    Code
      sa <- sdm_area(pr_gpkg, cell_size = 2, variables_selected = c("CODIGOIB1",
        "NOMEUF2", "foo"))
    Condition
      Warning:
      ! Some selected variables not found!
      i Variables: foo.
    Message
      ! Making grid over study area is an expensive task. Please, be patient!
      i Using GDAL to make the grid and resample the variables.

# sdm_area - print

    Code
      print(sa)
    Output
                caretSDM         
      ...........................
      Class                     : sdm_area
      Extent                    : -55.32071 -27.64062 -47.32071 -21.64062 (xmin, xmax, ymin, ymax)
      CRS                       : WGS 84 
      Resolution                : (2, 2) (x, y)
      Number of Predictors      : 4 
      Predictors Names          : GID0, CODIGOIB1, NOMEUF2, SIGLAUF3 

---

    Code
      sa
    Output
                caretSDM         
      ...........................
      Class                     : sdm_area
      Extent                    : -5371070 -3380515 -4571070 -2680515 (xmin, xmax, ymin, ymax)
      CRS                       : WGS 84 / NSIDC EASE- 
      Resolution                : (1e+05, 1e+05) (x, y)
      Number of Predictors      : 3 
      Predictors Names          : bio1, bio4, bio12 

# sdm_area - sdm_area para ser detectado

    Code
      expect_equal(caretSDM:::.detect_sdm_area(sa$grid, 1e+05, 6933, gdal = TRUE,
      lines_as_sdm_area = FALSE), sa)

# sdm_area - sdm_area para ser detectado com parametros diferentes

    Code
      expect_equal(caretSDM:::.detect_sdm_area(sa$grid, 90000, 5839, gdal = TRUE,
      lines_as_sdm_area = FALSE), sa)
    Condition
      Warning:
      ! A sdm_area object was detected but some parameters are different. Please check it!
      i The cell size of the polygon of the row 1 of the grid is different from the cell_size.
      i Detected CRS (WGS 84 / NSIDC EASE-) is different from informed one (EPSG:5839).

# sdm_area - sdm_area para ser detectado com avisos

    Code
      expect_equal(sa2 <- sdm_area(sa$grid, cell_size = 90000, crs = 5839), sa)
    Condition
      Warning:
      ! A sdm_area object was detected but some parameters are different. Please check it!
      i The cell size of the polygon of the row 1 of the grid is different from the cell_size.
      i Detected CRS (WGS 84 / NSIDC EASE-) is different from informed one (EPSG:5839).

# sdm_area - sf+gdal=F

    Code
      sa
    Output
                caretSDM         
      ...........................
      Class                     : sdm_area
      Extent                    : -5301744 -3295037 -4601744 -2795037 (xmin, xmax, ymin, ymax)
      CRS                       : EPSG:6933 
      Resolution                : (1e+05, 1e+05) (x, y)
      Number of Predictors      : 4 
      Predictors Names          : GID0, CODIGOIB1, NOMEUF2, SIGLAUF3 

# sdm_area - stars+gdal=F

    Code
      sa
    Output
                caretSDM         
      ...........................
      Class                     : sdm_area
      Extent                    : -5288678 -3292030 -4588678 -2692030 (xmin, xmax, ymin, ymax)
      CRS                       : EPSG:6933 
      Resolution                : (1e+05, 1e+05) (x, y)
      Number of Predictors      : 2 
      Predictors Names          : wc2.1_10m_bio_1, wc2.1_10m_bio_12 

