# sdm_area - sf/predictors no variables selected

    Code
      sa <- sdm_area(pr_gpkg, cell_size = 1, variables_selected = c("CODIGOIB1",
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
      Extent                    : -54.65471 -26.80772 -47.98671 -22.47352 (xmin, xmax, ymin, ymax)
      CRS                       : WGS 84 
      Resolution                : (0.3334, 0.3334) (x, y)
      Number of Predictors      : 4 
      Predictors Names          : GID0, CODIGOIB1, NOMEUF2, SIGLAUF3 

---

    Code
      sa
    Output
                caretSDM         
      ...........................
      Class                     : sdm_area
      Extent                    : -5371070 -3380515 -4521070 -2680515 (xmin, xmax, ymin, ymax)
      CRS                       : WGS 84 / NSIDC EASE- 
      Resolution                : (50000, 50000) (x, y)
      Number of Predictors      : 3 
      Predictors Names          : bio1, bio4, bio12 

# sdm_area - sdm_area para ser detectado

    Code
      expect_equal(.detect_sdm_area(sa$grid, 50000, 6933), sa)

# sdm_area - sdm_area para ser detectado com parametros diferentes

    Code
      expect_equal(.detect_sdm_area(sa$grid, 30000, 5839), sa)
    Condition
      Warning:
      ! A sdm_area object was detected but some parameters are different. Please check it!
      i The cell size of the polygon of the row 1 of the grid is different from the cell_size.
      i Detected CRS (WGS 84 / NSIDC EASE-) is different from informed one (EPSG:5839).

# sdm_area - sdm_area para ser detectado com avisos

    Code
      expect_equal(sa2 <- sdm_area(sa$grid, cell_size = 40000, crs = 5839), sa)
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
      Extent                    : -5276744 -3295037 -4626744 -2795037 (xmin, xmax, ymin, ymax)
      CRS                       : EPSG:6933 
      Resolution                : (50000, 50000) (x, y)
      Number of Predictors      : 4 
      Predictors Names          : GID0, CODIGOIB1, NOMEUF2, SIGLAUF3 

# sdm_area - stars+gdal=F

    Code
      sa
    Output
                caretSDM         
      ...........................
      Class                     : sdm_area
      Extent                    : -5286881 -3291876 -4586881 -2691876 (xmin, xmax, ymin, ymax)
      CRS                       : EPSG:6933 
      Resolution                : (50000, 50000) (x, y)
      Number of Predictors      : 2 
      Predictors Names          : wc2.1_10m_bio_1, wc2.1_10m_bio_12 

