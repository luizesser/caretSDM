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

# sdm_area - sdm_area para ser detectado

    Code
      expect_equal(.detect_sdm_area(sa$grid, 50000, 6933), sa)

# sdm_area - sdm_area para ser detectado com parametros diferentes

    Code
      expect_equal(.detect_sdm_area(sa$grid, 30000, 5388), sa)
    Condition
      Warning:
      ! A sdm_area object was detected but some parameters are different. Please check it!
      i The cell size of the polygon of the row 1 of the grid is different from the cell_size.
      i Detected CRS (WGS 84 / NSIDC EASE-) is different from informed one (EPSG:5388).

# sdm_area - sdm_area para ser detectado com avisos

    Code
      expect_equal(sa2 <- sdm_area(sa$grid, cell_size = 40000, crs = 5388), sa)
    Condition
      Warning:
      ! A sdm_area object was detected but some parameters are different. Please check it!
      i The cell size of the polygon of the row 1 of the grid is different from the cell_size.
      i Detected CRS (WGS 84 / NSIDC EASE-) is different from informed one (EPSG:5388).

