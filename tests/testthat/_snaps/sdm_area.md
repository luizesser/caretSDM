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
      ................................
      Class                          : sdm_area
      
      =========== Overview ===========
      -- Predictor variables --
      Number of predictors           : 4 
      Predictor names                : GID0, CODIGOIB1, NOMEUF2, SIGLAUF3 
      Spatial extent                 : -55.3207104, -27.6406166199261, -47.3207104, -21.6406166199261  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (2, 2) 
      Coordinate reference system    : WGS 84 ( EPSG: 4326 ) 

---

    Code
      sa
    Output
                   caretSDM           
      ................................
      Class                          : sdm_area
      
      =========== Overview ===========
      -- Predictor variables --
      Number of predictors           : 3 
      Predictor names                : bio1, bio4, bio12 
      Spatial extent                 : -5371069.60063324, -3380515.33951445, -4571069.60063324, -2680515.33951445  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (1e+05, 1e+05) 
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 

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
      expect_equal(sa2 <- sdm_area(sa$grid, cell_size = 90000, output_crs = 5839), sa)
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
      ................................
      Class                          : sdm_area
      
      =========== Overview ===========
      -- Predictor variables --
      Number of predictors           : 4 
      Predictor names                : GID0, CODIGOIB1, NOMEUF2, SIGLAUF3 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (1e+05, 1e+05) 
      Coordinate reference system    : EPSG:6933 ( EPSG: 6933 ) 

# sdm_area - stars+gdal=F

    Code
      sa
    Output
                   caretSDM           
      ................................
      Class                          : sdm_area
      
      =========== Overview ===========
      -- Predictor variables --
      Number of predictors           : 2 
      Predictor names                : wc2.1_10m_bio_1, wc2.1_10m_bio_12 
      Spatial extent                 : -5288677.89676539, -3292029.92647635, -4588677.89676539, -2692029.92647635  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (1e+05, 1e+05) 
      Coordinate reference system    : EPSG:6933 ( EPSG: 6933 ) 

