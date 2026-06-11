# predict_sdm

    Code
      p
    Output
                   caretSDM           
      ................................
      Class                          : input_sdm
      
      =========== Overview ===========
      Focal Taxon                    : Araucaria angustifolia 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Temporal extent                : Current
      Observation type               : Presence-absence (pseudo-absence) 
      Predictor names                : bio1, bio12 
      Modelling algorithms           : naive_bayes, kknn 
      Model complexity (tuneLength)  : 1 
      Software                       : caretSDM v1.9.6, R version 4.6.0 (2026-04-24)
      
      ============= Data =============
      -- Biodiversity data --
      Taxon names                    : Araucaria angustifolia 
      Sample size                    : 419 
      (Pseudo)Absence data method    : random 
      Number of PA sets              : 3 
      PAs per set                    : 419 
      PA-to-presence ratio           : 1 
      -- Data partitioning --
      Training/validation method     : cv 
      Number of folds/repeats        : 2 
      -- Predictor variables --
      Number of predictors           : 2 
      Predictor names                : bio1, bio12 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (1e+05, 1e+05) 
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 
      -- Transfer data --
      Number of scenarios            : 1 
      Scenario names                 : current 
      Temporal extent                : Current
      
      ============= Model ============
      -- Model settings --
      Predictors used                : bio1, bio12 
      Model hyperparameters          :
                       species       algorithm                          parameters
      1 Araucaria angustifolia naive_bayes_pa1 laplace=0, usekernel=TRUE, adjust=1
      2 Araucaria angustifolia        kknn_pa1  kmax=9, distance=2, kernel=optimal
      3 Araucaria angustifolia naive_bayes_pa2 laplace=0, usekernel=TRUE, adjust=1
      4 Araucaria angustifolia        kknn_pa2  kmax=9, distance=2, kernel=optimal
      5 Araucaria angustifolia naive_bayes_pa3 laplace=0, usekernel=TRUE, adjust=1
      6 Araucaria angustifolia        kknn_pa3  kmax=9, distance=2, kernel=optimal
      -- Threshold selection --
      Threshold method               : threshold 
      Threshold criteria             : 0.5 
      
      ========== Assessment ==========
      -- Performance statistics --
      Cross-validation metrics       :
      $`Araucaria angustifolia`
               algo       ROC      Sens       Spec      ROCSD
      1        kknn 0.5832341 0.9936242 0.03194444 0.09516020
      2 naive_bayes 0.8595126 0.9737298 0.37986111 0.03705704
      
      
      ========== Prediction ==========
      Prediction layers              : current 
      Prediction unit                : Occurrence Probability 

---

    Code
      p$predictions
    Output
                   caretSDM           
      ................................
      Class                          : predictions
      
      =========== Overview ===========
      Focal Taxon                    : Araucaria angustifolia 
      Threshold method               : threshold 
      Threshold criteria             : 0.5 
      Prediction layers              : current 
      Prediction unit                : Occurrence Probability 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 

# predict_sdm - th 0

    Code
      p
    Output
                   caretSDM           
      ................................
      Class                          : input_sdm
      
      =========== Overview ===========
      Focal Taxon                    : Araucaria angustifolia 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Temporal extent                : Current
      Observation type               : Presence-absence (pseudo-absence) 
      Predictor names                : bio1, bio12 
      Modelling algorithms           : naive_bayes, kknn 
      Model complexity (tuneLength)  : 1 
      Software                       : caretSDM v1.9.6, R version 4.6.0 (2026-04-24)
      
      ============= Data =============
      -- Biodiversity data --
      Taxon names                    : Araucaria angustifolia 
      Sample size                    : 419 
      (Pseudo)Absence data method    : random 
      Number of PA sets              : 3 
      PAs per set                    : 419 
      PA-to-presence ratio           : 1 
      -- Data partitioning --
      Training/validation method     : cv 
      Number of folds/repeats        : 2 
      -- Predictor variables --
      Number of predictors           : 2 
      Predictor names                : bio1, bio12 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (1e+05, 1e+05) 
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 
      -- Transfer data --
      Number of scenarios            : 1 
      Scenario names                 : current 
      Temporal extent                : Current
      
      ============= Model ============
      -- Model settings --
      Predictors used                : bio1, bio12 
      Model hyperparameters          :
                       species       algorithm                          parameters
      1 Araucaria angustifolia naive_bayes_pa1 laplace=0, usekernel=TRUE, adjust=1
      2 Araucaria angustifolia        kknn_pa1  kmax=9, distance=2, kernel=optimal
      3 Araucaria angustifolia naive_bayes_pa2 laplace=0, usekernel=TRUE, adjust=1
      4 Araucaria angustifolia        kknn_pa2  kmax=9, distance=2, kernel=optimal
      5 Araucaria angustifolia naive_bayes_pa3 laplace=0, usekernel=TRUE, adjust=1
      6 Araucaria angustifolia        kknn_pa3  kmax=9, distance=2, kernel=optimal
      -- Threshold selection --
      Threshold method               : threshold 
      Threshold criteria             : 0 
      
      ========== Assessment ==========
      -- Performance statistics --
      Cross-validation metrics       :
      $`Araucaria angustifolia`
               algo       ROC      Sens       Spec      ROCSD
      1        kknn 0.5832341 0.9936242 0.03194444 0.09516020
      2 naive_bayes 0.8595126 0.9737298 0.37986111 0.03705704
      
      
      ========== Prediction ==========
      Prediction layers              : current 
      Prediction unit                : Occurrence Probability 

---

    Code
      p$predictions
    Output
                   caretSDM           
      ................................
      Class                          : predictions
      
      =========== Overview ===========
      Focal Taxon                    : Araucaria angustifolia 
      Threshold method               : threshold 
      Threshold criteria             : 0 
      Prediction layers              : current 
      Prediction unit                : Occurrence Probability 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 

# predict_sdm - th function

    Code
      p
    Output
                   caretSDM           
      ................................
      Class                          : input_sdm
      
      =========== Overview ===========
      Focal Taxon                    : Araucaria angustifolia 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Temporal extent                : Current
      Observation type               : Presence-absence (pseudo-absence) 
      Predictor names                : bio1, bio12 
      Modelling algorithms           : naive_bayes, kknn 
      Model complexity (tuneLength)  : 1 
      Software                       : caretSDM v1.9.6, R version 4.6.0 (2026-04-24)
      
      ============= Data =============
      -- Biodiversity data --
      Taxon names                    : Araucaria angustifolia 
      Sample size                    : 419 
      (Pseudo)Absence data method    : random 
      Number of PA sets              : 3 
      PAs per set                    : 419 
      PA-to-presence ratio           : 1 
      -- Data partitioning --
      Training/validation method     : cv 
      Number of folds/repeats        : 2 
      -- Predictor variables --
      Number of predictors           : 2 
      Predictor names                : bio1, bio12 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (1e+05, 1e+05) 
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 
      -- Transfer data --
      Number of scenarios            : 1 
      Scenario names                 : current 
      Temporal extent                : Current
      
      ============= Model ============
      -- Model settings --
      Predictors used                : bio1, bio12 
      Model hyperparameters          :
                       species       algorithm                          parameters
      1 Araucaria angustifolia naive_bayes_pa1 laplace=0, usekernel=TRUE, adjust=1
      2 Araucaria angustifolia        kknn_pa1  kmax=9, distance=2, kernel=optimal
      3 Araucaria angustifolia naive_bayes_pa2 laplace=0, usekernel=TRUE, adjust=1
      4 Araucaria angustifolia        kknn_pa2  kmax=9, distance=2, kernel=optimal
      5 Araucaria angustifolia naive_bayes_pa3 laplace=0, usekernel=TRUE, adjust=1
      6 Araucaria angustifolia        kknn_pa3  kmax=9, distance=2, kernel=optimal
      -- Threshold selection --
      Threshold method               : mean 
      Threshold criteria             : 0.7213733 
      
      ========== Assessment ==========
      -- Performance statistics --
      Cross-validation metrics       :
      $`Araucaria angustifolia`
               algo       ROC      Sens       Spec      ROCSD
      1        kknn 0.5832341 0.9936242 0.03194444 0.09516020
      2 naive_bayes 0.8595126 0.9737298 0.37986111 0.03705704
      
      
      ========== Prediction ==========
      Prediction layers              : current 
      Prediction unit                : Occurrence Probability 

---

    Code
      p$predictions
    Output
                   caretSDM           
      ................................
      Class                          : predictions
      
      =========== Overview ===========
      Focal Taxon                    : Araucaria angustifolia 
      Threshold method               : mean 
      Threshold criteria             : 0.7213733 
      Prediction layers              : current 
      Prediction unit                : Occurrence Probability 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 

# add_input_sdm

    Code
      p1
    Output
                   caretSDM           
      ................................
      Class                          : occurrences
      
      =========== Overview ===========
      Focal Taxon                    : Salminus brasiliensis, Araucaria angustifolia 
      Observation type               : Presence-absence (pseudo-absence) 
      Taxon names                    : Salminus brasiliensis, Araucaria angustifolia 
      Sample size                    : 419, 46 
      (Pseudo)Absence data method    : random 
      Number of PA sets              : 2, 2 
      PAs per set                    : 46, 419 
      PA-to-presence ratio           : 0.11, 9.11 
      Data structure                 :
      Simple feature collection with 6 features and 2 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -5263273 ymin: -3156734 xmax: -5002956 ymax: -2830253
      Projected CRS: WGS 84 / NSIDC EASE-Grid 2.0 Global
        cell_id               species                  geometry
      1      17 Salminus brasiliensis POINT (-5002956 -3034581)
      2      16 Salminus brasiliensis POINT (-5123570 -3049429)
      3       2 Salminus brasiliensis POINT (-5138591 -2830253)
      4      22 Salminus brasiliensis POINT (-5263273 -3143263)
      5      23 Salminus brasiliensis POINT (-5172118 -3156734)
      6      23 Salminus brasiliensis POINT (-5172118 -3156734)

---

    Code
      p2
    Output
                   caretSDM           
      ................................
      Class                          : sdm_area
      
      =========== Overview ===========
      -- Predictor variables --
      Number of predictors           : 2 
      Predictor names                : bio1, bio12 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (1e+05, 1e+05) 
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 

---

    Code
      p3
    Output
                   caretSDM           
      ................................
      Class                          : sdm_area
      
      =========== Overview ===========
      -- Predictor variables --
      Number of predictors           : 2 
      Predictor names                : bio1, bio12 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (1e+05, 1e+05) 
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 

---

    Code
      p4
    Output
                   caretSDM           
      ................................
      Class                          : models
      
      =========== Overview ===========
      Predictors used                : bio1, bio12 
      Modelling techniques           : naive_bayes 
      Model complexity (tuneLength)  : naive_bayes 
      Training/validation method     : boot 
      Number of folds/repeats        : 1 
      Model hyperparameters          :
                       species       algorithm                           parameters
      1  Salminus brasiliensis naive_bayes_pa1 laplace=0, usekernel=FALSE, adjust=1
      2  Salminus brasiliensis naive_bayes_pa2 laplace=0, usekernel=FALSE, adjust=1
      3 Araucaria angustifolia naive_bayes_pa1  laplace=0, usekernel=TRUE, adjust=1
      4 Araucaria angustifolia naive_bayes_pa2  laplace=0, usekernel=TRUE, adjust=1
      
      Validation metrics           :
      $`Salminus brasiliensis`
               algo       ROC       TSS Sensitivity Specificity
      1 naive_bayes 0.6609127 0.2277778        0.95      0.2775
      
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1 naive_bayes 0.8759758 0.3906614       0.974      0.4165
      

---

    Code
      p5
    Output
                   caretSDM           
      ................................
      Class                          : predictions
      
      =========== Overview ===========
      Focal Taxon                    : Salminus brasiliensis 
      Threshold method               : threshold 
      Threshold criteria             : 0.5 0.6 
      Prediction layers              : current, current 
      Prediction unit                : Occurrence Probability 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 

---

    Code
      p6
    Output
                   caretSDM           
      ................................
      Class                          : predictions
      
      =========== Overview ===========
      Focal Taxon                    : Salminus brasiliensis 
      Threshold method               : threshold 
      Threshold criteria             : 0.5 
      Prediction layers              : current 
      Prediction unit                : Occurrence Probability 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 

---

    Code
      p7
    Output
                   caretSDM           
      ................................
      Class                          : predictions
      
      =========== Overview ===========
      Focal Taxon                    : Araucaria angustifolia 
      Threshold method               : threshold 
      Threshold criteria             : 0.6 
      Prediction layers              : current 
      Prediction unit                : Occurrence Probability 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 

