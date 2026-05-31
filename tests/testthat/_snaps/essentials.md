# full structure check

    Code
      i_ss
    Output
                   caretSDM           
      ................................
      Class                          : input_sdm
      
      =========== Overview ===========
      Focal Taxon                    : Araucaria angustifolia 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Temporal extent                : Current
      Observation type               : Presence-absence (pseudo-absence) and Presence-background 
      Predictor names                : bio1, bio12 
      Modelling techniques           : naive_bayes 
      Model complexity (tuneLength)  : naive_bayes 
      Model averaging                : average 
      Software                       : caretSDM v1.9.4, R version 4.5.2 (2025-10-31)
      
      ============= Data =============
      -- Biodiversity data --
      Taxon names                    : Araucaria angustifolia 
      Sample size                    : 419 
      (Pseudo)Absence data method    : random 
      Number of PA sets              : 2 
      PAs per set                    : 419 
      PA-to-presence ratio           : 1 
      Background data method         : random 
      Number of background sets      : 2 
      BGs per set                    : 31 
      Background proportion          : 1 
      -- Data partitioning --
      Training/validation method     : boot 
      Number of folds/repeats        : 1 
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
      2 Araucaria angustifolia naive_bayes_pa2 laplace=0, usekernel=TRUE, adjust=1
      -- Threshold selection --
      Threshold method               : threshold 
      Threshold criteria             : 0.5 
      
      ========== Assessment ==========
      -- Performance statistics --
      Cross-validation metrics       :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1 naive_bayes 0.8495643 0.1953128       0.977       0.218
      
      
      ========== Prediction ==========
      Prediction layers              : current 
      Prediction unit                : Occurrence Probability 
      Ensemble method                : average 
      Ensemble names                 :  

---

    Code
      i_sm
    Output
                   caretSDM           
      ................................
      Class                          : input_sdm
      
      =========== Overview ===========
      Focal Taxon                    : Araucaria angustifolia 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Temporal extent (inferred)     : 2090 - 2090 
      Observation type               : Presence-absence (pseudo-absence) and Presence-background 
      Predictor names                : bio1, bio12 
      Modelling techniques           : naive_bayes 
      Model complexity (tuneLength)  : naive_bayes 
      Model averaging                : average 
      Software                       : caretSDM v1.9.4, R version 4.5.2 (2025-10-31)
      
      ============= Data =============
      -- Biodiversity data --
      Taxon names                    : Araucaria angustifolia 
      Sample size                    : 419 
      (Pseudo)Absence data method    : random 
      Number of PA sets              : 2 
      PAs per set                    : 419 
      PA-to-presence ratio           : 1 
      Background data method         : random 
      Number of background sets      : 2 
      BGs per set                    : 31 
      Background proportion          : 1 
      -- Data partitioning --
      Training/validation method     : boot 
      Number of folds/repeats        : 1 
      -- Predictor variables --
      Number of predictors           : 2 
      Predictor names                : bio1, bio12 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (1e+05, 1e+05) 
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 
      -- Transfer data --
      Number of scenarios            : 5 
      Scenario names                 : ca_ssp245_2090, ca_ssp585_2090, mi_ssp245_2090, mi_ssp585_2090, current 
      Temporal extent (inferred)     : 2090 - 2090 
      
      ============= Model ============
      -- Model settings --
      Predictors used                : bio1, bio12 
      Model hyperparameters          :
                       species       algorithm                          parameters
      1 Araucaria angustifolia naive_bayes_pa1 laplace=0, usekernel=TRUE, adjust=1
      2 Araucaria angustifolia naive_bayes_pa2 laplace=0, usekernel=TRUE, adjust=1
      -- Threshold selection --
      Threshold method               : threshold 
      Threshold criteria             : 0.5 
      
      ========== Assessment ==========
      -- Performance statistics --
      Cross-validation metrics       :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1 naive_bayes 0.8301571 0.4364947       0.946         0.5
      
      
      ========== Prediction ==========
      Prediction layers              : current, ca_ssp245_2090, ca_ssp585_2090, mi_ssp245_2090, mi_ssp585_2090 
      Prediction unit                : Occurrence Probability 
      Ensemble method                : average 
      Ensemble names                 : _ssp245_2090, _ssp585_2090 

---

    Code
      i_ms
    Output
                   caretSDM           
      ................................
      Class                          : input_sdm
      
      =========== Overview ===========
      Focal Taxon                    : Salminus brasiliensis, Araucaria angustifolia 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Temporal extent                : Current
      Observation type               : Presence-absence (pseudo-absence) and Presence-background 
      Predictor names                : bio1, bio12 
      Modelling techniques           : naive_bayes 
      Model complexity (tuneLength)  : naive_bayes 
      Model averaging                : average 
      Software                       : caretSDM v1.9.4, R version 4.5.2 (2025-10-31)
      
      ============= Data =============
      -- Biodiversity data --
      Taxon names                    : Salminus brasiliensis, Araucaria angustifolia 
      Sample size                    : 419, 46 
      (Pseudo)Absence data method    : random 
      Number of PA sets              : 2 
      PAs per set                    : 419, 46 
      PA-to-presence ratio           : 1, 1 
      Background data method         : random 
      Number of background sets      : 2 
      BGs per set                    : 31, 31 
      Background proportion          : 1, 1 
      -- Data partitioning --
      Training/validation method     : boot 
      Number of folds/repeats        : 1 
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
                       species       algorithm                           parameters
      1  Salminus brasiliensis naive_bayes_pa1 laplace=0, usekernel=FALSE, adjust=1
      2  Salminus brasiliensis naive_bayes_pa2 laplace=0, usekernel=FALSE, adjust=1
      3 Araucaria angustifolia naive_bayes_pa1  laplace=0, usekernel=TRUE, adjust=1
      4 Araucaria angustifolia naive_bayes_pa2  laplace=0, usekernel=TRUE, adjust=1
      -- Threshold selection --
      Threshold method               : threshold 
      Threshold criteria             : 0.5 
      
      ========== Assessment ==========
      -- Performance statistics --
      Cross-validation metrics       :
      $`Salminus brasiliensis`
               algo       ROC  TSS Sensitivity Specificity
      1 naive_bayes 0.9041667 0.65           1        0.65
      
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1 naive_bayes 0.8925104 0.3524286      0.9935      0.3655
      
      
      ========== Prediction ==========
      Prediction layers              : current 
      Prediction unit                : Occurrence Probability 
      Ensemble method                : average 
      Ensemble names                 :  

---

    Code
      i_mm
    Output
                   caretSDM           
      ................................
      Class                          : input_sdm
      
      =========== Overview ===========
      Focal Taxon                    : Salminus brasiliensis, Araucaria angustifolia 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Temporal extent (inferred)     : 2090 - 2090 
      Observation type               : Presence-absence (pseudo-absence) and Presence-background 
      Predictor names                : bio1, bio12 
      Modelling techniques           : naive_bayes 
      Model complexity (tuneLength)  : naive_bayes 
      Model averaging                : average 
      Software                       : caretSDM v1.9.4, R version 4.5.2 (2025-10-31)
      
      ============= Data =============
      -- Biodiversity data --
      Taxon names                    : Salminus brasiliensis, Araucaria angustifolia 
      Sample size                    : 419, 46 
      (Pseudo)Absence data method    : random 
      Number of PA sets              : 2 
      PAs per set                    : 419, 46 
      PA-to-presence ratio           : 1, 1 
      Background data method         : random 
      Number of background sets      : 2 
      BGs per set                    : 31, 31 
      Background proportion          : 1, 1 
      -- Data partitioning --
      Training/validation method     : boot 
      Number of folds/repeats        : 1 
      -- Predictor variables --
      Number of predictors           : 2 
      Predictor names                : bio1, bio12 
      Spatial extent                 : -5301744.44724281, -3295036.62222337, -4601744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
      Spatial resolution             : (1e+05, 1e+05) 
      Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 
      -- Transfer data --
      Number of scenarios            : 5 
      Scenario names                 : ca_ssp245_2090, ca_ssp585_2090, mi_ssp245_2090, mi_ssp585_2090, current 
      Temporal extent (inferred)     : 2090 - 2090 
      
      ============= Model ============
      -- Model settings --
      Predictors used                : bio1, bio12 
      Model hyperparameters          :
                       species       algorithm                           parameters
      1  Salminus brasiliensis naive_bayes_pa1 laplace=0, usekernel=FALSE, adjust=1
      2  Salminus brasiliensis naive_bayes_pa2  laplace=0, usekernel=TRUE, adjust=1
      3 Araucaria angustifolia naive_bayes_pa1 laplace=0, usekernel=FALSE, adjust=1
      4 Araucaria angustifolia naive_bayes_pa2  laplace=0, usekernel=TRUE, adjust=1
      -- Threshold selection --
      Threshold method               : threshold 
      Threshold criteria             : 0.5 
      
      ========== Assessment ==========
      -- Performance statistics --
      Cross-validation metrics       :
      $`Salminus brasiliensis`
               algo       ROC      TSS Sensitivity Specificity
      1 naive_bayes 0.7311363 0.251462        0.89      0.4165
      
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1 naive_bayes 0.8938589 0.3466975      0.9845      0.3715
      
      
      ========== Prediction ==========
      Prediction layers              : current, ca_ssp245_2090, ca_ssp585_2090, mi_ssp245_2090, mi_ssp585_2090 
      Prediction unit                : Occurrence Probability 
      Ensemble method                : average 
      Ensemble names                 : _ssp245_2090, _ssp585_2090 

