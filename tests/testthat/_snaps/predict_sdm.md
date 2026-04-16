# predict_sdm

    Code
      p
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 419 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 419 
      --------  Predictors  ---------
      Number of Predictors          : 2 
      Predictors Names              : bio1, bio12 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes kknn 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : cv 
          Number                    : 2 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC      Sens       Spec      ROCSD
      1        kknn 0.5832341 0.9936242 0.03194444 0.09516020
      2 naive_bayes 0.8595126 0.9737298 0.37986111 0.03705704
      
      --------  Predictions  --------
      Thresholds                    :
          Method                    : threshold 
          Criteria                  : 0.5 

---

    Code
      p$predictions
    Output
               caretSDM        
      .........................
      Class             : Predictions
      Thresholds        :
               Method   : threshold 
               Criteria : 0.5 
               Metrics  :
      $`Araucaria angustifolia`
                             algo       ROC      Sens       Spec       ROCSD
      kknn_pa1               kknn 0.6094911 0.9904306 0.03125000 0.124182775
      kknn_pa2               kknn 0.5287031 0.9928230 0.06458333 0.003601790
      kknn_pa3               kknn 0.6115079 0.9976190 0.00000000 0.157696036
      naive_bayes_pa1 naive_bayes 0.8569157 0.9665414 0.42500000 0.020945159
      naive_bayes_pa2 naive_bayes 0.8426247 0.9809068 0.35833333 0.006854678
      naive_bayes_pa3 naive_bayes 0.8789975 0.9737412 0.35625000 0.083371284
                            SensSD      SpecSD
      kknn_pa1        1.353314e-02 0.044194174
      kknn_pa2        1.014986e-02 0.002946278
      kknn_pa3        1.683588e-02 0.000000000
      naive_bayes_pa1 2.711462e-02 0.247487373
      naive_bayes_pa2 8.055443e-05 0.153206469
      naive_bayes_pa3 3.479951e-03 0.129636243
      

# predict_sdm - th 0

    Code
      p
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 419 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 419 
      --------  Predictors  ---------
      Number of Predictors          : 2 
      Predictors Names              : bio1, bio12 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes kknn 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : cv 
          Number                    : 2 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC      Sens       Spec      ROCSD
      1        kknn 0.5832341 0.9936242 0.03194444 0.09516020
      2 naive_bayes 0.8595126 0.9737298 0.37986111 0.03705704
      
      --------  Predictions  --------
      Thresholds                    :
          Method                    : threshold 
          Criteria                  : 0 

---

    Code
      p$predictions
    Output
               caretSDM        
      .........................
      Class             : Predictions
      Thresholds        :
               Method   : threshold 
               Criteria : 0 
               Metrics  :
      $`Araucaria angustifolia`
                             algo       ROC      Sens       Spec       ROCSD
      kknn_pa1               kknn 0.6094911 0.9904306 0.03125000 0.124182775
      kknn_pa2               kknn 0.5287031 0.9928230 0.06458333 0.003601790
      kknn_pa3               kknn 0.6115079 0.9976190 0.00000000 0.157696036
      naive_bayes_pa1 naive_bayes 0.8569157 0.9665414 0.42500000 0.020945159
      naive_bayes_pa2 naive_bayes 0.8426247 0.9809068 0.35833333 0.006854678
      naive_bayes_pa3 naive_bayes 0.8789975 0.9737412 0.35625000 0.083371284
                            SensSD      SpecSD
      kknn_pa1        1.353314e-02 0.044194174
      kknn_pa2        1.014986e-02 0.002946278
      kknn_pa3        1.683588e-02 0.000000000
      naive_bayes_pa1 2.711462e-02 0.247487373
      naive_bayes_pa2 8.055443e-05 0.153206469
      naive_bayes_pa3 3.479951e-03 0.129636243
      

# predict_sdm - th function

    Code
      p
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 419 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 419 
      --------  Predictors  ---------
      Number of Predictors          : 2 
      Predictors Names              : bio1, bio12 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes kknn 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : cv 
          Number                    : 2 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC      Sens       Spec      ROCSD
      1        kknn 0.5832341 0.9936242 0.03194444 0.09516020
      2 naive_bayes 0.8595126 0.9737298 0.37986111 0.03705704
      
      --------  Predictions  --------
      Thresholds                    :
          Method                    : mean 
          Criteria                  : 0.7213733 

---

    Code
      p$predictions
    Output
               caretSDM        
      .........................
      Class             : Predictions
      Thresholds        :
               Method   : mean 
               Criteria : 0.7213733 
               Metrics  :
      $`Araucaria angustifolia`
                             algo       ROC      Sens      Spec       ROCSD
      naive_bayes_pa1 naive_bayes 0.8569157 0.9665414 0.4250000 0.020945159
      naive_bayes_pa2 naive_bayes 0.8426247 0.9809068 0.3583333 0.006854678
      naive_bayes_pa3 naive_bayes 0.8789975 0.9737412 0.3562500 0.083371284
                            SensSD    SpecSD
      naive_bayes_pa1 2.711462e-02 0.2474874
      naive_bayes_pa2 8.055443e-05 0.1532065
      naive_bayes_pa3 3.479951e-03 0.1296362
      

# add_input_sdm

    Code
      p1
    Output
              caretSDM       
      .......................
      Class                 : occurrences
      Species Names         : Araucaria angustifolia Salminus brasiliensis 
      Number of presences   : 419 46 
      ========================================
      Data:
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
      ...........................
      Class                     : sdm_area
      Extent                    : -5301744 -3295037 -4601744 -2795037 (xmin, xmax, ymin, ymax)
      CRS                       : WGS 84 / NSIDC EASE- 
      Resolution                : (1e+05, 1e+05) (x, y)
      Number of Predictors      : 2 
      Predictors Names          : bio1, bio12 

---

    Code
      p3
    Output
                caretSDM         
      ...........................
      Class                     : sdm_area
      Extent                    : -5301744 -3295037 -4601744 -2795037 (xmin, xmax, ymin, ymax)
      CRS                       : WGS 84 / NSIDC EASE- 
      Resolution                : (1e+05, 1e+05) (x, y)
      Number of Predictors      : 2 
      Predictors Names          : bio1, bio12 

---

    Code
      p4
    Output
               caretSDM        
      .........................
      Class                   : Models
      Algorithms Names        : naive_bayes 
      Variables Names         : bio1 bio12 
      Model Validation        :
               Method          : boot 
               Number          : 1 
               Metrics         :
      $`Salminus brasiliensis`
                             algo       ROC        TSS Sensitivity Specificity
      naive_bayes_pa1 naive_bayes 0.6194444 0.01111111         0.9       0.111
      naive_bayes_pa2 naive_bayes 0.7023810 0.44444444         1.0       0.444
                      Pos Pred Value Neg Pred Value Precision Recall    F1 Prevalence
      naive_bayes_pa1          0.692          0.333     0.692    0.9 0.783      0.690
      naive_bayes_pa2          0.737          1.000     0.737    1.0 0.848      0.609
                      Detection Rate Detection Prevalence Balanced Accuracy Accuracy
      naive_bayes_pa1          0.621                0.897             0.506    0.655
      naive_bayes_pa2          0.609                0.913             0.722    0.783
                      Kappa AccuracyLower AccuracyUpper AccuracyNull AccuracyPValue
      naive_bayes_pa1 0.014         0.457         0.821        0.690          0.732
      naive_bayes_pa2 0.493         0.563         0.925        0.609          0.264
                      McnemarPValue Positive Negative True Positive False Positive
      naive_bayes_pa1         0.114       20        9            18              2
      naive_bayes_pa2         0.074       14        9            14              0
                      True Negative False Negative   CBI pAUC Omission_10pct ROCSD
      naive_bayes_pa1             1              8 0.300  NaN          0.100    NA
      naive_bayes_pa2             4              7 0.899  NaN          0.071    NA
                      TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      naive_bayes_pa1    NA            NA            NA               NA
      naive_bayes_pa2    NA            NA            NA               NA
                      Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD
      naive_bayes_pa1               NA          NA       NA   NA           NA
      naive_bayes_pa2               NA          NA       NA   NA           NA
                      Detection RateSD Detection PrevalenceSD Balanced AccuracySD
      naive_bayes_pa1               NA                     NA                  NA
      naive_bayes_pa2               NA                     NA                  NA
                      AccuracySD KappaSD AccuracyLowerSD AccuracyUpperSD
      naive_bayes_pa1         NA      NA              NA              NA
      naive_bayes_pa2         NA      NA              NA              NA
                      AccuracyNullSD AccuracyPValueSD McnemarPValueSD PositiveSD
      naive_bayes_pa1             NA               NA              NA         NA
      naive_bayes_pa2             NA               NA              NA         NA
                      NegativeSD True PositiveSD False PositiveSD True NegativeSD
      naive_bayes_pa1         NA              NA               NA              NA
      naive_bayes_pa2         NA              NA               NA              NA
                      False NegativeSD CBISD pAUCSD Omission_10pctSD
      naive_bayes_pa1               NA    NA     NA               NA
      naive_bayes_pa2               NA    NA     NA               NA
      
      $`Araucaria angustifolia`
                             algo       ROC       TSS Sensitivity Specificity
      naive_bayes_pa1 naive_bayes 0.9132420 0.3264840       0.993       0.333
      naive_bayes_pa2 naive_bayes 0.8387097 0.4548387       0.955       0.500
                      Pos Pred Value Neg Pred Value Precision Recall    F1 Prevalence
      naive_bayes_pa1          0.935          0.833     0.935  0.993 0.963      0.907
      naive_bayes_pa2          0.967          0.417     0.967  0.955 0.961      0.939
                      Detection Rate Detection Prevalence Balanced Accuracy Accuracy
      naive_bayes_pa1          0.901                0.963             0.663    0.932
      naive_bayes_pa2          0.897                0.927             0.727    0.927
                      Kappa AccuracyLower AccuracyUpper AccuracyNull AccuracyPValue
      naive_bayes_pa1 0.447         0.881         0.965        0.907          0.669
      naive_bayes_pa2 0.416         0.876         0.962        0.939          1.000
                      McnemarPValue Positive Negative True Positive False Positive
      naive_bayes_pa1         0.211      146       15           145              5
      naive_bayes_pa2         0.773      155       10           148             23
                      True Negative False Negative   CBI pAUC Omission_10pct ROCSD
      naive_bayes_pa1             5             11 0.862  NaN          0.075    NA
      naive_bayes_pa2             5              5 0.383  NaN          0.090    NA
                      TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      naive_bayes_pa1    NA            NA            NA               NA
      naive_bayes_pa2    NA            NA            NA               NA
                      Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD
      naive_bayes_pa1               NA          NA       NA   NA           NA
      naive_bayes_pa2               NA          NA       NA   NA           NA
                      Detection RateSD Detection PrevalenceSD Balanced AccuracySD
      naive_bayes_pa1               NA                     NA                  NA
      naive_bayes_pa2               NA                     NA                  NA
                      AccuracySD KappaSD AccuracyLowerSD AccuracyUpperSD
      naive_bayes_pa1         NA      NA              NA              NA
      naive_bayes_pa2         NA      NA              NA              NA
                      AccuracyNullSD AccuracyPValueSD McnemarPValueSD PositiveSD
      naive_bayes_pa1             NA               NA              NA         NA
      naive_bayes_pa2             NA               NA              NA         NA
                      NegativeSD True PositiveSD False PositiveSD True NegativeSD
      naive_bayes_pa1         NA              NA               NA              NA
      naive_bayes_pa2         NA              NA               NA              NA
                      False NegativeSD CBISD pAUCSD Omission_10pctSD
      naive_bayes_pa1               NA    NA     NA               NA
      naive_bayes_pa2               NA    NA     NA               NA
      

---

    Code
      p5
    Output
               caretSDM        
      .........................
      Class             : Predictions
      Thresholds        :
               Method   : threshold 
               Criteria : 0.5 0.6 
               Metrics  :
      $`Salminus brasiliensis`
                             algo       ROC        TSS Sensitivity Specificity
      naive_bayes_pa1 naive_bayes 0.6194444 0.01111111         0.9       0.111
      naive_bayes_pa2 naive_bayes 0.7023810 0.44444444         1.0       0.444
                      Pos Pred Value Neg Pred Value Precision Recall    F1 Prevalence
      naive_bayes_pa1          0.692          0.333     0.692    0.9 0.783      0.690
      naive_bayes_pa2          0.737          1.000     0.737    1.0 0.848      0.609
                      Detection Rate Detection Prevalence Balanced Accuracy Accuracy
      naive_bayes_pa1          0.621                0.897             0.506    0.655
      naive_bayes_pa2          0.609                0.913             0.722    0.783
                      Kappa AccuracyLower AccuracyUpper AccuracyNull AccuracyPValue
      naive_bayes_pa1 0.014         0.457         0.821        0.690          0.732
      naive_bayes_pa2 0.493         0.563         0.925        0.609          0.264
                      McnemarPValue Positive Negative True Positive False Positive
      naive_bayes_pa1         0.114       20        9            18              2
      naive_bayes_pa2         0.074       14        9            14              0
                      True Negative False Negative   CBI pAUC Omission_10pct ROCSD
      naive_bayes_pa1             1              8 0.300  NaN          0.100    NA
      naive_bayes_pa2             4              7 0.899  NaN          0.071    NA
                      TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      naive_bayes_pa1    NA            NA            NA               NA
      naive_bayes_pa2    NA            NA            NA               NA
                      Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD
      naive_bayes_pa1               NA          NA       NA   NA           NA
      naive_bayes_pa2               NA          NA       NA   NA           NA
                      Detection RateSD Detection PrevalenceSD Balanced AccuracySD
      naive_bayes_pa1               NA                     NA                  NA
      naive_bayes_pa2               NA                     NA                  NA
                      AccuracySD KappaSD AccuracyLowerSD AccuracyUpperSD
      naive_bayes_pa1         NA      NA              NA              NA
      naive_bayes_pa2         NA      NA              NA              NA
                      AccuracyNullSD AccuracyPValueSD McnemarPValueSD PositiveSD
      naive_bayes_pa1             NA               NA              NA         NA
      naive_bayes_pa2             NA               NA              NA         NA
                      NegativeSD True PositiveSD False PositiveSD True NegativeSD
      naive_bayes_pa1         NA              NA               NA              NA
      naive_bayes_pa2         NA              NA               NA              NA
                      False NegativeSD CBISD pAUCSD Omission_10pctSD
      naive_bayes_pa1               NA    NA     NA               NA
      naive_bayes_pa2               NA    NA     NA               NA
      
      $`Araucaria angustifolia`
                             algo       ROC       TSS Sensitivity Specificity
      naive_bayes_pa1 naive_bayes 0.9132420 0.3264840       0.993       0.333
      naive_bayes_pa2 naive_bayes 0.8387097 0.4548387       0.955       0.500
                      Pos Pred Value Neg Pred Value Precision Recall    F1 Prevalence
      naive_bayes_pa1          0.935          0.833     0.935  0.993 0.963      0.907
      naive_bayes_pa2          0.967          0.417     0.967  0.955 0.961      0.939
                      Detection Rate Detection Prevalence Balanced Accuracy Accuracy
      naive_bayes_pa1          0.901                0.963             0.663    0.932
      naive_bayes_pa2          0.897                0.927             0.727    0.927
                      Kappa AccuracyLower AccuracyUpper AccuracyNull AccuracyPValue
      naive_bayes_pa1 0.447         0.881         0.965        0.907          0.669
      naive_bayes_pa2 0.416         0.876         0.962        0.939          1.000
                      McnemarPValue Positive Negative True Positive False Positive
      naive_bayes_pa1         0.211      146       15           145              5
      naive_bayes_pa2         0.773      155       10           148             23
                      True Negative False Negative   CBI pAUC Omission_10pct ROCSD
      naive_bayes_pa1             5             11 0.862  NaN          0.075    NA
      naive_bayes_pa2             5              5 0.383  NaN          0.090    NA
                      TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      naive_bayes_pa1    NA            NA            NA               NA
      naive_bayes_pa2    NA            NA            NA               NA
                      Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD
      naive_bayes_pa1               NA          NA       NA   NA           NA
      naive_bayes_pa2               NA          NA       NA   NA           NA
                      Detection RateSD Detection PrevalenceSD Balanced AccuracySD
      naive_bayes_pa1               NA                     NA                  NA
      naive_bayes_pa2               NA                     NA                  NA
                      AccuracySD KappaSD AccuracyLowerSD AccuracyUpperSD
      naive_bayes_pa1         NA      NA              NA              NA
      naive_bayes_pa2         NA      NA              NA              NA
                      AccuracyNullSD AccuracyPValueSD McnemarPValueSD PositiveSD
      naive_bayes_pa1             NA               NA              NA         NA
      naive_bayes_pa2             NA               NA              NA         NA
                      NegativeSD True PositiveSD False PositiveSD True NegativeSD
      naive_bayes_pa1         NA              NA               NA              NA
      naive_bayes_pa2         NA              NA               NA              NA
                      False NegativeSD CBISD pAUCSD Omission_10pctSD
      naive_bayes_pa1               NA    NA     NA               NA
      naive_bayes_pa2               NA    NA     NA               NA
      

---

    Code
      p6
    Output
               caretSDM        
      .........................
      Class             : Predictions
      Thresholds        :
               Method   : threshold 
               Criteria : 0.5 
               Metrics  :
      $`Salminus brasiliensis`
                             algo       ROC        TSS Sensitivity Specificity
      naive_bayes_pa1 naive_bayes 0.6194444 0.01111111         0.9       0.111
      naive_bayes_pa2 naive_bayes 0.7023810 0.44444444         1.0       0.444
                      Pos Pred Value Neg Pred Value Precision Recall    F1 Prevalence
      naive_bayes_pa1          0.692          0.333     0.692    0.9 0.783      0.690
      naive_bayes_pa2          0.737          1.000     0.737    1.0 0.848      0.609
                      Detection Rate Detection Prevalence Balanced Accuracy Accuracy
      naive_bayes_pa1          0.621                0.897             0.506    0.655
      naive_bayes_pa2          0.609                0.913             0.722    0.783
                      Kappa AccuracyLower AccuracyUpper AccuracyNull AccuracyPValue
      naive_bayes_pa1 0.014         0.457         0.821        0.690          0.732
      naive_bayes_pa2 0.493         0.563         0.925        0.609          0.264
                      McnemarPValue Positive Negative True Positive False Positive
      naive_bayes_pa1         0.114       20        9            18              2
      naive_bayes_pa2         0.074       14        9            14              0
                      True Negative False Negative   CBI pAUC Omission_10pct ROCSD
      naive_bayes_pa1             1              8 0.300  NaN          0.100    NA
      naive_bayes_pa2             4              7 0.899  NaN          0.071    NA
                      TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      naive_bayes_pa1    NA            NA            NA               NA
      naive_bayes_pa2    NA            NA            NA               NA
                      Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD
      naive_bayes_pa1               NA          NA       NA   NA           NA
      naive_bayes_pa2               NA          NA       NA   NA           NA
                      Detection RateSD Detection PrevalenceSD Balanced AccuracySD
      naive_bayes_pa1               NA                     NA                  NA
      naive_bayes_pa2               NA                     NA                  NA
                      AccuracySD KappaSD AccuracyLowerSD AccuracyUpperSD
      naive_bayes_pa1         NA      NA              NA              NA
      naive_bayes_pa2         NA      NA              NA              NA
                      AccuracyNullSD AccuracyPValueSD McnemarPValueSD PositiveSD
      naive_bayes_pa1             NA               NA              NA         NA
      naive_bayes_pa2             NA               NA              NA         NA
                      NegativeSD True PositiveSD False PositiveSD True NegativeSD
      naive_bayes_pa1         NA              NA               NA              NA
      naive_bayes_pa2         NA              NA               NA              NA
                      False NegativeSD CBISD pAUCSD Omission_10pctSD
      naive_bayes_pa1               NA    NA     NA               NA
      naive_bayes_pa2               NA    NA     NA               NA
      

---

    Code
      p7
    Output
               caretSDM        
      .........................
      Class             : Predictions
      Thresholds        :
               Method   : threshold 
               Criteria : 0.6 
               Metrics  :
      $`Araucaria angustifolia`
                             algo       ROC       TSS Sensitivity Specificity
      naive_bayes_pa1 naive_bayes 0.9132420 0.3264840       0.993       0.333
      naive_bayes_pa2 naive_bayes 0.8387097 0.4548387       0.955       0.500
                      Pos Pred Value Neg Pred Value Precision Recall    F1 Prevalence
      naive_bayes_pa1          0.935          0.833     0.935  0.993 0.963      0.907
      naive_bayes_pa2          0.967          0.417     0.967  0.955 0.961      0.939
                      Detection Rate Detection Prevalence Balanced Accuracy Accuracy
      naive_bayes_pa1          0.901                0.963             0.663    0.932
      naive_bayes_pa2          0.897                0.927             0.727    0.927
                      Kappa AccuracyLower AccuracyUpper AccuracyNull AccuracyPValue
      naive_bayes_pa1 0.447         0.881         0.965        0.907          0.669
      naive_bayes_pa2 0.416         0.876         0.962        0.939          1.000
                      McnemarPValue Positive Negative True Positive False Positive
      naive_bayes_pa1         0.211      146       15           145              5
      naive_bayes_pa2         0.773      155       10           148             23
                      True Negative False Negative   CBI pAUC Omission_10pct ROCSD
      naive_bayes_pa1             5             11 0.862  NaN          0.075    NA
      naive_bayes_pa2             5              5 0.383  NaN          0.090    NA
                      TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      naive_bayes_pa1    NA            NA            NA               NA
      naive_bayes_pa2    NA            NA            NA               NA
                      Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD
      naive_bayes_pa1               NA          NA       NA   NA           NA
      naive_bayes_pa2               NA          NA       NA   NA           NA
                      Detection RateSD Detection PrevalenceSD Balanced AccuracySD
      naive_bayes_pa1               NA                     NA                  NA
      naive_bayes_pa2               NA                     NA                  NA
                      AccuracySD KappaSD AccuracyLowerSD AccuracyUpperSD
      naive_bayes_pa1         NA      NA              NA              NA
      naive_bayes_pa2         NA      NA              NA              NA
                      AccuracyNullSD AccuracyPValueSD McnemarPValueSD PositiveSD
      naive_bayes_pa1             NA               NA              NA         NA
      naive_bayes_pa2             NA               NA              NA         NA
                      NegativeSD True PositiveSD False PositiveSD True NegativeSD
      naive_bayes_pa1         NA              NA               NA              NA
      naive_bayes_pa2         NA              NA               NA              NA
                      False NegativeSD CBISD pAUCSD Omission_10pctSD
      naive_bayes_pa1               NA    NA     NA               NA
      naive_bayes_pa2               NA    NA     NA               NA
      

