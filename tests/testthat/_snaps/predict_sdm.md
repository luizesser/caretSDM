# predict_sdm

    Code
      p
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 420 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 420 
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
      1        kknn 0.6025314 0.9865079 0.03333333 0.14949089
      2 naive_bayes 0.8488211 0.9706349 0.43055556 0.06122881
      
      --------  Predictions  --------
      Ensembles                     :
          Scenarios                 : current 
          Methods                   : mean_occ_prob wmean_AUC committee_avg 
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
      Ensembles         :
               Methods  : mean_occ_prob wmean_AUC committee_avg 
      Thresholds        :
               Method   : threshold 
               Criteria : 0.5 
               Metrics  :
      $`Araucaria angustifolia`
                  algo       ROC      Sens       Spec      ROCSD      SensSD
      m1.2        kknn 0.5214286 0.9761905 0.06666667 0.03030458 0.033671751
      m2.2        kknn 0.6622768 0.9952381 0.00000000 0.23622838 0.006734350
      m3.2        kknn 0.6238889 0.9880952 0.03333333 0.18193970 0.003367175
      m1.1 naive_bayes 0.8760020 0.9714286 0.42083333 0.05907989 0.026937401
      m2.1 naive_bayes 0.8547569 0.9738095 0.45208333 0.02506441 0.016835876
      m3.1 naive_bayes 0.8157044 0.9666667 0.41875000 0.09954212 0.090913729
               SpecSD
      m1.2 0.09428090
      m2.2 0.00000000
      m3.2 0.04714045
      m1.1 0.15026019
      m2.1 0.15026019
      m3.1 0.07954951
      

# predict_sdm - th 0

    Code
      p
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 420 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 420 
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
      1        kknn 0.6025314 0.9865079 0.03333333 0.14949089
      2 naive_bayes 0.8488211 0.9706349 0.43055556 0.06122881
      
      --------  Predictions  --------
      Ensembles                     :
          Scenarios                 : current 
          Methods                   : mean_occ_prob wmean_AUC committee_avg 
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
      Ensembles         :
               Methods  : mean_occ_prob wmean_AUC committee_avg 
      Thresholds        :
               Method   : threshold 
               Criteria : 0 
               Metrics  :
      $`Araucaria angustifolia`
                  algo       ROC      Sens       Spec      ROCSD      SensSD
      m1.2        kknn 0.5214286 0.9761905 0.06666667 0.03030458 0.033671751
      m2.2        kknn 0.6622768 0.9952381 0.00000000 0.23622838 0.006734350
      m3.2        kknn 0.6238889 0.9880952 0.03333333 0.18193970 0.003367175
      m1.1 naive_bayes 0.8760020 0.9714286 0.42083333 0.05907989 0.026937401
      m2.1 naive_bayes 0.8547569 0.9738095 0.45208333 0.02506441 0.016835876
      m3.1 naive_bayes 0.8157044 0.9666667 0.41875000 0.09954212 0.090913729
               SpecSD
      m1.2 0.09428090
      m2.2 0.00000000
      m3.2 0.04714045
      m1.1 0.15026019
      m2.1 0.15026019
      m3.1 0.07954951
      

# predict_sdm - th function

    Code
      p
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 420 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 420 
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
      1        kknn 0.6025314 0.9865079 0.03333333 0.14949089
      2 naive_bayes 0.8488211 0.9706349 0.43055556 0.06122881
      
      --------  Predictions  --------
      Ensembles                     :
          Scenarios                 : current 
          Methods                   : mean_occ_prob wmean_AUC committee_avg 
      Thresholds                    :
          Method                    : function (x, ...)  UseMethod("mean") 
          Criteria                  : function (x, ...)  UseMethod("mean") 

---

    Code
      p$predictions
    Output
               caretSDM        
      .........................
      Class             : Predictions
      Ensembles         :
               Methods  : mean_occ_prob wmean_AUC committee_avg 
      Thresholds        :
               Method   : function (x, ...)  UseMethod("mean") 
               Criteria : function (x, ...)  UseMethod("mean") 
               Metrics  :
      $`Araucaria angustifolia`
                  algo       ROC      Sens      Spec      ROCSD     SensSD     SpecSD
      m1.1 naive_bayes 0.8760020 0.9714286 0.4208333 0.05907989 0.02693740 0.15026019
      m2.1 naive_bayes 0.8547569 0.9738095 0.4520833 0.02506441 0.01683588 0.15026019
      m3.1 naive_bayes 0.8157044 0.9666667 0.4187500 0.09954212 0.09091373 0.07954951
      

# add_input_sdm

    Code
      p1
    Output
              caretSDM       
      .......................
      Class                 : occurrences
      Species Names         : Araucaria angustifolia Salminus brasiliensis 
      Number of presences   : 420 46 
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
                  algo       ROC        TSS Sensitivity Specificity Pos Pred Value
      m1.1 naive_bayes 0.5666667 0.07222222         0.9       0.222          0.708
      m2.1 naive_bayes 0.6269841 0.44444444         1.0       0.444          0.737
           Neg Pred Value Precision Recall    F1 Prevalence Detection Rate
      m1.1            0.4     0.708    0.9 0.783      0.690          0.621
      m2.1            1.0     0.737    1.0 0.848      0.609          0.609
           Detection Prevalence Balanced Accuracy Accuracy Kappa AccuracyLower
      m1.1                0.897             0.536    0.655 0.082         0.457
      m2.1                0.913             0.722    0.783 0.493         0.563
           AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1         0.821        0.690          0.732         0.343       20        9
      m2.1         0.925        0.609          0.264         0.074       14        9
           True Positive False Positive True Negative False Negative   CBI pAUC
      m1.1            18              3             2              8 0.462  NaN
      m2.1            14              0             4              7 0.400  NaN
           Omission_10pct ROCSD TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      m1.1          0.100    NA    NA            NA            NA               NA
      m2.1          0.143    NA    NA            NA            NA               NA
           Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD Detection RateSD
      m1.1               NA          NA       NA   NA           NA               NA
      m2.1               NA          NA       NA   NA           NA               NA
           Detection PrevalenceSD Balanced AccuracySD AccuracySD KappaSD
      m1.1                     NA                  NA         NA      NA
      m2.1                     NA                  NA         NA      NA
           AccuracyLowerSD AccuracyUpperSD AccuracyNullSD AccuracyPValueSD
      m1.1              NA              NA             NA               NA
      m2.1              NA              NA             NA               NA
           McnemarPValueSD PositiveSD NegativeSD True PositiveSD False PositiveSD
      m1.1              NA         NA         NA              NA               NA
      m2.1              NA         NA         NA              NA               NA
           True NegativeSD False NegativeSD CBISD pAUCSD Omission_10pctSD
      m1.1              NA               NA    NA     NA               NA
      m2.1              NA               NA    NA     NA               NA
      
      $`Araucaria angustifolia`
                  algo       ROC       TSS Sensitivity Specificity Pos Pred Value
      m1.1 naive_bayes 0.8763975 0.4937888       0.994       0.500          0.970
      m2.1 naive_bayes 0.8222934 0.5106838       0.955       0.556          0.974
           Neg Pred Value Precision Recall    F1 Prevalence Detection Rate
      m1.1          0.833     0.970  0.994 0.982      0.942          0.936
      m2.1          0.417     0.974  0.955 0.964      0.945          0.903
           Detection Prevalence Balanced Accuracy Accuracy Kappa AccuracyLower
      m1.1                0.965             0.747    0.965 0.608         0.925
      m2.1                0.933             0.755    0.933 0.441         0.884
           AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1         0.987        0.942          0.326         0.724      161       10
      m2.1         0.966        0.945          0.882         0.773      156        9
           True Positive False Positive True Negative False Negative   CBI pAUC
      m1.1           160              3             5              5 0.304  NaN
      m2.1           149              7             5              5 0.495  NaN
           Omission_10pct ROCSD TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      m1.1          0.075    NA    NA            NA            NA               NA
      m2.1          0.083    NA    NA            NA            NA               NA
           Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD Detection RateSD
      m1.1               NA          NA       NA   NA           NA               NA
      m2.1               NA          NA       NA   NA           NA               NA
           Detection PrevalenceSD Balanced AccuracySD AccuracySD KappaSD
      m1.1                     NA                  NA         NA      NA
      m2.1                     NA                  NA         NA      NA
           AccuracyLowerSD AccuracyUpperSD AccuracyNullSD AccuracyPValueSD
      m1.1              NA              NA             NA               NA
      m2.1              NA              NA             NA               NA
           McnemarPValueSD PositiveSD NegativeSD True PositiveSD False PositiveSD
      m1.1              NA         NA         NA              NA               NA
      m2.1              NA         NA         NA              NA               NA
           True NegativeSD False NegativeSD CBISD pAUCSD Omission_10pctSD
      m1.1              NA               NA    NA     NA               NA
      m2.1              NA               NA    NA     NA               NA
      

---

    Code
      p5
    Output
               caretSDM        
      .........................
      Class             : Predictions
      Ensembles         :
               Methods  : mean_occ_prob wmean_AUC committee_avg 
      Thresholds        :
               Method   : threshold 
               Criteria : 0.5 0.6 
               Metrics  :
      $`Salminus brasiliensis`
                  algo       ROC        TSS Sensitivity Specificity Pos Pred Value
      m1.1 naive_bayes 0.5666667 0.07222222         0.9       0.222          0.708
      m2.1 naive_bayes 0.6269841 0.44444444         1.0       0.444          0.737
           Neg Pred Value Precision Recall    F1 Prevalence Detection Rate
      m1.1            0.4     0.708    0.9 0.783      0.690          0.621
      m2.1            1.0     0.737    1.0 0.848      0.609          0.609
           Detection Prevalence Balanced Accuracy Accuracy Kappa AccuracyLower
      m1.1                0.897             0.536    0.655 0.082         0.457
      m2.1                0.913             0.722    0.783 0.493         0.563
           AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1         0.821        0.690          0.732         0.343       20        9
      m2.1         0.925        0.609          0.264         0.074       14        9
           True Positive False Positive True Negative False Negative   CBI pAUC
      m1.1            18              3             2              8 0.462  NaN
      m2.1            14              0             4              7 0.400  NaN
           Omission_10pct ROCSD TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      m1.1          0.100    NA    NA            NA            NA               NA
      m2.1          0.143    NA    NA            NA            NA               NA
           Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD Detection RateSD
      m1.1               NA          NA       NA   NA           NA               NA
      m2.1               NA          NA       NA   NA           NA               NA
           Detection PrevalenceSD Balanced AccuracySD AccuracySD KappaSD
      m1.1                     NA                  NA         NA      NA
      m2.1                     NA                  NA         NA      NA
           AccuracyLowerSD AccuracyUpperSD AccuracyNullSD AccuracyPValueSD
      m1.1              NA              NA             NA               NA
      m2.1              NA              NA             NA               NA
           McnemarPValueSD PositiveSD NegativeSD True PositiveSD False PositiveSD
      m1.1              NA         NA         NA              NA               NA
      m2.1              NA         NA         NA              NA               NA
           True NegativeSD False NegativeSD CBISD pAUCSD Omission_10pctSD
      m1.1              NA               NA    NA     NA               NA
      m2.1              NA               NA    NA     NA               NA
      
      $`Araucaria angustifolia`
                  algo       ROC       TSS Sensitivity Specificity Pos Pred Value
      m1.1 naive_bayes 0.8763975 0.4937888       0.994       0.500          0.970
      m2.1 naive_bayes 0.8222934 0.5106838       0.955       0.556          0.974
           Neg Pred Value Precision Recall    F1 Prevalence Detection Rate
      m1.1          0.833     0.970  0.994 0.982      0.942          0.936
      m2.1          0.417     0.974  0.955 0.964      0.945          0.903
           Detection Prevalence Balanced Accuracy Accuracy Kappa AccuracyLower
      m1.1                0.965             0.747    0.965 0.608         0.925
      m2.1                0.933             0.755    0.933 0.441         0.884
           AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1         0.987        0.942          0.326         0.724      161       10
      m2.1         0.966        0.945          0.882         0.773      156        9
           True Positive False Positive True Negative False Negative   CBI pAUC
      m1.1           160              3             5              5 0.304  NaN
      m2.1           149              7             5              5 0.495  NaN
           Omission_10pct ROCSD TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      m1.1          0.075    NA    NA            NA            NA               NA
      m2.1          0.083    NA    NA            NA            NA               NA
           Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD Detection RateSD
      m1.1               NA          NA       NA   NA           NA               NA
      m2.1               NA          NA       NA   NA           NA               NA
           Detection PrevalenceSD Balanced AccuracySD AccuracySD KappaSD
      m1.1                     NA                  NA         NA      NA
      m2.1                     NA                  NA         NA      NA
           AccuracyLowerSD AccuracyUpperSD AccuracyNullSD AccuracyPValueSD
      m1.1              NA              NA             NA               NA
      m2.1              NA              NA             NA               NA
           McnemarPValueSD PositiveSD NegativeSD True PositiveSD False PositiveSD
      m1.1              NA         NA         NA              NA               NA
      m2.1              NA         NA         NA              NA               NA
           True NegativeSD False NegativeSD CBISD pAUCSD Omission_10pctSD
      m1.1              NA               NA    NA     NA               NA
      m2.1              NA               NA    NA     NA               NA
      

---

    Code
      p6
    Output
               caretSDM        
      .........................
      Class             : Predictions
      Ensembles         :
               Methods  : mean_occ_prob wmean_AUC committee_avg 
      Thresholds        :
               Method   : threshold 
               Criteria : 0.5 
               Metrics  :
      $`Salminus brasiliensis`
                  algo       ROC        TSS Sensitivity Specificity Pos Pred Value
      m1.1 naive_bayes 0.5666667 0.07222222         0.9       0.222          0.708
      m2.1 naive_bayes 0.6269841 0.44444444         1.0       0.444          0.737
           Neg Pred Value Precision Recall    F1 Prevalence Detection Rate
      m1.1            0.4     0.708    0.9 0.783      0.690          0.621
      m2.1            1.0     0.737    1.0 0.848      0.609          0.609
           Detection Prevalence Balanced Accuracy Accuracy Kappa AccuracyLower
      m1.1                0.897             0.536    0.655 0.082         0.457
      m2.1                0.913             0.722    0.783 0.493         0.563
           AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1         0.821        0.690          0.732         0.343       20        9
      m2.1         0.925        0.609          0.264         0.074       14        9
           True Positive False Positive True Negative False Negative   CBI pAUC
      m1.1            18              3             2              8 0.462  NaN
      m2.1            14              0             4              7 0.400  NaN
           Omission_10pct ROCSD TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      m1.1          0.100    NA    NA            NA            NA               NA
      m2.1          0.143    NA    NA            NA            NA               NA
           Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD Detection RateSD
      m1.1               NA          NA       NA   NA           NA               NA
      m2.1               NA          NA       NA   NA           NA               NA
           Detection PrevalenceSD Balanced AccuracySD AccuracySD KappaSD
      m1.1                     NA                  NA         NA      NA
      m2.1                     NA                  NA         NA      NA
           AccuracyLowerSD AccuracyUpperSD AccuracyNullSD AccuracyPValueSD
      m1.1              NA              NA             NA               NA
      m2.1              NA              NA             NA               NA
           McnemarPValueSD PositiveSD NegativeSD True PositiveSD False PositiveSD
      m1.1              NA         NA         NA              NA               NA
      m2.1              NA         NA         NA              NA               NA
           True NegativeSD False NegativeSD CBISD pAUCSD Omission_10pctSD
      m1.1              NA               NA    NA     NA               NA
      m2.1              NA               NA    NA     NA               NA
      

---

    Code
      p7
    Output
               caretSDM        
      .........................
      Class             : Predictions
      Ensembles         :
               Methods  : mean_occ_prob wmean_AUC committee_avg 
      Thresholds        :
               Method   : threshold 
               Criteria : 0.6 
               Metrics  :
      $`Araucaria angustifolia`
                  algo       ROC       TSS Sensitivity Specificity Pos Pred Value
      m1.1 naive_bayes 0.8763975 0.4937888       0.994       0.500          0.970
      m2.1 naive_bayes 0.8222934 0.5106838       0.955       0.556          0.974
           Neg Pred Value Precision Recall    F1 Prevalence Detection Rate
      m1.1          0.833     0.970  0.994 0.982      0.942          0.936
      m2.1          0.417     0.974  0.955 0.964      0.945          0.903
           Detection Prevalence Balanced Accuracy Accuracy Kappa AccuracyLower
      m1.1                0.965             0.747    0.965 0.608         0.925
      m2.1                0.933             0.755    0.933 0.441         0.884
           AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1         0.987        0.942          0.326         0.724      161       10
      m2.1         0.966        0.945          0.882         0.773      156        9
           True Positive False Positive True Negative False Negative   CBI pAUC
      m1.1           160              3             5              5 0.304  NaN
      m2.1           149              7             5              5 0.495  NaN
           Omission_10pct ROCSD TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      m1.1          0.075    NA    NA            NA            NA               NA
      m2.1          0.083    NA    NA            NA            NA               NA
           Neg Pred ValueSD PrecisionSD RecallSD F1SD PrevalenceSD Detection RateSD
      m1.1               NA          NA       NA   NA           NA               NA
      m2.1               NA          NA       NA   NA           NA               NA
           Detection PrevalenceSD Balanced AccuracySD AccuracySD KappaSD
      m1.1                     NA                  NA         NA      NA
      m2.1                     NA                  NA         NA      NA
           AccuracyLowerSD AccuracyUpperSD AccuracyNullSD AccuracyPValueSD
      m1.1              NA              NA             NA               NA
      m2.1              NA              NA             NA               NA
           McnemarPValueSD PositiveSD NegativeSD True PositiveSD False PositiveSD
      m1.1              NA         NA         NA              NA               NA
      m2.1              NA         NA         NA              NA               NA
           True NegativeSD False NegativeSD CBISD pAUCSD Omission_10pctSD
      m1.1              NA               NA    NA     NA               NA
      m2.1              NA               NA    NA     NA               NA
      

