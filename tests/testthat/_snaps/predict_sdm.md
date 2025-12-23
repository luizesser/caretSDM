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
      1        kknn 0.5828637 0.9944179 0.03194444 0.09463642
      2 naive_bayes 0.8525695 0.9737374 0.41180556 0.04199922
      
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
                  algo       ROC      Sens       Spec       ROCSD       SensSD
      m1.2        kknn 0.6094911 0.9904306 0.03125000 0.124182775 1.353314e-02
      m2.2        kknn 0.5287031 0.9928230 0.06458333 0.003601790 1.014986e-02
      m3.2        kknn 0.6103968 1.0000000 0.00000000 0.156124688 1.683588e-02
      m1.1 naive_bayes 0.8583184 0.9665414 0.45625000 0.026537650 2.711462e-02
      m2.1 naive_bayes 0.8398397 0.9809068 0.38958333 0.001625186 8.055443e-05
      m3.1 naive_bayes 0.8595505 0.9737640 0.38958333 0.097834831 1.003708e-02
                SpecSD
      m1.2 0.044194174
      m2.2 0.002946278
      m3.2 0.000000000
      m1.1 0.203293200
      m2.1 0.150260191
      m3.1 0.109012295
      

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
      1        kknn 0.5828637 0.9944179 0.03194444 0.09463642
      2 naive_bayes 0.8525695 0.9737374 0.41180556 0.04199922
      
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
                  algo       ROC      Sens       Spec       ROCSD       SensSD
      m1.2        kknn 0.6094911 0.9904306 0.03125000 0.124182775 1.353314e-02
      m2.2        kknn 0.5287031 0.9928230 0.06458333 0.003601790 1.014986e-02
      m3.2        kknn 0.6103968 1.0000000 0.00000000 0.156124688 1.683588e-02
      m1.1 naive_bayes 0.8583184 0.9665414 0.45625000 0.026537650 2.711462e-02
      m2.1 naive_bayes 0.8398397 0.9809068 0.38958333 0.001625186 8.055443e-05
      m3.1 naive_bayes 0.8595505 0.9737640 0.38958333 0.097834831 1.003708e-02
                SpecSD
      m1.2 0.044194174
      m2.2 0.002946278
      m3.2 0.000000000
      m1.1 0.203293200
      m2.1 0.150260191
      m3.1 0.109012295
      

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
      1        kknn 0.5828637 0.9944179 0.03194444 0.09463642
      2 naive_bayes 0.8525695 0.9737374 0.41180556 0.04199922
      
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
                  algo       ROC      Sens      Spec       ROCSD       SensSD
      m1.1 naive_bayes 0.8583184 0.9665414 0.4562500 0.026537650 2.711462e-02
      m2.1 naive_bayes 0.8398397 0.9809068 0.3895833 0.001625186 8.055443e-05
      m3.1 naive_bayes 0.8595505 0.9737640 0.3895833 0.097834831 1.003708e-02
              SpecSD
      m1.1 0.2032932
      m2.1 0.1502602
      m3.1 0.1090123
      

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
      m1.1 naive_bayes 0.9004566 0.3264840       0.993       0.333          0.935
      m2.1 naive_bayes 0.8329032 0.4548387       0.955       0.500          0.967
           Neg Pred Value Precision Recall    F1 Prevalence Detection Rate
      m1.1          0.833     0.935  0.993 0.963      0.907          0.901
      m2.1          0.417     0.967  0.955 0.961      0.939          0.897
           Detection Prevalence Balanced Accuracy Accuracy Kappa AccuracyLower
      m1.1                0.963             0.663    0.932 0.447         0.881
      m2.1                0.927             0.727    0.927 0.416         0.876
           AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1         0.965        0.907          0.669         0.211      146       15
      m2.1         0.962        0.939          1.000         0.773      155       10
           True Positive False Positive True Negative False Negative   CBI pAUC
      m1.1           145              5             5             11 0.890  NaN
      m2.1           148             23             5              5 0.781  NaN
           Omission_10pct ROCSD TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      m1.1          0.075    NA    NA            NA            NA               NA
      m2.1          0.090    NA    NA            NA            NA               NA
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
      m1.1 naive_bayes 0.9004566 0.3264840       0.993       0.333          0.935
      m2.1 naive_bayes 0.8329032 0.4548387       0.955       0.500          0.967
           Neg Pred Value Precision Recall    F1 Prevalence Detection Rate
      m1.1          0.833     0.935  0.993 0.963      0.907          0.901
      m2.1          0.417     0.967  0.955 0.961      0.939          0.897
           Detection Prevalence Balanced Accuracy Accuracy Kappa AccuracyLower
      m1.1                0.963             0.663    0.932 0.447         0.881
      m2.1                0.927             0.727    0.927 0.416         0.876
           AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1         0.965        0.907          0.669         0.211      146       15
      m2.1         0.962        0.939          1.000         0.773      155       10
           True Positive False Positive True Negative False Negative   CBI pAUC
      m1.1           145              5             5             11 0.890  NaN
      m2.1           148             23             5              5 0.781  NaN
           Omission_10pct ROCSD TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      m1.1          0.075    NA    NA            NA            NA               NA
      m2.1          0.090    NA    NA            NA            NA               NA
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
      m1.1 naive_bayes 0.9004566 0.3264840       0.993       0.333          0.935
      m2.1 naive_bayes 0.8329032 0.4548387       0.955       0.500          0.967
           Neg Pred Value Precision Recall    F1 Prevalence Detection Rate
      m1.1          0.833     0.935  0.993 0.963      0.907          0.901
      m2.1          0.417     0.967  0.955 0.961      0.939          0.897
           Detection Prevalence Balanced Accuracy Accuracy Kappa AccuracyLower
      m1.1                0.963             0.663    0.932 0.447         0.881
      m2.1                0.927             0.727    0.927 0.416         0.876
           AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1         0.965        0.907          0.669         0.211      146       15
      m2.1         0.962        0.939          1.000         0.773      155       10
           True Positive False Positive True Negative False Negative   CBI pAUC
      m1.1           145              5             5             11 0.890  NaN
      m2.1           148             23             5              5 0.781  NaN
           Omission_10pct ROCSD TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD
      m1.1          0.075    NA    NA            NA            NA               NA
      m2.1          0.090    NA    NA            NA            NA               NA
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
      

