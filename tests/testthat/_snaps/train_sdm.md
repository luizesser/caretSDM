# train_sdm

    Code
      i2
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
      Algorithms Names              : kknn naive_bayes 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : cv 
          Number                    : 2 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC      Sens      Spec      ROCSD
      1        kknn 0.6535438 0.9840662 0.1715278 0.11123526
      2 naive_bayes 0.8715896 0.9689679 0.4506944 0.03534877
      

# train_sdm - pca

    Code
      i2
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
      Number of Predictors          : 4 
      Predictors Names              : bio1, bio12, PC1, PC2 
      PCA-transformed variables     : DONE 
      Cummulative proportion ( 0.99 ) : PC1 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : kknn naive_bayes 
      Variables Names               : PC1 
      Model Validation              :
          Method                    : cv 
          Number                    : 2 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC      Sens      Spec      ROCSD
      1        kknn 0.5823967 0.9832954 0.1284722 0.05134120
      2 naive_bayes 0.8619322 0.9968254 0.1395833 0.07221789
      

# train_sdm - vif

    Code
      i2
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
      Area (VIF)                    : all
      Threshold                     : 0.5
      Selected Variables (VIF)      : bio1, bio12 
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
      1        kknn 0.6096715 0.9841042 0.09583333 0.12433514
      2 naive_bayes 0.8656633 0.9721805 0.41666667 0.07424423
      

# train_sdm - change ctrl

    Code
      i2
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
      Number of Predictors          : 4 
      Predictors Names              : bio1, bio12, PC1, PC2 
      PCA-transformed variables     : DONE 
      Cummulative proportion ( 0.99 ) : PC1 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : kknn naive_bayes 
      Variables Names               : PC1 
      Model Validation              :
          Method                    : boot 
          Number                    : 10 
          Metrics                   :
      $`Araucaria angustifolia`
               algo      ROC      Sens      Spec      ROCSD
      1        kknn 0.554307 0.9888971 0.0696780 0.06237039
      2 naive_bayes 0.845411 0.9972601 0.1927016 0.11454939
      

# train_sdm - selecting vars

    Code
      i2
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
               algo       ROC      Sens       Spec     ROCSD
      1        kknn 0.6237304 0.9952267 0.05347222 0.0814423
      2 naive_bayes 0.8647437 0.9697767 0.45277778 0.0273860
      

# train_sdm - ESM

    Code
      i2
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
      Ensemble of Small Models (ESM): TRUE
          Number of Records         : 20 
      --------  Predictors  ---------
      Number of Predictors          : 3 
      Predictors Names              : bio1, bio4, bio12 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes kknn 
      Variables Names               : bio1 bio4 bio12 
      Model Validation              :
          Method                    : cv 
          Number                    : 2 
          Metrics                   :
      $`Araucaria angustifolia`
               algo      ROC      Sens       Spec      ROCSD
      1        kknn 0.633690 0.9907104 0.09583333 0.10103510
      2 naive_bayes 0.861077 0.9785170 0.42500000 0.06547645
      

# mahal.dist train

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 419 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
          Number of PA sets         : 3 
          Number of PAs in each set : 419 
      --------  Predictors  ---------
      Number of Predictors          : 7 
      Predictors Names              : GID0, CODIGOIB1, NOMEUF2, SIGLAUF3, bio1, bio4, bio12 
      -----------  Models  ----------
      Algorithms Names              : mahal.custom 
      Variables Names               : bio1 bio4 bio12 
      Model Validation              :
          Method                    : cv 
          Number                    : 3 
          Metrics                   :
      $`Araucaria angustifolia`
                algo       ROC       TSS Sensitivity Specificity
      1 mahal.custom 0.9880458 0.8154277   0.8154444           1
      

# train_sdm - independent data

    Code
      i1
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Salminus brasiliensis Araucaria angustifolia 
      Number of presences           : 79 19 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
          Number of PA sets         : 10 
          Number of PAs in each set : 79 19 
      Independent Test              : TRUE (number of records =  27 )
      Data Cleaning                 : NAs, Capitals, Centroids, Geographically Duplicated, Identical Lat/Long, Institutions, Invalid, Non-terrestrial, Duplicated Cell (grid), Methods also applied in independent_test 
      --------  Predictors  ---------
      Number of Predictors          : 3 
      Predictors Names              : bio1, bio4, bio12 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : kknn 
      Variables Names               : bio1 bio4 bio12 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Salminus brasiliensis`
        algo       ROC    TSS Sensitivity Specificity
      1 kknn 0.9055625 0.7725        0.83      0.9525
      
      $`Araucaria angustifolia`
        algo       ROC       TSS Sensitivity Specificity
      1 kknn 0.9567862 0.8862061      0.9695      0.9166
      
      Independent Validation        :
          ROC (mean +- sd)            :  NA  +-  NA 

---

    Code
      i1$models
    Output
               caretSDM        
      .........................
      Class                   : Models
      Algorithms Names        : kknn 
      Variables Names         : bio1 bio4 bio12 
      Model Validation        :
               Method          : repeatedcv 
               Number          : 4 
               Metrics         :
      $`Salminus brasiliensis`
            algo      ROC    TSS Sensitivity Specificity Pos Pred Value
      m1.1  kknn 0.925000 0.8000      0.8000      1.0000        1.00000
      m2.1  kknn 0.962500 0.8875      0.8875      1.0000        1.00000
      m3.1  kknn 0.877500 0.7250      0.8375      0.9375        0.95825
      m4.1  kknn 0.875000 0.7500      0.8000      0.9500        0.95000
      m5.1  kknn 0.893750 0.7875      0.7875      1.0000        1.00000
      m6.1  kknn 0.890000 0.7000      0.8500      0.8500        0.85000
      m7.1  kknn 0.930000 0.8000      0.9000      0.9500        0.95000
      m8.1  kknn 0.923750 0.7875      0.8500      0.9375        0.95000
      m9.1  kknn 0.925000 0.7500      0.7500      1.0000        1.00000
      m10.1 kknn 0.853125 0.7375      0.8375      0.9000        0.91675
            Neg Pred Value Precision Recall      F1 Prevalence Detection Rate
      m1.1         0.83325   1.00000 0.8000 0.88200        0.5        0.39425
      m2.1         0.91650   1.00000 0.8875 0.93650        0.5        0.44725
      m3.1         0.88675   0.95825 0.8375 0.85625        0.5        0.42225
      m4.1         0.82850   0.95000 0.8000 0.85975        0.5        0.39700
      m5.1         0.83675   1.00000 0.7875 0.87400        0.5        0.39375
      m6.1         0.85825   0.85000 0.8500 0.84450        0.5        0.42500
      m7.1         0.90825   0.95000 0.9000 0.88200        0.5        0.45000
      m8.1         0.87475   0.95000 0.8500 0.88900        0.5        0.42500
      m9.1         0.80350   1.00000 0.7500 0.84725        0.5        0.36925
      m10.1        0.84575   0.91675 0.8375 0.86825        0.5        0.41875
            Detection Prevalence Balanced Accuracy Accuracy   Kappa AccuracyLower
      m1.1               0.39425           0.90000  0.89450 0.79275       0.54350
      m2.1               0.44725           0.94375  0.94725 0.89225       0.60725
      m3.1               0.47500           0.86250  0.86950 0.73450       0.50875
      m4.1               0.42200           0.87500  0.87225 0.74500       0.51750
      m5.1               0.39375           0.89375  0.89375 0.78750       0.54100
      m6.1               0.50300           0.85000  0.84725 0.69500       0.49975
      m7.1               0.53075           0.90000  0.89725 0.79500       0.54525
      m8.1               0.45625           0.89375  0.89375 0.78750       0.53450
      m9.1               0.36925           0.87500  0.86950 0.74275       0.51575
      m10.1              0.46875           0.86875  0.86875 0.73750       0.51700
            AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1        0.99150        0.528        0.04600     0.8266667     4.75     4.75
      m2.1        0.99850        0.528        0.02350     1.0000000     4.75     4.75
      m3.1        0.99150        0.528        0.03700     0.8700000     4.75     4.75
      m4.1        0.98675        0.528        0.03900     0.8266667     4.75     4.75
      m5.1        0.99225        0.500        0.02550     0.8266667     4.75     4.75
      m6.1        0.96800        0.528        0.10850     1.0000000     4.75     4.75
      m7.1        0.99225        0.528        0.03700     1.0000000     4.75     4.75
      m8.1        0.99700        0.500        0.01700     1.0000000     4.75     4.75
      m9.1        0.98600        0.528        0.05700     0.6533333     4.75     4.75
      m10.1       0.98175        0.500        0.05475     1.0000000     4.75     4.75
            True Positive False Positive True Negative False Negative    CBI pAUC
      m1.1           3.75           1.00          4.75           0.00  1.000  NaN
      m2.1           4.25           0.75          4.75           0.00  1.000  NaN
      m3.1           4.00           1.00          4.50           0.50    NaN  NaN
      m4.1           3.75           1.00          4.50           0.25    NaN  NaN
      m5.1           3.75           1.00          4.75           0.00    NaN  NaN
      m6.1           4.00           0.75          4.00           0.75 -0.211  NaN
      m7.1           4.25           0.75          4.50           0.75    NaN  NaN
      m8.1           4.00           0.75          4.50           0.25  1.000  NaN
      m9.1           3.50           1.25          4.75           0.00  1.000  NaN
      m10.1          4.00           0.75          4.25           0.50 -1.000  NaN
            Omission_10pct      ROCSD      TSSSD SensitivitySD SpecificitySD
      m1.1          0.1000 0.09574271 0.16329932     0.1632993     0.0000000
      m2.1          0.2125 0.06574889 0.13149778     0.1314978     0.0000000
      m3.1          0.1125 0.06538348 0.08660254     0.1973787     0.1314978
      m4.1          0.1625 0.09574271 0.19148542     0.1632993     0.1000000
      m5.1          0.1125 0.08260095 0.16520190     0.1652019     0.0000000
      m6.1          0.1000 0.09865766 0.34641016     0.1914854     0.1914854
      m7.1          0.1500 0.08164966 0.16329932     0.1914854     0.1108678
      m8.1          0.1500 0.05543389 0.02500000     0.1000000     0.1250000
      m9.1          0.0500 0.09574271 0.19148542     0.1914854     0.0000000
      m10.1         0.1625 0.12762861 0.24958299     0.1108678     0.2000000
            Pos Pred ValueSD Neg Pred ValueSD PrecisionSD  RecallSD       F1SD
      m1.1        0.00000000       0.13594699  0.00000000 0.1632993 0.10238164
      m2.1        0.00000000       0.09641749  0.00000000 0.1314978 0.07447818
      m3.1        0.09641749       0.13950239  0.09641749 0.1973787 0.07495499
      m4.1        0.10000000       0.12130815  0.10000000 0.1632993 0.10975845
      m5.1        0.00000000       0.11983704  0.00000000 0.1652019 0.10290125
      m6.1        0.19148542       0.18931169  0.19148542 0.1914854 0.17119287
      m7.1        0.10000000       0.13950239  0.10000000 0.1914854 0.10238164
      m8.1        0.10000000       0.08350000  0.10000000 0.1000000 0.00000000
      m9.1        0.00000000       0.14846436  0.00000000 0.1914854 0.12109328
      m10.1       0.16650000       0.10834628  0.16650000 0.1108678 0.11234879
            PrevalenceSD Detection RateSD Detection PrevalenceSD Balanced AccuracySD
      m1.1    0.04572381       0.04579938             0.04579938          0.08164966
      m2.1    0.04572381       0.09982443             0.09982443          0.06574889
      m3.1    0.04572381       0.12495966             0.18554245          0.04330127
      m4.1    0.04572381       0.06791171             0.08551023          0.09574271
      m5.1    0.00000000       0.08260095             0.08260095          0.08260095
      m6.1    0.04572381       0.10610058             0.07356630          0.17320508
      m7.1    0.04572381       0.10610058             0.12574578          0.08164966
      m8.1    0.00000000       0.05000000             0.11250000          0.01250000
      m9.1    0.04572381       0.06491726             0.06491726          0.09574271
      m10.1   0.00000000       0.05543389             0.10282469          0.12479149
            AccuracySD    KappaSD AccuracyLowerSD AccuracyUpperSD AccuracyNullSD
      m1.1  0.09085336 0.17533848      0.10859251     0.013076697     0.03233162
      m2.1  0.06107577 0.12506099      0.08386249     0.001732051     0.03233162
      m3.1  0.04662260 0.09084969      0.04655731     0.011000000     0.03233162
      m4.1  0.09493989 0.19000000      0.10370953     0.013622897     0.03233162
      m5.1  0.08260095 0.16520190      0.11110056     0.011586630     0.00000000
      m6.1  0.17223119 0.34462540      0.17018495     0.060016664     0.03233162
      m7.1  0.08183469 0.16360522      0.09163469     0.011586630     0.03233162
      m8.1  0.01250000 0.02500000      0.04100000     0.000000000     0.00000000
      m9.1  0.10191990 0.19944151      0.11841277     0.014537308     0.03233162
      m10.1 0.12479149 0.24958299      0.14441376     0.032530755     0.00000000
            AccuracyPValueSD McnemarPValueSD PositiveSD NegativeSD True PositiveSD
      m1.1        0.07405403       0.3002221        0.5        0.5       0.5000000
      m2.1        0.02061553       0.0000000        0.5        0.5       0.9574271
      m3.1        0.01854724       0.2600000        0.5        0.5       1.1547005
      m4.1        0.02360791       0.3002221        0.5        0.5       0.5000000
      m5.1        0.02429678       0.3002221        0.5        0.5       0.9574271
      m6.1        0.17969140       0.0000000        0.5        0.5       0.8164966
      m7.1        0.02391652       0.3002221        0.5        0.5       0.8164966
      m8.1        0.01200000       0.0000000        0.5        0.5       0.0000000
      m9.1        0.07029462       0.3002221        0.5        0.5       0.5773503
      m10.1       0.07945806       0.0000000        0.5        0.5       0.8164966
            False PositiveSD True NegativeSD False NegativeSD     CBISD pAUCSD
      m1.1         0.8164966       0.5000000        0.0000000        NA     NA
      m2.1         0.5773503       0.5000000        0.0000000 0.3949076     NA
      m3.1         0.9574271       1.0000000        0.5773503        NA     NA
      m4.1         0.8164966       0.5773503        0.5000000        NA     NA
      m5.1         0.8164966       0.5000000        0.0000000        NA     NA
      m6.1         0.9574271       0.8164966        0.9574271        NA     NA
      m7.1         0.9574271       0.8164966        0.5000000        NA     NA
      m8.1         0.5000000       1.0000000        0.5000000        NA     NA
      m9.1         0.9574271       0.5000000        0.0000000        NA     NA
      m10.1        0.5000000       0.9574271        1.0000000        NA     NA
            Omission_10pctSD
      m1.1         0.1154701
      m2.1         0.0250000
      m3.1         0.1314978
      m4.1         0.1154701
      m5.1         0.1314978
      m6.1         0.1154701
      m7.1         0.1000000
      m8.1         0.1000000
      m9.1         0.1000000
      m10.1        0.1108678
      
      $`Araucaria angustifolia`
            algo       ROC       TSS Sensitivity Specificity Pos Pred Value
      m1.1  kknn 0.9330044 0.8660088     0.94925     0.91650        0.97425
      m2.1  kknn 0.9520833 0.9041667     0.98750     0.91675        0.97625
      m3.1  kknn 0.9583333 0.8333333     1.00000     0.83350        0.95350
      m4.1  kknn 0.9520833 0.8666667     0.95000     0.91650        0.97550
      m5.1  kknn 0.9594298 0.9188596     0.96050     0.95825        0.98800
      m6.1  kknn 0.9729167 0.9458333     0.98750     0.95825        0.98800
      m7.1  kknn 0.9758772 0.9451754     0.98675     0.95825        0.98675
      m8.1  kknn 0.9533333 0.7958333     0.96250     0.83325        0.95225
      m9.1  kknn 0.9590461 0.8951754     0.93675     0.95825        0.98675
      m10.1 kknn 0.9517544 0.8910088     0.97425     0.91650        0.97475
            Neg Pred Value Precision  Recall      F1 Prevalence Detection Rate
      m1.1         0.85400   0.97425 0.94925 0.96100    0.76675        0.72800
      m2.1         0.95000   0.97625 0.98750 0.98175    0.75975        0.75025
      m3.1         1.00000   0.95350 1.00000 0.97550    0.77450        0.77450
      m4.1         0.87500   0.97550 0.95000 0.96125    0.75950        0.72125
      m5.1         0.91675   0.98800 0.96050 0.97250    0.76675        0.73675
      m6.1         0.96425   0.98800 0.98750 0.98750    0.76675        0.75725
      m7.1         0.95825   0.98675 0.98675 0.98675    0.75975        0.74975
      m8.1         0.90625   0.95225 0.96250 0.95525    0.77450        0.74450
      m9.1         0.85825   0.98675 0.93675 0.95900    0.76675        0.71825
      m10.1        0.92250   0.97475 0.97425 0.97425    0.77450        0.75500
            Detection Prevalence Balanced Accuracy Accuracy   Kappa AccuracyLower
      m1.1               0.74750            0.9330  0.94150 0.84250       0.77650
      m2.1               0.76950            0.9520  0.97125 0.91375       0.82525
      m3.1               0.81375            0.9165  0.96075 0.87675       0.80500
      m4.1               0.74050            0.9335  0.94250 0.84775       0.77975
      m5.1               0.74650            0.9595  0.96050 0.90100       0.80700
      m6.1               0.76700            0.9730  0.98100 0.94575       0.83475
      m7.1               0.76950            0.9725  0.98000 0.94525       0.83700
      m8.1               0.79400            0.8980  0.93100 0.80400       0.75900
      m9.1               0.72825            0.9475  0.94150 0.85750       0.78175
      m10.1              0.77475            0.9455  0.96100 0.89100       0.80275
            AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1        0.99300      0.76675        0.03100     0.8266667    19.75     6.00
      m2.1        0.99400      0.75975        0.03400     1.0000000    19.75     6.25
      m3.1        0.99525      0.77450        0.02175     0.4800000    19.75     5.75
      m4.1        0.99150      0.75950        0.04225     0.7493333    19.75     6.25
      m5.1        0.99350      0.76675        0.03225     0.6240000    19.75     6.00
      m6.1        0.99950      0.76675        0.00550     1.0000000    19.75     6.00
      m7.1        0.99750      0.75975        0.01300     1.0000000    19.75     6.25
      m8.1        0.99075      0.77450        0.09275     0.6820000    19.75     5.75
      m9.1        0.98650      0.76675        0.07325     0.5670000    19.75     6.00
      m10.1       0.99700      0.77450        0.01625     1.0000000    19.75     5.75
            True Positive False Positive True Negative False Negative    CBI pAUC
      m1.1          18.75           1.00          5.50           0.50 1.0000  NaN
      m2.1          19.50           0.75          5.75           0.50 1.0000  NaN
      m3.1          19.75           0.00          4.75           1.00 1.0000  NaN
      m4.1          18.75           1.00          5.75           0.50 1.0000  NaN
      m5.1          19.00           0.75          5.75           0.25 1.0000  NaN
      m6.1          19.50           0.25          5.75           0.25    NaN  NaN
      m7.1          19.50           0.25          6.00           0.50 1.0000  NaN
      m8.1          19.00           0.75          4.75           1.25 0.6055  NaN
      m9.1          18.50           1.25          5.75           0.25 0.2110  NaN
      m10.1         19.25           0.50          5.25           0.50 1.0000  NaN
            Omission_10pct      ROCSD      TSSSD SensitivitySD SpecificitySD
      m1.1         0.06325 0.05261620 0.10523239    0.04085238    0.09641749
      m2.1         0.06375 0.09583333 0.19166667    0.04787136    0.16650000
      m3.1         0.03750 0.09622504 0.19245009    0.00000000    0.19225764
      m4.1         0.06375 0.05626286 0.09329364    0.07071068    0.09641749
      m5.1         0.02500 0.04688061 0.09376122    0.07900000    0.08350000
      m6.1         0.03875 0.03930825 0.07861651    0.02500000    0.08350000
      m7.1         0.05125 0.04824561 0.11347139    0.02650000    0.09641749
      m8.1         0.03875 0.04311913 0.16962650    0.07500000    0.20967356
      m9.1         0.03825 0.05243426 0.12129711    0.09452821    0.08350000
      m10.1        0.06325 0.05673570 0.10141194    0.02975875    0.09641749
            Pos Pred ValueSD Neg Pred ValueSD PrecisionSD   RecallSD       F1SD
      m1.1        0.02975875       0.10490313  0.02975875 0.04085238 0.02603843
      m2.1        0.04750000       0.12215155  0.04750000 0.04787136 0.03727823
      m3.1        0.05371840       0.00000000  0.05371840 0.00000000 0.02830194
      m4.1        0.02830194       0.15945532  0.02830194 0.07071068 0.03478865
      m5.1        0.02400000       0.16650000  0.02400000 0.07900000 0.04060788
      m6.1        0.02400000       0.07150000  0.02400000 0.02500000 0.01445683
      m7.1        0.02922756       0.08350000  0.02922756 0.02650000 0.02650000
      m8.1        0.05670979       0.18750000  0.05670979 0.07500000 0.03077337
      m9.1        0.02650000       0.18931169  0.02650000 0.09452821 0.05293392
      m10.1       0.02922756       0.09002407  0.02922756 0.02975875 0.02166987
            PrevalenceSD Detection RateSD Detection PrevalenceSD Balanced AccuracySD
      m1.1    0.00450000       0.03188521             0.03724245          0.05262446
      m2.1    0.01320038       0.04611128             0.05974948          0.09600000
      m3.1    0.01752142       0.01752142             0.03615130          0.09641749
      m4.1    0.01900000       0.04827957             0.06567851          0.04650806
      m5.1    0.00450000       0.06450000             0.07334167          0.04679387
      m6.1    0.00450000       0.01800694             0.03178050          0.03914929
      m7.1    0.01320038       0.02382401             0.02819574          0.05679422
      m8.1    0.01752142       0.04320880             0.08323461          0.08491712
      m9.1    0.00450000       0.07260567             0.07561911          0.06075909
      m10.1   0.01752142       0.03661512             0.03593861          0.05077073
            AccuracySD    KappaSD AccuracyLowerSD AccuracyUpperSD AccuracyNullSD
      m1.1  0.03902563 0.10561092      0.06114736    0.0046904158     0.00450000
      m2.1  0.05750000 0.17250000      0.08491319    0.0120000000     0.01320038
      m3.1  0.04533854 0.14232211      0.06998571    0.0055000000     0.01752142
      m4.1  0.04957486 0.12154937      0.07304051    0.0110905365     0.01900000
      m5.1  0.05657738 0.13289344      0.08487638    0.0123423391     0.00450000
      m6.1  0.02193931 0.06286692      0.03556567    0.0005773503     0.00450000
      m7.1  0.04000000 0.10950000      0.06469415    0.0050000000     0.01320038
      m8.1  0.04734272 0.14150707      0.06697263    0.0138564065     0.01752142
      m9.1  0.07399775 0.17314445      0.10601376    0.0208726296     0.00450000
      m10.1 0.03268027 0.08956562      0.05023528    0.0046904158     0.01752142
            AccuracyPValueSD McnemarPValueSD PositiveSD NegativeSD True PositiveSD
      m1.1       0.020000000       0.3002221        0.5        0.0       0.9574271
      m2.1       0.058668561              NA        0.5        0.5       0.8164966
      m3.1       0.022261701       0.0000000        0.5        0.5       0.5000000
      m4.1       0.053431420       0.4341674        0.5        0.5       1.2583057
      m5.1       0.056659068       0.5317443        0.5        0.0       2.0000000
      m6.1       0.005196152       0.0000000        0.5        0.0       0.5773503
      m7.1       0.020172176              NA        0.5        0.5       1.0000000
      m8.1       0.107828181       0.4341674        0.5        0.5       1.4142136
      m9.1       0.119332519       0.6123545        0.5        0.0       1.9148542
      m10.1      0.016740669       0.0000000        0.5        0.5       0.9574271
            False PositiveSD True NegativeSD False NegativeSD     CBISD pAUCSD
      m1.1         0.8164966       0.5773503        0.5773503        NA     NA
      m2.1         0.9574271       1.2583057        1.0000000        NA     NA
      m3.1         0.0000000       0.9574271        1.1547005        NA     NA
      m4.1         1.4142136       0.9574271        0.5773503        NA     NA
      m5.1         1.5000000       0.5000000        0.5000000        NA     NA
      m6.1         0.5000000       0.5000000        0.5000000        NA     NA
      m7.1         0.5000000       0.9574271        0.5773503 0.0000000     NA
      m8.1         1.5000000       1.0000000        1.2583057 0.5579073     NA
      m9.1         1.8929694       0.5000000        0.5000000        NA     NA
      m10.1        0.5773503       0.5000000        0.5773503        NA     NA
            Omission_10pctSD
      m1.1        0.02454078
      m2.1        0.04922313
      m3.1        0.04787136
      m4.1        0.04922313
      m5.1        0.05000000
      m6.1        0.05006246
      m7.1        0.05921360
      m8.1        0.05006246
      m9.1        0.04815513
      m10.1       0.04763315
      

