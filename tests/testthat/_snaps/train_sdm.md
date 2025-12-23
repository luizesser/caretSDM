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
      1        kknn 0.6098982 0.9896674 0.1368056 0.09384373
      2 naive_bayes 0.8568873 0.9729627 0.3534722 0.09172166
      

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
               algo       ROC      Sens       Spec      ROCSD
      1        kknn 0.5741124 0.9841232 0.09861111 0.07773838
      2 naive_bayes 0.8166048 0.9968254 0.20416667 0.12142982
      

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
               algo       ROC      Sens      Spec      ROCSD
      1        kknn 0.6384010 0.9832840 0.1069444 0.07948238
      2 naive_bayes 0.8622556 0.9705931 0.3972222 0.03370339
      

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
      1        kknn 0.589734 0.9900095 0.1042964 0.07118376
      2 naive_bayes 0.860746 0.9954639 0.1670393 0.09747757
      

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
               algo       ROC      Sens       Spec      ROCSD
      1        kknn 0.6136269 0.9920521 0.09513889 0.14934461
      2 naive_bayes 0.8592828 0.9681856 0.43055556 0.03952856
      

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
               algo       ROC      Sens       Spec      ROCSD
      1        kknn 0.6387549 0.9909762 0.08310185 0.08653450
      2 naive_bayes 0.8500438 0.9779892 0.43518519 0.06371352
      

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
      1 mahal.custom 0.9910671 0.7947642   0.8225556           1
      

# train_sdm - independent data

    Code
      i1
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Salminus brasiliensis Araucaria angustifolia 
      Number of presences           : 77 21 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
          Number of PA sets         : 10 
          Number of PAs in each set : 77 21 
      Independent Test              : TRUE (number of records =  28 )
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
        algo       ROC       TSS Sensitivity Specificity
      1 kknn 0.9119167 0.7941667    0.859125    0.934975
      
      $`Araucaria angustifolia`
        algo       ROC       TSS Sensitivity Specificity
      1 kknn 0.9417066 0.8426378     0.97135      0.8711
      
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
            algo       ROC       TSS Sensitivity Specificity Pos Pred Value
      m1.1  kknn 0.9241667 0.8083333     0.85000     0.95825        0.95000
      m2.1  kknn 0.9391667 0.8083333     0.85825     0.95000        0.95000
      m3.1  kknn 0.8083333 0.5750000     0.70825     0.86675        0.70825
      m4.1  kknn 0.8841667 0.7083333     0.80825     0.90000        0.92850
      m5.1  kknn 0.9200000 0.8000000     0.80000     1.00000        1.00000
      m6.1  kknn 0.9350000 0.9000000     0.95000     0.95000        0.95825
      m7.1  kknn 0.8541667 0.7083333     0.85000     0.85825        0.86425
      m8.1  kknn 0.9583333 0.9166667     0.95825     0.95825        0.95825
      m9.1  kknn 0.9291667 0.8583333     0.90000     0.95825        0.95000
      m10.1 kknn 0.9666667 0.8583333     0.90825     0.95000        0.95825
            Neg Pred Value Precision  Recall        F1 Prevalence Detection Rate
      m1.1         0.87475   0.95000 0.85000 0.8945000        0.5        0.42725
      m2.1         0.87250   0.95000 0.85825 0.8995000        0.5        0.42925
      m3.1         0.81100   0.70825 0.70825 0.9443333        0.5        0.36375
      m4.1         0.85100   0.92850 0.80825 0.8452500        0.5        0.40425
      m5.1         0.87500   1.00000 0.80000 0.8650000        0.5        0.40675
      m6.1         0.95825   0.95825 0.95000 0.9495000        0.5        0.47500
      m7.1         0.85825   0.86425 0.85000 0.8530000        0.5        0.42500
      m8.1         0.95825   0.95825 0.95825 0.9545000        0.5        0.47750
      m9.1         0.91650   0.95000 0.90000 0.9222500        0.5        0.45225
      m10.1        0.91425   0.95825 0.90825 0.9305000        0.5        0.45475
            Detection Prevalence Balanced Accuracy Accuracy   Kappa AccuracyLower
      m1.1               0.45000           0.90425  0.90450 0.80825       0.57675
      m2.1               0.45425           0.90425  0.90425 0.80825       0.57650
      m3.1               0.43175           0.78750  0.79550 0.57050       0.49375
      m4.1               0.47925           0.85425  0.85425 0.70825       0.51450
      m5.1               0.40675           0.90000  0.90675 0.80525       0.58800
      m6.1               0.50000           0.95000  0.95000 0.90000       0.63500
      m7.1               0.49575           0.85425  0.85425 0.70825       0.51450
      m8.1               0.50000           0.95850  0.95450 0.91000       0.63950
      m9.1               0.50000           0.92925  0.92950 0.85825       0.61100
      m10.1              0.47725           0.92925  0.93175 0.86175       0.61325
            AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1        0.99275       0.5225        0.02100         1.000     5.25     5.25
      m2.1        0.99250       0.5000        0.02000         1.000     5.25     5.25
      m3.1        0.91725       0.5225        0.24950         0.725     5.25     5.25
      m4.1        0.98625       0.5000        0.06025         0.870     5.25     5.25
      m5.1        0.98425       0.5225        0.04900         0.624     5.25     5.25
      m6.1        0.99850       0.5225        0.00600         1.000     5.25     5.25
      m7.1        0.98625       0.5000        0.03100         1.000     5.25     5.25
      m8.1        0.99900       0.5225        0.00700         1.000     5.25     5.25
      m9.1        0.99350       0.5225        0.02100         1.000     5.25     5.25
      m10.1       0.99375       0.5225        0.01900         1.000     5.25     5.25
            True Positive False Positive True Negative False Negative   CBI  pAUC
      m1.1           4.50           0.75          5.00           0.25   NaN   NaN
      m2.1           4.50           1.00          5.00           0.25 0.731   NaN
      m3.1           3.75           1.50          4.50           0.75 0.750 0.505
      m4.1           4.25           1.00          4.75           0.75   NaN   NaN
      m5.1           4.25           1.00          5.25           0.00 1.000   NaN
      m6.1           5.00           0.25          5.00           0.25 0.500   NaN
      m7.1           4.50           0.75          4.50           0.75   NaN   NaN
      m8.1           5.00           0.25          5.00           0.25   NaN   NaN
      m9.1           4.75           0.50          5.00           0.50 1.000   NaN
      m10.1          4.75           0.50          5.00           0.25 0.447   NaN
            Omission_10pct      ROCSD      TSSSD SensitivitySD SpecificitySD
      m1.1         0.15000 0.08368260 0.15000000    0.10000000    0.08350000
      m2.1         0.19175 0.04374802 0.16414763    0.09577186    0.10000000
      m3.1         0.09175 0.27504208 0.62974128    0.47868457    0.16316326
      m4.1         0.14175 0.06291529 0.20069324    0.16413079    0.20000000
      m5.1         0.05000 0.14696938 0.28284271    0.28284271    0.00000000
      m6.1         0.05000 0.07895146 0.11547005    0.10000000    0.10000000
      m7.1         0.15000 0.06291529 0.12583057    0.10000000    0.09577186
      m8.1         0.04175 0.04811252 0.09622504    0.08350000    0.08350000
      m9.1         0.19175 0.08858455 0.17716910    0.11547005    0.10679693
      m10.1        0.09175 0.04714045 0.17716910    0.10679693    0.10000000
            Pos Pred ValueSD Neg Pred ValueSD PrecisionSD   RecallSD       F1SD
      m1.1        0.10000000       0.08350000  0.10000000 0.10000000 0.08189628
      m2.1        0.10000000       0.08815328  0.10000000 0.09577186 0.08205892
      m3.1        0.47868457       0.26220348  0.47868457 0.47868457 0.09641749
      m4.1        0.15542630       0.13646092  0.15542630 0.16413079 0.10981651
      m5.1        0.00000000       0.15945532  0.00000000 0.28284271 0.20286449
      m6.1        0.08350000       0.08350000  0.08350000 0.10000000 0.05888124
      m7.1        0.09440471       0.09577186  0.09440471 0.10000000 0.06275349
      m8.1        0.08350000       0.08350000  0.08350000 0.08350000 0.05253887
      m9.1        0.10679693       0.09641749  0.10679693 0.11547005 0.09685169
      m10.1       0.08350000       0.10171324  0.08350000 0.10679693 0.08344459
            PrevalenceSD Detection RateSD Detection PrevalenceSD Balanced AccuracySD
      m1.1    0.03674235       0.08031345             0.06843488          0.07487044
      m2.1    0.00000000       0.04784262             0.05328149          0.08209090
      m3.1    0.03674235       0.24342607             0.16784591          0.31504127
      m4.1    0.00000000       0.08209090             0.17177189          0.10044360
      m5.1    0.03674235       0.16162379             0.16162379          0.14142136
      m6.1    0.03674235       0.06204837             0.08953584          0.05773503
      m7.1    0.00000000       0.05000000             0.07487044          0.06302579
      m8.1    0.03674235       0.02598076             0.03674235          0.04792007
      m9.1    0.03674235       0.08446054             0.08953584          0.08844348
      m10.1   0.03674235       0.06411123             0.07842353          0.08844348
            AccuracySD   KappaSD AccuracyLowerSD AccuracyUpperSD AccuracyNullSD
      m1.1  0.07448266 0.1501297      0.09838149     0.010594810     0.02598076
      m2.1  0.08209090 0.1641308      0.10462791     0.011733144     0.00000000
      m3.1  0.30018827 0.6382291      0.27494045     0.150557575     0.02598076
      m4.1  0.10044360 0.2005964      0.11850316     0.030412443     0.00000000
      m5.1  0.12877209 0.2729657      0.14970861     0.029533879     0.02598076
      m6.1  0.05773503 0.1154701      0.09237604     0.001732051     0.02598076
      m7.1  0.06302579 0.1257203      0.08501176     0.012996794     0.00000000
      m8.1  0.05253887 0.1039230      0.06062178     0.001154701     0.02598076
      m9.1  0.08802083 0.1773102      0.11128642     0.011090537     0.02598076
      m10.1 0.08712587 0.1759098      0.10052985     0.011206397     0.02598076
            AccuracyPValueSD McnemarPValueSD PositiveSD NegativeSD True PositiveSD
      m1.1       0.027080128       0.0000000        0.5        0.5       1.0000000
      m2.1       0.025370587       0.0000000        0.5        0.5       0.5773503
      m3.1       0.457874437       0.3889087        0.5        0.5       2.5000000
      m4.1       0.077928921       0.3002221        0.5        0.5       0.9574271
      m5.1       0.089457625       0.5317443        0.5        0.5       1.7078251
      m6.1       0.005773503       0.0000000        0.5        0.5       0.8164966
      m7.1       0.027904599       0.0000000        0.5        0.5       1.0000000
      m8.1       0.006928203       0.0000000        0.5        0.5       0.0000000
      m9.1       0.028722813       0.0000000        0.5        0.5       0.9574271
      m10.1      0.028565714       0.0000000        0.5        0.5       0.5000000
            False PositiveSD True NegativeSD False NegativeSD     CBISD pAUCSD
      m1.1         0.5000000       0.0000000        0.5000000        NA     NA
      m2.1         0.5000000       0.8164966        0.5000000        NA     NA
      m3.1         2.3804761       0.5773503        0.9574271 0.3535534     NA
      m4.1         0.8164966       1.2909944        1.0000000        NA     NA
      m5.1         1.4142136       0.5000000        0.0000000        NA     NA
      m6.1         0.5000000       0.8164966        0.5000000        NA     NA
      m7.1         0.5000000       0.5773503        0.5000000        NA     NA
      m8.1         0.5000000       0.0000000        0.5000000        NA     NA
      m9.1         0.5773503       0.5000000        0.5773503        NA     NA
      m10.1        0.5773503       0.8164966        0.5000000        NA     NA
            Omission_10pctSD
      m1.1        0.10000000
      m2.1        0.09577186
      m3.1        0.10679693
      m4.1        0.10679693
      m5.1        0.10000000
      m6.1        0.10000000
      m7.1        0.10000000
      m8.1        0.08350000
      m9.1        0.10000000
      m10.1       0.10679693
      
      $`Araucaria angustifolia`
            algo       ROC       TSS Sensitivity Specificity Pos Pred Value
      m1.1  kknn 0.9519110 0.8380326     0.98675     0.85100        0.95175
      m2.1  kknn 0.9309367 0.8158208     0.93475     0.88075        0.95950
      m3.1  kknn 0.9086779 0.8261278     0.98675     0.83925        0.95175
      m4.1  kknn 0.9349937 0.8699875     0.94725     0.92250        0.97250
      m5.1  kknn 0.9309211 0.7785088     0.98675     0.79175        0.94225
      m6.1  kknn 0.9307801 0.8314850     0.97425     0.85700        0.95250
      m7.1  kknn 0.9491620 0.8142857     0.97500     0.83925        0.95000
      m8.1  kknn 0.9702538 0.9029135     0.97425     0.92850        0.97500
      m9.1  kknn 0.9347588 0.8695175     0.96100     0.90825        0.97425
      m10.1 kknn 0.9746711 0.8796992     0.98675     0.89275        0.96225
            Neg Pred Value Precision  Recall      F1 Prevalence Detection Rate
      m1.1         0.96875   0.95175 0.98675 0.96825    0.74050        0.73075
      m2.1         0.83325   0.95950 0.93475 0.94600    0.74800        0.69875
      m3.1         0.96875   0.95175 0.98675 0.96825    0.74775        0.73800
      m4.1         0.88900   0.97250 0.94725 0.95825    0.75525        0.71525
      m5.1         0.96875   0.94225 0.98675 0.96250    0.74800        0.73825
      m6.1         0.92250   0.95250 0.97425 0.96250    0.74075        0.72150
      m7.1         0.91675   0.95000 0.97500 0.96200    0.74775        0.72850
      m8.1         0.92250   0.97500 0.97425 0.97425    0.74775        0.72850
      m9.1         0.87850   0.97425 0.96100 0.96700    0.77025        0.74025
      m10.1        0.96425   0.96225 0.98675 0.97425    0.74075        0.73100
            Detection Prevalence Balanced Accuracy Accuracy   Kappa AccuracyLower
      m1.1               0.76925           0.91925  0.95225 0.86850       0.79025
      m2.1               0.72825           0.90800  0.92125 0.79825       0.74825
      m3.1               0.77725           0.91325  0.95150 0.86025       0.78800
      m4.1               0.73450           0.93500  0.94075 0.85775       0.78225
      m5.1               0.78825           0.88925  0.94050 0.81525       0.77600
      m6.1               0.75925           0.91575  0.94300 0.84450       0.78025
      m7.1               0.76725           0.90725  0.94200 0.83725       0.77975
      m8.1               0.74775           0.95150  0.96175 0.89775       0.80825
      m9.1               0.76075           0.93500  0.94975 0.85625       0.78150
      m10.1              0.75950           0.94000  0.96200 0.90050       0.80650
            AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1        0.99700      0.74050        0.00800     0.8700000    19.25     6.75
      m2.1        0.98575      0.74800        0.07375     0.9042500    19.25     6.50
      m3.1        0.99675      0.74775        0.01425     0.8700000    19.25     6.50
      m4.1        0.98275      0.75525        0.10700     0.6855000    19.25     6.25
      m5.1        0.99100      0.74800        0.04025     0.5760000    19.25     6.50
      m6.1        0.99150      0.74075        0.01975     0.8266667    19.25     6.75
      m7.1        0.98850      0.74775        0.06550     1.0000000    19.25     6.50
      m8.1        0.99375      0.74775        0.01600     1.0000000    19.25     6.50
      m9.1        0.99675      0.77025        0.03125     1.0000000    19.25     5.75
      m10.1       0.99725      0.74075        0.00575     1.0000000    19.25     6.75
            True Positive False Positive True Negative False Negative    CBI pAUC
      m1.1          19.00           0.25          5.75           1.00 1.0000  NaN
      m2.1          18.00           1.25          5.75           0.75 0.2110  NaN
      m3.1          19.00           0.25          5.50           1.00 1.0000  NaN
      m4.1          18.25           1.00          5.75           0.50    NaN  NaN
      m5.1          19.00           0.25          5.25           1.25 1.0000  NaN
      m6.1          18.75           0.50          5.75           1.00    NaN  NaN
      m7.1          18.75           0.50          5.50           1.00 0.6055  NaN
      m8.1          18.75           0.50          6.00           0.50 0.4740  NaN
      m9.1          18.50           0.75          5.25           0.50    NaN  NaN
      m10.1         19.00           0.25          6.00           0.75 1.0000  NaN
            Omission_10pct      ROCSD      TSSSD SensitivitySD SpecificitySD
      m1.1         0.06575 0.04174198 0.09601198    0.02650000    0.11737405
      m2.1         0.03875 0.06656051 0.11640745    0.06643982    0.08030100
      m3.1         0.03950 0.06661745 0.11710536    0.02650000    0.13646092
      m4.1         0.00000 0.08900827 0.17801655    0.10550000    0.09002407
      m5.1         0.03950 0.12125032 0.23639496    0.02650000    0.24994449
      m6.1         0.03900 0.08342539 0.16685078    0.02975875    0.16512218
      m7.1         0.05125 0.05573510 0.18074340    0.05000000    0.13646092
      m8.1         0.03875 0.04441778 0.16255747    0.02975875    0.14300000
      m9.1         0.03900 0.04896248 0.09792495    0.02603843    0.10679693
      m10.1        0.09075 0.04830629 0.08395055    0.02650000    0.07150000
            Pos Pred ValueSD Neg Pred ValueSD PrecisionSD   RecallSD       F1SD
      m1.1        0.03880185       0.06250000  0.03880185 0.02650000 0.01223043
      m2.1        0.02725803       0.15590248  0.02725803 0.06643982 0.04007493
      m3.1        0.03880185       0.06250000  0.03880185 0.02650000 0.01223043
      m4.1        0.03226453       0.22200000  0.03226453 0.10550000 0.06844158
      m5.1        0.06875258       0.06250000  0.06875258 0.02650000 0.03126766
      m6.1        0.05484828       0.09002407  0.05484828 0.02975875 0.03126766
      m7.1        0.04082483       0.16650000  0.04082483 0.05000000 0.04311226
      m8.1        0.05000000       0.09002407  0.05000000 0.02975875 0.03629853
      m9.1        0.02975875       0.08534049  0.02975875 0.02603843 0.01334166
      m10.1       0.02525041       0.07150000  0.02525041 0.02650000 0.02166987
            PrevalenceSD Detection RateSD Detection PrevalenceSD Balanced AccuracySD
      m1.1    0.01900000       0.03143644             0.05468318          0.04821048
      m2.1    0.01444530       0.04070524             0.04314607          0.05817789
      m3.1    0.01968714       0.03468910             0.06381941          0.05885788
      m4.1    0.00950000       0.07735363             0.06356886          0.08918520
      m5.1    0.01444530       0.03210789             0.08678085          0.11832547
      m6.1    0.01367175       0.01276715             0.04109643          0.08357980
      m7.1    0.01968714       0.02791057             0.02824152          0.09056995
      m8.1    0.01968714       0.02791057             0.01968714          0.08119319
      m9.1    0.01510794       0.01808084             0.04108021          0.04887399
      m10.1   0.01367175       0.02864728             0.02037155          0.04197618
            AccuracySD    KappaSD AccuracyLowerSD AccuracyUpperSD AccuracyNullSD
      m1.1  0.01950000 0.05633531      0.02750000     0.004000000     0.01900000
      m2.1  0.05775451 0.14053795      0.07942449     0.020934421     0.01444530
      m3.1  0.02100000 0.07268368      0.03200000     0.004500000     0.01968714
      m4.1  0.09544064 0.22232465      0.12861668     0.033836617     0.00950000
      m5.1  0.05190697 0.17456494      0.07966597     0.011575837     0.01444530
      m6.1  0.04779819 0.13181932      0.06866525     0.011090537     0.01367175
      m7.1  0.06659329 0.18743777      0.09168197     0.021671794     0.01968714
      m8.1  0.05421178 0.14472128      0.07903744     0.011842719     0.01968714
      m9.1  0.02185368 0.07309526      0.03453983     0.004500000     0.01510794
      m10.1 0.03144307 0.07964295      0.04660830     0.004193249     0.01367175
            AccuracyPValueSD McnemarPValueSD PositiveSD NegativeSD True PositiveSD
      m1.1       0.006271629       0.2600000        0.5  0.5000000       0.8164966
      m2.1       0.117539710       0.1915000        0.5  0.5773503       1.4142136
      m3.1       0.018136060       0.2600000        0.5  0.5773503       0.8164966
      m4.1       0.210668776       0.4447702        0.5  0.5000000       2.2173558
      m5.1       0.054463290       0.3850818        0.5  0.5773503       0.8164966
      m6.1       0.023753947       0.3002221        0.5  0.5000000       0.5000000
      m7.1       0.123056897       0.0000000        0.5  0.5773503       0.5000000
      m8.1       0.025073226       0.0000000        0.5  0.5773503       0.5000000
      m9.1       0.043835868       0.0000000        0.5  0.5000000       0.5773503
      m10.1      0.006898067       0.0000000        0.5  0.5000000       0.8164966
            False PositiveSD True NegativeSD False NegativeSD     CBISD pAUCSD
      m1.1         0.5000000       0.9574271        0.8164966 0.0000000     NA
      m2.1         1.2583057       0.9574271        0.5000000        NA     NA
      m3.1         0.5000000       1.2909944        0.8164966        NA     NA
      m4.1         2.0000000       0.5000000        0.5773503        NA     NA
      m5.1         0.5000000       2.0615528        1.5000000        NA     NA
      m6.1         0.5773503       0.9574271        1.1547005        NA     NA
      m7.1         1.0000000       1.2909944        0.8164966 0.5579073     NA
      m8.1         0.5773503       0.8164966        1.0000000        NA     NA
      m9.1         0.5000000       0.9574271        0.5773503        NA     NA
      m10.1        0.5000000       0.0000000        0.5000000 0.0000000     NA
            Omission_10pctSD
      m1.1        0.05022201
      m2.1        0.05006246
      m3.1        0.05030904
      m4.1        0.00000000
      m5.1        0.05030904
      m6.1        0.02975875
      m7.1        0.05921360
      m8.1        0.05006246
      m9.1        0.02603843
      m10.1       0.02527680
      

