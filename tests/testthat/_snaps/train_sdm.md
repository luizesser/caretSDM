# train_sdm

    Code
      i2
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
      Algorithms Names              : kknn naive_bayes 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : cv 
          Number                    : 2 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC      Sens      Spec      ROCSD
      1        kknn 0.6605192 0.9920635 0.1097222 0.08681466
      2 naive_bayes 0.8402910 0.9706349 0.3631944 0.11121967
      

# train_sdm - pca

    Code
      i2
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
               algo       ROC      Sens       Spec     ROCSD
      1        kknn 0.5741815 0.9801587 0.08819444 0.0710965
      2 naive_bayes 0.8392427 0.9857143 0.15208333 0.1099336
      

# train_sdm - vif

    Code
      i2
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
      1        kknn 0.6314964 0.9865079 0.1381944 0.06344787
      2 naive_bayes 0.8625942 0.9666667 0.3645833 0.04378965
      

# train_sdm - change ctrl

    Code
      i2
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
               algo       ROC      Sens       Spec      ROCSD
      1        kknn 0.5646017 0.9898871 0.07271169 0.06067877
      2 naive_bayes 0.8402030 0.9966436 0.14952596 0.10851423
      

# train_sdm - selecting vars

    Code
      i2
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
      1        kknn 0.5642758 0.9785714 0.05347222 0.08865492
      2 naive_bayes 0.8495552 0.9690476 0.40902778 0.04172257
      

# train_sdm - ESM

    Code
      i2
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
      1        kknn 0.5662136 0.9883598 0.05787037 0.06801226
      2 naive_bayes 0.8406983 0.9748677 0.43356481 0.08100005
      

# mahal.dist train

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 420 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
          Number of PA sets         : 3 
          Number of PAs in each set : 420 
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
      1 mahal.custom 0.9886484 0.8152958   0.8254444           1
      

# train_sdm - independent data

    Code
      i1
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Salminus brasiliensis Araucaria angustifolia 
      Number of presences           : 76 21 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
          Number of PA sets         : 10 
          Number of PAs in each set : 76 21 
      Independent Test              : TRUE (number of records =  26 )
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
        algo     ROC       TSS Sensitivity Specificity
      1 kknn 0.90775 0.7841667    0.848325    0.940825
      
      $`Araucaria angustifolia`
        algo       ROC       TSS Sensitivity Specificity
      1 kknn 0.9652412 0.8711842     0.97355    0.897375
      
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
      m1.1  kknn 0.9000000 0.8000000     0.90000     0.90000        0.92250
      m2.1  kknn 0.9250000 0.8500000     0.85000     1.00000        1.00000
      m3.1  kknn 0.8791667 0.7583333     0.80825     0.95000        0.95825
      m4.1  kknn 0.9458333 0.8166667     0.86675     0.95000        0.95825
      m5.1  kknn 0.9250000 0.8500000     0.85000     1.00000        1.00000
      m6.1  kknn 0.9583333 0.8583333     0.90825     0.95000        0.95825
      m7.1  kknn 0.9233333 0.7666667     0.81675     0.95000        0.95000
      m8.1  kknn 0.8791667 0.7583333     0.80825     0.95000        0.95825
      m9.1  kknn 0.8125000 0.5250000     0.81675     0.75825        0.82500
      m10.1 kknn 0.9291667 0.8583333     0.85825     1.00000        1.00000
            Neg Pred Value Precision  Recall      F1 Prevalence Detection Rate
      m1.1         0.92850   0.92250 0.90000 0.89550        0.5        0.45000
      m2.1         0.89275   1.00000 0.85000 0.90975        0.5        0.42725
      m3.1         0.87050   0.95825 0.80825 0.84725        0.5        0.40425
      m4.1         0.88675   0.95825 0.86675 0.89950        0.5        0.42975
      m5.1         0.89575   1.00000 0.85000 0.90975        0.5        0.42950
      m6.1         0.90825   0.95825 0.90825 0.93050        0.5        0.45250
      m7.1         0.83675   0.95000 0.81675 0.87225        0.5        0.40475
      m8.1         0.86450   0.95825 0.80825 0.84725        0.5        0.40250
      m9.1         0.74975   0.82500 0.81675 0.79225        0.5        0.40700
      m10.1        0.87475   1.00000 0.85825 0.92175        0.5        0.42750
            Detection Prevalence Balanced Accuracy Accuracy   Kappa AccuracyLower
      m1.1               0.52025           0.90000  0.90225 0.80350       0.57525
      m2.1               0.42725           0.92500  0.92725 0.85350       0.60950
      m3.1               0.42925           0.87925  0.87925 0.75825       0.55250
      m4.1               0.45475           0.90825  0.90450 0.81125       0.57675
      m5.1               0.42950           0.92500  0.92950 0.85525       0.61100
      m6.1               0.47500           0.92925  0.92950 0.85825       0.61100
      m7.1               0.42975           0.88325  0.87950 0.76125       0.54900
      m8.1               0.42750           0.87925  0.87725 0.75500       0.55125
      m9.1               0.54550           0.76250  0.76825 0.52375       0.43975
      m10.1              0.42750           0.92925  0.92725 0.85500       0.60300
            AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1        0.99250       0.5225        0.02300     0.8700000     5.25     5.25
      m2.1        0.99325       0.5225        0.01750     0.7400000     5.25     5.25
      m3.1        0.98200       0.5000        0.04675     0.7493333     5.25     5.25
      m4.1        0.99275       0.5225        0.02400     0.8700000     5.25     5.25
      m5.1        0.99350       0.5225        0.01850     0.7400000     5.25     5.25
      m6.1        0.99350       0.5225        0.01850     1.0000000     5.25     5.25
      m7.1        0.98725       0.5225        0.03200     0.8266667     5.25     5.25
      m8.1        0.98200       0.5225        0.04925     0.7493333     5.25     5.25
      m9.1        0.93425       0.5225        0.22525     0.9207500     5.25     5.25
      m10.1       0.99800       0.5225        0.00900     1.0000000     5.25     5.25
            True Positive False Positive True Negative False Negative      ROCSD
      m1.1           4.75           0.50          4.75           0.75 0.08164966
      m2.1           4.50           0.75          5.25           0.00 0.09574271
      m3.1           4.25           1.00          5.00           0.25 0.12720281
      m4.1           4.50           1.00          5.00           0.25 0.05335937
      m5.1           4.50           0.75          5.25           0.00 0.09574271
      m6.1           4.75           0.50          5.00           0.25 0.08858455
      m7.1           4.25           1.00          5.00           0.25 0.07272475
      m8.1           4.25           1.00          5.00           0.25 0.12720281
      m9.1           4.25           1.25          4.00           1.50 0.26714367
      m10.1          4.50           0.75          5.25           0.00 0.04787136
                 TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD Neg Pred ValueSD
      m1.1  0.16329932    0.20000000     0.1154701       0.09002407        0.1430000
      m2.1  0.19148542    0.19148542     0.0000000       0.00000000        0.1369121
      m3.1  0.25440563    0.28332358     0.1000000       0.08350000        0.1770056
      m4.1  0.13743685    0.16316326     0.1000000       0.08350000        0.1395024
      m5.1  0.19148542    0.19148542     0.0000000       0.00000000        0.1250557
      m6.1  0.17716910    0.10679693     0.1000000       0.08350000        0.1067969
      m7.1  0.17638342    0.13731563     0.1000000       0.10000000        0.1198370
      m8.1  0.25440563    0.28332358     0.1000000       0.08350000        0.1780197
      m9.1  0.44586578    0.13731563     0.3804019       0.23629078        0.2887714
      m10.1 0.09574271    0.09577186     0.0000000       0.00000000        0.0835000
            PrecisionSD   RecallSD       F1SD PrevalenceSD Detection RateSD
      m1.1   0.09002407 0.20000000 0.10492696   0.03674235       0.10653638
      m2.1   0.00000000 0.19148542 0.11866023   0.03674235       0.11452911
      m3.1   0.08350000 0.28332358 0.18909676   0.00000000       0.14167657
      m4.1   0.08350000 0.16316326 0.08205892   0.03674235       0.06428063
      m5.1   0.00000000 0.19148542 0.11866023   0.03674235       0.12065516
      m6.1   0.08350000 0.10679693 0.08344459   0.03674235       0.04092676
      m7.1   0.10000000 0.13731563 0.09493989   0.03674235       0.03755330
      m8.1   0.08350000 0.28332358 0.18909676   0.03674235       0.13665650
      m9.1   0.23629078 0.13731563 0.15492229   0.03674235       0.06428063
      m10.1  0.00000000 0.09577186 0.05301179   0.03674235       0.03175426
            Detection PrevalenceSD Balanced AccuracySD AccuracySD    KappaSD
      m1.1              0.15329791          0.08164966 0.08177357 0.16344928
      m2.1              0.11452911          0.09574271 0.09506270 0.19039170
      m3.1              0.17016340          0.12723567 0.12723567 0.25437292
      m4.1              0.11327842          0.06883979 0.07448266 0.14550916
      m5.1              0.12065516          0.09574271 0.08802083 0.18241962
      m6.1              0.06204837          0.08844348 0.08802083 0.17731023
      m7.1              0.05994650          0.08825484 0.09136557 0.18075651
      m8.1              0.16635805          0.12723567 0.12650527 0.25317978
      m9.1              0.15649601          0.22312403 0.21238075 0.44813568
      m10.1             0.03175426          0.04784262 0.04868521 0.09712535
            AccuracyLowerSD AccuracyUpperSD AccuracyNullSD AccuracyPValueSD
      m1.1       0.11151196     0.011733144     0.02598076      0.023916521
      m2.1       0.12360286     0.012203142     0.02598076      0.025632011
      m3.1       0.14741438     0.032690468     0.00000000      0.083611702
      m4.1       0.09838149     0.010594810     0.02598076      0.027080128
      m5.1       0.11128642     0.011090537     0.02598076      0.028722813
      m6.1       0.11128642     0.011090537     0.02598076      0.028722813
      m7.1       0.11986937     0.013073510     0.02598076      0.030397368
      m8.1       0.15211481     0.032690468     0.02598076      0.082001524
      m9.1       0.18506103     0.112562205     0.02598076      0.395868981
      m10.1      0.07617524     0.001414214     0.02598076      0.005416026
            McnemarPValueSD PositiveSD NegativeSD True PositiveSD False PositiveSD
      m1.1        0.3002221        0.5        0.5       1.2583057        1.0000000
      m2.1        0.3676955        0.5        0.5       1.2909944        0.9574271
      m3.1        0.4341674        0.5        0.5       1.5000000        1.4142136
      m4.1        0.3002221        0.5        0.5       0.5773503        0.9574271
      m5.1        0.3676955        0.5        0.5       1.2909944        0.9574271
      m6.1        0.0000000        0.5        0.5       0.5000000        0.5773503
      m7.1        0.3002221        0.5        0.5       0.5000000        0.8164966
      m8.1        0.4341674        0.5        0.5       1.5000000        1.4142136
      m9.1        0.1585000        0.5        0.5       0.5000000        0.8164966
      m10.1       0.0000000        0.5        0.5       0.5773503        0.5000000
            True NegativeSD False NegativeSD
      m1.1        0.9574271        0.5773503
      m2.1        0.5000000        0.0000000
      m3.1        0.8164966        0.5000000
      m4.1        0.8164966        0.5000000
      m5.1        0.5000000        0.0000000
      m6.1        0.8164966        0.5000000
      m7.1        0.8164966        0.5000000
      m8.1        0.8164966        0.5000000
      m9.1        2.0000000        1.8929694
      m10.1       0.5000000        0.0000000
      
      $`Araucaria angustifolia`
            algo       ROC       TSS Sensitivity Specificity Pos Pred Value
      m1.1  kknn 0.9682018 0.8486842     0.97375     0.87500        0.96375
      m2.1  kknn 0.9747807 0.8903509     0.97350     0.91650        0.97425
      m3.1  kknn 0.9714912 0.7653509     0.97350     0.79150        0.94325
      m4.1  kknn 0.9791667 0.9451754     0.98675     0.95825        0.98750
      m5.1  kknn 0.9802632 0.8903509     0.97350     0.91650        0.97500
      m6.1  kknn 0.9451754 0.8903509     0.97350     0.91650        0.97425
      m7.1  kknn 0.9353070 0.8640351     0.94725     0.91650        0.97275
      m8.1  kknn 0.9475877 0.8951754     0.98675     0.90825        0.97425
      m9.1  kknn 0.9660088 0.8903509     0.97350     0.91650        0.97425
      m10.1 kknn 0.9844298 0.8320175     0.97350     0.85825        0.96175
            Neg Pred Value Precision  Recall      F1 Prevalence Detection Rate
      m1.1         0.93750   0.96375 0.97375 0.96700    0.76000        0.74000
      m2.1         0.92250   0.97425 0.97350 0.97350    0.76000        0.74000
      m3.1         0.92250   0.94325 0.97350 0.95625    0.75275        0.73275
      m4.1         0.95825   0.98750 0.98675 0.98675    0.76800        0.75750
      m5.1         0.92850   0.97500 0.97350 0.97350    0.76000        0.74000
      m6.1         0.92700   0.97425 0.97350 0.97350    0.75275        0.73300
      m7.1         0.85100   0.97275 0.94725 0.95975    0.76000        0.72000
      m8.1         0.95000   0.97425 0.98675 0.98025    0.76800        0.75750
      m9.1         0.91650   0.97425 0.97350 0.97350    0.76800        0.74750
      m10.1        0.91425   0.96175 0.97350 0.96700    0.77600        0.75550
            Detection Prevalence Balanced Accuracy Accuracy   Kappa AccuracyLower
      m1.1               0.77000           0.92425  0.95000 0.85975       0.78475
      m2.1               0.76000           0.94525  0.96000 0.89025       0.79875
      m3.1               0.78275           0.88275  0.93000 0.77725       0.76075
      m4.1               0.76750           0.97275  0.97950 0.94150       0.82775
      m5.1               0.76000           0.94550  0.96000 0.89000       0.79600
      m6.1               0.75300           0.94525  0.96050 0.89275       0.80075
      m7.1               0.74000           0.93200  0.94000 0.84150       0.77175
      m8.1               0.77800           0.94775  0.96925 0.90775       0.81300
      m9.1               0.76750           0.94525  0.95950 0.88675       0.79700
      m10.1              0.78625           0.91625  0.94875 0.84775       0.77775
            AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
      m1.1        0.99475      0.76000         0.0230     0.6533333       19     6.00
      m2.1        0.99700      0.76000         0.0150     1.0000000       19     6.00
      m3.1        0.98600      0.75275         0.0745     0.7113333       19     6.25
      m4.1        0.99950      0.76800         0.0270     1.0000000       19     5.75
      m5.1        0.99900      0.76000         0.0090     1.0000000       19     6.00
      m6.1        0.99700      0.75275         0.0135     1.0000000       19     6.25
      m7.1        0.99100      0.76000         0.0420     1.0000000       19     6.00
      m8.1        0.99725      0.76800         0.0270     1.0000000       19     5.75
      m9.1        0.99700      0.76800         0.0195     1.0000000       19     5.75
      m10.1       0.99675      0.77600         0.0355     1.0000000       19     5.50
            True Positive False Positive True Negative False Negative      ROCSD
      m1.1          18.50           0.50          5.25           0.75 0.03623402
      m2.1          18.50           0.50          5.50           0.50 0.05517432
      m3.1          18.50           0.50          5.00           1.25 0.05413307
      m4.1          18.75           0.50          5.50           0.25 0.04166667
      m5.1          18.50           0.50          5.50           0.50 0.03250254
      m6.1          18.50           0.50          5.75           0.50 0.05045449
      m7.1          18.00           1.00          5.50           0.50 0.06258010
      m8.1          18.75           0.25          5.25           0.50 0.06301305
      m9.1          18.50           0.50          5.25           0.50 0.05194175
      m10.1         18.50           0.50          4.75           0.75 0.01569783
                 TSSSD SensitivitySD SpecificitySD Pos Pred ValueSD Neg Pred ValueSD
      m1.1  0.13950310    0.05250000    0.15945532       0.04571196       0.12500000
      m2.1  0.10090897    0.03059956    0.09641749       0.02975875       0.09002407
      m3.1  0.30280104    0.03059956    0.31564167       0.08206248       0.09002407
      m4.1  0.08240480    0.05250000    0.08350000       0.02500000       0.14300000
      m5.1  0.06583819    0.03059956    0.09641749       0.02886751       0.08256109
      m6.1  0.10090897    0.03059956    0.09641749       0.02975875       0.08601938
      m7.1  0.13020728    0.04286704    0.09641749       0.03148942       0.11737405
      m8.1  0.12602611    0.02650000    0.10679693       0.02975875       0.10000000
      m9.1  0.10090897    0.03059956    0.09641749       0.02975875       0.09641749
      m10.1 0.08465253    0.03059956    0.09577186       0.02553919       0.10171324
            PrecisionSD   RecallSD         F1SD PrevalenceSD Detection RateSD
      m1.1   0.04571196 0.05250000 0.0255342907   0.00000000       0.04000000
      m2.1   0.02975875 0.03059956 0.0216410105   0.00000000       0.02309401
      m3.1   0.08206248 0.03059956 0.0404423458   0.01450000       0.01889224
      m4.1   0.02500000 0.05250000 0.0266520793   0.01600000       0.02600000
      m5.1   0.02886751 0.03059956 0.0005773503   0.00000000       0.02309401
      m6.1   0.02975875 0.03059956 0.0216410105   0.01450000       0.03320643
      m7.1   0.03148942 0.04286704 0.0347311100   0.00000000       0.03265986
      m8.1   0.02975875 0.02650000 0.0253294953   0.01600000       0.00500000
      m9.1   0.02975875 0.03059956 0.0216410105   0.01600000       0.01892969
      m10.1  0.02553919 0.03059956 0.0133416641   0.01847521       0.02968164
            Detection PrevalenceSD Balanced AccuracySD AccuracySD     KappaSD
      m1.1              0.06831301          0.06984447 0.03829708 0.108149202
      m2.1              0.03265986          0.05057915 0.03265986 0.089540959
      m3.1              0.09304255          0.15129965 0.06831301 0.246935045
      m4.1              0.03771825          0.04112582 0.03960955 0.105880436
      m5.1              0.04618802          0.03290897 0.00000000 0.006928203
      m6.1              0.04482559          0.05057915 0.03267517 0.089893919
      m7.1              0.02309401          0.06525846 0.05163978 0.135411718
      m8.1              0.02103965          0.06283510 0.03960955 0.120311748
      m9.1              0.02217356          0.05057915 0.03267517 0.089514896
      m10.1             0.04759814          0.04236646 0.02118765 0.068451321
            AccuracyLowerSD AccuracyUpperSD AccuracyNullSD AccuracyPValueSD
      m1.1       0.05846580     0.005500000     0.00000000       0.02103965
      m2.1       0.05031484     0.004690416     0.00000000       0.01773885
      m3.1       0.09660702     0.021150256     0.01450000       0.11699715
      m4.1       0.06371290     0.004856267     0.01600000       0.04681880
      m5.1       0.00000000     0.000000000     0.00000000       0.00000000
      m6.1       0.05032809     0.004690416     0.01450000       0.01864582
      m7.1       0.07513710     0.011575837     0.00000000       0.05290243
      m8.1       0.06371290     0.004856267     0.01600000       0.04681880
      m9.1       0.05056349     0.004690416     0.01600000       0.01799074
      m10.1      0.03200391     0.004500000     0.01847521       0.04186884
            McnemarPValueSD PositiveSD NegativeSD True PositiveSD False PositiveSD
      m1.1        0.3002221          0  0.0000000       1.0000000        1.0000000
      m2.1        0.0000000          0  0.0000000       0.5773503        0.5773503
      m3.1        0.4999853          0  0.5000000       0.5773503        0.5773503
      m4.1        0.3676955          0  0.5000000       1.0000000        1.0000000
      m5.1        0.0000000          0  0.0000000       0.5773503        0.5773503
      m6.1        0.0000000          0  0.5000000       0.5773503        0.5773503
      m7.1        0.0000000          0  0.0000000       0.8164966        0.8164966
      m8.1        0.0000000          0  0.5000000       0.5000000        0.5000000
      m9.1        0.0000000          0  0.5000000       0.5773503        0.5773503
      m10.1       0.0000000          0  0.5773503       0.5773503        0.5773503
            True NegativeSD False NegativeSD
      m1.1        0.9574271        0.9574271
      m2.1        0.5773503        0.5773503
      m3.1        2.1602469        1.8929694
      m4.1        0.5773503        0.5000000
      m5.1        0.5773503        0.5773503
      m6.1        0.9574271        0.5773503
      m7.1        0.5773503        0.5773503
      m8.1        0.9574271        0.5773503
      m9.1        0.5000000        0.5773503
      m10.1       0.9574271        0.5000000
      

