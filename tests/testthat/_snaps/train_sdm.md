# train_sdm

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 418 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 418 
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
      1        kknn 0.6469233 0.9856459 0.2600733 0.05959788
      2 naive_bayes 0.8576489 0.9792663 0.2994505 0.06242039
      

# train_sdm - pca

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 418 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 418 
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
      1        kknn 0.5936322 0.9912281 0.1739927 0.04869515
      2 naive_bayes 0.8307438 1.0000000 0.1474359 0.07067102
      

# train_sdm - vif

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 418 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 418 
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
      1        kknn 0.6407715 0.9920255 0.2472527 0.05269499
      2 naive_bayes 0.8414612 0.9816587 0.2957875 0.12695378
      

# train_sdm - change ctrl

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 418 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 418 
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
               algo       ROC      Sens      Spec      ROCSD
      1        kknn 0.5915856 0.9930731 0.1839508 0.04563348
      2 naive_bayes 0.8169982 0.9991820 0.1631217 0.12930768
      

# train_sdm - selecting vars

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 418 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 3 
          Number of PAs in each set : 418 
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
               algo       ROC      Sens      Spec      ROCSD
      1        kknn 0.6368237 0.9904306 0.1978022 0.08078370
      2 naive_bayes 0.8538370 0.9784689 0.3104396 0.07867379
      

