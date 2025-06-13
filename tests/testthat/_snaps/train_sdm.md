# train_sdm

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 414 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
          Number of PA sets         : 10 
          Number of PAs in each set : 414 
      --------  Predictors  ---------
      Number of Predictors          : 6 
      Predictors Names              : bio1, bio12, div, prod, sub, soma 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : kknn mda naive_bayes 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1        kknn 0.9918481 0.9815342    0.991975    0.989525
      2         mda 0.9870577 0.9414377    0.989575    0.955225
      3 naive_bayes 0.9953786 0.9676901    0.992925    0.983075
      

# train_sdm - pca

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 414 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
          Number of PA sets         : 10 
          Number of PAs in each set : 414 
      --------  Predictors  ---------
      Number of Predictors          : 12 
      Predictors Names              : bio1, bio12, div, prod, sub, soma, PC1, PC2, PC3, PC4, PC5, PC6 
      PCA-transformed variables     : DONE 
      Cummulative proportion (0.99) : PC1 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : kknn mda naive_bayes 
      Variables Names               : PC1 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1        kknn 0.9996394 0.9992788    0.999275      1.0000
      2         mda 1.0000000 0.9228272    1.000000      0.9250
      3 naive_bayes 0.9986756 0.8846107    0.997075      0.8875
      

# train_sdm - change ctrl

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 414 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
          Number of PA sets         : 10 
          Number of PAs in each set : 414 
      --------  Predictors  ---------
      Number of Predictors          : 12 
      Predictors Names              : bio1, bio12, div, prod, sub, soma, PC1, PC2, PC3, PC4, PC5, PC6 
      PCA-transformed variables     : DONE 
      Cummulative proportion (0.99) : PC1 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : kknn mda naive_bayes 
      Variables Names               : PC1 
      Model Validation              :
          Method                    : boot 
          Number                    : 10 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC      Sens      Spec      ROCSD
      1        kknn 0.9948919 0.9992282 0.9905556 0.01545422
      2         mda 1.0000000 1.0000000 0.7528889 0.00000000
      3 naive_bayes 0.9891688 0.9949012 0.8358730 0.08506030
      

# train_sdm - vif

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 414 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
          Number of PA sets         : 10 
          Number of PAs in each set : 414 
      --------  Predictors  ---------
      Number of Predictors          : 6 
      Predictors Names              : bio1, bio12, div, prod, sub, soma 
      Area (VIF)                    : all
      Threshold                     : 0.5
      Selected Variables (VIF)      : div, prod 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes mda kknn 
      Variables Names               : div prod 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1        kknn 0.9944007 0.9888014    0.994825    0.993925
      2         mda 0.9992931 0.9686132    0.982350    0.987925
      3 naive_bayes 0.9986406 0.9386259    0.987600    0.951200
      

# train_sdm - selecting vars

    Code
      i2
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 414 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
          Number of PA sets         : 10 
          Number of PAs in each set : 414 
      --------  Predictors  ---------
      Number of Predictors          : 6 
      Predictors Names              : bio1, bio12, div, prod, sub, soma 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes mda kknn 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1        kknn 0.9920913 0.9832597    0.990225    0.992925
      2         mda 0.9894955 0.9447250    0.989350    0.960775
      3 naive_bayes 0.9953773 0.9700069    0.992675    0.985275
      

