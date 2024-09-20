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
      Predictors Names              : bio01, bio12, div, prod, sub, soma 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : kknn mda naive_bayes 
      Variables Names               : bio01 bio12 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1        kknn 0.9915652 0.9799213    0.993825    0.985975
      2         mda 0.9941093 0.9701066    0.989000    0.984400
      3 naive_bayes 0.9962166 0.9600901    0.991625    0.976625
      

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
      Predictors Names              : bio01, bio12, div, prod, sub, soma, PC1, PC2, PC3, PC4, PC5, PC6 
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
      1        kknn 0.9735477 0.9341787    0.998025    0.936125
      2         mda 0.9992853 0.8926292    1.000000    0.897250
      3 naive_bayes 0.9970729 0.8803861    0.997600    0.897800
      

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
      Predictors Names              : bio01, bio12, div, prod, sub, soma, PC1, PC2, PC3, PC4, PC5, PC6 
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
      1        kknn 0.9700477 0.9974091 0.9397002 0.03098120
      2         mda 0.9931358 0.9989349 0.8862610 0.07748935
      3 naive_bayes 0.9923798 0.9917623 0.9019343 0.06881540
      

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
      Predictors Names              : bio01, bio12, div, prod, sub, soma 
      Area (VIF)                    : all
      Threshold                     : 0.5
      Selected Variables (VIF)      : bio01, prod 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes mda kknn 
      Variables Names               : bio01 prod 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1        kknn 0.9907693 0.9778210    0.994600     0.98315
      2         mda 0.9875022 0.9403439    0.989550     0.95325
      3 naive_bayes 0.9960442 0.9519730    0.984425     0.97350
      

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
      Predictors Names              : bio01, bio12, div, prod, sub, soma 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes mda kknn 
      Variables Names               : bio01 bio12 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1        kknn 0.9928341 0.9810302     0.99340    0.987550
      2         mda 0.9918512 0.9692244     0.98885    0.981400
      3 naive_bayes 0.9960313 0.9598039     0.98975    0.977225
      

