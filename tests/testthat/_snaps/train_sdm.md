# train_sdm

    Code
      i
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
      Number of Predictors          : 2 
      Predictors Names              : bio01, bio12 
      ---------  Scenarios  ---------
      Scenarios Names               : current 
      Number of Scenarios           : 1 
      -----------  Models  ----------
      Algorithms Names              : svmLinear2 mda nnet kknn 
      Variables Names               : bio01 bio12 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
              algo       ROC       TSS Sensitivity Specificity
      1       kknn 0.9937239 0.9834901    0.994425    0.989000
      2        mda 0.9924916 0.9700045    0.987850    0.983325
      3       nnet 0.9938679 0.9281474    1.000000    0.932050
      4 svmLinear2 0.9520057 0.9203348    0.987150    0.933175
      

# train_sdm - pca

    Code
      i
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
      Scenarios Names               : current 
      Number of Scenarios           : 1 
      -----------  Models  ----------
      Algorithms Names              : svmLinear2 mda nnet kknn 
      Variables Names               : PC1 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
              algo       ROC       TSS Sensitivity Specificity
      1       kknn 0.9760767 0.9367208      0.9978    0.938900
      2        mda 0.9993776 0.8861145      1.0000    0.891700
      3       nnet 0.9722440 0.8820169      1.0000    0.882525
      4 svmLinear2 0.8977778 0.7772222      1.0000    0.777275
      

# train_sdm - change ctrl

    Code
      i
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
      Scenarios Names               : current 
      Number of Scenarios           : 1 
      -----------  Models  ----------
      Algorithms Names              : svmLinear2 mda nnet kknn 
      Variables Names               : PC1 
      Model Validation              :
          Method                    : boot 
          Number                    : 10 
          Metrics                   :
      $`Araucaria angustifolia`
              algo       ROC     Sens      Spec      ROCSD
      1       kknn 0.9691691 0.996277 0.9371996 0.03311052
      2        mda 0.9925113 0.998841 0.8836304 0.08687516
      3       nnet 0.9580712 1.000000 0.8453276 0.09673926
      4 svmLinear2 0.8903401 1.000000 0.7591607 0.06386788
      

# train_sdm - vif

    Code
      i
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
      Scenarios Names               : current 
      Number of Scenarios           : 1 
      -----------  Models  ----------
      Algorithms Names              : svmLinear2 mda nnet kknn 
      Variables Names               : bio01 prod 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
              algo       ROC       TSS Sensitivity Specificity
      1       kknn 0.9887991 0.9756446    0.992500    0.983150
      2        mda 0.9850524 0.9383663    0.990475    0.951125
      3       nnet 0.6906980 0.3870676    1.000000    0.391200
      4 svmLinear2 0.9653114 0.9156171    0.955350    0.961325
      

# train_sdm - selecting vars

    Code
      i
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
      Number of Predictors          : 2 
      Predictors Names              : bio01, bio12 
      ---------  Scenarios  ---------
      Scenarios Names               : current 
      Number of Scenarios           : 1 
      -----------  Models  ----------
      Algorithms Names              : svmLinear2 mda nnet kknn 
      Variables Names               : bio01 bio12 
      Model Validation              :
          Method                    : repeatedcv 
          Number                    : 4 
          Metrics                   :
      $`Araucaria angustifolia`
              algo       ROC       TSS Sensitivity Specificity
      1       kknn 0.9908311 0.9816621     0.99265    0.988875
      2        mda 0.9924188 0.9706946     0.98980    0.983850
      3       nnet 0.9918583 0.9287376     1.00000    0.933375
      4 svmLinear2 0.9541996 0.9201095     0.98300    0.937125
      

