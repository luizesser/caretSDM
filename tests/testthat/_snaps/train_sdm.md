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
      # A tibble: 4 x 7
        algo         ROC  Sens  Spec   ROCSD  SensSD SpecSD
        <chr>      <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl>
      1 kknn       0.993 0.994 0.991 0.00834 0.00784 0.0155
      2 mda        0.994 0.990 0.982 0.0163  0.0152  0.0286
      3 nnet       0.995 1     0.932 0.0907  0.0106  0.164 
      4 svmLinear2 0.953 0.984 0.935 0.0374  0.0150  0.0380
      

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
      # A tibble: 4 x 7
        algo         ROC  Sens  Spec  ROCSD  SensSD SpecSD
        <chr>      <dbl> <dbl> <dbl>  <dbl>   <dbl>  <dbl>
      1 kknn       0.974 0.997 0.938 0.0353 0.00523 0.0851
      2 mda        0.999 1     0.888 0.106  0.0111  0.145 
      3 nnet       0.976 1     0.887 0.129  0.00241 0.535 
      4 svmLinear2 0.898 1     0.784 0.101  0       0.153 
      

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
      # A tibble: 4 x 7
        algo         ROC  Sens  Spec  ROCSD   SensSD SpecSD
        <chr>      <dbl> <dbl> <dbl>  <dbl>    <dbl>  <dbl>
      1 kknn       0.973 0.996 0.944 0.0303 0.00731  0.0617
      2 mda        0.995 0.998 0.904 0.0809 0.00995  0.0934
      3 nnet       0.960 1     0.854 0.102  0.00565  0.470 
      4 svmLinear2 0.901 1.00  0.773 0.0663 0.000780 0.139 
      

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
      # A tibble: 4 x 7
        algo         ROC  Sens  Spec  ROCSD  SensSD SpecSD
        <chr>      <dbl> <dbl> <dbl>  <dbl>   <dbl>  <dbl>
      1 kknn       0.987 0.993 0.978 0.0114 0.00975 0.0266
      2 mda        0.985 0.990 0.949 0.0278 0.0165  0.0437
      3 nnet       0.726 1     0.440 0.251  0.0120  0.464 
      4 svmLinear2 0.963 0.955 0.959 0.0274 0.0204  0.0334
      

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
      # A tibble: 4 x 7
        algo         ROC  Sens  Spec   ROCSD  SensSD SpecSD
        <chr>      <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl>
      1 kknn       0.993 0.994 0.991 0.00906 0.00770 0.0156
      2 mda        0.994 0.989 0.984 0.0146  0.0147  0.0256
      3 nnet       0.994 1     0.929 0.185   0.00737 0.250 
      4 svmLinear2 0.954 0.985 0.934 0.0424  0.0159  0.0417
      

