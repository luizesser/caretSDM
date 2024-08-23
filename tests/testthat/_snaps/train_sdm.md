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
      1 kknn       0.992 0.994 0.990 0.00826 0.00788 0.0156
      2 mda        0.993 0.989 0.986 0.0157  0.0145  0.0206
      3 nnet       0.995 1     0.936 0.0978  0.00829 0.169 
      4 svmLinear2 0.953 0.985 0.934 0.0311  0.0171  0.0309
      

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
      1 kknn       0.967 0.997 0.935 0.0477 0.00757  0.104
      2 mda        0.999 1     0.898 0.0981 0.00987  0.122
      3 nnet       0.965 1     0.857 0.146  0.00338  0.528
      4 svmLinear2 0.898 1     0.794 0.0853 0        0.138
      

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
      1 kknn       0.966 0.998 0.930 0.0405 0.00420  0.0862
      2 mda        0.993 0.999 0.892 0.0802 0.00972  0.0970
      3 nnet       0.962 1     0.854 0.105  0.00568  0.470 
      4 svmLinear2 0.901 1     0.752 0.0704 0.000208 0.143 
      

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
        algo         ROC  Sens  Spec   ROCSD  SensSD SpecSD
        <chr>      <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl>
      1 kknn       0.990 0.997 0.980 0.00904 0.00538 0.0222
      2 mda        0.987 0.991 0.953 0.0200  0.0175  0.0298
      3 nnet       0.683 1     0.339 0.246   0.00876 0.476 
      4 svmLinear2 0.965 0.955 0.959 0.0313  0.0218  0.0379
      

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
      1 kknn       0.994 0.992 0.991 0.00788 0.00950 0.0140
      2 mda        0.993 0.988 0.983 0.0187  0.0151  0.0276
      3 nnet       0.994 1     0.929 0.0981  0.00829 0.163 
      4 svmLinear2 0.954 0.985 0.933 0.0382  0.0130  0.0362
      

