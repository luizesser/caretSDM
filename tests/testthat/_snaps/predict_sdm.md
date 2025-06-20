# predict_sdm

    Code
      p
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 418 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
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
               algo       ROC      Sens      Spec       ROCSD
      1        kknn 0.8541667 1.0000000 0.7083333 0.088388348
      2 naive_bayes 0.9950159 0.9960128 0.7916667 0.009585977
      
      --------  Predictions  --------
      Ensembles                     :
          Scenarios                 : current 
          Methods                   : mean_occ_prob wmean_AUC committee_avg 
      Thresholds                    :
          Method                    : threshold 
          Criteria                  : 0.9 

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
               Criteria : 0.9 
               Metrics  :
      $`Araucaria angustifolia`
                  algo       ROC      Sens  Spec       ROCSD     SensSD    SpecSD
      m1.1 naive_bayes 1.0000000 1.0000000 0.750 0.002537465 0.04398272 0.3535534
      m2.1 naive_bayes 0.9922249 0.9880383 0.875 0.010995680 0.02706629 0.1767767
      m3.1 naive_bayes 0.9928230 1.0000000 0.750 0.015224787 0.04059943 0.1767767
      

# predict_sdm - th 0

    Code
      p
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 418 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
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
               algo       ROC      Sens      Spec       ROCSD
      1        kknn 0.8541667 1.0000000 0.7083333 0.088388348
      2 naive_bayes 0.9950159 0.9960128 0.7916667 0.009585977
      
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
                  algo       ROC      Sens  Spec       ROCSD     SensSD    SpecSD
      m1.2        kknn 0.8750000 1.0000000 0.750 0.000000000 0.00000000 0.0000000
      m2.2        kknn 0.8750000 1.0000000 0.750 0.000000000 0.00000000 0.0000000
      m3.2        kknn 0.8125000 1.0000000 0.625 0.265165043 0.00000000 0.5303301
      m1.1 naive_bayes 1.0000000 1.0000000 0.750 0.002537465 0.04398272 0.3535534
      m2.1 naive_bayes 0.9922249 0.9880383 0.875 0.010995680 0.02706629 0.1767767
      m3.1 naive_bayes 0.9928230 1.0000000 0.750 0.015224787 0.04059943 0.1767767
      

# predict_sdm - th function

    Code
      p
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 418 
      Pseudoabsence methods         :
          Method to obtain PAs      : bioclim 
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
               algo       ROC      Sens      Spec       ROCSD
      1        kknn 0.8541667 1.0000000 0.7083333 0.088388348
      2 naive_bayes 0.9950159 0.9960128 0.7916667 0.009585977
      
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
                  algo       ROC      Sens  Spec       ROCSD     SensSD    SpecSD
      m1.1 naive_bayes 1.0000000 1.0000000 0.750 0.002537465 0.04398272 0.3535534
      m2.1 naive_bayes 0.9922249 0.9880383 0.875 0.010995680 0.02706629 0.1767767
      m3.1 naive_bayes 0.9928230 1.0000000 0.750 0.015224787 0.04059943 0.1767767
      

