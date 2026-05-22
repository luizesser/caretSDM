# full structure check

    Code
      i_ss
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 419 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 2 
          Number of PAs in each set : 419 
      Background methods            :
          Method to obtain BGs.     : random 
          Number of Background sets : 2 
          Number of Bg in each set  : 31 
          Background proportion     : 1 
      --------  Predictors  ---------
      Number of Predictors          : 2 
      Predictors Names              : bio1, bio12 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : boot 
          Number                    : 1 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1 naive_bayes 0.8495643 0.1953128       0.977       0.218
      
      --------  Predictions  --------
      Thresholds                    :
          Method                    : threshold 
          Criteria                  : 0.5 
      ---------  Ensembles  ---------
      Ensembles                     :
          Methods                   : average 

---

    Code
      i_sm
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Araucaria angustifolia 
      Number of presences           : 419 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 2 
          Number of PAs in each set : 419 
      Background methods            :
          Method to obtain BGs.     : random 
          Number of Background sets : 2 
          Number of Bg in each set  : 31 
          Background proportion     : 1 
      --------  Predictors  ---------
      Number of Predictors          : 2 
      Predictors Names              : bio1, bio12 
      ---------  Scenarios  ---------
      Number of Scenarios           : 5 
      Scenarios Names               : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : boot 
          Number                    : 1 
          Metrics                   :
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1 naive_bayes 0.8301571 0.4364947       0.946         0.5
      
      --------  Predictions  --------
      Thresholds                    :
          Method                    : threshold 
          Criteria                  : 0.5 
      ---------  Ensembles  ---------
      Ensembles                     :
          Methods                   : average 

---

    Code
      i_ms
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Salminus brasiliensis Araucaria angustifolia 
      Number of presences           : 419 46 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 2 
          Number of PAs in each set : 419 46 
      Background methods            :
          Method to obtain BGs.     : random 
          Number of Background sets : 2 
          Number of Bg in each set  : 31 31 
          Background proportion     : 1 1 
      --------  Predictors  ---------
      Number of Predictors          : 2 
      Predictors Names              : bio1, bio12 
      ---------  Scenarios  ---------
      Number of Scenarios           : 1 
      Scenarios Names               : current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : boot 
          Number                    : 1 
          Metrics                   :
      $`Salminus brasiliensis`
               algo       ROC  TSS Sensitivity Specificity
      1 naive_bayes 0.9041667 0.65           1        0.65
      
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1 naive_bayes 0.8925104 0.3524286      0.9935      0.3655
      
      --------  Predictions  --------
      Thresholds                    :
          Method                    : threshold 
          Criteria                  : 0.5 
      ---------  Ensembles  ---------
      Ensembles                     :
          Methods                   : average 

---

    Code
      i_mm
    Output
                  caretSDM           
      ...............................
      Class                         : input_sdm
      --------  Occurrences  --------
      Species Names                 : Salminus brasiliensis Araucaria angustifolia 
      Number of presences           : 419 46 
      Pseudoabsence methods         :
          Method to obtain PAs      : random 
          Number of PA sets         : 2 
          Number of PAs in each set : 419 46 
      Background methods            :
          Method to obtain BGs.     : random 
          Number of Background sets : 2 
          Number of Bg in each set  : 31 31 
          Background proportion     : 1 1 
      --------  Predictors  ---------
      Number of Predictors          : 2 
      Predictors Names              : bio1, bio12 
      ---------  Scenarios  ---------
      Number of Scenarios           : 5 
      Scenarios Names               : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current 
      -----------  Models  ----------
      Algorithms Names              : naive_bayes 
      Variables Names               : bio1 bio12 
      Model Validation              :
          Method                    : boot 
          Number                    : 1 
          Metrics                   :
      $`Salminus brasiliensis`
               algo       ROC      TSS Sensitivity Specificity
      1 naive_bayes 0.7311363 0.251462        0.89      0.4165
      
      $`Araucaria angustifolia`
               algo       ROC       TSS Sensitivity Specificity
      1 naive_bayes 0.8938589 0.3466975      0.9845      0.3715
      
      --------  Predictions  --------
      Thresholds                    :
          Method                    : threshold 
          Criteria                  : 0.5 
      ---------  Ensembles  ---------
      Ensembles                     :
          Methods                   : average 

