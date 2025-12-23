# gcms_ensembles/names

    Code
      i
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
      1 naive_bayes 0.8323889 0.2460904      0.9665      0.2865
      
      --------  Predictions  --------
      Ensembles                     :
          Scenarios                 : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current _ssp245_2090 _ssp585_2090 
          Methods                   : mean_occ_prob wmean_AUC committee_avg 
      Thresholds                    :
          Method                    : threshold 
          Criteria                  : 0.8 

