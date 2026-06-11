# vif_predictors - normal path

    Code
      vif_summary(i)
    Output
      4 variables from the 6 input variables have collinearity problem: 
       
      sub soma bio12 div 
      
      After excluding the collinear variables, the linear correlation coefficients ranges between: 
      min correlation ( prod ~ bio1 ):  0.163641 
      max correlation ( prod ~ bio1 ):  0.163641 
      
      ---------- VIFs of the remained variables -------- 
        Variables      VIF
      1      bio1 1.027515
      2      prod 1.027515

---

    Code
      vif_summary(i)
    Output
      4 variables from the 6 input variables have collinearity problem: 
       
      sub soma bio12 div 
      
      After excluding the collinear variables, the linear correlation coefficients ranges between: 
      min correlation ( prod ~ bio1 ):  0.163641 
      max correlation ( prod ~ bio1 ):  0.163641 
      
      ---------- VIFs of the remained variables -------- 
        Variables      VIF
      1      bio1 1.027515
      2      prod 1.027515

# vif_predictors - facnum

    Code
      vif_summary(i)
    Output
      No variable from the 3 input variables has collinearity problem. 
      
      The linear correlation coefficients ranges between: 
      min correlation ( bio12 ~ bio1 ):  -0.520394 
      max correlation ( test ~ bio1 ):  -0.7721316 
      
      ---------- VIFs of the remained variables -------- 
        Variables      VIF
      1      bio1 2.480791
      2     bio12 1.725264
      3      test 3.115418

