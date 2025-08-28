# vif_predictors - normal path

    Code
      vif_summary(i)
    Output
      4 variables from the 6 input variables have collinearity problem: 
       
      sub soma bio1 bio12 
      
      After excluding the collinear variables, the linear correlation coefficients ranges between: 
      min correlation ( prod ~ div ):  -0.09041999 
      max correlation ( prod ~ div ):  -0.09041999 
      
      ---------- VIFs of the remained variables -------- 
        Variables      VIF
      1       div 1.008243
      2      prod 1.008243

---

    Code
      vif_summary(i)
    Output
      4 variables from the 6 input variables have collinearity problem: 
       
      sub soma bio1 bio12 
      
      After excluding the collinear variables, the linear correlation coefficients ranges between: 
      min correlation ( prod ~ div ):  -0.09041999 
      max correlation ( prod ~ div ):  -0.09041999 
      
      ---------- VIFs of the remained variables -------- 
        Variables      VIF
      1       div 1.008243
      2      prod 1.008243

# vif_predictors - facnum

    Code
      vif_summary(i)
    Output
      No variable from the 3 input variables has collinearity problem. 
      
      The linear correlation coefficients ranges between: 
      min correlation ( bio12 ~ bio1 ):  -0.5114804 
      max correlation ( test ~ bio1 ):  -0.8292101 
      
      ---------- VIFs of the remained variables -------- 
        Variables      VIF
      1      bio1 3.211198
      2     bio12 1.527657
      3      test 3.610643

