# vif_predictors - normal path

    Code
      vif_summary(i)
    Output
      4 variables from the 6 input variables have collinearity problem: 
       
      sub soma bio1 bio12 
      
      After excluding the collinear variables, the linear correlation coefficients ranges between: 
      min correlation ( prod ~ div ):  0.03743047 
      max correlation ( prod ~ div ):  0.03743047 
      
      ---------- VIFs of the remained variables -------- 
        Variables      VIF
      1       div 1.001403
      2      prod 1.001403

---

    Code
      vif_summary(i)
    Output
      4 variables from the 6 input variables have collinearity problem: 
       
      sub soma bio1 bio12 
      
      After excluding the collinear variables, the linear correlation coefficients ranges between: 
      min correlation ( prod ~ div ):  0.03743047 
      max correlation ( prod ~ div ):  0.03743047 
      
      ---------- VIFs of the remained variables -------- 
        Variables      VIF
      1       div 1.001403
      2      prod 1.001403

# vif_predictors - facnum

    Code
      vif_summary(i)
    Output
      No variable from the 3 input variables has collinearity problem. 
      
      The linear correlation coefficients ranges between: 
      min correlation ( bio12 ~ bio1 ):  -0.4991922 
      max correlation ( test ~ bio1 ):  -0.7721316 
      
      ---------- VIFs of the remained variables -------- 
        Variables      VIF
      1      bio1 2.512167
      2     bio12 1.491410
      3      test 2.772972

