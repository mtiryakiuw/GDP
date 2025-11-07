library(plm)
library(tidyverse)

# Load data
panel2 <- read.csv('output/data/panel2_sector_occupation_4years.csv')
pdata2 <- pdata.frame(panel2, index = c('panel_id', 'year'))

cat("Testing if plm drops one of the 4 sector dummies...\n\n")

# Try the exact same model as panel2_main_analysis.R
model_4sectors <- tryCatch({
  plm(
    gender_pay_gap ~ 
      industry + construction + services + public_sector +
      high_skill + managerial + 
      factor(year),
    data = pdata2, 
    model = "random"
  )
}, error = function(e) {
  cat("ERROR with 4 sectors:", e$message, "\n")
  return(NULL)
})

if (!is.null(model_4sectors)) {
  cat("✅ Model with 4 sectors SUCCEEDED!\n")
  cat("\nCoefficients:\n")
  print(coef(model_4sectors)[1:7])
  
  cat("\n\nWhich variables are in the model matrix?\n")
  mm <- model.matrix(model_4sectors)
  cat("Model matrix columns:\n")
  print(colnames(mm)[1:10])
} else {
  cat("\n❌ Model FAILED\n")
}

cat("\n\n" , rep("=", 60), "\n\n")

# Now try with only 3 sectors (services omitted)
model_3sectors <- plm(
  gender_pay_gap ~ 
    industry + construction + public_sector +
    high_skill + managerial + 
    factor(year),
  data = pdata2, 
  model = "random"
)

cat("✅ Model with 3 sectors (services omitted):\n")
cat("\nCoefficients:\n")
print(coef(model_3sectors)[1:6])
