library(plm)
library(tidyverse)

# Load data
panel2 <- read.csv('output/data/panel2_sector_occupation_4years.csv')

# Create pdata
pdata2 <- pdata.frame(panel2, index = c('panel_id', 'year'))

# Check if all 4 sector dummies sum to 1
cat('Checking sector dummy sum:\n')
test_sum <- panel2 %>% 
  mutate(sector_sum = industry + construction + services + public_sector) %>%
  summarise(
    min_sum = min(sector_sum),
    max_sum = max(sector_sum),
    mean_sum = mean(sector_sum),
    unique_vals = n_distinct(sector_sum)
  )
print(test_sum)

cat('\n\nAttempting model with ALL 4 sector dummies:\n')
model_test <- tryCatch({
  plm(
    gender_pay_gap ~ industry + construction + services + public_sector + 
    high_skill + managerial + factor(year),
    data = pdata2, 
    model = 'random'
  )
}, error = function(e) {
  cat('ERROR:', e$message, '\n')
  return(NULL)
})

if (!is.null(model_test)) {
  cat('\n✅ Model successfully estimated!\n')
  cat('Coefficients:\n')
  print(coef(model_test)[1:7])
}
