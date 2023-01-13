test_data <- read_csv("readable_form/16_readable.csv")
test_data
test_data$select <- ""
set.seed(123)
test_data[sample(nrow(df),size= 100), "select"] <- "TRUE"
write.csv(test_data, "readable_form/16_readable_sampled.csv")
