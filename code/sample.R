# read all data files in readable_form and concatenate
file.names <- c("2_readable.csv", "4_readable.csv", "6_readable.csv", "8_readable.csv", "9_readable.csv", 
                "10_readable.csv", "12_readable.csv", "13_readable.csv", "15_readable.csv", "16_readable.csv")
file_index <- 1
for (file.name in file.names){
  # print(paste0("/Users/ywang2466/Documents/GitHub/BabaIsYou_2023/data/readable_form/",file.name))
  temp <- read.csv(paste0("/Users/ywang2466/Documents/GitHub/BabaIsYou_2023/data/readable_form/",file.name))
  print(file_index)
  print(nrow(temp))
  if(file_index == 1){
    df <- temp
  }
  df <- rbind(df,temp)
  file_index <- file_index + 1
}
code_cols <- c("StartorRestart", "RuleBreak", "Deviation", "PassedBoundary", "Flag", "YouObject", "YouRule", "TextObstacle", "BrokenStop", "OutsideText", "WinText", "Is.Outside", "ObjectObstacle", "OtherObstacle")           
df$code_sum <- rowSums(df[,code_cols])
non_zero_df <- df[df$code_sum != 0,]
unique_ids <- as.numeric(unique(c(non_zero_df$unique_eye_gaze_id, non_zero_df$unique_log_id, non_zero_df$unique_pos_id)))
set.seed(123)
unique_ids_sampled <- sample(unique_ids,100)
unique_ids_sampled



df$sampled <- "FALSE"

df$sampled[df$unique_eye_gaze_id %in% unique_ids_sampled | df$unique_log_id %in% unique_ids_sampled | df$unique_pos_id %in% unique_ids_sampled] <- "TRUE"
df$sampled
View(df)
write.csv(df, "data/combined_sampled.csv")
