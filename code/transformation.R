source("code/find_digits.R")

library(readr)
library(tidyr)
library(dplyr)

df_all <- read_csv("data/combined.csv")
code_cols <- c("StartorRestart", "RuleBreak", "Deviation", "PassedBoundary", 
               "Flag", "YouObject", "YouRule", "TextObstacle", "BrokenStop", "OutsideText", "WinText", "Is-Outside", "ObjectObstacle", "OtherObstacle")

### Split the whole dataset into different players
players <- unique(df$PlayerID)
# players <- 4
one_to_multi_lines <- function(line, time_unit){
  # browser()
  lines <- data.frame(matrix(ncol = ncol(df)+1, nrow = 0))
  colnames(lines) <- c("time", colnames(df))
  line_num <- ceiling((line$End - line$Start) / time_unit)
  for (num in seq(0, line_num-1)){
    print(line_num-1)
    line_temp <- line
    line_temp <- cbind(round(line$Start + num * time_unit, digit = decimalplaces(time_unit)), line)
    colnames(line_temp)[1] <- "time"
    lines <- rbind(lines, line_temp)
  }
  return(lines)
}

for(player in players){
  print(player)
  df_all <- df[df$PlayerID == player, ]
  ### Standardize time to seconds
  df_eye <- df_all[df_all$modality == "eye", ]
  df_eye <- df_eye[rowSums(is.na(df_eye)) != ncol(df_eye), ]
  
  # time_unit <- 0.05
  time_unit <- 0.1
  rep_df <- data.frame(matrix(ncol = ncol(df_eye)+1, nrow = 0))
  colnames(rep_df) <- c("time", colnames(df_eye))
  
  for(i in seq(1,nrow(df_eye))){
    print(i)
    rep_df <- rbind(rep_df, one_to_multi_lines(df_eye[i,], time_unit))
    # print(rep_df)
  }
  ### Add the log and position modality
  df_log <- df_all[df_all$modality == "log",]
  # unique(df_log$Content)
  df_log$content <- df_log$Content
  df_detail <- df_log %>% separate(content, c("A", "B"), "_")
  # View(df_detail)
  df_log$modality <- ifelse(df_detail$A == "change", "position", "log")
  # View(df_log)
  
  ### Group by Start and Modality, aggregate contents in different lines, add codes together, keep the min of unique event index
  df_log_long <- df_log %>%
    group_by(PlayerID, Learned, LevelID, StuckID, Start, modality) %>%
    mutate(content = paste0(Content, collapse = ","),
           StartorRestart = sum(StartorRestart),
           RuleBreak = sum(RuleBreak),
           Deviation = sum(Deviation),
           PassedBoundary = sum(PassedBoundary)) %>%
    group_by(PlayerID, Learned, LevelID, StuckID, Start, modality,content) %>%
    slice_sample(n = 1) %>%
    arrange(unique_index)
  df_log_long$Start <- round(df_log_long$Start, 1)
  # # df_log_long <- head(df_log_long,10)
  # df_log_long <- df_log_long %>% drop_na(PlayerID)
  # colnames(df_log_long)
  colnames(rep_df)[colnames(rep_df) == 'unique_index'] <- 'unique_eye_gaze_id'
  colnames(rep_df)[colnames(rep_df) == 'Content'] <- 'Eye_gaze'
  rep_df$Log <- NA
  rep_df$Position <- NA
  rep_df$unique_log_id <- NA
  rep_df$unique_pos_id <- NA
  # ### Loop through the rows of df_log_long
  
  for(line_ind in seq(1,nrow(df_log_long))){
    print(line_ind)
    # if(line_ind == 56){ browser()}
    line <- df_log_long[line_ind,]
    selected_rows <- rep_df[rep_df$time == line$Start & rep_df$PlayerID == line$PlayerID & rep_df$Learned == line$Learned & rep_df$LevelID == line$LevelID& rep_df$StuckID == line$StuckID, ]
    if(nrow(selected_rows) == 0){ next }
    for (i in seq(1, nrow(selected_rows))) {
      if(line$modality == "position"){
        rep_df[rep_df$time == line$Start & rep_df$PlayerID == line$PlayerID & rep_df$Learned == line$Learned & rep_df$LevelID == line$LevelID & rep_df$StuckID == line$StuckID, ][i,]$Position  <- line$Content
        rep_df[rep_df$time == line$Start & rep_df$PlayerID == line$PlayerID & rep_df$Learned == line$Learned & rep_df$LevelID == line$LevelID & rep_df$StuckID == line$StuckID, ][i,]$unique_pos_id  <- line$unique_index
        rep_df[rep_df$time == line$Start & rep_df$PlayerID == line$PlayerID & rep_df$Learned == line$Learned & rep_df$LevelID == line$LevelID & rep_df$StuckID == line$StuckID, ][i,code_cols] <- rep_df[rep_df$time == line$Start & rep_df$PlayerID == line$PlayerID & rep_df$Learned == line$Learned & rep_df$LevelID == line$LevelID & rep_df$StuckID == line$StuckID, ][i,code_cols] + line[,code_cols]
      }
      else{
        rep_df[rep_df$time == line$Start & rep_df$PlayerID == line$PlayerID & rep_df$Learned == line$Learned & rep_df$LevelID == line$LevelID & rep_df$StuckID == line$StuckID, ][i,]$Log  <- line$Content
        rep_df[rep_df$time == line$Start & rep_df$PlayerID == line$PlayerID & rep_df$Learned == line$Learned & rep_df$LevelID == line$LevelID & rep_df$StuckID == line$StuckID, ][i,]$unique_log_id  <- line$unique_index
        rep_df[rep_df$time == line$Start & rep_df$PlayerID == line$PlayerID & rep_df$Learned == line$Learned & rep_df$LevelID == line$LevelID & rep_df$StuckID == line$StuckID, ][i,code_cols] <- rep_df[rep_df$time == line$Start & rep_df$PlayerID == line$PlayerID & rep_df$Learned == line$Learned & rep_df$LevelID == line$LevelID& rep_df$StuckID == line$StuckID, ][i,code_cols] + line[,code_cols]
      }
    }
    
  }
  
  # View(rep_df)
  # colnames(rep_df)
  useful_df <- rep_df[,c("time", "unique_eye_gaze_id", "unique_log_id", "unique_pos_id","PlayerID", "Learned", "Level", "StuckID",
                         "Eye_gaze", "Log", "Position",
                         code_cols)]
  useful_df[is.na(useful_df)] <- ""
  write.csv(useful_df, paste0("data/readable_form/",player,"_readable.csv"))
}
# View(rep_df)




