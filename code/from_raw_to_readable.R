library(readr)
library(tidyr)
library(dplyr)
library(stringr)

# Read data and oncatenate two modes
df_log <- read_csv("data/TMA-LogData-WithFlag-27Sept2022.csv")
df_log$modality <- "log"
df_eye <- read_csv("data/TMA-EyeGaze-WithFlag-27Sept2022.csv")
df_eye$modality <- "eye"
df <- rbind(df_log, df_eye)
colnames(df) <- gsub(" ", "", colnames(df))

# Filter out invalid timestamps
df <- df[df$Start >= 0,]

# Use level one as example
# df <- df[df$Level == "01 - Where Do I Go?", ]

# Sort by playerID, levelID, stuckID, start time
df$PlayerID <- as.numeric(sub('Player ','', df$PlayerID)) 
df$StuckID <- as.numeric(sub('Stuck','', df$StuckID)) 
df$LevelID <- ifelse(df$Level == "02 - Now What Is This?", 2, 1)


df <- df[order(df$PlayerID,df$LevelID, df$StuckID, df$Learned, df$Start),]
# View(df)
colnames(df)
# Replace NA and null with 0. 
code_cols <- c("StartorRestart", "RuleBreak", "Deviation", "PassedBoundary", 
               "Flag", "YouObject", "YouRule", "TextObstacle", "BrokenStop", "OutsideText", "WinText", "Is-Outside", "ObjectObstacle", "OtherObstacle")
df$Flag[df$Flag == 'null'] <- 0
df$Flag <- df$Flag %>% replace(is.na(.), 0)
df$Flag <- as.numeric(df$Flag)
df[,code_cols] <- df[,code_cols] %>% replace(is.na(.), 0)
df$unique_index <- seq(1,nrow(df))
df$Start <- df$Start/1000
df$End <- df$End/1000
df <- df %>% drop_na(PlayerID)
write.csv(df, "data/combined.csv")

