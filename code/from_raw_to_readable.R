library(readr)
library(tidyr)
library(dplyr)
library(stringr)

# Read data and oncatenate two modes
df_log <- read_csv("data/log.csv")
df_log$modality <- "log"
df_eye <- read_csv("data/eye.csv")
df_eye$modality <- "eye"
df <- rbind(df_log, df_eye)
colnames(df) <- gsub(" ", "", colnames(df))
df
# # Filter out invalid timestamps
# df <- df[df$Start >= 0,]

# Pull numerical values out of strings
df$PlayerID <- as.numeric(sub('Player ','', df$PlayerID)) 
df$StuckID <- as.numeric(sub('Stuck','', df$StuckID)) 
df$LevelID <- ifelse(df$Level == "02 - Now What Is This?", 2, 1)

# Re-coordinate the timestamp so we don't have negative timestamps
### Muhammad 

# Attach a group_ID to each group, a negative boolean to each case (T if neg, F if pos)
df <- df %>%
  ungroup() %>%
  group_by(PlayerID, StuckID, Learned, LevelID) %>%
  mutate(group_ID = cur_group_id(), neg = ifelse(Start < 0 | End < 0, TRUE, FALSE)) 

# Get addends
addends <- df %>%
  filter(neg == TRUE) %>%
  group_by(group_ID) %>%
  summarize(addend = abs(Start), group_ID) %>%
  distinct()
 
# Join addends to df so that all of df is preserved 
df <- left_join(df, addends, by = "group_ID")
  
# Set NA addends to 0, and add addend for each group to each case
df <- df %>%
  mutate(addend = ifelse(is.na(addend), 0, addend)) %>%
  mutate(Start = Start + addend, End = End + addend) %>%
  select(-c(neg, addend)) # groupID removed, uncomment for demo purposes 


### Each session  has a different current (negative) origin value. Figure out a way to set origin to 0 and recalibrate the 
### rest of the timeline 

# Use level one as example
# df <- df[df$Level == "01 - Where Do I Go?", ]


# Sort by playerID, levelID, stuckID, start time
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

