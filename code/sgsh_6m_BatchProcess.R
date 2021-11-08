# this script was obtained from https://github.com/clay-j/ZANTIKS_YMaze_Analysis_Script
# BEFORE YOU START --------------------------------------------------------
# Set input and output to the correct paths in the "YMazeAnalysisScript" directory

library(tidyverse)
library(here)
library(lubridate)

input <- here("data/raw_data/sgsh/6m")
output <- here("data/processed_data/sgsh/6m")


# IMPORT DATA -------------------------------------------------------------
setwd(input)
file_list <- list.files(pattern = "*.csv", recursive = TRUE) #Create a list of files from ./data directory.

df <-
  tibble(file_list = file_list) %>% #Import data as a tibble with nested lists
  mutate(tail = map(file_list, ~ read_csv(.x, col_names = F, skip = 4)), #tail = data
         head = map(file_list, ~ read_csv(.x, col_names = F, n_max = 4))) # head = demographic info

df[[3]] <- #Convert data nested in "head" into a usable format
  df[[3]] %>%
  map(~ mutate(.x, X1 = "000:00:00.000")) %>% # fix some of the headers which for some reason were not showing this.
  map(~ spread(.x, X3, X4)) %>%
  map(~ separate(.x,"Subject Identification", c("fish1", "fish2"))) %>%
  map(~ dplyr::select(.x, X1,X2, "Apparatus", "fish1","fish2", "Unit ID")) %>%
  map(~ dplyr::rename(.x, X1_head = X1, X2_head = X2))

df[[2]] <- df[[2]] %>% map(~ mutate(.x, X4 = as.character(X4))) # ensure that "X4" is character because unnest() can't handle mixed data types in list

df <- #convert df from a tibble w/ nested lists to a basic tibble
  df %>%
  unnest(head) %>%
  unnest(tail)

# DATA WANGLING -----------------------------------------------------------
## STEP 1 - select variables that we are intrested in
df <-
  df %>%
  dplyr::select(file_list, Apparatus, `Unit ID`,X1, X4, X5, X6, fish1, fish2) %>% #select the vars we're intrested in
  rename(file_id = file_list, #renames vars
         apparatus = Apparatus,
         unit_id = `Unit ID`,
         time = X1,
         arena = X4)

## STEP 2 - convert to tidy data
df <- na.omit(df) #remove na values
df$time <- hms(df$time) #fixing time varible

df <-
  df %>%
  mutate(i = row_number()) %>%
  spread(X5, X6) %>% #create separate columns for "Enter_Zone" and "Exit_Zone"
  rename(enter_zone = Enter_Zone, exit_zone = Exit_Zone)

## STEP 3 - create "fish_id" column and arrange in chronological order
df <-
  df %>%
  mutate(fish_id = ifelse(arena == 1, fish1,
                          ifelse(arena == 2, fish2,
                                 NA))) %>%
  dplyr::select(file_id, apparatus, unit_id, time, arena, fish_id, enter_zone, exit_zone)

## STEP 4 - export for backup/later use
setwd(output)
write.csv(df, "tidy_data.csv")

# ANALYSIS ----------------------------------------------------------------
df_a <- df #create a new tibble to analyse

## STEP 1 - create 10 min bins
df_a <-
  df_a %>%
  mutate(elapsed_secs = seconds(df_a$time)) %>%  #create an elapse_seconds column to bin from
  mutate(bin = ifelse(elapsed_secs >0 & elapsed_secs <600, 1,     #bin1
                      ifelse(elapsed_secs >600 & elapsed_secs <1200, 2,  #bin2
                             ifelse(elapsed_secs >1200 & elapsed_secs <1800, 3, #bin3
                                    ifelse(elapsed_secs >1800 & elapsed_secs <2400, 4, #bin4
                                           ifelse(elapsed_secs >2400 & elapsed_secs <3000, 5, #bin5
                                                  ifelse(elapsed_secs >3000 & elapsed_secs <6000, 6, #bin6
                                                         NA)))))))                                   #NA

## STEP 2 - Prepare data ready to figure out which way the fish turned
df_a$time <- as.numeric(df_a$time) #time variables are not compatible with ddplr
df_a$elapsed_secs <- as.numeric(df_a$elapsed_secs)
df_a <- df_a %>% arrange(file_id,fish_id,elapsed_secs, exit_zone)  # order by fish_id and tie break by elepsed_secs
# and then exit_zone so line 1 = enter, line 2 = exit
#Create new variables ready to make new data tibble
df_a <- df_a %>%
  mutate(zone = ifelse(enter_zone == lead(exit_zone), enter_zone, 999)) %>%
  mutate(time_enter = ifelse(enter_zone >= 1, elapsed_secs, 999)) %>%
  mutate(time_exit = ifelse(exit_zone >= 1, elapsed_secs, 999))

#ERROR CHECK - if any rows == 999 then something has gone wronge with the previous line of code.
df_a %>% filter(zone == 999)
df_a %>% filter(time_enter == 999)
df_a %>% filter(time_enter == 999)

#Wrangle data to figure out which way the fish turned
df_a <- df_a %>% dplyr::select(file_id, apparatus, unit_id, arena, fish_id, bin, zone, time_enter, time_exit) %>%
  mutate(time_exit = lead(time_exit)) #create one row per zone

df_a <- df_a %>% na.omit() #remove empty/pointless rows
df_a <- df_a %>% mutate(time_in_zone = time_exit - time_enter) #calculate time spent in each zone

## STEP 3 - Export for backup/later use
setwd(output)
write.csv(df_a, "time_in_zone.csv")

##STEP 4 - Which way did the fish turn?
df_list <- df_a %>% filter(zone != 4) #remove centre zone (i.e. zone 4)
split_tibble <- function(tibble, col = "col") tibble %>% split(., .[,col]) #function to split tibble into list
df_list <- split_tibble(df_list, "fish_id") #split tibble into list

df_list <- df_list %>%
  map(~ mutate(.x, lag_zone = lag(zone))) %>% #calculate direction turned for each fish
  map(~ mutate(.x, turn=case_when(lag_zone==1 & zone==2 ~ 'L',
                                  lag_zone==1 & zone==3 ~ 'R',
                                  lag_zone==2 & zone==1 ~ 'R',
                                  lag_zone==2 & zone==3 ~ 'L',
                                  lag_zone==3 & zone==1 ~ 'L',
                                  lag_zone==3 & zone==2 ~ 'R',
                                  TRUE~ NA_character_ ))) %>%
  map(~ dplyr::select(.x, file_id, apparatus, unit_id, arena, fish_id, bin, zone, turn)) #select rows that we are intrested in

df_b <- bind_rows(df_list) #merge list into single dataframe
df_b <- df_b %>%
  arrange(file_id, fish_id, bin) %>% #restore previous order(i.e. by fish_id)
  na.omit() #remove NA rows

## STEP 5 - Tetragrams
df_c <- df_b %>%
  group_by(fish_id, bin) %>% #create groups for tetragrams
  dplyr::select(-arena, -zone) #remove arena and zone columns as they are no longer needed

df_c <- df_c %>%
  mutate(tetragrams = str_c(turn, #create tetragrams column
                            lead(turn),
                            lead(turn,2),
                            lead(turn,3))) %>%
  ungroup() %>%
  dplyr::select(fish_id, bin, turn, tetragrams) #delete unnecessary columns


## STEP 6 - Summarise
unique_tetragrams <- unique(df_c$tetragrams)
## Tetragrams
tet_long <- df_c %>%
  dplyr::select(-turn) %>%
  na.omit() %>%
  group_by(fish_id, bin) %>%
  table() %>%
  as.tibble() %>%
  arrange(fish_id, bin)

tet_wide <- tet_long %>% spread(tetragrams, n)

## Turns
turn_long <- df_c %>%
  dplyr::select(-tetragrams) %>%
  na.omit() %>%
  group_by(fish_id, bin) %>%
  table() %>%
  as.tibble() %>%
  arrange(fish_id, bin)

turn_wide <- turn_long %>% spread(turn, n)

## Final tibble
final_data <- tet_wide %>%
  left_join(turn_wide, by = c("fish_id","bin")) %>%
  mutate(total_turns = L + R,
         reps = LLLL + RRRR,
         alts = RLRL + LRLR,
         rel_reps = (reps*100)/total_turns,
         rel_alts = (alts*100)/total_turns,
         rel_R = (R*100)/total_turns,
         rel_L = (L*100)/total_turns)

## STEP 7 - Final Output
setwd(output)
write.csv(final_data, "final_output.csv")

# Set the wd back to the project home dir
setwd(here())

