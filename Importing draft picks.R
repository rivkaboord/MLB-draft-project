# MLB DRAFT AND PROSPECT DATA ANALYSIS ----

# load libraries
library(tidyverse)
library(readxl)
library(writexl)
library(stringr)
library(ggplot2)
library(tidyquant)
library(data.table)
library(plotly)
library(Lahman)

## 1. Analyze some data from first round picks since 1965 ----

# 1A. Batch import data from first round of MLB draft ----
first_round_picks_excel_files <- "Draft Excel files/"

file.list <- list.files(path = first_round_picks_excel_files, pattern = "*.xlsx", full.names = T)
df.list <- lapply(file.list, read_excel)
first_round_picks <- rbindlist(df.list, idcol = "id")

# 1B. Clean the imported first round pick data ----
first_round_picks_cleaned_df <- first_round_picks %>% 
    select(-DT, -FrRnd, -"Drafted Out of", -Bonus, -id, -Type) %>% 
    rename(PickNo = RdPck) %>% 
    filter(Rnd == 1) %>%
    mutate(Rnd = str_replace(Rnd,"s","")) %>% 
    mutate(Name = str_remove(Name,"\\s\\(minors\\)"))

# Remove unneeded variables
rm(file.list)
rm(df.list)
rm(first_round_picks)

# 1C. create a new WAR tibble ----
first_round_war_df <- 
    first_round_picks_cleaned_df %>% 
    select(Name, Year, Rnd, PickNo, WAR) %>% 
    arrange(desc(WAR)) %>% 
    mutate_at(., c("WAR"), ~replace(., is.na(.), 0.01))

# 1D. mean and median WAR for each pick number ----
war_avg_by_pick_df <- first_round_war_df %>% 
    select(PickNo, WAR) %>% 
    group_by(PickNo) %>% 
    summarise(WAR_med = median(WAR),
              WAR_mean = mean(WAR)) %>% 
    mutate(label_text = str_glue("Pick: {PickNo}
                                 WAR: {WAR_med %>% round()}")) %>% 
    ungroup()

# 1E. lineplot depicting the average WAR for each pick number ----
war_med_lineplot <- war_avg_by_pick_df %>% 
    
    ggplot(aes(PickNo, WAR_med)) +
    geom_line(size = 1) +
    geom_point(aes(text = label_text), color = "#1c8e20", size = 3) +
    
    theme_tq() +
  
    labs(title = "Average WAR Produced By Each Draft Position", align = "center",
         subtitle = "A steep drop from Pick 1 to Pick 3 and a consistent dropoff from there", 
        x = "Pick",
        y = "Median WAR Produced")

# 1F. interactive lineplot ----
war_med_lineplot_interactive <- 
  war_med_lineplot %>% ggplotly(tooltip = "text")

## 2. Analyze Baseball America prospect data from 1990-2020 ----

# 2A. read in Excel files for prospect list ----
path = "BA Prospects 1990-2021.xlsx"
prospect_data_df <- excel_sheets(path = path) %>% 
    map(~ as.data.frame(read_excel(path, sheet = .,
                                   col_types = c("numeric", "text", "guess", "text", "text", "text", "text", "text",
                                                 "text", "date", "text", "text", "text", "text", "text", "text"))))

# 2B. function to pull and clean data from big list ----
pull_df_fn <- function(data, year) {
    data %>%
        filter_all(any_vars(!is.na(.))) %>% 
        mutate(year = year) %>% 
        select(year, everything())
}

# 2C. combine data into one large dataframe ----
combine_prospect_df <- map2_df(prospect_data_df, 1990:2020, pull_df_fn) %>% 
    separate(col = `Draft Info`, into = c("year_picked", "round_picked", "pickNo", "team_picked"), sep = "-") %>%
    rename("player_name" = `Player Name`,
           "team_name" = `Team Name`,
           "team_rank" = `Team Rank`,
           "born_date" = `Born Date`,
           "birthplace" = `Place Of Birth`,
           "mlb_years" = `MLB Years`,
           "stat_years" = `Stat Years`,
           "other_rankings" = `Other Rankings this Year`) %>% 
    mutate(team_rank = as.numeric(team_rank)) %>% suppressWarnings() %>% 
    mutate(player_name = as.character(player_name))

combine_prospect_df_cleaned <- combine_prospect_df %>%
    select(-Ht, -Wt, -Ba, -Th, -birthplace, -mlb_years, -stat_years, -other_rankings) %>% 
    separate(col = `born_date`, into = c("birthYear", "birthMonth", "birthDay"), sep = "-") %>% 
    mutate("birthDay" = as.integer(birthDay),
           "birthMonth" = as.integer(birthMonth),
           "birthYear" = as.integer(birthYear))

# 2D. making sure we have players' full names and IDs to make it easier to join to prospect data ----
names_and_ids_df <- People %>% 
    unite(nameFirst, nameLast, col = "player_name", sep = " ") %>% 
    select(playerID, player_name, birthYear, birthMonth, birthDay, everything())

# 2E. join prospect data with their player IDs (+ height/weight and batting info in R-friendly format) ----
prospects_majors_since_1990_df <- combine_prospect_df_cleaned %>% 
    left_join(names_and_ids_df, c("player_name", "birthMonth", "birthYear")) %>% 
    select(year, playerID, player_name, Rk, team_name, team_rank, Pos, year_picked, round_picked, pickNo, team_picked, height, weight, bats, throws)

## 3. Analyze career WAR of top prospects since 1990 by combining prospects' names with their career stats ----

# 3A. adding WAR batting statistics to the Lahman database and then to batting prospects ----
lahman_batters_with_war_df <- read.csv("lahman_db_with_bbrefid_batters.csv") %>% 
    rename("playerID" = player_ID) %>% 
    select(playerID, WAR) %>% 
    group_by(playerID) %>% 
    summarize(career_WAR = round(sum(WAR), 1))

prospects_1990_war_batters_df <- prospects_majors_since_1990_df %>% 
    inner_join(lahman_batters_with_war_df, c("playerID")) %>% 
    filter(Pos != "P")

# 3B. adding WAR pitching statistics to Lahman database and then to pitching prospects ----
lahman_pitchers_with_war_df <- read.csv("lahman_db_with_bbrefid_pitchers.csv") %>% 
  select(playerID, WAR) %>% 
  group_by(playerID) %>% 
  summarize(career_WAR = round(sum(WAR), 1))

prospects_1990_war_pitchers_df <- prospects_majors_since_1990_df %>% 
  inner_join(lahman_pitchers_with_war_df, c("playerID")) %>% 
  filter(Pos == "P")

# 3C. filtering out prospects who never made the majors ----
never_made_majors_1990_df <- prospects_majors_since_1990_df %>% 
  filter(is.na(playerID))

# 3D. join together batters and pitchers ----
prospects_1990_war_full_df <- prospects_1990_war_batters_df %>% 
  full_join(prospects_1990_war_pitchers_df) %>% 
  mutate(Rk = as.factor(Rk)) %>% 
  arrange(year, Rk)

# 3E. aggregate mean, median, stdev, and var of WAR ----
prospects_averages_df <- prospects_1990_war_full_df %>% 
  select(Rk, career_WAR) %>% 
  mutate(career_WAR = na.fill0(career_WAR, fill = 0)) %>% 
  group_by(Rk) %>% 
  summarize(sum_war = round(sum(career_WAR), 2),
            mean_war = round(mean(career_WAR), 2),
            med_war = round(median(career_WAR), 2),
            var_war = round(var(career_WAR), 2),
            stdv_war = round(StdDev(career_WAR), 2)
            ) %>% 
  ungroup()

# 3F. scatterplot of median WAR data for prospects ----
top_prospects_career_war_scatterplot <- prospects_averages_df %>% 
  select(Rk, med_war) %>% 
  ggplot(aes(Rk, med_war)) +
  geom_point() +
  theme_tq() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Top Prospect Position and Career WAR",
       x = "Prospect Ranking",
       y = "Median Career WAR") +
  theme(title = element_text(face = "bold"))

# 3G. Kmeans and UMAP ----

# kmeans_obj <- prospects_averages_df %>% 
#   select(med_war) %>% 
#   kmeans(centers = 4, nstart = 100)
# 
# broom::tidy(kmeans_obj) %>% 
#   glimpse()
# 
# broom::augment(kmeans_obj, prospects_averages_df) %>% 
#   select(Rk, .cluster)
# 
# center <- 3
# 
# kmeans_mapper <- function(centers = 3) {
#   
#   prospects_averages_df %>% 
#     select(med_war) %>% 
#     kmeans(centers = center, nstart = 100)
# }
# 
# kmeans_mapped_tbl <- tibble(centers = 1:15) %>% 
#   mutate(k_means = centers %>% map(kmeans_mapper)) %>% 
#   mutate(glance = k_means %>% map(broom::glance))
# 
# 
# kmeans_mapped_tbl %>% 
#   unnest(glance) %>% 
#   select(centers, tot.withinss)
# 
# kmeans_mapped_tbl %>% 
#   unnest(glance) %>% 
#   select(centers, tot.withinss) %>% 
#   
#   # Visualization
#   
#   ggplot(aes(centers, tot.withinss)) +
#   geom_point(color = "#2c3e50", size = 4) +
#   geom_line(color = "#2c3e50", size = 1) +
#   ggrepel::geom_label_repel(aes(label = centers), color = "#2c3e50") +
#   
#   # Formatting
#   theme_tq()
# 
# library(umap)
# 
# umap_obj <- prospects_averages_df %>% 
#   select(med_war) %>% 
#   umap()
# 
# umap_results_tbl <- umap_obj$layout %>% 
#   as_tibble() %>% 
#   set_names(c("x", "y")) %>% 
#   bind_cols(
#     prospects_averages_df %>% select(Rk)
#   )
# 
# umap_results_tbl %>% 
#   
#   ggplot(aes(x,y)) +
#   geom_point() +
#   geom_label_repel(aes(label = Rk), size = 2.5)

## 4. Possible analysis of all players to compare to top prospects; should remove top prospects from data ----

# 4A. mean of all players in MLB history WAR ----
lahman_all_players_war_df <- lahman_batters_with_war_df %>% 
  full_join(lahman_pitchers_with_war_df) %>% 
  na.omit(career_WAR)

lahman_all_players_war_df$career_WAR %>% mean() %>% round(digits = 1)

lahman_all_players_war_df %>% 
  arrange(desc(career_WAR))

## 5. Comparing top prospects' career data to All Stars' - in other words, a proven player ----

# 5A. All Stars' mean and median career WAR
all_all_star_app_df <- Lahman::AllstarFull %>% 
  group_by(playerID) %>% 
  count() %>% 
  inner_join(lahman_all_players_war_df, c("playerID")) %>% 
  na.omit(career_WAR)

all_all_star_app_df$career_WAR %>% median() %>% round(1)