# Join all tibbles into master

library(magrittr)

top_30_picks <- 
    pick1 %>% 
    full_join(pick2) %>% full_join(pick3) %>% full_join(pick4) %>% full_join(pick5) %>% 
    full_join(pick6) %>% full_join(pick7) %>% full_join(pick8) %>% full_join(pick9) %>% 
    full_join(pick10) %>% full_join(pick11) %>% full_join(pick12) %>% full_join(pick13) %>% 
    full_join(pick14) %>% full_join(pick15) %>% full_join(pick16) %>% full_join(pick17) %>% 
    full_join(pick18) %>% full_join(pick19) %>% full_join(pick20) %>% full_join(pick21) %>% 
    full_join(pick22) %>% full_join(pick23) %>% full_join(pick24) %>% full_join(pick25) %>% 
    full_join(pick26) %>% full_join(pick27) %>% full_join(pick28) %>% full_join(pick29) %>% 
    full_join(pick30)

# Create a new WAR tibble

first_round_war <- 
    top_30_picks %>% 
    select(Name,
           Year,
           Rnd,
           PickNo,
           WAR
    )

# Order by WAR, descending

library(tidyverse)

first_round_war <- 
    first_round_war %>% 
    arrange(-WAR)

mean_first_round_war_vector <- 
    c(mean(pick1$WAR, na.rm = TRUE), mean(pick2$WAR, na.rm = TRUE),
      mean(pick3$WAR, na.rm = TRUE), mean(pick4$WAR, na.rm = TRUE),
      mean(pick5$WAR, na.rm = TRUE), mean(pick6$WAR, na.rm = TRUE),
      mean(pick7$WAR, na.rm = TRUE), mean(pick8$WAR, na.rm = TRUE),
      mean(pick9$WAR, na.rm = TRUE), mean(pick10$WAR, na.rm = TRUE),
      mean(pick11$WAR, na.rm = TRUE), mean(pick12$WAR, na.rm = TRUE),
      mean(pick13$WAR, na.rm = TRUE), mean(pick14$WAR, na.rm = TRUE),
      mean(pick15$WAR, na.rm = TRUE), mean(pick16$WAR, na.rm = TRUE),
      mean(pick17$WAR, na.rm = TRUE), mean(pick18$WAR, na.rm = TRUE),
      mean(pick19$WAR, na.rm = TRUE), mean(pick20$WAR, na.rm = TRUE),
      mean(pick21$WAR, na.rm = TRUE), mean(pick22$WAR, na.rm = TRUE),
      mean(pick23$WAR, na.rm = TRUE), mean(pick24$WAR, na.rm = TRUE),
      mean(pick25$WAR, na.rm = TRUE), mean(pick26$WAR, na.rm = TRUE),
      mean(pick27$WAR, na.rm = TRUE), mean(pick28$WAR, na.rm = TRUE),
      mean(pick29$WAR, na.rm = TRUE), mean(pick30$WAR, na.rm = TRUE),
      mean(pick31$WAR, na.rm = TRUE), mean(pick32$WAR, na.rm = TRUE),
      mean(pick33$WAR, na.rm = TRUE))

median_war_vector <- 
    c(
        median(pick1$WAR, na.rm = TRUE), median(pick2$WAR, na.rm = TRUE),
        median(pick3$WAR, na.rm = TRUE), median(pick4$WAR, na.rm = TRUE),
        median(pick5$WAR, na.rm = TRUE), median(pick6$WAR, na.rm = TRUE),
        median(pick7$WAR, na.rm = TRUE), median(pick8$WAR, na.rm = TRUE),
        median(pick9$WAR, na.rm = TRUE), median(pick10$WAR, na.rm = TRUE),
        median(pick11$WAR, na.rm = TRUE), median(pick12$WAR, na.rm = TRUE),
        median(pick13$WAR, na.rm = TRUE), median(pick14$WAR, na.rm = TRUE),
        median(pick15$WAR, na.rm = TRUE), median(pick16$WAR, na.rm = TRUE),
        median(pick17$WAR, na.rm = TRUE), median(pick18$WAR, na.rm = TRUE),
        median(pick19$WAR, na.rm = TRUE), median(pick20$WAR, na.rm = TRUE),
        median(pick21$WAR, na.rm = TRUE), median(pick22$WAR, na.rm = TRUE),
        median(pick23$WAR, na.rm = TRUE), median(pick24$WAR, na.rm = TRUE),
        median(pick25$WAR, na.rm = TRUE), median(pick26$WAR, na.rm = TRUE),
        median(pick27$WAR, na.rm = TRUE), median(pick28$WAR, na.rm = TRUE),
        median(pick29$WAR, na.rm = TRUE), median(pick30$WAR, na.rm = TRUE),
        median(pick31$WAR, na.rm = TRUE), median(pick32$WAR, na.rm = TRUE),
        median(pick33$WAR, na.rm = TRUE))

draft_position_vector <- c(1:33)

total_non_major_leaguers_by_draft_position <-
    c(
        sum(is.na(pick1$WAR)), sum(is.na(pick2$WAR)), sum(is.na(pick3$WAR)),
        sum(is.na(pick4$WAR)), sum(is.na(pick5$WAR)), sum(is.na(pick6$WAR)),
        sum(is.na(pick7$WAR)), sum(is.na(pick8$WAR)), sum(is.na(pick9$WAR)),
        sum(is.na(pick10$WAR)), sum(is.na(pick11$WAR)), sum(is.na(pick12$WAR)),
        sum(is.na(pick13$WAR)), sum(is.na(pick14$WAR)), sum(is.na(pick15$WAR)),
        sum(is.na(pick16$WAR)), sum(is.na(pick17$WAR)), sum(is.na(pick18$WAR)),
        sum(is.na(pick19$WAR)), sum(is.na(pick20$WAR)), sum(is.na(pick21$WAR)),
        sum(is.na(pick22$WAR)), sum(is.na(pick23$WAR)), sum(is.na(pick24$WAR)),
        sum(is.na(pick25$WAR)), sum(is.na(pick26$WAR)), sum(is.na(pick27$WAR)),
        sum(is.na(pick28$WAR)), sum(is.na(pick29$WAR)), sum(is.na(pick30$WAR)),
        sum(is.na(pick31$WAR)), sum(is.na(pick32$WAR)), sum(is.na(pick33$WAR))
    )

pct_non_major_leaguers <- 
    total_non_major_leaguers_by_draft_position/56 * 100

first_round_mean_war_by_position <- 
    matrix(data = draft_position_vector) %>% 
    cbind(mean_first_round_war_vector) %>% 
    cbind(median_war_vector) %>% 
    cbind(total_non_major_leaguers_by_draft_position) %>% 
    cbind(pct_non_major_leaguers)