library(readxl)
library(writexl)

#importing data on each pick in the first round of the MLB draft

pick1 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick1.xlsx')
pick2 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick2.xlsx')
pick3 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick3.xlsx')
pick4 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick4.xlsx')
pick5 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick5.xlsx')
pick6 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick6.xlsx')
pick7 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick7.xlsx')
pick8 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick8.xlsx')
pick9 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick9.xlsx')
pick10 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick10.xlsx')

library(dplyr)

#Cleaning data to remove extraneous columns using select

pick1 <- select(pick1, -DT, -FrRnd, -RdPck, -Bonus, -Type)
pick2 <- select(pick2, -DT, -FrRnd, -RdPck, -Bonus, -Type)
pick3 <- select(pick3, -DT, -FrRnd, -RdPck, -Bonus, -Type)
pick4 <- select(pick4, -DT, -FrRnd, -RdPck, -Bonus, -Type)
pick5 <- select(pick5, -DT, -FrRnd, -RdPck, -Bonus, -Type)
pick6 <- select(pick6, -DT, -FrRnd, -RdPck, -Bonus, -Type)
pick7 <- select(pick7, -DT, -FrRnd, -RdPck, -Bonus, -Type)
pick8 <- select(pick8, -DT, -FrRnd, -RdPck, -Bonus, -Type)
pick9 <- select(pick9, -DT, -FrRnd, -RdPck, -Bonus, -Type)
pick10 <- select(pick10, -DT, -FrRnd, -RdPck, -Bonus, -Type)

pick1 <- select(pick1, -"Drafted Out of")
pick2 <- select(pick2, -"Drafted Out of")
pick3 <- select(pick3, -"Drafted Out of")
pick4 <- select(pick4, -"Drafted Out of")
pick5 <- select(pick5, -"Drafted Out of")
pick6 <- select(pick6, -"Drafted Out of")
pick7 <- select(pick7, -"Drafted Out of")
pick8 <- select(pick8, -"Drafted Out of")
pick9 <- select(pick9, -"Drafted Out of")
pick10 <- select(pick10, -"Drafted Out of")

# Remove the (minors) part of the "Name" column

library(tidyr)
library(stringr)

pick1$Name <- pick1$Name %>% 
    str_remove("\\s\\(minors\\)")
pick2$Name <- pick2$Name %>% 
    str_remove("\\s\\(minors\\)")
pick3$Name <- pick3$Name %>% 
    str_remove("\\s\\(minors\\)")
pick4$Name <- pick4$Name %>% 
    str_remove("\\s\\(minors\\)")
pick5$Name <- pick5$Name %>% 
    str_remove("\\s\\(minors\\)")
pick6$Name <- pick6$Name %>% 
    str_remove("\\s\\(minors\\)")
pick7$Name <- pick7$Name %>% 
    str_remove("\\s\\(minors\\)")
pick8$Name <- pick8$Name %>% 
    str_remove("\\s\\(minors\\)")
pick9$Name <- pick9$Name %>% 
    str_remove("\\s\\(minors\\)")
pick10$Name <- pick10$Name %>% 
    str_remove("\\s\\(minors\\)")

# Add a column to show what number pick each prospect was

library(tibble)

pick1 <- pick1 %>% 
    add_column("Pick" = 1, .after = "Rnd")
pick2 <- pick2 %>% 
    add_column("Pick" = 2, .after = "Rnd")
pick3 <- pick3 %>% 
    add_column("Pick" = 3, .after = "Rnd")
pick4 <- pick4 %>% 
    add_column("Pick" = 4, .after = "Rnd")
pick5 <- pick5 %>% 
    add_column("Pick" = 5, .after = "Rnd")
pick6 <- pick6 %>% 
    add_column("Pick" = 6, .after = "Rnd")
pick7 <- pick7 %>% 
    add_column("Pick" = 7, .after = "Rnd")
pick8 <- pick8 %>% 
    add_column("Pick" = 8, .after = "Rnd")
pick9 <- pick9 %>% 
    add_column("Pick" = 9, .after = "Rnd")
pick10 <- pick10 %>% 
    add_column("Pick" = 10, .after = "Rnd")

# Clean a couple of columns with erroneous NAs
pick7$Rnd = 1
pick10$Rnd = 1

# Join all tibbles into master

library(magrittr)

top_10_picks <- 
    pick1 %>% 
    full_join(pick2) %>% 
    full_join(pick3) %>% 
    full_join(pick4) %>% 
    full_join(pick5) %>% 
    full_join(pick6) %>% 
    full_join(pick7) %>% 
    full_join(pick8) %>% 
    full_join(pick9) %>% 
    full_join(pick10)

# Create a new WAR tibble

first_round_war <- 
    top_10_picks %>% 
    select(Name,
           Year,
           Rnd,
           Pick,
           WAR)