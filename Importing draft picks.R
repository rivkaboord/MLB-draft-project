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
pick11 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick11.xlsx')
pick12 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick12.xlsx')
pick13 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick13.xlsx')
pick14 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick14.xlsx')
pick15 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick15.xlsx')
pick16 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick16.xlsx')
pick17 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick17.xlsx')
pick18 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick18.xlsx')
pick19 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick19.xlsx')
pick20 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick20.xlsx')
pick21 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick21.xlsx')
pick22 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick22.xlsx')
pick23 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick23.xlsx')
pick24 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick24.xlsx')
pick25 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick25.xlsx')
pick26 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick26.xlsx')
pick27 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick27.xlsx')
pick28 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick28.xlsx')
pick29 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick29.xlsx')
pick30 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick30.xlsx')
pick31 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick31.xlsx')
pick32 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick32.xlsx')
pick33 <- read_xlsx(path = '~/Programming and Data Science/R/MLB Draft/Draft Excel Files/pick33.xlsx')

library(dplyr)
library(magrittr)

#Cleaning data to remove extraneous columns using select and rename RdPck column for clarity

pick1 <- select(pick1, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick2 <- select(pick2, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick3 <- select(pick3, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick4 <- select(pick4, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick5 <- select(pick5, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick6 <- select(pick6, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick7 <- select(pick7, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick8 <- select(pick8, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick9 <- select(pick9, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick10 <- select(pick10, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick11 <- select(pick11, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick12 <- select(pick12, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick13 <- select(pick13, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick14 <- select(pick14, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick15 <- select(pick15, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick16 <- select(pick16, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick17 <- select(pick17, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick18 <- select(pick18, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick19 <- select(pick19, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick20 <- select(pick20, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick21 <- select(pick21, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick22 <- select(pick22, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick23 <- select(pick23, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick24 <- select(pick24, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick25 <- select(pick25, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick26 <- select(pick26, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick27 <- select(pick27, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick28 <- select(pick28, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick29 <- select(pick29, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick30 <- select(pick30, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick31 <- select(pick31, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick32 <- select(pick32, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)
pick33 <- select(pick33, -DT, -FrRnd, -Bonus, -Type, -"Drafted Out of") %>% 
    rename(PickNo = RdPck)

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
pick11$Name <- pick11$Name %>% 
    str_remove("\\s\\(minors\\)")
pick12$Name <- pick12$Name %>% 
    str_remove("\\s\\(minors\\)")
pick13$Name <- pick13$Name %>% 
    str_remove("\\s\\(minors\\)")
pick14$Name <- pick14$Name %>% 
    str_remove("\\s\\(minors\\)")
pick15$Name <- pick15$Name %>% 
    str_remove("\\s\\(minors\\)")
pick16$Name <- pick16$Name %>% 
    str_remove("\\s\\(minors\\)")
pick17$Name <- pick17$Name %>% 
    str_remove("\\s\\(minors\\)")
pick18$Name <- pick18$Name %>% 
    str_remove("\\s\\(minors\\)")
pick19$Name <- pick19$Name %>% 
    str_remove("\\s\\(minors\\)")
pick20$Name <- pick20$Name %>% 
    str_remove("\\s\\(minors\\)")
pick21$Name <- pick21$Name %>% 
    str_remove("\\s\\(minors\\)")
pick22$Name <- pick22$Name %>% 
    str_remove("\\s\\(minors\\)")
pick23$Name <- pick23$Name %>% 
    str_remove("\\s\\(minors\\)")
pick24$Name <- pick24$Name %>% 
    str_remove("\\s\\(minors\\)")
pick25$Name <- pick25$Name %>% 
    str_remove("\\s\\(minors\\)")
pick26$Name <- pick26$Name %>% 
    str_remove("\\s\\(minors\\)")
pick27$Name <- pick27$Name %>% 
    str_remove("\\s\\(minors\\)")
pick28$Name <- pick28$Name %>% 
    str_remove("\\s\\(minors\\)")
pick29$Name <- pick29$Name %>% 
    str_remove("\\s\\(minors\\)")
pick30$Name <- pick30$Name %>% 
    str_remove("\\s\\(minors\\)")
pick31$Name <- pick31$Name %>% 
    str_remove("\\s\\(minors\\)")
pick32$Name <- pick32$Name %>% 
    str_remove("\\s\\(minors\\)")
pick33$Name <- pick33$Name %>% 
    str_remove("\\s\\(minors\\)")

# Clean a couple of columns with erroneous NAs, change column type to numeric
pick7$Rnd = 1
pick10$Rnd = 1
pick11$Rnd = 1
pick12$Rnd = 1
pick13$Rnd = 1
pick14$Rnd = 1
pick15$Rnd = 1
pick16$Rnd = 1
pick17$Rnd = 1
pick18$Rnd = 1
pick19$Rnd = 1
pick20$Rnd = 1
pick27$Rnd = 1
pick28$Rnd = 1
pick29$Rnd = 1
pick30$Rnd = 1
pick31$Rnd = 1
pick32$Rnd <- str_replace(pick32$Rnd,"s","")
as.numeric(pick32$Rnd)

# Remove second-round picks who were picked at 31 or later, since draft position is mixed between Rounds 1 and 2

pick31 <- pick31[pick31$Rnd == 1,]
pick32 <- pick32[pick32$Rnd == 1,]
pick33 <- pick33[pick33$Rnd == 1,]