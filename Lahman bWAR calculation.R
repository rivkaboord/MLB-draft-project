library(dplyr)
library(baseballr)
library(Lahman)
library(readxl)

## Batting WAR addition ----

#Write the download to a data frame
df <- read.csv(file = "war_daily_bat.csv")

master <- Master %>% select(playerID, bbrefID)
teams <- Teams %>% select(yearID, teamID, teamIDBR)

df2 <- left_join(df, master, by = c("player_ID" = "playerID")) %>% 
  select(player_ID, bbrefID, everything())

teams$teamIDBR <- as.factor(teams$teamIDBR)

names(teams)[names(teams)=="teamIDBR"] <- "team_ID"
names(teams)[names(teams)=="yearID"] <- "year_ID"

df3 <- left_join(df2, teams)

final <- subset(df3, select = c(player_ID, year_ID, age, team_ID, stint_ID, lg_ID, PA, G, 
                                Inn, runs_bat, runs_br, runs_dp, runs_field, runs_infield, 
                                runs_outfield, runs_catcher, runs_good_plays, runs_defense, 
                                runs_position, runs_position_p, runs_replacement, runs_above_rep, 
                                runs_above_avg, runs_above_avg_off, runs_above_avg_def, WAA, WAA_off, 
                                WAA_def, WAR, WAR_def, WAR_off, WAR_rep, salary, pitcher, teamRpG, 
                                oppRpG, oppRpPA_rep, oppRpG_rep, pyth_exponent, pyth_exponent_rep, 
                                waa_win_perc, waa_win_perc_off, waa_win_perc_def, waa_win_perc_rep))


names(final)[names(final)=="stint_ID"] <- "stint"
names(final)[names(final)=="lg_ID"] <- "lgID"
names(final)[names(final)=="year_ID"] <- "yearID"
names(final)[names(final)=="team_ID"] <- "teamID"


final$age <- as.integer(as.character(final$age))
final$stint <- as.integer(as.character(final$stint))
final$PA <- as.integer(as.character(final$PA))
final$G <- as.integer(as.character(final$G))
final$Inn <- as.integer(as.character(final$Inn))
final$runs_bat <- as.double(as.character(final$runs_bat))
final$runs_br <- as.double(as.character(final$runs_br))
final$runs_dp <- as.double(as.character(final$runs_dp))
final$runs_field <- as.double(as.character(final$runs_field))
final$runs_infield <- as.double(as.character(final$runs_infield))
final$runs_outfield <- as.double(as.character(final$runs_outfield))
final$runs_catcher <- as.double(as.character(final$runs_catcher))
final$runs_good_plays <- as.double(as.character(final$runs_good_plays))
final$runs_position <- as.double(as.character(final$runs_position))
final$runs_replacement <- as.double(as.character(final$runs_replacement))
final$runs_above_rep <- as.double(as.character(final$runs_above_rep))
final$runs_above_avg <- as.double(as.character(final$runs_above_avg))
final$runs_above_avg_off <- as.double(as.character(final$runs_above_avg_off))
final$runs_above_avg_def <- as.double(as.character(final$runs_above_avg_def))
final$WAA <- as.double(as.character(final$WAA))
final$WAA_off <- as.double(as.character(final$WAA_off))
final$WAA_def <- as.double(as.character(final$WAA_def))
final$WAR <- as.double(as.character(final$WAR))
final$WAR_def <- as.double(as.character(final$WAR_def))
final$WAR_off <- as.double(as.character(final$WAR_off))
final$WAR_rep <- as.double(as.character(final$WAR_rep))
final$salary <- as.integer(as.character(final$salary))
final$teamrRpG <- as.double(as.character(final$teamRpG))
final$oppRpG <- as.double(as.character(final$oppRpG))
final$oppRpPA_rep <- as.double(as.character(final$oppRpPA_rep))
final$oppRpG_rep <- as.double(as.character(final$oppRpG_rep))
final$pyth_exponent <- as.double(as.character(final$pyth_exponent))
final$pyth_exponent_rep <- as.double(as.character(final$pyth_exponent_rep))
final$waa_win_perc <- as.double(as.character(final$waa_win_perc))
final$waa_win_perc_off <- as.double(as.character(final$waa_win_perc_off))
final$waa_win_perc_def <- as.double(as.character(final$waa_win_perc_def))
final$waa_win_perc_rep <- as.double(as.character(final$waa_win_perc_rep))

write.csv(x = final, file = "lahman_db_with_bbrefid_batters.csv")

## Pitching WAR Addition ----

dfpitching <- read.csv("war_daily_pitch.csv") %>% 
    rename("playerID" = player_ID)

master_pitching <- Master %>% select(playerID, bbrefID)
teams_pitching <- Teams %>% select(yearID, teamID, teamIDBR)

df2pitching <- left_join(dfpitching, master_pitching, by = c("playerID" = "bbrefID"))

names(teams_pitching)[names(teams_pitching)=="teamIDBR"] <- "team_ID"
names(teams_pitching)[names(teams_pitching)=="yearID"] <- "year_ID"

#Now we index the teams
df3pitching <- left_join(df2pitching, teams_pitching)

#Reorder data frame
final_pitching <- subset(df3pitching, select = c(playerID,  year_ID, age, team_ID, stint_ID, lg_ID, G, GS, IPouts, IPouts_start, 
                                IPouts_relief, RA, xRA, xRA_sprp_adj, xRA_def_pitcher, PPF, PPF_custom, xRA_final, 
                                BIP, BIP_perc, RS_def_total, runs_above_avg, runs_above_avg_adj, runs_above_rep, 
                                RpO_replacement, GR_leverage_index_avg, WAR, salary, teamRpG, oppRpG, pyth_exponent, 
                                waa_win_perc, WAA, WAA_adj, oppRpG_rep, pyth_exponent_rep, waa_win_perc_rep, WAR_rep))

#Rename a couple of columns in our new tidy data set to fit to Lahman standards
names(final_pitching)[names(final_pitching)=="stint_ID"] <- "stint"
names(final_pitching)[names(final_pitching)=="lg_ID"] <- "lgID"
names(final_pitching)[names(final_pitching)=="year_ID"] <- "yearID"
names(final_pitching)[names(final_pitching)=="team_ID"] <- "teamID"

# Clean up the data types before loading into Lahman
final_pitching$age <- as.integer(as.character(final_pitching$age))
final_pitching$stint <- as.integer(as.character(final_pitching$stint))
final_pitching$G <- as.integer(as.character(final_pitching$G))
final_pitching$GS <- as.integer(as.character(final_pitching$GS))
final_pitching$IPouts <- as.integer(as.character(final_pitching$IPouts))
final_pitching$salary <- as.integer(as.character(final_pitching$salary))
final_pitching$IPouts_start <- as.integer(as.character(final_pitching$IPouts_start))
final_pitching$IPouts_relief <- as.integer(as.character(final_pitching$IPouts_relief))
final_pitching$RA <- as.integer(as.character(final_pitching$RA))
final_pitching$xRA_sprp_adj <- as.double(as.character(final_pitching$xRA_sprp_adj))
final_pitching$xRA_def_pitcher <- as.double(as.character(final_pitching$xRA_def_pitcher))
final_pitching$PPF <- as.integer(as.character(final_pitching$PPF))
final_pitching$PPF_custom <- as.double(as.character(final_pitching$PPF_custom))
final_pitching$xRA_final <- as.double(as.character(final_pitching$xRA_final))
final_pitching$BIP <- as.integer(as.character(final_pitching$BIP))
final_pitching$BIP_perc <- as.integer(as.character(final_pitching$BIP_perc))
final_pitching$RS_def_total <- as.double(as.character(final_pitching$RS_def_total))
final_pitching$runs_above_avg <- as.double(as.character(final_pitching$runs_above_avg))
final_pitching$runs_above_avg_adj <- as.double(as.character(final_pitching$runs_above_avg_adj))
final_pitching$runs_above_rep <- as.double(as.character(final_pitching$runs_above_rep))
final_pitching$RpO_replacement <- as.double(as.character(final_pitching$RpO_replacement))
final_pitching$GR_leverage_index_avg <- as.double(as.character(final_pitching$GR_leverage_index_avg))
final_pitching$WAR <- as.double(as.character(final_pitching$WAR))
final_pitching$oppRpG <- as.double(as.character(final_pitching$oppRpG))
final_pitching$pyth_exponent <- as.double(as.character(final_pitching$pyth_exponent))
final_pitching$waa_win_perc <- as.double(as.character(final_pitching$waa_win_perc))
final_pitching$WAA <- as.double(as.character(final_pitching$WAA))
final_pitching$WAA_adj <- as.double(as.character(final_pitching$WAA_adj))
final_pitching$oppRpG_rep <- as.double(as.character(final_pitching$oppRpG_rep))
final_pitching$pyth_exponent_rep <- as.double(as.character(final_pitching$pyth_exponent_rep))
final_pitching$waa_win_perc_rep <- as.double(as.character(final_pitching$waa_win_perc_rep))
final_pitching$WAR_rep <- as.double(as.character(final_pitching$WAR_rep))

write.csv(final_pitching, "lahman_db_with_bbrefid_pitchers.csv")
