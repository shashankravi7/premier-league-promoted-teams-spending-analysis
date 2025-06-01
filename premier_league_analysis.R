library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(corrplot)

transfers <- read_csv("transfers.csv", show_col_types = FALSE) #list of transfer in PL 
epl_standings <- read_csv("EPLStandings.csv", show_col_types = FALSE) #PL Table for each season 


# Reshaping EPL standings to long format

standings_long <- epl_standings %>%
  pivot_longer(
    cols = -Team,
    names_to = "season",
    values_to = "position"
  ) %>%
  filter(!is.na(position)) %>%
  mutate(season = as.numeric(season)) %>%
  rename(team = Team) %>%
  filter(season >= 2007 & season <= 2016) %>%
  arrange(team, season)

# Identify promoted teams by checking teams missing from previous season 
promoted_teams <- standings_long %>%
  arrange(team, season) %>%
  group_by(team) %>%
  mutate(
    prev_season = lag(season),
    season_gap = ifelse(is.na(prev_season), 0, season - prev_season),
    is_promoted = (is.na(prev_season)) | (season_gap > 1)
  ) %>%
  filter(is_promoted == TRUE) %>%
  select(team, season, position) %>%
  ungroup() %>%
  mutate(
    relegated = position >= 18,
    survival_status = ifelse(relegated, "Relegated", "Survived")
  )


promoted_summary <- promoted_teams %>%
  group_by(season) %>%
  summarise(
    count = n(),
    teams = paste(team, collapse = ", "),
    relegated_count = sum(relegated),
    survival_rate = round((count - relegated_count) / count * 100, 1),
    .groups = "drop"
  )
print("Promoted teams identified (2007-2016):")
print(promoted_summary)

total_promoted <- nrow(promoted_teams)
total_relegated <- sum(promoted_teams$relegated)
overall_survival_rate <- round((total_promoted - total_relegated) / total_promoted * 100, 1)

print(paste("Overall promoted team statistics (2007-2016):"))
print(paste("Total promoted teams:", total_promoted))
print(paste("Total relegated:", total_relegated))
print(paste("Overall survival rate:", overall_survival_rate, "%"))

#Transfer Spending
#converting seasons to year 
convert_season_to_year <- function(season_str) {
  if (is.na(season_str)) return(NA)
  
  if (str_detect(season_str, "^[0-9]{2}/[0-9]{2}$")) {
    parts <- str_split(season_str, "/")[[1]]
    end_year <- as.numeric(parts[2])
    if (end_year >= 90) {
      return(1900 + end_year)
    } else {
      return(2000 + end_year)
    }
  }
  
  if (str_detect(season_str, "^[0-9]{4}$")) {
    return(as.numeric(season_str))
  }
  
  return(NA)
}

# Clean up team names 
clean_team_names <- function(name) {
  name %>%
    str_to_lower() %>%
    str_trim() %>%
    str_replace("fc$|fc ", "") %>%
    str_replace("^fc ", "") %>%
    str_replace(" united$", "") %>%
    str_replace(" city$", "") %>%
    str_replace(" town$", "") %>%
    str_replace(" rovers$", "") %>%
    str_replace(" albion$", "") %>%
    str_replace("^manchester united", "manutd") %>%
    str_replace("^manchester city", "mancity") %>%
    str_replace("^tottenham hotspur", "tottenham") %>%
    str_replace("^west bromwich albion", "westbrom") %>%
    str_replace("^west brom", "westbrom") %>%
    str_replace("^queens park rangers", "qpr") %>%
    str_replace("^crystal palace", "crystalpalace") %>%
    str_replace("^brighton & hove albion", "brighton") %>%
    str_replace("^aston villa", "astonvilla") %>%
    str_replace("^west ham united", "westham") %>%
    str_replace("^leicester city", "leicester") %>%
    str_replace("^newcastle united", "newcastle") %>%
    str_replace("^norwich city", "norwich") %>%
    str_replace("^swansea city", "swansea") %>%
    str_replace("^cardiff city", "cardiff") %>%
    str_replace("^hull city", "hull") %>%
    str_replace_all("[^a-z0-9]", "")
}

# Add transfer data from 2005-2016
transfers_clean <- transfers %>%
  filter(!is.na(transfer_fee) & transfer_fee > 0) %>%
  filter(!is.na(transfer_season)) %>%
  mutate(season_year = map_dbl(transfer_season, convert_season_to_year)) %>%
  filter(!is.na(season_year) & season_year >= 2005 & season_year <= 2016) %>%
  filter(
    str_detect(str_to_lower(to_club_name), "arsenal|chelsea|liverpool|manchester|tottenham|leicester|west ham|everton|newcastle|aston villa|brighton|crystal palace|fulham|watford|burnley|norwich|leeds|sheffield|wolves|southampton|bournemouth|cardiff|swansea|hull|blackburn|bolton|wigan|birmingham|portsmouth|reading|derby|west brom|stoke|middlesbrough") |
      str_detect(str_to_lower(from_club_name), "arsenal|chelsea|liverpool|manchester|tottenham|leicester|west ham|everton|newcastle|aston villa|brighton|crystal palace|fulham|watford|burnley|norwich|leeds|sheffield|wolves|southampton|bournemouth|cardiff|swansea|hull|blackburn|bolton|wigan|birmingham|portsmouth|reading|derby|west brom|stoke|middlesbrough")
  ) %>%
  mutate(
    from_club_clean = clean_team_names(from_club_name),
    to_club_clean = clean_team_names(to_club_name)
  )

print(paste("\nTransfer data processed:"))
print(paste("Transfer records (2005-2016):", nrow(transfers_clean)))
print(paste("Total transfer value: €", round(sum(transfers_clean$transfer_fee) / 1000000, 1), "M"))

# Calculate spending and income by team and season
money_spent <- transfers_clean %>%
  group_by(season_year, to_club_clean) %>%
  summarise(
    total_spent = sum(transfer_fee, na.rm = TRUE),
    players_bought = n(),
    .groups = "drop"
  )

money_received <- transfers_clean %>%
  group_by(season_year, from_club_clean) %>%
  summarise(
    total_received = sum(transfer_fee, na.rm = TRUE),
    players_sold = n(),
    .groups = "drop"
  )

print(paste("Teams with spending data:", nrow(money_spent)))
print(paste("Teams with income data:", nrow(money_received)))

#Matching Spend to Teams

# Add cleaned names to promoted teams
promoted_teams_clean <- promoted_teams %>%
  mutate(team_clean = clean_team_names(team))

# Create datasets for multiple time periods to capture promotion summer spending
money_spent_current <- money_spent
money_received_current <- money_received

money_spent_promo1 <- money_spent %>% 
  mutate(season_year = season_year + 1) %>%
  rename_with(~paste0(., "_promo1"), c(total_spent, players_bought))

money_received_promo1 <- money_received %>% 
  mutate(season_year = season_year + 1) %>%
  rename_with(~paste0(., "_promo1"), c(total_received, players_sold))

money_spent_promo2 <- money_spent %>% 
  mutate(season_year = season_year + 2) %>%
  rename_with(~paste0(., "_promo2"), c(total_spent, players_bought))

money_received_promo2 <- money_received %>% 
  mutate(season_year = season_year + 2) %>%
  rename_with(~paste0(., "_promo2"), c(total_received, players_sold))

# Join all datasets with comprehensive matching strategy
promoted_spending <- promoted_teams_clean %>%

  left_join(money_spent_current, by = c("team_clean" = "to_club_clean", "season" = "season_year")) %>%
  left_join(money_received_current, by = c("team_clean" = "from_club_clean", "season" = "season_year")) %>%

  left_join(money_spent_promo1, by = c("team_clean" = "to_club_clean", "season" = "season_year")) %>%
  left_join(money_received_promo1, by = c("team_clean" = "from_club_clean", "season" = "season_year")) %>%

  left_join(money_spent_promo2, by = c("team_clean" = "to_club_clean", "season" = "season_year")) %>%
  left_join(money_received_promo2, by = c("team_clean" = "from_club_clean", "season" = "season_year")) %>%

  mutate(
    total_spent_final = coalesce(total_spent, total_spent_promo1, total_spent_promo2, 0),
    total_received_final = coalesce(total_received, total_received_promo1, total_received_promo2, 0),
    players_bought_final = coalesce(players_bought, players_bought_promo1, players_bought_promo2, 0),
    players_sold_final = coalesce(players_sold, players_sold_promo1, players_sold_promo2, 0),
    
    # Track data source for transparency
    data_source = case_when(
      !is.na(total_spent) & total_spent > 0 ~ "Current Season",
      !is.na(total_spent_promo1) & total_spent_promo1 > 0 & 
        (is.na(total_spent) | total_spent == 0) ~ "Promotion Summer (-1 year)",
      !is.na(total_spent_promo2) & total_spent_promo2 > 0 & 
        (is.na(total_spent) | total_spent == 0) & 
        (is.na(total_spent_promo1) | total_spent_promo1 == 0) ~ "Early Promotion (-2 years)",
      !is.na(total_spent) & total_spent > 0 & 
        (!is.na(total_spent_promo1) | !is.na(total_spent_promo2)) ~ "Multiple Years",
      TRUE ~ "No Data"
    ),
    
    # Final calculations
    total_spent = total_spent_final,
    total_received = total_received_final,
    players_bought = players_bought_final,
    players_sold = players_sold_final,
    net_spend = total_spent - total_received,
    net_spend_millions = net_spend / 1000000
  ) %>%
  select(-ends_with("_promo1"), -ends_with("_promo2"), -ends_with("_final"))

# Summary of matching results
teams_with_spending <- promoted_spending %>%
  filter(net_spend_millions != 0)

print(paste("\nEnhanced spending data matching:"))
print(paste("Promoted teams with spending data:", nrow(teams_with_spending)))
print(paste("Promoted teams without spending data:", nrow(promoted_spending) - nrow(teams_with_spending)))
print(paste("Data coverage:", round(nrow(teams_with_spending) / nrow(promoted_spending) * 100, 1), "%"))

# Categorize teams by spending levels
promoted_spending_categorized <- promoted_spending %>%
  mutate(
    spending_category = case_when(
      net_spend_millions >= 30 ~ "Very High (30M+)",
      net_spend_millions >= 15 ~ "High (15-30M)",
      net_spend_millions >= 10 ~ "Medium-High (10-15M)",
      net_spend_millions >= 5 ~ "Medium (5-10M)",
      net_spend_millions > 0 ~ "Low (0-5M)",
      net_spend_millions < 0 ~ "Net Sellers",
      TRUE ~ "No Spending Data"
    )
  )

# Calculate relegation rates by spending category
spending_analysis <- promoted_spending_categorized %>%
  group_by(spending_category) %>%
  summarise(
    teams = n(),
    relegated_count = sum(relegated),
    relegation_rate = round(relegated_count / teams * 100, 1),
    avg_position = round(mean(position), 1),
    avg_spending = round(mean(net_spend_millions), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_spending))

print("\nRelegation rates by spending category:")
print(spending_analysis)

# Maximum spender among promoted teams each season
max_spenders_by_season <- promoted_spending %>%
  filter(net_spend_millions > 0) %>%
  group_by(season) %>%
  slice_max(net_spend_millions, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(season)

print("\nMaximum spenders among promoted teams by season:")
max_spenders_by_season %>%
  mutate(
    spending_formatted = paste0("€", round(net_spend_millions, 1), "M"),
    outcome = paste0(survival_status, " (", position, 
                     ifelse(position <= 3, c("st", "nd", "rd")[position], "th"), ")")
  ) %>%
  select(season, team, spending_formatted, outcome) %>%
  print()

#survival statistics
max_survival_stats <- max_spenders_by_season %>%
  summarise(
    total_seasons = n(),
    survived_count = sum(!relegated),
    relegated_count = sum(relegated),
    survival_rate = round(survived_count / total_seasons * 100, 1),
    avg_spending = round(mean(net_spend_millions), 1),
    max_spending = round(max(net_spend_millions), 1)
  )

print("\nMaximum spender survival statistics:")
print(max_survival_stats)

#Visualisations 

# Bar Chart by Spending
p1 <- ggplot(spending_analysis %>% filter(spending_category != "No Spending Data"), 
             aes(x = reorder(spending_category, avg_spending), 
                 y = relegation_rate, fill = spending_category)) +
  geom_col(alpha = 0.8, color = "black") +
  geom_text(aes(label = paste0(relegation_rate, "%\n(", teams, " teams)")), 
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_y_continuous(limits = c(0, max(spending_analysis$relegation_rate) * 1.2),
                     labels = label_percent(scale = 1)) +
  labs(
    title = "Relegation Rate by Transfer Spending Category (2007-2016)",
    subtitle = "Clear inverse relationship between spending and relegation risk",
    x = "Spending Category",
    y = "Relegation Rate (%)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p1)

#Spending vs Position
p2 <- ggplot(teams_with_spending, aes(x = net_spend_millions, y = position, 
                                      color = survival_status, size = abs(net_spend_millions))) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 17.5, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "blue", linewidth = 1) +
  scale_color_manual(
    values = c("Survived" = "darkgreen", "Relegated" = "darkred"),
    labels = c("Survived", "Relegated")
  ) +
  scale_y_reverse() +
  scale_size_continuous(range = c(3, 8), guide = "none") +
  labs(
    title = "Premier League Promoted Teams: Spending vs Final Position (2007-2016)",
    subtitle = "Red line = relegation zone, Blue line = €10M threshold",
    x = "Net Transfer Spending (€ Millions)",
    y = "Final League Position",
    color = "Outcome",
    caption = "Data: Premier League promoted teams with transfer spending data"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

#Maximum spenders
p3 <- ggplot(max_spenders_by_season, aes(x = season, y = net_spend_millions)) +
  geom_line(color = "darkblue", size = 1.2, alpha = 0.7) +
  geom_point(aes(color = survival_status, size = net_spend_millions), alpha = 0.8) +
  geom_text(aes(label = paste0(team, "\n€", round(net_spend_millions, 1), "M")), 
            vjust = -1.5, size = 2.5, fontface = "bold") +
  scale_color_manual(
    values = c("Survived" = "darkgreen", "Relegated" = "darkred"),
    name = "Outcome"
  ) +
  scale_size_continuous(range = c(3, 8), guide = "none") +
  scale_x_continuous(breaks = seq(2007, 2016, 1)) +
  scale_y_continuous(labels = label_currency(prefix = "€", suffix = "M"),
                     expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title = "Maximum Promoted Team Spending vs Survival (2007-2016)",
    subtitle = paste("Highest spenders among promoted teams each season •", 
                     max_survival_stats$survival_rate, "% survival rate"),
    x = "Season",
    y = "Net Transfer Spending (€ Millions)",
    caption = "Data: Premier League promoted teams\nGreen = Survived, Red = Relegated"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p3)

#Key Findings

print("\n=== KEY FINDINGS (2007-2016) ===")
print(paste("Analysis period: 2007-2016"))
print(paste("Total promoted teams:", total_promoted))
print(paste("Teams with spending data:", nrow(teams_with_spending)))
print(paste("Data coverage:", round(nrow(teams_with_spending) / total_promoted * 100, 1), "%"))
print(paste("Overall survival rate:", overall_survival_rate, "%"))

# Test the €10M threshold
high_spenders <- teams_with_spending %>%
  filter(net_spend_millions >= 10)

if (nrow(high_spenders) > 0) {
  high_spender_relegations <- sum(high_spenders$relegated)
  print(paste("Teams spending €10M+:", nrow(high_spenders)))
  print(paste("€10M+ teams relegated:", high_spender_relegations))
  
  if (high_spender_relegations == 0) {
    print("✅ €10M threshold validated: No team spending €10M+ was relegated")
  } else {
    print("❌ €10M threshold violated: Some high spenders were relegated")
    print("High spending relegated teams:")
    high_spenders %>%
      filter(relegated) %>%
      select(team, season, net_spend_millions, position) %>%
      print()
  }
} else {
  print("No teams spent €10M+ in this period")
}

# Correlation analysis
correlation <- cor(teams_with_spending$net_spend_millions, 
                   teams_with_spending$position, use = "complete.obs")
print(paste("Correlation (spending vs position):", round(correlation, 3)))
print("(Negative correlation = higher spending leads to better position)")
summary(correlation)

# Export final results
write_csv(promoted_spending, "promoted_teams_complete_analysis_2007_2016.csv")
write_csv(spending_analysis, "spending_categories_analysis_2007_2016.csv")
write_csv(max_spenders_by_season, "maximum_spenders_by_season_2007_2016.csv")

