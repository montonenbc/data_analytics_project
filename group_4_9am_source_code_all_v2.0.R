# Group 4 - project source code
# Author: Mika and team 
# Project for ISYS3350 Data Management for Analytics and Applications.


# Stage 0: brainstorm & project proposal ---------------------------------------
# No code required for this stage of the project.
# Stage 1a:  espn data ---------------------------------------------------------

# scraping

# Author: Group 4 (montonen@bc.edu, and team emails)
# Purpose: Scrape teams and players from ESPN.


library(httr2)
library(tidyverse)
library(polite)
library(rvest)
library(purrr)

IS_DEBUG <- TRUE

# Source: https://www.espn.com/nfl

url_base <- "https://www.espn.com"
url_teams <- str_glue("{url_base}/nfl/teams")

session <- bow(
  url = url_base,  # base URL
  user_agent = "Mika Montonen (BC)",  # identify ourselves
  force = TRUE
)
print(session) 
# While it doesn't like that we're scraping it, this is also
# not illegal in any way, so we are fine, we just can't use the
# polite package's functions.


## Function to get all teams
get_team_data <- function(){
  
  html_teams <- read_html(url_teams)
  
  team_division <- html_teams |>
    html_element("div.page-container") |>
    html_elements("div.mt7") |>
    html_element("div.headline") |>
    html_text2()
  team_division 
  # There are 4 teams in each division.
  team_division_clean <- rep(team_division, 4)
  
  teams <- html_teams |>
    html_element("div.page-container") |>
    html_elements("div.pl3")
  teams
  
  team_urls <- teams |>
    html_element("a.AnchorLink:first-of-type") |>
    html_attr("href")
  team_urls_clean <- str_glue("{url_base}{team_urls}")
  team_urls_clean
  
  team_roster_urls <- teams |>
    html_element("div.TeamLinks__Links") |>
    html_element("span:nth-of-type(3)") |>
    html_element("a") |>
    html_attr("href")
  team_roster_urls
  
  team_roster_urls_clean <- str_glue("{url_base}{team_roster_urls}")
  team_roster_urls_clean
  
  team_names <- teams |>
    html_elements("h2") |>
    html_text2()
  team_names
  
  df_teams <- tibble(
    team_name = team_names,
    team_url = team_urls_clean,
    team_roster_url = team_roster_urls_clean,
    team_division = team_division_clean
  )
  
  return(df_teams)
}

## Function to get all players of a team
get_roster_data <- function(roster_url){
  Sys.sleep((5)) # Wait for 5 seconds.
  
  html_roster <- read_html(roster_url) |>
    html_element("section.Card")
  
  roster_types <- html_roster |>
    html_elements("div.Table__Title") |>
    html_text2()
  
  roster_player_urls <- html_roster |>
    html_elements("a") |>
    html_attr("href")
  
  roster_player_urls_clean <- roster_player_urls[c(TRUE, FALSE)] |>
    str_replace(pattern = "http://", replacement = "https://") |>
    str_replace(pattern = "/_/", replacement = "/bio/_/")
  
  roster_player_urls_clean <- roster_player_urls_clean[str_detect(
    string = roster_player_urls_clean, 
    pattern = fixed(url_base))]
  
  roster_offense <- html_roster |>
    html_element("div.ResponsiveTable.Offense") |>
    html_element("table") |>
    html_table() |>
    select(-1) |>
    # Additional column cleaning here.
    mutate(
      type = "Offense",
      player_number = str_extract(string = Name, pattern = "\\d*$"),
      player_name = str_extract(string = Name, pattern = "\\D+(?=\\d*$)"),
      experience = case_when(
        str_detect(string = Exp, pattern = "R") ~ "0",
        TRUE ~ as.character(Exp)
      ),
      experience_clean = parse_number(experience)
    ) |>
    select(-Name, -Exp, -experience)
  
  roster_defense <- html_roster |>
    html_element("div.ResponsiveTable.Defense") |>
    html_element("table") |>
    html_table() |>
    select(-1) |>
    # Additional column cleaning here.
    mutate(
      type = "Defense",
      player_number = str_extract(string = Name, pattern = "\\d*$"),
      player_name = str_extract(string = Name, pattern = "\\D+(?=\\d*$)"),
      experience = case_when(
        str_detect(string = Exp, pattern = "R") ~ "0",
        TRUE ~ as.character(Exp)
      ),
      experience_clean = parse_number(experience)
    ) |>
    select(-Name, -Exp, -experience)
  
  roster_special <- html_roster |>
    html_element("div.ResponsiveTable.Special.Teams") |>
    html_element("table") |>
    html_table() |>
    select(-1) |>
    # Additional column cleaning here.
    mutate(
      type = "Special",
      player_number = str_extract(string = Name, pattern = "\\d*$"),
      player_name = str_extract(string = Name, pattern = "\\D+(?=\\d*$)"),
      experience = case_when(
        # R means rookie, so 0 would work for '0 years of experience'.
        str_detect(string = Exp, pattern = "R") ~ "0",
        TRUE ~ as.character(Exp)
      ),
      experience_clean = parse_number(experience)
    ) |>
    select(-Name, -Exp, -experience)
  
  roster <- bind_rows(
    roster_offense, 
    roster_defense, 
    roster_special
  ) 
  
  roster1 <- tibble(roster, player_urls = roster_player_urls_clean)
  
  player_info_list <- map_df(roster_player_urls_clean, get_player_data)
  roster1 <- left_join(roster1, player_info_list, join_by(player_urls == player_urls))
  
  #column orders
  roster1 <- roster1 |>
    relocate(state, .after = College) |>
    relocate(city, .before = state) |>
    relocate(player_name) |>
    relocate(College, .after = experience_clean)
  
  return(roster1)
}

## Helper method for get_roster_data, to get the player hometown
get_player_data <- function(player_url) {
  Sys.sleep((5)) # Wait for 5 seconds.
  biography_text <- read_html(player_url) |>
    html_element("div.Wrapper.Card__Content") |>
    html_elements(".Bio__Item.n8.mb4") |>
    html_text2()
  
  hometown_index <- which(str_detect(biography_text, "Birthplace"))
  hometown <- ifelse(length(hometown_index) > 0, biography_text[hometown_index] |>
                       str_sub(11, ), NA)
  
  # Extract city and state from hometown
  city <- str_extract(hometown, ".*(?=,)")
  state <- str_extract(hometown, "(?<=,\\s).*")
  
  # Create a tibble with player information
  player_info <- tibble(player_urls = player_url, city = city, state = state)
  
  return(player_info)
}

get_roster_data("https://www.espn.com/nfl/team/roster/_/name/buf/buffalo-bills")


# Loop over teams.
team_data <- get_team_data()
team_data
write.csv(team_data)
write.csv(team_data, "C:\\Users\\Mika Montonen\\OneDrive\\Desktop\\OneDrive - bc.edu\\14\\Fall 2023\\Data Applications for Analytics and Management\\RStudio\\team_data.csv", row.names=TRUE)

IS_DEBUG <- FALSE
if (IS_DEBUG){
  roster_urls <- team_data |> slice(1:3) |> pull(team_roster_url)
} else{
  roster_urls <- c(team_data |> pull(team_roster_url))
}
roster_urls

write.csv(df_team_rosters)
write.csv(df_team_rosters, "C:\\Users\\Mika Montonen\\OneDrive\\Desktop\\OneDrive - bc.edu\\14\\Fall 2023\\Data Applications for Analytics and Management\\RStudio\\player_data.csv", row.names=TRUE)


# Stage 1b:  tidycensus data ---------------------------------------------------

library(tidyverse)
library(stringr)
library(rvest)
library(scales)
library(httr2)
library(tibble)
library(dplyr)
library(xml2)
library(tools)
library(tidycensus)
library(tidyverse)

census_api_key("c2c12b180bda1aa5648bf8bfb5d6b0db8db36f55", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

#list of variables

variables <- load_variables(2010, "acs5", cache = TRUE)


#get data:
#medium household income data by county in the United States 2006-2010

income_data <- get_acs(geography = "place",
                       variables = c(medincome = "B19013_001"),
                       year = 2010)


#clean data

income_by_city <- income_data |>
  separate_wider_delim(
    NAME, delim = ",", names = c("city", "state")
  ) |>
  mutate(
    state = str_trim(state, "left"),
    state_abbr = state.abb[match(state, state.name)],
    city = toTitleCase(city)
  ) |>
  select(
    med_income = estimate,
    margin_error = moe,
    !variable
  ) |>
  filter(!is.na(state))

View(income_by_city)

write.csv(income_by_city, 
          "C:/Users/Mika Montonen/OneDrive/Desktop/OneDrive - bc.edu/14/Fall 2023/Data Applications for Analytics and Management/RStudio/income_by_city.csv", 
          row.names=TRUE
)

library(tidyverse) #includes many basic packages
library(rvest) #for scraping
library(scales) #for details on graphs
library(httr2) #for API requests
library(xml2) #work with HTML and XML
library(tools) #random tools

install.packages("tidycensus")
library(tidycensus)

census_api_key("c2c12b180bda1aa5648bf8bfb5d6b0db8db36f55", install = TRUE)

#list of variables

variables <- load_variables(2010, "acs5", cache = TRUE)

#get data:
#medium household income data by county in the United States 2006-2010

income_data <- get_acs(geography = "place",
                       variables = c(medincome = "B19013_001"),
                       year = 2010)

#clean data

income_by_city <- income_data |>
  separate_wider_delim(
    NAME, delim = ",", names = c("city", "state")
  ) |>
  mutate(
    state = str_trim(state, "left"),
    state_abbr = state.abb[match(state, state.name)],
    city = toTitleCase(city)
  ) |>
  select(
    med_income = estimate,
    margin_error = moe,
    !variable
  ) |>
  filter(
    if_any(everything(), ~!is.na(.))
  ) |>
  group_by(city) |>
  mutate(
    med_income = mean(med_income)
  ) |>
  distinct(city, .keep_all = TRUE)

income_by_city |> head()
nrow(income_by_city)
n_distinct(income_by_city$city)

#export data to excel

write.csv(income_by_city, file = "income_by_city.csv")

#summary stats

glimpse(income_by_city)
n_distinct(income_by_city$city)
round(mean(income_by_city$med_income, na.rm = TRUE), 2)
round(median(income_by_city$med_income, na.rm = TRUE), 2)

# Stage 2:   project progress report -------------------------------------------
#summary stats - income

glimpse(income_by_city)
n_distinct(income_by_city$city)
round(mean(income_by_city$med_income, na.rm = TRUE), 2)
round(median(income_by_city$med_income, na.rm = TRUE), 2)

#sample visualization of medium income by state

income_by_state <- income_by_city |>
  filter(!is.na(state_abbr)) |>
  group_by(state_abbr) |>
  summarise(
    med_inc_state = mean(med_income, na.rm = TRUE)
  )

income_by_state |>
  ggplot(aes(x = state_abbr, y = med_inc_state)) +
  geom_point() +
  labs(
    title = "Median Household Income By State",
    x = "State",
    y = "Income ($)"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90)
  )

#sample state: New Jersey

income_nj <- income_by_city |>
  filter(state_abbr == "NJ")

income_nj |>
  ggplot(aes(x = city, y = med_income)) +
  geom_point() +
  labs(
    title = "Median Household Income by Cities in New Jersey",
    x = "City",
    y = "Income ($)"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90)
  )

#player data analysis, without income data

library(readr)
library(dplyr)

#player data file (espn)
player_data <- read_csv("C:/Users/Mika Montonen/OneDrive/Desktop/OneDrive - bc.edu/14/Fall 2023/Data Applications for Analytics and Management/RStudio/player_data.csv")
View(player_data)


distinct(player_data, POS)

QB <- player_data |> filter(POS == "QB") |> select(player_name, POS, Age, city, state)
RB <- player_data |> filter(POS == "RB") |> select(player_name, POS, Age, city, state)
FB <- player_data |> filter(POS == "FB") |> select(player_name, POS, Age, city, state) 
WR <- player_data |> filter(POS == "WR") |> select(player_name, POS, Age, city, state)  
TE <- player_data |> filter(POS == "TE") |> select(player_name, POS, Age, city, state)   
C  <- player_data |> filter(POS == "C") |> select(player_name, POS, Age, city, state)
G  <- player_data |> filter(POS == "G") |> select(player_name, POS, Age, city, state)
OT <- player_data |> filter(POS == "OT") |> select(player_name, POS, Age, city, state)  
DE <- player_data |> filter(POS == "DE") |> select(player_name, POS, Age, city, state)   
DT <- player_data |> filter(POS == "DT") |> select(player_name, POS, Age, city, state)
LB <- player_data |> filter(POS == "LB") |> select(player_name, POS, Age, city, state)
CB <- player_data |> filter(POS == "CB") |> select(player_name, POS, Age, city, state)
S  <- player_data |> filter(POS == "S") |> select(player_name, POS, Age, city, state)
PK <- player_data |> filter(POS == "PK") |> select(player_name, POS, Age, city, state)
P  <- player_data |> filter(POS == "P") |> select(player_name, POS, Age, city, state)
LS <- player_data |> filter(POS == "LS") |> select(player_name, POS, Age, city, state)



# for (position in distinct(player_data, POS)){
#   player_data |> filter(POS == position) |> select(player_name, POS, Age, city, state)
# }

position_counts <- player_data |>
  count(POS) |>
  arrange(desc(n))

state_counts <- player_data |>
  count(state) |>
  arrange(desc(n))

city_counts <- player_data |>
  count(city) |>
  arrange(desc(n))

# Stage 3:   merging datasets --------------------------------------------------

# libraries used
library(readr)
library(dplyr)
library(stringdist)
library(fuzzyjoin)
library(stringr)
library(gt) # Source: https://gt.rstudio.com/ 

#player data file and income data (tidycensus)
player_data <- read_csv("C:/Users/Mika Montonen/OneDrive/Desktop/OneDrive - bc.edu/14/Fall 2023/Data Applications for Analytics and Management/RStudio/player_data.csv")
income_by_city <- read_csv("C:/Users/Mika Montonen/OneDrive/Desktop/OneDrive - bc.edu/14/Fall 2023/Data Applications for Analytics and Management/RStudio/income_by_city.csv")

# Clean player data, just like for income_data in mika_fuzzy_join_v5.0
player_data$city <- str_remove_all(string = player_data$city, pattern = "\\sVillage") #Remove "Village"
player_data$city <- str_remove_all(string = player_data$city, pattern = "\\sCity") #Remove "City"
player_data$city <- str_remove_all(string = player_data$city, pattern = '[ \t]+$') #Remove any trailing whitespace (Source: StackOverflow)

# Removing NAs in both. previous work days I got Error cannot join with NAs
player_data <- player_data |> na.omit()
income_by_city <- income_by_city |> na.omit()

# Source: https://youtu.be/lR2_GaaHyls?si=CKzXwwyNtINfs0jE 
#joining data, via a a type of fuzzy join
m_string_inner <- stringdist_inner_join(
                              player_data,
                              income_by_city,
                              by = c("city" = "city"),
                              max_dist = 4,
                              distance_col = "distance"
                            )

# filtering best matches by accuracy. Above 2 are almost all innacurate
m_filtered <- m_string_inner |>
  filter(distance <= 2)

# Optional: to see column names
names(m_string_inner)

# selecting appropiate columns. can be modified. Put state and state x and y together for testing.
m_filtered_02 <- m_filtered |>
  select(player_name, POS, Age, HT, WT, city.x, state.x,  
         city.y, state_abbr, #locate x and y city, state columns next to each other. best way for a visual learners like myself.
         type, player_number, experience_clean, College, player_urls,
         med_income, margin_error, distance)

# filtering city matches 
m_filtered_03 <- m_filtered_02 |> #10090 obs. 
  filter(city.x == city.y)

# filtering state matches
final_result <- m_filtered_03 |>
  filter(state.x == state_abbr) #2157 obs. But there are 2443 obs. in player_data

# save to cloud
write.csv(final_result, 
          "C:/Users/Mika Montonen/OneDrive/Desktop/OneDrive - bc.edu/14/Fall 2023/Data Applications for Analytics and Management/RStudio/merged_player_and_income_data.csv", 
          row.names=TRUE
)

gt(final_result, caption = "mika test joining")

df <- read_csv("C:/Users/Mika Montonen/OneDrive/Desktop/OneDrive - bc.edu/14/Fall 2023/Data Applications for Analytics and Management/RStudio/merged_player_and_income_data.csv")
gt(df, caption = "stringdist_inner_join on city")

# cleaning table. keeping most important columns
df_clean <- df |>
  select(player_name, POS, city.x, state.x,
         med_income)
gt(df_clean, caption = "NFL Players and Hometown Income")


# Stage 4:   regressions, models, and viz --------------------------------------

income_data <- read.csv('income_by_city.csv')
income_data

summary(income_data)

Median_Income <- (43333)
Mean_Income <- (49053)
Num_Cities <- (24464)

summary_table <- data.frame(
  'Median Income' = Median_Income,
  'Mean Income' = Mean_Income,
  'Number of Cities' = Num_Cities
)

print(summary_table)

# Income by State
median_income_by_state <- median_income_by_state %>%
  arrange(desc(median_income))

ggplot(median_income_by_state, aes(x = reorder(state, -median_income), y = median_income)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "State", y = "Median Income", title = "Median Income by State") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


median_income_by_state$highlight_state <- median_income_by_state$state %in% c("CA", "TX", "OH", "FL", "GA")

median_income_by_state$highlight_state <- factor(median_income_by_state$highlight_state, levels = c(FALSE, TRUE))

ggplot(median_income_by_state, aes(x = reorder(state, -median_income), y = median_income, fill = highlight_state)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("FALSE" = "skyblue", "TRUE" = "orange"), guide = FALSE) +
  labs(x = "State", y = "Median Income", title = "Median Income by State") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Per Capita
median_income_by_state$highlight_state <- median_income_by_state$state %in% c("AL", "TX", "MS", "FL", "GA")

median_income_by_state$highlight_state <- factor(median_income_by_state$highlight_state, levels = c(FALSE, TRUE))

ggplot(median_income_by_state, aes(x = reorder(state, -median_income), y = median_income, fill = highlight_state)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("FALSE" = "skyblue", "TRUE" = "orange"), guide = FALSE) +
  labs(x = "State", y = "Median Income", title = "Median Income by State") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

merged_data <- read.csv('merged_player_and_income_data.csv')
merged_data

library(ggplot2)
library(dplyr)


merged_data <- merged_data %>%
  mutate(income_brackets = case_when(
    med_income >= 20000 & med_income < 40000 ~ "20000-40000",
    med_income >= 40000 & med_income < 60000 ~ "40000-60000",
    med_income >= 60000 & med_income < 100000 ~ "60000-100000",
    med_income >= 100000 ~ "100000+",
    TRUE ~ "Other"
  ))

specific_positions <- merged_data %>%
  filter(POS %in% c("QB", "PK", "P", "CB", "RB"))


income_position_counts <- specific_positions %>%
  group_by(POS, income_brackets) %>%
  summarise(player_count = n()) %>%
  mutate(percentage = player_count / sum(player_count) * 100) %>%
  ungroup() %>%
  arrange(POS, income_brackets)

ggplot(income_position_counts, aes(x = "", y = percentage, fill = income_brackets)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5), color = "black") + # Add percentage labels
  coord_polar("y", start = 0) +
  facet_wrap(~ POS, ncol = 3) +  # Facet by position for individual pie charts
  labs(title = "Percentage of Players by Income Category in Each Position",
       fill = "Income Category", x = NULL, y = NULL) +
  scale_fill_manual(values = c("20000-40000" = "#FF9999", "40000-60000" = "#66CCCC", "60000-100000" = "#008000", "100000+" = "#B2FF66")) +
  theme_void() +
  theme(strip.text = element_text(size = 8, color = "black", face = "bold"),
        strip.background = element_blank())

library(dplyr)

threshold <- 48412.32

merged_data %>%
  mutate(income_category = ifelse(med_income > threshold, "Above Threshold", "Below Threshold")) %>%
  group_by(type, income_category) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

library(ggplot2)


merged_data <- merged_data %>%
  mutate(income_category = ifelse(med_income > 43333, "Above Threshold", "Below Threshold"))

income_counts <- merged_data %>%
  count(income_category)


income_counts <- income_counts %>%
  mutate(percentage = n / sum(n) * 100)


ggplot(income_counts, aes(x = "", y = n, fill = income_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Players by Median Income Category", fill = "Income Category") +
  scale_fill_manual(values = c("Above Threshold" = "skyblue", "Below Threshold" = "salmon")) + # Custom colors
  theme_void() +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))  # Add percentage labels

library(ggplot2)
library(dplyr)


merged_data <- merged_data %>%
  mutate(income_category = cut(med_income, breaks = c(20000, 40000, 60000, 100000, Inf),
                               labels = c("20000-40000", "40000-60000", "60000-100000", "100000+")))


income_position_counts <- merged_data %>%
  count(income_category, POS) %>%
  group_by(income_category) %>%
  arrange(income_category, desc(n)) %>%
  slice_max(n, n = 3) %>%
  ungroup()


income_counts <- merged_data %>%
  count(income_category) %>%
  mutate(percentage = n / sum(n) * 100)


ggplot(income_counts, aes(x = "", y = n, fill = income_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Players by Income Bracket", fill = "Income Bracket") +
  scale_fill_manual(values = c("20000-40000" = "salmon", "40000-60000" = "lightgreen",
                               "60000-100000" = "skyblue", "100000+" = "gold")) +
  theme_void() +
  geom_text(data = income_position_counts, aes(label = POS),
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  geom_text(data = income_counts, aes(label = paste0(round(percentage), "%")),
            position = position_stack(vjust = 0.5))

summary_stats <- summary(merged_data$med_income)
summary_stats


specific_positions <- merged_data %>%
  filter(POS %in% c("QB", "P", "K", "RB", "CB"))


income_position_counts <- specific_positions %>%
  count(income_category, POS) %>%
  group_by(income_category) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(income_category, desc(percentage))

top_positions_by_percentage <- income_position_counts %>%
  group_by(income_category) %>%
  filter(rank(desc(percentage)) <= 3) %>%
  summarise(top_positions = toString(paste(POS, "(", round(percentage, 2), "%)")))

top_positions_by_percentage

library(dplyr)

median_income_by_position <- merged_data %>%
  group_by(POS) %>%
  summarise(median_income = median(med_income, na.rm = TRUE)) %>%
  arrange(desc(median_income))

median_income_by_position



income_position_counts <- specific_positions %>%
  count(POS, income_category) %>%
  group_by(POS) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(POS, desc(percentage))

ggplot(income_position_counts, aes(x = "", y = n, fill = income_category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Breakdown of Income Categories by Position", fill = "Income Category") +
  theme_void() +
  facet_wrap(~ POS, ncol = 2) +
  scale_fill_brewer(palette = "Set3") +
  coord_equal()

library(dplyr)

avg_income_by_position <- merged_data %>%
  group_by(POS) %>%
  summarise(avg_med_income = mean(med_income, na.rm = TRUE)) %>%
  arrange(desc(avg_med_income))

print(avg_income_by_position)

library(dplyr)

threshold <- 48412.32


merged_data <- merged_data %>%
  mutate(income_category = ifelse(med_income > threshold, "Above Threshold", "Below Threshold"))

summary_stats <- merged_data %>%
  count(income_category) %>%
  mutate(percentage = n / sum(n) * 100)

summary_stats

ggplot(avg_income_by_position, aes(x = reorder(POS, -avg_med_income), y = avg_med_income)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Mean Income by Position", x = "Position", y = "Average Mean Income") +
  geom_hline(yintercept = 48412, color = "black", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# Your tibble data
data <- data.frame(
  POS = c("PK", "LS", "P", "TE", "QB", "C", "OT", "G", "FB", "WR", "LB", "DT", "DE", "RB", "CB", "S"),
  median_income = c(53074.0, 52569.0, 52470.0, 50520.0, 49768.0, 49138.0, 47397.0, 47209.0, 47153.0, 46188.0, 45400.0, 44792.0, 44642.0, 44090.5, 43594.0, 43117.0)
)

# Reorder POS in descending order based on median_income
data$POS <- factor(data$POS, levels = data$POS[order(-data$median_income)])

# Create a bar plot
ggplot(data, aes(x = POS, y = median_income)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Median Income by Position", x = "Position", y = "Median Income") +
  geom_hline(yintercept = 43333, color = "red", linetype = "dashed") +  # Add a horizontal line
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

library(ggplot2)
library(dplyr)

threshold <- 43333  # Median income threshold

# Assuming 'merged_data' is your data frame containing player information

# Creating a new column 'above_below_threshold' based on the median income threshold
merged_data <- merged_data %>%
  mutate(above_below_threshold = ifelse(med_income > threshold, "Above Threshold", "Below Threshold"))

# Calculating counts by position and above/below threshold
position_counts <- merged_data %>%
  count(POS, above_below_threshold) %>%
  group_by(POS) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

# Plotting pie charts for each position
ggplot(position_counts, aes(x = "", y = percentage, fill = above_below_threshold)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~ POS, ncol = 4) +  # Facet by position for multiple charts per row
  coord_polar("y", start = 0) +  # To create a pie chart
  labs(title = "Percentage of Players Above/Below Median Income by Position",
       fill = "Income Category", x = NULL, y = NULL) +
  scale_fill_manual(values = c("Above Threshold" = "#8BC34A", "Below Threshold" = "#EF9A9A")) +
  theme_void() +
  theme(strip.text = element_text(size = 8, color = "black", face = "bold"),
        strip.background = element_blank())


#You can get a free API key here: https://api.census.gov/data/key_signup.html

census_api_key("0dd2a427da16332bb2af57709bcf6e34b0b1bf1a")

# Searching for variables
SEARCH_TERM <- "median income"
v2010 <- load_variables(2010, "acs1", cache = TRUE)
v2010 |> 
  mutate(concept_lower = str_to_lower(concept)) |>
  filter(str_detect(string = concept_lower, pattern = SEARCH_TERM)) |>
  print(n = 50)


# Get relevant variable names.

# Example
city_level_income_2010 <- get_acs(
  geography = "place", 
  variables = c(medincome = "B19013_001"), 
  year = 2010)


# Tract level income in TX 2010
tract_level_income_tx_2010 <- get_acs(
  geography = "tract", 
  state = "TX",
  variables = c(medincome = "B19013_001"), 
  year = 2010, 
  geometry = TRUE)

map_income_tx_2010 <- ggplot(tract_level_income_tx_2010, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c(labels = scales::dollar)

map_income_tx_2010
# Tract level income in CA 2010

tract_level_income_ca_2010 <- get_acs(
  geography = "tract", 
  state = "CA",
  variables = c(medincome = "B19013_001"), 
  year = 2010, 
  geometry = TRUE)

map_income_ca_2010 <- ggplot(tract_level_income_ca_2010, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c(labels = scales::dollar)

map_income_ca_2010

# Tract level income in FL 2010
tract_level_income_fl_2010 <- get_acs(
  geography = "tract", 
  state = "FL",
  variables = c(medincome = "B19013_001"), 
  year = 2010, 
  geometry = TRUE)

map_income_fl_2010 <- ggplot(tract_level_income_fl_2010, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c(labels = scales::dollar)
map_income_fl_2010

# Tract level income in GA 2010
tract_level_income_ga_2010 <- get_acs(
  geography = "tract", 
  state = "GA",
  variables = c(medincome = "B19013_001"), 
  year = 2010, 
  geometry = TRUE)

map_income_ga_2010 <- ggplot(tract_level_income_ga_2010, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c(labels = scales::dollar)

map_income_ga_2010

# Tract level income in OH 2010
tract_level_income_oh_2010 <- get_acs(
  geography = "tract", 
  state = "OH",
  variables = c(medincome = "B19013_001"), 
  year = 2010, 
  geometry = TRUE)

map_income_oh_2010 <- ggplot(tract_level_income_oh_2010, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c(labels = scales::dollar)
map_income_oh_2010

#sample visualization of medium income by state

income_by_state <- income_by_city |>
  filter(!is.na(state_abbr)) |>
  group_by(state_abbr) |>
  summarise(
    med_inc_state = mean(med_income, na.rm = TRUE)
  )

income_by_state |>
  ggplot(aes(x = state_abbr, y = med_inc_state)) +
  geom_point() +
  labs(
    title = "Median Household Income By State",
    x = "State",
    y = "Income ($)"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90)
  )

#sample state: New Jersey

income_nj <- income_by_city |>
  filter(state_abbr == "NJ")

income_nj |>
  ggplot(aes(x = city, y = med_income)) +
  geom_point() +
  labs(
    title = "Median Household Income by Cities in New Jersey",
    x = "City",
    y = "Income ($)"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# Stage 5:   project final report ----------------------------------------------

library(tidyverse)
library(stringr)
library(rvest)
library(scales)
library(httr2)
library(tibble)
library(xml2)

data <- read.csv("merged_data")
data

result_anova <- aov(med_income ~ POS, data = data)
summary(result_anova)


# All rights reserved