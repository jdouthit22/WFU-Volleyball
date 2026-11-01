install.packages("datavolley",
                 repos = c("https://openvolley.r-universe.dev", "https://cloud.r-project.org"))

#Open files
library(datavolley)
library(dplyr)

files <- list.files("~/Downloads", pattern = "\\.dvw$",
                    recursive = TRUE, full.names = TRUE)
files


x <- dv_read(files[1], skill_evaluation_decode = "volleymetrics")
summary(x)

unzip(file.choose(), exdir = "~/Downloads/dvw_files")
files <- list.files("~/Downloads/dvw_files", pattern = "\\.dvw$",
                    recursive = TRUE, full.names = TRUE)
files


library(datavolley)
library(dplyr)

all_plays <- lapply(files, function(f) {
  plays(dv_read(f, skill_evaluation_decode = "volleymetrics"))
}) |> bind_rows()

dim(all_plays)   # rows x columns


#Real touches only
touches <- all_plays |>
  filter(!is.na(skill))

count(touches, skill)   # Serve, Reception, Set, Attack, Block, Dig, Freeball


#Keep the columns you need and add helpful labels
clean <- touches |>
  mutate(
    player_name = trimws(player_name),
    opponent    = if_else(grepl("Clemson", home_team), visiting_team, home_team),
    clemson     = grepl("Clemson", team),
    won_point   = point_won_by == team
  ) |>
  select(match_id, opponent, set_number, point_id, team, clemson,
         player_number, player_name, skill, skill_type, evaluation_code,
         evaluation, attack_code, start_zone, end_zone, phase,
         home_team_score, visiting_team_score, won_point)

#Checking for problems
# Missing values per column
colSums(is.na(clean))

# Names spelled differently across matches
clean |> distinct(team, player_number, player_name) |> arrange(team, player_number)

# Each match should be there
count(clean, opponent)

#Saving it
write.csv(clean, "~/Downloads/clemson_oct_plays_clean.csv", row.names = FALSE)


#Quick check: Clemson attack efficiency by player
clean |>
  filter(clemson, skill == "Attack") |>
  group_by(player_name) |>
  summarise(
    attempts = n(),
    kills    = sum(evaluation_code == "#"),
    errors   = sum(evaluation_code %in% c("=", "/")),
    hit_pct  = round((kills - errors) / attempts, 3)
  ) |>
  arrange(desc(attempts))
