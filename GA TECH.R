library(datavolley)
library(dplyr)

# 1. Find the GT files (unzip first if character(0) prints)
gt_files <- list.files("~/Downloads", pattern = "GT-.*\\.dvw$",
                       recursive = TRUE, full.names = TRUE)
gt_files   # should print 4 paths

# If it printed character(0), run these two lines and pick the new zip:
unzip(file.choose(), exdir = "~/Downloads/gt_dvw_files")
gt_files <- list.files("~/Downloads/gt_dvw_files", pattern = "\\.dvw$", recursive = TRUE, full.names = TRUE)

# 2. Read and stack all four matches
gt_all <- lapply(gt_files, function(f) {
  plays(dv_read(f, skill_evaluation_decode = "volleymetrics"))
}) |> bind_rows()

# 3. Keep real touches, add labels, trim columns
gt_clean <- gt_all |>
  filter(!is.na(skill)) |>
  mutate(
    player_name = trimws(player_name),
    opponent    = if_else(grepl("Georgia", home_team), visiting_team, home_team),
    gatech      = grepl("Georgia", team),
    won_point   = point_won_by == team
  ) |>
  select(match_id, opponent, set_number, point_id, team, gatech,
         player_number, player_name, skill, skill_type, evaluation_code,
         evaluation, attack_code, start_zone, end_zone, phase,
         home_team_score, visiting_team_score, won_point)

# 4. Quality checks
colSums(is.na(gt_clean))
count(gt_clean, opponent)   # should show BC, Pitt, SMU, Syracuse
gt_clean |> distinct(team, player_number, player_name) |> arrange(team, player_number)

# 5. Save
write.csv(gt_clean, "~/Downloads/gatech_oct_plays_clean.csv", row.names = FALSE)
