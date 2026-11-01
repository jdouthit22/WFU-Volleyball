read.csv("acc_rosters.csv")
ACC_roster = read.csv("acc_rosters.csv")

# Loading + quick look
library(tidyverse)

raw <- read_csv("acc_rosters.csv", show_col_types = FALSE)

glimpse(raw)
colSums(is.na(raw))
count(raw, class, sort = TRUE)

#Dimensions
dim(raw)     # rows, columns
nrow(raw)    # just rows
ncol(raw)    # just columns
names(raw)   # column names

head(raw, 10)

summary(raw)

#Convert height to inches
height_parts <- str_match(raw$height, "^(\\d)\\D+(\\d{1,2})")

rosters <- raw |>
  mutate(
    height_in = as.numeric(height_parts[, 2]) * 12 +
      as.numeric(height_parts[, 3])
  )

#Standardize class year
rosters <- rosters |>
  mutate(
    class_tmp = class |>
      str_to_lower() |>
      str_remove_all("\\.") |>
      str_squish(),
    redshirt  = str_detect(class_tmp, "^(r-|rs |redshirt )"),
    class_tmp = str_remove(class_tmp, "^(r-|rs |redshirt )"),
    class_year = case_when(
      str_detect(class_tmp, "^(fr|freshman|1st)") ~ "Freshman",
      str_detect(class_tmp, "^(so|sophomore|2nd)") ~ "Sophomore",
      str_detect(class_tmp, "^(jr|junior|3rd)")    ~ "Junior",
      str_detect(class_tmp, "^(sr|senior|4th)")    ~ "Senior",
      str_detect(class_tmp, "^(5th|fifth|gr)")     ~ "Grad/5th",
      TRUE ~ NA_character_
    ),
    class_year = factor(class_year,
                        levels = c("Freshman", "Sophomore", "Junior", "Senior", "Grad/5th"),
                        ordered = TRUE),
    class_num = as.integer(class_year)
  ) |>
  select(-class_tmp)

count(rosters, class, class_year, redshirt) |> print(n = Inf)


#Fix positions
rosters <- rosters |>
  mutate(
    position_std = if_else(is.na(position_std) & position == "A",
                           "Pin", position_std),
    position_group = case_when(
      position_std %in% c("Outside", "Opposite", "Pin") ~ "Pin Hitter",
      position_std == "Middle"                          ~ "Middle",
      position_std == "Setter"                          ~ "Setter",
      position_std %in% c("Libero", "DS")               ~ "Defense"
    )
  )

count(rosters, position_group, position_std)

#Convert Hometown to 2-Letter state
state_lookup <- c(
  al = "AL", az = "AZ", ariz = "AZ", arizona = "AZ",
  ca = "CA", calif = "CA", co = "CO", colo = "CO", conn = "CT",
  dc = "DC", fl = "FL", fla = "FL", ga = "GA",
  hawaii = "HI", ill = "IL", ind = "IN", indiana = "IN", iowa = "IA",
  ks = "KS", kan = "KS", kansas = "KS", ky = "KY", md = "MD",
  mi = "MI", mich = "MI", michigan = "MI", minn = "MN",
  mo = "MO", missouri = "MO", nc = "NC", northcarolina = "NC",
  ny = "NY", ne = "NE", neb = "NE", nj = "NJ", nev = "NV",
  ohio = "OH", or = "OR", ore = "OR",
  pa = "PA", penn = "PA", pennsylvania = "PA",
  sc = "SC", tn = "TN", tenn = "TN", tx = "TX", texas = "TX",
  utah = "UT", va = "VA", wash = "WA",
  wi = "WI", wis = "WI", wisc = "WI",
  puertorico = "PR"
)


# Fix typos and put the high schools in its own column
rosters <- rosters |>
  mutate(
    high_school = str_split_i(hometown, " / ", 2) |> str_squish(),
    home        = str_split_i(hometown, " / ", 1) |> str_squish(),
    home        = str_replace(home, "^Dallas Texas$", "Dallas, Texas"),
    
    city   = if_else(str_detect(home, ","),
                     str_remove(home, ",[^,]*$") |> str_squish(),
                     NA_character_),
    region = str_extract(home, "[^,]+$") |> str_squish(),
    
    region_key = region |> str_to_lower() |> str_remove_all("[.'\\s]"),
    state      = unname(state_lookup[region_key]),
    
    country = case_when(
      !is.na(state)       ~ "USA",
      region == "Ontario" ~ "Canada",
      TRUE                ~ region
    ),
    international = country != "USA"
  ) |>
  select(-home, -region_key)

rosters |> filter(is.na(state)) |> count(region, country) |> print(n = Inf)


#Tidy and reorder columns
rosters_clean <- rosters |>
  mutate(
    name       = str_squish(name),
    team       = factor(team),
    jersey     = as.integer(jersey),
    scraped_on = as.Date(scraped_on)
  ) |>
  select(team, jersey, name,
         position_raw = position, position_std, position_group,
         class_raw = class, class_year, class_num, redshirt,
         height_raw = height, height_in,
         city, state, country, international, high_school,
         source_url, scraped_on)

# Stop with an error if any check fails; no output means all passed
stopifnot(
  nrow(rosters_clean) == nrow(raw),
  !anyDuplicated(rosters_clean[, c("team", "jersey")]),
  all(!is.na(rosters_clean$class_year)),
  all(!is.na(rosters_clean$position_group))
)

# Min, quartiles, mean, max, and NA count for height
# Expect 60 (5'0") to 79 (6'7"); anything outside means a parsing error
summary(rosters_clean$height_in)

rosters_clean |>
  group_by(team) |>            # one group per team
  summarise(players = n(), missing_height = sum(is.na(height_in))) |>      # roster size 
  filter(missing_height > 0)                 # show only teams with gaps

write_excel_csv(rosters_clean, "acc_rosters_clean.csv")  


rosters_clean |>
  filter(!is.na(height_in)) |>
  group_by(position_group) |>
  summarise(n = n(), avg_height_in = round(mean(height_in), 1)) |>
  arrange(desc(avg_height_in))


write_excel_csv(rosters_clean, "~/Downloads/acc_rosters_clean.csv")
