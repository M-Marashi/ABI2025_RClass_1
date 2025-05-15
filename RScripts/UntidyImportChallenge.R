pacman::p_load(conflicted, tidyverse, readxl, here, wrappedtools)

# Read the entire sheet into R
data <- read_excel(path = here("Data/UntidyImportChallenge.xlsx"))

# View data to understand the structure of your data
view(data)

# Import 3 Sub-Tables into 3 Variables:
table_1 <- read_excel(path = here("Data/UntidyImportChallenge.xlsx"), sheet = 1, range = "A4:E11")
table_2 <- read_excel(path = here("Data/UntidyImportChallenge.xlsx"), sheet = 1, range = "G4:K12")
table_3 <- read_excel(path = here("Data/UntidyImportChallenge.xlsx"), sheet = 1, range = "M4:Q11")


# Combine all tables
combined_data <- bind_rows(
  fast = table_1, medium = table_2, slow = table_3,
  .id = "Tumorgrowth"
) |>
  rename(
    Treatment = "Start-Day :",
    AnimalCode = "Meas./Treatm."
  ) |>
  rename_with(
    .fn = ~ paste("Weight [g]", .x),
    .cols = contains(" h")
  ) |>
  fill(Treatment) |>
  arrange(Treatment) |>
  distinct(AnimalCode,
    .keep_all = TRUE
  )


# View the cleaned dataset
View(combined_data)

## pivot_longer to change the format of table (from wide to long)
# Option A
combined_data_long <- pivot_longer(
  data = combined_data,
  cols = contains("Weight"),
  names_to = "Time",
  names_pattern = "Weight \\[g\\] (\\d+) h",
  values_to = "Weight"
)
# View the result
View(combined_data_long)


# Option B
combined_data_long_1 <- pivot_longer(
  data = combined_data,
  cols = starts_with("Weight"),
  names_to = c(".value", "Time"),
  names_pattern = "(.*\\]) (\\d+) h"
)

# View the result
View(combined_data_long_1)
