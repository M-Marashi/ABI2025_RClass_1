# ===============================
# Tumor Data Cleaning Script
# ===============================

# --- Load Required Packages ---
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  conflicted, tidyverse, readxl, here, wrappedtools
)

# Optional: Set preferred function conflict resolutions
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# --- Define Cleaning Function ---
clean_tumor_data <- function(file_path, table_ranges, labels = NULL) {
  # Assign default labels if none provided
  if (is.null(labels)) {
    labels <- paste0("Table", seq_along(table_ranges))
  }

  # Read each sub-table with its label
  tables <- map2(
    table_ranges, labels,
    ~ read_excel(path = file_path, sheet = 1, range = .x) %>%
      mutate(Tumorgrowth = .y)
  )

  # Combine and clean all tables
  combined_data <- bind_rows(tables) %>%
    rename(
      Treatment = `Start-Day :`,
      AnimalCode = `Meas./Treatm.`
    ) %>%
    rename_with(~ paste("Weight [g]", .x), .cols = contains(" h")) %>%
    fill(Treatment) %>%
    arrange(Treatment) %>%
    distinct(AnimalCode, .keep_all = TRUE)

  return(combined_data)
}

# --- Example Usage ---

# Define Excel file path
excel_file <- here("Data/UntidyImportChallenge.xlsx")

# Define sub-table ranges and their labels
ranges <- c("A4:E11", "G4:K12", "M4:Q11")
labels <- c("fast", "medium", "slow")

# Clean the data
cleaned_data <- clean_tumor_data(file_path = excel_file, table_ranges = ranges, labels = labels)

# View cleaned data
View(cleaned_data)

# --- Optional: Save the cleaned dataset as CSV ---
write_csv(cleaned_data, here("Output/cleaned_tumor_data.csv"))
