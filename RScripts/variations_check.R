pacman::p_load(tidyverse, lubridate)
# Exercise

testset1 <- c("Meier", "Mayer", "Maier", "Meyer", "Mayr", "Maya", "Mayor")
# find all variations of the name "Meier" (not Maya or Mayor)

str_detect(string = testset1, pattern = "^M[ae][iy]e?r$")

testset2 <- c("weight_mm", "height_cm", "age_yr", "temp_c")
# replace _ with space
# replace _ with space and add unit in brackets

testset2 |> str_replace(pattern = "-", replacement = "")
testset2 |> str_replace(pattern = "-(.+)", " [\\1]")
testset2 |> str_view("-(.+)")

testset3 <- c("1980_12_30", "13.04.2005", "2005/04/25", "24121990")
# transform into YYYY-MM-DD
testset3 |>
  str_replace_all(
    c(
      "(\\d{4})[_/](\\d{2})[_/](\\d{2})" = "\\1-\\2-\\3",
      "([0123]\\d)([01]\\d)([12]\\d{3})" = "\\3-\\2-\\1",
      "(\\d{2})\\.(\\d{2})\\.(\\d{4})" = "\\3-\\2-\\1"
    )
  )

## alternative solution for testset3:
testset3 <- c("1980_12_30", "13.04.2005", "2005/04/25", "24121990")

# Define possible formats
formats <- c("Y_m_d", "d.m.Y", "Y/m/d", "dmy")

# Parse and convert to Date
parsed_dates <- parse_date_time(testset3, orders = formats)

# Format into YYYY-MM-DD
formatted_dates <- format(parsed_dates, "%Y-%m-%d")

formatted_dates


testset4 <- c("pw2000", "That1sb3tt3r", "M@kesSense?", "NoDigits@this1")
# test pwd strength, rules: Upper, lower, special char, number, min 8 char long
# Apply all checks using logical AND on separate str_detect() calls
strong_passwords <- testset4[
  str_detect(testset4, "[A-Z]") & # has uppercase
    str_detect(testset4, "[a-z]") & # has lowercase
    str_detect(testset4, "[0-9]") & # has digit
    str_detect(testset4, "\\W") & # has special character (non-word character)
    str_detect(testset4, ".{8,}") # at least 8 characters
]

strong_passwords
