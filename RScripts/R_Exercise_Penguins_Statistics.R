pacman::p_load(conflicted, tidyverse, wrappedtools, here, ggplot2, dplyr, patchwork, xkcd, extrafont)
conflicts_prefer(dplyr::filter) # solves name conflict

## Load the penguins dataset
data(penguins)

## Preview the dataset
head(penguins)

## Clean the data by removing rows with NA in relevant columns
penguins_clean <- penguins |>
  filter(
    !is.na(sex),
    !is.na(species),
    !is.na(body_mass),
    !is.na(flipper_len)
  )
## view and check
colnames(penguins_clean)
nrow(penguins_clean)
head(penguins_clean)

### Explore Normallity in the Penguins data set. Use figures and various Tests, in total sample as well as sub-groups
## Kolmogorov-Smirnov (KS) test to compare body_mass to a normal distribution

ksOut <- ks.test(penguins_clean$body_mass, "pnorm", # comparing the sample against the normal distribution (pnorm)
  mean(penguins_clean$body_mass, na.rm = TRUE), # Removes NA values so the test isn't thrown off by missing data (na.rm =T)
  sd(penguins_clean$body_mass, na.rm = TRUE),
  exact = FALSE
)

ksnormal(penguins_clean$body_mass, lillie = FALSE) # similar to ks.test(), but it's simplified and tailored to normality checks
shapiro.test(penguins_clean$body_mass) # To test whether sample data comes from a normally distributed population

## Sub-group analysis
penguins_clean |>
  group_by(species) |>
  summarise(shapiro_p = shapiro.test(body_mass)$p.value)

penguins_clean |>
  group_by(sex) |>
  summarise(shapiro_p = shapiro.test(body_mass)$p.value)

### visual check:
# Option A
penguins_clean |>
  ggplot(aes(x = body_mass)) +
  geom_density(fill = "pink")

ggplot(penguins_clean, aes(x = body_mass, fill = species)) +
  geom_density(alpha = .4)

# Option B to check and visualize the normality: Histogram and QQ plot
hist(penguins_clean$body_mass, main = "Histogram of body_mass", xlab = "body_mass")
qqnorm(penguins_clean$body_mass) # to visually assess whether a variable is normally distributed
qqline(penguins_clean$body_mass) # to visually assess whether a variable is normally distributed


### facet_grid
penguins_clean |>
  ggplot(aes(x = body_mass)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  # facet_grid(species ~ sex) +
  facet_grid(rows = vars(species), cols = vars(sex), margins = TRUE) +
  labs(
    title = "Density Plot of Body Mass by Species and Sex",
    x = "Body Mass", y = "Density"
  )

### facet_grid - combining density plot with histogram plot
## Shared color scale
my_palette <- scale_fill_brewer(palette = "Set2")

# Density plot (no legend)
p_density <- penguins_clean |>
  ggplot(aes(x = body_mass, fill = species)) +
  geom_density(alpha = 0.5) +
  facet_grid(rows = vars(species), cols = vars(sex), margins = TRUE) +
  labs(
    title = "Density Plot of Body Mass by Species and Sex",
    x = "Body Mass", y = "Density"
  ) +
  theme(legend.position = "none") +
  my_palette

# Histogram plot (no legend)
p_hist <- penguins_clean |>
  ggplot(aes(x = body_mass, fill = species)) +
  geom_histogram(bins = 30, color = "black", position = "identity", alpha = 0.6) +
  facet_grid(rows = vars(species), margins = TRUE) + # , cols = vars(sex),
  labs(
    title = "Histogram of Body Mass by Species and Sex",
    x = "Body Mass", y = "Count"
  ) +
  theme(legend.position = "none") +
  my_palette

# Combine side-by-side
p_density + p_hist + plot_layout(ncol = 2)

### across() - Version:
# to Confirm that the data loaded correctly, Verify column names, and Check for obvious issues (e.g., NAs, unexpected values)
head(penguins_clean)

# to identify and extract the numeric variables (columns) in the data set
numvars <- names(select(penguins_clean, where(is.numeric)))

result_table <- penguins_clean |>
  summarize(across(
    .cols = all_of(numvars),
    .fns = list(
      pKS = ~ ksnormal(.x) |>
        formatP(mark = TRUE), # adds significance markers or symbols (like asterisks) based on conventional thresholds
      pSh = ~ shapiro.test(.x)$p.value |>
        formatP(mark = TRUE)
    )
  )) |>
  pivot_longer(everything(),
    names_to = c("Variable", ".value"),
    # names_sep = "_p"
    names_pattern = "(.+)_(p.+)"
  )

head(result_table)

### Picking column names and positions using colseeker
gauss_var <- ColSeeker(
  data = penguins_clean,
  namepattern = c("mass", "len", "dep") # alternatively we can use c("_")
  # casesensitive = FALSE
)
gauss_var$names


### how creat result for report
numvars <- ColSeeker(
  data = penguins_clean,
  namepattern = c("bill", "flipper", "depth", "mass"),
  casesensitive = FALSE
)

result_table1 <- tibble(
  Measures = NA_character_,
  pKS_Adelie = NA_character_,
  pKS_Chinstrap = NA_character_,
  pKS_Gentoo = NA_character_,
  .rows = 0
)
for (var_i in numvars$names) {
  # Run normality test by species
  ks_tmp <- by(
    data = penguins_clean[[var_i]],
    INDICES = penguins_clean$species,
    FUN = ksnormal,
    lillie = FALSE
  )

  # Add formatted results to the result table
  result_table1 <- add_row(result_table1,
    Measures = var_i,
    pKS_Adelie = ks_tmp[["Adelie"]] |>
      formatP(),
    pKS_Chinstrap = ks_tmp[["Chinstrap"]] |>
      formatP(),
    pKS_Gentoo = ks_tmp[["Gentoo"]] |>
      formatP()
  )
}
print(result_table1)
