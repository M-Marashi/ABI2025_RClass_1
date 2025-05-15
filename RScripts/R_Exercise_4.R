# Install and load necessary packages using pacman
pacman::p_load(conflicted, tidyverse, wrappedtools, ggbeeswarm, hrbrthemes)

# Resolve conflicts, prefer dplyr's filter function
conflicts_prefer(dplyr::filter)

# Load the penguins dataset
data(penguins)

# Preview the dataset
head(penguins)


## Clean the data by removing rows with NA in relevant columns
# Option A
penguins_clean <- penguins |>
  filter(
    !is.na(sex),
    !is.na(species),
    !is.na(body_mass),
    !is.na(flipper_len)
  )

# Option B
penguins_clean <- penguins[!is.na(penguins$sex) &
  !is.na(penguins$species) &
  !is.na(penguins$body_mass) &
  !is.na(penguins$flipper_len), ]

## view and check
print(sex_count)
colnames(penguins_clean)
nrow(penguins_clean)
head(penguins)

## vizualize the Count sex within species
ggplot(penguins_clean, aes(species)) +
  geom_bar(fill = "darkorange")

ggplot(penguins_clean, aes(species, fill = sex)) +
  geom_bar()
ggplot(penguins_clean, aes(fill = species, x = sex)) +
  geom_bar()

## Boxplot + beeswarm: body mass vs species
penguins_clean |>
  ggplot(aes(species, body_mass, fill = sex)) +
  geom_boxplot(outlier.alpha = 0, alpha = .1) +
  geom_beeswarm(alpha = .5, cex = .9, shape = 21, dodge.width = .75) +
  labs(title = "Body Mass vs Species", y = "Body Mass", x = "Species")

## Boxplot + beeswarm: body mass vs species and sex
penguins_clean |>
  ggplot(aes(x = interaction(species, sex), y = body_mass, fill = sex)) +
  geom_boxplot(outlier.alpha = 0, alpha = .1) +
  geom_beeswarm(alpha = .5, cex = .9, shape = 21, dodge.width = .75) +
  labs(
    title = "Body Mass by Species and Sex",
    x = "Species and Sex",
    y = "Body Mass"
  )

## Scatterplot: flipper length vs body mass with regression
penguins_clean |>
  ggplot(aes(body_mass, flipper_len)) +
  geom_point()

penguins_clean |>
  ggplot(aes(body_mass, flipper_len, colour = species)) +
  geom_point(alpha = .5)

penguins_clean |>
  ggplot(aes(x = flipper_len, y = body_mass)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm") +
  labs(
    title = "Flipper Length vs Body Mass",
    x = "Flipper Length",
    y = "Body Mass"
  )

baseplot <-
  penguins_clean |>
  ggplot(aes(body_mass, flipper_len, colour = species, shape = sex)) +
  geom_point(size = 5) +
  scale_color_manual(values = c("royalblue", "orangered4", "#FFA500")) +
  scale_shape_manual(values = c("\u2640", "\u2642"))

baseplot

baseplot <-
  penguins_clean |>
  ggplot(aes(body_mass, flipper_len, colour = species, shape = sex)) +
  geom_point(size = 5) +
  scale_color_manual(values = c("royalblue", "orangered4", "#FFA500")) +
  scale_shape_manual(values = c("\u2640", "\u2642")) +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(
    method = lm, se = FALSE,
    color = "black",
    aes(group = "all")
  )

baseplot

## Scatterplot: flipper length vs body mass with regression, grouped by species and sex
penguins_clean |>
  ggplot(aes(x = flipper_len, y = body_mass)) +
  geom_point(aes(color = species, shape = sex)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Flipper Length vs Body Mass",
    x = "Flipper Length",
    y = "Body Mass"
  )

penguins_clean |>
  ggplot(aes(x = flipper_len, y = body_mass)) +
  geom_point(aes(color = species, shape = sex)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Flipper Length vs Body Mass",
    x = "Flipper Length",
    y = "Body Mass"
  )

penguins_clean |>
  ggplot(aes(x = flipper_len, y = body_mass, shape = sex)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Flipper Length vs Body Mass",
    x = "Flipper Length",
    y = "Body Mass"
  )

penguins_clean |>
  ggplot(aes(x = flipper_len, y = body_mass)) +
  geom_point(aes(color = species, shape = sex)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Flipper Length vs Body Mass",
    x = "Flipper Length",
    y = "Body Mass"
  )

## Define your own colors for species(scale or name or rgb)
# Custom outline colors for species (used in geom_beeswarm)
species_colors <- c(
  "Adelie" = "#1b9e77", # Teal
  "Chinstrap" = "#d95f02", # Orange
  "Gentoo" = "#7570b3" # Purple
)

penguins_clean |>
  ggplot(aes(x = interaction(species, sex), y = body_mass, fill = sex)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.1) +
  geom_beeswarm(aes(color = species), alpha = 0.5, cex = 0.9, shape = 21, dodge.width = 0.75) +
  scale_color_manual(values = species_colors) +
  labs(
    title = "Body Mass by Species and Sex",
    x = "Species and Sex",
    y = "Body Mass)"
  ) +
  theme(legend.position = "top")

## Apply custom colors to previous plot
penguins_clean |>
  ggplot(aes(x = interaction(species, sex), y = body_mass, fill = sex)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.1) +
  geom_beeswarm(aes(color = species),
    alpha = 0.5, cex = 0.9, shape = 21, dodge.width = 0.75
  ) +
  scale_color_manual(values = species_colors) +
  labs(
    title = "Body Mass by Species and Sex",
    x = "Species and Sex",
    y = "Body Mass"
  ) +
  theme(legend.position = "top")


## Visualise bill depth vs. body mass, scatter only
# optionA
penguins_clean |>
  ggplot(aes(x = body_mass, y = bill_dep)) + # Set the variables for the scatter plot
  geom_point(alpha = .25) + # Scatter plot with some transparency
  geom_smooth(method = "lm", color = "orange", se = TRUE) + # Linear regression (with confidence interval)
  geom_smooth(method = "loess", color = "blue", se = TRUE) + # Non-linear regression (LOESS, with confidence interval)
  facet_grid(sex ~ species, margins = TRUE) + # Facet by sex and species, include margins
  labs(
    title = "Bill Depth vs. Body Mass in Penguins",
    x = "Body Mass (g)",
    y = "Bill Depth (mm)"
  ) +
  theme_minimal()

# option B
ggplot(data = penguins_clean, aes(x = body_mass, y = bill_dep)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  facet_grid(sex ~ species, margins = TRUE) + # facet_grid(rows = vars(sex), cols = vars(species), margins=TRUE)
  labs(
    title = "Bill Depth vs. Body Mass in Penguins",
    x = "Body Mass",
    y = "Bill Depth"
  )

# option C
ggplot(data = penguins_clean, aes(x = body_mass, y = bill_dep)) +
  geom_point(aes(color = species, shape = sex)) +
  geom_smooth(method = "lm") +
  facet_grid(rows = vars(sex), cols = vars(species), margins = TRUE) +
  labs(
    title = "Bill Depth vs. Body Mass in Penguins",
    x = "Body Mass",
    y = "Bill Depth"
  )

# Option D
facet_grid(sex ~ species, margins = TRUE)
theme(
  strip.background = element_rect(fill = "lightgray"),
  strip.text = element_text(face = "bold")
)
ggplot(data = penguins_clean, aes(x = body_mass, y = bill_dep)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(color = "Linear"), method = "lm", se = FALSE, linetype = "dashed") +
  geom_smooth(aes(color = "LOESS"), method = "loess", se = FALSE) +
  facet_grid(sex ~ species, margins = TRUE) +
  labs(
    title = "Bill Depth vs. Body Mass in Penguins",
    x = "Body Mass (g)",
    y = "Bill Depth (mm)",
    color = "Regression Type"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )
