pacman::p_load(
  conflicted, tidyverse, here,
  grid, gridExtra, car,
  ggsci, ggsignif, ggthemes, ggridges,
  # gganimate,
  ggforce,
  ggbeeswarm,
  wrappedtools,
  emojifont,
  patchwork,
  ggh4x
)
conflicts_prefer(dplyr::filter) # solves name conflict
theme_set(theme_grey(base_size = 20))

(plot_tmp <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point())
plot_tmp + facet_grid(
  rows = vars(gear),
  cols = vars(cyl)
)
cat("facet labeling improved:\n")
plot_tmp + facet_grid(
  rows = vars(gear),
  cols = vars(cyl),
  labeller = label_both, margins = "gear"
)
# options(warn=-1)

### option 1
mtcars2 <- mtcars %>%
  mutate(
    cyl = factor(cyl),
    gear = factor(gear)
  )
# Create marginal versions of the data
margins_manual <- bind_rows(
  mtcars2 %>% mutate(facet_type = "normal"),
  mtcars2 %>% mutate(gear = "All", facet_type = "cyl_all"),
  mtcars2 %>% mutate(cyl = "All", facet_type = "gear_all"),
  mtcars2 %>% mutate(
    gear = "All", cyl = "All", facet_type =
      "both_all"
  )
)

# Ensure factors include "All" for plotting
margins_manual <- margins_manual %>%
  mutate(
    gear = factor(gear, levels = c("3", "4", "5", "All")),
    cyl = factor(cyl, levels = c("4", "6", "8", "All"))
  )

# Plot with color by facet_type
ggplot(margins_manual, aes(wt, mpg, color = facet_type)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_grid(rows = vars(gear), cols = vars(cyl), labeller = label_both) +
  scale_color_manual(values = c(
    normal = "black",
    gear_all = "blue",
    cyl_all = "green",
    both_all = "red"
  )) +
  # Add background tiles to highlight the panels for `gear: (all)` and `cyl: (all)`
  geom_tile(
    data = subset(margins_manual, gear == "All"), aes(x = wt, y = mpg),
    color = "black", fill = NA, size = 2, inherit.aes = FALSE
  ) + # Thicker border for gear: (all)
  geom_tile(
    data = subset(margins_manual, cyl == "All"), aes(x = wt, y = mpg),
    color = "black", fill = NA, size = 2, inherit.aes = FALSE
  )


## option 2
mtcars2 <- mtcars %>%
  mutate(
    cyl = factor(cyl),
    gear = factor(gear)
  )
# Create marginal versions of the data
margins_manual <- bind_rows(
  mtcars2 %>% mutate(facet_type = "normal"),
  mtcars2 %>% mutate(gear = "All", facet_type = "cyl_all"),
  mtcars2 %>% mutate(cyl = "All", facet_type = "gear_all"),
  mtcars2 %>% mutate(gear = "All", cyl = "All", facet_type = "both_all")
)

# Ensure factors include "All" for plotting
margins_manual <- margins_manual %>%
  mutate(
    gear = factor(gear, levels = c("3", "4", "5", "All")),
    cyl = factor(cyl, levels = c("4", "6", "8", "All"))
  )
# Plot with color by facet_type
ggplot(margins_manual, aes(wt, mpg, color = facet_type)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_grid(rows = vars(gear), cols = vars(cyl), labeller = label_both) +
  scale_color_manual(values = c(
    normal = "black",
    gear_all = "blue",
    cyl_all = "green",
    both_all = "red"
  ))
