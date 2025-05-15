# define and import packages to be used
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  conflicted, # tests/solutions for name conflicts
  tidyverse, # metapackage
  wrappedtools # my own tools package
)

conflict_scout()
conflicts_prefer(
  dplyr::filter,
  stats::lag
)

## Exercise: Think of a cruet_stand / Gewürzmenage
# define the number of elements
n_elements <- 5 * 10^3

# create a tibble "menage" with columns saltshaker, peppercaster and n_elements each for saltgrain and pepperflake
menage <- tibble(
  saltshaker = paste0("saltgrains", 1:n_elements), # alternatively use rep("saltgrains", n_elements)
  peppercaster = paste0("pepperflake", 1:n_elements), # alternatively use rep("saltgrains", n_elements)
)
str(menage)

# print saltshaker
menage$saltshaker # or menage[1]
menage |> select(saltshaker)
select(.data = menage, saltshaker)

# print salt
menage[["saltshaker"]] # menage$saltshaker
menage |> pull(saltshaker)
pull(menage, saltshaker)

# print 100 saltgrains
menage$saltshaker[1:100] # menage[[2]][1:100]
menage |>
  slice(1:100) |>
  pull(saltshaker)
