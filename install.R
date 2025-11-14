# install.R
packages <- c(
  "shiny",
  "DT",
  "dplyr",
  "DBI",
  "RSQLite",
  "pool",
  "shinyjs"
)

install.packages(packages, repos = "https://cloud.r-project.org")
