# Create a new R package with standard structure and files

if (!requireNamespace("desc", quietly = TRUE)) {
  install.packages("desc")
}
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}
if (!requireNamespace("git2r", quietly = TRUE)) {
  install.packages("git2r")
}
library(desc)
library(remotes)
library(devtools)
library(usethis)
library(git2r)

library(afcommon)

source("R/af_packages.R")

af_create_package(
  package_name = "afextremism",
  package_path = "C:/Users/USER/Dropbox/Amir/Amir personal/R",
  github_repo = "https://github.com/amirhome61/afextremism",
  author_name = "Amir Freund",
  author_email = "amir@email.com"
)
