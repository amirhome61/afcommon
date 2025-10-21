# This script automates the release process for the afcommon package.

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

# Unload the package if it's already loaded
if ("package:afcommon" %in% search()) {
  detach("package:afcommon", unload = TRUE)
}

# Source the release function
source("R/af_packages.R")

# Run it with your repo details and version bump type
new_version <- af_release_package(
  package_path = ".",
  github_repo = "amirhome61/afcommon",
  version_bump = "minor", # change to "patch" or "major" as needed
  release_message = "New package release and create functions"
)

# Automatically remove any functions in the global environment that start with "af_"
to_remove <- ls(envir = .GlobalEnv, pattern = "^af_")
if (length(to_remove) > 0) {
  rm(list = to_remove, envir = .GlobalEnv)
  message(sprintf(
    "Removed %d local af_* function(s) from global environment.",
    length(to_remove)
  ))
}

# --- Install and reload the new version cleanly ---
unloadNamespace("afcommon") # Ensure nothing is attached
remotes::install_github(paste0("amirhome61/afcommon@v", new_version))

library(afcommon)

message(sprintf("✅ afcommon version %s loaded successfully.", new_version))
