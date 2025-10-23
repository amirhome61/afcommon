# This script automates the release process for the afcommon package.

# Install librarian if not already installed
if (!("librarian" %in% rownames(installed.packages()))) {
  install.packages("librarian")
}

librarian::shelf(desc, remotes, devtools, usethis, git2r)
librarian::unshelf(afcommon)

# Source the release function
source("R/af_packages.R")

# Run it with your repo details and version bump type
new_version <- af_release_package(
  package_path = ".",
  github_repo = "amirhome61/afcommon",
  version_bump = "minor", # change to "patch", "minor", or "major" as needed
  release_message = "Testing before major release"
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
