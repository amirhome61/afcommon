# This script automates the release process for the afcommon package.

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
  version_bump = "minor",  # change to "patch" or "major" as needed
  release_message = "New package release and create functions"
)

# --- Install and reload the new version cleanly ---
unloadNamespace("afcommon")  # Ensure nothing is attached
remotes::install_github(paste0("amirhome61/afcommon@v", new_version))
library(afcommon)

message(sprintf("✅ afcommon version %s loaded successfully.", new_version))