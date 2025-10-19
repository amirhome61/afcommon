# Source the release function
source("R/af_packages.R")

# Run it with your repo details and version bump type
af_release_package(
  package_path = ".",
  github_repo = "amirhome61/afcommon",
  version_bump = "minor",  # change to "patch" or "major" as needed
  release_message = "New package release and create functions"
)
