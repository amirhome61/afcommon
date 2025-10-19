# --- Automated R package release function with roxygen2 documentation ---
# Requires: desc, remotes, devtools packages
if (!requireNamespace("desc", quietly = TRUE)) {
  install.packages("desc")
}
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

library(desc)
library(remotes)
library(devtools)

#' Automate R Package Release Workflow
#'
#' `af_release_package()` automates the process of releasing a new version of an R package.
#' It increments the package version, updates documentation using roxygen2, commits all changes,
#' creates a Git tag, pushes commits and the tag to GitHub, and installs the new release locally.
#'
#' This function is designed to be reusable for any R package. It requires that the working directory
#' contains a valid R package structure with a DESCRIPTION file and that Git is initialized with a
#' remote repository configured.
#'
#' @param package_path Character string specifying the path to the root folder of the package.
#'   Defaults to the current working directory `"."`.
#' @param github_repo Character string specifying the GitHub repository in the format
#'   `"username/repo"`. This is used to push changes and install the release from GitHub.
#' @param version_bump Character string specifying which part of the semantic version to increment.
#'   Must be one of `"patch"`, `"minor"`, or `"major"`. Defaults to `"minor"`.
#' @param initial_version Optional character string specifying the version to use for the first release.
#'   If provided, the function will use this version instead of incrementing.
#' @param release_message Optional character string specifying a description for this release.
#'   Used as the Git tag message and GitHub release body.
#'
#' @return The new version string (character) after incrementing.
#'
#' @details
#' Steps performed by this function:
#' 1. Determine the new package version (optionally using `initial_version`).
#' 2. Update documentation using roxygen2.
#' 3. Stage and commit all changes in Git.
#' 4. Create an annotated Git tag with the release message.
#' 5. Push commits and tag to GitHub.
#' 6. Create a GitHub release with the provided release message.
#'
#' @examples
#' \dontrun{
#' af_release_package(
#'   package_path = "C:/Users/USER/Dropbox/Amir/Amir personal/R/afcommon",
#'   github_repo = "amirhome61/afcommon",
#'   version_bump = "minor" # Options: "patch", "minor", "major",
#'   initial_version = NULL # e.g., "0.1.0" for first release,
#'   release_message = "Added new utility functions and updated workflow documentation"
#' )
#' }
#'
#' @export
af_release_package <- function(
  package_path = ".",
  github_repo,
  version_bump = "minor",
  initial_version = NULL,
  release_message = NULL
) {
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
  library(desc)
  library(remotes)
  library(devtools)
  library(usethis)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(package_path)

  # ----------------------------
  # Ensure Git identity
  # ----------------------------
  system('git config user.name "Amir Freund"')
  system('git config user.email "amir61@gmail.com"')

  # ----------------------------
  # 1. Determine version
  # ----------------------------
  d <- desc::desc(file = "DESCRIPTION")
  old_version <- d$get_version()

  if (!is.null(initial_version)) {
    new_version <- initial_version
    message(sprintf("Using provided initial version: %s", new_version))
  } else {
    # Parse version components
    ver_parts <- unclass(old_version)[[1]]

    # Increment based on version_bump
    new_version <- switch(
      version_bump,
      "major" = package_version(paste(ver_parts[1] + 1, 0, 0, sep = ".")),
      "minor" = package_version(paste(
        ver_parts[1],
        ver_parts[2] + 1,
        0,
        sep = "."
      )),
      "patch" = package_version(paste(
        ver_parts[1],
        ver_parts[2],
        ver_parts[3] + 1,
        sep = "."
      )),
      stop("version_bump must be 'major', 'minor', or 'patch'")
    )
    d$set_version(new_version)
    d$write(file = "DESCRIPTION")
    message(sprintf("Version updated: %s → %s", old_version, new_version))
  }

  # ----------------------------
  # 2. Update documentation
  # ----------------------------
  suppressWarnings(
    suppressMessages(
      devtools::document()
    )
  )
  message("Documentation updated using roxygen2.")

  # ----------------------------
  # 3. Stage and commit changes
  # ----------------------------
  system("git add -A")
  commit_message <- sprintf("Update package to version %s", new_version)
  system(sprintf("git commit -m %s", shQuote(commit_message)))
  message("Committed changes.")

  # ----------------------------
  # 4. Create Git tag
  # ----------------------------
  tag_message <- ifelse(
    is.null(release_message),
    commit_message,
    release_message
  )
  tag_name <- paste0("v", new_version)
  system(sprintf("git tag -a %s -m %s", tag_name, shQuote(tag_message)))
  message(sprintf("Created Git tag: %s", tag_name))

  # ----------------------------
  # 5. Push commits and tag
  # ----------------------------
  system("git push")
  system(sprintf("git push origin %s", tag_name))
  message("Pushed commits and tag to GitHub.")

  # ----------------------------
  # 6. Create GitHub release
  # ----------------------------
  if (!is.null(github_repo)) {
    # Create GitHub release with notes (requires gh CLI installed)
    if (nzchar(Sys.which("gh"))) {
      system(sprintf(
        'gh release create %s --title "Version %s" --notes %s',
        tag_name,
        new_version,
        shQuote(tag_message)
      ))
      message(
        "GitHub release created with provided release message via gh CLI."
      )
    } else {
      message(
        "gh CLI not found; GitHub release not created. Install GitHub CLI to enable."
      )
    }
  }

  return(new_version)
}

#' Automate Initial Package Creation
#'
#' `af_create_package()` creates a new R package skeleton, initializes Git, optionally sets a GitHub
#' remote, adds LICENSE and DESCRIPTION metadata, and generates a release runner script for the package.
#' The release runner script automatically sources `af_packages.R` from the installed `afcommon` library.
#'
#' @param package_name Character string specifying the name of the new package.
#' @param package_path Character string specifying the path where the package should be created.
#'   Defaults to the current working directory `"."`.
#' @param github_repo Optional character string specifying the GitHub repository in the format
#'   `"username/repo"`. If provided, the function will add it as the remote and push the initial commit.
#' @param author_name Character string with the author name for DESCRIPTION and LICENSE.
#' @param author_email Character string with the author email for DESCRIPTION.
#' @param license Character string specifying the license type. Defaults to `"MIT"`.
#'
#' @return The full path of the newly created package (invisibly).
#'
#' @details
#' Steps performed by this function:
#' 1. Creates the package skeleton using `usethis::create_package()`.
#' 2. Sets DESCRIPTION fields: author, email, license.
#' 3. Adds LICENSE file.
#' 4. Initializes Git and creates the initial commit.
#' 5. Adds GitHub remote and pushes initial commit if `github_repo` is provided.
#' 6. Creates a `release_<package>.R` script in the package root that sources `af_packages.R`
#'    from the installed `afcommon` library and calls `af_release_package()` for this package.
#'
#' @examples
#' \dontrun{
#' af_create_package(
#'   package_name = "mypackage",
#'   package_path = "C:/Users/USER/Dropbox/Amir/Amir personal/R",
#'   github_repo = "https://github.com/amirhome61/mypackage",
#'   author_name = "Amir Freund",
#'   author_email = "amir@email.com"
#' )
#' }
#' @export
af_create_package <- function(
  package_name,
  package_path = ".",
  github_repo = NULL,
  author_name = "Your Name",
  author_email = "you@example.com",
  license = "MIT"
) {
  if (!requireNamespace("usethis", quietly = TRUE)) {
    install.packages("usethis")
  }
  if (!requireNamespace("desc", quietly = TRUE)) {
    install.packages("desc")
  }
  if (!requireNamespace("git2r", quietly = TRUE)) {
    install.packages("git2r")
  }

  library(usethis)
  library(desc)
  library(git2r)

  # Full path to new package
  full_path <- file.path(package_path, package_name)

  # Check if folder already exists
  if (dir.exists(full_path)) {
    stop("Folder already exists: ", full_path)
  }

  # Create package skeleton
  usethis::create_package(full_path, open = FALSE)
  message("Package skeleton created at: ", full_path)

  # Navigate to package folder
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(full_path)

  # Set DESCRIPTION fields
  d <- desc::desc(file = "DESCRIPTION")
  d$set(
    "Authors@R",
    sprintf(
      'person("%s", email = "%s", role = c("aut", "cre"))',
      author_name,
      author_email
    )
  )
  d$set("License", paste0(license, " + file LICENSE"))
  d$write(file = "DESCRIPTION")

  # Store initial version
  initial_version <- d$get_version()

  # Create LICENSE file
  usethis::use_mit_license(author_name)

  # Initialize Git
  git2r::init(full_path)

  # Rename branch to main if needed
  system("git branch -M main")

  # Ensure Git identity
  system(sprintf('git config user.name "%s"', author_name))
  system(sprintf('git config user.email "%s"', author_email))

  # Initial commit
  system("git add -A")
  system(sprintf("git commit -m %s", shQuote("Initial package commit")))

  # Add GitHub remote and push if provided
  if (!is.null(github_repo)) {
    system(sprintf("git remote add origin %s", github_repo))
    system("git push -u origin main")
  }

  # --- Create release_<package>.R script ---
  runner_file <- file.path(full_path, paste0("release_", package_name, ".R"))

  cat(sprintf("# --- release_%s.R ---\n", package_name), file = runner_file)
  cat(
    sprintf(
      "# This script runs af_release_package() for %s with initial version\n\n",
      package_name
    ),
    file = runner_file,
    append = TRUE
  )

  cat("library(afcommon)\n\n", file = runner_file, append = TRUE)

  cat(
    sprintf(
      "new_version <- af_release_package(\n  package_path = '.',\n  github_repo = '%s'\n)\n\n",
      github_repo %||% ""
    ),
    file = runner_file,
    append = TRUE
  )

  cat(
    "# Remove local af_* functions to avoid conflicts\n",
    file = runner_file,
    append = TRUE
  )
  cat(
    "to_remove <- ls(envir = .GlobalEnv, pattern = '^af_')\n",
    file = runner_file,
    append = TRUE
  )
  cat(
    "if (length(to_remove) > 0) rm(list = to_remove, envir = .GlobalEnv)\n\n",
    file = runner_file,
    append = TRUE
  )

  cat(
    sprintf(
      "# Unload the package if already loaded\nif ('%s' %%in%% loadedNamespaces()) unloadNamespace('%s')\n\n",
      package_name,
      package_name
    ),
    file = runner_file,
    append = TRUE
  )

  cat(
    sprintf(
      "# Install and load the newly released package from GitHub\nremotes::install_github(paste0('%s@v', new_version))\nlibrary('%s', character.only = TRUE)\n\n",
      github_repo %||% "",
      package_name
    ),
    file = runner_file,
    append = TRUE
  )

  cat(
    sprintf(
      "message(sprintf('Successfully released and loaded %s version %%s', new_version))\n",
      package_name
    ),
    file = runner_file,
    append = TRUE
  )

  message("Portable release runner script created: ", runner_file)

  invisible(full_path)
}
