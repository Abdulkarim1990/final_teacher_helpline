# Heroku R buildpack initialization script
# Installs system dependencies and restores renv packages

# Install renv if not already available
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# Restore packages from renv.lock
renv::restore(prompt = FALSE)
