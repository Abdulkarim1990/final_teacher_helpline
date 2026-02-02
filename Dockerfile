FROM rocker/shiny:4.3.2

# Install system libraries commonly needed by R packages
RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libpq-dev \
  && rm -rf /var/lib/apt/lists/*

# Install renv (safe even if you do not use it)
RUN R -q -e "install.packages('renv', repos='https://cloud.r-project.org')"

# App directory used by Shiny Server
WORKDIR /srv/shiny-server/app

# Copy your app into the container
COPY . .

# Restore R packages if renv.lock exists
RUN R -q -e "if (file.exists('renv.lock')) renv::restore(prompt = FALSE)"

# Shiny runs on port 3838
EXPOSE 3838
