FROM rocker/shiny:4.3.2

# System libraries needed by common R packages (incl. MySQL)
RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libmysqlclient-dev \
  && rm -rf /var/lib/apt/lists/*

# Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Set working directory for Shiny
WORKDIR /srv/shiny-server/app

# Copy app files (including renv/)
COPY . .

# Restore R packages from renv.lock
RUN R -e "renv::restore(prompt = FALSE)"

# Expose Shiny port
EXPOSE 3838
