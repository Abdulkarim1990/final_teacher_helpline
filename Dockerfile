FROM rocker/shiny:4.3.2

RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  default-libmysqlclient-dev \
  && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

WORKDIR /srv/shiny-server/app

# Copy lockfile + renv settings first (better caching)
COPY renv.lock ./
COPY settings.json ./

# Restore packages into the project library
RUN R -e "renv::restore(prompt = FALSE)"

# Copy the rest of the app
COPY . .

EXPOSE 3838
