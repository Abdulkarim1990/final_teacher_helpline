FROM rocker/shiny:4.5.1


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
COPY renv/settings.json renv/settings.json
RUN R -e "renv::restore(prompt = FALSE)"
COPY . .

# Ensure renv library path exists and is writable by the shiny user
RUN mkdir -p /srv/shiny-server/app/renv/library && \
    chown -R shiny:shiny /srv/shiny-server/app



EXPOSE 3838
