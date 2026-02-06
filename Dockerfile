FROM rocker/shiny:4.5.1

RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  default-libmysqlclient-dev \
  && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

WORKDIR /srv/shiny-server/app

# Copy lockfile + renv folder first (better caching)
COPY renv.lock ./
COPY renv/ renv/

RUN R -e "renv::restore(prompt = FALSE)"

# Copy the rest of the app
COPY . .

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf


# Ensure renv library path exists and is writable by the shiny user
RUN mkdir -p /srv/shiny-server/app/renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu && \
    chown -R shiny:shiny /srv/shiny-server/app

EXPOSE 3838
