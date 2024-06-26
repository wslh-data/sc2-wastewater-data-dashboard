FROM rocker/shiny-verse:4.1.0

LABEL base.image="rocker/shiny-verse"
LABEL dockerfile.version="2"
LABEL software="R Shiny Server"
LABEL software.version="4.1.0"
LABEL description="R Shiny Server for hosting Data Dashboards"
LABEL website=""
LABEL license=""
LABEL maintainer="Adelaide Roguet <adelaide.roguet@slh.wisc.edu>"


RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  build-essential \
  libpoppler-cpp-dev \
  pkg-config \
  python-dev \
  libjpeg-dev \
  libpng-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libgdal-dev \
  libudunits2-dev \
  libjq-dev \
  libprotobuf-dev \
  protobuf-compiler \
  libv8-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev

RUN R -e "install.packages(c(\
'shiny', \
'dplyr', \
'plotly', \
'forcats', \ 
'ggplot2', \ 
'leaflet', \
'leaflet.minicharts', \
'viridis', \
'shinycssloaders' \
), repos = 'http://cran.us.r-project.org')"


RUN rm -r /srv/shiny-server/*

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY wwBarplot /srv/shiny-server/wwBarplot
COPY wwHeatmap /srv/shiny-server/wwHeatmap
COPY wwMap /srv/shiny-server/wwMap
COPY wwTime /srv/shiny-server/wwTime
COPY wwBanner /srv/shiny-server/wwBanner


EXPOSE 3838
