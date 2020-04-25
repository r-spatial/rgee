FROM python:3.7
FROM rocker/verse:3.6.1
MAINTAINER "Cesar Aybar" csaybar@gmail.com

# set display port to avoid crash
ENV DISPLAY=:99

# R
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libv8-3.14-dev \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    liblwgeom-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev \
  && install2.r --error \
    RColorBrewer \
    cptcity \
    rnaturalearth \
    gganimate

RUN apt-get install -y \
		python3-pip \
		python3-dev	

RUN  pip3 install pyasn1 --upgrade

RUN pip3 install coveralls \
    oauth2client \        
    numpy \    
    earthengine-api \    
    virtualenv

RUN  R -e 'devtools::install_github("csaybar/rgee")'

RUN  mkdir -p /home/rgee/.config/earthengine/ && \
     echo 'RETICULATE_PYTHON=/usr/bin/python3' > /home/rgee/.Renviron

ADD  demo /home/rgee/demo
ADD  demo /home/rgee/.config/earthengine/