FROM python:3.7
FROM rocker/verse:3.6.1
MAINTAINER "Cesar Aybar" csaybar@gmail.com

# SELENIUM - chrome
RUN apt-get update && apt-get install -y gnupg2
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'
RUN apt-get -y update
RUN apt-get install -y google-chrome-stable

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
    RandomFields \
    RNetCDF \
    classInt \
    deldir \
    gstat \
    hdf5r \
    lidR \
    lwgeom \
    mapdata \
    maptools \
    mapview \
    ncdf4 \
    proj4 \
    raster \
    rgdal \
    rgeos \
    rlas \
    sf \
    sp \
    spacetime \
    spatstat \
    spatialreg \
    spdep \
    geoR \
    geosphere \
    reticulate \
    stars \
    leaflet \
    mapview \
    geojsonio \
    getPass \
    crayon	\
    cli \
    googledrive \
    googleCloudStorageR \
    geojsonlint \
    lwgeom \
    cptcity \
    rnaturalearth \
    gganimate

RUN apt-get install -y \
		python3-pip \
		python3-dev \
	&& pip3 install virtualenv

RUN pip3 install coveralls \
    oauth2client \
    selenium \
    bs4 \
    numpy \
    requests_toolbelt \
    earthengine-api==0.1.210 \
    pyasn1

# Install anaconda
RUN echo 'export PATH=/opt/conda/bin:$PATH' > /etc/profile.d/conda.sh && \
    wget --quiet https://repo.continuum.io/archive/Anaconda2-4.3.1-Linux-x86_64.sh -O ~/anaconda.sh && \
    /bin/bash ~/anaconda.sh -b -p /opt/conda && \
    rm ~/anaconda.sh

#ENV PATH /opt/conda/bin:$PATH

RUN  R -e 'devtools::install_github("csaybar/rgee")'

RUN  mkdir -p /home/rgee/.config/earthengine/NA && \
     echo 'RETICULATE_PYTHON=/usr/bin/python3' > /home/rgee/.Renviron

ADD  demo /home/rgee/demo
ADD  demo /home/rgee/.config/earthengine/
COPY credentials/cd26ed5dc626f11802a652e81d02762e_data.colec.fbf@gmail.com /home/rgee/.config/earthengine/NA/cd26ed5dc626f11802a652e81d02762e_data.colec.fbf@gmail.com
COPY credentials/credentials /home/rgee/.config/earthengine/credentials
COPY credentials/rgee_sessioninfo.txt /home/rgee/.config/earthengine/rgee_sessioninfo.txt
RUN  pip3 install pyasn1 --upgrade
