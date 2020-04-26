FROM python:3.7
FROM rocker/verse:3.6.1
MAINTAINER "Cesar Aybar" csaybar@gmail.com

# set display port to avoid crash
ENV DISPLAY=:99

# R
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable

RUN apt install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
                libv8-3.14-dev libjq-dev libnetcdf-dev libjson-c-dev \
                libprotobuf-dev protobuf-compiler unixodbc-dev liblwgeom-dev \
                libssl-dev

RUN apt-get install -y python3-pip python3-dev

RUN  pip3 install pyasn1 --upgrade

RUN pip3 install coveralls \
    oauth2client \
    numpy \
    earthengine-api \
    virtualenv

RUN  Rscript -e "remotes::install_github('rstudio/reticulate')"
RUN  Rscript -e "remotes::install_github('csaybar/rgee')"

RUN  mkdir -p /home/rgee/.config/earthengine/ && \
     echo 'RETICULATE_PYTHON=/usr/bin/python3' > /home/rgee/.Renviron

ADD  demo /home/rgee/demo
ADD  demo /home/rgee/.config/earthengine/
