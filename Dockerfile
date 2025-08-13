FROM rocker/r-ver:4.2.2

RUN mkdir -p /home
RUN mkdir -p /home/code
RUN mkdir -p /home/gis

WORKDIR /home

COPY /code/install_packages.R /home/code/install_packages.R

COPY /gis/reservoirs.dbf /home/gis/reservoirs.dbf
COPY /gis/reservoirs.prj /home/gis/reservoirs.prj
COPY /gis/reservoirs.qpj /home/gis/reservoirs.qpj
COPY /gis/reservoirs.shp /home/gis/reservoirs.shp
COPY /gis/reservoirs.shx /home/gis/reservoirs.shx
COPY /gis/stream_recharge.dbf /home/gis/stream_recharge.dbf
COPY /gis/stream_recharge.prj /home/gis/stream_recharge.prj
COPY /gis/stream_recharge.qpj /home/gis/stream_recharge.qpj
COPY /gis/stream_recharge.shp /home/gis/stream_recharge.shp
COPY /gis/stream_recharge.shx /home/gis/stream_recharge.shx
COPY /gis/usgs_dissolved.dbf /home/gis/usgs_dissolved.dbf
COPY /gis/usgs_dissolved.prj /home/gis/usgs_dissolved.prj
COPY /gis/usgs_dissolved.qpj /home/gis/usgs_dissolved.qpj
COPY /gis/usgs_dissolved.shp /home/gis/usgs_dissolved.shp
COPY /gis/usgs_dissolved.shx /home/gis/usgs_dissolved.shx

COPY /app.R /home/app.R
COPY /deploy.R /home/deploy.R

RUN Rscript /home/code/install_packages.R

CMD Rscript /home/deploy.R