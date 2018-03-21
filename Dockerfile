FROM rocker/geospatial:3.4.3
RUN apt-get update

RUN install2.r --error \
  jpmesh

RUN install2.r --error \
  usethis

RUN installGithub.r \
  r-lib/roxygen2md \
  r-lib/devtools
