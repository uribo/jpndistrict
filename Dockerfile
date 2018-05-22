FROM rocker/geospatial:3.5.0
RUN apt-get update

RUN install2.r --error \
  jpmesh \
  pryr \
  usethis

RUN installGithub.r \
  r-lib/roxygen2md \
  r-lib/devtools
