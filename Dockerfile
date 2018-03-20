FROM rocker/geospatial:3.4.3
RUN apt-get update

RUN install2.r --eroor \
  jpmesh

RUN install2.r --error \
  usethis

RUN installGitHub.r \
  r-lib/roxygen2md \
  r-lib/devtools
