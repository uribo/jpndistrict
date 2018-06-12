FROM rocker/geospatial:3.5.0

RUN set -x && \
  apt-get update && \
  rm -rf /var/lib/apt/lists/*

RUN set -x && \
  install2.r --error \
    jpmesh \
    pryr \
    usethis && \
  installGithub.r \
    "r-lib/roxygen2md" \
    "r-lib/devtools"
