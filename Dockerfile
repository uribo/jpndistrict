FROM rocker/geospatial:3.6.0

RUN set -x && \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    libmagick++-dev && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

ARG GITHUB_PAT

RUN set -x && \
  echo "GITHUB_PAT=$GITHUB_PAT" >> /usr/local/lib/R/etc/Renviron

RUN set -x && \
  install2.r --error \
    assertr \
    here \
    jpmesh \
    magick \
    pryr \
    usethis && \
  installGithub.r \
    "r-lib/pkgload" \
    "klutometis/roxygen" \
    "r-lib/roxygen2md" \
    "r-lib/devtools" \
    "r-lib/pkgdown" \
    "r-spatial/lwgeom" \
    "tidyverse/tidyr" \
    "uribo/odkitchen" && \
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
