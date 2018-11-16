FROM rocker/geospatial:3.5.1

RUN set -x && \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    libmagick++-dev && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

RUN set -x && \
  install2.r --error \
    assertr \
    here \
    jpmesh \
    lwgeom \
    magick \
    pryr \
    usethis && \
  installGithub.r \
    "r-lib/pkgload" \
    "klutometis/roxygen" \
    "r-lib/roxygen2md" \
    "r-lib/devtools" \
    "r-lib/pkgdown" && \
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
