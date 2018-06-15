FROM rocker/geospatial:3.5.0

RUN set -x && \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    libmagick++-dev && \
  rm -rf /var/lib/apt/lists/*

RUN set -x && \
  install2.r --error \
    jpmesh \
    magick \
    pryr \
    usethis && \
  installGithub.r \
    "r-lib/roxygen2md" \
    "r-lib/devtools" \
    "r-lib/pkgdown"
