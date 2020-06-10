FROM rocker/geospatial:4.0.0

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
  install2.r --error --ncpus -1 --repos 'http://mran.revolutionanalytics.com/snapshot/2020-06-07' \
    assertr \
    jpmesh \
    here \
    kokudosuuchi \
    lwgeom \
    magick \
    pkgload \
    pryr \
    roxygen2 \
    usethis \
    zipangu && \
  installGithub.r \
    r-lib/revdepcheck && \
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
