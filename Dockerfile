FROM rocker/geospatial:4.0.3@sha256:645ac1e38fd639d09e3bbdbb812bab470c4eeb6f8d24fed499bd19275eccb15d

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
  install2.r --error --ncpus -1 --repos 'https://cran.microsoft.com/snapshot/2020-11-25/' \
    assertr \
    googlePolylines \
    here \
    lwgeom \
    magick \
    pkgload \
    pryr \
    rhub \
    roxygen2 \
    usethis \
    zipangu && \
  installGithub.r \
    uribo/jpmesh \
    r-lib/revdepcheck && \
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
