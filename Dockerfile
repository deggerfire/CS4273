# Base image
FROM rocker/shiny

RUN apt-get update
RUN apt-get install -y libcurl4-gnutls-dev libssl-dev

# Copy install script to root of container
COPY ./install_packages.R /
# Copy all files to server
COPY ./ /srv/shiny-server/

# Install packages
RUN Rscript /install_packages.R

# Run
CMD ["/usr/bin/shiny-server"]
