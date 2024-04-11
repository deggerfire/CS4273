# Base image
FROM rocker/r-base:latest

# Copy all files to container
COPY . .

# Install packages
RUN Rscript /install_packages.R

# Currently used port
EXPOSE 5111
# Run
CMD Rscript /app.R
