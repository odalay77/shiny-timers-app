# Use rocker/shiny base image
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy app files to image
COPY . /srv/shiny-server/

# Install R packages from install.R
COPY install.R /tmp/install.R
RUN Rscript /tmp/install.R

# Expose port 3838 (default shiny port)
EXPOSE 3838

# Run the shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', port=3838, host='0.0.0.0')"]
