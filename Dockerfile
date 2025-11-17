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

# Expose dynamic port (Render will set PORT)
EXPOSE 3838

# Run the shiny app (use Render’s PORT variable)
CMD ["R", "-e", "port <- as.numeric(Sys.getenv('PORT', '3838')); shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=port)"]
