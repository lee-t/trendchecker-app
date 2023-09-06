FROM rocker/shiny-verse:latest

WORKDIR /code

# Install stable packages from CRAN
RUN install2.r --error \
    shiny \
    shinyalert \
    shinythemes \
    shinycssloaders \
    shinyjs \
    httr \
    bslib \
    thematic \
    gtrendsR \
    plotly \
    dplyr \
    ggplot2
    

# Install development packages from GitHub
RUN installGithub.r \
    rstudio/bslib \
    rstudio/httpuv

COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
