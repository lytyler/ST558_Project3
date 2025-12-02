# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev  libpng-dev pandoc 
    
    
# install packages
RUN R -e "install.packages(c('plumber', 'tidyverse', 'tidymodels', 'ranger'))"

# copy modelAPI.R, modeling data, and diabetes model from the current directory into the container
COPY modelAPI.R modelAPI.R
COPY diabetes_modeling_data.csv diabetes_modeling_data.csv
COPY final_model.rds final_model.rds

# open port to traffic
EXPOSE 8000

# when the container starts, start the modelAPI.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('modelAPI.R'); pr$run(host='0.0.0.0', port=8000)"]
