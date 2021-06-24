# build base image
FROM rocker/tidyverse:4.0.0

# install preliminary requirements
RUN apt-get update -y\
    && apt-get install -y dpkg-dev zlib1g-dev libssl-dev libffi-dev libglu1-mesa-dev\
    && apt-get install -y curl libcurl4-openssl-dev\
    && apt-get install -y python3-dev python3-venv\
    && apt-get install -y git\
    && apt-get install -y software-properties-common\
    && apt-get install -y libmagick++-dev

## run git cloning
RUN git clone https://github.com/Sage-Bionetworks/psorcast-validation-analysis.git /root/psorcast-validation-analysis

## change work dir
WORKDIR /root/psorcast-validation-analysis

## python dependencies
RUN bash init_py_env.sh

## get packages from lockfile
ENV RENV_VERSION 0.13.2
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "install.packages('synapser', repos=c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN R -e "renv::init(bare = TRUE)"
RUN R -e "renv::restore()"
RUN R -e "renv::use_python(name = 'env', type = 'virtualenv')"
