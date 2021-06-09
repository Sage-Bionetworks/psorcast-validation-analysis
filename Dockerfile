# build base image
FROM rocker/tidyverse:4.0.0

# install preliminary requirements
RUN apt-get update -y\
    && apt-get install -y dpkg-dev zlib1g-dev libssl-dev libffi-dev libglu1-mesa-dev\
    && apt-get install -y curl libcurl4-openssl-dev\
    && apt-get install -y python3-dev python3-venv\
    && apt-get install -y git

## python dependencies
RUN python3 -m venv ~/env\
    && . ~/env/bin/activate \
    && python3 -m pip install --upgrade pip\
    && python3 -m pip install synapseclient\
    && python3 -m pip install git+https://github.com/arytontediarjo/PDKitRotationFeatures.git\
    && python3 -m pip install opencv-python\
    && python3 -m pip install imageio

RUN R -e "install.packages('renv')"\
    && R -e "install.packages('PythonEmbedInR', repos=c('http://cran.fhcrc.org', 'http://ran.synapse.org'))"\
    && R -e "install.packages('synapser', repos = c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"\
    && R -e "install.packages('synapserutils', repos=c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"\
    && R -e "devtools::install_github('brian-bot/githubr')"\
    && R -e "devtools::install_github('Sage-Bionetworks/knit2synapse')"\
    && R -e "install.packages('doMC')"\
    && R -e "install.packages('reticulate')"\
    && R -e "install.packages('png')"\
    && R -e "install.packages('jpeg')"\
    && R -e "install.packages('ggpval')"\
    && R -e "install.packages('ggpubr')"\
    && R -e "install.packages('ggExtra')"\
    && R -e "install.packages('data.table')"\
    && R -e "install.packages('blandr')"\
    && R -e "install.packages('patchwork')"\
    && R -e "install.packages('DescTools')"

WORKDIR /root