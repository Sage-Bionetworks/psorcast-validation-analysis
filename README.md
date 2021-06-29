# Sage Bionetworks - Psorcast Validation Analysis
<img alt="GitHub pull requests" src="https://img.shields.io/github/issues-pr/Sage-Bionetworks/psorcast-validation-analysis">  <img alt="GitHub issues" src="https://img.shields.io/github/issues/Sage-Bionetworks/psorcast-validation-analysis">  <img alt="Docker Cloud Build Status" src="https://img.shields.io/docker/cloud/build/arytontediarjo/psorcast-validation-analysis">

Welcome to Psorcast Validation Analysis Github Repository, this repository is purposed to be the analytical pipeline for Psorcast Validation Study.

## Script Directories:
- <b>curate_tables</b>: Folder for storing all scripts related to curating tables from Bridge to Validation Analysis Project
- <b>feature_extraction</b>: Folder for storing all scripts related to extracting key features from previously extracted Synapse Tables
- <b>analysis</b>: Folder for storing analysis codes and notebooks
- <b>utils</b>: Folder for script helper across data pipeline

## How to run
To run this repository, you will be required to have several credentials:
- A Github Token Credentials for fetching github urls
- A .synapseConfig File for getting your credentials to synapse
- A .synapseCache Folder for caching synapse files I/O

### HOW-TO:
We will be using docker for reproducing this analysis, we will require your Synapse/Github credentials (in file location) to be placed in .env so that as we build the container, it will contain all the required credentials.

#### a. Rerunning Pipeline using Docker:
```zsh
docker pull arytontediarjo/psorcast-validation-analysis

docker run -v <SYNAPSE_CONFIG_PATH>:/root/.synapseConfig\ 
            -v <SYNAPSE_CACHE_PATH>:/root/.synapseCache\
            -v <GIT_TOKEN_PATH>:/git_token.txt\
            -d arytontediarjo/psorcast-validation-analysis

docker exec <CONTAINER_ID> make pipeline
```

#### b. RStudio Environment 

##### i. Git Clone
```zsh
git clone git clone https://github.com/Sage-Bionetworks/psorcast-validation-analysis.git
```

#### ii. Create Python Virtual Environment
```zsh
bash init_py_env.sh
```

##### iii. Restore R Libraries using R's `renv` package
Some of the steps of the pipeline will use python and will be using R renv library for managing it (https://rstudio.github.io/renv/articles/python.html)
```R
remotes::install_github('rstudio/renv@0.13.2')
renv::init(bare = T)
renv::restore()
renv::use_python(name = "env", type = "virtualenv") # parse in python location with installed packages from requirements.txt
```
