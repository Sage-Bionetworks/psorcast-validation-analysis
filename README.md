# Sage Bionetworks - Psorcast Validation Analysis
<img alt="GitHub pull requests" src="https://img.shields.io/github/issues-pr/Sage-Bionetworks/psorcast-validation-analysis">  <img alt="GitHub issues" src="https://img.shields.io/github/issues/Sage-Bionetworks/psorcast-validation-analysis">  <img alt="Docker Cloud Build Status" src="https://img.shields.io/docker/cloud/build/arytontediarjo/psorcast-validation-analysis">

Welcome to Psorcast Validation Analysis Github Repository, this repository is purposed to be the analytical pipeline for Psorcast Validation Study.

Maintainer: 
- meghasyam@sagebase.org (sole maintainer)
- aryton.tediarjo@sagebase.org (Retired from Sage as of Jan 28th 2022)
- dan.webster@sagebase.org (Retired from Sage as of Nov 21st)

Note: This repo used for dumping all our analyses.
For rerunnable analysis related to **Psorcast Manuscript** please refer to this [git repository](https://github.com/Sage-Bionetworks/psorcast-validation-manuscript)


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

## Running in Docker (Recommended):
Docker image is designed to build R & Python Environment and deployed in a container. Environment in R uses `renv` and Python `virtualenv` package management. 

### i. Clone the repository: 
```zsh
git clone https://github.com/Sage-Bionetworks/psorcast-validation-analysis.git
```
### ii. Build Image:
```zsh
docker build -t 'psorcast-validation-analysis' .
```
### iii. Run Image as Container:
```zsh
docker run -itd psorcast-validation-analysis
```
Notes: Argument -itd is used to make sure that container is run in detached mode (not removed after running once)

### iv. Execute Container:
#### Check Container ID:
```zsh
docker ps -a
```
Using this command, it will output container that contains the saved image. Fetch the container ID to proceed.

#### Fetch container ID and create Synapse Authentication:
```zsh
docker exec -it <CONTAINER_ID> make authenticate PARAMS="-u <username> -p <password> -g <git_token>"
```

## Using RStudio Environment 
The process goes the same way in 

### i. Create Python Virtual Environment
```zsh
bash init_py_env.sh
```

#### ii. Restore R Libraries using R's `renv` package
Some of the steps of the pipeline will use python and will be using R renv library for managing it (https://rstudio.github.io/renv/articles/python.html)
```R
remotes::install_github('rstudio/renv@0.13.2')
renv::init(bare = T)
renv::restore()
renv::use_python(name = "env", type = "virtualenv") # parse in python location with installed packages from requirements.txt
```
