# Sage Bionetworks - Psorcast Validation Analysis
<img alt="GitHub pull requests" src="https://img.shields.io/github/issues-pr/Sage-Bionetworks/psorcast-validation-analysis"> <img alt="GitHub issues" src="https://img.shields.io/github/issues/Sage-Bionetworks/psorcast-validation-analysis">

Welcome to Psorcast Validation Analysis Github Repository, this repository is purposed to be the analytical pipeline for Psorcast Validation Study.

## How to run
To run this repository, you will be required to have several credentials:
- A Github Token Credentials for fetching github urls
- A .synapseConfig File for getting your credentials to synapse
- A .synapseCache Folder for caching synapse files I/O

### How to run:

#### 1. Clone Repository:
```git
git clone https://github.com/Sage-Bionetworks/psorcast-validation-analysis.git
```

#### 2. Build Environment:

a. Using Docker Bash (automation):

We will be using docker for reproducing this analysis, we will require your credentials (in file location)
to be placed in .env so that as we build the container, it will contain all the required credentials.

```zsh
docker compose up -d
docker exec -it <container ID> /bin/bash
```

b. Using renv (Rstudio)
```R
renv::init(bare = T)
renv::restore()
renv::use_python(...) # parse in python location with installed packages from requirements.txt
```

#### 3. Run data pipeline:
```zsh
make pipeline
```
