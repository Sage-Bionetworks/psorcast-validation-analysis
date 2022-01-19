"""
This script is used for copying stats wiki of a digital health study
it will prompt user target id (which health summary to build dashboard on),
and the desired synapseformation template,
and the wiki id of the dashboard to copy

Author: aryton.tediarjo@sagebase.org
"""
import sys
import pandas as pd
import numpy as np
import logging
import argparse
from yaml import safe_load

import synapseclient
import synapseutils
from synapseclient import File
from synapseformation import client as synapseformation_client

TEMPLATE_PATH = "manuscript.yaml"

syn = synapseclient.login()
logging.basicConfig()
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


def get_project_id(syn, project_name):
    logger.info(f'Getting Synapse project id for {project_name}')
    response = syn.findEntityId(project_name)
    return "" if response is None else response

def create_project(syn, template_path, project_name):

    logger.info(f'Creating Synapse project {project_name}, ' + f'with template_path {template_path}')
    try:
        response = synapseformation_client.create_synapse_resources(template_path)
        logger.debug(f'Project response: {response}')
        if response is not None:
            return response.get('id')
    except Exception as e:
        logger.error(e)
        sys.exit(1)
        
        
def main():
    # get argument
    template_path = TEMPLATE_PATH

    # get data
    with open(template_path, 'r') as f:
        yaml_data = pd.json_normalize(safe_load(f))

    project_name = yaml_data["name"].iloc[0]

    # get project_id
    project_id = get_project_id(syn, project_name)
    print(project_id)

    # if there's a project id, assume the project is already connected to synapse
    connected_to_synapse = True if project_id else False

    # if no project id is available, create a new project
    if project_id == '':
        create_project(syn, template_path, project_name)
        project_id = get_project_id(syn, project_name)



if __name__ == "__main__":
    main()
