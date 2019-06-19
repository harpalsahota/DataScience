import os
from os.path import (
    dirname,
    join,
)
from dotenv import load_dotenv
import yaml

# Load environment variables from the .env file
dotenv_path = join(dirname(__file__), '.env')
load_dotenv(dotenv_path)

# Application Variables
MODEL_NAME = None

# Load custom settings from the YAML file specified by APP_SETTINGS_YAML
custom_settings_filepath = os.environ.get('APP_SETTINGS_YAML')
if custom_settings_filepath is not None:
    with open(custom_settings_filepath) as infile:
        globals().update(yaml.safe_load(infile))

for var, value in os.environ.items():
    if var in globals():
        globals()[var] = value
