# Model API

This README accompanies [this]() blog post on medium. 

## Requirements

- Python 3.7, not tested with 3.6 but should work

## Installation
### With Pipenv
Install `pipenv` globally via `pip` (if not already installed):
> pip3 install pipenv

Change directory to `Model_API_Example`. To install the packages for the API run:
> pipenv install

Enter the virtural environment shell:
> pipenv shell

### With Pip
Go through the package list in the `Pipfile` and install with pip by running:
> pip3 install \<package name\>

## Configuration
Copy both the `config.yml.example` and `.env.example` files and remove the 
`.example` suffix.

Set `APP_SETTINGS_YAML` to `'config.yml'` in the `.env` file.

Set `MODEL_NAME` to `rf_classifier.pkl` in `config.yml`

## Launching the API

Mac / UNIX: `gunicorn -b 0.0.0.0:5000 -w 1 wsgi:api`

Windows: `waitress-serve --listed=*5000 wsgi:api`

Navigate to `http://localhost:5000/apidocs` to view the Swagger docs of the API