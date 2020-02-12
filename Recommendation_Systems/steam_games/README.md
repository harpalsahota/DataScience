# Steam Game Recommender

This repo contains the code to train a neural network recommendation engine to recommend games based on steam tags. Furthermore, the `app` folder contains a Flask web server and pre-trained weights to search for recommend games.
## Description

The repo is split into the following components:

- **notebooks**: Python notebooks showing how to obtain the training data and train the model
- **app**: Contains a flask web server loading pre-trained weights and is accessible via a web UI.

## Installation

The project uses Python 3.7 via a virtual environment created by [poerty]( https://github.com/python-poetry/poetry).
To install the required packages with poetry run:

> poetry install

## Instructions

### Flask App
To run the flask app ensure the dependencies in `pyproject.toml` are installed. The development dependencies are not
required. Change directory to the `app` folder and run:

> python app.py

This should start a flask web server which is accessible at: http://localhost:5000/. Navigating to this page should
load the home page with instructions on how to use the recommendation engine.

### Docker

## Acknowledgments
Big thanks to [steamspy](https://steamspy.com/about) for gathering the data and making it available via an 
[api](https://steamspy.com/api.php), this project wouldn't be possible with out it.



