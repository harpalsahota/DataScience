# DataScience

This repo contains reference material for myself regarding all things Data Science!

## Social and Portfolio

- [Personal Portfolio](https://www.drdatascience.co.uk)
- [Medium Profile](https://medium.com/@harpalsahota)

## Directories
- **AB_Testing**: Notebooks regarding Bayesian AB testing
- **Books**: Notebooks from text books
- **Data Analysis**: Analysis of datasets with notebooks
- **Datasets_Tools_and_Packages**: README listing useful data science tools, datasets and packages
- **Deep_Learning**: Notebooks regarding deep learning
- **fastai**: Notebooks from the fastai course
- **Kaggle**: Kaggle competition attempts 
- **Model_API_Example**: Example of data science model deployment. There is a medium article accompanying this
- **Python**: Python tip, tricks, best practise and coding paradigms i've learnt
- **Recommendation Systems**: Building a recommendation system
- **sklearn_example**: Examples of sklearn usage along with code snippets
- **Theory**: Notebooks illustrating some theory in data science

## Compatibility

All work is done in python 3+. Most of the work will require >= 3.6 as due `f-strings`

## Requirements

Any notebooks that require additional packages will have either a `Pipfile` or a `pyproject.toml` listing the
dependencies. Older projects will be using the `Pipfile` via the `pipenv` package manager. I have now switched to
using `Poetry` as my dependency manager which utilises the `pyproject.toml` file.