FROM python:3.7

RUN mkdir /app

COPY . /app
COPY ../pyproject.toml /app

WORKDIR /app
ENV PYTHONPATH=${PYTHONPATH}:${PWD}

RUN pip3 install poetry
RUN poetry install --no-dev

ENTRYPOINT ["gunicorn", "-w", "3", "-b", "0.0.0.0:5000", "wsgi:app"]