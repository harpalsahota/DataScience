
from flask import jsonify
from flask.views import MethodView


import joblib

import settings

MODEL = joblib.load(f'./models/{settings.MODEL_NAME}')

class PropertyPricePrediction(MethodView):

    methods = ['GET']

    def get(self):
        predicted_price = MODEL.predict([[1, 0, 3]]).tolist()
        return jsonify({'predicted_price': predicted_price})