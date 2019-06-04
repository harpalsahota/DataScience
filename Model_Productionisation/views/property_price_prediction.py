
from flask import (
    request,
    jsonify,
)
from flask.views import MethodView


import joblib

from common import schemas

import settings

MODEL = joblib.load(f'./models/{settings.MODEL_NAME}')

class PropertyPricePrediction(MethodView):

    methods = ['GET']

    def get(self):
        features = request.args.to_dict()
        price_prediction = schemas.PricePredictionFeaturesSchema().load(features)
        print(price_prediction)
        predicted_price = MODEL.predict([[1, 0, 3]]).tolist()
        return jsonify({'predicted_price': predicted_price})