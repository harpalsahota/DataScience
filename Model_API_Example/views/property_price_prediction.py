""" Views for property price prediction """

from flask import (
    request,
    jsonify,
)
from flasgger import SwaggerView
import joblib

from common import schemas

import settings

MODEL = joblib.load(f'./models/{settings.MODEL_NAME}')

class PropertyPricePrediction(SwaggerView):

    methods = ['GET']

    def get(self):
        try:
            features = request.args.to_dict()
            price_prediction = schemas.PricePredictionFeaturesSchema().load(features)
            predicted_price = MODEL.predict([[
                price_prediction.is_house,
                price_prediction.has_garden,
                price_prediction.n_bedrooms,
            ]]).tolist()
            return jsonify({
                'data': {'predicted_price': predicted_price}
            })
        except Exception as error:
            return jsonify({'error': str(error)}), 400

