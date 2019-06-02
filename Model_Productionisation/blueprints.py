"""
Blueprints for the models
"""

from flask import Blueprint

from views.property_price_prediction import PropertyPricePrediction

price_prediction_blueprint = Blueprint('price_prediction_blueprint', __name__)

price_prediction_blueprint.add_url_rule(
    '/property-price-prediction',
    view_func=PropertyPricePrediction.as_view(name='property_price_prediction')
)