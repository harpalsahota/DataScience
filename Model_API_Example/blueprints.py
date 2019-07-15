"""
Blueprints for the models
"""
from flask import (
    request,
    Blueprint
)

from common.logger import LOGGER
from views.property_price_prediction import PropertyPricePrediction


class BaseBlueprint(Blueprint):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.before_request(self._log_before_request)
        self.after_request(self._log_post_request)

    def _log_before_request(self, *args, **kwargs):
        LOGGER.info(f'Request for endpoint: {request.full_path}')
        LOGGER.info(f'With args: {request.args}')


    def _log_post_request(self, response):
        LOGGER.info(f'Response status: {response.status}')
        LOGGER.info(f'Response json: {response.json}')
        return response


price_prediction_blueprint = BaseBlueprint('price_prediction_blueprint', __name__)

price_prediction_blueprint.add_url_rule(
    '/v1/property-price-prediction',
    view_func=PropertyPricePrediction.as_view(name='property_price_prediction_v1')
)
