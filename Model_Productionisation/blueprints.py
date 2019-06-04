"""
Blueprints for the models
"""
import logging

from flask import (
    request,
    Blueprint
)

from views.property_price_prediction import PropertyPricePrediction


logger = logging.getLogger()
handler = logging.StreamHandler()
formatter = logging.Formatter('%(asctime)s %(name)-12s %(levelname)-8s %(message)s')
handler.setFormatter(formatter)
logger.addHandler(handler)
logger.setLevel(logging.DEBUG)

class BaseBlueprint(Blueprint):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.before_request(self._log_before_request)
        self.after_request(self._log_post_request)

    def _log_before_request(self, *args, **kwargs):
        logger.info(f'Request for endpoint: {request.full_path}')
        logger.info(f'With args: {request.args}')


    def _log_post_request(self, response):
        logger.info(f'Response status: {response.status}')
        logger.info(f'Response json: {response.json}')
        return response


price_prediction_blueprint = BaseBlueprint('price_prediction_blueprint', __name__)

price_prediction_blueprint.add_url_rule(
    '/property-price-prediction',
    view_func=PropertyPricePrediction.as_view(name='property_price_prediction')
)