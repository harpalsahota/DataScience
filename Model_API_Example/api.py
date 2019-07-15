from flask import Flask
from flask_cors import CORS
from flasgger import Swagger

from blueprints import price_prediction_blueprint


api = Flask(__name__)

_ = CORS(
    api,
    resources={
        r'/api/*': {'origins': '*'}
    }
)

api.register_blueprint(
    price_prediction_blueprint,
    url_prefix='/api'
)
api.config['SWAGGER'] = {
    'doc_dir': './endpoint_docs'
}
swag = Swagger(
    api,
    parse=False,  # If true forces the incoming data to be validated
    template={
        'info': {
            'title': 'Price Prediction API',
            'description': 'API for predicting property prices',
            'version': '1.0.0'
        }
    }
)

if __name__ == '__main__':
    api.run(
        host='0.0.0.0',
        port=5000,
        debug=True
    )
