from flask import Flask
from flask_cors import CORS

from blueprints import price_prediction_blueprint

import settings

api = Flask(__name__)

_ = CORS(
    api,
    resources={
        r'/api/*': {'origins': '*'}
    }
)

api.register_blueprint(
    price_prediction_blueprint,
    url_prefix='/api/v1'
)

if __name__ == '__main__':
    api.run(
        host='0.0.0.0',
        port=5000,
        debug=True
    )