
from flask.views import MethodView

class PropertyPricePrediction(MethodView):

    methods = ['GET']

    def get(self):
        from flask import jsonify
        return jsonify({'test': 1})