from marshmallow import (
    fields,
    post_load,
    validates,
    Schema,
    ValidationError
)

from common.data_models import PricePredictionFeaturesModel


class PricePredictionFeaturesSchema(Schema):
    is_house = fields.Int()
    has_garden = fields.Int()
    n_bedrooms = fields.Int()

    @validates('is_house')
    def validate_is_house(self, value):
        pass

    @validates('has_garden')
    def validate_is_house(self, value):
        pass

    @validates('n_bedrooms')
    def validate_is_house(self, value):
        pass

    @post_load
    def make_price_prediction_data_model(self, data):
        """
        Once the schema checks have passes create an instance of the model
        """
        return PricePredictionFeaturesModel(**data)
