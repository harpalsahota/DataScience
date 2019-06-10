from marshmallow import (
    fields,
    post_load,
    validates,
    Schema,
    ValidationError
)

from common.data_models import PricePredictionFeaturesModel


class PricePredictionFeaturesSchema(Schema):
    is_house = fields.Integer()
    has_garden = fields.Integer()
    n_bedrooms = fields.Integer()

    @validates('is_house')
    def validate_is_house(self, value):
        if 0 < value > 1:
            raise ValidationError('is_house must be either 0 or 1')

    @validates('has_garden')
    def validate_has_garden(self, value):
        if 0 < value > 1:
            raise ValidationError('has_garden must be either 0 or 1')

    @validates('n_bedrooms')
    def validate_n_bedrooms(self, value):
        if value < 0:
            raise ValidationError('n_bedrooms must be >= 0')

    @post_load
    def make_price_prediction_data_model(self, data):
        """
        Once the schema checks have passes create an instance of the model
        """
        return PricePredictionFeaturesModel(**data)
