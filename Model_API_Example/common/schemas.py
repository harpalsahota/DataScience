""" Schemas for incoming requests """

from typing import Dict

from marshmallow import (
    fields,
    post_load,
    validates,
    Schema,
    ValidationError
)

from common.data_models import PricePredictionFeaturesModel


class PricePredictionFeaturesSchema(Schema):
    is_house = fields.Integer(required=True)
    has_garden = fields.Integer(required=True)
    n_bedrooms = fields.Integer(required=True)

    @validates('is_house')
    def validate_is_house(self, value: int):
        if 0 < value > 1:
            raise ValidationError('is_house must be either 0 or 1')

    @validates('has_garden')
    def validate_has_garden(self, value: int):
        if 0 < value > 1:
            raise ValidationError('has_garden must be either 0 or 1')

    @validates('n_bedrooms')
    def validate_n_bedrooms(self, value: int):
        if value < 0:
            raise ValidationError('n_bedrooms must be >= 0')

    @post_load
    def make_price_prediction_data_model(
            self,
            data: Dict[str, int]
    ) -> PricePredictionFeaturesModel:
        """
        Once the schema checks have passes create an instance of the model
        """
        return PricePredictionFeaturesModel(**data)
