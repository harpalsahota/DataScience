from marshmallow import (
    Schema,
    fields,
)

class PricePredictionFeaturesSchema(Schema):
    is_house = fields.Int()
    has_garden = fields.Int()
    n_bedrooms = fields.Int()

