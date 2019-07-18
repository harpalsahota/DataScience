""" Module containing Data models """

from dataclasses import dataclass


@dataclass(init=True)
class PricePredictionFeaturesModel:
    """ Data Class for Price Prediction """

    is_house: int
    has_garden: int
    n_bedrooms: int

    def __repr__(self):
        return (
            f'<PricePredictionModel is_house={self.is_house} '
            f'has_garden={self.has_garden} n_bedrooms={self.n_bedrooms}>'
        )
