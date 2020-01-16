"""Module to help with downloading data from steamspy API"""

import json
from typing import (
    Dict,
    Union,
)
from urllib.request import urlopen


STEAMSPY_URL = 'https://steamspy.com/api.php'


def download_all_json_data() -> Dict:
    """
    Obtains the all data from steamspy

    :return: JSON object from requested endpoint
    """
    response = urlopen(f'{STEAMSPY_URL}?request=all')
    return json.load(response)


def get_games_with_tag(tag: str) -> Dict:
    """
    Obtain games that have a given tag

    :type tag: str
    :param tag: Tag to download data for
    :rtype: Dict
    :return: JSON object with the data
    """
    tag = tag.replace(' ', '+')
    response = urlopen(f'{STEAMSPY_URL}?request=tag&tag={tag}')
    return json.load(response)

def get_game_data(game_id: Union[int, str]) -> Dict:
    """
    Obtain the data for a given game including tag data

    :type game_id: Union[str, int]
    :param game_id: ID of the game
    :rtype: Dict
    :return: JSON object with the data
    """
    response = urlopen(f'{STEAMSPY_URL}?request=appdetails&appid={game_id}')
    return json.load(response)
