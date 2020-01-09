"""Module to help with downloading data from steamspy API"""

import json
from typing import Dict
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

    :param tag:
    :return:
    """
    tag = tag.replace(' ', '+')
    response = urlopen(f'{STEAMSPY_URL}?request=tag&tag={tag}')
    return json.load(response)