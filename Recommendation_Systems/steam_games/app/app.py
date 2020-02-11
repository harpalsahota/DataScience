import json
from typing import (
    List,
    Mapping,
    Tuple,
    Union,
)

from flask import (
    Flask,
    jsonify,
    render_template,
    request,
)
from flask_material import Material
import numpy as np

JSON = Union[str, int, float, bool, None, Mapping[str, 'JSON'], List['JSON']]

with open('./data/games_none.json', 'r') as none_json,\
        open('./data/games_with_tags_double_filter.json', 'r') as data_json:
    GAME_AUTOCOMPLETE = json.load(none_json)
    GAME_TAGS = json.load(data_json)

GAME_INDEX = {game: idx for idx, game in enumerate(GAME_TAGS)}
INDEX_GAME = {idx: game for game, idx in GAME_INDEX.items()}

GAME_WEIGHTS = np.load('./data/game_weights_200.pkl', allow_pickle=True)
TAG_WEIGHTS = np.load('./data/tag_weights_200.pkl', allow_pickle=True)

GAME_WEIGHTS = (
        GAME_WEIGHTS / np.linalg.norm(GAME_WEIGHTS, axis=1).reshape((-1, 1))
)
TAG_WEIGHTS = (
        TAG_WEIGHTS / np.linalg.norm(TAG_WEIGHTS, axis=1).reshape((-1, 1))
)

tag_count = 0
TAG_INDEX = {}
for game, tags in GAME_TAGS.items():
    for tag in tags:
        if tag not in TAG_INDEX:
            TAG_INDEX[tag] = tag_count
            tag_count += 1
INDEX_TAG = {idx: tag for tag, idx in TAG_INDEX.items()}

app = Flask(__name__)
Material(app)


@app.route('/')
def index() -> render_template:
    """
    App route method
    """
    return render_template(
        'index.html',
        games_autocomplete=GAME_AUTOCOMPLETE,
        all_tags=sorted(list(TAG_INDEX.keys())),
        n_games=len(GAME_INDEX),
        n_tags=len(TAG_INDEX),
    )


@app.route('/recommendation', methods=['POST'])
def selected_game() -> JSON:
    """
    For a selected game return the recommendations

    :rtype: JSON
    :return: JSON containing the recommendations, tags and game embedding
    """
    if request.method == 'POST':
        data = request.json
        embedding = GAME_WEIGHTS[GAME_INDEX[data['game']]]
        related = find_closest(embedding)
        tags = GAME_TAGS[data['game']]
        return jsonify({
            'related': _remove_self(data['game'], related),
            'tags': tags,
            'embedding': embedding.tolist()
        })


@app.route('/modify-embedding', methods=['POST'])
def modify_embedding() -> JSON:
    """
    Modify the embedding of the selected game

    :rtype: JSON
    :return: JSON containing the new recommended games and modified embedding
    """
    if request.method == 'POST':
        data = request.json
        if data['modification'] == 'addition':
            related, embedding = add_tag(data['tag'], data['embedding'])
            return jsonify({
                'related': _remove_self(data['game'], related),
                'embedding': embedding.tolist()
            })
        else:
            related, embedding = subtract_tag(data['tag'], data['embedding'])
            return jsonify({
                'related': _remove_self(data['game'], related),
                'embedding': embedding.tolist()
            })


def _remove_self(
        game: str,
        related: List[Tuple[str, float]]
) -> List[Tuple[str, float]]:
    """
    Removes the selected game from the list of recommendations

    :type game: str
    :param game: Selected game
    :type related: List[Tuple[str, float]]
    :param related: Related games with similarity scores
    :rtype: List[Tuple[str, float]]
    :return: New recommendations with the selected game removed
    """
    return [i for i in related if i[0] != game][:10]


def find_closest(game_embedding: np.array) -> List[Tuple[str, float]]:
    """
    Find the nearest games to the embedding based on cosine distance

    :type game_embedding: np.array
    :param game_embedding: Embedding of the selected game
    :rtype: List[Tuple[str, float]]
    :return: List of nearest game titles along with similarity score
    """
    dists = np.dot(GAME_WEIGHTS, game_embedding)
    sorted_dists = np.argsort(dists)
    closest = sorted_dists[-11:]
    return [(INDEX_GAME[i], f'{dists[i]:.{2}}') for i in reversed(closest)]


def add_tag(tag: str, embedding: List[float]) -> np.array:
    """
    Subtracts a tag embedding from a game embedding and normalises

    :type tag: str
    :param tag: Tag to add to game embedding
    :rtype: np.array
    :return: New game array with the tag embedding added
    """
    new_game_weight = np.array(embedding) + TAG_WEIGHTS[TAG_INDEX[tag]]
    ne = new_game_weight / np.linalg.norm(new_game_weight).reshape((-1, 1))[0]
    return find_closest(ne), new_game_weight


def subtract_tag(tag: str, embedding: List[float]) -> np.array:
    """
    Subtracts a tag embedding from a game embedding and normalises

    :type tag: str
    :param tag: Tag to subtract from game embedding
    :rtype: np.array
    :return: New game array with the tag embedding removed
    """
    new_game_weight = np.array(embedding) - TAG_WEIGHTS[TAG_INDEX[tag]]
    ne = new_game_weight / np.linalg.norm(new_game_weight).reshape((-1, 1))[0]
    return find_closest(ne), new_game_weight


if __name__ == '__main__':
    app.run(debug=True)
