import json

from flask import (
    Flask,
    jsonify,
    render_template,
    request,
)
from flask_material import Material
import numpy as np


with open('./data/games_none.json', 'r') as in_json:
    GAME_AUTOCOMPLETE = json.load(in_json)
with open('./data/games_with_tags_double_filter.json', 'r') as in_json:
    GAME_TAGS = json.load(in_json)
GAME_INDEX = {game: idx for idx, game in enumerate(GAME_TAGS)}
INDEX_GAME = {idx: game for game, idx in GAME_INDEX.items()}

GAME_WEIGHTS = np.load('./data/game_weights_200.pkl', allow_pickle=True)
TAG_WEIGHTS = np.load('./data/tag_weights_200.pkl', allow_pickle=True)

GAME_WEIGHTS = GAME_WEIGHTS / np.linalg.norm(GAME_WEIGHTS, axis=1).reshape((-1, 1))

app = Flask(__name__)
Material(app)


@app.route('/')
def index():
    return render_template('index.html', games_autocomplete=GAME_AUTOCOMPLETE)


@app.route('/recommendation', methods=['POST'])
def selected_game():
    if request.method == 'POST':
        data = request.json
        related = find_closest(GAME_WEIGHTS[GAME_INDEX[data['game']]])
        tags = GAME_TAGS[data['game']]
        return jsonify({'related': related, 'tags': tags})


def find_closest(game_embedding: np.array):
    dists = np.dot(GAME_WEIGHTS, game_embedding)
    sorted_dists = np.argsort(dists)
    closest = sorted_dists[-6:-1]
    return [(INDEX_GAME[i], f'{dists[i]:.{2}}') for i in reversed(closest)]


if __name__ == '__main__':
    app.run(debug=True)