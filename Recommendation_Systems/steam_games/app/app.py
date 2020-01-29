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
GAME_WEIGHTS = np.load('./data/game_weights_200.pkl')
TAG_WEIGHTS = np.load('./data/tag_weights_200.pkl')

app = Flask(__name__)
Material(app)


@app.route('/')
def index():
    return render_template('index.html', games_autocomplete=GAME_AUTOCOMPLETE)


@app.route('/recommendation', methods=['POST'])
def selected_game():
    if request.method == 'POST':
        data = request.json
        return jsonify(GAME_TAGS[data['game']])


if __name__ == '__main__':
    app.run(debug=True)