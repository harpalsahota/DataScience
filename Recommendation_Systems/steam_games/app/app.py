import json

from flask import (
    Flask,
    jsonify,
    render_template,
    request,
)
from flask_material import Material

with open('./data/games_none.json', 'r') as in_json:
    GAME_AUTOCOMPLETE = json.load(in_json)
with open('./data/games_with_tags_double_filter.json', 'r') as in_json:
    GAME_TAGS = json.load(in_json)

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