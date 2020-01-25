from flask import (
    Flask,
    jsonify,
    render_template,
)
from flask_material import Material


app = Flask(__name__)
Material(app)


@app.route('/')
def index():
    games2=['game1', 'game2', 'game3']
    games = {'BattleField': None, 'Age of Empires': None, 'game3': None}
    return render_template('index.html', games=games, games2=games2)


if __name__ == '__main__':
    app.run(debug=True)