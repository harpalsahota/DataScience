from flask import (
    Flask,
    jsonify,
    render_template,
    request,
)
from flask_material import Material


app = Flask(__name__)
Material(app)


@app.route('/')
def index():
    games = {'BattleField': None, 'Age of Empires': None, 'game3': None}
    return render_template('index.html', games=games)


@app.route('/recommendation', methods=['POST'])
def selected_game():
    if request.method == 'POST':
        data = request.json
        print(data)
        return jsonify(['game x', 'game y', 'game x'])


if __name__ == '__main__':
    app.run(debug=True)