from flask import Flask
# you need to pip install flask-cors, or else you'll run into problems
from flask.ext.cors import CORS, cross_origin
import json
import os
import random

app = Flask(__name__)
cors = CORS(app)
app.config['CORS_HEADERS'] = 'Content-Type'

START = "__START__"
STOP = "__STOP__"

with open("questions.json") as f:
    q_transitions = json.loads(f.read())

with open("answers.json") as f:
    a_transitions = json.loads(f.read())

def smart_join(words):
    separators = ["" if w in ['.', ',', '?'] else " " for w in words]
    return ''.join(token
                   for word, sep in zip(words, separators)
                   for token in [sep, word]).strip()

def markov_gen(transitions):
    word = START
    words = []
    while True:
        word = random.choice(transitions.get(word, [STOP]))
        if word == STOP:
            return smart_join(words)
        else:
            words.append(word)

with open('questions.json', 'w') as f:
    f.write(json.dumps(q_transitions))

with open('answers.json', 'w') as f:
    f.write(json.dumps(a_transitions))

@app.route('/question')
@cross_origin()
def get_question():
    return json.dumps({
        "questionText": markov_gen(q_transitions),
        "answers": [markov_gen(a_transitions) for _ in range(4)],
        "correctAnswer" : random.randint(0, 3)
    })

if __name__ == '__main__':
    PORT = int(os.environ.get('PORT', 8080))
    app.run(port=PORT)
