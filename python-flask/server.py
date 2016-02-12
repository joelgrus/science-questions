from flask import Flask
# you need to pip install flask-cors, or else you'll run into problems
# trying to call this API cross-origin
from flask.ext.cors import CORS, cross_origin
import json
import os
import random

app = Flask(__name__)
cors = CORS(app)
app.config['CORS_HEADERS'] = 'Content-Type'

# sentinel tokens
START = "__START__"
STOP = "__STOP__"

# load the transitions
with open("questions.json") as f:
    q_transitions = json.loads(f.read())

with open("answers.json") as f:
    a_transitions = json.loads(f.read())

# naively you'd join "words" together with spaces, but we don't want to put
# spaces before punctuation marks.
def smart_join(words):
    # the separator that *precedes* each word
    separators = ["" if w in ['.', ',', '?'] else " " for w in words]
    return ''.join(token
                   for word, sep in zip(words, separators)
                   for token in [sep, word]).strip()

# generate a sentence by starting with the START sentinel and repeatedly
# choosing random subsequence words from the provided transitions, stopping
# when the STOP sentinel is reached
def markov_gen(transitions):
    word = START
    words = []
    while True:
        word = random.choice(transitions.get(word, [STOP]))
        if word == STOP:
            return smart_join(words)
        else:
            words.append(word)

# the JSON schema for a question looks like
# { questionText : "What's your favorite language?"
#   answers : ["Python", "JavaScript", "Haskell", "Ruby"]
#   correctAnswer : 2 }

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
