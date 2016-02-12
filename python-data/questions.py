import re
import csv
import random
from collections import defaultdict, Counter
from itertools import accumulate
import json

# get all the questions from the csv
with open('questions.csv') as f:
    reader = csv.DictReader(f)
    raw_questions = [row['question'] for row in reader]

# want to split in questions and answers
# example: "What's fun? (A) python (B) ruby (C) haskell (D) C++"

split = "\([A-D]\)"

for q in raw_questions:
    if len(re.split(split, q)) not in [3, 4, 5]:
        print(q)
        break

splits = [
  "\([A-D]\)",
  "\s[A-D]\.\s",
  "\s[1-4]\.\s",
  "\s[A-D]\s",
  "\s[FGHJ]\s",
  "\n [A-D]\s"
]

for q in raw_questions:
    if not any(len(re.split(split, q)) in [3, 4, 5]
               for split in splits):
        print(q)

questions = []
answers = []

START = "__START__"
STOP = "__STOP__"

for q in raw_questions:
    for split in splits:
        pieces = [x.strip() for x in re.split(split, q)]
        if len(pieces) in [4,5]:
            questions.append(pieces[0])
            answers.extend(pieces[1:])
            break
    else:
        print(q + "\n")

def make_transitions(sentences):
    transitions = defaultdict(list)
    for sentence in sentences:
        words = [START] + re.findall("[^ ?\.,]+|\?|\.|\,", sentence) + [STOP]
        for prev_word, next_word in zip(words, words[1:]):
            transitions[prev_word].append(next_word)
    return transitions

q_transitions = make_transitions(questions)
a_transitions = make_transitions(answers)

def markov_gen(transitions):
    word = START
    words = []
    while True:
        word = random.choice(transitions.get(word, [STOP]))
        if word == STOP:
            return words
        else:
            words.append(word)

def make_question():
    question = markov_gen(q_transitions)
    answers = [markov_gen(a_transitions) for _ in range(4)]
    return " ".join(question +
                    ['(A)'] + answers[0] +
                    ['(B)'] + answers[1] +
                    ['(C)'] + answers[2] +
                    ['(D)'] + answers[3])

with open('questions.json', 'w') as f:
    f.write(json.dumps(q_transitions))

with open('answers.json', 'w') as f:
    f.write(json.dumps(a_transitions))

def compress_transitions(transitions):
    compressed = {}
    for token, next_tokens in transitions.items():
        counts = Counter(next_tokens)
        compressed[token] = list(zip(counts.keys(), accumulate(counts.values())))
    return compressed

with open('questions_compressed.json', 'w') as f:
    f.write(json.dumps(compress_transitions(q_transitions)))

with open('answers_compressed.json', 'w') as f:
    f.write(json.dumps(compress_transitions(a_transitions)))
