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
# re.split divides a string into pieces based on a regex
split = "\([A-D]\)"

# but that doesn't catch all the cases. let's see what we miss:
for q in raw_questions:
    if len(re.split(split, q)) not in [4, 5]:
        print(q)
        break

# after playing around, this list seems to be exhaustive
splits = [
  "\([A-D]\)",    # (A) python (B) haskell (C) javascript (D) ruby
  "\s[A-D]\.\s",  #  A. python  B. haskell  C. javascript  D. ruby
  "\s[1-4]\.\s",  #  1. python  2. haskell  3. javascript  4. ruby
  "\s[A-D]\s",    #  A  python  B  haskell  C  javascript  D  ruby
  "\s[FGHJ]\s",   #  F  python  G  haskell  H  javascript  J  ruby
  "\n [A-D]\s"    #   A python
                  #   B haskell
                  #   C javascript
                  #   D ruby
]

# see if there's any we missed
for q in raw_questions:
    if not any(len(re.split(split, q)) in [4, 5]
               for split in splits):
        print(q)

# OK, now we're ready to parse the questions
questions = []
answers = []

# we'll use sentinel tokens for the start and stop of a sentence
START = "__START__"
STOP = "__STOP__"

# for each question, find the first split that works. add the question to our
# list of questions, and the answers to the list of answers
for q in raw_questions:
    for split in splits:
        pieces = [x.strip() for x in re.split(split, q)]
        if len(pieces) in [4,5]:
            questions.append(pieces[0])
            answers.extend(pieces[1:])
            break
    else:
        print(q + "\n")

# we'll store transitions as a dict with string keys and string list values
# transitions["What"] is all the words that we observed following "What"
def make_transitions(sentences):
    transitions = defaultdict(list)
    for sentence in sentences:
        # regex looks for "?", ".", "," or groups of characters that aren't
        # any of those, and aren't spaces
        words = [START] + re.findall("[^ ?\.,]+|\?|\.|\,", sentence) + [STOP]
        for prev_word, next_word in zip(words, words[1:]):
            transitions[prev_word].append(next_word)
    return transitions

# one set of transitions for questions, one for answers, and we'll write them
# out to json, so that our service can use them
q_transitions = make_transitions(questions)
a_transitions = make_transitions(answers)

with open('questions.json', 'w') as f:
    f.write(json.dumps(q_transitions))

with open('answers.json', 'w') as f:
    f.write(json.dumps(a_transitions))

# it's inefficient to store those lists with multiplicity. that is, if all 2000
# questions start with "What is" then the entry for "What" will be a list of
# 2000 "is". Here I tried compressing the lists to (word, cumulative count)
# which you could then pick from uniformly using linear or binary search.
# But it ended up not saving that much space, so I didn't bother.
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
