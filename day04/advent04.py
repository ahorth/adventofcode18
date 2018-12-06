import re
import datetime
from pprint import pprint 
from collections import Counter, defaultdict

re_timestamp = r'\[(\d+)-(\d+)-(\d+) (\d+):(\d+)] (.+)'
re_state = r'Guard #(\d+)'

def parse_state(state_string):
    if state_string == 'falls asleep':
        return 'asleep'
    elif state_string == 'wakes up':
        return 'awake'
    else:
        state = re.match(re_state, state_string).groups()
        return int(state[0]) # return the guard's ID

def parse_states(filepatch):
    guard_states = []
    with open(filepatch) as f:
        for line in f:
            temp = re.match(re_timestamp, line).groups()
            date, state = temp[0:-1], temp[-1]
            date = [int(x) for x in date]
            date = datetime.datetime(*date)
            guard_states.append((date, parse_state(state)))
    return guard_states

def get_sleeps(guard_states):
    c = Counter()
    for timestamp, state in guard_states:
        if isinstance(state, int):
            id = state
        elif state == 'asleep':
            tic = timestamp
        elif state == 'awake':
            toc = timestamp
            time_asleep = toc.minute - tic.minute
            c.update({id: time_asleep})
    return c

def get_most_minute(guard_states):
    c = defaultdict(Counter)
    for timestamp, state in guard_states:
        if isinstance(state, int):
            id = state
        elif state == 'asleep':
            tic = timestamp
        elif state == 'awake':
            toc = timestamp
            c[id].update(range(int(tic.minute), int(toc.minute)))
    return c
    

states = parse_states('data04.txt')
states = sorted(states, key=lambda x: x[0])

test = states[2][0] - states[1][0]

sleeps = get_sleeps(states)
most_mins = get_most_minute(states)
print(sleeps.most_common(1))
print(most_mins[521].most_common(1))

agg = []
for id, minutes_count in most_mins.items():
    temp = minutes_count.most_common(1)
    minute = temp[0][0]
    count = temp[0][1]
    agg.append((id, minute, count))

agg = sorted(agg, key= lambda x: x[-1], reverse=True)
print(agg[0])