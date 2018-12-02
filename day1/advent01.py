from typing import List
from itertools import cycle

def readfile(filepath: str) -> List[int]:
    with open(filepath) as f:
        data = [int(line) for line in f]
    return data

def first_repeat(deltas: List[int], current_freq: int) -> int:
    uniques = set([current_freq])
    for delta in cycle(deltas):
        current_freq += delta
        if current_freq in uniques:
            return current_freq
        else:
            uniques.add(current_freq)

if __name__ == "__main__":
    deltas = readfile('data01.txt')
    print(f'final frequency: {sum(deltas)}')

    s = first_repeat(deltas, 0)
    print(f'first repeated frequency: {s}')