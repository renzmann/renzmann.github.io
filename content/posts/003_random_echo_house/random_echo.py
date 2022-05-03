#!/usr/bin/env python3
import random
from itertools import chain
from typing import Any, Callable, Sequence

Poem = Sequence[str]
PoemTransform = Callable[[Poem], Poem]


# TODO separate lines on "That" for better printing?
HOUSE_POEM = [
        "the horse and the hound and the horn that belonged to",
        "the farmer sowing his corn that kept",
        "the rooster that crowed in the morn that woke",
        "the judge all shaven and shorn that married",
        "the man all tattered and torn that kissed",
        "the maiden all forlorn that milked",
        "the cow with the crumpled horn that tossed",
        "the dog that worried the cat that chased",
        "the rat that ate the cheese that lay in",
        "the house that Jack built",
    ]


def identity(x: Any) -> Any:
    return x


def random_order(poem: Poem, random_seed: int = 42) -> Poem:
    random.seed(random_seed)
    return random.sample(poem, len(poem))


def echo_format(poem: Poem) -> Poem:
    return [f"{line} {line}" for line in poem]


def linum_format(poem: Poem) -> Poem:
    return [f"{i}: {line}" for i, line in enumerate(poem)]


class PipeHouse:
    def __init__(self, *funcs: PoemTransform):
        self.lines = HOUSE_POEM

        for f in funcs:
            self.lines = f(self.lines)

        self.num_stanzas = len(self.lines)


class House:
    def __init__(
        self, order: PoemTransform = identity, format: PoemTransform = identity
    ):
        self.lines = order(format(HOUSE_POEM))

    @property
    def num_stanzas(self) -> int:
        return len(self.lines)

    def _recite_stanza(self, poem: Poem | None, stanza: int = 0) -> None:
        if poem is None:
            poem = self.lines

        lines = poem[-(stanza + 1):]
        joined = "\n".join(lines)

        print("This is ", joined, ".", sep="", end="\n\n")

    def recite(self, stanza: int | None = None) -> None:
        if stanza is not None:
            self._recite_stanza(self.lines, stanza=stanza)
            return

        # stanza is None - Recite the whole poem
        for i in range(self.num_stanzas):
            self._recite_stanza(self.lines, stanza=i)

# House(order=random_order, format=echo_format).recite()

# Attempt 2: Functional version, accepts any number of `PoemTransform`s
from dataclasses import dataclass
from functools import reduce

def _recite_stanza(poem: Poem, stanza: int = 0) -> None:
    stanza_lines = poem[-(stanza + 1):]
    joined = "\n".join(stanza_lines)

    print("This is ", joined, ".", sep="", end="\n\n")


def recite(*funcs: PoemTransform, stanza: int | None = None) -> None:
    lines = reduce(lambda lines, f: f(lines), funcs, HOUSE_POEM)

    if stanza is not None:
        _recite_stanza(lines, stanza=stanza)
        return

    # stanza is None - Recite the whole poem
    for i, _ in enumerate(lines):
        _recite_stanza(lines, stanza=i)

@dataclass
class Esuoh:
    order: PoemTransform = identity
    fmt: PoemTransform = identity

    def recite(self, stanza: int | None = None) -> None:
        recite(self.order, self.fmt, stanza=stanza)

# TODO memory profile of:
#  - HOUSE_POEM as module constant + transform + pass to recite_stanza
#  - This functional version where we just initialize HOUSE_POEM

# Performance benchmark?

# `recite` examples
# recite()
# recite(stanza=0)
# recite(random_order, echo_format, stanza=9)
# recite(echo_format, linum_format, random_order, stanza=5)
