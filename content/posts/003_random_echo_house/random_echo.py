#!/usr/bin/env python3
import random
from dataclasses import dataclass
from functools import reduce, partial
from typing import Any, Callable, Sequence

# TODO mention that this requires a pip-install
from more_itertools import always_iterable

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


class House:  # type: ignore
    # fmt: off
    def __init__(
        self,
        order: PoemTransform = identity,
        format: PoemTransform = identity,
    ):
        self.lines = order(format(HOUSE_POEM))

    # fmt: on
    def recite(self, stanza: int | None = None) -> None:
        if stanza is None:
            indices = range(len(self.lines))
        else:
            indices = always_iterable(stanza)

        for i in indices:
            stanza_lines = self.lines[-(i + 1) :]
            joined = "\n".join(stanza_lines)
            print("This is ", joined, ".", sep="", end="\n\n")

# House(order=random_order, format=echo_format).recite()

# Attempt 2: function that accepts any number of `PoemTransform`s
def recite(*funcs: PoemTransform, stanza: int | None = None) -> None:
    lines = reduce(lambda lines, f: f(lines), funcs, HOUSE_POEM)

    if stanza is None:
        indices = range(len(lines))
    else:
        indices = always_iterable(stanza)

    for i in indices:
        stanza_lines = lines[-(i + 1) :]
        joined = "\n".join(stanza_lines)
        print("This is ", joined, ".", sep="", end="\n\n")



random_house = partial(recite, random_order)
# echo_house = partial(recite, echo_format)
# random_echo_house = partial(recite, random_order, echo_format)
# random_linum_house = partial(recite, random_order, linum_format)
# linum_random_house = partial(recite, linum_format, random_order)
brick_house = partial(recite, echo_format, linum_format, random_order)

# brick_house(stanza=9)

# TODO key difference is that House evaluates the composed functions once. This
# evaluates on `recite` and on `num_stanzas`


@dataclass
class House:
    order: PoemTransform = identity
    fmt: PoemTransform = identity

    def recite(self, stanza: int | None = None) -> None:
        recite(self.order, self.fmt, stanza=stanza)


# TODO introduce in article
Poet = Callable[[int | None], None]

# Attempt 3: keeping stateful transformed version bound
def poet(*funcs: PoemTransform) -> Poet:
    lines = reduce(lambda lines, f: f(lines), funcs, HOUSE_POEM)

    def _recite_stanza(stanza: int | None) -> None:
        if stanza is None:
            indices = range(len(lines))
        else:
            indices = always_iterable(stanza)

        for i in indices:
            stanza_lines = lines[-(i + 1) :]
            joined = "\n".join(stanza_lines)
            print("This is ", joined, ".", sep="", end="\n\n")

    return _recite_stanza

random_linum_house = poet(random_order, linum_format)
random_linum_house(9)
# This is 0: the farmer sowing his corn that kept
# 1: the horse and the hound and the horn that belonged to
# 2: the man all tattered and torn that kissed
# 3: the house that Jack built
# 4: the cow with the crumpled horn that tossed
# 5: the maiden all forlorn that milked
# 6: the rat that ate the cheese that lay in
# 7: the rooster that crowed in the morn that woke
# 8: the judge all shaven and shorn that married
# 9: the dog that worried the cat that chased.

# TODO stumbled into single-function PoemTransform decorator


random_echo_house = poet(random_order, echo_format)

# TODO memory profile of:
#  - HOUSE_POEM as module constant + transform + pass to recite_stanza
#  - This functional version where we just initialize HOUSE_POEM

# Performance benchmark?

# `recite` examples
# recite()
# recite(stanza=0)
# recite(random_order, echo_format, stanza=9)
# recite(echo_format, linum_format, random_order, stanza=5)
