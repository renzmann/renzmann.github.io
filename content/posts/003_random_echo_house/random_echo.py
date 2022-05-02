#!/usr/bin/env python3
import random
from itertools import chain
from typing import Any, Callable, Sequence

Poem = Sequence[str]
PoemTransform = Callable[[Poem], Poem]


# TODO separate lines on "That" for better printing?
def house_poem() -> Poem:
    # TODO avoid duplicating data in memory by making this
    # a constant function instead of a module constant
    return [
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
    return list(chain.from_iterable(zip(poem, poem)))


def linum_format(poem: Poem) -> Poem:
    return [f"{i}: {line}" for i, line in enumerate(poem)]



# Attempt 1: Literal translation
# Introduce code this way - show how we hit a problem when we want numbering + random
# class House:
#     def __init__(
#         self, order: PoemTransform = identity, format: PoemTransform = identity
#     ):
#         self.lines = order(format(house_poem()))

#     def stanza(self, stanza: int = 0) -> None:
#         lines = self.lines[-(stanza + 1):]
#         joined = "\n".join(lines)
#         print("This is ", joined, ".", sep="", end="\n\n")

#     def recite(self) -> None:
#         for i in range(1, len(self.lines) + 1):
#             self.stanza(i)

# House(order=random_order, format=echo_format).recite()


# Attempt 2: Functional version, accepts any number of `PoemTransform`s
def _recite_stanza(poem: Poem | None = None, stanza: int = 0) -> None:
    if poem is None:
        poem = house_poem()

    stanza_lines = poem[-(stanza + 1):]
    joined = "\n".join(stanza_lines)

    print("This is ", joined, ".", sep="", end="\n\n")


def recite(*funcs: PoemTransform, stanza: int | None = None) -> None:
    lines = house_poem()

    for f in funcs:
        lines = f(lines)

    # TODO note on "clever one-liner"
    # lines = reduce(lambda lines, f: f(lines), funcs, house_poem())

    if stanza is not None:
        _recite_stanza(lines, stanza=stanza)
        return

    # stanza is None - Recite the whole poem
    for i, _ in enumerate(lines):
        _recite_stanza(lines, stanza=i)


# TODO memory profile of:
#  - HOUSE_POEM as module constant + transform + pass to recite_stanza
#  - This functional version where we just initialize house_poem()

# Performance benchmark?

# `recite` examples
# recite()
# recite(stanza=0)
# recite(random_order, echo_format, stanza=9)
recite(echo_format, linum_format, random_order, stanza=5)
