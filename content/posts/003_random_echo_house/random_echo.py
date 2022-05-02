#!/usr/bin/env python3
import random
from functools import partial, reduce
from itertools import chain
from typing import Any, Callable, Sequence, Tuple

Poem = Sequence[str]
PoemTransform = Callable[[Poem], Poem]
Pipeline = Sequence[PoemTransform]


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
# class House:
#     def __init__(
#         self, order: PoemTransform = identity, format: PoemTransform = identity
#     ):
#         self.lines = order(format(house_poem()))

#     def stanza(self, stanza: int = 0) -> None:
#         lines = self.lines[-stanza:]
#         joined = " ".join(lines)
#         print("This is ", joined, ".", sep="", end="\n\n")

#     def recite(self) -> None:
#         for i in range(1, len(self.lines) + 1):
#             self.stanza(i)


# reh = House(order=random_order, format=echo_format)
# House(order=random_order, format=echo_format).recite()


# Part 1: functional random echo house
# def recite(
#     order: PoemTransform = identity,
#     format: PoemTransform = identity,
# ) -> None:

#     # TODO incorrect - need to store the randomized, formatted data
#     # so that we can recite each line in turn
#     lines = order(format(HOUSE_POEM))

#     for i in range(1, len(lines) + 1):
#         joined = " ".join(lines[-i:])
#         print("This is ", joined, ".", sep="", end="\n\n")

# TODO what about stateful recovery of stanzas?

# Option 3: `build`
# def build(*funcs: PoemTransform, linesep: str = " ") -> Tuple[Callable[[], None], Callable[[int], None]]:
#     lines = reduce(lambda l, f: f(l), funcs, house_poem())

#     def stanza(i: int = 0) -> None:
#         joined = linesep.join(lines[-i:])
#         print("This is ", joined, ".", sep="", end="\n\n")

#     def recite() -> None:
#         for i in range(1, len(lines) + 1):
#             stanza(i)

#     return recite, stanza


# rec, stan = build(echo_format)
# stan(10)
# rec()

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
    # lines = reduce(lambda l, f: f(l), funcs, HOUSE_POEM)

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

recite()
print()

# Option 4: Separate steps and make transparent
# def transform(data: Poem, *funcs: PoemTransform):
#     return reduce(lambda l, f: f(l), funcs, data)

# print("-" * 20, "Func House", "-" * 20)
# random_echo = partial(recite, order=random_order, format=echo_format)
# random_echo()

# Part 2 - arbitrary ordering and more transformations
# TODO in the webpage, can we highlight the diff?
# class PipeHouse:
#     def __init__(self, *funcs: PoemTransform, sep: str = " "):
#         self.lines = reduce(lambda l, f: f(l), funcs, house_poem())
#         self.sep = sep

#     def stanza(self, i: int = 0) -> None:
#         lines = self.lines[-i:]
#         joined = self.sep.join(lines)
#         print("This is ", joined, ".", sep="", end="\n\n")

#     def recite(self) -> None:
#         for i in range(1, len(self.lines) + 1):
#             self.stanza(i)


# print("-" * 20, "Pipe House", "-" * 20)
# h = PipeHouse(echo_format, linum_format, random_order, sep="\n")

# h.stanza(4)
# h.stanza(2)

