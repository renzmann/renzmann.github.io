#!/usr/bin/env python3
import random
from dataclasses import dataclass, field
from functools import partial, reduce
from typing import Any, Callable, Optional

PoemTransform = Callable[[list[str]], list[str]]
Pipeline = list[PoemTransform]

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


def random_order(poem: list[str]) -> list[str]:
    return random.sample(poem, len(poem))


def echo_format(poem: list[str]) -> list[str]:
    return [f"{x} {x}" for x in poem]


def linum_format(poem: list[str]) -> list[str]:
    return [f"{i}: {line}" for i, line in enumerate(poem)]


# Option 0: show what that dataclass looks like without @dataclass?
# Option 1: dataclass
@dataclass
class House:
    lines: list[str] = field(default_factory=lambda: HOUSE_POEM)
    order: PoemTransform = identity
    format: PoemTransform = identity

    def recite(self, num_lines: Optional[int] = None) -> None:
        lines = self.lines if num_lines is None else self.lines[-num_lines:]
        formatted = self.format(lines)
        ordered = self.order(formatted)
        joined = " ".join(ordered)
        print("This is ", joined, ".", sep="")


# print("-" * 20, "JACK'S HOUSE", "-" * 20)
# House().recite(3)
# print()

# print("-" * 20, "RANDOM HOUSE", "-" * 20)
# House(order=random_order).recite(3)
# print()

# print("-" * 20, "ECHO HOUSE", "-" * 20)
# House(format=echo_format).recite(3)
# print()

# print("-" * 20, "Dataclass echo house", "-" * 20)
# reh = House(order=random_order, format=echo_format)
# reh.recite(3)
# print()
# reh.recite(1)
# print()


# Option 2: `partial` application of functions
def recite(
    num_lines: int | None = None,
    lines: list[str] = HOUSE_POEM,
    order: PoemTransform = identity,
    format: PoemTransform = identity,
) -> None:

    lines = lines if num_lines is None else lines[-num_lines:]
    formatted = format(lines)
    ordered = order(formatted)
    joined = " ".join(ordered)

    print("This is ", joined, ".", sep="")


# print("-" * 20, "partial echo house", "-" * 20)
# reh_recite = partial(recite, order=random_order, format=echo_format)
# reh_recite(3)
# print()
# reh_recite(5)
# print()


# Option 3: pipeline approach
def pipe_recite(
    poem: list[str] = HOUSE_POEM,
    num_lines: int | None = None,
    pipeline: Pipeline | None = None,
    sep: str = " ",
) -> None:

    if pipeline is None:
        pipeline = []

    poem = poem if num_lines is None else poem[-num_lines:]
    processed = reduce(lambda l, f: f(l), pipeline, poem)
    joined = sep.join(processed)

    print("This is", sep, joined, ".", sep="")


print("-" * 20, "pipeline echo house", "-" * 20)
random_echo_number = partial(
    pipe_recite, pipeline=[echo_format, linum_format, random_order], sep="\n"
)
random_echo_number(num_lines=3)
print()
pipe_recite(num_lines=5, pipeline=[random_order, echo_format])
print()
pipe_recite(num_lines=5, pipeline=[random_order, linum_format], sep="\n")
print()
pipe_recite(num_lines=5, pipeline=[linum_format, random_order])
