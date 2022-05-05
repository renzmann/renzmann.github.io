---
title: "Data Pipelines as Function Composition"
date: 2022-05-01T00:00:00-00:00
draft: False
---

Some time ago, I saw this fantastic talk by Sandy Metz on favoring object
composition over inheritance.

{{< youtube "OMPfEXIlTVE?t=1020" >}}

At the 17:00 mark, she introduces the cumulative folk tale 
[_This Is the House That Jack Built_][jacks-wiki], the first few lines of which
looks like this:

> This is the house that Jack built.</br></br>
> This is the rat that ate the cheese that lay in the house that Jack
> built.</br></br>
> This is the dog that worried the cat that chased the rat that ate the
> cheese that lay in the house that Jack built.</br></br>
> This is the cow with the crumpled horn that tossed the dog that worried the
> cat that chased the rat that ate the cheese that lay in the house that Jack
> built.</br></br>
> This is the maiden all forlorn that milked the cow with the crumpled horn that
> tossed the dog that worried the cat that chased the rat that ate the cheese
> that lay in the house that Jack built.</br></br>
> ...

For the rest of the talk, she focuses on the problem of programming a class that
can perform two transformations on the poem:

1. Randomize the order in which lines are added, so that the "rat that ate the
   cheese" might come before the "maiden all forlorn"
1. "Echo" each line, so that we get "the cow with the crumpled horn that
   tossed the cow with the crumpled horn"

That is, a user should be able to instantiate a class that can recite the poem
verbatim, recite a randomized poem, recite an echoed poem, or recite a poem
that's both random and echoed.  What makes this interesting as a
data scientist or engineer is that she's tackling the problem of object
composition in the context of a data pipeline.  So in this article, I'm going
to cover Python solutions to this problem in three broad strokes:

1. What does a literal translation of the object-oriented version look like in
   Python, while still remaining "pythonic?"
2. How can we extend the code to swap the order in which transformations happen?
3. How can we simplify the user experience by translating the logic to pure functions?

On point 3 - I tend to believe a functional style, where data is
immutable and pure functions create new, transformed data is _usually_ the right
approach to any system that's "data first".  It also reduces overhead for
most users by avoiding the introduction of a new object type. That's not
always a good thing, but specifically in the context of a _Python end
user_, this means they need only remember the `recite()` function, and not both
the `House()` object _and_ its `recite()` method.  While not strictly a
functional language, Python does offer some key functional components, namely
[first-class functions][first-class-funcs] and [currying][currying], so we'll
take a look at how those can still be useful even when building a more
object-centric solution.

</br>

The Object-Oriented Python Solution to `RandomEchoHouse`
========================================================

<div class="flex px-4 py-2 mb-8 text-base rounded-md bg-primary-100 dark:bg-primary-900">
  <span class="flex items-center ltr:pr-3 rtl:pl-3 text-primary-400">
    {{< icon "triangle-exclamation" >}}
  </span>
  <span class="flex items-center justify-between grow dark:text-neutral-300">
  <span class="prose dark:prose-invert">
    This article uses Python 3.10 syntax. To run examples on older versions of
    Python, some adjustments to the type annotations are required.
  </span>
  </span>
</div>

First, let's set up a new Python file `random_echo.py` with some imports we'll
need, the poem's data as a module constant[^1], and a couple type aliases to
make future code more readable:

```py3
#!/usr/bin/env python3
import random
from typing import Callable

# requires pip install of `more-itertools`
from more_itertools import always_iterable

Poem = list[str]
PoemTransform = Callable[[Poem], Poem]

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
```

So from now on, a `Poem` is any list of string values, just like `HOUSE_POEM`,
and a `PoemTransform` is any function that takes in a `Poem` as its only
argument and returns a `Poem`.

Our objective is to produce variations on this poem using a single interface:

1. Recite the original poem
1. Recite a version of the poem in random order
1. Recite a version of the poem with each line "echoed" (duplicated)
1. Recite the poem both in random order and with duplicated lines

There are three possible transformations of a poem - we echo it, we randomize
it, or we do nothing.  The fourth option is a composition of the two other
non-identity transformations, so we don't consider it a separate object.
Ruby has a much stricter object-oriented paradigm than
Python, so Sandy's example uses a dedicated class with a single method for each
role.  Such ceremony isn't required in Python, though. We can just define a pure
function for each processing step.

```py3
# --snip--
def identity(x: Any) -> Any:
    return x

def random_order(poem: Poem, random_seed: int = 42) -> Poem:
    random.seed(random_seed)
    return random.sample(poem, len(poem))

def echo_format(poem: Poem) -> Poem:
    return [f"{line} {line}" for line in poem]
```

To start, let's look at a literal translation of Sandy's `House` class into
Python:

```py3
# --snip--
class House:
    def __init__(
        self,
        order: PoemTransform = identity,
        fmt: PoemTransform = identity,
    ):
        self.lines = order(fmt(HOUSE_POEM))

    def recite(self, stanza: int | Sequence[int] | None = None) -> None:
        if stanza is None:
            indices = range(len(self.lines))
        else:
            indices = always_iterable(stanza)

        for i in indices:
            stanza_lines = self.lines[-(i + 1) :]
            joined = "\n".join(stanza_lines)
            print("This is ", joined, ".", sep="", end="\n\n")
```

Anyone who's seen Jack Diederich's [_Stop Writing
Classes_][stop-writing-classes] should notice a red flag here.  We have two
methods, one of which is `__init__()`, so that means this class is really just an
obfuscated call to a `recite` function.  In the next section we'll refactor
this down to a flatter API, but for the moment  let's just examine how this
class works by dropping into an interactive session:

```
$ python3 -i random_echo.py
>>> house = House()
>>> house.recite()  # the whole tale
This is the house that Jack built.
...
the rat that ate the cheese that lay in
the house that Jack built.

>>> house.recite(2)  # just stanza 2
This is the dog that worried the cat that chased
the rat that ate the cheese that lay in
the house that Jack built.

>>> # We can "plug in" any function for the `order` role
>>> random_house = House(order=random_order)
>>> random_house.recite(4)
This is the maiden all forlorn that milked
the rat that ate the cheese that lay in
the rooster that crowed in the morn that woke
the judge all shaven and shorn that married
the dog that worried the cat that chased.

>>> # Similarly for the `fmt` role
>>> echo_house = House(fmt=echo_format)
>>> echo_house.recite(3)
This is the cow with the crumpled horn that tossed the cow with the crumpled horn that tossed
the dog that worried the cat that chased the dog that worried the cat that chased
the rat that ate the cheese that lay in the rat that ate the cheese that lay in
the house that Jack built the house that Jack built.

>>> # Including both at once
>>> random_echo_house = House(order=random_order, fmt=echo_format)
>>> random_echo_house.recite()
This is the dog that ...
```

Feature Request: Line Numbers
-----------------------------

"Can we get line numbers before each of the chunks even when randomizing? It
makes it easier to read."

Easy. We just make a new formatter:

```py3
def linum_format(poem: Poem) -> Poem:
    return [f"{i}: {line}" for i, line in enumerate(poem)]

random_line_house = House(fmt=linum_format, order=random_order)
random_line_house.recite(9)
# This is 0: the farmer sowing his corn that kept
# 1: the horse and the hound and the horn that belonged to
# 2: the man all tattered and torn that kissed
# ...
```

"We noticed something," our client says. "It looks like the randomization
happens before the line numbers are made. What we really wanted was to keep the
original line numbers, so we know what happened. But that's fine, we were able
to just swap the two functions and now it's working great!"

To our horror, we open their code and see this:

```py3
myhouse = House(fmt=random_order, order=linum_format)
myhouse.recite(9)
# This is 1: the farmer sowing his corn that kept
# 0: the horse and the hound and the horn that belonged to
# 4: the man all tattered and torn that kissed
# ...
```

And even worse:

```py3
def mynumbers(p):
    return linum_format(echo_format(x))

myhouse2 = House(order=mynumbers, fmt=random_order)
```

Uh oh. We baked the ordering into `House.__init__`, and because we didn't
provide a generic enough API for composing functions, it's getting used in a way
we didn't expect, which will certainly put mental burden on future maintainers
as well.  We now have three options:

1. Force an API change that prevents the situation above
1. Deprecate the `House` class and point users to a newer, better function
1. Open up the public interface with a little more flexibility, at the expense
   of directly representing business logic

In my experience, #1 is rarely prudent.  # 2 may or may not be appropriate,
depending on what the actual product is.  However, as library authors it's our
responsibility to keep the public interface as consistent as possible over time.
So let's explore what it means to abstract our code a little to achieve better
"pluggability":

</br>

Functions First
===============

I want to take it all the way back to the drawing board.  What's the simplest
part we can keep the same?  Probably all of `linum_format`, `echo_format`, and
`random_order` remain unchanged.

Following that, we need a small adjustment to the `recite` function:  given a
`Poem`, just print it out on the correct `stanza`.

```py3
#!/usr/bin/env python3

# Identical to the `House` class version, but doesn't 
# rely on stateful `self.lines`
def recite(poem: Poem, stanza: int| Sequence[int] | None = None) -> None:
    if stanza is None:
        indices = range(len(poem))
    else:
        indices = always_iterable(stanza)

    for i in indices:
        stanza_lines = poem[-(i + 1) :]
        joined = "\n".join(stanza_lines)
        print("This is ", joined, ".", sep="", end="\n\n")
```

With that totally compartmentalized, now we can focus entirely on the
composition part.

Notice that the `House` class utilized one stateful object - the transformed
poem after applying the `order` and `fmt` functions.  This got stored in the
`self.lines` attribute, and subsequent calls for specific stanzas didn't have to
re-transform the poem.  `_recite_stanza` just read the data and printed it.
Depending on how expensive we expect the functions to be, we can either keep
this behavior, or switch to a version where we transform the poem each time we
pass it in to `recite`.  All in all, though, it's impossible for us as library
authors to predict which of these cases our users will be bound to, so it's
actually a poor design in the first place to force this data to persist in
memory without their consent.  The `recite` function now takes _any_ poem.  So
we can pass in a transient, quickly garbage-collected one like this:

```py3
recite(echo_format(HOUSE_POEM))
```

Or collect a transformed version, persist it, and pass that in:

```py3
random_echo_house = echo_format(random_order(HOUSE_POEM))
recite(random_echo_house)
```

In the end, only the developers implementing the data (`HOUSE_POEM`) and the
functions that act on it (`random_order` and `echo_format`) will know which of
the two approaches above is appropriate, so we should give them that freedom.

Next, we have the problem of arbitrary function composition.  It's a bit clunky
to manually produce each composition like this:

```py3
def random_echo(poem: Poem) -> Poem:
    return echo_format(random_order(poem))

def echo_linum(poem: Poem) -> Poem:
    return linum_format(echo_format(poem))

# ... likewise for other combinations
```

Can we provide a generic factory that lets users define a new transformation
pipeline on the fly?  How about a function that takes a variable set of
functions as arguments, and returns a `PoemTransform`?

```py3
def compose(*funcs: PoemTransform) -> PoemTransform:
    def pipeline(poem: Poem) -> Poem:
        for f in funcs:
            poem = f(poem)
        return poem
    return pipeline

linum_echo_random = compose(linum_format, echo_format, random_order)
recite(linum_echo_random(HOUSE_POEM), stanza=9)
# This is 1: the farmer sowing his corn that kept 1: the farmer sowing his corn that kept
# 0: the horse and the hound and the horn that belonged to 0: the horse and the hound and the horn that belonged to
# 4: the man all tattered and torn that kissed 4: the man all tattered and torn that kissed
# ...
```

It might not look like much, but because most programmers prefer reading
function application from left-to-right rather than inside-out (mathematicians
being the notable holdout here), some may prefer this.  If our functions had
varying input and output types, I would keep the slightly clunkier version where
we explicitly compose functions via `def` and `return f1(f2(...))` solely for
the reason of having the [`pyright`][pyright] static type checker ensure that I've chained
inputs and outputs correctly.  Since all of our functions are `PoemTransform`,
though, we don't need to worry about type checking within the `compose`
function. That is, all the input and output types are `Poem`, so the resulting
function chain is safe.

</br>

A note on mixins
================

At around the 23:35 mark, an audience member shouts out "use multiple
inheritance!" to which Sandy says "just stop that - we're not using multiple
inheritance here, it's not the right solution for this problem."

It's interesting to note that `scikit-learn` [does exactly this][ridge-src] as a
way of composing model behavior.  Every ridge regression is a regression, after
all, and it will always need the attributes that come with a regression, such as
its coefficient of determination and `fit()` method.


[jacks-wiki]: <https://en.wikipedia.org/wiki/This_Is_the_House_That_Jack_Built>
[first-class-funcs]: <https://en.wikipedia.org/wiki/First-class_function>
[currying]: <https://en.wikipedia.org/wiki/Currying>
[ridge-src]: <https://github.com/scikit-learn/scikit-learn/blob/920ab2508fe634ad32df68bb0ebd4f4512fbfb53/sklearn/linear_model/_ridge.py#L910>
[monomorphize]: <https://en.wikipedia.org/wiki/Monomorphization>
[stop-writing-classes]: <https://www.youtube.com/watch?v=o9pEzgHorH0>
[pyright]: <https://github.com/Microsoft/pyright>

[^1]: Python doesn't actually have "constants", but by convention an
  all-uppercase variable is meant to signify it's _supposed_ to be constant
