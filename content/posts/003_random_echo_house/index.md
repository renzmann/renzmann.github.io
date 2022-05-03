---
title: "(Python) Composition and Data Pipelines"
date: 2022-05-01T00:00:00-00:00
draft: False
---

TODO better title - some working thoughts:
  - object orientation
  - random echo house
  - composition vs. inheritance
  - data, immutability, functional concepts in python

Some time ago, I saw this fantastic talk by Sandy Metz on object
composition, specifically her example starting at the 17:00 mark until the end.

{{< youtube "OMPfEXIlTVE?t=1020" >}}

In the video, she uses the cumulative folk tale [_This Is the House That Jack Built_][jacks-wiki]
as the basis for transformation. The poem's first few lines looks like this:

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

Sandy then goes on to define a couple transformations of this poem

1. Randomize the order in which lines are added, so that the "rat that ate the
   cheese" might come before the "maiden all forlorn"
1. "Echo" some lines, so that we get "the cow with the crumpled horn that
   tossed the cow with the crumpled horn"

What makes this interesting as a data scientist or engineer is that taking the
raw poem and producing a modified version of it is just a data pipeline, and
she's tackling the problem of composing pieces of this pipeline together.  So in
this article, we're going to cover python solutions to this problem in three
broad strokes:

1. What does a literal translation of the object-oriented version look like in
   python, while still remaining "pythonic?"
2. How can we extend the code to swap the order in which transformations happen?
3. How can we simplify the user API by translating the logic to a pure function?

On point 3 - I tend to believe a functional style, where data is
immutable and pure functions create new, transformed data is _usually_ the right
approach to any system that's "data first".  Also, it reduces overhead for
_most_ users by avoiding the introduction of a new object type. That's not
always considered a good thing, but specifically in the context of a _python end
user_, this means they need only remember the `recite()` function, and not both
the `House()` object _and_ its `recite()` method.  While not strictly a
functional language, python does offer some key functional components, namely
[first-class functions][first-class-funcs] and [currying][currying], so we'll
take a look at how those can still be useful even when building a more
object-centric solution.

</br>
</br>

The object-oriented python solution to `RandomEchoHouse`
========================================================

</br>
<div class="flex px-4 py-2 mb-8 text-base rounded-md bg-primary-100 dark:bg-primary-900">
  <span class="flex items-center ltr:pr-3 rtl:pl-3 text-primary-400">
    {{< icon "triangle-exclamation" >}}
  </span>
  <span class="flex items-center justify-between grow dark:text-neutral-300">
  <span class="prose dark:prose-invert">
    This article uses python 3.10 syntax. To run examples on older versions of
    python, some adjustments to the type annotations are required.
  </span>
  </span>
</div>
</br>

First, let's set up a new python file `random_echo.py` with some imports we'll
need, the poem's data as a module constant[^1], and a couple type aliases to
make future code more readable:

```py3
#!/usr/bin/env python3
import random
from typing import Callable

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

Our objective is to produce variable versions of this poem using a single
interface:

1. Recite the original poem
1. Recite a version of the poem in random order
1. Recite a version of the poem with each of the lines "echoed" (duplicated)
1. Recite the poem both in random order and with duplicated lines

There are three possible transformations of a poem - we echo it, we randomize
it, or we do nothing. Ruby has a much stricter object-oriented paradigm than
python, so Sandy's example uses a dedicated class with a single method for each
role.  Such ceremony isn't required in python, though. We can just define a pure
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

Now we're ready to define a `House` that can `recite()` the poem:

```py3
# --snip--
class House:
    def __init__(
        self,
        order: PoemTransform = identity,
        fmt: PoemTransform = identity
    ):
        self.lines = order(fmt(HOUSE_POEM))

    @property
    def num_stanzas(self) -> int:
        return len(self.lines)

    def _recite_stanza(self, poem: Poem, stanza: int = 0) -> None:
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
```

TODO: note on the `@property` part?

Let's see how this class works by dropping into an interactive session:

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

```
>>> def linum_format(poem: Poem) -> Poem:
...     return [f"{i}: {line}" for i, line in enumerate(poem)]
>>> random_line_house = House(fmt=linum_format, order=random_order)
>>> random_line_house.recite(9)
This is 0: the farmer sowing his corn that kept
1: the horse and the hound and the horn that belonged to
2: the man all tattered and torn that kissed
...
```

"We noticed something," our client says. "It looks like the randomization
happens before the line numbers are made. What we really wanted was to keep the
original line numbers, so we know what happened. But that's fine, we were able
to just swap the two functions and now it's working great!"

To our horror, we open their code and see this:

```
>>> myhouse = House(fmt=random_order, order=linum_format)
>>> myhouse.recite(9)
This is 1: the farmer sowing his corn that kept
0: the horse and the hound and the horn that belonged to
4: the man all tattered and torn that kissed
...
```

And worse: TODO manual composition of two functions into one and passing that into one of the keywords

Uh oh. We baked the ordering into `House.__init__`, and because we didn't
provide a generic enough API, it's getting used in a way we didn't expect.
We now have two options:

1. Force an API change that prevents the situation above
1. Open up the public interface with a little more flexibility, at the expense
   of directly representing business logic

In my experience, #1 is rarely prudent.  So let's explore what it
means to abstract the `__init__` a little to achieve better "pluggability":

TODO copy over functions `recite` and `_recite_stanza`

TODO note on "clever one liner" with `reduce`

We can then point our customer to the more generic API for their line
number + random order request:

```py3
>>> recite(linum_format, random_order)
This is the 0: ...
```

</br>
</br>

Functional RandomEchoHouse
==========================


A note on mixins
================

At around the 23:35 mark, an audience member shouts out "just use multiple
inheritance!" to which Sandy says "just stop that - we're not using multiple
inheritance here, it's not the right solution for this problem.

TODO scikit-learn example of multiple inheritance


[jacks-wiki]: <https://en.wikipedia.org/wiki/This_Is_the_House_That_Jack_Built>
[first-class-funcs]: <https://en.wikipedia.org/wiki/First-class_function>
[currying]: <https://en.wikipedia.org/wiki/Currying>
[ridge-src]: <https://github.com/scikit-learn/scikit-learn/blob/920ab2508fe634ad32df68bb0ebd4f4512fbfb53/sklearn/linear_model/_ridge.py#L910>

[^1]: Python doesn't actually have "constants", but by convention an
  all-uppercase variable is meant to signify it's _supposed_ to be constant
