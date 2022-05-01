---
title: "Object-oriented composition with python's multiple inheritance"
date: 2022-05-01T00:00:00-00:00
draft: False
---

A little while ago I saw this fantastic talk by Sandy Metz on object
composition, specifically her example starting at the 17:00 mark until the end.

{{< youtube "OMPfEXIlTVE?t=1020" >}}

In the video, she uses the cumulative folk tale [_This is the house that Jack Built_][jacks-wiki]
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

Her presentation is aimed at Ruby, which has a much stricter object-oriented
model than python does. Interestingly,  taking the raw poem and producing a
modified version of it is just a data pipeline, and she's tackling the problem
of composing pieces of this pipeline together.  I tend to believe a functional
style, where data is immutable and pure functions create new, transformed data
is _usually_ the right approach to any system that's "data first".  While not
strictly a functional language, python does offer some key functional pieces,
namely [first-class functions][first-class-funcs] and [currying][currying].

<div class="flex px-4 py-2 mb-8 text-base rounded-md bg-primary-100 dark:bg-primary-900">
  <span class="flex items-center ltr:pr-3 rtl:pl-3 text-primary-400">
    {{< icon "triangle-exclamation" >}}
  </span>
    <span class="flex items-center justify-between grow dark:text-neutral-300">
        <span class="prose dark:prose-invert">
            This article uses python 3.10 syntax. To run examples on older
            versions of python, some adjustments to the type annotations are
            required.
        </span>
    </span>
  </span>
</div>

# TODO Quick intro on composition, functional style
# TODO mention that we're dealing with a data pipeline, and functional style is great for that
# TODO mention python3.10 syntax
# TODO mention "monomorphization?"

# TODO kill this section

The Idea

We're going to take a look at an object-oriented concept called mixins.

```py3
class RandomOrderMixin:
    def order(self, data: list[str]) -> list[str]:
        return random.sample(data, len(data))
```

This class does only one thing - randomly order a list of strings that come in
via the order method. The intent is not to use this class on its own, but in
conjunction with many other classes that implement a small number of methods,
including a final `__init__` method. Our final class will use python's multiple
inheritance to mimic the mixin flavor of composition.

```py3
class FinalModel(RandomOrderMixin, FormatterMixin, BaseModel):
    pass

model = FinalModel() # `model` now has all the methods from the three classes it inherited from
model.order(["one", "two", "three"]) # calls RandomOrderMixin.order(model, ["one", "two", "three])
```

# TODO mention "duck-typing" and how it's accepted by python
For an end user, the fact that model is a `FinalModel` isn't important. All that
matters is that it has an order method at runtime. In practice, this could go as
far as loading a persistent version of the object from disk, in which case we
really don't know the type of model unless we were to inspect it.

```py3
model = read_from_joblib("my_model.joblib")
model.order(["a", "new", "list"])
```

In theory, this means we could swap out `my_model.joblib` for another type that
implements a different order method, but the generic code of reading the object
and applying its method remains unchanged.

```py3
class NonRandomModel(IdentityOrderMixin, FormatterMixin, BaseModel):
    pass

non_random_model = NonRandomModel()
non_random_model.save("my_model.joblib")
```



Assuming `BaseModel` implements `save`, we've just changed out `RandomOrderMixin` for
`IdentityOrderMixin`, and created a new object that will still work in the
`read_from_joblib` example.

Example: Jack's House
=====================

(Adapted from a great talk on object composition by Sandy Metz)

A popular children's poem is about the house that Jack built. On each verse, a
new piece is added that reveals all the players around Jack's house:

    This is the house that Jack built.

    This is the rat that ate the cheese that lay in the house that Jack built.

    This is the dog that worried the cat that chased the rat that ate the cheese
    that lay in the house that Jack built.

    This is the cow with the crumpled horn that tossed the dog that worried the cat
    that chased the rat that ate the cheese that lay in the house that Jack built.

    This is the maiden all forlorn that milked the cow with the crumpled horn that
    tossed the dog that worried the cat that chased the rat that ate the cheese that
    lay in the house that Jack built.

    This is the man all tattered and torn that kissed the maiden all forlorn that
    milked the cow with the crumpled horn that tossed the dog that worried the cat
    that chased the rat that ate the cheese that lay in the house that Jack built.

    This is the judge all shaven and shorn that married the man all tattered and
    torn that kissed the maiden all forlorn that milked the cow with the crumpled
    horn that tossed the dog that worried the cat that chased the rat that ate the
    cheese that lay in the house that Jack built.

    This is the rooster that crowed in the morn that woke the judge all shaven and
    shorn that married the man all tattered and torn that kissed the maiden all
    forlorn that milked the cow with the crumpled horn that tossed the dog that
    worried the cat that chased the rat that ate the cheese that lay in the house
    that Jack built.

    This is the farmer sowing his corn that kept the rooster that crowed in the morn
    that woke the judge all shaven and shorn that married the man all tattered and
    torn that kissed the maiden all forlorn that milked the cow with the crumpled
    horn that tossed the dog that worried the cat that chased the rat that ate the
    cheese that lay in the house that Jack built.

    This is the horse and the hound and the horn that belonged to the farmer sowing
    his corn that kept the rooster that crowed in the morn that woke the judge all
    shaven and shorn that married the man all tattered and torn that kissed the
    maiden all forlorn that milked the cow with the crumpled horn that tossed the
    dog that worried the cat that chased the rat that ate the cheese that lay in the
    house that Jack built.

We've been given the task to implement an object with a recite method that takes
a verse number as an argument, and prints the appropriate version of the poem.
Easy enough -

```py3
POEM = [
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

class House:
    def __init__(self, data: list[str] = None):
        if data is None:
            data = POEM

        self.data = data

    def recite(self, num_lines: int = None) -> None:
        if num_lines is None:
            num_lines = len(self.data)

        poem = " ".join(self.data[-num_lines:])

        print("This is ", poem, ".", sep="")


house = House()

house.recite(3)
# This is the dog that worried the cat that chased the rat that ate the
# cheese that lay in the house that Jack built.

house.recite(5)
# This is the maiden all forlorn that milked the cow with the crumpled horn
# that tossed the dog that worried the cat that chased the rat that ate the
# cheese that lay in the house that Jack built.
```

Fabulous. Our class works and our clients are happy.


New Feature Request: RandomHouse
================================

"The `House` is fantastic!" our clients say. "Just one thing - could we make
it so that the lines are randomly ordered before we recite the poem? Thanks."

Now, we don't really want to remove our original House, we just want a slightly
modified version that's capable of randomizing the data at initialization. So
there are two specializations:

1. A House whose line order is identical to the original version
1. A house whose line order is random

The role we're looking for is somebody to handle the ordering of our data at instantiation.

```py3
from typing import Callable

class House:

    order: Callable[[list[str]], list[str]]

    def __init__(self, data: list[str] = None):
        if data is None:
            data = POEM

        self.data = self.order(data)

        def recite(self, lines: int = None) -> None:
            # same as above
            ...
```

# TODO there was a reason for not using abstract methods ... Why?

Now we've explicitly said that House needs a partner mixin that implements an
order method, which gets called by `__init__` as it sets the data attribute.

```py3
class IdentityOrder:
    def order(self, data: list[str]) -> list[str]:
        return data

class RandomOrder:
    def order(self, data: list[str]) -> list[str]:
        cp = data.copy()
        random.shuffle(cp)
        return cp


class DefaultHouse(IdentityOrder, House):
    pass


class RandomHouse(RandomOrder, House):
    pass


default_house = DefaultHouse()
default_house.recite() # Prints the poem just as before

random_house = RandomHouse()
random_house.recite() # Prints the poem in a random order
```

Making either a `RandomHouse` or `DefaultHouse` is just a matter of picking what
does the ordering of data when we're composing our final class.

New Feature Request: `EchoHouse`
==============================

"Actually," the client says, "we'd like to have lines repeated when they're
recited. The random part is great, but it just doesn't fit our needs at the
moment."

"No problem," we say, "we'll just swap out the ordering part for a formatting
role."

In truth, we already know that the ordering role can stay, we just use the
`IdentityOrder` mixin. What we actually need, is to include a formatting role into
the `__init__`, like we did before.

```py3
class House:

    order: Callable[[list[str]], list[str]]
    format_: Callable[[list[str]], list[str]]  # Trailing `_` avoids conflict with the python built-in `format`

    def __init__(self, data: list[str] = None):
        if data is None:
            data = POEM

        self.data = self.format_(self.order(data))

    def recite(self, lines: int = None) -> None:
        # same as above
        ...
```


```py3
class IdentityFormat:
    def format_(self, data: list[str]) -> list[str]:
        return data


class EchoFormat:
    def format_(self, data: list[str]) -> list[str]:
        return [" ".join([line, line]) for line in data]


class EchoHouse(EchoFormat, IdentityOrder, House):
    pass


eh = EchoHouse()
eh.recite(2)

# This is the rat that ate the cheese that lay in the rat

# that ate the cheese that lay in the house that Jack built

# the house that Jack built.
```

New Feature Request: `RandomEchoHouse`
====================================

Pleased with our progress so far, the client comes back to us and says, "hey,
you remember that random thing? Can we get that back?"

The moment we've been waiting for. "Of course we can!" we say with a smile.

```py3
class RandomEchoHouse(EchoFormat, RandomOrder, House):
    pass


random_echo_house = RandomEchoHouse()
random_echo_house.recite()
```

# TODO kill this section, copied from "nirvana"
In the Model Composition story, we built a RandomEchoHouse out of composable
blocks with two known roles in the base class: formatting and ordering. Each
time a new role became clear, we had to edit the original House object with a
data processing step. The __init__ method had two distinct stages:

class House:

 def __init__(self, data: list[str] = None):

 if data is None:

 data = POEM

 self.data = self.format_(self.order(data))

I find it annoying that if we wanted to add a new role to our House, we'd need
to go edit the self.format_(self.order(data)) again. What if we could compose
data processing steps without explicitly declaring them in the base object we
are inheriting from? Is it really the role of the House to understand what's
happening to the data at instantiation, or is it good enough to just have it
access and recite? I think the latter is plausible, and more in line with what
Sandy Metz was getting at in her original talk.

Here's a fake example of what could happen.

Feature Request: Line Numbers
=============================

"Can we get line numbers before each of the chunks even when randomizing? It
makes it easier to read."

Easy. We just make a new formatter mixin:

class LineNumberFormat:

 def format_(self, data: list[str]) -> list[str]:

 return [f"{i}: {line}\n" for i, line in enumerate(data)]

class NumberedRandomHouse(LineNumberFormat, RandomOrder, House):

 pass

We hand this off and call it a day. What could go wrong?

"We noticed something," our client says. "It looks like the randomization
happens before the line numbers are made. What we really wanted was to keep the
original line numbers, so we know what happened. But, we have people already
relying on the current way it works, so we'll need a version like that too."

Uh oh. We've baked the ordering into House.__init__, and isn't something we can
fix with the composition method we've already set out on. We'll have to make
House more generic, so that all these future requests can happen at a layer of
abstraction above House, and so that we never have to go back to it.

In case you want to copy the code and don't want to go back to the first
article, here's the poem data and the imports you need:

import random

POEM = [

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

Challenge: Implement RandomEchoHouse and NumberedRandomHouse Without Modifying House
====================================================================================

Our original House just declared a recite method after initializing the data.
Suppose for good measure the author of House expected overrides to data and
provided getter and setter methods ("descriptors" in python) for it:

class House:

 def __init__(self, data: list[str] = None):

 if data is None:

 data = POEM

 self.data = data

 def recite(self, lines: int = None) -> None:

 if lines is None:

 lines = len(self.data)

 poem = " ".join(self.data[-lines:])

 print("This is ", poem, ".", sep="")

 @property

 def data(self) -> list[str]:

 return self.__data

 @data.setter

 def data(self, value: list[str]):

 self.__data = value

Some find the dunder __data method of storing "private" information distasteful, because it invokes python's name-mangling. I like it, since now future inheritors are still free to use their own _data (single underscore) attribute, should they need it. We're going to be overriding the behavior of the data descriptors, so we won't need to access the underlying __data (dunder) version anyway.

Consider the task we originally set out for: we have two data processing steps, each of which performs some processing at initialization time, and saving the processed result into self.data. We're going to super-charge the overriding behavior by calling up the method resolution order (MRO) stack to apply all data processing steps from our mixin classes. Then, the order of inheritance allows us to change whether the formatting or randomization happens first.

Goal: Customizable Behavior via MRO

Ultimately, we want to be able to build our client's final classes with a single call each:

class NumberedRandomHouse(LineNumberFormat, RandomOrder, House):

 pass

class RandomNumberedHouse(RandomOrder, LineNumberFormat, House):

 pass

The first class will add the line numbers first, then randomly order. The latter will do the opposite.

Defining data Descriptors

Let's tackle the RandomOrder first, since I have a feeling the formatting classes will closely follow suit. The easy part is accessing the data if it's already been set:

class RandomOrder:

 @property

 def data(self) -> list[str]:

 return super().data

If we are mixing this class alone with House, then the House.data method will be overridden. But House is in charge of storing the actual data, since we used the name-mangled __data attribute to store it. Calling super() searches up the MRO for any declaration of a data descriptor, and pulls data from that. Since our House class has such a property, we'll get back the value of House._House__data when we call on the data attribute from a RandomHouse.

Harder is the setting component of this. How do we apply this mixin's processing step and all of the other processing steps above this mixin when we go to set the data? Again, we turn to super(), but we'll also need a bit of knowledge on how descriptors work. The @property decorator along with a partnering @<name>.setter actually turns the data method into an object with __get__ and __set__ methods, that tell python what to do when accessing the decorated name. We can use that information to define our mixin's setter:

class RandomOrder:

 @property

 def data(self) -> list[str]:

 return super().data

 @data.setter

 def data(self, value) -> None:

 cp = value.copy()

 random.shuffle(cp)

 super(RandomOrder, self.__class__).data.__set__(self, cp)

The setter is the same as our original RandomOrder.order(), except we call up the MRO and tell it to continue processing the data via __set__. If the next class we inherited from is House, then it doesn't do any more processing and just saves the data. If the next class is an EchoFormat or LineFormat, then we'll get some formatting after the randomization.

Let's check that this works as expected:

class RandomHouse(RandomOrder, House):

 pass

random_house = RandomHouse()

random_house.recite()

# This is the ...

Now the fun part - let's do the same for our line formatting role:

class LineNumberFormat:

 @property

 def data(self):

 return super().data

 @data.setter

 def data(self, value: list[str]):

 numbered = [f"\n{i + 1}: {line}" for i, line in enumerate(value)]

 super(LineNumberFormat, self.__class__).data.__set__(self, numbered)

class NumberedRandomHouse(LineNumberFormat, RandomOrder, House):

 pass

num_rand_house = NumberedRandomHouse()

num_rand_house.recite(2)

# This is

# 6: the maiden all forlorn that milked

# 4: the judge all shaven and shorn that married.

class RandomNumberedHouse(RandomOrder, LineNumberFormat, House):

 pass

rand_num_house = RandomNumberedHouse()

rand_num_house.recite(2)

# This is

# 9: the cow with the crumpled horn that tossed

# 10: the farmer sowing his corn that kept.

We'll leave it as an exercise to verify that the the line numbers match the original in the first example, questionable as the ethics of the resulting poem might be when reciting to children. The second example, rand_num_house also matches what we expect. The lines were randomized, but we're only printing out the last two, so we get lines 9 and 10 after the randomization.

Further Composability

We've achieved our original goal, but we've actually gotten more from it than we first expected. We can even make numbered echo houses, or echoed number houses too:

class EchoFormat:

 @property

 def data(self):

 return super().data

 @data.setter

 def data(self, value: list[str]):

 numbered = [f"{line} {line}" for line in value]

 super(LineNumberFormat, self.__class__).data.__set__(self, numbered)

class EchoNumberHouse(EchoFormat, LineNumberFormat, House):

 pass

class NumberEchoHouse(LineNumberFormat, EchoFormat, House):

 pass

Decomposing Our Composables

Each of our mixins has a lot of copy-paste going on, with just a single line in the setter method changing between them. That's a sign of a missing abstraction. The getting/setting boilerplate seems to be a common functionality that's mixed in just like everything else. So let's separate those duties:

```py3
class DataProcessor:
    process_data: Callable[[list[str]], list[str]]

    @property
    def data(self) -> list[str]:
        return super().data

    @data.setter
        def data(self, value) -> None:
            processed = self.process_data(value)
            super(RandomOrder, self.__class__).data.__set__(self, processed)


class RandomOrder(DataProcessor):
    def process_data(self, value: list[str]) -> list[str]:
        return random.sample(value, len(value))


class EchoFormat(DataProcessor):
    def process_data(self, value: list[str]) -> list[str]:
        return [f"{line} {line}" for line in value]


class LineNumberFormat(DataProcessor):
    def process_data(self, value: list[str]) -> list[str]:
        return [f"\n{i + 1}: {line}" for i, line in enumerate(value)]
```

A note on mixins
================

At around the 23:35 mark, an audience member shouts out "just use multiple
inheritance!" to which Sandy says "just stop that - we're not using multiple
inheritance here, it's not the right solution for this problem.

# TODO scikit-learn example of multiple inheritance

Further reading
===============

[Real python article on first-class functions]

[jacks-wiki]: <https://en.wikipedia.org/wiki/This_Is_the_House_That_Jack_Built>
[first-class-funcs]: <https://en.wikipedia.org/wiki/First-class_function>
[currying]: <https://en.wikipedia.org/wiki/Currying>
[ridge-src]: <https://github.com/scikit-learn/scikit-learn/blob/920ab2508fe634ad32df68bb0ebd4f4512fbfb53/sklearn/linear_model/_ridge.py#L910>
