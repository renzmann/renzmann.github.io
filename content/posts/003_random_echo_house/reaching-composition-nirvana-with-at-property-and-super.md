---
title: "Reaching Composition Nirvana With @property and super()"
date: 2022-02-26T15:38:14-05:00
draft: true
---

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

Now I think we've done it. Three perfectly pluggable data processors that we can
use in any combination. Moreover, we've nixed the requirement of defining an
identity operator version of each mixin, dropping the amount of code required to
make a default implementation.


