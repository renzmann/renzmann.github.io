---
title: "Object-oriented compisition with python's multiple inheritance"
date: 2022-02-26T15:38:14-05:00
draft: true
---

Model Composition

Building new models out of familiar parts.

The Idea

We're going to take a look at an object-oriented concept called mixins.

class RandomOrderMixin:

 def order(self, data: list[str]) -> list[str]:

 cp = data.copy()

 random.shuffle(cp)

 return cp

This class does only one thing - randomly order a list of strings that come in via the order method. The intent is not to use this class on its own, but in conjunction with many other classes that implement a small number of methods, including a final __init__ method. Our final class will use python's multiple inheritance to mimic the mixin flavor of composition.

class FinalModel(RandomOrderMixin, FormatterMixin, BaseModel):

 pass

model = FinalModel() # `model` now has all the methods from the three classes it inherited from

model.order(["one", "two", "three"]) # calls RandomOrderMixin.order(model, ["one", "two", "three])

For an end user, the fact that model is a FinalModel isn't important. All that matters is that it has an order method at runtime. In practice, this could go as far as loading a persistent version of the object from disk, in which case we really don't know the type of model unless we were to inspect it.

model = read_from_joblib("my_model.joblib")

model.order(["a", "new", "list"])

In theory, this means we could swap out my_model.joblib for another type that implements a different order method, but the generic code of reading the object and applying its method remains unchanged.

class NonRandomModel(IdentityOrderMixin, FormatterMixin, BaseModel):

	pass



non_random_model = NonRandomModel()

non_random_model.save("my_model.joblib")

Assuming BaseModel implements save, we've just changed out RandomOrderMixin for IdentityOrderMixin, and created a new object that will still work in the read_from_joblib example.

Example: Jack's House

(Adapted from a great talk on object composition by Sandy Metz)

A popular children's poem is about the house that Jack built. On each verse, a new piece is added that reveals all the players around Jack's house:

This is the house that Jack built.

This is the rat that ate the cheese that lay in the house that Jack built.

This is the dog that worried the cat that chased the rat that ate the cheese that lay in the house that Jack built.

This is the cow with the crumpled horn that tossed the dog that worried the cat that chased the rat that ate the cheese that lay in the house that Jack built.

This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that chased the rat that ate the cheese that lay in the house that Jack built.

This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that chased the rat that ate the cheese that lay in the house that Jack built.

This is the judge all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that chased the rat that ate the cheese that lay in the house that Jack built.

This is the rooster that crowed in the morn that woke the judge all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that chased the rat that ate the cheese that lay in the house that Jack built.

This is the farmer sowing his corn that kept the rooster that crowed in the morn that woke the judge all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that chased the rat that ate the cheese that lay in the house that Jack built.

This is the horse and the hound and the horn that belonged to the farmer sowing his corn that kept the rooster that crowed in the morn that woke the judge all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that chased the rat that ate the cheese that lay in the house that Jack built.

We've been given the task to implement an object with a recite method that takes a verse number as an argument, and prints the appropriate version of the poem. Easy enough -

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

 def recite(self, lines: int = None) -> None:

 if lines is None:

 lines = len(self.data)

 poem = " ".join(self.data[-lines:])

 print("This is ", poem, ".", sep="")



house = House()

house.recite(3)

# This is the dog that worried the cat that chased the rat that ate the

# cheese that lay in the house that Jack built.

house.recite(5)

# This is the maiden all forlorn that milked the cow with the crumpled horn

# that tossed the dog that worried the cat that chased the rat that ate the

# cheese that lay in the house that Jack built.

Fabulous. Our class works great and our clients are happy.

New Feature Request: RandomHouse

"The House is working great!" our clients say. "Just one thing - could we make it so that the lines are randomly ordered before we recite the poem? Thanks."

Now, we don't really want to remove our original House, we just want a slightly modified version that's capable of randomizing the data at initialization. So there's two specializations

A House whose line order is identical to the original version

A house whose line order is random

The role we're looking for is somebody to handle the ordering of our data at instantiation.

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

Now we've explicitly said that House needs a partner mixin that implements an order method, which gets called by __init__ as it sets the data attribute.

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

Making either a RandomHouse or DefaultHouse is just a matter of picking what does the ordering of data when we're composing our final class.

New Feature Request: EchoHouse

"Actually," the client says, "we'd like to have lines repeated when they're recited. The random part is great, but it just doesn't fit our needs at the moment."

"No problem," we say, "we'll just swap out the ordering part for a formatting role."

In truth, we already know that the ordering role can stay, we just use the IdentityOrder mixin. What we actually need, is to include a formatting role into the __init__, like we did before.

class House:

 order: Callable[[list[str]], list[str]]

 format_: Callable[[list[str]], list[str]] # avoiding conflict with the python built-in `format`

 def __init__(self, data: list[str] = None):

 if data is None:

 data = POEM

 self.data = self.format_(self.order(data))

 def recite(self, lines: int = None) -> None:

 # same as above

 ...



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

We have an echoing poem, and once again our clients are pleased.

New Feature Request: RandomEchoHouse

Pleased with our progress so far, the client comes back to us and says, "hey, you remember that random thing? Can we get that back?"

The moment we've been waiting for. "Of course we can!" we say with a smile.

class RandomEchoHouse(EchoFormat, RandomOrder, House):

	pass



random_echo_house = RandomEchoHouse()

random_echo_house.recite()

Further Reading

Our random echo house mandates that the formatting happens after the ordering. To change that, we'd need to edit our House class once again. A follow-up article to this one shows how we could take a related, but slightly modified approach to circumvent this problem.

