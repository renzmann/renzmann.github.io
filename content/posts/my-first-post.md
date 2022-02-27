---
title: "Objective"
date: 2022-02-26T21:05:32-05:00
draft: true
---

Why create a new tech blog when so many great ones already exist? In short,
because data scientists often get the advice that they should "improve their
software engineering skills," but the advice they are given on how to do so is
usually terrible[^0]. Data science, from a programming perspective, is in a
weird place. We aren't quite front end, but we create maps, charts, and plots to
display in the browser all the time. We aren't quite back end, yet we write code
that targets the file system, databases, and external APIs. And
we aren't quite consultants, but still have to be [the
expert](https://www.youtube.com/watch?v=BKorP55Aqvg), data-driven decision
maker. Data science is not a slow moving field, either, which leaves little
space for pausing to reflect on how to write better code.

I primarily work in a world that uses python, SQL, the standard GNU toolkit
(bash, make, vim), and the usual host of data-sciency things that come with all
that, like Jupyter and plotly. My posts will be largely targeted at an audience
who intends to become more efficient with these tools, or to introduce those who
want to become better coders, but aren't sure where to start, to some of the
tools that can improve your data science workflow.  For about five years now,
I've mentored Data Science team members from writing their first line of code to
maintaining 100k "source lines of code" (SLOC) code bases[^1]. I've received
(and given) many questions prefaced with "this may be a stupid question,
but...", and thought that it was about time I wrote down the answers for others
to learn from.

[^0]: As of writing, it's February of 2022 and I still see Reddit comments
telling new python programmers that `requirements.txt` is an acceptable form of
project dependency management.

[^1]: Seasoned programming veterans out there might scoff at how low 100k sounds,
but for those accustomed to working mainly in Jupyter notebooks, this can be a
daunting amount of code.
