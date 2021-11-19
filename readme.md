# Learn Log

This repo is to track what I am learning and what I have learned in the past.
The main reason for this is to keep me focused on what I am doing at one time and
also track how much time I spend doing what.

## Structure

There are just two files: this readme and `log.json`.
The readme contains a high-level overview of what I am currently learning/reading/doing.
The log consists of a list of entries, each entry including the what/when and a brief note.

---

## Current

### (Reading/Doing) - [Haskell in Depth](https://www.manning.com/books/haskell-in-depth)

## Previous
Nice book, fairly concrete, giving an overview of architecture types and an idea of the nuance that goes into making
decisions. Also touches on managing complexity, breaking up systems and soft skills.

#### What I learned
Better knowledge of a number of architecture styles and their pros/cons. Architecture characteristics, modularity.
A general overview of the aspects involved within an architect's role rather than anything in depth or specific.

---

### (Reading) - [Fundamentals of Software Architecture](https://www.oreilly.com/library/view/fundamentals-of-software/9781492043447/)

---

### (Doing) - [pg-analyze](https://github.com/andreamazza89/pg-analyze) (a utility to help troubleshoot slow SQL queries)
Super-rudimentary, but it does work, and I hope I will be using this at some point in the future. I enjoyed putting this
together though the os-specific parts were tricky (e.g. creating a directory) and not at all well solved (issuing shell
commands as a string).

#### What I learned
Had fun playing with ReaderT/Colonnade.
Making software that interacts with the operating system is hard (or at least feels that way).
Distributing software to be used on a specific platform is hard.

---

### (Course) - [Docker exercises](https://github.com/andreamazza89/docker-exercises)
This course touches on the basics of Docker and then moves on to work with compose and introduce advanced concepts like
image optimisations, multi-step builds and a hint at orchestration platforms.

#### What I learned
Strengthened my docker basics and got comfortable with docker-compose. The current project I work on is fully
docker-based, including the local development setup.

---

### (Reading/Doing) - [SQL Performance Explained](https://sql-performance-explained.com/)
Great book. The author got the balance right between explaining the details in enough depth and staying practical/actionable.
He provides a model for thinking about balanced-tree indexes and builds on that, touching on a variety of topics.

#### What I learned
I will make sure to think about indexing every time I consider writing new queries or editing existing ones.
The book should have given me enough intuition and ammunition on the topic that I should be able to set the right example
in future projects and troubleshoot existing slow queries.

The book is also well organised and to the point, so I can use it as a reference in the future.

---

### (Reading) - [Data and Reality](https://wiki.c2.com/?DataAndReality)
A nice book. Not at all practical; the overarching topic is that of showing/reflecting on the fact that
reality is almost impossible to model/record/grasp as data. So in order to have a workable system we must
understand that we are creating a limited model of reality so we can record it as data, where _context_ plays a fundamental role.

Topics include
- a description of how to describe 'one' thing in a system, as well as how to tell two things apart (sameness).
- naming, with scopes and qualifiers
- relationship. One thing I liked here is that he says he likes to describe them as the business 'reason' for the
relationship between two pieces of data.
- attributes - often we can describe the same thing as either an attribute or relationship, but we have a 'feel' for
which one applies where. One way to think of this is - what's the difference between a colum and a relationship?

#### What I learned
Not a ton. I do like the perspective of being more aware that reality is extremely complex/contradictory, but to
produce something useful we build a model/map of it, which narrows down the scope and makes it manageable by a computer.
To achieve this, context is very important, i.e. being deliberate about what the domain is.

I also like the idea of appreciating that technology poses limitations on how we can describe/process reality, so we must
start from the abstract model of the business needs/processes and then selecting the right technology + adapting to it.
