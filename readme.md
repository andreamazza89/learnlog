# Learn Log

This repo is to track what I am learning and what I have learned in the past.
The main reason for this is to keep me focused on what I am doing at one time and
also track how much time I spend doing what.

## Structure

There are just two files: this readme and `log.json`.
The readme contains a high-level overview of what I am currently learning/reading/doing.
The log consists of a list of entries, each entry including the what/when and a brief note.

I lied, there is also a subproject, under `./visualiser`, which has code to view/add entries to the log.

---

## Current

### (Doing) - [jq-exercises](https://github.com/andreamazza89/jq-exercises)

### (Course) - [Introduction to Discrete Mathematics for Computer Science](https://www.coursera.org/specializations/discrete-mathematics?)

---

## Previous

### (Online course) - [Advanced Functional Programming with Haskell](http://www.cs.nott.ac.uk/~pszgmh/afp.html)
Nice lectures on Haskell topics. Unfortunately, this course used to be publicly available but then got pulled,
so I had to stop before things got interesting.

#### What I learned
Not a lot more than I already knew, as the course was removed before I even got into Monads, but I enjoyed
working on the connect4 exercise

---

### (Reading) - [Monolith to Microservices](https://samnewman.io/books/monolith-to-microservices/)
Really enjoyed this book. It's very practical and forms a good foundation for:

- Making sure you understand the tradeoffs of Microservices and having a goal in mind. It provides
  plenty of reasons to avoid going down the microservices route.
- Plenty of practical/actionable advice if you do decide to migrate a Monolith into Microservices.

#### What I learned
I think the most important point to take away from this is to make sure you have a goal that Microservices
facilitate and you've considered alternative (and probably simpler) options. If 'doing Microservices' is the
_what_ rather than the _how_, then you're probably going to be in trouble.

I really enjoyed the stress on Domain-driven design, as that resonates with me. The general idea is that it
makes sense to 'group code' by Buiseness domain rather than technology. In other words, rather than having things
like `database`, `UI`, you should have `Orders`, `Tasks`, etc.. The fundamental idea is that we want to optimize for making future changes to our system; because changes normally revolve around a domain (e.g. `Orders`) rather than a technology (e.g. `database`), if we organise our systems by domain, it will be easier to make and reason about future changes. This concept very much applies to drawing boundaries for microservices but also applies to less distributed systems.

Speaking of boundaries, I think the book's advice is that in most cases it might be better to start with a
Monolith, and once this has evolved enough that we can easily see the boundaries, then it's safer to extract
services from it. One could argue that splitting a tangled monolith is worse compared to starting with Microservices from scratch. However, if you don't get the boundaries right, adjusting will be very hard and expensive in a distributed system; not to mention that the upfront cost of getting something up and running is way more complex/expensive. From chapter 2:

> In many ways, having an existing codebase you want to decompose into microservices is much easier than
> trying to go to microservices from the beginning.

The book also covers the non-technical aspects of a migration, including the need for a Guiding Coalition, haing a Vision/Strategy and potentially needing to reorganise/upskill teams. It's very important to have ways to periodically check whether the Transition is working or not.

Another concept stressed throughout the book is that of keeping any change small and incremental.
This allows fast feedback and creates a smaller surface for errors.

Finally, the more technical sections on patterns for extracting code/data, as well as the one on
Growing pains are very down to earth and I will refer back to them if/when I am part of a Monolith extraction.

---

### (Reading/Doing) - [Haskell in Depth](https://www.manning.com/books/haskell-in-depth)
Overall quite a nice book, with the codebase that comes with it being a great source of examples.
I did not read the full extent both because some chapters are very advanced (e.g. type-level programming) and
because I'm jumping on the monolith to microservices book-club train.

I might come back to cover concepts ad-hoc in the future.

#### What I learned
Very good exposure to a number of topics in Haskell. Definitely increased my confidence/intuition in more advanced topics.

---

### (Doing) - Terminal User interface to visualise and manage this repository (i.e. learnlog)
Got something up and running, it's far from fancy, but does let me add new entries by copying existing ones.

#### What I learned
Some familiarity with the Brick library (for terminal user interfaces), I also spent some time trying to understand some
of the library (specifically the Form module).

---

### (Reading) - [Fundamentals of Software Architecture](https://www.oreilly.com/library/view/fundamentals-of-software/9781492043447/)

Nice book, fairly concrete, giving an overview of architecture types and an idea of the nuance that goes into making
decisions. Also touches on managing complexity, breaking up systems and soft skills.

#### What I learned
Better knowledge of a number of architecture styles and their pros/cons. Architecture characteristics, modularity.
A general overview of the aspects involved within an architect's role rather than anything in depth or specific.


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
