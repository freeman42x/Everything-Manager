## Everything Manager

System used to optimize efficiency of achieving personal goals.

#### Features:

* information storage and organization
* tasks

#### General comments about the application

This system is initially based on very few simple rules and since any complex system which tries to be modeled using simpler rules that possible will be imperfectly modeled expect that any suggested rules should be broken as appropriate whenever it makes sense

We start with a very simple model and will enhance it as we get more user feedback so that the user experience is as good as possible. Since we are programming this using Haskell we will try to maintain as many invariants as possible both at compile time and at runtime.

#### How you can use it before it is implemented

You can use another time management application which allows you the flexibility of performing the algorithm described bellow.

We store our information in:

* Notes
* Queue
* Inbox

Notes are where you store any information on which you do not initially want to act upon. Example: list of anime you watched, nix commands, etc.

Queue is a fixed small size ordered list where you keep actionable items (ToDo's). Whenever you want to start working on something you will attempt to initiate work on the most important item on this list which will be first since the list is ordered. The size of the list is up to you - choose some manageable size so you do not get overwhelmed (10 items should be a pretty good number).

Inbox is where you add a new ToDo or Note if you can not add it to the Queue or Notes. You might not be able to add a ToDo to the queue because the priority of the ToDo is lower than any item in the Queue and the Queue is full. You might also not be able to add a ToDo or a Note if you are just in a situation in which you do not have time to prioritize the new item or categorize the Note.

> Note that many types of ToDo's have not been modeled yet. This includes core concerns regarding: parallelism, async, scheduling, habits, time, space, work context, etc.

#### Software for running the algorithm mentally

Currently I use [Freeplane mind mapping software](https://www.freeplane.org/wiki/index.php/Home) for this and the system works pretty decently as long as you are somewhat disciplined. Currently my system involves more than just the 3 categories mentioned above but you can start with that and add as needed.

If you also want to give it a try using Freeplane, download it and then create a new mindmap with 3 child nodes: Inbox, Queue and Notes.

#### The algorithm

Everything starts empty: Inbox, Queue and Notes.

When adding a new item classify it as ToDo or Note depending on your intention to do it or not at the moment.

When adding a ToDo:

* if its priority is higher than items in the queue then add it to the Queue at the right position. If this causes the queue to grow to 11 items then move the least important one to the Inbox
* if you can't prioritize it for any reason like having no time then add it to the Inbox

When adding a Note:

* if you can categorize it then add it to the Notes in the appropriate location
* if you can't categorize it for any reason then add it to the Inbox

When intending to starting work on a ToDo:

* if the number of items in the Queue is less than 10 then prioritize items from Inbox until the Queue is of size 10 or you run out of Inbox items
* iterate through all items in the Queue and perform any smart actions regarding them as required, this can involve: reprioritizing them since reality changed, moving them to Inbox or Notes, adding new items anywhere, etc.
* the process described above should be timeboxed as to not take too much time
* after you finished the grooming process you can begin work on the ToDo which has the highest priority