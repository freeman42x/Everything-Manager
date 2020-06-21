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

<!-- TODO algorithm -->

Note that many types of ToDo's have not been modeled yet. This includes core concerns regarding: parallelism, async, scheduling, habits, time, space, work context, etc.