# Phocus

This is a small utility that I can use from the command-line to keep track of
where in the bigger picture my current efforts fit.

## Problem
Too often I find myself completing some small and intricate part of my
hobby/personal project, and realise that I don't remember where I stopped last
in the project, which I last looked at as an overview two weeks ago.

I deep-dive into something tiny and complex that takes all of my attention,
and once that is done and integrated, I'm not too sure what I had planned
next.

## Solution
Storing the tasks as a tree, with each task having a single parent and zero or
more child tasks lets me determine at a glance where I am in the project and
where I will be continuing from next once the current small task is finished.

I'll put in a concrete example once the code is finished.

## Mental Model
For now, MVP is a gui command that loads and interrogates a local DB. To make
it easier for **me** to use, I'm making it all command-line based. I
especially like how git branches show up in my prompt (PS1) so that's an
important feature I need in the MVP.

At any given point in time, the user will logically be working on a single
node in the tree. Creating a new node creates a child node of the current
working node. Nodes can be switched to at will using IDs/titles, but to remain
focused, only `push` and `pop` should be used (see below).

Basically, the help page of phocus needs to look like this:

```sh
phocus [options] <command> [options]

[options] must be one of
   --dbfile=<filename>     Specify the location of the DB file to use.
                           Defaults to $HOME/.phocusdb

<command> must be one of
   title             Prints the title and ID of the current node. Intended to
                  be used for $PS1 prompt.

   info [ID]         Prints all the information on the current node, including
                  the title of the parent node and the titles of all child
                  nodes. If an ID is specified, information for the the node
                  identified by ID will be printed.

   ancestry          Prints the entire ancestry of the current node (titles and
                  IDs only).

   push <title>      Creates a new child of the current node using the specified
                  title, and switches to it.

   pop               Deletes the current node and switches to the direct parent.
                  Note that deletion may not actually remove the node, just
                  mark it as deleted.

   switch <ID>       Switch to the node specified by ID. Any node can be switched
                  to, even one that is not an ancestor of the current node.

   return            Return to whatever the previous node was, whether it was
                  changed via a 'push', 'pop' or 'switch'. Phocus stores all
                  previous nodes as history, so the user can instantly return
                  to the node they were working with.

   visual            Print a visual representation of the tree (might not be
                  text-based for MVP) with the current node highlighted.
```

## Implementation
At some point I'd like to write a GUI for this, and the best way to future
proof for that eventuality is to make all of `phocus` a C library. This means
that if I find it useful I can eventualyl make an Android App for it.

To ensure this, all of `phocus` will be implemented as a C library with only a
thin program around it for the command-line application.

