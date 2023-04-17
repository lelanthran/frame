# FRAME

This is a small utility that I can use from the command-line to keep
track of where in the bigger picture my current efforts fit.

## Problem

Too often I find myself completing some small and intricate part of my
hobby/personal project, and realise that I don't remember where I stopped
last in the project, which I last looked at as an overview two weeks ago.

I deep-dive into something tiny and complex that takes all of my
attention, and once that is done and integrated, I'm not too sure what
I had planned next.

## Solution

I store context as a tree of nodes. I call each node in this tree a `frame`
(as in *frame of context*, *frame of mind*, or *framing the problem*, etc). At any
given time only a single frame can be active. To see all the notes I made
while working during that frame of context I use `frame status`.

When I start a task I first do `push frame 'title of task name here'` and
enter a short name, and that new frame is the current frame. When I am done I
do `frame pop` and the current frame is then whatever frame I was in prior to
the `push`.

## Mental Model

For now, MVP is a cli command that loads and interrogates a local DB. To
make it easier for **me** to use, I'm making it all command-line based. I
especially like how git branches show up in my prompt (PS1) so that's
an important feature I need in the MVP.

At any given point in time, the user will be in a particular frame of
mind. This will correspond to a single node in the tree. Any time they issue a
terminal command the PS1 prompt will ensure that the new frame is displayed.


## Implementation

At some point I'd like to write a GUI for this, and the best way to future
proof for that eventuality is to make all of `frame` a C library. This
means that if I find it useful I can eventually make an Android App
for it.

To ensure this, all of `frame` will be implemented as a C library with
only a thin program around it for the command-line application.

