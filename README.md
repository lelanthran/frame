# FRAME

Estimated time to read this document: 5m

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

All frames descend from an initial frame called `root`[^1]
## Setup for terminal usage
The first thing to do is to integrate the frame name into the shell's
`PS1` variable. The command `frame current` prints the name of the current
frame and the date it was changed, with the two fields separated by a colon.

```sh
$ frame current
root/projects/frame: Thu May 11 22:26:29 2023
```

I added the following function which is called within my `PS1` variable:

```sh
frame_ps1() {
   frame current --quiet | cut -f 1 -d :
}
```
Calling `frame_ps1` within the `PS1` variable prints out the current frame
after each command, in every terminal I am logged into[^2].

```sh
[root/projects/frame]$ echo "Hello World"
Hello World
[root/projects/frame]$ 
```

## Example Usage
Continuing with the frame `root/projects/frame`, in this case I
want to create a GUI browser for this project:

```sh
[root/projects/frame]$ frame push 'create GUI' --message="
Create a GUI for frame browsing
"
Frame 0.1.2, (© 2023 Lelanthran Manickum)
Created new frame [root/projects/frame/create GUI]
[root/projects/frame/create GUI]$ 
```

> Had I neglected to provide a `--message` parameter then $EDITOR is opened to
> allow me to enter a message, similar to how `git commit` works when no message
> is specified. Each frame can store a single note of unlimited size; the
> `--message` argument provides the notes when creating a new frame.

The current frame's name is `create GUI`. It has a parent, `projects`, which
itself has the parent `root`.

```mermaid
graph TD;
   root-->projects;
   projects-->frame;
   frame-->create GUI;
```
Some useful commands at this point:

1. `frame status` returns the notes (message) for this frame.
2. `frame edit` opens the editor to allow editing the notes for this frame.
was entered.

I have some idea of what needs to be done, namely, startup a new Lazarus
project, create the GUI elements, include the `libframe.so` library, ensure
that the build compiles, etc.

```sh
[root/projects/frame/create GUI]$ frame append --message="
1. Create new lazarus project
2. Add `libframe.so` to the lazarus project
3. Create GUI elements
"
Frame 0.1.2, (© 2023 Lelanthran Manickum)
root/frame/create GUI: Sat May 13 08:20:32 2023
```

It takes a few minutes to create the Lazarus project, add the `libframe.so`
file and ensure that the linker is finding and accepting the library.

My next task is to create the GUI elements. This is much more involved, and
so I create a new frame for it:

```sh
[root/projects/frame]$ frame push 'GUI Elements' --message="
Needs to display:
1. The history, with a filter
2. A treeview of all frames from root, with the current frame selected and
expanded.
3. An editor box that displays the notes for the current frame, and allows the
user to edit those notes.
"
Frame 0.1.2, (© 2023 Lelanthran Manickum)
Created new frame [root/frame/create GUI/GUI Elements]
[root/projects/frame/create GUI/GUI Elements]$ 
```

I do the form design (ridiculously easy in Lazarus, takes about 10m). I
attempt to populate the `history` GUI element, and realise I forgot to declare
the `C` functions in the library file. My Lazarus code cannot call undeclared
functions even if the library is linked correctly.

```sh
[root/projects/frame/create GUI/GUI Elements]$ frame push 'decls needed" --message='
Declare all the `C` functions in `libframe.so`.
'
Frame 0.1.2, (© 2023 Lelanthran Manickum)
Created new frame [root/frame/create GUI/GUI Elements/decls needed]
```

To make things easier, I paste the `C` header file into ChatGPT and ask for
the Lazarus definitions. It provides the definitions, helpfully hallucinating
a few types, and I copy the results into a Lazarus file and fix all the
compilation errors.

Finally, the needed declarations are in. To return to whatever I was
previously doing (what was it again):
```sh
[root/frame/create GUI/GUI Elements/decls needed] $ frame pop
Frame 0.1.2, (© 2023 Lelanthran Manickum)
[src/frm.c:55] Failed to switch to new dir [root/frame/create GUI/GUI Elements/decls needed]
[src/frm.c:1440] Warning: using relative path [root/frame/create GUI/GUI Elements/decls needed] failed, trying absolute path
Current frame
   root/frame/create GUI/GUI Elements

Notes (Sat May 13 08:26:59 2023)
   Needs to display:
   1. The history, with a filter
   2. A treeview of all frames from root, with the current frame selected and
   expanded.
   3. An editor box that displays the notes for the current frame, and allows the
   user to edit those notes.

```

Right, I was on the first item. Maybe create a frame for that:

```sh
[root/frame/create GUI/GUI Elements] $ frame push 'populate history element' --message='
> create a populate_history() procedure
> '
Frame 0.1.2, (© 2023 Lelanthran Manickum)
Created new frame [root/frame/create GUI/GUI Elements/populate history element]
```

I create the named function, run it, watch in disbelief as it crashes, and
then investigate the crash. Turns out there's a bug in the library. New frame
time:
```sh
[root/frame/create demo/create GUI/GUI Elements/populate history element] $ frame push 'debug frm_history' --message='
> frm_history, when called multiple times in the same session, has a double-free
> bug. This does not show up in the c/line `frame history` command because the `frame`
> program runs the specified command and then exits.
> '
Frame 0.1.2, (© 2023 Lelanthran Manickum)
Created new frame [root/frame/create demo/create GUI/GUI Elements/populate history element/debug frm_history]
```

I switch to working on the frame library instead. I update the test script to
reproduce the error, and using valgrind, and then some gdb, I come up with an
appropriate fix.

```mermaid
graph TD;
   root-->projects;
   projects-->frame;
   frame-->create GUI;
   create GUI-->populate history element;
   populate history element-->debug frm_history;
   debug frm_history-->update tests to reproduce bugs;
   update tests to reproduce bugs-->fix bug;
```

Now that the bug in frm_history has been fixed, I perform multiple `frame pop`s,
until I reach a frame that is still incomplete: `populate history element'.

After each `pop`, the current frame is **deleted**, never to be seen again.
This helps keep the signal/noise ratio to an acceptable number.

Once I am at `populate history element`, I relink and rerun the program. It
populates the history just fine. I continue my development in this manner;
`push`ing new frames when I start something even slightly long, `pop`ping
frames once I am done with whatever I am currently working on.

When I need to record more notes or thoughts related to the current frame, I
use `frame edit` and record my notes in the editor that opens.

When I come
back to my computer after (for example) a good nights sleep, my terminal will
display the name of the current frame. If that is not sufficient to remind me
where I stopped last, then `frame status` displays all my notes.

Sometimes I come back to a frame after multiple days; I create a frame, store
my notes, then 'push' new frames diving ever deeper into whatever it is I am
doing, and then two days later I have finally `pop`ped my way back to the
original frame, and thankfully I can use `frame status` to remember what it
was I wanted to do next.

It's all very linear, and intentionally so. You can create new frames
without immediately switching to the newly created frame by using `frame new`.
I do this when I know in advance that I need to do $X, $Y and $Z: then it's
simpler to just do `frame create $X`, etc.

## Not only linear
I create multiple frames off `root`; typically one for each project. The
`frame switch` command accepts a target frame, and attempts to switch to the
sub-branch of that frame that you were last working on.

For example, having frames

```mermaid
graph TD;
   root--frame;
   root->game;
   frame-->create GUI;
   create GUI-->populate history;
   populate history-->debug crash;
   frame-->write docs;
   game-->Scrabble Clone;
   Scrabble Clone-->implement basic user mgmt;
   implement basic user mgmt-->email verification on signup;
   Scrabble Clone-->create board;
   create board-->store game state;
```
My current frame is *[root/frame/create GUI/populate history]*. The last time I
was in a sub-branch of *[root/game]*, it was in the *[email verification on
signup] sub-branch.

When I do `frame switch root/game`, it returns me to the branch
*[root/game/Scrabble Clone/implement basic user mgmt/ email verification on
signup]*.

The next time I do `frame switch root/frame`, it returns me to
*[root/frame/create GUI/populate history]*.

## Command reference
You can get a list of commands using `frame --help`. The important commands
are

1. `push <title>` - push a new child frame with specified title.
2. `pop` - discard current frame and make parent current.
3. `list` - list all child frames of current frame.
4. `switch <path>` - (smart) switch to a new path.


[^1]: I made a conscious decision at the start of this project that `root` will
always be explicit and not implicit. This avoids errors where a typo results
in addressing root when the user did not mean to. For example the following
error is impossible when root is explicit: `rm -rf myhomedir/Downloads /lib`.
```

[^2]: In reality, my `PS1` variable is a lot more complex than this, as it
includes the git branch (if any) and other information (current directory,
username, etc). My prompt uses a separate line for the `frame` name, as this
name can get quite long.
