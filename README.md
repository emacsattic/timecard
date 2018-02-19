# An Emacs mode for keeping track of where the time goes

A timecard is an ordinary text buffer with time frobs in it.  It
might look like this:

    [2:25:05] public patch review
    [1:12:29] meetings
    [3:55:58] separate debug info patch

Each frob indicates the amount of time the user has spent on a given
task, in hours, minutes, and seconds.  A buffer may have one frob that
is 'active', meaning that the user is currently doing that task.  The
currently active frob, if any, is shown in red.

The command `C-c C-c` makes the last frob at or before point in the current
buffer as active; if that frob is already active, then it deactivates it, and no
frob is active. Deactivating a frob updates its total time.

You can also include estimates, in the form `(HH:MM)` or `(:MM)`.
If you like, you can mark an estimate 'done', like this: `(03:00 done)`.

The command `C-c C-t` displays totals for frobs and estimates in the current
region; the command `C-c C-p` does the same for the current page. Estimates
marked 'done' are totalled up separately. (More generally, estimates are grouped
by whatever appears in the parenthesis after the time.)

Here is the complete list of key bindings for this mode:

|key           |binding                    |
|--------------|---------------------------|
|C-c           |Prefix Command             |
|<M-tab>       |timecard-complete-task-name|
|C-c C-c       |timecard-activate          |
|C-c C-h       |timecard-total-headline    |
|C-c TAB       |timecard-insert-frob       |
|C-c C-p       |timecard-total-page        |
|C-c C-t       |timecard-total-region      |
