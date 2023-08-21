# lolfm

I hate this, but it works.

Listens to cmus status and populates a sqlite database with playback data, like last.fm does, but with extra fields and no restrictions.

Intended for people who know how to script/pipe/etc and query SQL. No support offered.

[Demo here](http://dalrym.pl/lolfm.html)

## Setup 

Requires Chicken Scheme version 5 and the srfi-1 egg.

```
brew install chicken
sudo chicken-install srfi-1
```

Might want to do `brew pin chicken` because any updates will delete all your Chicken eggs and this will silently stop working.

Set cmus handler to executable with

```
chmod +x /path/to/src/record-event.scm
```

Then in cmus' `:` menu, enter

```
set status_display_program=/path/to/src/record-event.scm
```

Set the listener to executable with

```
chmod +x /path/to/src/source-events.scm
```

Run it with `&` to put it in the background, but you might want to see if it's spitting out any errors.

It will set up a `lolfm.db` in your default cmus config directory. Change the code itself if you want something else. You will get errors about existing tables on subsequent runs; this is fine. It should write the tracks you play in cmus to this db. Manually verify with the `sqlite3` command line tool.

You can use `ps` to manipulate the process by PID, but if you ever lose this, `killall csi` will destroy the Chicken interpreter running lolfm.

## Interaction

cmus will pipe its status to the `record-event.scm` script whenever the player's state changes (next song, pause, etc). These state changes are piped into `/tmp/lolfm-fifo`, which is listened to by `source-events.scm`. The latter script interprets these state changes and writes play events to `lolfm.db` when it is sure that a song has finished. It is possible to fuck this process up through deliberately malicious behavior, like hammering play/pause and flooding the queue with events, so treat it gently.

One can interact with the logger by echoing some commands to `/tmp/lolfm-fifo`. Improper input crashes for now.

- `(dump)`: Will print the current contents of the events queue to stdout. If starting lolfm in the background, pipe its stdout to somewhere readable from outside the current terminal session, or you may not see anything.
- `(clear)`: Clears the contents of the events queue. Useful if events are unsynced or you don't want to record a play that's on deck.
- `(love "Artist" "Title")`: Searches for a song by "Artist" with a title "Title" and adds it to the user's loved tracks if it exists. Case insensitive.

Example:

```
echo '(love "Throwing Muses" "Honeychain")' > /tmp/lolfm-fifo 
```

Note the quotes.

## Caveats

This logs plays under 30 seconds, unlike last.fm. Good news for grindcore fans, bad news if you're skipping through your queue and don't want those plays registered. The user is expected to handle this on the analysis side of things, writing custom queries to ignore deltas on less than n seconds.

Escaping quotes and special characters remains a pain in the ass.

On a fresh boot with an empty `/tmp` directory, running `cmus` before `source-events &` might write to `/tmp/lolfm-fifo` as a regular file before `source-events` has a chance to `mkfifo /tmp/lolfm-fifo`. This could lead to a deluge of non-draining events and hammer the DB with superfluous writes. Make sure to run `source-events` before opening `cmus` for the first time. You can always fix the DB by running a query like `DELETE FROM plays WHERE date > (unix timestamp of when problem occurred)`. 

## Importing

See the `conversion` folder for notes on how I imported my last.fm data. This was an excruciating process and your experience will be different. No support is offered for this.

## Analysis

Write whatever SQL scripts you'd like and make cron jobs for them. See the `queries` folder for examples. The `html-report.scm` script also demonstrates how they might be incorporated into a static html template.
