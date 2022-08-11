# lolfm

I hate this, but it works.

Listens to cmus status and populates a sqlite database with playback data, like last.fm does, but with extra fields and no restrictions.

Intended for people who know how to script/pipe/etc and query SQL. No support offered.

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

## Caveats

This is very, very fragile. You can fuck it up if you are doing deliberately malicious things like hammering pause/unpause. There are probably other ways to break it. You can always echo commands to `/tmp/lolfm-fifo` like `(clear)` to reset the event queue and `(dump)` to print its contents. Look at the source code for other commands. You can also see the most recent query to the db in `/tmp/lolfm-query`.

This logs plays under 30 seconds, unlike last.fm. Good news for grindcore fans, bad news if you're skipping through your queue and don't want those plays registered. The user is expected to handle this on the analysis side of things, writing custom queries to ignore deltas on less than n seconds.

Escaping quotes and special characters remains a pain in the ass.

## Importing

See the `conversion` folder for notes on how I imported my last.fm data. This was an excruciating process and your experience will be different. No support is offered for this.

## Analysis

Write whatever SQL scripts you'd like and make cron jobs for them. I might add some of my own here later.
