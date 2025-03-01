# lolfm

I hate this, but it works.

Listens to cmus status and populates a sqlite database with playback data, like last.fm does, but with extra fields and no restrictions.

Intended for people who know how to script/pipe/etc and query SQL. No support offered.

[Demo here](http://dalrym.pl/lolfm.html)

## New in 2025

lolfm has undergone the dreaded Rust rewrite. It has done away with a clunky client:server FIFO approach and now runs as a solitary command. If you prefer the older Scheme approach, please look further back in git history.

Note that older lolfm databases are not quite compatible with new ones, but Rust tool makes migration simple.

## Installation

Requires sqlite3, Rust, and cargo. Some steps might have to be root.

```
make
make install
make uninstall
```

## Setup

1. Open `cmus` to make your library available
2. Run `lolfm init your/db/path.sql` to populate it with all your cmus info
3. Edit `lolfm-event.sh` to point to `your/db/path.sql`
4. `chmod +x lolfm-event.sh`
5. `sudo cp lolfm-event.sh /usr/local/bin/lolfm-event` (or wherever)
6. In `cmus`, do `:set status_display_program=/usr/local/bin/lolfm-event`
7. If you have an existing lolfm database `old.db`, do:

```
lolfm dump -e plays old.db | lolfm read plays your/db/path.sql
lolfm dump -e loved old.db | lolfm read loved your/db/path.sql
```

This could take some time.

## Usage

If setup correctly, lolfm will silently write events to your database every time cmus starts, pauses, plays, or changes songs. These events are intelligently parsed to record played songs whenever a song changes, or the playback is stopped.

The user is generally expected to read/interact with the database directly through `sqlite3`, but some convenience commands are provided.

| command | parameters | description |
|---------|------------|-------------|
| dump    | [-e] plays/loved/songs <db-name> | Dumps the `plays`, `loved`, and `songs` tables of `<db-name>` to stdout in tab separated format. If the `-e` option is provided, the dump is in cmus event format instead, which is ingestible by other instances of `lolfm`. |
| read    | plays/loved <db-name> | reads stdin to the `plays` or `loved` table of `<db-name>`, provided it's in cmus event format. |
| love    | <song-id> <db-name> | adds `<song-id>` (column `songs.id` in the database) to the `loved` table of `<db-name>`. |
| unlove  | <song-id> <db-name> | removes `<song-id>` from the loved table of `<db-name>`. |
| init    | <db-name>           | starts a new database `<db-name>` with all the tags found in a running instance of `cmus`. Can be re-run to update library info. |

Use `grep`, `cut`, etc to get these commands to do what you want. Anything more complicated calls for SQL itself.

### Example: loving a track

Search for your favorite track in the songs dump. `grep` should be enough for visual confirmation.

```
$ lolfm dump songs /tmp/db.sql | grep Honeychain
21108   Throwing Muses  Honeychain
```

Love it based on the first column id. No quote escaping needed.

```
$ lolfm love 21108 /tmp/db.sql
$ lolfm dump loved | tail -n 5
26389	ゼルダ	灰色少年
13380	New Edition	That's the Way We're Livin'
13384	New Edition	Crucial
13386	New Edition	Superlady
21108	Throwing Muses	Honeychain
```

You can unlove it with `lolfm unlove`.

### cmus event format

`lolfm` uses `cmus-remote -Q` under the hood, which spits out events like

```
status paused
file /music/Throwing Muses/The Real Ramona/10 Honeychain.mp3
duration 264
position 1
tag artist Throwing Muses
tag album The Real Ramona
tag title Honeychain
tag date 1991
tag genre Alternative
tag discnumber 01
tag tracknumber 10
tag albumartist Throwing Muses
```

It parses these events into db rows. Running `lolfm dump -e` will also output tweaked versions of this format for db migrations. You are generally advised to treat this as a black box, but feel free to hack on it. If you have another instance of `cmus` running on a different computer, its `status_display_program` could be a shell script that just logs `cmus-remote -Q` with one extra line of `timemilliseconds <time-in-milliseconds>`. You can `cat` this dumb log of events back into your main db at any time with `lolfm read plays`.

### Future events

You can feed `lolfm` timestamped events that haven't transpired yet. For example, if you've just put on a 50 minute vinyl record at time T, you can have a script that generates play events for each track T+n seconds out into the future. `lolfm` will ingest them immediately, but won't record the songs until T+n has actually arrived. The program can be invoked again at T+n to write these plays.

I hope to include small scripts for generating play events for Discogs/Musicbrainz/etc albums sometime.

## Caveats

This logs plays under 30 seconds, unlike last.fm. Good news for grindcore fans, bad news if you're skipping through your queue and don't want those plays registered. The user is expected to handle this on the analysis side of things, writing custom queries to ignore deltas on less than n seconds.

I don't know if hammering the db with events will break anything. I think it's supposed to be serial, but don't do anything stupid.

Performance is quite nice in most cases, but migrating is slower than it should be. I don't know if this is because of rollbacks. I am more interested in data integrity than speeding this up, but do let me know if I've missed something obvious.

## Importing historical last.fm data

I have some writeups and helper scripts in the `conversion-notes` folder. This was an excruciating process and your experience will be different. No support is offered for this.

## Analysis

Write whatever SQL scripts you'd like and make cron jobs for them. See the `db` folder for examples. The `html-report.scm` script also demonstrates how they might be incorporated into a static html template.
