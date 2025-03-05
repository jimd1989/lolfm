# lolfm

I hate this, but it works.

Listens to cmus status and populates a sqlite database with playback data, like last.fm does, but with extra fields and no restrictions.

Intended for people who know how to script/pipe/etc and query SQL. No support offered.

[Demo here](http://dalrym.pl/lolfm.html)

## New in 2025

lolfm has undergone the dreaded Rust rewrite. It has done away with a clunky client:server FIFO approach and now runs as a solitary command. If you prefer the older Scheme code, please look further back in git history.

Note that older lolfm databases are not quite compatible with new ones, but Rust tool makes migration simple.

## Installation

Requires sqlite3, Rust, and cargo. Some steps might have to be root.

```
make
make install
make uninstall
```

## Setup

lolfm unfortunately requires a wrapper script around it to make cmus happy.

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
| dump    | [-e] plays/loved/songs db-name | Dumps the `plays`, `loved`, and `songs` tables of `db-name` to stdout in tab separated format. If the `-e` option is provided, the dump is in cmus event format instead, which is ingestible by other instances of `lolfm`. |
| read    | plays/loved db-name | reads stdin to the `plays` or `loved` table of `db-name`, provided it's in cmus event format. |
| love    | song-id db-name | adds `song-id` (column `songs.id` in the database) to the `loved` table of `db-name`. |
| unlove  | song-id db-name | removes `song-id` from the loved table of `db-name`. |
| init    | db-name           | starts a new database `db-name` with all the tags found in a running instance of `cmus`. Can be re-run to update library info. |

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
$ lolfm dump loved /tmp/db.sql | tail -n 5
26389	ゼルダ	灰色少年
13380	New Edition	That's the Way We're Livin'
13384	New Edition	Crucial
13386	New Edition	Superlady
21108	Throwing Muses	Honeychain
```

You can unlove it with `lolfm unlove`.

### cmus event format

lolfm uses `cmus-remote -Q` under the hood, which spits out events like

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

### Example: manually recording plays for an existing album

If you like CDs or vinyl, you can (ab)use `lolfm dump` to record another round of plays for an album that's already in your db. Just run a command like this every time you drop the needle. You can even record playing sessions from many days ago. I listened to _Steve McQueen_ on vinyl two days back, but I'll put it in my db.

You can isolate the tracks of an album with `grep` and `awk`.

(You can use a literal tab as a `cut` delimiter by doing ctrl-v + tab. Dunno how common that knowledge is.)

```
$ lolfm dump plays ~/.config/cmus/lolfm.db | grep "Steve McQueen" | cut -d "    " -f 2- | awk '!seen[$0]++'
Prefab Sprout   Faron Young     Steve McQueen   1985    Prefab Sprout   Pop     230
Prefab Sprout   Bonny   Steve McQueen   1985    Prefab Sprout   Pop     225
Prefab Sprout   Appetite        Steve McQueen   1985    Prefab Sprout   Pop     236
Prefab Sprout   When Love Breaks Down   Steve McQueen   1985    Prefab Sprout   Pop     248
Prefab Sprout   Goodbye Lucille #1      Steve McQueen   1985    Prefab Sprout   Pop     271
Prefab Sprout   Hallelujah      Steve McQueen   1985    Prefab Sprout   Pop     261
Prefab Sprout   Moving the River        Steve McQueen   1985    Prefab Sprout   Pop     238
Prefab Sprout   Horsin' Around  Steve McQueen   1985    Prefab Sprout   Pop     280
Prefab Sprout   Desire As       Steve McQueen   1985    Prefab Sprout   Pop     320
Prefab Sprout   Blueberry Pies  Steve McQueen   1985    Prefab Sprout   Pop     144
Prefab Sprout   When the Angels Steve McQueen   1985    Prefab Sprout   Pop     269
```

Two big assumptions here:

1. You've filtered accordingly. Here "Steve McQueen" is all I need, but this might have to be a fancier regex or something.
2. Your plays are in the correct track order. Might want to write this to a text file and move around if not.

This input can go into another `awk`, with an arbitrary date as the starting `sum` timestamp. You can record past/future events like this. It should generate timestamped events using the `duration` column.

```
awk -F '\t' -v sum="$(date -j -f "%Y-%m-%d %H:%M" "2025-03-03 19:00" +%s)" '{sum += $7; print "status playing\ntag artist " $1 "\ntag title " $2 "\ntag album " $3 "\ntag date " $4 "\ntag albumartist " $5 "\ntag genre " $6 "\nduration " $7 "\ntimemilliseconds " (sum - 1) * 1000 "\nstatus stopped\ntimemilliseconds " sum * 1000}'
```

This can be read back into the same db with `lolfm read`. The full command is as follows.

```
$ lolfm dump plays ~/.config/cmus/lolfm.db | grep "Steve McQueen" | cut -d "    " -f 2- | awk '!seen[$0]++' | awk -F '\t' -v sum="$(date -j -f "%Y-%m-%d %H:%M" "2025-03-03 19:00" +%s)" '{sum += $7; print "status playing\ntag artist " $1 "\ntag title " $2 "\ntag album " $3 "\ntag date " $4 "\ntag albumartist " $5 "\ntag genre " $6 "\nduration " $7 "\ntimemilliseconds " (sum - 1) * 1000 "\nstatus stopped\ntimemilliseconds " sum * 1000}' | lolfm read plays ~/.config/cmus/lolfm.db
$ lolfm dump plays ~/.config/cmus/lolfm.db | tail -n 40
1740816327      Throwing Muses  Honeychain      The Real Ramona 1991    Throwing Muses  Alternative     264
1740816651      Wang Chung      Dance Hall Days Points on the Curve     1983    Wang Chung      Synth Pop       238
1740817274      Varg    Snake City / Maserati Music     Nordic Flora Series Pt.3 (Gore-Tex City)        2017    Varg    Techno  328
1740989043      Prefab Sprout   Faron Young     Steve McQueen   1985    Prefab Sprout   Pop     230
1740989268      Prefab Sprout   Bonny   Steve McQueen   1985    Prefab Sprout   Pop     225
1740989504      Prefab Sprout   Appetite        Steve McQueen   1985    Prefab Sprout   Pop     236
1740989752      Prefab Sprout   When Love Breaks Down   Steve McQueen   1985    Prefab Sprout   Pop     248
1740990023      Prefab Sprout   Goodbye Lucille #1      Steve McQueen   1985    Prefab Sprout   Pop     271
1740990284      Prefab Sprout   Hallelujah      Steve McQueen   1985    Prefab Sprout   Pop     261
1740990522      Prefab Sprout   Moving the River        Steve McQueen   1985    Prefab Sprout   Pop     238
1740990802      Prefab Sprout   Horsin' Around  Steve McQueen   1985    Prefab Sprout   Pop     280
1740991122      Prefab Sprout   Desire As       Steve McQueen   1985    Prefab Sprout   Pop     320
1740991266      Prefab Sprout   Blueberry Pies  Steve McQueen   1985    Prefab Sprout   Pop     144
1740991535      Prefab Sprout   When the Angels Steve McQueen   1985    Prefab Sprout   Pop     269
1741040177      Big City Orchestra      Quiet Kids Noize Please A Child's Garden of Noise       1994    Big City Orchestra      Experimental    99
1741040345      Big City Orchestra      Noize All Day   A Child's Garden of Noise       1994    Big City Orchestra      Experimental    160
1741040475      Big City Orchestra      Popub's Party   A Child's Garden of Noise       1994    Big City Orchestra      Experimental    129
1741040584      Big City Orchestra      Go      A Child's Garden of Noise       1994    Big City Orchestra      Experimental    108
1741041040      Big City Orchestra      The Pirate Song A Child's Garden of Noise       1994    Big City Orchestra      Experimental    456
1741046498      Yoran   Concert Maillol Montparnasse    1981    Yoran   Experimental    252
1741046655      Yoran   Je Derve Avec Láir      Montparnasse    1981    Yoran   Experimental    158
1741048049      Yoran   Montparnasse    Montparnasse    1981    Yoran   Experimental    260
1741048082      Yoran   La Bou.....     Montparnasse    1981    Yoran   Experimental    35
1741048329      Bourbonese Qualk        Shutdown        The Spike       1985    Bourbonese Qualk        Industrial      248
1741048517      Bourbonese Qualk        Suburb City     The Spike       1985    Bourbonese Qualk        Industrial      190
1741048614      Bourbonese Qualk        About This      The Spike       1985    Bourbonese Qualk        Industrial      97
1741048807      Bourbonese Qualk        New England     The Spike       1985    Bourbonese Qualk        Industrial      195
1741048951      Bourbonese Qualk        Preparing for Power     The Spike       1985    Bourbonese Qualk        Industrial      145
1741049109      Bourbonese Qualk        Pogrom  The Spike       1985    Bourbonese Qualk        Industrial      159
1741055771      Bourbonese Qualk        Call to Arms    The Spike       1985    Bourbonese Qualk        Industrial      283
1741055889      Bourbonese Qualk        Frontline       The Spike       1985    Bourbonese Qualk        Industrial      119
1741056088      Bourbonese Qualk        Spanner in the Works    The Spike       1985    Bourbonese Qualk        Industrial      200
1741056317      Bourbonese Qualk        In-flux The Spike       1985    Bourbonese Qualk        Industrial      230
1741056524      Bourbonese Qualk        Deadbeat        The Spike       1985    Bourbonese Qualk        Industrial      208
1741056965      Robert Turman   Way Down        Way Down        1987    Robert Turman   Minimal Synth   441
1741057296      Robert Turman   Lotek   Way Down        1987    Robert Turman   Minimal Synth   330
1741057899      Robert Turman   Mind the Gap    Way Down        1987    Robert Turman   Minimal Synth   602
1741058249      Robert Turman   Freedom from Fear       Way Down        1987    Robert Turman   Minimal Synth   349
1741058745      Robert Turman   Deadkingspeak   Way Down        1987    Robert Turman   Minimal Synth   496
1741148274      Robert Turman   Clean Living    Way Down        1987    Robert Turman   Minimal Synth   597
```

The vinyl play on March 3rd was successfully recorded into the past, even though other digital plays have taken place afterwards.

### Future events

You can feed `lolfm` timestamped events that haven't transpired yet. For example, if you've just put on a 50 minute vinyl record at time T, you can have a script that generates play events for each track T+n seconds out into the future. `lolfm` will ingest them immediately, but won't record the songs until T+n has actually arrived. The program can be invoked again at T+n to write these plays.

## Caveats

This logs plays under 30 seconds, unlike last.fm. Good news for grindcore fans, bad news if you're skipping through your queue and don't want those plays registered. The user is expected to handle this on the analysis side of things, writing custom queries to ignore deltas on less than n seconds.

I don't know if hammering the db with events will break anything. I think it's supposed to be serial, but don't do anything stupid.

Performance is quite nice in most cases, but migrating is slower than it should be. I don't know if this is because of rollbacks. I am more interested in data integrity than speeding this up, but do let me know if I've missed something obvious.

## Importing historical last.fm data

I have some writeups and helper scripts in the `conversion-notes` folder. This was an excruciating process and your experience will be different. No support is offered for this.

## Analysis

Write whatever SQL scripts you'd like and make cron jobs for them. See the `db` folder for examples. The `html-report.scm` script also demonstrates how they might be incorporated into a static html template.
