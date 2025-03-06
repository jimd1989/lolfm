# Helper scripts

Some shell scripts that can (ab)use lolfm's features to go beyond cmus scrobbling. Just `chmod +x` them and put them in `/usr/local/bin` or wherever.

## cmus event format

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

It parses these events into db rows, using `status` as its delimiter. If you wish to mock these events, you should also begin each one with `status playing|paused|stopped`. Songs are written with subsequent events of `playing|paused` and `stopped` are passed through, or if one `playing|paused` event is followed by another `playing|paused` event with different song info.

Running `lolfm dump -e` will also output tweaked versions of this format for db migrations. 

If you think about it, you aren't really wedded to cmus at all for most of lolfm's facilities: only `init` and `event`. You can always use `lolfm read plays` on stdin to ingest arbitrary text in this format.

The program uses some custom rows too.

- `love`: A delimiter representing a loved song. For importing loved tracks.
- `time n`: A Unix timestamp in seconds. For importing loved tracks.
- `timemilliseconds n`: A Unix timestamp in milliseconds. For recording play events.

Feel free to hack on it. If you have another instance of `cmus` running on a different computer, its `status_display_program` could be a shell script that just logs `cmus-remote -Q` with one extra line of `timemilliseconds <time-in-milliseconds>`. You can `cat` this dumb log of events back into your main db at a later date with `lolfm read plays`.

Just be aware of overlapping timestamps causing problems.

## Example: manually recording plays for an existing album

If you like CDs or vinyl, you can `lolfm dump` to record another round of plays for an album that's already in your db. I spun _Steve McQueen_ two days back, but I'll record it now.

You can isolate the tracks of an album with `grep` and `awk`. `lolfm-album-search` does this.

```sh
#!/bin/sh
lolfm dump plays "PATH TO YOUR DB" | grep "$1" | cut -d "	" -f 2- | awk '!seen[$0]++'
```

(You can use a literal tab as a `cut` delimiter by doing ctrl-v + tab. Dunno how common that knowledge is.)

```
$ lolfm-album-search "Steve McQueen"
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

1. You've filtered accordingly. Here "Steve McQueen" is all I need, but this might have to be a fancier. I find that further piping into `grep` and `grep -v` gets everything eventually.
2. Your plays are in the correct track order. This is determined based on when they were first played. You might want to write this to a text file and move around if not.

This output can go into another `awk` that breaks one line into cmus event format. This is `lolfm-to-events`. Its sole argument is a `YYYY-MM-DD HH:MM` timestamp, which it will count forward from using the `duration` column.

```sh
#!/bin/sh
awk -F '\t' -v sum="$(date -j -f "%Y-%m-%d %H:%M" "$1" +%s)" '{sum += $7; print "status playing\ntag artist " $1 "\ntag title " $2 "\ntag album " $3 "\ntag date " $4 "\ntag albumartist " $5 "\ntag genre " $6 "\nduration " $7 "\ntimemilliseconds " (sum - 1) * 1000 "\nstatus stopped\ntimemilliseconds " sum * 1000}'
```

This can be read back into the same db with `lolfm read`. The full command is as follows.

```
$ lolfm-album-search "Steve McQueen" | lolfm-to-event "2025-03-03 15:00" | lolfm read plays /path/to/my.db
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

### Example: logging a new album

If you think about it, only the tracklist and runtimes of an album are tedious to record. Everything else is a constant value. `discogs-search` calculates these from a Discogs release (not master!) page.

```sh
#!/bin/sh
id="$(echo "$1" | cut -d "/" -f 5 | cut -d "-" -f 1)"
curl https://api.discogs.com/releases/$id --user-agent "jq test" | jq -r '.tracklist[] | select(.type_ == "track") | "\(.title)\t\((.duration | split(":") | .[0] | tonumber) * 60 + (.duration | split(":") | .[1] | tonumber))"'
```

This should work for most releases. Hour+ songs, V/A compilations, etc won't benefit. That's why it's a helper script, not a solution.

```
$ discogs-search https://www.discogs.com/release/11400-Joy-Division-Unknown-Pleasures
Disorder	216
Day Of The Lords	283
Candidate	180
Insight	264
New Dawn Fades	287
She's Lost Control	238
Shadowplay	230
Wilderness	155
Interzone	130
I Remember Nothing	360
```

This can be fed into another `awk` with the constants "artist", "album", "year", and "genre" as arguments. It wasn't worth it to mangle `jq` further to extract any of this. This is available as `discogs-extra-tags`.

```sh
#!/bin/sh
awk -F '\t' -v artist="$1" -v album="$2" -v year="$3" -v genre="$4" '{print artist "\t" $1 "\t" album "\t" year "\t" artist "\t" genre "\t" $2}'
```

Example.

```
$ discogs-search https://www.discogs.com/release/11400-Joy-Division-Unknown-Pleasures | discogs-extra-tags "Joy Division" "Unknown Pleasures" 1979 "Post Punk"
Joy Division	Disorder	Unknown Pleasures	1979	Joy Division	Post Punk	216
Joy Division	Day Of The Lords	Unknown Pleasures	1979	Joy Division	Post Punk	283
Joy Division	Candidate	Unknown Pleasures	1979	Joy Division	Post Punk	180
Joy Division	Insight	Unknown Pleasures	1979	Joy Division	Post Punk	264
Joy Division	New Dawn Fades	Unknown Pleasures	1979	Joy Division	Post Punk	287
Joy Division	She's Lost Control	Unknown Pleasures	1979	Joy Division	Post Punk	238
Joy Division	Shadowplay	Unknown Pleasures	1979	Joy Division	Post Punk	230
Joy Division	Wilderness	Unknown Pleasures	1979	Joy Division	Post Punk	155
Joy Division	Interzone	Unknown Pleasures	1979	Joy Division	Post Punk	130
Joy Division	I Remember Nothing	Unknown Pleasures	1979	Joy Division	Post Punk	360
```

You can see this is the same format as `lolfm dump` now. Which means it can be piped back into `lolfm-to-events` and `lolfm read`. I usually like to write this bit to a file so I can confirm it's all good.

## Future events

You can feed lolfm events that have yet to transpire. It will save them all, but won't write played songs until the system time has moved beyond the event timestamp. This is (maybe) useful if you want to record a vinyl album right when you put it on, rather than later. I can also see it being a source of headaches—use it at your discretion. Because lolfm is event driven, you'll have to feed it another event to actually write the events later. It won't automatically record them when the time has arrived.
