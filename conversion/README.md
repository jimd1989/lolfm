The last.fm API makes a user's play history available as a dump of XML data. I'm not interested in documenting the whole process here, but I will leave scripts and snippets of steps I took to get that data into the lol.fm database.

## Parsing last.fm dump

All play history was fetched with a last.fm scraping script, written in (outdated) Python. I forget which. Just search for it. I heavily edited it to stream raw XML data because I found the "fetch everything, then decode it" approach obnoxious when I had close to 100K plays.

I made sure all the strings were decoded as UTF-8. Double check this.

I used `xmlstarlet`, a tool like `jq`, to isolate the date, artist, title, and album fields respectively into a four-column TSV file. Tabs were chosen over commas since commas can and do appear in song titles. I am not sure if tabs do. The title field should probably actually be the final column for this reason. The file was called `tsv`.

I used the `to-unix-epoch.sh` script to convert the date strings into unix timestamps.

```
cat tsv | cut -d "    " -f 1 | xargs -I {} ~/prog/misc/lolfm/conversion/to-unix-epoch.sh "{}" > /tmp/timestamps
```

Might want to run it through `uniq` in the case of mistaken double logs. I found a few.

The resolution on the dates in the XML also isn't good enough for very short songs to be converted to their proper Unix timestamp. Looking at you The Gerogerigegege. You might have to manually edit stamps like these because the SQL manipulation of some of this data will rely on unique timestamps. In Vim, you can highlight entire blocks of duplicate timestamp rows and `:! fix-timestamp.scm` them using the included Scheme script. This manual approach is good for the (hopefully few and contiguous) rows in the log with lots of matching stamps. For simple pairs of matching stamps, it's probably saner to import just the timestamp column into K, cast it to a row of integers `xs`, and run the following before exporting and pasting it back into the log.

```
{$[x=y;x+1;x]}':xs

paste -d "    " /tmp/timestamps tsv > ~/.config/cmus/new-tsv
```


## Comparing dump against local tags

The dump is missing plenty of good information like genres, and song runtimes. This dump data can be matched up against local files, but I couldn't find a catch-all programmatic solution for this.

- I initially tried using `cmus-remote -C`, then passing in `filter` commands to navigate to the dump's artist - title - album in cmus, then using `format_print`, but escaping special characters in tag names was an absolute shitshow. `format_print` also doesn't show duration for some reason.
- Also considered running `id3v2` or `eyeD3` against every line of `lib.pl`, but `id3v2` _still_ doesn't support utf-8 and eyeD3's output format is stupid as fuck. It's also slow. 
- Eventually wound up with a mostly automated solution:
  - quit cmus to ensure the play queue is entirely clear
  - reopen it, and add the whole library to the queue
  - navigate to the queue panel and ensure the cursor is at the top of the list
  - running the following code will dump the tag information for the entire library—regardless of whether or not it corresponds with a line in the last.fm dump—to the file `dumped-tags`

```
wc -l < ~/.config/cmus/lib.pl | xargs seq | xargs -I {} ./dump-tags.sh > /tmp/dumped-tags
```

Each track is delimited with a silly `+===+` line, while the individual tags are newline buffered.

The next step is to parse this text file into a sqlite database of every possible song. Running

```
./conversion/to-tsv.scm dumped-tags > tracks.tsv
```

filters out all the irrelevant tags and returns a tsv of your entire library, ready to be loaded into the database.

The TSV is loaded into the `library_dump` table (run all migration scripts first) through the sqlite shell with the following commands

```
.mode ascii
.separator "\t" "\n"
.import tracks.tsv library_dump
```

Importing with the advised `.mode tabs` setting causes all sorts of issues with escaping quotes. This appears to import without issue, assuming all tags are present in every row of the TSV. Adjust your script if they aren't.

Do the same thing for the dumped last.fm tracks.

```
.import lastfm.tsv lastfm_dump
```

There is also a table available for libre.fm tracks, whose dump comes in a slightly different CSV format. Import that and then add its rows to the `lastfm_dump` table.

Once again, it is very important that every `date` timestamp be unique.

Run the first query in `insert-from-library-dump.sql` to perform best guess matching of `lastfm_dump` against `library_dump`. If artists, song titles, and album titles all match, this is simple enough, but it will also attempt to join `lastfm_dump` rows missing an "album" field by considering song titles, inferred track position, etc. This can become tricky for artists with many live albums and accuracy is not guaranteed. If no match can be made whatsoever, the missing fields will be filled with durations of zero and placeholder text like "Unknown Album". The dump rows with extra library-provided info will be written to another table: `master_dump`.

Run the second query in `insert-from-library-dump.sql` to write the contents of `master_dump` to the respective `plays`, `songs`, `albums`, `genres`, and `artists` tables. You can delete `library_dump`, `lastfm_dump`, and `librefm_dump` after this. Though you might want to record all the contents of `library_dump` to the `songs`/`artists`/etc tables for completeness' sake. This is an exercise left to the reader.
