The last.fm API makes a user's play history available as a dump of XML data. I'm not interested in documenting the whole process here, but I will leave scripts and snippets of steps I took to get that data into the lol.fm database.

## Parsing last.fm dump

All play history was fetched with a last.fm scraping script, written in (outdated) Python. I forget which. Just search for it. I heavily edited it to stream raw XML data because I found the "fetch everything, then decode it" approach obnoxious when I had close to 100K plays.

I made sure all the strings were decoded as UTF-8. Double check this.

I used `xmlstarlet`, a tool like `jq`, to isolate the date, artist, title, and album fields respectively into a four-column TSV file. Tabs were chosen over commas since commas can and do appear in song titles. I am not sure if tabs do. The title field should probably actually be the final column for this reason. The file was called `tsv`.

I used the `to-unix-epoch.sh` script to convert the date strings into unix timestamps.

```
cat tsv | cut -d "    " -f 1 | xargs -I {} ~/prog/misc/lolfm/conversion/to-unix-epoch.sh "{}" > /tmp/timestamps
```

I pasted this column to the front of the TSV.

```
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
