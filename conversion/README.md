The last.fm API makes a user's play history available as a dump of XML data. I'm not interested in documenting the whole process here, but I will leave scripts and snippets of steps I took to get that data into the lol.fm database.

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

Next up is querying cmus with this listening data for missing fields like genre, etc.
