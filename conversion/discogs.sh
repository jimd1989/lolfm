#!/bin/sh

rm /tmp/discogs-durations /tmp/discogs-tracks /tmp/discogs-titles /tmp/discogs-artists /tmp/discogs-artists /tmp/discogs-genres /tmp/discogs-years
genre="$1"
url="$2"
id=$(echo $url | cut -d "/" -f5 | cut -d "-" -f 1)
request_url="https://api.discogs.com/releases/$id"
user_agent="SimpleDiscogsScript/0.1 +http://dalrym.pl"
# weird-ass unicode errors when using echo; writing to /tmp files, sorry
curl $request_url --user-agent "'$user_agent'" > /tmp/discogs-payload
cat /tmp/discogs-payload | jq -r '.artists[] | .name, .join' | tr '\n' ' ' | rev | cut -c 3- | rev > /tmp/discogs-album-artist
cat /tmp/discogs-payload | jq -r '.title' > /tmp/discogs-title
cat /tmp/discogs-payload | jq -r '.year' > /tmp/discogs-year
cat /tmp/discogs-payload | jq -r '.tracklist[]?.duration' | xargs -I {} ./to-seconds.sh '{}' > /tmp/discogs-durations
cat /tmp/discogs-payload | jq -r '.tracklist[]?.title' > /tmp/discogs-tracks
for k in $(cat /tmp/discogs-durations) ; do echo "$genre" >> /tmp/discogs-genres ; done
for k in $(cat /tmp/discogs-durations) ; do cat /tmp/discogs-album-artist >> /tmp/discogs-artists ; done
for k in $(cat /tmp/discogs-durations) ; do cat /tmp/discogs-title >> /tmp/discogs-titles ; done
for k in $(cat /tmp/discogs-durations) ; do cat /tmp/discogs-year >> /tmp/discogs-years ; done
paste /tmp/discogs-durations /tmp/discogs-tracks /tmp/discogs-titles /tmp/discogs-artists /tmp/discogs-artists /tmp/discogs-genres /tmp/discogs-years
