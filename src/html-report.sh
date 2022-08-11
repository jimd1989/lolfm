#!/bin/sh

# Change paths to suit your own filesystem, of course

echo "<html>" > /tmp/lolfm.html
echo '<head><title>lol.fm</title><meta name="viewport" content="width=device-width" initial-scale=1.0 maximum-scale=12.0 user-scalable=yes><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><style>' >> /tmp/lolfm.html
echo 'html {-webkit-text-size-adjust: 100%;}body {font-family: sans-serif; background-color: #FFFFEA; margin: 0 auto; max-width: 52rem; padding: 1rem;}a {color: #0493DD}p {line-height: 1.5rem;}' >> /tmp/lolfm.html
echo 'th {color:white;text-align:center;border-bottom:2pt solid #0493DD;border-right:2pt solid #0493DD;padding:3px;background-color: #0493DD;}td {padding:3px;;border-right:2pt solid #0493DD;}table {border-collapse:collapse;padding: 1rem; background-color: #EAFFFF; border: 3px solid #0493DD; margin-bottom: 1.5rem; margin-top: 1.5rem;}' >> /tmp/lolfm.html
echo '</style></head><body>' >> /tmp/lolfm.html
echo "<h1>lol.fm</h1>" >> /tmp/lolfm.html
echo '<p>lolfm is an industry leading amazingly simple scrobbling service (ASSS). Just cmus and a local sqlite file on your hard drive. You make the stats. If you would like to run it yourself, check it out on <a href="https://github.com/jimd1989/lolfm">Github</a>.' >> /tmp/lolfm.html
echo "<h2>Recent</h2>" >> /tmp/lolfm.html
echo "<table>" >> /tmp/lolfm.html
sqlite3 ~/.config/cmus/lolfm.db < ~/prog/misc/lolfm/db/queries/most-recent.sql >> /tmp/lolfm.html
echo "</table>" >> /tmp/lolfm.html
echo "<h2>Top Artists</h2>" >> /tmp/lolfm.html
echo "<table>" >> /tmp/lolfm.html
sqlite3 ~/.config/cmus/lolfm.db < ~/prog/misc/lolfm/db/queries/top-artists.sql >> /tmp/lolfm.html
echo "</table>" >> /tmp/lolfm.html
echo "<h2>Top Genres</h2>" >> /tmp/lolfm.html
echo "<table>" >> /tmp/lolfm.html
sqlite3 ~/.config/cmus/lolfm.db < ~/prog/misc/lolfm/db/queries/top-genres.sql >> /tmp/lolfm.html
echo "</table>" >> /tmp/lolfm.html
echo "</body></html>" >> /tmp/lolfm.html
