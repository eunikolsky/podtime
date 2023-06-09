#!/usr/bin/env sh

set -eu

seconds="$(
    for podid in $( sqlite3 ~/gPodder/Database 'select id from podcast;' )
    do
        sqlite3 ~/gPodder/Database 'select """'"$HOME"'/gPodder/Downloads/" || p.download_folder || "/" || e.download_filename || """" from episode e join podcast p on e.podcast_id=p.id where p.id='"$podid"' and e.state=1 and e.published>=(SELECT min(published) FROM episode WHERE podcast_id='"$podid"' and state=1 and is_new);'
    done \
        | xargs -n20 -P8 sox --info -D 2>/dev/null \
        | paste -sd+ - \
        | bc \
        | cut -d '.' -f1
    )"
printf '%dd %02d:%02d:%02d\n' $((seconds/86400)) $((seconds%86400/3600)) $((seconds%3600/60)) $((seconds%60))
