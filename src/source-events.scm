#!/usr/local/bin/csi -s

(import 
  (chicken file posix)
  (chicken io) 
  (chicken process)
  (chicken string) 
  srfi-1)

(define-syntax λ (syntax-rules () ((_ . α) (lambda . α))))
(define-syntax ∃ (syntax-rules () ((_ . α) (let* . α))))
(define ◇ conc)

(define-constant DB "~/.config/cmus/lolfm.db")
(define-constant FIFO "/tmp/lolfm-fifo")
(define-constant MIGRATIONS "~/prog/misc/lolfm/db/migrations")
(define-constant QUERY "/tmp/lolfm-query")

(define (run-query x)
  (∃ ((fn (open-output-file QUERY)))
    (display x fn)
    (close-output-port fn)
    (system (◇ "sqlite3 " DB " < " QUERY))))

; this is running locally, so don't complain about injection.
(define (log-query artist aartist album title genre year duration)
  (apply ◇
  `("BEGIN TRANSACTION;"
    "INSERT INTO artists(name) VALUES('",artist"')"
    "ON CONFLICT(name) DO UPDATE SET name=excluded.name;"

    "INSERT INTO artists(name) VALUES('",aartist"')"
    "ON CONFLICT(name) DO UPDATE SET name=excluded.name;"

    "INSERT INTO genres(name) VALUES('",genre"')"
    "ON CONFLICT(name) DO UPDATE SET name=excluded.name;"

    "WITH album_artist AS (SELECT id FROM artists WHERE name='",aartist"')"
    "INSERT INTO albums(title, artist, year)"
    "VALUES('",album"', (SELECT id FROM album_artist), ",year")"
    "ON CONFLICT(title, artist) DO UPDATE SET year=excluded.year;"

    "WITH artist AS (SELECT id FROM artists WHERE name='",artist"'),"
    "genre AS (SELECT id FROM genres WHERE name='",genre"')"
    "INSERT INTO songs(title, artist, genre)"
    "VALUES('",title"',"
    "(SELECT id FROM artist),"
    "(SELECT id FROM genre))"
    "ON CONFLICT(title, artist)"
    "DO UPDATE SET genre=excluded.genre;"

    "WITH song AS ("
    "SELECT id FROM songs "
    "WHERE artist=(SELECT id FROM artists WHERE name='",artist"')"
    "AND title=('",title"')),"
    "album AS ("
    "SELECT id FROM albums "
    "WHERE artist=(SELECT id FROM artists WHERE name='",aartist"')"
    "AND title=('",album"'))"
    "INSERT INTO plays(song, album, duration)"
    "VALUES((SELECT id FROM song), (SELECT id FROM album), ",duration");"
    "COMMIT;"
    )))

(define (prepare-log-query x)
  (∃ ((get (λ (α) (cadr (assoc α x))))
      (ordered-fields '(artist albumartist album title genre date duration))
      (args (map get ordered-fields)))
    (run-query (apply log-query args))))

(define (apostrophe-quote x)
  (list->string
    (foldr 
      (λ (x acc) (if (char=? #\' x) (cons #\' (cons #\' acc)) (cons x acc)))
      '()
      (string->list x))))

(define (love-track artist title)
  (∃ ((a (apostrophe-quote artist))
      (t (apostrophe-quote title))
      (query 
        (◇ "INSERT INTO loved(song) SELECT songs.id "
           "FROM songs JOIN artists ON (artists.id = songs.artist) "
           "WHERE (songs.title = '" t "' COLLATE NOCASE) AND "
           "      (artists.name = '" a "' COLLATE NOCASE);")))
     (run-query query)))

(define (new-song? now prev)
  (not (equal?
         `(,(assoc 'artist now) ,(assoc 'album now) ,(assoc 'title now))
         `(,(assoc 'artist prev) ,(assoc 'album prev) ,(assoc 'title prev)))))

(define (log-event args events)
  (∃ ((now (car args))
      (now-status (cadr (assoc 'status now)))
      (prev (if (null? events) '((status empty)) (car events)))
      (prev-status (cadr (assoc 'status prev))))
     (cond
       ((and (eq? now-status 'playing) (eq? prev-status 'empty))
        `(,now))
       ((and (eq? now-status 'playing) (eq? prev-status 'playing))
        (prepare-log-query prev)
        `(,now))
       ((and (eq? now-status 'paused) (eq? prev-status 'playing))
        `(,now))
       ((and (new-song? now prev)
             (eq? now-status 'playing) (eq? prev-status 'paused))
        (prepare-log-query prev)
        `(,now))
       ((and (eq? now-status 'playing) (eq? prev-status 'paused))
        `(,now))
       ((and (not (eq? now-status 'playing)) (eq? prev-status 'playing))
        (prepare-log-query prev)
        '())
       (else events))))

(define (dump events)
  (write events)
  (newline)
  events)

(define (clear) '())

(define (dispatch input events)
  (∃ ((cmd (if (and (list? input) (not (null? input))) input '(invalid)))
      (type (car cmd))
      (args (cdr cmd)))
    (cond ((eq? type 'log) (log-event args events))
          ((eq? type 'dump) (dump events))
          ((eq? type 'clear) (clear))
          ((eq? type 'love) (apply love-track args) events)
          ((eq? type 'unlove) events)
          ((eq? type 'suppress) events)
          ((eq? type 'unsuppress) events)
          (else events))))

(define (drain events)
  (∃ ((fn (open-input-file FIFO))
      (cmds (read-list fn)))
    (close-input-port fn)
    (foldr dispatch events cmds)))

(define (listen events) (listen (drain events)))

(define (migrate)
  (∃ ((cmd (◇ "ls " MIGRATIONS "/*.sql"))
      (scripts (call-with-input-pipe cmd read-list))
      (run-migration (λ (x) (system (◇ "sqlite3 " DB " < " x)))))
    (for-each run-migration scripts)))

(define (main)
  (display (◇ "Creating/checking for " FIFO)) (newline)
  (system (◇ "mkfifo " FIFO)) 
  (display (◇ "Creating/checking for " DB)) (newline)
  (system (◇ "touch " DB))
  (display (◇ "Applying migration scripts from " MIGRATIONS)) (newline)
  (migrate)
  (display "Listening for events") (newline)
  (listen '()))

(main)
