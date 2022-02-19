(import 
  (chicken file posix)
  (chicken io) 
  (chicken process)
  (chicken string) 
  srfi-1)

(define-syntax λ (syntax-rules () ((_ . α) (lambda . α))))
(define ∘ compose) 
(define ◇ conc)

(define-constant DB "/tmp/db.db")
(define-constant FIFO "/tmp/fifo")
(define-constant MIGRATIONS "~/prog/misc/lolfm/db/migrations")

(define (log args events)
  (let ((event (car args)))
    (append `(,event) events)))

(define (dump events)
  (write events)
  (newline)
  events)

(define (clear) '())

(define (dispatch cmd events)
  (let ((type (car cmd))
        (args (cdr cmd)))
    (cond ((eq? type 'log) (log args events))
          ((eq? type 'dump) (dump events))
          ((eq? type 'clear) (clear))
          ((eq? type 'love) '())
          ((eq? type 'unlove) '())
          ((eq? type 'suppress) '())
          ((eq? type 'unsuppress) '()))))

(define (drain events)
  (let* ((fn (open-input-file FIFO))
         (cmds (read-list fn)))
    (close-input-port fn)
    (foldr dispatch events cmds)))

(define (listen events) (listen (drain events)))

(define (migrate)
  (let* ((cmd (◇ "ls " MIGRATIONS "/*.sql"))
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

; this is running locally, so don't complain about injection.
(define (log-play artist aartist album title genre year duration)
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
      "VALUES('",title"',(SELECT id FROM artist),(SELECT id FROM genre))"
      "ON CONFLICT(title, artist) DO UPDATE SET genre=excluded.genre;"

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
