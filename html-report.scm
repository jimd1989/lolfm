#!/opt/homebrew/bin/csi -s
;#!/usr/local/bin/csi -s

(import (chicken file posix) (chicken io) (chicken process)(chicken string) srfi-1)

(define ◇ conc)
(define-syntax λ (syntax-rules () ((_ . α) (lambda . α))))
(define-syntax ∃ (syntax-rules () ((_ . α) (let* . α))))

(define-syntax ▼
  (syntax-rules (! @)
    ((_ ! (@ )) ">")
    ((_ ! (@ (β γ) ω ...)) (◇ (property (quote β) γ) (▼ ! (@ ω ...))))
    ((_ ! (@ β ω ...)) (◇ (quote β) (▼ ! (@ ω ...))))
    ((_ α (@ β ...)) (◇ "<" (quote α) (▼ ! (@ β ...))))
    ((_ α) (◇ "<" (quote α) ">"))))

(define-syntax ▽
  (syntax-rules (! @)
    ((_ ! α (@ ) ω ...) (◇ ">" ω ... "</" (quote α) ">"))
    ((_ ! α (@ (β γ) δ ...) ω ...) (◇ (property (quote β) γ) (▽ ! α (@ δ ...) ω ...)))
    ((_ ! α (@ β δ ...) ω ...) (◇ (quote β) (▽ ! α (@ δ ...) ω ...)))
    ((_ α (@ β ...) ω ...) (◇ "<" (quote α) (▽ ! α (@ β ...) ω ...)))
    ((_ α ω ...) (◇ "<" (quote α) ">" ω ... "</" (quote α) ">")))) 

(define-syntax css-property
  (syntax-rules () ((_ α ω) (◇ (quote α) ":" ω ";"))))

(define-syntax css-row
  (syntax-rules ()
    ((_ α (β γ) ...) (◇ (quote α) "{" (css-property β γ) ... "}"))))

(define-syntax css
  (syntax-rules () ((_ (α (β γ) ...) ...) (◇ (css-row α (β γ) ...) ...))))

(define-constant OUT (open-output-file "/tmp/lolfm.html"))
(define-constant DB "~/.config/cmus/lolfm.db")
(define-constant QUERIES-PATH "~/prog/misc/lolfm/db/")

(define-constant TAB-CSS
 (◇ ".tabset > input[type=\"radio\"] {position: absolute; left: -200vw;}"
    ".tabset .tab-panel {display: none;}"
    ".tabset > input:first-child:checked ~ .tab-panels > .tab-panel:first-child, "
    ".tabset > input:nth-child(3):checked ~ .tab-panels > .tab-panel:nth-child(2), "
    ".tabset > input:nth-child(5):checked ~ .tab-panels > .tab-panel:nth-child(3), "
    ".tabset > input:nth-child(7):checked ~ .tab-panels > .tab-panel:nth-child(4), "
    ".tabset > input:nth-child(9):checked ~ .tab-panels > .tab-panel:nth-child(5), "
    ".tabset > input:nth-child(11):checked ~ .tab-panels > .tab-panel:nth-child(6) { "
    "display: block;}"))

(define-constant STYLE
  (css (html (-webkit-text-size-adjust "100%"))
       (body (font-family "sans-serif") (background-color "#FFFFEA")
             (margin "0 auto") (max-width "52rem") (padding "1rem"))
       (a (color "#0493DD"))
       (p (line-height "1.5rem"))
       ("tr:nth-child(even)" (background-color "white"))
       (th (color "white") (text-align "left") (padding "10px 0")
           (background-color "#0493DD"))
       ("th + th" (padding-left "5px"))
       ("th:first-child" (padding-left "5px"))
       ("th:last-child" (padding-right "5px"))
       (td (padding "5px 0"))
       ("td + td" (padding-left "5px"))
       ("td:first-child" (padding-left "5px"))
       ("td:last-child" (padding-right "5px"))
       (table (border-collapse "collapse") (padding "1rem") 
              (background-color "#EAFFFF") (border "3px solid #0493DD")
              (margin-bottom "1.5rem") (magin-top "1.5rem")
              (width "95%"))
       (".tabset > label" (display "inline-block") (text-align "center") (padding "10px")
       (background-color "#EAFFFF"))
       (".tabset > input:checked + label" (color "white") (background-color "#0493DD"))))

(define (property key value)
  (if (string? value)
    (◇ " " key "=\"" value "\"")
    (◇ " " key "=" value)))

(define (query sql)
  (with-input-from-pipe (◇ "sqlite3 " DB "< "  QUERIES-PATH sql) read-lines))

(define (a url txt) (▽ a (@ (href url)) txt))

(define (table sql) (▽ table (apply ◇ (query sql))))

(define (tab tabset-name n title checked?)
  (∃ ((tab-name (◇ tabset-name "-" n))
      (button (if checked?
                (▼ input (@ (type "radio") (name tabset-name) (id tab-name) checked))
                (▼ input (@ (type "radio") (name tabset-name) (id tab-name))))))
     (◇ button (▽ label (@ (for tab-name)) title))))

(define (tabs tabset-name . body)
  (∃ ((titles (map car body))
      (sections (map cadr body))
      (ixs (iota (- (length body) 1) 1))
      (first-tab (tab tabset-name 0 (car titles) #t))
      (rest-tabs (apply ◇ (map (λ (α n) (tab tabset-name n α #f)) (cdr titles) ixs)))
      (contents (apply ◇ (map (λ (α) (▽ section (@ (class "tab-panel")) α)) sections))))
    (▽ div (@ (class "tabset"))
      first-tab 
      rest-tabs 
      (▽ div (@ (class "tab-panels")) contents))))

(define-constant HTML
(▽ html
  (▽ head
    (▽ title "lol.fm")
      (▼ meta (@ (name "viewport") (content "width=device-width")
                 (initial-scale 1.0) (maximum-scale 12.0) (user-scalable 'yes)))
      (▼ meta (@ (http-equiv "Content-Type") 
                 (content "text/html; charset=UTF-8")))
      (▽ style (◇ TAB-CSS STYLE)))
    (▽ body
      (▽ h1 "lol.fm")
      (▽ p 
        "lolfm is an industry leading amazingly simple scrobbler (ASS). "
        "Just cmus and a local sqlite file on your hard drive. "
        "If you'd like to run it yourself, check it out on "
        (a "https://github.com/jimd1989/lolfm" "Github") ".")
      (▽ h2 (◇ "Activity (" (car (query "total-years.sql")) " years)"))
      (tabs "recent"
        `("Plays" ,(table "most-recent.sql"))
        `("Discoveries" ,(table "recent-discoveries.sql"))
        `("Loves" ,(table "recently-loved.sql")))
      (▽ h2 (◇ "Top Artists (" (car (query "total-artists.sql")) " total)"))
      (tabs "top-artists"
        `("Plays" ,(table "top-artists-by-plays.sql"))
        `("Hours" ,(table "top-artists-by-time.sql"))
        `("Year" ,(table "top-artists-12-months.sql"))
        `("Month" ,(table "top-artists-1-month.sql"))
        `("Week" ,(table "top-artists-1-week.sql")))
      (▽ h2 (◇ "Top Albums (" (car (query "total-albums.sql")) " total)"))
      (tabs "top-albums"
        `("Plays" ,(table "top-albums-by-plays.sql"))
        `("Hours" ,(table "top-albums-by-time.sql"))
        `("Year" ,(table "top-albums-12-months.sql")))
      (▽ h2 (◇ "Top Songs (" (car (query "total-songs.sql")) " total)"))
      (tabs "top-songs"
        `("Plays" ,(table "top-songs.sql"))
        `("Hours" ,(table "top-songs-by-time.sql"))
        `("Year" ,(table "top-songs-12-months.sql")))
      (▽ h2 (◇ "Top Genres (" (car (query "total-genres.sql")) " total)"))
      (tabs "top-genres"
        `("Plays" ,(table "top-genres-by-plays.sql"))
        `("Hours" ,(table "top-genres-by-time.sql")))
      (▽ h2 "Top Years")
      (tabs "top-years"
        `("Plays" ,(table "top-years.sql"))
        `("Hours" ,(table "top-years-by-time.sql")))
      (▽ h2 "Discover")
      (tabs "discover"
        `("Unfamiliar" ,(table "listen-again.sql"))
        `("Neglected" ,(table "forgotten-favorites.sql"))))))

(display HTML OUT)
(close-output-port OUT)
