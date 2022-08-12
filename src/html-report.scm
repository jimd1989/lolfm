#!/usr/local/bin/csi -s

(import (chicken file posix) (chicken io) (chicken process)(chicken string))

(define ◇ conc)

(define-syntax ▽
  (syntax-rules (@)
    ((_ α (@ (β γ) ...) ω ...)
     (◇ "<" (quote α) (property (quote β) γ) ... ">" ω ... "</" (quote α) ">"))
    ((_ α ω ...)
     (◇ "<" (quote α) ">" ω ... "</" (quote α) ">"))))

(define-syntax css-property
  (syntax-rules () ((_ α ω) (◇ (quote α) ":" ω ";"))))

(define-syntax css-row
  (syntax-rules ()
    ((_ α (β γ) ...) (◇ (quote α) "{" (css-property β γ) ... "}"))))

(define-syntax css
  (syntax-rules () ((_ (α (β γ) ...) ...) (◇ (css-row α (β γ) ...) ...))))

(define-constant OUT (open-output-file "/tmp/lolfm.html"))
(define-constant DB "~/.config/cmus/lolfm.db")
(define-constant QUERIES-PATH "~/prog/misc/lolfm/db/queries/")

(define-constant STYLE
  (css (html (-webkit-text-size-adjust "100%"))
       (body (font-family "sans-serif") (background-color "#FFFFEA")
             (margin "0 auto") (max-width "52rem") (padding "1rem"))
       (a (color "#0493DD"))
       (p (line-height "1.5rem"))
       ("tr:nth-child(even)" (background-color "white"))
       (th (color "white") (text-align "center") (padding "5px")
           (background-color "#0493DD"))
       (td (padding "5px") (border-right "1pt solid #0493DD"))
       (table (border-collapse "collapse") (padding "1rem") 
              (background-color "#EAFFFF") (border "3px solid #0493DD")
              (margin-bottom "1.5rem") (magin-top "1.5rem"))))

(define (property key value)
  (if (string? value)
      (◇ " " key "=\"" value "\"")
      (◇ " " key "=" value)))

(define (query sql)
  (with-input-from-pipe (◇ "sqlite3 " DB "< "  QUERIES-PATH sql) read-lines))

(define (table title sql) (◇ (▽ h2 title) (▽ table (apply ◇ (query sql)))))

(define-constant HTML
(▽ html
  (▽ head
    (▽ title "lol.fm")
      (▽ meta (@ (name "viewport") (content "width=device-width")
                 (initial-scale 1.0) (maximum-scale 12.0) (user-scalable 'yes)))
      (▽ meta (@ (http-equiv "Content-Type") 
                 (content "text/html; charset=UTF-8")))
      (▽ style STYLE))
    (▽ body
      (▽ h1 "lol.fm")
      (▽ p 
        "lolfm is an industry leading amazingly simple scrobbler (ASS). "
        "Just cmus and a local sqlite file on your hard drive. "
        "If you'd like to run it yourself, check it out on "
        (▽ a (@ (href "https://github.com/jimd1989/lolfm")) "Github") ".")
      (table "Recent" "most-recent.sql")
      (table "Top Artists" "top-artists.sql")
      (table "Top Songs" "top-songs.sql")
      (table "Top Genres" "top-genres.sql"))))

(display HTML OUT)
(close-output-port OUT)
