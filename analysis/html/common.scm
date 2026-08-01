(import (chicken fixnum) (chicken load) (chicken string) srfi-1 sxml-transforms)
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/prelude.scm")
(include-relative "../helpers/sorted-slices.scm")
(include-relative "../helpers/syntax.scm")

; figure out css, file naming, etc
(← (write-html dir file ω)
  (for (← directory-created? (either (create-directory dir #t)))
       (path (◇ dir "/" file))
       (_ (print (◇ "writing " path)))
       (← written? (either (with-output-to-file path (λ () (SXML->HTML ω)))))
       (← success? (ensure written? (◇ "Error writing html file: " file) #t))
       (yield success?)))

(← (write-css dir file ω)
  (for (← directory-created? (either (create-directory dir #t)))
       (path (◇ dir "/" file))
       (_ (print (◇ "writing " path)))
       (← written? (either (with-output-to-file path (λ () (display ω)))))
       (← success? (ensure written? (◇ "Error writing html file: " file) #t))
       (yield success?)))

(← (link path id name)
   (? (string=? "Various Artists" name)
      name
      `(a (@ (href ,(◇ path id ".html"))) ,name)))

(← (loved loved?)
  (? loved? `(span (@ (style "font-family: 'Times New Roman', serif;")) "♥") ∅))

(← (tag α . ω) (⊂ α ω))

(← column-title ↑)
(← column-key ↑↓)
(← column-transformer (∘ ↑ ↓↓))

(← (row-transformer . columns)
  (∃ ((fs (∀ (λ (c) (∘ (D tag 'td) (column-transformer c) (column-key c)))
             columns)))
    (λ (row) (∃ ((r (⊂ 'tr (∀ (D & row) fs)))
                 (rank (↑↓ (↑↓ r)))) 
               (? (∧ (number? rank) (= -1 rank)) ∅ r))))) ; -1 = null

(← (table-transformer sort-key . columns)
  (∃ ((titles (⊂ 'tr (∀ (∘ (D tag 'th) column-title) columns)))
      (row-f ($ row-transformer columns))
      (wrapper (∘ (D tag 'table) (D tag 'tbody))))
    (λ (table) ((∘ (◁ wrapper) (◁ (D ⊂ titles)) (◁ (D ∀? (∘ not ∅?))))
                   (? (list? table) 
                     (right (∀ row-f table))
                     (⊆v⍋∀ row-f sort-key table))))))

(← (table-transformer-truncated n sort-key . columns)
  (∃ ((titles (⊂ 'tr (∀ (∘ (D tag 'th) column-title) columns)))
      (row-f ($ row-transformer columns))
      (wrapper (∘ (D tag 'table) (D tag 'tbody))))
    (λ (table) ((∘ (◁ wrapper) (◁ (D ⊂ titles)) (◁ (D ↑n n))
                   (◁ (D ∀? (∘ not ∅?))))
                   (? (list? table) 
                     (right (∀ row-f table))
                     (⊆v⍋∀ row-f sort-key table))))))

(← tabbed-table-name ↑)
(← tabbed-table-f ↑↓)

(← (render-table-inputs title named-transformer n)
  (∃ ((ω (◇ title "-" n)) (name (tabbed-table-name named-transformer))
      (checked? (? (= 0 n) " checked" ∅)))
    `((input (@ (id ,ω) (name ,title) (type radio) ,checked?))
      (label (@ (for ,ω)) ,name))))

(← (render-table rows named-transformer)
  (⊙ (λ (ω) `(section (@ (class tab-panel)) ,ω))
     ((tabbed-table-f named-transformer) rows)))

(← (render-tabbed-table inputs contents)
  `(div (@ (class tabset)) ,@inputs (div (@ (class tab-panels)) ,@contents)))

(← (tabbed-table-transformer title . fs)
  (λ (rows)
    (for (inputs (∀ (λ (α ω) (render-table-inputs title α ω)) fs (iota (ρ fs))))
         (← contents (traverse (D render-table rows) fs))
         (yield (render-tabbed-table inputs contents)))))

(← (n⊥s n)
  (letrec ((ωs (string->list (number->string n)))
           (▽ (λ (αs θ) (cond ((∅? αs) ∅)
                              ((= θ 3) (⊂ #\, (▽ αs 0)))
                              (else (⊂ (↑ αs) (▽ (↓ αs) (+ θ 1))))))))
    ($ ◇ (⊖ (▽ (⊖ ωs) 0)))))

(← (seconds⊥hours n) (n⊥s (fx/ (fx/ n 60) 60)))

(← (html title . body)
   `(html (head 
            (meta (@ (name "viewport") (content "width=device-width")
                     (initial-scale 1.0) (maximum-scale 12.0)
                     (user-scalable yes)))
            (meta (@ (http-equiv "Content-Type")
                     (content "text/html; charset=UTF-8")))
            (title ,title)
            (link (@ (rel "stylesheet") (type "text/css")
                     (href ,(? (string=? "lol.fm" title)
                               "./style.css" 
                               "../style.css")))))
          (body ,@body)))

(← css
"
.tabset > input[type=\"radio\"] {
  position: absolute;
  left: -200vw;
}
.tabset .tab-panel {
  display: none;
}
.tabset > input:first-child:checked ~ .tab-panels > .tab-panel:first-child,
.tabset > input:nth-child(3):checked ~ .tab-panels > .tab-panel:nth-child(2),
.tabset > input:nth-child(5):checked ~ .tab-panels > .tab-panel:nth-child(3),
.tabset > input:nth-child(7):checked ~ .tab-panels > .tab-panel:nth-child(4),
.tabset > input:nth-child(9):checked ~ .tab-panels > .tab-panel:nth-child(5),
.tabset > input:nth-child(11):checked ~ .tab-panels > .tab-panel:nth-child(6) {
  display: block;
}
html {
  -webkit-text-size-adjust:80%;
}
body {
  font-family:sans-serif;
  background-color:#FFFFEA;
  margin:0 auto;
  max-width:52rem;
  padding:1rem;
}
a {
  color:#0493DD;
}
p {
  line-height:1.5rem;
}
tr:nth-child(even) {
  background-color:white;
}
th {
  color:white;
  text-align:left;
  padding:10px 0;
  background-color:#0493DD;
}
th + th {
  padding-left:5px;
}
th:first-child {
  padding-left:5px;
}
th:last-child {
  padding-right:5px;
}
td {
  padding:5px 0;
}
td a {
  color: inherit;
}
td + td {
  padding-left:5px;
}
td:first-child {
  padding-left:5px;
}
td:last-child {
  padding-right:5px;
}
table {
  border-collapse:collapse;
  padding:1rem;
  background-color:#EAFFFF;
  border:3px solid #0493DD;
  margin-bottom:1.5rem;
  magin-top:1.5rem;
  width:95%;
}
.tabset > label {
  display:inline-block;
  text-align:center;
  padding:10px;
  background-color:#EAFFFF;
}
.tabset > input:checked + label {
  color:white;
  background-color:#0493DD;
}
"
)
