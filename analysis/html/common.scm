(import (chicken load) (chicken string) sxml-transforms)
(include-relative "../helpers/monad.scm")
(include-relative "../helpers/syntax.scm")

; figure out css, file naming, etc

(← (write-html file ω)
  (for (← written? (either (with-output-to-file file (λ () (SXML->HTML ω)))))
       (← success? (ensure written? (◇ "Error writing html file: " file) #t))
       (yield success?)))

(← EXAMPLE 
   '(div (@ (id "content") checked)
         (h1 "Welcome")
         (p "This is a " (strong "nested") " layout.")))
