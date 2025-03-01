#!/usr/local/bin/csi -s

(import (chicken io) (chicken load) (chicken string) srfi-1)
(load-relative "../src/helpers.scm")
(← column (? (∘ (⊥ = 3) ρ) (⊥ ⊢ "") I))
(← (fix α ω) (▭ (($s (∘ column (∴∴ (∘ (⊥ + ω) s→n) I)) "	") α)))
((S (Λ ∀∀ fix % %) (∘ ι ρ)) (read-lines))
