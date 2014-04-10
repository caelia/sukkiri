(define project-spec
  '(#t ((title "one" "string") (deadline "one" "date") (contact "zoma" "nref") (step "ooma" "nref"))))

;; context should actually be a vocab
(define action-spec
  '(#f ((content "one" "string") (context "zoma" "string"))))

(define person-spec
  '(#f ((given-name "one" "string") (surname "one" "string") (email "zoma" "nref") (phone "zoma" "nref"))))

(define email-spec
  '(#f ((label "one" "string") (address "one" "string"))))

(define project-scm
  `((%TYPE . "project")
    (%ID . "breed-mutant-seahorses")
    (%LABEL . "title")
    (title . "Breed Mutant Seahorses")
    (deadline . "2014-03-28T17:00:00")
    (contact . "%NREF%jane-morgan")
    (step . (vector "%NREF%build-tank" "%NREF%catch-seahorses" "%NREF%assign-to-tanks"))))

(define action1-scm
  `((%TYPE . "action")
    (%ID . "build-tank")
    (%LABEL . "content")
    (content . "Build tank for seahorses")
    (context . ,(vector "lab"))))

(define action2-scm
  `((%TYPE . "action")
    (%ID . "catch-seahorses")
    (%LABEL . "content")
    (content . "Catch us some seahorses")
    (context .  ,(vector "atlantic" "pacific" "indian"))))

(define action3-scm
  `((%TYPE . "action")
    (%ID . "assign-to-tanks")
    (%LABEL . "content")
    (content . "Put 'em in da tanks")
    (context . ,(vector "lab"))))

(define person-scm
  `((%TYPE . "person")
    (%ID . "jane-morgan")
    (%LABEL . "given-name & surname")
    (given-name . "Jane")
    (surname . "Morgan")
    (email . ,(vector "%NREF%jane-morgan-work-email"))))

(define email-scm
  '((%TYPE . "email-address")
    (%ID . "jane-morgan-work-email")
    (label . "Work")
    (address . "dr.jane.v.morgan@sea-creature-research.com" )))

(define project-json
  "{ \"%TYPE\": \"project\",
     \"%ID\": \"breed-mutant-seahorses\",
     \"%LABEL\": \"title\",
     \"title\": \"Breed Mutant Seahorses\",
     \"deadline\": \"2014-03-28T17:00:00\",
     \"contact\": \"%NREF%jane-morgan\",
     \"step\": [\"%NREF%build-tank\", \"%NREF%catch-seahorses\", \"%NREF%assign-to-tanks\"] }")

(define action1-json
  "{ \"%TYPE\": \"action\",
     \"%ID\": \"build-tank\",
     \"%LABEL\": \"content\",
     \"content\": \"Build tank for seahorses\",
     \"context\": [\"lab\"] }")

(define action2-json
  "{ \"%TYPE\": \"action\",
     \"%ID\": \"catch-seahorses\",
     \"%LABEL\": \"content\",
     \"content\": \"Catch us some seahorses\",
     \"context\": [\"atlantic\",\"pacific\",\"indian\"] }")

(define action3-json
  "{ \"%TYPE\": \"action\",
     \"%ID\": \"assign-to-tanks\",
     \"%LABEL\": \"content\",
     \"content\": \"Put 'em in da tanks\",
     \"context\": [\"lab\"] }")

(define person-json
  "{ \"%TYPE\": \"person\",
     \"%ID\": \"jane-morgan\",
     \"%LABEL\": \"given-name & surname\",
     \"given-name\": \"Jane\",
     \"surname\": \"Morgan\",
     \"email\": [\"%NREF%jane-morgan-work-email\"] }")

(define email-json
  "{ \"%TYPE\": \"email-address\",
     \"%ID\": \"jane-morgan-work-email\",
     \"label\": \"Work\",
     \"address\": \"dr.jane.v.morgan@sea-creature-research.com\" }")
