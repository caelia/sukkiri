(include "rel8r-store.scm")
(import rel8r-store)

;; We probably need to implement some grouping concept in case of circular dependencies

(define (create-email-type db)
  (add-struct-type db "email-address"
                   extensible: 0
                   members: '(((rel-name . "label") (cardinality . "one") (type . "string"))
                              ((rel-name . "value") (cardinality . "one") (type . "string"))
                              ((rel-name . "preferred") (cardinality . "zoo") (type . "boolean")))))
  
(define (create-phone-type db)
  (add-struct-type db "phone-number"
                   extensible: 0
                   members: '(((rel-name . "label") (cardinality . "one") (type . "string"))
                              ((rel-name . "value") (cardinality . "one") (type . "string"))
                              ((rel-name . "preferred") (cardinality . "zoo") (type . "boolean")))))

(define (create-person-type db)
  (add-struct-type db "person"
                   members: '(((rel-name . "surname") (cardinality . "one") (type . "string"))
                              ((rel-name . "first-name") (cardinality . "zoo") (type . "string"))
                              ((rel-name . "middle-name") (cardinality . "zoo") (type . "string"))
                              ((rel-name . "title") (cardinality . "zoo") (type . "string"))
                              ((rel-name . "name-suffix") (cardinality . "zoo") (type . "string"))
                              ((rel-name . "email") (cardinality . "zoma") (type . "email-address"))
                              ((rel-name . "phone") (cardinality . "zoma") (type . "phone-number")))))

(define (create-context-type db)
  (add-vocab-type db "context" '("home" "office" "computer" "online" "errands")))

(define (create-action-type db)
  (add-struct-type db "action"
                   extensible: 0
                   members: '(((rel-name . "content") (cardinality . "one") (type . "string"))
                              ((rel-name . "context") (cardinality . "zoma") (type . "context"))
                              ((rel-name . "next") (cardinality . "zoo") (type . "boolean")))))

(define (create-project-type db)
  (add-struct-type db "project"
                   extensible: 0
                   members: '(((rel-name . "title") (cardinality . "one") (type . "string"))
                              ((rel-name . "description") (cardinality . "zoo") (type . "string"))
                              ((rel-name . "deadline") (cardinality . "zoo") (type . "date"))
                              ((rel-name . "contact") (cardinality . "zoma") (type . "person"))
                              ((rel-name . "step") (cardinality . "zoma") (type . "action")))))

