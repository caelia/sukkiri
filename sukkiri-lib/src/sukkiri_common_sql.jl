;;; sukkiri-common-sql.rkt -- SQL queries that work for all databases.
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the BSD license.
;;;   See the accompanying LICENSE file for details.

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  DATABASE SETUP  --------------------------------------------------

(define create-primitive-table-query
  "CREATE TABLE primitives (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT UNIQUE NOT NULL
  );")

(define populate-primitive-table-queries
  '("INSERT INTO primitives (name) VALUES ('integer');"
    "INSERT INTO primitives (name) VALUES ('float');"
    "INSERT INTO primitives (name) VALUES ('boolean');"
    "INSERT INTO primitives (name) VALUES ('string');"
    "INSERT INTO primitives (name) VALUES ('date');"
    "INSERT INTO primitives (name) VALUES ('time');"
    "INSERT INTO primitives (name) VALUES ('period');"
    "INSERT INTO primitives (name) VALUES ('nref');"
    "INSERT INTO primitives (name) VALUES ('rref');"
    "INSERT INTO primitives (name) VALUES ('sref');"
    "INSERT INTO primitives (name) VALUES ('xref');"))

(define create-string-type-table-query
  "CREATE TABLE string_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    pattern TEXT NOT NULL,
    description TEXT
  );")

(define create-number-type-table-query
  "CREATE TABLE number_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    minval FLOAT,
    maxval FLOAT,
    step FLOAT,
    digits INTEGER,
    description TEXT
  );")

(define create-vocab-table-query
  "CREATE TABLE vocab_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    term TEXT NOT NULL
  );")

(define create-cardinality-table-query
  "CREATE TABLE cardinalities (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
  );")

(define populate-cardinality-table-queries
  '("INSERT INTO cardinalities (name) VALUES ('one');"
    "INSERT INTO cardinalities (name) VALUES ('zoo');"
    "INSERT INTO cardinalities (name) VALUES ('zoma');"
    "INSERT INTO cardinalities (name) VALUES ('ooma');"))

(define create-struct-type-table-query
  "CREATE TABLE struct_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    extensible INTEGER default 0,
    description TEXT
  );")

(define create-type-class-table-query
  "CREATE TABLE type_classes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
  );")

(define populate-type-class-table-queries
  '("INSERT INTO type_classes (name) VALUES ('primitive');"
    "INSERT INTO type_classes (name) VALUES ('string');"
    "INSERT INTO type_classes (name) VALUES ('number');"
    "INSERT INTO type_classes (name) VALUES ('vocab');"
    "INSERT INTO type_classes (name) VALUES ('struct');"
    "INSERT INTO type_classes (name) VALUES ('union');"))

(define create-types-table-query
  "CREATE TABLE types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    class INTEGER REFERENCES type_classes(id)
  );")

(define populate-types-table-queries
  '("INSERT INTO types (name, class)
     SELECT 'integer', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'float', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'boolean', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'string', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'date', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'time', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'period', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'nref', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'rref', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'sref', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'xref', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'any', id FROM type_classes WHERE name = 'union';"))
    
(define create-union-type-table-query
  "CREATE TABLE union_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    member_type INTEGER REFERENCES types(id)
  );")

(define populate-union-type-table-queries
  '("INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'integer';" 
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'float';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'boolean';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'string';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'date';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'time';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'period';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'nref';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'rref';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'sref';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'xref';"))

(define create-struct-members-table-query
  "CREATE TABLE struct_type_members (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    struct_type INTEGER REFERENCES struct_types(id), 
    rel_name TEXT NOT NULL,
    cardinality INTEGER REFERENCES cardinalities(id),
    mem_type INTEGER REFERENCES types(id)
  );")

(define create-statement-table-query
  "CREATE TABLE statements (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    s  TEXT NOT NULL,
    p  TEXT NOT NULL,
    o  TEXT NOT NULL,
    t  INTEGER REFERENCES types(id) NOT NULL,
    dt TEXT DEFAULT CURRENT_TIMESTAMP
  );")

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  USER-DEFINED TYPE MANAGEMENT  ------------------------------------

(define add-string-type-query
  "INSERT INTO string_types (name, pattern, description) VALUES ($1, $2, $3);")

(define add-number-type-query
  "INSERT INTO number_types (name, minval, maxval, step, digits, description)
   VALUES ($1, $2, $3, $4, $5, $6);")

(define add-vocab-type-term-query
  "INSERT INTO vocab_types (name, term) VALUES ($1, $2);")
    "company")
(define add-struct-type-query
  "INSERT INTO struct_types (name, extensible, description) VALUES ($1, $2, $3);")

(define add-struct-member-query
  "INSERT INTO struct_type_members (struct_type, rel_name, cardinality, mem_type)
    SELECT struct_types.id, $1, cardinalities.id, types.id
    FROM struct_types, cardinalities, types
    WHERE struct_types.name = $2  AND cardinalities.name = $3 AND types.name = $4;")

(define add-union-type-member-query
  "INSERT INTO union_types (name, member_type)
    SELECT $1, id FROM types WHERE types.name = $2;")

(define add-type-query
  "INSERT INTO types (name, class)
    SELECT $1, id FROM type_classes WHERE type_classes.name = $2;")

(define update-string-type-query
  "UPDATE string_types SET pattern = $1 WHERE name = $2;")

(define update-number-type-query
  "UPDATE number_types SET minval = $1, maxval = $2, step = $3, digits = $4
   WHERE name = $5;")

(define update-number-type-min-query
  "UPDATE number_types SET minval = $1 WHERE name = $2;")

(define update-number-type-max-query
  "UPDATE number_types SET maxval = $1 WHERE name = $2;")

(define update-number-type-step-query
  "UPDATE number_types SET step = $1 WHERE name = $2;")

(define update-number-type-digits-query
  "UPDATE number_types SET digits = $1 WHERE name = $2;")

(define update-vocab-type-delete-term-query
  "DELETE FROM vocab_types WHERE name = $1 and term = $2;")

(define update-struct-type-extensible-query
  "UPDATE struct_types SET extensible = $1 WHERE name = $2;")

(define update-struct-type-description-query
  "UPDATE struct_types SET description = $1 WHERE name = $2;")

(define update-struct-member-query
  "UPDATE struct_type_members
   SET rel_name = $1, cardinality = $2, mem_type = $3
   WHERE struct_type = struct_types.id
   AND struct_types.name = $4 AND rel_name = $5;")

(define update-struct-member-type-query
  "UPDATE struct_type_members SET mem_type = types.id
   WHERE struct_type = struct_types.id AND struct_types.name = $1
   AND rel_name = $2 AND types.name = $3;")

(define update-struct-member-cardinality-query
  "UPDATE struct_type_members SET cardinality = cardinalities.id
   WHERE struct_type = struct_types.id AND struct_types.name = $1
   AND rel_name = $2 AND cardinalities.name = $3;")

(define update-struct-member-relname-query
  "UPDATE struct_type_members SET rel_name = $1
   WHERE struct_type = struct_types.id AND struct_types.name = $2
   AND rel_name = $3;")

(define update-union-type-delete-member-query
  "DELETE FROM union_types WHERE name = $1 and member_type = $2;")

(define delete-string-type-query
  "DELETE FROM string_types WHERE name = $1;")

(define delete-number-type-query
  "DELETE FROM number_types WHERE name = $1;")

(define delete-vocab-type-query
  "DELETE FROM vocab_types WHERE name = $1;")

(define delete-struct-type-query
  "DELETE FROM struct_types WHERE name = $1;")

(define delete-struct-member-query
  "DELETE FROM struct_type_members
   WHERE struct_type = struct_types.id AND struct_type.name = $1
   AND rel_name = $2;")

(define delete-struct-members-query
  "DELETE FROM struct_type_members
   WHERE struct_type = struct_types.id AND struct_type.name = $1;")

(define delete-union-type-query
  "DELETE FROM union_types WHERE name = $1;")

(define delete-type-query
  "DELETE FROM types WHERE name = $1;")

(define get-string-type-query
  "SELECT pattern FROM string_types WHERE name = $1;")

(define get-number-type-query
  "SELECT minval, maxval, step, digits
   FROM number_types WHERE name = $1;")

(define get-vocab-terms-query
  "SELECT term FROM vocab_types WHERE name = $1;")

(define get-struct-member-query
  "SELECT cardinality, mem_type FROM struct_type_members, struct_types
   WHERE struct_type = struct_types.id
   AND struct_types.name = $1 AND rel_name = $2;")

(define get-struct-type-query
  "SELECT extensible, rel_name, cardinalities.name as cardinality, types.name as mem_type
    FROM struct_types, struct_type_members, cardinalities, types
    WHERE struct_types.name = $1
      AND struct_type_members.struct_type = struct_types.id
      AND struct_type_members.cardinality = cardinalities.id
      AND struct_type_members.mem_type = types.id;")

(define get-union-type-members-query
  "SELECT types.name FROM union_types, types
    WHERE member_type = types.id AND union_types.name = $1;")

(define get-type-class-query
  "SELECT class FROM types WHERE name = $1;")

(define get-string-types-query
  "SELECT name FROM string_types;")

(define get-number-types-query
  "SELECT name FROM number_types;")

(define get-vocab-types-query
  "SELECT name FROM vocab_types;")

(define get-struct-types-query
  "SELECT name FROM struct_types;")

(define get-union-types-query
  "SELECT name FROM union_types;")

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  STATEMENT MANIPULATION  ------------------------------------------

(define add-statement-query
  "INSERT INTO statements (s, p, o, t) VALUES ($1, $2, $3, $4);")

(define delete-statements-s-query
  "DELETE FROM statements WHERE s = $1;")

(define delete-statements-p-query
  "DELETE FROM statements WHERE p = $1;")

(define delete-statements-o-query
  "DELETE FROM statements WHERE o = $1;")

(define delete-statements-t-query
  "DELETE FROM statements WHERE t = $1;")

(define delete-statements-sp-query
  "DELETE FROM statements WHERE s = $1 AND p = $2;")

(define delete-statements-so-query
  "DELETE FROM statements WHERE s = $1 AND o = $2;")

(define delete-statements-st-query
  "DELETE FROM statements WHERE s = $1 AND t = $2;")

(define delete-statements-po-query
  "DELETE FROM statements WHERE p = $1 AND o = $2;")

(define delete-statements-pt-query
  "DELETE FROM statements WHERE p = $1 AND t = $2;")

(define delete-statements-spo-query
  "DELETE FROM statements WHERE s = $1 AND p = $2 AND o = $3;")

(define delete-statements-spt-query
  "DELETE FROM statements WHERE s = $1 AND p = $2 AND t = $3;")

(define update-statement-object-query
  "UPDATE statements SET o = $1, t = $2, dt = datetime('now')  WHERE s = $3 AND p = $4 AND o = $5;")

(define exists-s-query
  "EXISTS (SELECT id FROM statements WHERE s = $1);") 

(define exists-p-query
  "EXISTS (SELECT id FROM statements WHERE p = $1);") 

(define exists-o-query
  "EXISTS (SELECT id FROM statements WHERE o = $1);") 

(define exists-t-query
  "EXISTS (SELECT id FROM statements WHERE t = $1);") 

(define exists-sp-query
  "EXISTS (SELECT id FROM statements WHERE s = $1 AND p = $2);") 

(define exists-so-query
  "EXISTS (SELECT id FROM statements WHERE s = $1 AND o = $2);") 

(define exists-st-query
  "EXISTS (SELECT id FROM statements WHERE s = $1 AND t = $2);") 

(define exists-po-query
  "EXISTS (SELECT id FROM statements WHERE p = $1 AND o = $2);") 

(define exists-pt-query
  "EXISTS (SELECT id FROM statements WHERE p = $1 AND t = $2);") 

(define exists-spo-query
  "EXISTS (SELECT id FROM statements WHERE s = $1 AND p = $2 AND o = $3);") 

(define exists-spt-query
  "EXISTS (SELECT id FROM statements WHERE s = $1 AND p = $2 AND t = $3);") 

(define get-statements-s-query
  "SELECT s, p, o, t FROM statements WHERE s = $1;")

(define get-statements-p-query
  "SELECT s, p, o, t FROM statements WHERE p = $1;")

(define get-statements-o-query
  "SELECT s, p, o, t FROM statements WHERE o = $1;")

(define get-statements-t-query
  "SELECT s, p, o, t FROM statements WHERE t = $1;")

(define get-statements-sp-query
  "SELECT s, p, o, t FROM statements WHERE s = $1 AND p = $2;")

(define get-statements-so-query
  "SELECT s, p, o, t FROM statements WHERE s = $1 AND o = $2;")

(define get-statements-st-query
  "SELECT s, p, o, t FROM statements WHERE s = $1 AND t = $2;")

(define get-statements-po-query
  "SELECT s, p, o, t FROM statements WHERE p = $1 AND o = $2;")

(define get-statements-pt-query
  "SELECT s, p, o, t FROM statements WHERE p = $1 AND t = $2;")

(define get-statements-spt-query
  "SELECT s, p, o, t FROM statements WHERE s = $1 AND p = $2 AND t = $3;")
