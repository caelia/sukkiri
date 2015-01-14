### sukkiri_common_sql.jl -- SQL queries that work for all databases.
###   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
###   This program is open-source software, released under the BSD license.
###   See the accompanying LICENSE file for details.

### IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
### ----  DATABASE SETUP  --------------------------------------------------

create_primitive_table_query =
  "CREATE TABLE primitives (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT UNIQUE NOT NULL
  );"

populate_primitive_table_queries =
   ["INSERT INTO primitives (name) VALUES ('integer');",
    "INSERT INTO primitives (name) VALUES ('float');",
    "INSERT INTO primitives (name) VALUES ('boolean');",
    "INSERT INTO primitives (name) VALUES ('string');",
    "INSERT INTO primitives (name) VALUES ('date');",
    "INSERT INTO primitives (name) VALUES ('time');",
    "INSERT INTO primitives (name) VALUES ('period');",
    "INSERT INTO primitives (name) VALUES ('nref');",
    "INSERT INTO primitives (name) VALUES ('rref');",
    "INSERT INTO primitives (name) VALUES ('sref');",
    "INSERT INTO primitives (name) VALUES ('xref');"]

create_string_type_table_query =
  "CREATE TABLE string_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    pattern TEXT NOT NULL,
    description TEXT
  );"

create_number_type_table_query =
  "CREATE TABLE number_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    minval FLOAT,
    maxval FLOAT,
    step FLOAT,
    digits INTEGER,
    description TEXT
  );"

create_vocab_table_query =
  "CREATE TABLE vocab_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    term TEXT NOT NULL
  );"

create_cardinality_table_query =
  "CREATE TABLE cardinalities (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
  );"

populate_cardinality_table_queries =
   ["INSERT INTO cardinalities (name) VALUES ('one');",
    "INSERT INTO cardinalities (name) VALUES ('zoo');",
    "INSERT INTO cardinalities (name) VALUES ('zoma');",
    "INSERT INTO cardinalities (name) VALUES ('ooma');"]

create_struct_type_table_query =
  "CREATE TABLE struct_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    extensible INTEGER default 0,
    description TEXT
  );"

create_type_class_table_query =
  "CREATE TABLE type_classes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
  );"

populate_type_class_table_queries =
   ["INSERT INTO type_classes (name) VALUES ('primitive');",
    "INSERT INTO type_classes (name) VALUES ('string');",
    "INSERT INTO type_classes (name) VALUES ('number');",
    "INSERT INTO type_classes (name) VALUES ('vocab');",
    "INSERT INTO type_classes (name) VALUES ('struct');",
    "INSERT INTO type_classes (name) VALUES ('union');"]

create_types_table_query =
  "CREATE TABLE types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    class INTEGER REFERENCES type_classes(id)
  );"

populate_types_table_queries =
   ["INSERT INTO types (name, class)
     SELECT 'integer', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'float', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'boolean', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'string', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'date', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'time', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'period', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'nref', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'rref', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'sref', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'xref', id FROM type_classes WHERE name = 'primitive';",
    "INSERT INTO types (name, class)
     SELECT 'any', id FROM type_classes WHERE name = 'union';"]

create_union_type_table_query =
  "CREATE TABLE union_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    member_type INTEGER REFERENCES types(id)
  );"

populate_union_type_table_queries =
   ["INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'integer';",
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'float';",
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'boolean';",
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'string';",
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'date';",
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'time';",
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'period';",
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'nref';",
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'rref';",
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'sref';",
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'xref';"]

create_struct_members_table_query =
  "CREATE TABLE struct_type_members (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    struct_type INTEGER REFERENCES struct_types(id),
    rel_name TEXT NOT NULL,
    cardinality INTEGER REFERENCES cardinalities(id),
    mem_type INTEGER REFERENCES types(id)
  );"

create_statement_table_query =
  "CREATE TABLE statements (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    s  TEXT NOT NULL,
    p  TEXT NOT NULL,
    o  TEXT NOT NULL,
    t  INTEGER REFERENCES types(id) NOT NULL,
    dt TEXT DEFAULT CURRENT_TIMESTAMP
  );"

### IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
### ----  USER-DEFINED TYPE MANAGEMENT  ------------------------------------

add_string_type_query =
  "INSERT INTO string_types (name, pattern, description) VALUES (\$1, \$2, \$3);"

add_number_type_query =
  "INSERT INTO number_types (name, minval, maxval, step, digits, description)
   VALUES (\$1, \$2, \$3, \$4, \$5, \$6);"

add_vocab_type_term_query =
  "INSERT INTO vocab_types (name, term) VALUES (\$1, \$2);"

add_struct_type_query =
  "INSERT INTO struct_types (name, extensible, description) VALUES (\$1, \$2, \$3);"

add_struct_member_query =
  "INSERT INTO struct_type_members (struct_type, rel_name, cardinality, mem_type)
    SELECT struct_types.id, \$1, cardinalities.id, types.id
    FROM struct_types, cardinalities, types
    WHERE struct_types.name = \$2  AND cardinalities.name = \$3 AND types.name = \$4;"

add_union_type_member_query =
  "INSERT INTO union_types (name, member_type)
    SELECT \$1, id FROM types WHERE types.name = \$2;"

add_type_query =
  "INSERT INTO types (name, class)
    SELECT \$1, id FROM type_classes WHERE type_classes.name = \$2;"

update_string_type_query =
  "UPDATE string_types SET pattern = \$1 WHERE name = \$2;"

update_number_type_query =
  "UPDATE number_types SET minval = \$1, maxval = \$2, step = \$3, digits = \$4
   WHERE name = \$5;"

update_number_type_min_query =
  "UPDATE number_types SET minval = \$1 WHERE name = \$2;"

update_number_type_max_query =
  "UPDATE number_types SET maxval = \$1 WHERE name = \$2;"

update_number_type_step_query =
  "UPDATE number_types SET step = \$1 WHERE name = \$2;"

update_number_type_digits_query =
  "UPDATE number_types SET digits = \$1 WHERE name = \$2;"

update_vocab_type_delete_term_query =
  "DELETE FROM vocab_types WHERE name = \$1 and term = \$2;"

update_struct_type_extensible_query =
  "UPDATE struct_types SET extensible = \$1 WHERE name = \$2;"

update_struct_type_description_query =
  "UPDATE struct_types SET description = \$1 WHERE name = \$2;"

update_struct_member_query =
  "UPDATE struct_type_members
   SET rel_name = \$1, cardinality = \$2, mem_type = \$3
   WHERE struct_type = struct_types.id
   AND struct_types.name = \$4 AND rel_name = \$5;"

update_struct_member_type_query =
  "UPDATE struct_type_members SET mem_type = types.id
   WHERE struct_type = struct_types.id AND struct_types.name = \$1
   AND rel_name = \$2 AND types.name = \$3;"

update_struct_member_cardinality_query =
  "UPDATE struct_type_members SET cardinality = cardinalities.id
   WHERE struct_type = struct_types.id AND struct_types.name = \$1
   AND rel_name = \$2 AND cardinalities.name = \$3;"

update_struct_member_relname_query =
  "UPDATE struct_type_members SET rel_name = \$1
   WHERE struct_type = struct_types.id AND struct_types.name = \$2
   AND rel_name = \$3;"

update_union_type_delete_member_query =
  "DELETE FROM union_types WHERE name = \$1 and member_type = \$2;"

delete_string_type_query =
  "DELETE FROM string_types WHERE name = \$1;"

delete_number_type_query =
  "DELETE FROM number_types WHERE name = \$1;"

delete_vocab_type_query =
  "DELETE FROM vocab_types WHERE name = \$1;"

delete_struct_type_query =
  "DELETE FROM struct_types WHERE name = \$1;"

delete_struct_member_query =
  "DELETE FROM struct_type_members
   WHERE struct_type = struct_types.id AND struct_type.name = \$1
   AND rel_name = \$2;"

delete_struct_members_query =
  "DELETE FROM struct_type_members
   WHERE struct_type = struct_types.id AND struct_type.name = \$1;"

delete_union_type_query =
  "DELETE FROM union_types WHERE name = \$1;"

delete_type_query =
  "DELETE FROM types WHERE name = \$1;"

get_string_type_query =
  "SELECT pattern FROM string_types WHERE name = \$1;"

get_number_type_query =
  "SELECT minval, maxval, step, digits
   FROM number_types WHERE name = \$1;"

get_vocab_terms_query =
  "SELECT term FROM vocab_types WHERE name = \$1;"

get_struct_member_query =
  "SELECT cardinality, mem_type FROM struct_type_members, struct_types
   WHERE struct_type = struct_types.id
   AND struct_types.name = \$1 AND rel_name = \$2;"

get_struct_type_query =
  "SELECT extensible, rel_name, cardinalities.name as cardinality, types.name as mem_type
    FROM struct_types, struct_type_members, cardinalities, types
    WHERE struct_types.name = \$1
      AND struct_type_members.struct_type = struct_types.id
      AND struct_type_members.cardinality = cardinalities.id
      AND struct_type_members.mem_type = types.id;"

get_union_type_members_query =
  "SELECT types.name FROM union_types, types
    WHERE member_type = types.id AND union_types.name = \$1;"

get_type_class_query =
  "SELECT class FROM types WHERE name = \$1;"

get_string_types_query =
  "SELECT name FROM string_types;"

get_number_types_query =
  "SELECT name FROM number_types;"

get_vocab_types_query =
  "SELECT name FROM vocab_types;"

get_struct_types_query =
  "SELECT name FROM struct_types;"

get_union_types_query =
  "SELECT name FROM union_types;"

### IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
### ----  STATEMENT MANIPULATION  ------------------------------------------

add_statement_query =
  "INSERT INTO statements (s, p, o, t) VALUES (\$1, \$2, \$3, \$4);"

delete_statements_s_query =
  "DELETE FROM statements WHERE s = \$1;"

delete_statements_p_query =
  "DELETE FROM statements WHERE p = \$1;"

delete_statements_o_query =
  "DELETE FROM statements WHERE o = \$1;"

delete_statements_t_query =
  "DELETE FROM statements WHERE t = \$1;"

delete_statements_sp_query =
  "DELETE FROM statements WHERE s = \$1 AND p = \$2;"

delete_statements_so_query =
  "DELETE FROM statements WHERE s = \$1 AND o = \$2;"

delete_statements_st_query =
  "DELETE FROM statements WHERE s = \$1 AND t = \$2;"

delete_statements_po_query =
  "DELETE FROM statements WHERE p = \$1 AND o = \$2;"

delete_statements_pt_query =
  "DELETE FROM statements WHERE p = \$1 AND t = \$2;"

delete_statements_spo_query =
  "DELETE FROM statements WHERE s = \$1 AND p = \$2 AND o = \$3;"

delete_statements_spt_query =
  "DELETE FROM statements WHERE s = \$1 AND p = \$2 AND t = \$3;"

update_statement_object_query =
  "UPDATE statements SET o = \$1, t = \$2, dt = datetime('now')  WHERE s = \$3 AND p = \$4 AND o = \$5;"

exists_s_query =
  "EXISTS (SELECT id FROM statements WHERE s = \$1);"

exists_p_query =
  "EXISTS (SELECT id FROM statements WHERE p = \$1);"

exists_o_query =
  "EXISTS (SELECT id FROM statements WHERE o = \$1);"

exists_t_query =
  "EXISTS (SELECT id FROM statements WHERE t = \$1);"

exists_sp_query =
  "EXISTS (SELECT id FROM statements WHERE s = \$1 AND p = \$2);"

exists_so_query =
  "EXISTS (SELECT id FROM statements WHERE s = \$1 AND o = \$2);"

exists_st_query =
  "EXISTS (SELECT id FROM statements WHERE s = \$1 AND t = \$2);"

exists_po_query =
  "EXISTS (SELECT id FROM statements WHERE p = \$1 AND o = \$2);"

exists_pt_query =
  "EXISTS (SELECT id FROM statements WHERE p = \$1 AND t = \$2);"

exists_spo_query =
  "EXISTS (SELECT id FROM statements WHERE s = \$1 AND p = \$2 AND o = \$3);"

exists_spt_query =
  "EXISTS (SELECT id FROM statements WHERE s = \$1 AND p = \$2 AND t = \$3);"

get_statements_s_query =
  "SELECT s, p, o, t FROM statements WHERE s = \$1;"

get_statements_p_query =
  "SELECT s, p, o, t FROM statements WHERE p = \$1;"

get_statements_o_query =
  "SELECT s, p, o, t FROM statements WHERE o = \$1;"

get_statements_t_query =
  "SELECT s, p, o, t FROM statements WHERE t = \$1;"

get_statements_sp_query =
  "SELECT s, p, o, t FROM statements WHERE s = \$1 AND p = \$2;"

get_statements_so_query =
  "SELECT s, p, o, t FROM statements WHERE s = \$1 AND o = \$2;"

get_statements_st_query =
  "SELECT s, p, o, t FROM statements WHERE s = \$1 AND t = \$2;"

get_statements_po_query =
  "SELECT s, p, o, t FROM statements WHERE p = \$1 AND o = \$2;"

get_statements_pt_query =
  "SELECT s, p, o, t FROM statements WHERE p = \$1 AND t = \$2;"

get_statements_spt_query =
  "SELECT s, p, o, t FROM statements WHERE s = \$1 AND p = \$2 AND t = \$3;"
