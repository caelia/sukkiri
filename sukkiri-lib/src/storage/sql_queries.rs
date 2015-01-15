/* sql_queries.rs -- SQL queries for Sukkiri.
   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
   This program is open-source software, released under the BSD license.
   See the accompanying LICENSE file for details. */

// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
// -----  DATABASE SETUP  --------------------------------------------------

mod sqlite3_pgsql {
    pub const CREATE_PRIMITIVE_TABLE: &'static str =
      "CREATE TABLE primitives (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT UNIQUE NOT NULL
      );";
    
    pub const POPULATE_PRIMITIVE_TABLE_QQ: [&'static str; 11] =
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
        "INSERT INTO primitives (name) VALUES ('xref');"];
    
    pub const CREATE_STRING_TYPE_TABLE: &'static str =
      "CREATE TABLE string_types (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        pattern TEXT NOT NULL,
        description TEXT
      );";
    
    pub const CREATE_NUMBER_TYPE_TABLE: &'static str =
      "CREATE TABLE number_types (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        minval FLOAT,
        maxval FLOAT,
        step FLOAT,
        digits INTEGER,
        description TEXT
      );";
    
    pub const CREATE_VOCAB_TABLE: &'static str =
      "CREATE TABLE vocab_types (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        term TEXT NOT NULL
      );";
    
    pub const CREATE_CARDINALITY_TABLE: &'static str =
      "CREATE TABLE cardinalities (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL
      );";
    
    pub const POPULATE_CARDINALITY_TABLE_QQ: [&'static str; 4] =
       ["INSERT INTO cardinalities (name) VALUES ('one');",
        "INSERT INTO cardinalities (name) VALUES ('zoo');",
        "INSERT INTO cardinalities (name) VALUES ('zoma');",
        "INSERT INTO cardinalities (name) VALUES ('ooma');"];
    
    pub const CREATE_STRUCT_TYPE_TABLE: &'static str =
      "CREATE TABLE struct_types (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        extensible INTEGER default 0,
        description TEXT
      );";
    
    pub const CREATE_TYPE_CLASS_TABLE: &'static str =
      "CREATE TABLE type_classes (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL
      );";
    
    pub const POPULATE_TYPE_CLASS_TABLE_QQ: [&'static str; 6] =
       ["INSERT INTO type_classes (name) VALUES ('primitive');",
        "INSERT INTO type_classes (name) VALUES ('string');",
        "INSERT INTO type_classes (name) VALUES ('number');",
        "INSERT INTO type_classes (name) VALUES ('vocab');",
        "INSERT INTO type_classes (name) VALUES ('struct');",
        "INSERT INTO type_classes (name) VALUES ('union');"];
    
    pub const CREATE_TYPES_TABLE: &'static str =
      "CREATE TABLE types (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL UNIQUE,
        class INTEGER REFERENCES type_classes(id)
      );";
    
    pub const POPULATE_TYPES_TABLE_QQ: [&'static str; 12] =
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
         SELECT 'any', id FROM type_classes WHERE name = 'union';"];
    
    pub const CREATE_UNION_TYPE_TABLE: &'static str =
      "CREATE TABLE union_types (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        member_type INTEGER REFERENCES types(id)
      );";
    
    pub const POPULATE_UNION_TYPE_TABLE_QQ: [&'static str; 11] =
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
        "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'xref';"];
    
    pub const CREATE_STRUCT_MEMBERS_TABLE: &'static str =
      "CREATE TABLE struct_type_members (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        struct_type INTEGER REFERENCES struct_types(id),
        rel_name TEXT NOT NULL,
        cardinality INTEGER REFERENCES cardinalities(id),
        mem_type INTEGER REFERENCES types(id)
      );";
    
    pub const CREATE_STATEMENT_TABLE: &'static str =
      "CREATE TABLE statements (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        s  TEXT NOT NULL,
        p  TEXT NOT NULL,
        o  TEXT NOT NULL,
        t  INTEGER REFERENCES types(id) NOT NULL,
        dt TEXT DEFAULT CURRENT_TIMESTAMP
      );";
    
    // IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
    // -----  USER-DEFINED TYPE MANAGEMENT  ------------------------------------
    
    pub const ADD_STRING_TYPE: &'static str =
      "INSERT INTO string_types (name, pattern, description) VALUES ($1, $2, $3);";
    
    pub const ADD_NUMBER_TYPE: &'static str =
      "INSERT INTO number_types (name, minval, maxval, step, digits, description)
       VALUES ($1, $2, $3, $4, $5, $6);";
    
    pub const ADD_VOCAB_TYPE_TERM: &'static str =
      "INSERT INTO vocab_types (name, term) VALUES ($1, $2);";
    
    pub const ADD_STRUCT_TYPE: &'static str =
      "INSERT INTO struct_types (name, extensible, description) VALUES ($1, $2, $3);";
    
    pub const ADD_STRUCT_MEMBER: &'static str =
      "INSERT INTO struct_type_members (struct_type, rel_name, cardinality, mem_type)
        SELECT struct_types.id, $1, cardinalities.id, types.id
        FROM struct_types, cardinalities, types
        WHERE struct_types.name = $2  AND cardinalities.name = $3 AND types.name = $4;";
    
    pub const ADD_UNION_TYPE_MEMBER: &'static str =
      "INSERT INTO union_types (name, member_type)
        SELECT $1, id FROM types WHERE types.name = $2;";
    
    pub const ADD_TYPE: &'static str =
      "INSERT INTO types (name, class)
        SELECT $1, id FROM type_classes WHERE type_classes.name = $2;";
    
    pub const UPDATE_STRING_TYPE: &'static str =
      "UPDATE string_types SET pattern = $1 WHERE name = $2;";
    
    pub const UPDATE_NUMBER_TYPE: &'static str =
      "UPDATE number_types SET minval = $1, maxval = $2, step = $3, digits = $4
       WHERE name = $5;";
    
    pub const UPDATE_NUMBER_TYPE_MIN: &'static str =
      "UPDATE number_types SET minval = $1 WHERE name = $2;";
    
    pub const UPDATE_NUMBER_TYPE_MAX: &'static str =
      "UPDATE number_types SET maxval = $1 WHERE name = $2;";
    
    pub const UPDATE_NUMBER_TYPE_STEP: &'static str =
      "UPDATE number_types SET step = $1 WHERE name = $2;";
    
    pub const UPDATE_NUMBER_TYPE_DIGITS: &'static str =
      "UPDATE number_types SET digits = $1 WHERE name = $2;";
    
    pub const UPDATE_VOCAB_TYPE_DELETE_TERM: &'static str =
      "DELETE FROM vocab_types WHERE name = $1 and term = $2;";
    
    pub const UPDATE_STRUCT_TYPE_EXTENSIBLE: &'static str =
      "UPDATE struct_types SET extensible = $1 WHERE name = $2;";
    
    pub const UPDATE_STRUCT_TYPE_DESCRIPTION: &'static str =
      "UPDATE struct_types SET description = $1 WHERE name = $2;";
    
    pub const UPDATE_STRUCT_MEMBER: &'static str =
      "UPDATE struct_type_members
       SET rel_name = $1, cardinality = $2, mem_type = $3
       WHERE struct_type = struct_types.id
       AND struct_types.name = $4 AND rel_name = $5;";
    
    pub const UPDATE_STRUCT_MEMBER_TYPE: &'static str =
      "UPDATE struct_type_members SET mem_type = types.id
       WHERE struct_type = struct_types.id AND struct_types.name = $1
       AND rel_name = $2 AND types.name = $3;";
    
    pub const UPDATE_STRUCT_MEMBER_CARDINALITY: &'static str =
      "UPDATE struct_type_members SET cardinality = cardinalities.id
       WHERE struct_type = struct_types.id AND struct_types.name = $1
       AND rel_name = $2 AND cardinalities.name = $3;";
    
    pub const UPDATE_STRUCT_MEMBER_RELNAME: &'static str =
      "UPDATE struct_type_members SET rel_name = $1
       WHERE struct_type = struct_types.id AND struct_types.name = $2
       AND rel_name = $3;";
    
    pub const UPDATE_UNION_TYPE_DELETE_MEMBER: &'static str =
      "DELETE FROM union_types WHERE name = $1 and member_type = $2;";
    
    pub const DELETE_STRING_TYPE: &'static str =
      "DELETE FROM string_types WHERE name = $1;";
    
    pub const DELETE_NUMBER_TYPE: &'static str =
      "DELETE FROM number_types WHERE name = $1;";
    
    pub const DELETE_VOCAB_TYPE: &'static str =
      "DELETE FROM vocab_types WHERE name = $1;";
    
    pub const DELETE_STRUCT_TYPE: &'static str =
      "DELETE FROM struct_types WHERE name = $1;";
    
    pub const DELETE_STRUCT_MEMBER: &'static str =
      "DELETE FROM struct_type_members
       WHERE struct_type = struct_types.id AND struct_type.name = $1
       AND rel_name = $2;";
    
    pub const DELETE_STRUCT_MEMBERS: &'static str =
      "DELETE FROM struct_type_members
       WHERE struct_type = struct_types.id AND struct_type.name = $1;";
    
    pub const DELETE_UNION_TYPE: &'static str =
      "DELETE FROM union_types WHERE name = $1;";
    
    pub const DELETE_TYPE: &'static str =
      "DELETE FROM types WHERE name = $1;";
    
    pub const GET_STRING_TYPE: &'static str =
      "SELECT pattern FROM string_types WHERE name = $1;";
    
    pub const GET_NUMBER_TYPE: &'static str =
      "SELECT minval, maxval, step, digits
       FROM number_types WHERE name = $1;";
    
    pub const GET_VOCAB_TERMS: &'static str =
      "SELECT term FROM vocab_types WHERE name = $1;";
    
    pub const GET_STRUCT_MEMBER: &'static str =
      "SELECT cardinality, mem_type FROM struct_type_members, struct_types
       WHERE struct_type = struct_types.id
       AND struct_types.name = $1 AND rel_name = $2;";
    
    pub const GET_STRUCT_TYPE: &'static str =
      "SELECT extensible, rel_name, cardinalities.name as cardinality, types.name as mem_type
        FROM struct_types, struct_type_members, cardinalities, types
        WHERE struct_types.name = $1
          AND struct_type_members.struct_type = struct_types.id
          AND struct_type_members.cardinality = cardinalities.id
          AND struct_type_members.mem_type = types.id;";
    
    pub const GET_UNION_TYPE_MEMBERS: &'static str =
      "SELECT types.name FROM union_types, types
        WHERE member_type = types.id AND union_types.name = $1;";
    
    pub const GET_TYPE_CLASS: &'static str =
      "SELECT class FROM types WHERE name = $1;";
    
    pub const GET_STRING_TYPES: &'static str =
      "SELECT name FROM string_types;";
    
    pub const GET_NUMBER_TYPES: &'static str =
      "SELECT name FROM number_types;";
    
    pub const GET_VOCAB_TYPES: &'static str =
      "SELECT name FROM vocab_types;";
    
    pub const GET_STRUCT_TYPES: &'static str =
      "SELECT name FROM struct_types;";
    
    pub const GET_UNION_TYPES: &'static str =
      "SELECT name FROM union_types;";
    
    // IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
    // -----  STATEMENT MANIPULATION  ------------------------------------------
    
    pub const ADD_STATEMENT: &'static str =
      "INSERT INTO statements (s, p, o, t) VALUES ($1, $2, $3, $4);";
    
    pub const DELETE_STATEMENTS_S: &'static str =
      "DELETE FROM statements WHERE s = $1;";
    
    pub const DELETE_STATEMENTS_P: &'static str =
      "DELETE FROM statements WHERE p = $1;";
    
    pub const DELETE_STATEMENTS_O: &'static str =
      "DELETE FROM statements WHERE o = $1;";
    
    pub const DELETE_STATEMENTS_T: &'static str =
      "DELETE FROM statements WHERE t = $1;";
    
    pub const DELETE_STATEMENTS_SP: &'static str =
      "DELETE FROM statements WHERE s = $1 AND p = $2;";
    
    pub const DELETE_STATEMENTS_SO: &'static str =
      "DELETE FROM statements WHERE s = $1 AND o = $2;";
    
    pub const DELETE_STATEMENTS_ST: &'static str =
      "DELETE FROM statements WHERE s = $1 AND t = $2;";
    
    pub const DELETE_STATEMENTS_PO: &'static str =
      "DELETE FROM statements WHERE p = $1 AND o = $2;";
    
    pub const DELETE_STATEMENTS_PT: &'static str =
      "DELETE FROM statements WHERE p = $1 AND t = $2;";
    
    pub const DELETE_STATEMENTS_SPO: &'static str =
      "DELETE FROM statements WHERE s = $1 AND p = $2 AND o = $3;";
    
    pub const DELETE_STATEMENTS_SPT: &'static str =
      "DELETE FROM statements WHERE s = $1 AND p = $2 AND t = $3;";
    
    pub const UPDATE_STATEMENT_OBJECT: &'static str =
      "UPDATE statements SET o = $1, t = $2, dt = datetime('now')  WHERE s = $3 AND p = $4 AND o = $5;";
    
    pub const EXISTS_S: &'static str =
      "EXISTS (SELECT id FROM statements WHERE s = $1);";
    
    pub const EXISTS_P: &'static str =
      "EXISTS (SELECT id FROM statements WHERE p = $1);";
    
    pub const EXISTS_O: &'static str =
      "EXISTS (SELECT id FROM statements WHERE o = $1);";
    
    pub const EXISTS_T: &'static str =
      "EXISTS (SELECT id FROM statements WHERE t = $1);";
    
    pub const EXISTS_SP: &'static str =
      "EXISTS (SELECT id FROM statements WHERE s = $1 AND p = $2);";
    
    pub const EXISTS_SO: &'static str =
      "EXISTS (SELECT id FROM statements WHERE s = $1 AND o = $2);";
    
    pub const EXISTS_ST: &'static str =
      "EXISTS (SELECT id FROM statements WHERE s = $1 AND t = $2);";
    
    pub const EXISTS_PO: &'static str =
      "EXISTS (SELECT id FROM statements WHERE p = $1 AND o = $2);";
    
    pub const EXISTS_PT: &'static str =
      "EXISTS (SELECT id FROM statements WHERE p = $1 AND t = $2);";
    
    pub const EXISTS_SPO: &'static str =
      "EXISTS (SELECT id FROM statements WHERE s = $1 AND p = $2 AND o = $3);";
    
    pub const EXISTS_SPT: &'static str =
      "EXISTS (SELECT id FROM statements WHERE s = $1 AND p = $2 AND t = $3);";
    
    pub const GET_STATEMENTS_S: &'static str =
      "SELECT s, p, o, t FROM statements WHERE s = $1;";
    
    pub const GET_STATEMENTS_P: &'static str =
      "SELECT s, p, o, t FROM statements WHERE p = $1;";
    
    pub const GET_STATEMENTS_O: &'static str =
      "SELECT s, p, o, t FROM statements WHERE o = $1;";
    
    pub const GET_STATEMENTS_T: &'static str =
      "SELECT s, p, o, t FROM statements WHERE t = $1;";
    
    pub const GET_STATEMENTS_SP: &'static str =
      "SELECT s, p, o, t FROM statements WHERE s = $1 AND p = $2;";
    
    pub const GET_STATEMENTS_SO: &'static str =
      "SELECT s, p, o, t FROM statements WHERE s = $1 AND o = $2;";
    
    pub const GET_STATEMENTS_ST: &'static str =
      "SELECT s, p, o, t FROM statements WHERE s = $1 AND t = $2;";
    
    pub const GET_STATEMENTS_PO: &'static str =
      "SELECT s, p, o, t FROM statements WHERE p = $1 AND o = $2;";
    
    pub const GET_STATEMENTS_PT: &'static str =
      "SELECT s, p, o, t FROM statements WHERE p = $1 AND t = $2;";
    
    pub const GET_STATEMENTS_SPT: &'static str =
      "SELECT s, p, o, t FROM statements WHERE s = $1 AND p = $2 AND t = $3;";
}

pub mod sqlite3 {
    pub use super::sqlite3_pgsql::*;
}
