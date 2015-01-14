### sukkiri_db_sqlite.jl -- SQLite3 implementation for Sukkiri database layer
###   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
###   This program is open-source software, released under the BSD license.
###   See the accompanying LICENSE file for details.


module SukkiriDBSqlite
using SQLite

export SukkiriSqliteDB, connect, disconnect, setup_db

include("sukkiri_common_sql.jl")

type SukkiriSqliteDB
    file::String
    connection::Union(Nothing,SQLiteDB)
    SukkiriSqliteDB(filename) = new(filename, nothing)
end

function connect(db::SukkiriSqliteDB)
    if db.connection == nothing
        db.connection = SQLiteDB(db.file)
    end
end

function disconnect(db::SukkiriSqliteDB)
    if db.connection != nothing
        close(db.connection)
    end
    db.connection = nothing
end

### IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
### ----  DATABASE SETUP  --------------------------------------------------

function setup_db(db::SukkiriSqliteDB)
    connect(db)
    execute(db.connection, "BEGIN")
    for q = [create_primitive_table_query, create_string_type_table_query,
             create_number_type_table_query, create_vocab_table_query,
             create_cardinality_table_query, create_struct_type_table_query,
             create_type_class_table_query, create_types_table_query,
             create_union_type_table_query, create_struct_members_table_query,
             create_statement_table_query]
        execute(db.connection, q)
    end
    for qlist = (populate_primitive_table_queries, populate_cardinality_table_queries,
                 populate_type_class_table_queries, populate_types_table_queries,
                 populate_union_type_table_queries)
        for q = qlist
            execute(db.connection, q)
        end
    end
    execute(db.connection, "COMMIT")
    disconnect(db)
end

### OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

end
