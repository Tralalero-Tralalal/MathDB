  open Parser
  open Pre_parser
  open Cabs
  open Uchar

let string_of_loc (loc : Lexing.position) =
  Printf.sprintf "line: %d, offset: %d" loc.pos_lnum (loc.pos_cnum - loc.pos_bol)

let convert_token = function
  | Pre_parser.SELECT loc -> Parser.SELECT loc
  | Pre_parser.EOF -> Parser.EOF ()
  | Pre_parser.ABORT loc -> Parser.ABORT loc
  | Pre_parser.ACTION loc -> Parser.ACTION loc
  | Pre_parser.ADD loc -> Parser.ADD loc
  | Pre_parser.AFTER loc -> Parser.AFTER loc
  | Pre_parser.ALL loc -> Parser.ALL loc
  | Pre_parser.ALTER loc -> Parser.ALTER loc
  | Pre_parser.ALWAYS loc -> Parser.ALWAYS loc
  | Pre_parser.ANALYZE loc -> Parser.ANALYZE loc
  | Pre_parser.AND loc -> Parser.AND loc
  | Pre_parser.AS loc -> Parser.AS loc
  | Pre_parser.ASC loc -> Parser.ASC loc
  | Pre_parser.ATTACH loc -> Parser.ATTACH loc
  | Pre_parser.AUTOINCREMENT loc -> Parser.AUTOINCREMENT loc
  | Pre_parser.BEFORE loc -> Parser.BEFORE loc
  | Pre_parser.BEGIN loc -> Parser.BEGIN loc
  | Pre_parser.BETWEEN loc -> Parser.BETWEEN loc
  | Pre_parser.BY loc -> Parser.BY loc
  | Pre_parser.CASCADE loc -> Parser.CASCADE loc
  | Pre_parser.CASE loc -> Parser.CASE loc
  | Pre_parser.CAST loc -> Parser.CAST loc
  | Pre_parser.CHECK loc -> Parser.CHECK loc
  | Pre_parser.COLLATE loc -> Parser.COLLATE loc
  | Pre_parser.COLUMN loc -> Parser.COLUMN loc
  | Pre_parser.COMMIT loc -> Parser.COMMIT loc
  | Pre_parser.CONFLICT loc -> Parser.CONFLICT loc
  | Pre_parser.CONSTRAINT loc -> Parser.CONSTRAINT loc
  | Pre_parser.CREATE loc -> Parser.CREATE loc
  | Pre_parser.CROSS loc -> Parser.CROSS loc
  | Pre_parser.CURRENT loc -> Parser.CURRENT loc
  | Pre_parser.CURRENT_DATE loc -> Parser.CURRENT_DATE loc
  | Pre_parser.CURRENT_TIME loc -> Parser.CURRENT_TIME loc
  | Pre_parser.CURRENT_TIMESTAMP loc -> Parser.CURRENT_TIMESTAMP loc
  | Pre_parser.DATABASE loc -> Parser.DATABASE loc
  | Pre_parser.DEFAULT loc -> Parser.DEFAULT loc
  | Pre_parser.DEFERRABLE loc -> Parser.DEFERRABLE loc
  | Pre_parser.DEFERRED loc -> Parser.DEFERRED loc
  | Pre_parser.DELETE loc -> Parser.DELETE loc
  | Pre_parser.DESC loc -> Parser.DESC loc
  | Pre_parser.DETACH loc -> Parser.DETACH loc
  | Pre_parser.DISTINCT loc -> Parser.DISTINCT loc
  | Pre_parser.DO loc -> Parser.DO loc
  | Pre_parser.DROP loc -> Parser.DROP loc
  | Pre_parser.EACH loc -> Parser.EACH loc
  | Pre_parser.ELSE loc -> Parser.ELSE loc
  | Pre_parser.END loc -> Parser.END loc
  | Pre_parser.ESCAPE loc -> Parser.ESCAPE loc
  | Pre_parser.EXCEPT loc -> Parser.EXCEPT loc
  | Pre_parser.EXCLUDE loc -> Parser.EXCLUDE loc
  | Pre_parser.EXCLUSIVE loc -> Parser.EXCLUSIVE loc
  | Pre_parser.EXISTS loc -> Parser.EXISTS loc
  | Pre_parser.EXPLAIN loc -> Parser.EXPLAIN loc
  | Pre_parser.FAIL loc -> Parser.FAIL loc
  | Pre_parser.FILTER loc -> Parser.FILTER loc
  | Pre_parser.FIRST loc -> Parser.FIRST loc
  | Pre_parser.FOLLOWING loc -> Parser.FOLLOWING loc
  | Pre_parser.FOR loc -> Parser.FOR loc
  | Pre_parser.FOREIGN loc -> Parser.FOREIGN loc
  | Pre_parser.FROM loc -> Parser.FROM loc
  | Pre_parser.FULL loc -> Parser.FULL loc
  | Pre_parser.GENERATED loc -> Parser.GENERATED loc
  | Pre_parser.GLOB loc -> Parser.GLOB loc
  | Pre_parser.GROUP loc -> Parser.GROUP loc
  | Pre_parser.GROUPS loc -> Parser.GROUPS loc
  | Pre_parser.HAVING loc -> Parser.HAVING loc
  | Pre_parser.IF loc -> Parser.IF loc
  | Pre_parser.IGNORE loc -> Parser.IGNORE loc
  | Pre_parser.IMMEDIATE loc -> Parser.IMMEDIATE loc
  | Pre_parser.IN loc -> Parser.IN loc
  | Pre_parser.INDEX loc -> Parser.INDEX loc
  | Pre_parser.INDEXED loc -> Parser.INDEXED loc
  | Pre_parser.INITIALLY loc -> Parser.INITIALLY loc
  | Pre_parser.INNER loc -> Parser.INNER loc
  | Pre_parser.INSERT loc -> Parser.INSERT loc
  | Pre_parser.INSTEAD loc -> Parser.INSTEAD loc
  | Pre_parser.INTERSECT loc -> Parser.INTERSECT loc
  | Pre_parser.INTO loc -> Parser.INTO loc
  | Pre_parser.IS loc -> Parser.IS loc
  | Pre_parser.ISNULL loc -> Parser.ISNULL loc
  | Pre_parser.JOIN loc -> Parser.JOIN loc
  | Pre_parser.KEY loc -> Parser.KEY loc
  | Pre_parser.LAST loc -> Parser.LAST loc
  | Pre_parser.LEFT loc -> Parser.LEFT loc
  | Pre_parser.LIKE loc -> Parser.LIKE loc
  | Pre_parser.LIMIT loc -> Parser.LIMIT loc
  | Pre_parser.MATCH loc -> Parser.MATCH loc
  | Pre_parser.MATERIALIZED loc -> Parser.MATERIALIZED loc
  | Pre_parser.MINUS loc -> Parser.MINUS loc
  | Pre_parser.STAR loc -> Parser.STAR loc
  | Pre_parser.SLASH loc -> Parser.SLASH loc
  | Pre_parser.PERCENT loc -> Parser.PERCENT loc
  | Pre_parser.EQUAL loc -> Parser.EQUAL loc
  | Pre_parser.DOUBLE_EQUAL loc -> Parser.DOUBLE_EQUAL loc
  | Pre_parser.BANG_EQUAL loc -> Parser.BANG_EQUAL loc
  | Pre_parser.NOT_EQUAL loc -> Parser.NOT_EQUAL loc
  | Pre_parser.LPAREN loc -> Parser.LPAREN loc
  | Pre_parser.RPAREN loc -> Parser.RPAREN loc
  | Pre_parser.LT loc -> Parser.LT loc
  | Pre_parser.LTE loc -> Parser.LTE loc
  | Pre_parser.GT loc -> Parser.GT loc
  | Pre_parser.GTE loc -> Parser.GTE loc
  | Pre_parser.AMP loc -> Parser.AMP loc
  | Pre_parser.BAR loc -> Parser.BAR loc
  | Pre_parser.LSHIFT loc -> Parser.LSHIFT loc
  | Pre_parser.RSHIFT loc -> Parser.RSHIFT loc
  | Pre_parser.TILDE loc -> Parser.TILDE loc
  | Pre_parser.COMMA loc -> Parser.COMMA loc
  | Pre_parser.SEMICOLON loc -> Parser.SEMICOLON loc
  | Pre_parser.DOT loc -> Parser.DOT loc
  | Pre_parser.PLUS loc -> Parser.PLUS loc
  | Pre_parser.NATURAL loc -> Parser.NATURAL loc
  | Pre_parser.NO loc -> Parser.NO loc
  | Pre_parser.NOT loc -> Parser.NOT loc
  | Pre_parser.NOTHING loc -> Parser.NOTHING loc
  | Pre_parser.NOTNULL loc -> Parser.NOTNULL loc
  | Pre_parser.NULL loc -> Parser.NULL loc
  | Pre_parser.NULLS loc -> Parser.NULLS loc
  | Pre_parser.OF loc -> Parser.OF loc
  | Pre_parser.OFFSET loc -> Parser.OFFSET loc
  | Pre_parser.ON loc -> Parser.ON loc
  | Pre_parser.OR loc -> Parser.OR loc
  | Pre_parser.ORDER loc -> Parser.ORDER loc
  | Pre_parser.OTHERS loc -> Parser.OTHERS loc
  | Pre_parser.OUTER loc -> Parser.OUTER loc
  | Pre_parser.OVER loc -> Parser.OVER loc
  | Pre_parser.PARTITION loc -> Parser.PARTITION loc
  | Pre_parser.PLAN loc -> Parser.PLAN loc
  | Pre_parser.PRAGMA loc -> Parser.PRAGMA loc
  | Pre_parser.PRECEDING loc -> Parser.PRECEDING loc
  | Pre_parser.PRIMARY loc -> Parser.PRIMARY loc
  | Pre_parser.QUERY loc -> Parser.QUERY loc
  | Pre_parser.RAISE loc -> Parser.RAISE loc
  | Pre_parser.RANGE loc -> Parser.RANGE loc
  | Pre_parser.RECURSIVE loc -> Parser.RECURSIVE loc
  | Pre_parser.REFERENCES loc -> Parser.REFERENCES loc
  | Pre_parser.REGEXP loc -> Parser.REGEXP loc
  | Pre_parser.REINDEX loc -> Parser.REINDEX loc
  | Pre_parser.RELEASE loc -> Parser.RELEASE loc
  | Pre_parser.RENAME loc -> Parser.RENAME loc
  | Pre_parser.REPLACE loc -> Parser.REPLACE loc
  | Pre_parser.RESTRICT loc -> Parser.RESTRICT loc
  | Pre_parser.RETURNING loc -> Parser.RETURNING loc
  | Pre_parser.RIGHT loc -> Parser.RIGHT loc
  | Pre_parser.ROLLBACK loc -> Parser.ROLLBACK loc
  | Pre_parser.ROW loc -> Parser.ROW loc
  | Pre_parser.ROWS loc -> Parser.ROWS loc
  | Pre_parser.SAVEPOINT loc -> Parser.SAVEPOINT loc
  | Pre_parser.SET loc -> Parser.SET loc
  | Pre_parser.TABLE loc -> Parser.TABLE loc
  | Pre_parser.TEMP loc -> Parser.TEMP loc
  | Pre_parser.TEMPORARY loc -> Parser.TEMPORARY loc
  | Pre_parser.THEN loc -> Parser.THEN loc
  | Pre_parser.TIES loc -> Parser.TIES loc
  | Pre_parser.TO loc -> Parser.TO loc
  | Pre_parser.TRANSACTION loc -> Parser.TRANSACTION loc
  | Pre_parser.TRIGGER loc -> Parser.TRIGGER loc
  | Pre_parser.UNBOUNDED loc -> Parser.UNBOUNDED loc
  | Pre_parser.UNION loc -> Parser.UNION loc
  | Pre_parser.UNIQUE loc -> Parser.UNIQUE loc
  | Pre_parser.UPDATE loc -> Parser.UPDATE loc
  | Pre_parser.USING loc -> Parser.USING loc
  | Pre_parser.VACUUM loc -> Parser.VACUUM loc
  | Pre_parser.VALUES loc -> Parser.VALUES loc
  | Pre_parser.VIEW loc -> Parser.VIEW loc
  | Pre_parser.VIRTUAL loc -> Parser.VIRTUAL loc
  | Pre_parser.WHEN loc -> Parser.WHEN loc
  | Pre_parser.WHERE loc -> Parser.WHERE loc
  | Pre_parser.WINDOW loc -> Parser.WINDOW loc
  | Pre_parser.WITH loc -> Parser.WITH loc
  | Pre_parser.WITHOUT loc -> Parser.WITHOUT loc
  | Pre_parser.IDENT (id, loc) -> Parser.IDENT (id, loc)
  | Pre_parser.INT_LIT (i, loc) -> Parser.INT_LIT (i, loc)
  | Pre_parser.FLOAT_LIT (f, loc) -> Parser.FLOAT_LIT (f, loc)
  | Pre_parser.STRING_LIT (s, loc) -> Parser.STRING_LIT (s, loc)
  | Pre_parser.BLOB (s, loc) -> Parser.BLOB (s, loc)

let string_of_token = function
  | Parser.SELECT loc -> Printf.sprintf "SELECT, loc: %s" (string_of_loc loc)
  | Parser.EOF () -> "EOF"
  | Parser.ABORT loc -> Printf.sprintf "ABORT, loc: %s" (string_of_loc loc)
  | Parser.ACTION loc -> Printf.sprintf "ACTION, loc: %s" (string_of_loc loc)
  | Parser.ADD loc -> Printf.sprintf "ADD, loc: %s" (string_of_loc loc)
  | Parser.AFTER loc -> Printf.sprintf "AFTER, loc: %s" (string_of_loc loc)
  | Parser.ALL loc -> Printf.sprintf "ALL, loc: %s" (string_of_loc loc)
  | Parser.ALTER loc -> Printf.sprintf "ALTER, loc: %s" (string_of_loc loc)
  | Parser.ALWAYS loc -> Printf.sprintf "ALWAYS, loc: %s" (string_of_loc loc)
  | Parser.ANALYZE loc -> Printf.sprintf "ANALYZE, loc: %s" (string_of_loc loc)
  | Parser.AND loc -> Printf.sprintf "AND, loc: %s" (string_of_loc loc)
  | Parser.AS loc -> Printf.sprintf "AS, loc: %s" (string_of_loc loc)
  | Parser.ASC loc -> Printf.sprintf "ASC, loc: %s" (string_of_loc loc)
  | Parser.ATTACH loc -> Printf.sprintf "ATTACH, loc: %s" (string_of_loc loc)
  | Parser.AUTOINCREMENT loc -> Printf.sprintf "AUTOINCREMENT, loc: %s" (string_of_loc loc)
  | Parser.BEFORE loc -> Printf.sprintf "BEFORE, loc: %s" (string_of_loc loc)
  | Parser.BEGIN loc -> Printf.sprintf "BEGIN, loc: %s" (string_of_loc loc)
  | Parser.BETWEEN loc -> Printf.sprintf "BETWEEN, loc: %s" (string_of_loc loc)
  | Parser.BY loc -> Printf.sprintf "BY, loc: %s" (string_of_loc loc)
  | Parser.CASCADE loc -> Printf.sprintf "CASCADE, loc: %s" (string_of_loc loc)
  | Parser.CASE loc -> Printf.sprintf "CASE, loc: %s" (string_of_loc loc)
  | Parser.CAST loc -> Printf.sprintf "CAST, loc: %s" (string_of_loc loc)
  | Parser.CHECK loc -> Printf.sprintf "CHECK, loc: %s" (string_of_loc loc)
  | Parser.COLLATE loc -> Printf.sprintf "COLLATE, loc: %s" (string_of_loc loc)
  | Parser.COLUMN loc -> Printf.sprintf "COLUMN, loc: %s" (string_of_loc loc)
  | Parser.COMMIT loc -> Printf.sprintf "COMMIT, loc: %s" (string_of_loc loc)
  | Parser.CONFLICT loc -> Printf.sprintf "CONFLICT, loc: %s" (string_of_loc loc)
  | Parser.CONSTRAINT loc -> Printf.sprintf "CONSTRAINT, loc: %s" (string_of_loc loc)
  | Parser.CREATE loc -> Printf.sprintf "CREATE, loc: %s" (string_of_loc loc)
  | Parser.CROSS loc -> Printf.sprintf "CROSS, loc: %s" (string_of_loc loc)
  | Parser.CURRENT loc -> Printf.sprintf "CURRENT, loc: %s" (string_of_loc loc)
  | Parser.CURRENT_DATE loc -> Printf.sprintf "CURRENT_DATE, loc: %s" (string_of_loc loc)
  | Parser.CURRENT_TIME loc -> Printf.sprintf "CURRENT_TIME, loc: %s" (string_of_loc loc)
  | Parser.CURRENT_TIMESTAMP loc -> Printf.sprintf "CURRENT_TIMESTAMP, loc: %s" (string_of_loc loc)
  | Parser.DATABASE loc -> Printf.sprintf "DATABASE, loc: %s" (string_of_loc loc)
  | Parser.DEFAULT loc -> Printf.sprintf "DEFAULT, loc: %s" (string_of_loc loc)
  | Parser.DEFERRABLE loc -> Printf.sprintf "DEFERRABLE, loc: %s" (string_of_loc loc)
  | Parser.DEFERRED loc -> Printf.sprintf "DEFERRED, loc: %s" (string_of_loc loc)
  | Parser.DELETE loc -> Printf.sprintf "DELETE, loc: %s" (string_of_loc loc)
  | Parser.DESC loc -> Printf.sprintf "DESC, loc: %s" (string_of_loc loc)
  | Parser.DETACH loc -> Printf.sprintf "DETACH, loc: %s" (string_of_loc loc)
  | Parser.DISTINCT loc -> Printf.sprintf "DISTINCT, loc: %s" (string_of_loc loc)
  | Parser.DO loc -> Printf.sprintf "DO, loc: %s" (string_of_loc loc)
  | Parser.DROP loc -> Printf.sprintf "DROP, loc: %s" (string_of_loc loc)
  | Parser.EACH loc -> Printf.sprintf "EACH, loc: %s" (string_of_loc loc)
  | Parser.ELSE loc -> Printf.sprintf "ELSE, loc: %s" (string_of_loc loc)
  | Parser.END loc -> Printf.sprintf "END, loc: %s" (string_of_loc loc)
  | Parser.ESCAPE loc -> Printf.sprintf "ESCAPE, loc: %s" (string_of_loc loc)
  | Parser.EXCEPT loc -> Printf.sprintf "EXCEPT, loc: %s" (string_of_loc loc)
  | Parser.EXCLUDE loc -> Printf.sprintf "EXCLUDE, loc: %s" (string_of_loc loc)
  | Parser.EXCLUSIVE loc -> Printf.sprintf "EXCLUSIVE, loc: %s" (string_of_loc loc)
  | Parser.EXISTS loc -> Printf.sprintf "EXISTS, loc: %s" (string_of_loc loc)
  | Parser.EXPLAIN loc -> Printf.sprintf "EXPLAIN, loc: %s" (string_of_loc loc)
  | Parser.FAIL loc -> Printf.sprintf "FAIL, loc: %s" (string_of_loc loc)
  | Parser.FILTER loc -> Printf.sprintf "FILTER, loc: %s" (string_of_loc loc)
  | Parser.FIRST loc -> Printf.sprintf "FIRST, loc: %s" (string_of_loc loc)
  | Parser.FOLLOWING loc -> Printf.sprintf "FOLLOWING, loc: %s" (string_of_loc loc)
  | Parser.FOR loc -> Printf.sprintf "FOR, loc: %s" (string_of_loc loc)
  | Parser.FOREIGN loc -> Printf.sprintf "FOREIGN, loc: %s" (string_of_loc loc)
  | Parser.FROM loc -> Printf.sprintf "FROM, loc: %s" (string_of_loc loc)
  | Parser.FULL loc -> Printf.sprintf "FULL, loc: %s" (string_of_loc loc)
  | Parser.GENERATED loc -> Printf.sprintf "GENERATED, loc: %s" (string_of_loc loc)
  | Parser.GLOB loc -> Printf.sprintf "GLOB, loc: %s" (string_of_loc loc)
  | Parser.GROUP loc -> Printf.sprintf "GROUP, loc: %s" (string_of_loc loc)
  | Parser.GROUPS loc -> Printf.sprintf "GROUPS, loc: %s" (string_of_loc loc)
  | Parser.HAVING loc -> Printf.sprintf "HAVING, loc: %s" (string_of_loc loc)
  | Parser.IF loc -> Printf.sprintf "IF, loc: %s" (string_of_loc loc)
  | Parser.IGNORE loc -> Printf.sprintf "IGNORE, loc: %s" (string_of_loc loc)
  | Parser.IMMEDIATE loc -> Printf.sprintf "IMMEDIATE, loc: %s" (string_of_loc loc)
  | Parser.IN loc -> Printf.sprintf "IN, loc: %s" (string_of_loc loc)
  | Parser.INDEX loc -> Printf.sprintf "INDEX, loc: %s" (string_of_loc loc)
  | Parser.INDEXED loc -> Printf.sprintf "INDEXED, loc: %s" (string_of_loc loc)
  | Parser.INITIALLY loc -> Printf.sprintf "INITIALLY, loc: %s" (string_of_loc loc)
  | Parser.INNER loc -> Printf.sprintf "INNER, loc: %s" (string_of_loc loc)
  | Parser.INSERT loc -> Printf.sprintf "INSERT, loc: %s" (string_of_loc loc)
  | Parser.INSTEAD loc -> Printf.sprintf "INSTEAD, loc: %s" (string_of_loc loc)
  | Parser.INTERSECT loc -> Printf.sprintf "INTERSECT, loc: %s" (string_of_loc loc)
  | Parser.INTO loc -> Printf.sprintf "INTO, loc: %s" (string_of_loc loc)
  | Parser.IS loc -> Printf.sprintf "IS, loc: %s" (string_of_loc loc)
  | Parser.ISNULL loc -> Printf.sprintf "ISNULL, loc: %s" (string_of_loc loc)
  | Parser.JOIN loc -> Printf.sprintf "JOIN, loc: %s" (string_of_loc loc)
  | Parser.KEY loc -> Printf.sprintf "KEY, loc: %s" (string_of_loc loc)
  | Parser.LAST loc -> Printf.sprintf "LAST, loc: %s" (string_of_loc loc)
  | Parser.LEFT loc -> Printf.sprintf "LEFT, loc: %s" (string_of_loc loc)
  | Parser.LIKE loc -> Printf.sprintf "LIKE, loc: %s" (string_of_loc loc)
  | Parser.LIMIT loc -> Printf.sprintf "LIMIT, loc: %s" (string_of_loc loc)
  | Parser.MATCH loc -> Printf.sprintf "MATCH, loc: %s" (string_of_loc loc)
  | Parser.MATERIALIZED loc -> Printf.sprintf "MATERIALIZED, loc: %s" (string_of_loc loc)
  | Parser.MINUS loc -> Printf.sprintf "MINUS, loc: %s" (string_of_loc loc)
  | Parser.STAR loc -> Printf.sprintf "STAR, loc: %s" (string_of_loc loc)
  | Parser.SLASH loc -> Printf.sprintf "SLASH, loc: %s" (string_of_loc loc)
  | Parser.PERCENT loc -> Printf.sprintf "PERCENT, loc: %s" (string_of_loc loc)
  | Parser.EQUAL loc -> Printf.sprintf "EQUAL, loc: %s" (string_of_loc loc)
  | Parser.DOUBLE_EQUAL loc -> Printf.sprintf "DOUBLE_EQUAL, loc: %s" (string_of_loc loc)
  | Parser.BANG_EQUAL loc -> Printf.sprintf "BANG_EQUAL, loc: %s" (string_of_loc loc)
  | Parser.NOT_EQUAL loc -> Printf.sprintf "NOT_EQUAL, loc: %s" (string_of_loc loc)
  | Parser.LPAREN loc -> Printf.sprintf "LPAREN, loc: %s" (string_of_loc loc)
  | Parser.RPAREN loc -> Printf.sprintf "RPAREN, loc: %s" (string_of_loc loc)
  | Parser.LT loc -> Printf.sprintf "LT, loc: %s" (string_of_loc loc)
  | Parser.LTE loc -> Printf.sprintf "LTE, loc: %s" (string_of_loc loc)
  | Parser.GT loc -> Printf.sprintf "GT, loc: %s" (string_of_loc loc)
  | Parser.GTE loc -> Printf.sprintf "GTE, loc: %s" (string_of_loc loc)
  | Parser.AMP loc -> Printf.sprintf "AMP, loc: %s" (string_of_loc loc)
  | Parser.BAR loc -> Printf.sprintf "BAR, loc: %s" (string_of_loc loc)
  | Parser.LSHIFT loc -> Printf.sprintf "LSHIFT, loc: %s" (string_of_loc loc)
  | Parser.RSHIFT loc -> Printf.sprintf "RSHIFT, loc: %s" (string_of_loc loc)
  | Parser.TILDE loc -> Printf.sprintf "TILDE, loc: %s" (string_of_loc loc)
  | Parser.COMMA loc -> Printf.sprintf "COMMA, loc: %s" (string_of_loc loc)
  | Parser.SEMICOLON loc -> Printf.sprintf "SEMICOLON, loc: %s" (string_of_loc loc)
  | Parser.DOT loc -> Printf.sprintf "DOT, loc: %s" (string_of_loc loc)
  | Parser.PLUS loc -> Printf.sprintf "PLUS, loc: %s" (string_of_loc loc)
  | Parser.MATCH loc -> Printf.sprintf "MATCH, loc: %s" (string_of_loc loc)
  | Parser.MATERIALIZED loc -> Printf.sprintf "MATERIALIZED, loc: %s" (string_of_loc loc)
  | Parser.NATURAL loc -> Printf.sprintf "NATURAL, loc: %s" (string_of_loc loc)
  | Parser.NO loc -> Printf.sprintf "NO, loc: %s" (string_of_loc loc)
  | Parser.NOT loc -> Printf.sprintf "NOT, loc: %s" (string_of_loc loc)
  | Parser.NOTHING loc -> Printf.sprintf "NOTHING, loc: %s" (string_of_loc loc)
  | Parser.NOTNULL loc -> Printf.sprintf "NOTNULL, loc: %s" (string_of_loc loc)
  | Parser.NULL loc -> Printf.sprintf "NULL, loc: %s" (string_of_loc loc)
  | Parser.NULLS loc -> Printf.sprintf "NULLS, loc: %s" (string_of_loc loc)
  | Parser.OF loc -> Printf.sprintf "OF, loc: %s" (string_of_loc loc)
  | Parser.OFFSET loc -> Printf.sprintf "OFFSET, loc: %s" (string_of_loc loc)
  | Parser.ON loc -> Printf.sprintf "ON, loc: %s" (string_of_loc loc)
  | Parser.OR loc -> Printf.sprintf "OR, loc: %s" (string_of_loc loc)
  | Parser.ORDER loc -> Printf.sprintf "ORDER, loc: %s" (string_of_loc loc)
  | Parser.OTHERS loc -> Printf.sprintf "OTHERS, loc: %s" (string_of_loc loc)
  | Parser.OUTER loc -> Printf.sprintf "OUTER, loc: %s" (string_of_loc loc)
  | Parser.OVER loc -> Printf.sprintf "OVER, loc: %s" (string_of_loc loc)
  | Parser.PARTITION loc -> Printf.sprintf "PARTITION, loc: %s" (string_of_loc loc)
  | Parser.PLAN loc -> Printf.sprintf "PLAN, loc: %s" (string_of_loc loc)
  | Parser.PRAGMA loc -> Printf.sprintf "PRAGMA, loc: %s" (string_of_loc loc)
  | Parser.PRECEDING loc -> Printf.sprintf "PRECEDING, loc: %s" (string_of_loc loc)
  | Parser.PRIMARY loc -> Printf.sprintf "PRIMARY, loc: %s" (string_of_loc loc)
  | Parser.QUERY loc -> Printf.sprintf "QUERY, loc: %s" (string_of_loc loc)
  | Parser.RAISE loc -> Printf.sprintf "RAISE, loc: %s" (string_of_loc loc)
  | Parser.RANGE loc -> Printf.sprintf "RANGE, loc: %s" (string_of_loc loc)
  | Parser.RECURSIVE loc -> Printf.sprintf "RECURSIVE, loc: %s" (string_of_loc loc)
  | Parser.REFERENCES loc -> Printf.sprintf "REFERENCES, loc: %s" (string_of_loc loc)
  | Parser.REGEXP loc -> Printf.sprintf "REGEXP, loc: %s" (string_of_loc loc)
  | Parser.REINDEX loc -> Printf.sprintf "REINDEX, loc: %s" (string_of_loc loc)
  | Parser.RELEASE loc -> Printf.sprintf "RELEASE, loc: %s" (string_of_loc loc)
  | Parser.RENAME loc -> Printf.sprintf "RENAME, loc: %s" (string_of_loc loc)
  | Parser.REPLACE loc -> Printf.sprintf "REPLACE, loc: %s" (string_of_loc loc)
  | Parser.RESTRICT loc -> Printf.sprintf "RESTRICT, loc: %s" (string_of_loc loc)
  | Parser.RETURNING loc -> Printf.sprintf "RETURNING, loc: %s" (string_of_loc loc)
  | Parser.RIGHT loc -> Printf.sprintf "RIGHT, loc: %s" (string_of_loc loc)
  | Parser.ROLLBACK loc -> Printf.sprintf "ROLLBACK, loc: %s" (string_of_loc loc)
  | Parser.ROW loc -> Printf.sprintf "ROW, loc: %s" (string_of_loc loc)
  | Parser.ROWS loc -> Printf.sprintf "ROWS, loc: %s" (string_of_loc loc)
  | Parser.SAVEPOINT loc -> Printf.sprintf "SAVEPOINT, loc: %s" (string_of_loc loc)
  | Parser.SET loc -> Printf.sprintf "SET, loc: %s" (string_of_loc loc)
  | Parser.TABLE loc -> Printf.sprintf "TABLE, loc: %s" (string_of_loc loc)
  | Parser.TEMP loc -> Printf.sprintf "TEMP, loc: %s" (string_of_loc loc)
  | Parser.TEMPORARY loc -> Printf.sprintf "TEMPORARY, loc: %s" (string_of_loc loc)
  | Parser.THEN loc -> Printf.sprintf "THEN, loc: %s" (string_of_loc loc)
  | Parser.TIES loc -> Printf.sprintf "TIES, loc: %s" (string_of_loc loc)
  | Parser.TO loc -> Printf.sprintf "TO, loc: %s" (string_of_loc loc)
  | Parser.TRANSACTION loc -> Printf.sprintf "TRANSACTION, loc: %s" (string_of_loc loc)
  | Parser.TRIGGER loc -> Printf.sprintf "TRIGGER, loc: %s" (string_of_loc loc)
  | Parser.UNBOUNDED loc -> Printf.sprintf "UNBOUNDED, loc: %s" (string_of_loc loc)
  | Parser.UNION loc -> Printf.sprintf "UNION, loc: %s" (string_of_loc loc)
  | Parser.UNIQUE loc -> Printf.sprintf "UNIQUE, loc: %s" (string_of_loc loc)
  | Parser.UPDATE loc -> Printf.sprintf "UPDATE, loc: %s" (string_of_loc loc)
  | Parser.USING loc -> Printf.sprintf "USING, loc: %s" (string_of_loc loc)
  | Parser.VACUUM loc -> Printf.sprintf "VACUUM, loc: %s" (string_of_loc loc)
  | Parser.VALUES loc -> Printf.sprintf "VALUES, loc: %s" (string_of_loc loc)
  | Parser.VIEW loc -> Printf.sprintf "VIEW, loc: %s" (string_of_loc loc)
  | Parser.VIRTUAL loc -> Printf.sprintf "VIRTUAL, loc: %s" (string_of_loc loc)
  | Parser.WHEN loc -> Printf.sprintf "WHEN, loc: %s" (string_of_loc loc)
  | Parser.WHERE loc -> Printf.sprintf "WHERE, loc: %s" (string_of_loc loc)
  | Parser.WINDOW loc -> Printf.sprintf "WINDOW, loc: %s" (string_of_loc loc)
  | Parser.WITH loc -> Printf.sprintf "WITH, loc: %s" (string_of_loc loc)
  | Parser.WITHOUT loc -> Printf.sprintf "WITHOUT, loc: %s" (string_of_loc loc)
  | Parser.IDENT (x, loc) -> Printf.sprintf "IDENT(%s), loc: %s" x (string_of_loc loc)
  | Parser.STRING_LIT (x, loc) -> Printf.sprintf "STRING_LIT(%s), loc: %s" x (string_of_loc loc)
  | Parser.INT_LIT (x, loc) -> Printf.sprintf "INT_LIT(%s), loc: %s" x (string_of_loc loc)
  | Parser.FLOAT_LIT (x, loc) -> Printf.sprintf "FLOAT_LIT(%s), loc: %s" x (string_of_loc loc)
  | Parser.BLOB (x, loc) -> Printf.sprintf "BLOB(%s), loc: %s" x (string_of_loc loc)

open Format

let rec print_program fmt = function
  | Cabs.PROGRAM stmts -> print_sql_stmt fmt stmts

and print_sql_stmt fmt = function
  | Cabs.INSERT_STMT (Cabs.WITH_INSERT (const1, const2)) ->
    Format.fprintf fmt "INSERT with %s and %s\n" (print_const const1) (print_const const2)
  | _ -> Format.fprintf fmt "Unknown SQL statement\n" (* Added a default case *)

and print_const = function
  | Cabs.EXPR_LIT s -> s
  | _ -> "Unknown constant" (* Added a default case *)
