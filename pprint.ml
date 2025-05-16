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
  | Pre_parser.STRING_LIT (f, loc) -> Parser.STRING_LIT (f, loc)

let string_of_token = function
  | Parser.SELECT loc -> Printf.sprintf "SELECT, loc: %s" (string_of_loc loc)
  | Parser.EOF () -> "EOF"
  | Parser.ABORT _ -> "ABORT"
  | Parser.ACTION _ -> "ACTION"
  | Parser.ADD _ -> "ADD"
  | Parser.AFTER _ -> "AFTER"
  | Parser.ALL _ -> "ALL"
  | Parser.ALTER _ -> "ALTER"
  | Parser.ALWAYS _ -> "ALWAYS"
  | Parser.ANALYZE _ -> "ANALYZE"
  | Parser.AND _ -> "AND"
  | Parser.AS _ -> "AS"
  | Parser.ASC _ -> "ASC"
  | Parser.ATTACH _ -> "ATTACH"
  | Parser.AUTOINCREMENT _ -> "AUTOINCREMENT"
  | Parser.BEFORE _ -> "BEFORE"
  | Parser.BEGIN _ -> "BEGIN"
  | Parser.BETWEEN _ -> "BETWEEN"
  | Parser.BY _ -> "BY"
  | Parser.CASCADE _ -> "CASCADE"
  | Parser.CASE _ -> "CASE"
  | Parser.CAST _ -> "CAST"
  | Parser.CHECK _ -> "CHECK"
  | Parser.COLLATE _ -> "COLLATE"
  | Parser.COLUMN _ -> "COLUMN"
  | Parser.COMMIT _ -> "COMMIT"
  | Parser.CONFLICT _ -> "CONFLICT"
  | Parser.CONSTRAINT _ -> "CONSTRAINT"
  | Parser.CREATE _ -> "CREATE"
  | Parser.CROSS _ -> "CROSS"
  | Parser.CURRENT _ -> "CURRENT"
  | Parser.CURRENT_DATE _ -> "CURRENT_DATE"
  | Parser.CURRENT_TIME _ -> "CURRENT_TIME"
  | Parser.CURRENT_TIMESTAMP _ -> "CURRENT_TIMESTAMP"
  | Parser.DATABASE _ -> "DATABASE"
  | Parser.DEFAULT _ -> "DEFAULT"
  | Parser.DEFERRABLE _ -> "DEFERRABLE"
  | Parser.DEFERRED _ -> "DEFERRED"
  | Parser.DELETE _ -> "DELETE"
  | Parser.DESC _ -> "DESC"
  | Parser.DETACH _ -> "DETACH"
  | Parser.DISTINCT _ -> "DISTINCT"
  | Parser.DO _ -> "DO"
  | Parser.DROP _ -> "DROP"
  | Parser.EACH _ -> "EACH"
  | Parser.ELSE _ -> "ELSE"
  | Parser.END _ -> "END"
  | Parser.ESCAPE _ -> "ESCAPE"
  | Parser.EXCEPT _ -> "EXCEPT"
  | Parser.EXCLUDE _ -> "EXCLUDE"
  | Parser.EXCLUSIVE _ -> "EXCLUSIVE"
  | Parser.EXISTS _ -> "EXISTS"
  | Parser.EXPLAIN _ -> "EXPLAIN"
  | Parser.FAIL _ -> "FAIL"
  | Parser.FILTER _ -> "FILTER"
  | Parser.FIRST _ -> "FIRST"
  | Parser.FOLLOWING _ -> "FOLLOWING"
  | Parser.FOR _ -> "FOR"
  | Parser.FOREIGN _ -> "FOREIGN"
  | Parser.FROM _ -> "FROM"
  | Parser.FULL _ -> "FULL"
  | Parser.GENERATED _ -> "GENERATED"
  | Parser.GLOB _ -> "GLOB"
  | Parser.GROUP _ -> "GROUP"
  | Parser.GROUPS _ -> "GROUPS"
  | Parser.HAVING _ -> "HAVING"
  | Parser.IF _ -> "IF"
  | Parser.IGNORE _ -> "IGNORE"
  | Parser.IMMEDIATE _ -> "IMMEDIATE"
  | Parser.IN _ -> "IN"
  | Parser.INDEX _ -> "INDEX"
  | Parser.INDEXED _ -> "INDEXED"
  | Parser.INITIALLY _ -> "INITIALLY"
  | Parser.INNER _ -> "INNER"
  | Parser.INSERT _ -> "INSERT"
  | Parser.INSTEAD _ -> "INSTEAD"
  | Parser.INTERSECT _ -> "INTERSECT"
  | Parser.INTO _ -> "INTO"
  | Parser.IS _ -> "IS"
  | Parser.ISNULL _ -> "ISNULL"
  | Parser.JOIN _ -> "JOIN"
  | Parser.KEY _ -> "KEY"
  | Parser.LAST _ -> "LAST"
  | Parser.LEFT _ -> "LEFT"
  | Parser.LIKE _ -> "LIKE"
  | Parser.LIMIT _ -> "LIMIT"
  | Parser.MATCH _ -> "MATCH"
  | Parser.MATERIALIZED _ -> "MATERIALIZED"
  | Parser.NATURAL _ -> "NATURAL"
  | Parser.NO _ -> "NO"
  | Parser.NOT _ -> "NOT"
  | Parser.NOTHING _ -> "NOTHING"
  | Parser.NOTNULL _ -> "NOTNULL"
  | Parser.NULL _ -> "NULL"
  | Parser.NULLS _ -> "NULLS"
  | Parser.OF _ -> "OF"
  | Parser.OFFSET _ -> "OFFSET"
  | Parser.ON _ -> "ON"
  | Parser.OR _ -> "OR"
  | Parser.ORDER _ -> "ORDER"
  | Parser.OTHERS _ -> "OTHERS"
  | Parser.OUTER _ -> "OUTER"
  | Parser.OVER _ -> "OVER"
  | Parser.PARTITION _ -> "PARTITION"
  | Parser.PLAN _ -> "PLAN"
  | Parser.PRAGMA _ -> "PRAGMA"
  | Parser.PRECEDING _ -> "PRECEDING"
  | Parser.PRIMARY _ -> "PRIMARY"
  | Parser.QUERY _ -> "QUERY"
  | Parser.RAISE _ -> "RAISE"
  | Parser.RANGE _ -> "RANGE"
  | Parser.RECURSIVE _ -> "RECURSIVE"
  | Parser.REFERENCES _ -> "REFERENCES"
  | Parser.REGEXP _ -> "REGEXP"
  | Parser.REINDEX _ -> "REINDEX"
  | Parser.RELEASE _ -> "RELEASE"
  | Parser.RENAME _ -> "RENAME"
  | Parser.REPLACE _ -> "REPLACE"
  | Parser.RESTRICT _ -> "RESTRICT"
  | Parser.RETURNING _ -> "RETURNING"
  | Parser.RIGHT _ -> "RIGHT"
  | Parser.ROLLBACK _ -> "ROLLBACK"
  | Parser.ROW _ -> "ROW"
  | Parser.ROWS _ -> "ROWS"
  | Parser.SAVEPOINT _ -> "SAVEPOINT"
  | Parser.SET _ -> "SET"
  | Parser.TABLE _ -> "TABLE"
  | Parser.TEMP _ -> "TEMP"
  | Parser.TEMPORARY _ -> "TEMPORARY"
  | Parser.THEN _ -> "THEN"
  | Parser.TIES _ -> "TIES"
  | Parser.TO _ -> "TO"
  | Parser.TRANSACTION _ -> "TRANSACTION"
  | Parser.TRIGGER _ -> "TRIGGER"
  | Parser.UNBOUNDED _ -> "UNBOUNDED"
  | Parser.UNION _ -> "UNION"
  | Parser.UNIQUE _ -> "UNIQUE"
  | Parser.UPDATE _ -> "UPDATE"
  | Parser.USING _ -> "USING"
  | Parser.VACUUM _ -> "VACUUM"
  | Parser.VALUES _ -> "VALUES"
  | Parser.VIEW _ -> "VIEW"
  | Parser.VIRTUAL _ -> "VIRTUAL"
  | Parser.WHEN _ -> "WHEN"
  | Parser.WHERE _ -> "WHERE"
  | Parser.WINDOW _ -> "WINDOW"
  | Parser.WITH _ -> "WITH"
  | Parser.WITHOUT _ -> "WITHOUT"
  | Parser.IDENT (x, loc) -> Printf.sprintf "IDENT(%s), loc: %s" x (string_of_loc loc)
  | Parser.STRING_LIT (x, loc) -> Printf.sprintf "STRING_LIT(%s), loc: %s" x (string_of_loc loc)

open Format

let rec print_program fmt = function
  | Cabs.PROGRAM word -> Stdlib.List.iter (print_select fmt) word

and print_select fmt = function
  | Cabs.SELECT loc -> Format.fprintf fmt "select\n"
