exception Lexing_error of string

open List
open Sedlexing
open Uchar

let identifier_or_keyword =
  [%sedlex.regexp? xid_start, Star xid_continue]

(* Function to convert a Uchar array to a string *)
let uchar_array_to_string (arr: Uchar.t array) : string =
  let x = Array.map Uchar.to_char arr in
  Stdlib.String.init (Array.length x) (Array.get x)

(* Lexer utilities *)
let pos buf = lexing_position_start buf

let keyword_of_string s loc =
  match Stdlib.String.uppercase_ascii s with
  | "ABORT" -> Pre_parser.ABORT loc
  | "ACTION" -> Pre_parser.ACTION loc
  | "ADD" -> Pre_parser.ADD loc
  | "AFTER" -> Pre_parser.AFTER loc
  | "ALL" -> Pre_parser.ALL loc
  | "ALTER" -> Pre_parser.ALTER loc
  | "ALWAYS" -> Pre_parser.ALWAYS loc
  | "ANALYZE" -> Pre_parser.ANALYZE loc
  | "AND" -> Pre_parser.AND loc
  | "AS" -> Pre_parser.AS loc
  | "ASC" -> Pre_parser.ASC loc
  | "ATTACH" -> Pre_parser.ATTACH loc
  | "AUTOINCREMENT" -> Pre_parser.AUTOINCREMENT loc
  | "BEFORE" -> Pre_parser.BEFORE loc
  | "BEGIN" -> Pre_parser.BEGIN loc
  | "BETWEEN" -> Pre_parser.BETWEEN loc
  | "BY" -> Pre_parser.BY loc
  | "CASCADE" -> Pre_parser.CASCADE loc
  | "CASE" -> Pre_parser.CASE loc
  | "CAST" -> Pre_parser.CAST loc
  | "CHECK" -> Pre_parser.CHECK loc
  | "COLLATE" -> Pre_parser.COLLATE loc
  | "COLUMN" -> Pre_parser.COLUMN loc
  | "COMMIT" -> Pre_parser.COMMIT loc
  | "CONFLICT" -> Pre_parser.CONFLICT loc
  | "CONSTRAINT" -> Pre_parser.CONSTRAINT loc
  | "CREATE" -> Pre_parser.CREATE loc
  | "CROSS" -> Pre_parser.CROSS loc
  | "CURRENT" -> Pre_parser.CURRENT loc
  | "CURRENT_DATE" -> Pre_parser.CURRENT_DATE loc
  | "CURRENT_TIME" -> Pre_parser.CURRENT_TIME loc
  | "CURRENT_TIMESTAMP" -> Pre_parser.CURRENT_TIMESTAMP loc
  | "DATABASE" -> Pre_parser.DATABASE loc
  | "DEFAULT" -> Pre_parser.DEFAULT loc
  | "DEFERRABLE" -> Pre_parser.DEFERRABLE loc
  | "DEFERRED" -> Pre_parser.DEFERRED loc
  | "DELETE" -> Pre_parser.DELETE loc
  | "DESC" -> Pre_parser.DESC loc
  | "DETACH" -> Pre_parser.DETACH loc
  | "DISTINCT" -> Pre_parser.DISTINCT loc
  | "DO" -> Pre_parser.DO loc
  | "DROP" -> Pre_parser.DROP loc
  | "EACH" -> Pre_parser.EACH loc
  | "ELSE" -> Pre_parser.ELSE loc
  | "END" -> Pre_parser.END loc
  | "ESCAPE" -> Pre_parser.ESCAPE loc
  | "EXCEPT" -> Pre_parser.EXCEPT loc
  | "EXCLUDE" -> Pre_parser.EXCLUDE loc
  | "EXCLUSIVE" -> Pre_parser.EXCLUSIVE loc
  | "EXISTS" -> Pre_parser.EXISTS loc
  | "EXPLAIN" -> Pre_parser.EXPLAIN loc
  | "FAIL" -> Pre_parser.FAIL loc
  | "FILTER" -> Pre_parser.FILTER loc
  | "FIRST" -> Pre_parser.FIRST loc
  | "FOLLOWING" -> Pre_parser.FOLLOWING loc
  | "FOR" -> Pre_parser.FOR loc
  | "FOREIGN" -> Pre_parser.FOREIGN loc
  | "FROM" -> Pre_parser.FROM loc
  | "FULL" -> Pre_parser.FULL loc
  | "GENERATED" -> Pre_parser.GENERATED loc
  | "GLOB" -> Pre_parser.GLOB loc
  | "GROUP" -> Pre_parser.GROUP loc
  | "GROUPS" -> Pre_parser.GROUPS loc
  | "HAVING" -> Pre_parser.HAVING loc
  | "IF" -> Pre_parser.IF loc
  | "IGNORE" -> Pre_parser.IGNORE loc
  | "IMMEDIATE" -> Pre_parser.IMMEDIATE loc
  | "IN" -> Pre_parser.IN loc
  | "INDEX" -> Pre_parser.INDEX loc
  | "INDEXED" -> Pre_parser.INDEXED loc
  | "INITIALLY" -> Pre_parser.INITIALLY loc
  | "INNER" -> Pre_parser.INNER loc
  | "INSERT" -> Pre_parser.INSERT loc
  | "INSTEAD" -> Pre_parser.INSTEAD loc
  | "INTERSECT" -> Pre_parser.INTERSECT loc
  | "INTO" -> Pre_parser.INTO loc
  | "IS" -> Pre_parser.IS loc
  | "ISNULL" -> Pre_parser.ISNULL loc
  | "JOIN" -> Pre_parser.JOIN loc
  | "KEY" -> Pre_parser.KEY loc
  | "LAST" -> Pre_parser.LAST loc
  | "LEFT" -> Pre_parser.LEFT loc
  | "LIKE" -> Pre_parser.LIKE loc
  | "LIMIT" -> Pre_parser.LIMIT loc
  | "MATCH" -> Pre_parser.MATCH loc
  | "MATERIALIZED" -> Pre_parser.MATERIALIZED loc
  | "NATURAL" -> Pre_parser.NATURAL loc
  | "NO" -> Pre_parser.NO loc
  | "NOT" -> Pre_parser.NOT loc
  | "NOTHING" -> Pre_parser.NOTHING loc
  | "NOTNULL" -> Pre_parser.NOTNULL loc
  | "NULL" -> Pre_parser.NULL loc
  | "NULLS" -> Pre_parser.NULLS loc
  | "OF" -> Pre_parser.OF loc
  | "OFFSET" -> Pre_parser.OFFSET loc
  | "ON" -> Pre_parser.ON loc
  | "OR" -> Pre_parser.OR loc
  | "ORDER" -> Pre_parser.ORDER loc
  | "OTHERS" -> Pre_parser.OTHERS loc
  | "OUTER" -> Pre_parser.OUTER loc
  | "OVER" -> Pre_parser.OVER loc
  | "PARTITION" -> Pre_parser.PARTITION loc
  | "PLAN" -> Pre_parser.PLAN loc
  | "PRAGMA" -> Pre_parser.PRAGMA loc
  | "PRECEDING" -> Pre_parser.PRECEDING loc
  | "PRIMARY" -> Pre_parser.PRIMARY loc
  | "QUERY" -> Pre_parser.QUERY loc
  | "RAISE" -> Pre_parser.RAISE loc
  | "RANGE" -> Pre_parser.RANGE loc
  | "RECURSIVE" -> Pre_parser.RECURSIVE loc
  | "REFERENCES" -> Pre_parser.REFERENCES loc
  | "REGEXP" -> Pre_parser.REGEXP loc
  | "REINDEX" -> Pre_parser.REINDEX loc
  | "RELEASE" -> Pre_parser.RELEASE loc
  | "RENAME" -> Pre_parser.RENAME loc
  | "REPLACE" -> Pre_parser.REPLACE loc
  | "RESTRICT" -> Pre_parser.RESTRICT loc
  | "RETURNING" -> Pre_parser.RETURNING loc
  | "RIGHT" -> Pre_parser.RIGHT loc
  | "ROLLBACK" -> Pre_parser.ROLLBACK loc
  | "ROW" -> Pre_parser.ROW loc
  | "ROWS" -> Pre_parser.ROWS loc
  | "SAVEPOINT" -> Pre_parser.SAVEPOINT loc
  | "SELECT" -> Pre_parser.SELECT loc
  | "SET" -> Pre_parser.SET loc
  | "TABLE" -> Pre_parser.TABLE loc
  | "TEMP" -> Pre_parser.TEMP loc
  | "TEMPORARY" -> Pre_parser.TEMPORARY loc
  | "THEN" -> Pre_parser.THEN loc
  | "TIES" -> Pre_parser.TIES loc
  | "TO" -> Pre_parser.TO loc
  | "TRANSACTION" -> Pre_parser.TRANSACTION loc
  | "TRIGGER" -> Pre_parser.TRIGGER loc
  | "UNBOUNDED" -> Pre_parser.UNBOUNDED loc
  | "UNION" -> Pre_parser.UNION loc
  | "UNIQUE" -> Pre_parser.UNIQUE loc
  | "UPDATE" -> Pre_parser.UPDATE loc
  | "USING" -> Pre_parser.USING loc
  | "VACUUM" -> Pre_parser.VACUUM loc
  | "VALUES" -> Pre_parser.VALUES loc
  | "VIEW" -> Pre_parser.VIEW loc
  | "VIRTUAL" -> Pre_parser.VIRTUAL loc
  | "WHEN" -> Pre_parser.WHEN loc
  | "WHERE" -> Pre_parser.WHERE loc
  | "WINDOW" -> Pre_parser.WINDOW loc
  | "WITH" -> Pre_parser.WITH loc
  | "WITHOUT" -> Pre_parser.WITHOUT loc
  | _           -> raise (Lexing_error "skibi")

  let kwds = [
    "ABORT"; "ACTION"; "ADD"; "AFTER"; "ALL"; "ALTER"; "ALWAYS"; "ANALYZE"; "AND"; "AS";
    "ASC"; "ATTACH"; "AUTOINCREMENT"; "BEFORE"; "BEGIN"; "BETWEEN"; "BY"; "CASCADE"; "CASE";
    "CAST"; "CHECK"; "COLLATE"; "COLUMN"; "COMMIT"; "CONFLICT"; "CONSTRAINT"; "CREATE";
    "CROSS"; "CURRENT"; "CURRENT_DATE"; "CURRENT_TIME"; "CURRENT_TIMESTAMP"; "DATABASE";
    "DEFAULT"; "DEFERRABLE"; "DEFERRED"; "DELETE"; "DESC"; "DETACH"; "DISTINCT"; "DO"; "DROP";
    "EACH"; "ELSE"; "END"; "ESCAPE"; "EXCEPT"; "EXCLUDE"; "EXCLUSIVE"; "EXISTS"; "EXPLAIN";
    "FAIL"; "FILTER"; "FIRST"; "FOLLOWING"; "FOR"; "FOREIGN"; "FROM"; "FULL"; "GENERATED";
    "GLOB"; "GROUP"; "GROUPS"; "HAVING"; "IF"; "IGNORE"; "IMMEDIATE"; "IN"; "INDEX";
    "INDEXED"; "INITIALLY"; "INNER"; "INSERT"; "INSTEAD"; "INTERSECT"; "INTO"; "IS"; "ISNULL";
    "JOIN"; "KEY"; "LAST"; "LEFT"; "LIKE"; "LIMIT"; "MATCH"; "MATERIALIZED"; "NATURAL"; "NO";
    "NOT"; "NOTHING"; "NOTNULL"; "NULL"; "NULLS"; "OF"; "OFFSET"; "ON"; "OR"; "ORDER";
    "OTHERS"; "OUTER"; "OVER"; "PARTITION"; "PLAN"; "PRAGMA"; "PRECEDING"; "PRIMARY"; "QUERY";
    "RAISE"; "RANGE"; "RECURSIVE"; "REFERENCES"; "REGEXP"; "REINDEX"; "RELEASE"; "RENAME";
    "REPLACE"; "RESTRICT"; "RETURNING"; "RIGHT"; "ROLLBACK"; "ROW"; "ROWS"; "SAVEPOINT";
    "SELECT"; "SET"; "TABLE"; "TEMP"; "TEMPORARY"; "THEN"; "TIES"; "TO"; "TRANSACTION";
    "TRIGGER"; "UNBOUNDED"; "UNION"; "UNIQUE"; "UPDATE"; "USING"; "VACUUM"; "VALUES"; "VIEW";
    "VIRTUAL"; "WHEN"; "WHERE"; "WINDOW"; "WITH"; "WITHOUT"
]

let is_keyword str = 
  Stdlib.List.mem (Stdlib.String.uppercase_ascii str) kwds

let rec token buf =
  match%sedlex buf with
  | white_space -> token buf
  | '+' -> Pre_parser.PLUS (lexing_position_start buf)
  | '-' -> Pre_parser.MINUS (lexing_position_start buf)
  | '*' -> Pre_parser.STAR (lexing_position_start buf)
  | '/' -> Pre_parser.SLASH (lexing_position_start buf)
  | '%' -> Pre_parser.PERCENT (lexing_position_start buf)
  | '=' -> Pre_parser.EQUAL (lexing_position_start buf)
  | "==" -> Pre_parser.DOUBLE_EQUAL (lexing_position_start buf)
  | "!=" -> Pre_parser.BANG_EQUAL (lexing_position_start buf)
  | "<>" -> Pre_parser.NOT_EQUAL (lexing_position_start buf)
  | '<' -> Pre_parser.LT (lexing_position_start buf)
  | "<=" -> Pre_parser.LTE (lexing_position_start buf)
  | '>' -> Pre_parser.GT (lexing_position_start buf)
  | ">=" -> Pre_parser.GTE (lexing_position_start buf)
  | '&' -> Pre_parser.AMP (lexing_position_start buf)
  | '|' -> Pre_parser.BAR (lexing_position_start buf)
  | "<<" -> Pre_parser.LSHIFT (lexing_position_start buf)
  | ">>" -> Pre_parser.RSHIFT (lexing_position_start buf)
  | '~' -> Pre_parser.TILDE (lexing_position_start buf)
  | '(' -> Pre_parser.LPAREN (lexing_position_start buf)
  | ')' -> Pre_parser.RPAREN (lexing_position_start buf)
  | ',' -> Pre_parser.COMMA (lexing_position_start buf)
  | ';' -> Pre_parser.SEMICOLON (lexing_position_start buf)
  | '.' -> Pre_parser.DOT (lexing_position_start buf)

  (* Identifiers and keywords *)
  | identifier_or_keyword ->
      let uArr = Sedlexing.lexeme buf in
      let str = uchar_array_to_string uArr in
      if is_keyword str then
        keyword_of_string str (lexing_position_start buf)
      else 
        Pre_parser.IDENT (str, lexing_position_start buf)

  (* End of file *)
  | eof -> Pre_parser.EOF

  (* Fallback case *)
  | _ -> raise (Lexing_error "Unexpected character")
