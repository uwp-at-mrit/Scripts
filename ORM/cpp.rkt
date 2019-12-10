#lang racket

(provide (all-defined-out))

(define cstring 'std::string)
(define ns:: 'WarGrey::SCADA)

(define &separator
  (lambda []
    (printf "/~a/~n" (make-string 98 #\*))))

(define &linebreak
  (lambda [[count 1]]
    (let nbsp ([c count])
      (when (> c 0)
        (newline)
        (nbsp (sub1 c))))))

(define &hspace
  (lambda [[count 1]]
    (let hsp ([c count])
      (when (> c 0)
        (display #\space)
        (hsp (sub1 c))))))

(define &htab
  (lambda [[count 1]]
    (&hspace (* count 4))))

(define &brace
  (lambda [indent #:semicolon? [semicolon? #false]]
    (&htab indent)
    (display #\})

    (when (and semicolon?)
      (display #\;))
    (&linebreak)))

(define &pragma
  (lambda pragmas
    (for ([pragma (in-list pragmas)])
      (printf "#pragma ~a~n" pragma))
    (when (> (length pragmas) 0)
      (&linebreak 1))))

(define &include
  (lambda headers
    (for ([header (in-list headers)])
      (cond [(string? header) (printf "#include ~s~n" header)]
            [else (printf "#include <~a>~n" header)]))
    (when (> (length headers) 0)
      (&linebreak 1))))

(define &namespace
  (lambda [ns λbody]
    (printf "namespace ~a {~n" ns)
    (λbody 1)
    (&brace 0)))

(define &using-namespace
  (lambda namespaces
    (for ([ns (in-list namespaces)])
      (printf "using namespace ~a;~n" ns))
    (&linebreak 1)))

(define &primary-key
  (lambda [Table_pk rowids idtypes indent]
    (cond [(> (length rowids) 1) (&struct Table_pk rowids idtypes indent)]
          [else (&htab indent)
                (printf "typedef ~a ~a;~n" (car idtypes) Table_pk)
                (&linebreak 1)])))

(define &enum
  (lambda [name fields indent]
    (&htab indent)
    (printf "private enum class ~a { ~a, _ };~n" name (string-join (map symbol->string fields) ", "))
    (&linebreak 1)))

(define &struct
  (lambda [name fields types indent]
    (&htab indent)
    (printf "private struct ~a {~n" name)

    (for ([field (in-list fields)]
          [type (in-list types)])
      (&htab (add1 indent))
      (printf "~a ~a;~n" type field))
    
    (&brace indent #:semicolon? #true)
    (&linebreak 1)))

(define &interface
  (lambda [IName Table indent]
    (&htab indent)
    (printf "private class ~a abstract {~n" IName)
    (&htab indent)
    (printf "public:~n")
    (&htab (add1 indent))
    (printf "virtual bool step(~a::~a& occurrence, bool asc, int code) = 0;~n" ns:: Table)
    (&brace indent #:semicolon? #true)
    (&linebreak 1)))

(define &table-column-info
  (lambda [var_columns var_rowids rowids cols dbtypes not-nulls uniques]
    (printf "static const char* ~a[] = { ~a };~n" var_rowids
            (let strcat ([s (format "~s" (symbol->string (car rowids)))]
                         [r (cdr rowids)])
              (cond [(null? r) s]
                    [else (strcat (format "~a, ~s" s (symbol->string (car r)))
                                  (cdr r))])))
    (&linebreak 1)
    
    (printf "static TableColumnInfo ~a[] = {~n" var_columns)
    (for ([col (in-list cols)]
          [type (in-list dbtypes)]
          [nnil (in-list not-nulls)]
          [uniq (in-list uniques)])
      (&hspace 4) (printf "{ ~s, SDT::~a, nullptr, ~a | ~a | ~a },~n"
                          (symbol->string col)
                          (symbol->string type)
                          (if (memq col rowids) 'DB_PRIMARY_KEY 0)
                          (if (and nnil) 'DB_NOT_NULL 0)
                          (if (and uniq) 'DB_UNIQUE 0)))
    (&brace 0 #:semicolon? #true)
    (&linebreak 1)))

(define &#%table
  (case-lambda
    [(λname Table Table_pk indent)
     (&htab indent) (printf "~a::~a ~a(~a::~a& self);~n" ns:: Table_pk λname ns:: Table)]
    [(λname Table Table_pk rowids _)
     (printf "~a ~a::~a(~a& self) {~n" Table_pk ns:: λname Table)
     (&htab 1)
     (if (= (length rowids) 1)
         (printf "return self.~a;~n" (car rowids))
         (printf "return { ~a };~n" (string-join (map (curry format "self.~a") rowids) ", ")))
     (&brace 0)
     (&linebreak 1)]))

(define &make-table
  (case-lambda
    [(λname Table fields types defvals indent)
     (&htab indent) (printf "~a::~a ~a(~a);~n" ns:: Table λname (make-arguments fields types defvals 'declare))]
    [(λname Table fields types defvals default-table _)
     (printf "~a ~a::~a(~a) {~n" Table ns:: λname (make-arguments fields types defvals 'define))
     (&htab 1) (printf "~a self;~n" Table)
     (&linebreak 1)
     (&htab 1) (printf "~a(self, ~a);~n" default-table (make-arguments fields types defvals 'call))
     (&linebreak 1)
     (&htab 1) (printf "return self;~n")
     (&brace 0)
     (&linebreak 1)]))

(define &default-table
  (case-lambda
    [(λname Table fields types defvals indent)
     (&htab indent) (printf "void ~a(~a::~a& self, ~a);~n" λname ns:: Table (make-arguments fields types defvals 'declare))]
    [(λname Table fields types defvals)
     (printf "void ~a::~a(~a& self, ~a) {~n" ns:: λname Table (make-arguments fields types defvals 'define))
     (for ([field (in-list fields)]
           [val (in-list defvals)])
       (cond [(not val) (&htab 1) (printf "if (~a.has_value()) { self.~a = ~a.value(); }~n" field field field)]
             [else (&htab 1) (printf "self.~a = " field)
                   (cond [(symbol? val) (printf "~a();~n" val)]
                         [else (printf "((~a.has_value()) ? ~a.value() : ~s);~n" field field val)])]))
     (&brace 0)
     (&linebreak 1)]))

(define &refresh-table
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "void ~a(~a::~a& self);~n" λname ns:: Table)]
    [(λname Table fields autovals)
     (printf "void ~a::~a(~a& self) {~n" ns:: λname Table)
     (for ([field (in-list fields)]
           [val (in-list autovals)])
       (when (and val)
         (&htab 1) (printf "self.~a = ~a();~n" field val)))
     (&brace 0)
     (&linebreak 1)]))

(define &store-table
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "void ~a(~a::~a& self, ~a::IPreparedStatement* stmt);~n" λname ns:: Table ns::)]
    [(λname Table fields _)
     (printf "void ~a::~a(~a& self, IPreparedStatement* stmt) {~n" ns:: λname Table)
     (bind-parameters 'stmt 'self fields 0 1)
     (&brace 0)
     (&linebreak 1)]))

(define &restore-table
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "void ~a(~a::~a& self, ~a::IPreparedStatement* stmt);~n" λname ns:: Table ns::)]
    [(λname Table fields types not-nulls rowids)
     (printf "void ~a::~a(~a& self, IPreparedStatement* stmt) {~n" ns:: λname Table)
     (for ([field (in-list fields)]
           [type (in-list types)]
           [n-nil (in-list not-nulls)]
           [idx (in-naturals)])
       (&htab 1) (printf "self.~a = stmt->column~a~a(~aU);~n" field
                         (if (or (memq field rowids) n-nil) "_" "_maybe_")
                         (sql-type type)
                         idx))
     (&brace 0)
     (&linebreak 1)]))

(define &table-aggregate
 (case-lambda
   [(table λname type indent)
    (&htab indent) (printf "~a ~a_~a(~a::IDBSystem* dbc, ~a::~a column = ~a::_, bool distinct = false);~n"
                           type table λname ns:: ns:: table table)]
   [(table λname type query_value column_infos dbtable)
    (printf "~a ~a::~a_~a(~a::IDBSystem* dbc, ~a column, bool distinct) {~n" type ns:: table λname ns:: table)
    (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
    (&htab 1) (printf "const char* colname = ((column == ~a::_) ? nullptr : ~a[static_cast<unsigned int>(column)].name);~n" table column_infos)
    (&linebreak 1)
    (&htab 1) (printf "return dbc->~a(vsql->table_~a(~s, colname, distinct));~n" query_value λname (symbol->string dbtable))
    (&brace 0)
    (&linebreak 1)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define &create-table
  (case-lambda
    [(λname indent)
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc, bool if_not_exists = true);~n" λname ns::)]
    [(λname tablename dbtablename column_infos table_rowids)
     (printf "void ~a::~a(IDBSystem* dbc, bool if_not_exists) {~n" ns:: λname)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->create_table(~s, ~a, sizeof(~a)/sizeof(char*), if_not_exists);~n" cstring (symbol->string dbtablename) table_rowids table_rowids)
     (&linebreak 1)
     (&htab 1) (printf "dbc->exec(sql);~n")
     (&brace 0)
     (&linebreak 1)]))

(define &insert-table
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc, ~a& self, bool replace = false);~n" λname ns:: Table)
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc, ~a* selves, size_t count, bool replace = false);~n" λname ns:: Table)]
    [(λname Table tablename dbtablename store column_infos)
     (printf "void ~a::~a(IDBSystem* dbc, ~a& self, bool replace) {~n" ns:: λname Table)
     (&htab 1) (printf "~a(dbc, &self, 1, replace);~n" λname)
     (&brace 0)
     (&linebreak 1)
     (printf "void ~a::~a(IDBSystem* dbc, ~a* selves, size_t count, bool replace) {~n" ns:: λname Table)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->insert_into(~s, replace);~n" cstring (symbol->string dbtablename))
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "for (size_t i = 0; i < count; i ++) {~n")
     (&htab 3) (printf "~a(selves[i], stmt);~n" store)
     (&linebreak 1)
     (&htab 3) (printf "dbc->exec(stmt);~n")
     (&htab 3) (printf "stmt->reset(true);~n")
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&brace 0)
     (&linebreak 1)]))

(define &foreach-table
  (case-lambda
    [(λname ITableCursor Table tablename order_by indent)
     (&htab indent)
     (printf "void ~a(~a::IDBSystem* dbc, ~a::~a* cursor, uint64 limit = 0U, uint64 offset = 0U, ~a::~a order_by = ~a::~a, bool asc = true);~n"
             λname ns:: ns:: ITableCursor ns:: tablename tablename (or order_by '_))]
    [(λname ITableCursor Table tablename dbtablename restore column_infos _)
     (printf "void ~a::~a(IDBSystem* dbc, ~a* cursor, uint64 limit, uint64 offset, ~a order_by, bool asc) {~n" ns:: λname ITableCursor tablename)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "const char* colname = ((order_by == ~a::_) ? nullptr : ~a[static_cast<unsigned int>(order_by)].name);~n" tablename column_infos)
     (&htab 1) (printf "~a sql = vsql->select_from(~s, colname, asc, limit, offset);~n" cstring (symbol->string dbtablename))
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "~a self;~n" Table)
     (&linebreak 1)
     (&htab 2) (printf "while(stmt->step()) {~n")
     (&htab 3) (printf "~a(self, stmt);~n" restore)
     (&htab 3) (printf "if (!cursor->step(self, asc, dbc->last_errno())) break;~n")
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&linebreak 1)
     (&brace 0)
     (&linebreak 1)]))

(define &select-table
  (case-lambda
    [(λname Table tablename order_by indent)
     (&htab indent)
     (printf "std::list<~a::~a> ~a(~a::IDBSystem* dbc, uint64 limit = 0U, uint64 offset = 0U, ~a::~a order_by = ~a::~a, bool asc = true);~n"
             ns:: Table λname ns:: ns:: tablename tablename (or order_by '_))]
    [(λname Table tablename dbtablename restore column_infos _)
     (printf "std::list<~a> ~a::~a(IDBSystem* dbc, uint64 limit, uint64 offset, ~a order_by, bool asc) {~n" Table ns:: λname tablename)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "const char* colname = ((order_by == ~a::_) ? nullptr : ~a[static_cast<unsigned int>(order_by)].name);~n" tablename column_infos)
     (&htab 1) (printf "~a sql = vsql->select_from(~s, colname, asc, limit, offset);~n" cstring (symbol->string dbtablename))
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&htab 1) (printf "std::list<~a> queries;~n" Table)
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "~a self;~n" Table)
     (&linebreak 1)
     (&htab 2) (printf "while(stmt->step()) {~n")
     (&htab 3) (printf "~a(self, stmt);~n" restore)
     (&htab 3) (printf "queries.push_back(self);~n")
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&linebreak 1)
     (&htab 1) (printf "return queries;~n")
     (&brace 0)
     (&linebreak 1)]))

(define &seek-table
  (case-lambda
    [(λname Table Table_pk indent)
     (&htab indent) (printf "std::optional<~a::~a> ~a(~a::IDBSystem* dbc, ~a::~a where);~n" ns:: Table λname ns:: ns:: Table_pk)]
    [(λname Table tablename dbtablename restore column_infos Table_pk rowids table-rowids)
     (printf "std::optional<~a> ~a::~a(IDBSystem* dbc, ~a where) {~n" Table ns:: λname Table_pk)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->seek_from(~s, ~a, sizeof(~a)/sizeof(char*));~n" cstring (symbol->string dbtablename) table-rowids table-rowids)
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&htab 1) (printf "std::optional<~a> query;~n" Table)
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "~a self;~n" Table)
     (&linebreak 1)
     (bind-rowids 'stmt 'where rowids 2)
     (&linebreak 1)
     (&htab 2) (printf "if (stmt->step()) {~n")
     (&htab 3) (printf "~a(self, stmt);~n" restore)
     (&htab 3) (printf "query = self;~n")
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&linebreak 1)
     (&htab 1) (printf "return query;~n")
     (&brace 0)
     (&linebreak 1)]))

(define &update-table
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc, ~a::~a& self, bool refresh = true);~n" λname ns:: ns:: Table)
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc, ~a::~a* selves, size_t count, bool refresh = true);~n" λname ns:: ns:: Table)]
    [(λname Table tablename dbtablename rowids fields table-rowids column_infos refresh)
     (define nonrowids (remove* rowids fields))
     (printf "void ~a::~a(IDBSystem* dbc, ~a& self, bool refresh) {~n" ns:: λname Table)
     (&htab 1) (printf "~a(dbc, &self, 1, refresh);~n" λname)
     (&brace 0)
     (&linebreak 1)
     (printf "void ~a::~a(IDBSystem* dbc, ~a* selves, size_t count, bool refresh) {~n" ns:: λname Table)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->update_set(~s, ~a, sizeof(~a)/sizeof(char*));~n" cstring (symbol->string dbtablename) table-rowids table-rowids)
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "for (size_t i = 0; i < count; i ++) {~n")
     (&htab 3) (printf "if (refresh) {~n")
     (&htab 4) (printf "~a(selves[i]);~n" refresh)
     (&brace 3)
     (&linebreak 1)
     (bind-parameters 'stmt "selves[i]" rowids (length nonrowids) 3)
     (&linebreak 1)
     (bind-parameters 'stmt "selves[i]" nonrowids 0 3)
     (&linebreak 1)
     (&htab 3) (printf "dbc->exec(stmt);~n")
     (&htab 3) (printf "stmt->reset(true);~n")
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&brace 0)
     (&linebreak 1)]))

(define &delete-table
  (case-lambda
    [(λname Table_pk indent)
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc, ~a::~a& where);~n" λname ns:: ns:: Table_pk)
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc, ~a::~a* wheres, size_t count);~n" λname ns:: ns:: Table_pk)]
    [(λname Table_pk tablename dbtablename rowids table-rowids column_infos)
     (printf "void ~a::~a(IDBSystem* dbc, ~a& where) {~n" ns:: λname Table_pk)
     (&htab 1) (printf "~a(dbc, &where, 1);~n" λname)
     (&brace 0)
     (&linebreak 1)
     (printf "void ~a::~a(IDBSystem* dbc, ~a* wheres, size_t count) {~n" ns:: λname Table_pk)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->delete_from(~s, ~a, sizeof(~a)/sizeof(char*));~n" cstring (symbol->string dbtablename) table-rowids table-rowids)
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "for (size_t i = 0; i < count; i ++) {~n")
     (bind-rowids 'stmt "wheres[i]" rowids 3)
     (&linebreak 1)
     (&htab 3) (printf "dbc->exec(stmt);~n")
     (&htab 3) (printf "stmt->reset(true);~n")
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&brace 0)
     (&linebreak 1)]))

(define &drop-table
  (case-lambda
    [(λname indent)
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc);~n" λname ns::)]
    [(λname tablename dbtablename column_infos)
     (printf "void ~a::~a(IDBSystem* dbc) {~n" ns:: λname)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->drop_table(~s);~n" cstring (symbol->string dbtablename))
     (&linebreak)
     (&htab 1) (printf "dbc->exec(sql);~n")
     (&brace 0)
     (&linebreak 1)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define &template-insert
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "template<size_t N>~n")
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc, ~a (&selves)[N], bool replace = false) {~n" λname ns:: Table)
     (&htab (+ indent 1)) (printf "~a::~a(dbc, selves, N, replace);~n" ns:: λname)
     (&brace 1)
     (&linebreak 1)]))

(define &template-delete
  (case-lambda
    [(λname Table_pk indent)
     (&htab indent) (printf "template<size_t N>~n")
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc, ~a (&wheres)[N]) {~n" λname ns:: Table_pk)
     (&htab (+ indent 1)) (printf "~a::~a(dbc, wheres, N);~n" ns:: λname)
     (&brace 1)
     (&linebreak 1)]))

(define &template-update
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "template<size_t N>~n")
     (&htab indent) (printf "void ~a(~a::IDBSystem* dbc, ~a (&selves)[N], bool refresh = true) {~n" λname ns:: Table)
     (&htab (+ indent 1)) (printf "~a::~a(dbc, selves, N, refresh);~n" ns:: λname)
     (&brace 1)
     (&linebreak 1)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bind-parameters
  (lambda [stmt name fields start htab]
    (for ([field (in-list fields)]
          [idx (in-naturals start)])
      (&htab htab) (printf "~a->bind_parameter(~aU, ~a.~a);~n" stmt idx name field))))

(define bind-rowids
  (lambda [stmt name rowids htab]
    (cond [(= (length rowids) 1) (&htab htab) (printf "~a->bind_parameter(0U, ~a);~n" stmt name)]
          [else (bind-parameters stmt name rowids 0 htab)])))

(define make-arguments
  (lambda [fields types defvals hint]
    (string-join 
     (filter-map (λ [field type defval]
                   (and (not (symbol? defval))
                        (case hint
                          [(call) (symbol->string field)]
                          [(declare) (format "std::optional<~a> ~a = std::nullopt" type field)]
                          [else (format "std::optional<~a> ~a" type field)])))
                 fields types defvals)
     ", ")))

(define sql-type
  (lambda [type]
    (case type
      [(Integer) 'int64]
      [(Float) 'double]
      [else 'text])))
