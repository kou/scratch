(define-module number-table-servlet.http
  (use text.html-lite)
  (use text.tree)
  (use gauche.parameter)
  (use srfi-1)
  (use esm.gauche)
  (use scratch.session)
  (use scratch.view.http)
  (use scratch.db)
  (use number-table-servlet.clear-list)
  (export)
  )
(select-module number-table-servlet.http)

(define-macro (reload-files)
  '(load-esm-files "number-table-servlet/http/*.esm"))

(reload-files)

(define (default-view)
  (main))

(define (available-ways)
  (map (lambda (way available)
         (list way available))
       '("west" "north" "east" "south")
       (get-state 'available-ways)))

(define (move-navi)
  (navigator :params (available-ways)))

(define (navi-link way)
  (if (get-param way)
      (tree->string
       (html:a :href (href :action "move" 'way way)
               way))
      way))

(define (get-clear-list . do-sort)
  (let ((lst (get-value (servlet-db) 'clear-list '())))
    (if (get-optional do-sort #t)
        (sort lst (lambda (x y)
                    (< (clear-list-score x)
                       (clear-list-score y))))
        lst)))

(provide "number-table-servlet/http")