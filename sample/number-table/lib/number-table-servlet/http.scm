(define-module number-table-servlet.http
  (use text.html-lite)
  (use text.tree)
  (use scratch.session)
  (use scratch.view.http)
  (use gauche.parameter)
  (use esm.gauche)
  (use srfi-1)
  (export default)
  )
(select-module number-table-servlet.http)

(define-macro (reload-files)
  '(load-esm-files "number-table-servlet/http/*.esm"))

(reload-files)

(define (default . args)
  (apply main args))

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

(provide "number-table-servlet/http")