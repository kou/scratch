#!/usr/local/bin/gosh

(use text.tree)
(use text.html-lite)
(use www.cgi)
(use dsm.client)

(define _html-escape-string html-escape-string)
(define (html-escape-string obj)
  (_html-escape-string (x->string obj)))

(define (number-table->html:table table)
  (html:table
   :border 1
   (map (lambda (row)
          (html:tr
           (map (lambda (cell)
                  (html:td (html-escape-string cell)))
                row)))
        table)))

(define (navigator id west north east south)
  (let ((params `((id . ,id))))
    (html:table
     :border 1
     (html:tr (html:td "")
              (html:td (if north
                           (navi-link "north" (cons '(way . north) params))
                           "north"))
              (html:td ""))
     (html:tr (html:td (if west
                           (navi-link "west" (cons '(way . west) params))
                           "west"))
              (html:td "")
              (html:td (if east
                           (navi-link "east" (cons '(way . east) params))
                           "east")))
     (html:tr (html:td "")
              (html:td (if south
                           (navi-link "south" (cons '(way . south) params))
                           "south"))
              (html:td "")))))

(define (navi-link title params)
  (html:a :href (tree->string `(,#`",|*program-name*|?"
                                ,(string-join
                                  (map (lambda (param)
                                         #`",(car param)=,(cdr param)")
                                       params)
                                  ";")))
          title))

(define (main args)
  (cgi-main
   (lambda (params)
     (let* ((client (connect-server :host "localhost" :port 5969))
            (intep ((client "/start")))
            (id (cgi-get-parameter "id" params :convert string->number))
            (way (cgi-get-parameter "way" params)))
       (if id
           (set! intep (intep "restore" id))
           (set! id (intep "session-info")))
       (if way (intep "move" way))
       `(,(cgi-header)
         ,(html-doctype)
         ,(html:html
           (html:head (html:title "Number Table Example"))
           (html:body
            (html:a :href *program-name* "NEW GAME")
            (if (intep "clear?") (html:h2 "CLEAR!!!") "")
            (html:h2 "Number Table")
            (number-table->html:table (intep "show"))
            (apply navigator id (intep "available-ways")))))))))
