#!/usr/bin/env gosh

(use test.unit)

(define-module scratch.client.mail.test
  (use test.unit)
  (use gauche.charconv)
  (require "test/util")
  (extend scratch.client.mail))
(select-module scratch.client.mail.test)

(define-test-case "mail client test"
  ("test content-type->encoding"
   (assert-each (lambda (expect content-type . args)
                  (assert-equal expect
                                (apply content-type->encoding content-type
                                       args)))
                `(("iso-2022-jp" "charset=iso-2022-jp")
                  ("iso-2022-jp" "charset=ISO-2022-JP")
                  ("iso-2022-jp" "" "iso-2022-jp")
                  ("iso-2022-jp" "" "ISO-2022-JP")
                  ("ascii" "")
                  ("ascii" "BAD CONTENT-TYPE")
                  ("ascii" "Text/Plain;")
                  ("ascii" "Text/Plain")
                  ("iso-2022-jp" "Text/Plain;charset=iso-2022-jp")
                  ("iso-2022-jp" "Text/Plain;  charset=iso-2022-jp aaa"))))
  ("test parse-body"
   (assert-each (lambda (expect body)
                  (assert-equal expect
                                (parse-body (if (port? body)
                                                body
                                                (open-input-string body)))))
                `(((("a" "b")) "a:b")
                  ((("a" "b")) "a: b  ")
                  ((("a" "b")
                    ("c" "d e f")) "a: b  \nc:  d e f  ")
                  ((("body" "abcde")) "abcde")
                  ((("body" "abcde\nfghij")) "abcde\nfghij")
                  ((("a" "abcde\nfghij")) "a:\nabcde\nfghij")
                  ((("a" "abcde\nfghij")
                    ("b" "xyz")) "a:\nabcde\nfghij\nb: xyz")
                  ((("a" "abcde")
                    ("body" "xyz\nabc")) "a:abcde\nxyz\nabc")
                  ((("a" "b" "c")) "a:b\na:c"))))
  ("test add-param"
   (assert-each (lambda (expect name value params)
                  (assert-equal expect (add-param name value params)))
                '(((("a" "b")) "a" "b" ())
                  ((("a" "b" "c")) "a" "b" (("a" "c"))))))
  ("test port->header-list&body"
   (assert-each (lambda (expect-header expect-body mail)
                  (receive (header body)
                      (port->header-list&body (if (port? mail)
                                                  mail
                                                  (open-input-string mail)))
                    (assert-equal expect-header header)
                    (assert-equal expect-body body)))
                `((() "" "")
                  ((("subject" "abc")) "" "Subject: abc")
                  ((("subject" "abc")) "" "Subject: abc  ")
                  ((("subject" "abc")) "xyz" "Subject: abc\n\nxyz")
                  ((("subject" "あabc")) "xyz"
                   "Subject: =?iso-2022-jp?Q?=1B=24=42=24=22=1B=28=42?=abc\n\nxyz")
                  ((("subject" "abc")
                    ("content-type" "text/plain; charset=iso-2022-jp"))
                   "あいうabc"
                   ,(ces-convert
                     (string-join
                      (list
                       "Subject: abc"
                       "Content-Type: text/plain; charset=iso-2022-jp"
                       ""
                       "あいうabc")
                      "\n")
                     "UTF-8" "iso-2022-jp")))))
  ("test id&action"
   (assert-each (lambda (expect-id expect-action str)
                  (receive (id action)
                      (id&action str)
                    (assert-equal expect-id id)
                    (assert-equal expect-action action)))
                '((0 "" "")
                  (3 "" "3")
                  (3 "abc" "[abc]3")
                  (0 "abc" "[abc]")
                  (0 "abc" "  [ abc ]  ")
                  (33 "abc" "[ abc ] 33")
                  (33 "abc" " [ abc ] 33 ")))))

