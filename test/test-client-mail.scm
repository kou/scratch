#!/usr/bin/env gosh

(use test.unit)

(define-module scratch.client.mail.test
  (use test.unit)
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
                                (parse-body (if (string? body)
                                                (open-input-string body)
                                                body))))
                `(((("a" "b")) "a:b")
                  ((("a" "b")) "a: b  ")
                  ((("a" "b")
                    ("c" "d e f")) "a: b  \nc:  d e f  ")
                  ((("body" "abcde")) "abcde")
                  ((("body" "abcde\nfghij")) "abcde\nfghij")
                  ((("a" "abcde\nfghij")) "a:\nabcde\nfghij")
                  ((("a" "abcde\nfghij")
                    ("b" "xyz")) "a:\nabcde\nfghij\nb: xyz")))))
