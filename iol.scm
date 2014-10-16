;; Copyright 2014 Ian Denhardt <ian@zenhack.net>
;;
;; This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>

;; iol is a simple utility module for building up string outputs efficiently.
;;
;; iol stands for "IO-List," an idea borrowed from erlang. Basically, and iol
;; is either a list, or an object which may be formatted (via "~a").
;; Procedures which output iols in some fashion implictly flatten the iol as
;; they go; '(("Hello" ",") ("World") "!") is the same as "Hello, World!".
;;
;; iols allow for O(1) concatenation, which is nice when building up large
;; strings.
(define-module (iol)
  #:export (iol->string
            iter-iol
            display-iol))

(define (iol->string iol)
  "Convert iol to a string."
  (if (list? iol)
    (apply string-append (map iol->string iol))
    (format #f "~a" iol)))

(define (iter-iol f iol)
  "Apply f to each leaf of the iol.

  It is bad style for f to care about how the input is divided up. The main
  use-case for this is marshalling the iol. For example, display-iol is
  implemented in terms of iter-iol.

  iter-iol always returns #t.
  "
  (if (list? iol)
    (map (lambda (iol*) (iter-iol f iol*)) iol)
    (f iol))
  #t)

(define (display-iol iol)
  "Output iol to current-output-port."
  (iter-iol (lambda (str) (format #t "~a" str)) iol))
