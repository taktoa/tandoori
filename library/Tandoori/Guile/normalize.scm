;;; -*- mode: scheme; coding: utf-8 -*-
;;; File: library/Tandoori/Guile/normalize.scm
;;
;;; License:
;; Copyright © 2015 Remy Goldschmidt <taktoa@gmail.com>
;;
;; This file is part of typed-guile.
;;
;; typed-guile is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; typed-guile is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with typed-guile. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Author:     Remy Goldschmidt <taktoa@gmail.com>
;;; Maintainer: Remy Goldschmidt <taktoa@gmail.com>
;;
;;; Homepage:   https://github.com/taktoa/typed-guile
;;
;;; Commentary:
;;+; Normalizes Scheme code such that it is easy for Haskell to parse.
;;
;;; Code:

(define-module (typed-guile normalize)
  #:version    (0 0 1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi  srfi-26)
  #:use-module (oop   goops)
  #:export     (<location>
                normalize-read
                main))

(define* (natural? value)
  "Is the given @var{value} a natural number?"
  (and (integer? value)
       (or (zero?     value)
           (positive? value))))

(define-class <collection> () (collection #:getter get))

(define-method (->collection (list       <list>))
  (make <collection> list))
(define-method (->collection (vector     <vector>))
  (make <collection> vector))
(define-method (->collection (string     <string>))
  (make <collection> string))
(define-method (->collection (bitvector  <bitvector>))
  (make <collection> bitvector))
(define-method (->collection (bytevector <bytevector>))
  (make <collection> bytevector))

(define* (gcoll-dispatch collection)
  (match collection
    ['(_ ...)      'list]
    [#(_ ...)      'vector]
    [(? string? _) 'string]
    [_             (error "generic-ref: invalid collection: ~s"
                          collection)]))

(define* (gcoll-ref collection k)
  "Reference the @var{k}th element of a given @var{collection}."
  (if (natural? k)
      (match collection
        ['(_ ...)      (list-ref   collection k)]
        [#(_ ...)      (vector-ref collection k)]
        [(? string? _) (string-ref collection k)]
        [_             (error "generic-ref: invalid collection: ~s"
                              collection)])
      (error "generic-ref: invalid index: ~s" k)))

(define* (other-map procedure collection)
  "Map a @var{procedure} over the given @var{vector}."
  (let* ([len  (gcoll-length vector)]
         [newv (make-gcoll len coll)]
         [run  (λ [k] (procedure (gcoll-ref vector k)))])
    (let loop ([i 0])
      (vector-set! newv i (run i))
      (when (< i len) (loop (+ i 1))))
    newv))

(define* (gcoll-map proc value)
  "docstring"
  (cond [(list? value)      (map        proc value)]
        [(string? value)    (string-map proc value)]
        [(vector? value)    (vector-map proc value)]
        [(bitvector? value) '()]))



(define* (read-opt-set! option value)
  "If @var{value} is true, enable @var{option} (a quoted symbol).
If @var{value} is false, disable @var{option}. Otherwise, throw an error."
  (if (boolean? value)
      (if value
          (read-enable  option)
          (read-disable option))
      (error "read-opt-set!: non-boolean value")))

(define* (write-string value)
  "Write the given value to a string."
  (with-output-to-string
    (λ [] (write value))))

(define* (reset-read-options! opts)
  "This allows you to reset the reader options such that they match the result
of a previous invocation of @code{(read-options)}, where @var{opts} is a list
produced by running @code{(read-options)}."
  (for-each read-disable '(copy positions case-insensitive
                           r6rs-hex-escapes square-brackets
                           hungry-eol-escapes curly-infix
                           r7rs-symbols))
  (read-set! keywords #f)
  (letrec ([recurse (match-lambda
                      ['()                     '()]
                      [`(keywords ,kw . ,rest) (begin (read-set! keywords kw)
                                                      (recurse rest))]
                      [`(,name        . ,rest) (begin (read-enable name)
                                                      (recurse rest))])])
    (recurse opts)))

(define* (call-with-read-options thunk
                                 #:key
                                 [copy               #f] [positions       #t]
                                 [case-insensitive   #f] [keywords        #f]
                                 [r6rs-hex-escapes   #f] [square-brackets #t]
                                 [hungry-eol-escapes #f] [curly-infix     #f]
                                 [r7rs-symbols       #f])
  "docstring"
  (let ([syntax-options (read-options)])
    (read-opt-set! 'copy               copy)
    (read-opt-set! 'positions          positions)
    (read-opt-set! 'case-insensitive   case-insensitive)
    (read-set!      keywords           keywords)
    (read-opt-set! 'r6rs-hex-escapes   r6rs-hex-escapes)
    (read-opt-set! 'square-brackets    square-brackets)
    (read-opt-set! 'hungry-eol-escapes hungry-eol-escapes)
    (read-opt-set! 'curly-infix        curly-infix)
    (read-opt-set! 'r7rs-symbols       r7rs-symbols)
    (let ([result (thunk)])
      (reset-read-options! syntax-options)
      result)))

(define* (read-with-options #:optional port
                            #:key
                            [copy               #f] [positions       #t]
                            [case-insensitive   #f] [keywords        #f]
                            [r6rs-hex-escapes   #f] [square-brackets #t]
                            [hungry-eol-escapes #f] [curly-infix     #f]
                            [r7rs-symbols       #f])
  "docstring"
  (call-with-read-options
   (λ [] (if port (read port) (read)))
   #:copy               copy                #:positions          positions
   #:case-insensitive   case-insensitive    #:keywords           keywords
   #:r6rs-hex-escapes   r6rs-hex-escapes    #:square-brackets    square-brackets
   #:hungry-eol-escapes hungry-eol-escapes  #:curly-infix        curly-infix
   #:r7rs-symbols       r7rs-symbols))

(define* (normalize-read string
                         #:key
                         [case-insensitive #f]
                         [r6rs-hex-escapes #f])
  "docstring"
  (let ([opt-read (cut read-with-options <>
                       #:keywords         'postfix
                       #:positions        #t
                       #:square-brackets  #t
                       #:curly-infix      #t
                       #:r7rs-symbols     #t
                       #:case-insensitive case-insensitive
                       #:r6rs-hex-escapes r6rs-hex-escapes)])
    (call-with-input-string string opt-read)))

(define-class <located> (<class>)
  (line-number   #:init-keyword #:line-number
                 #:getter       get-line-number)
  (column-number #:init-keyword #:column-number
                 #:getter       get-column-number)
  (file-name     #:init-keyword #:file-name
                 #:getter       get-file-name)
  (value         #:init-keyword #:value
                 #:getter       get-value))

(define*-public (make-located props val)
  (match props
    [(? source-props? `((line . ,l) (column . ,c) (filename . ,fn)))
     (make <located>
       #:line-number   l
       #:column-number c
       #:file-name     fn
       #:value         val)]
    [_ (error "make-located: invalid properties")]))

(define*-public (annotate props value)
  (if (null? props)
      (gen-value)
      (make-located props value)))

(define* (or-false? pred)
  (λ [x] (if x (pred x) #t)))

(define*-public (source-props? value)
  (match value
    [`((line     . ,(? integer?            _))
       (column   . ,(? integer?            _))
       (filename . ,(? (or-false? string?) _))) #t]
    [_                                          #f]))

(define*-public (annotation? value)
  (and (list? value)
       (eq? (length value) 3)
       (eq? (car    value) 'source-annotation)
       (source-props? (cadr value))))

(define*-public (annotate-value value)
  "docstring"
  (let* ([props (source-properties value)]
         [ann   (cut annotate props <>)])
    (cond [(annotation? value) value]
          [(symbol? value)     (ann value)]
          [(string? value)     (ann value)]
          [(number? value)     (ann value)]
          [(boolean? value)    (ann value)]
          [(char? value)       (ann value)]
          [(null? value)       (ann value)]
          [(vector? value)     (ann (vector-map annotate-value value))]
          [(list? value)       (ann (map        annotate-value value))])))

;; (format #t "DBG: symbol  : ~s : ~s ~%" value (ann value))
;; (format #t "DBG: string  : ~s : ~s ~%" value (ann value))
;; (format #t "DBG: number  : ~s : ~s ~%" value (ann value))
;; (format #t "DBG: boolean : ~s : ~s ~%" value (ann value))
;; (format #t "DBG: char    : ~s : ~s ~%" value (ann value))
;; (format #t "DBG: null    : ~s : ~s ~%" value (ann value))
;; (format #t "DBG: vector  : ~s : ~s ~%" value (ann value))
;; (format #t "DBG: list    : ~s : ~s ~%" value (ann value))

(define*-public (read-annotate string)
  "docstring"
  (annotate-value (normalize-read string)))

(define* (main #:rest args)
  "docstring"
  'hi)

(define*-public all-classes
  '(<%memoized> <<abort>> <<applicable-struct-vtable>>
<<applicable-struct-with-setter-vtable>> <<arity-info>> <<binding-info>>
<<counter>> <<debug>> <<dynref>> <<dynset>> <<dynwind>> <<fix>> <<future>>
<<glil-bind>> <<glil-branch>> <<glil-call>> <<glil-const>> <<glil-kw-prelude>>
<<glil-label>> <<glil-lexical>> <<glil-module>> <<glil-mv-bind>>
<<glil-mv-call>> <<glil-opt-prelude>> <<glil-program>> <<glil-prompt>>
<<glil-source>> <<glil-std-prelude>> <<glil-toplevel>> <<glil-unbind>>
<<glil-void>> <<language>> <<let-values>> <<operand>> <<parameter>> <<prompt>>
<<reference-graph>> <<repl>> <<standard-vtable>> <<toplevel-info>>
<<trap-state>> <<trap-wrapper>> <<tree-analysis>> <<var>>
<<variable-cache-cell>> <<vlist>> <<warning-type>> <accessor-method> <accessor>
<applicable-struct-class> <applicable-struct-vtable>
<applicable-struct-with-setter-vtable> <applicable-struct> <applicable>
<arbiter> <array> <async> <bitvector> <boolean> <boot-closure> <bytevector>
<catch-closure> <char-set-cursor> <char> <character-set> <class> <complex>
<condition-variable> <continuation> <directory> <double-slot> <dynamic-object>
<dynamic-state> <eval-closure> <extended-accessor>
<extended-generic-with-setter> <extended-generic> <file-input-output-port>
<file-input-port> <file-output-port> <file-port> <float-slot> <fluid>
<foreign-slot> <foreign> <fraction> <frame> <generic-with-setter> <generic>
<guardian> <hashtable> <hidden-slot> <hook> <input-output-port> <input-port>
<int-slot> <integer> <keyword> <list> <macro> <malloc> <memoizer> <method>
<module> <mutex> <null> <number> <objcode> <object> <opaque-slot> <output-port>
<pair> <parameter> <port> <primitive-generic> <print-state> <procedure-class>
<procedure> <promise> <protected-hidden-slot> <protected-opaque-slot>
<protected-read-only-slot> <protected-slot> <random-state> <read-only-slot>
<real> <record-type> <regexp> <scm-slot> <self-slot> <soft-input-output-port>
<soft-input-port> <soft-output-port> <soft-port> <srcprops> <stack>
<standard-vtable> <string-input-output-port> <string-input-port>
<string-output-port> <string-port> <string> <symbol> <thread> <top> <unknown>
<uvec> <vector> <vm-continuation> <vm> <void-input-output-port>
<void-input-port> <void-output-port> <void-port> <winder>))

;; /* Local Variables:                    */
;; /* comment-column: 0                   */
;; /* indent-tabs-mode: nil               */
;; /* geiser-scheme-implementation: guile */
;; /* End:                                */
