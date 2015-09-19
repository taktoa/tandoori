#!/bin/sh
exec env guile -s $0 ${1+"$@"}
!#
;; -*- mode: scheme; coding: utf-8 -*-
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
  #:use-module (ice-9 rdelim)
  #:use-module (srfi  srfi-1)
  #:use-module (srfi  srfi-9)
  #:use-module (srfi  srfi-9 gnu)
  #:use-module (srfi  srfi-26)
  #:use-module (srfi  srfi-43)
  #:use-module (oop   goops)
  #:export     (<located>
                normalize-read
                main))

(define* (natural? value)
  "Is the given @var{value} a natural number?"
  (and (integer? value)
       (or (zero?     value)
           (positive? value))))

(define* (vector-map procedure vector)
  "Map a @var{procedure} over the given @var{vector}."
  (let* ([len (vector-length vector)]
         [new (vector-copy   vector)]
         [run (λ [k] (procedure (vector-ref vector k)))])
    (let loop ([i 0])
      (vector-set! new i (run i))
      (when (< i len) (loop (+ i 1))))
    new))

(define* (read-opt-set! option value)
  "If @var{value} is true, enable @var{option} (a quoted symbol).
If @var{value} is false, disable @var{option}. Otherwise, throw an error."
  (if (boolean? value)
      (if value
          (read-enable  option)
          (read-disable option))
      (error "Error: read-opt-set!: non-boolean value")))

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
    (call-with-input-string (format #f "'(~a)" string) opt-read)))

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
    [_ (error "Error: make-located: invalid properties")]))

(define*-public (located-map proc located)
  "docstring"
  (make <located>
    #:line-number   (get-line-number   located)
    #:column-number (get-column-number located)
    #:file-name     (get-file-name     located)
    #:value         (proc (get-value located))))

(define*-public (annotate props value)
  (if (null? props)
      value
      (make-located props value)))

(define* (convert-located located proc)
  "Convert a @var{located} to an s-expression, and run @var{proc} on the located
s-expression (i.e.: the result of @code{(get-value located)})."
  '())

(define-syntax-rule (list-map proc list)
  "Synonym for @code{map}, provided for name similarity with the other maps."
  (map proc list))

(define-syntax-rule (s++ strings ...)
  "Synonym for @code{string-append}."
  (string-append strings ...))

(define* (oexpr->object oexpr)
  "Convert the given @var{oexpr} to an instance of the relevant class."
  (eval oexpr (current-module)))



(define*-public (render-slot object
                             name
                             #:key init-keyword
                             #:allow-other-keys)
  "Helper function for @code{object->sexpr}."
  (if init-keyword
      `(,(symbol->keyword name)
        ,(slot-ref object name))
      '()))

;; (define-method (represent-initializer (object <top>))
;;   (let ([err (λ [e . a] (error (s++ "Error: object->oexpr: "
;;                                     (apply format #f e a))))])
;;     (err "not overridden for custom initializer: class = " (class-of object))))

(define-method (represent-initializer (object <top>))
  `(@type@ ,(class-of object) @literal@))

(define*-public (object->oexpr object)
  "Convert the given @var{object} to an s-expression that, when evaluated with
GOOPS available, will return a deep copy of the given @var{object}.

This is sort of like a self-evaluating representation for GOOPS objects."
  (let* ([err              (λ [e . a] (error (s++ "Error: object->oexpr: "
                                                  (apply format #f e a))))]
         [cls              (class-of object)]
         [cls-name         (class-name cls)]
         [init-classes     (map (compose car method-specializers)
                                (generic-function-methods initialize))]
         [custom-init?     (λ [c] (any (cut equal? <> c) init-classes))]
         [slots            (class-direct-slots cls)]
         [current-mod      (current-module)]
         [current-mod-name (module-name current-mod)]
         [current-used     (module-uses current-mod)]
         [env              (cons current-mod current-used)]
         [relevant-mods    (filter (cut module-defined? <> cls-name) env)]
         [cls-module       (match relevant-mods
                             ['()     (err "Could not find class ~a" cls-name)]
                             [(c . _) (module-name c)])]
         [mod-ref          (if (equal? cls-module current-mod-name) '@@ '@)])
    (if (custom-init? cls)
        (represent-initializer object)
        `(make (,mod-ref ,cls-module ,cls-name)
           ,@(apply append
                    (map (cut apply render-slot object <>) slots))))))

(define*-public (located->sexpr value #:key [pretty #f])
  "Replace any <located> objects with s-expressions in the given @var{value}."
  (let ([located? (cut is-a? <> <located>)])
    (match value
      [(? located? l) (object->oexpr
                       (located-map located->sexpr l))]
      [(? list?    l) (list-map     located->sexpr l)]
      [(? vector?  v) (vector-map   located->sexpr v)]
      [_                           value])))

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
          [(list? value)       (ann (list-map   annotate-value value))])))

(define*-public (read-file path)
  "Read the file at the given @var{path} and output a string with its contents."
  (call-with-input-file path
    read-string
    #:guess-encoding #t))

(define*-public (read-annotate string)
  "docstring"
  (annotate-value (normalize-read string)))

(define*-public (annotate-file path)
  "Read the file at the given @var{path} and return an annotated and normalized
s-expression corresponding to its contents."
  ((compose located->sexpr read-annotate read-file) path))

(define* (main args)
  "Main entry point for program."
  (write (annotate-file (car args))))

(main (command-line))

;; /* Local Variables:                    */
;; /* comment-column: 0                   */
;; /* indent-tabs-mode: nil               */
;; /* geiser-scheme-implementation: guile */
;; /* End:                                */
