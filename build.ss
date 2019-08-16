#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/make)

(def cc-options
  "-I/usr/local/include -I/usr/include")
(def ld-options
  "-L/usr/local/lib -L/usr/lib -lpthread -lunicorn")

(def build-spec
  `((gxc: "core"
          "-cc-options" ,cc-options
          "-ld-options" ,ld-options)
    (gxc: "arm64"
          "-cc-options" ,cc-options
          "-ld-options" ,ld-options)
    (gxc: "arm"
          "-cc-options" ,cc-options
          "-ld-options" ,ld-options)
    (gxc: "x86"
          "-cc-options" ,cc-options
          "-ld-options" ,ld-options)))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (main . args)
  (match args
    (["deps"]
     (let (build-deps (make-depgraph/spec build-spec))
       (call-with-output-file "build-deps" (cut write build-deps <>))))
    ([]
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             debug: 'src
             optimize: #f
             static: #f
             depgraph: depgraph
             verbose: #t
             build-spec)))))
