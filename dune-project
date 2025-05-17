(lang dune 3.12)

(name fungi)
(version 0.1.4)

; Generate opam file from this dune-project file
(generate_opam_files false)

; Set the minimum OCaml version required
;(using mdx 0.4)  ; If you plan to have documentation tests
(using directory-targets 0.1)  ; If you need to generate files

; Package definition
(package
 (name fungi)
 (version 0.1.4)
 (synopsis "A persistent functional graph library")
 (description "A graph library based on functional adjacency list with some common graph
algorithms")
 
 ; License must match your LICENSE file
 (license MIT)
 
 ; test
 ; Project maintainer(s)
 (maintainers "Harry K <kituyiharry@github.com>")
 
 ; Source code location (typically GitHub)
 ;(source
 ;(github username/my_library))
 
 ; Documentation location (if you have one)
 ;(documentation "https://url/to/documentation")
 
 ; Dependencies
 (depends
  ; Required dependencies
  (ocaml (>= 5.0.0))
  ;(dune  (>= 3.12))
  
  ; Optional dependencies with versions
  ; base
  (alcotest        :with-test)
  (qcheck-core     :with-test)
  (qcheck-alcotest :with-test)
  (core_bench      :with-test)
  ;(ppx_jane :with-test)  ; Only needed for running tests
  ;(odoc :with-doc)       ; Only needed for documentation generation
  ;(mdx :with-test)       ; Only needed if you use mdx for doc tests
  
  ; Add any other dependencies your library needs
  )
 
 ; Tags help people find your package
 (tags
  (topics "graph" "functional" "networks")))

; Package formatting preferences
(formatting
 (enabled_for ocaml))

; If you use cppo or other preprocessors
;(preprocessor_deps
 ;(file1.txt as file1)
 ;(file2.txt as file2))

; Use the newest OCaml features
;(using menhir 2.1)  ; If you need parser generation
;(using ctypes 0.3)  ; If you have C bindings
