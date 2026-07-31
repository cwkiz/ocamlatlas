(lang dune 3.21)

(name ocamlatlas)

(generate_opam_files true)

(source
 (github cwkiz/ocamlatlas))

(authors "Demetrius <illegalbassmedicine@proton.me>")

(maintainers "Demetrius <illegalbassmedicine@proton.me>")

(license LICENSE)

(package
 (name ocamlatlas)
 (synopsis "Finite Group Representations utilities for OCaml.")
 (description "It's a CAS for finite group representations in OCaml")
 (depends ocaml)
 (tags
  ("finitegroup" "algebra" "cas" "atlasoffinitegroups")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
