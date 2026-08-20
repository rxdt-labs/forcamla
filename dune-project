(lang dune 3.23)

(name forcamla)

(generate_opam_files true)

(source
 (github thyrgle/forcamla))

(authors "Christopher Sumnicht <csumnicht@berkeley.edu>")

(maintainers "Christopher Sumnicht <csumnicht@berkeley.edu>")

(license Apache-2.0)

(documentation https://thyrgle.github.io/forcamla/)

(package
 (name forcamla)
 (version 0.3.0)
 (synopsis "Organize program execution via formulae")
 (description "Formulas are either terms or compound statements (terms combined with binary operators). Terms may be updated and then compound statements will reflect the changed updated terms. With this event listeners can be constructed that wait for formulas to be satisfied, changed, etc.")
 (depends 
   (ocaml (>= 5.0.0))
   dune
   (alcotest :with-test)
   (odoc :with-doc))
 (tags
  ("mathematical programming" "formula" "event listeners")))
