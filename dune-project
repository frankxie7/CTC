(lang dune 3.16)

(name game)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)


(package
 (name game)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml tsdl tsdl-image dune))

(package
 (name lib)
 (synopsis "Library for game modules")
 (description "Internal utility modules for the game project")
 (depends ocaml tsdl tsdl-image dune))