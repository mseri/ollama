(lang dune 2.9)
(name ollama)
(version 0.1.0)

(generate_opam_files true)

(source (github yourusername/ollama_client))
(license MIT)
(authors "Your Name")
(maintainers "Your Name")

(package
 (name ollama)
 (synopsis "An Ollama client library for OCaml")
 (description "A client library for interacting with Ollama API in OCaml")
 (depends
  (ocaml (>= 4.08))
  dune
  lwt
  cohttp
  cohttp-lwt-unix
  yojson
  )
 )
