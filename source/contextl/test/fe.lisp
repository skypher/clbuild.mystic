(asdf:oos 'asdf:load-op :contextl)
(compile-file "figure-editor.lisp")
(load "figure-editor")
(in-package :contextl-user)
(run-test)

#+allegro (excl:exit)
#+cmu (ext:quit)
#+(or openmcl clozure-common-lisp) (ccl:quit)
#+sbcl (sb-ext:quit)
