;; (asdf:oos 'asdf:load-op :gbbopen)

(asdf:defsystem #:contextl
  :name "ContextL"
  :author "Pascal Costanza"
  :version "0.52"
  :licence "
Copyright (c) 2005 - 2008 Pascal Costanza

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
"
  :depends-on (#:closer-mop #-lispworks #:lw-compat #:portable-threads #:trivial-garbage)
  :components ((:file "contextl-packages")
               (:file "cx-util" :depends-on ("contextl-packages"))
               (:file "cx-dynascope" :depends-on ("contextl-packages"))
               (:file "cx-special-class" :depends-on ("cx-dynascope"))
               (:file "cx-singleton-class" :depends-on ("contextl-packages" "cx-util"))
               (:file "cx-layered-function-macros" :depends-on ("contextl-packages" "cx-util"))
               (:file "cx-layer-metaclasses" :depends-on ("cx-special-class" "cx-singleton-class"))
               (:file "cx-gc" :depends-on ("cx-layer-metaclasses" "cx-layered-function-macros"))
               (:file "cx-layer" :depends-on ("cx-layer-metaclasses" "cx-layered-function-macros" "cx-gc" "cx-util"))
               (:file "cx-partial-class" :depends-on ("cx-layer"))
               (:file "cx-class-in-layer" :depends-on ("cx-layer"))
               (:file "cx-layered-function" :depends-on ("cx-layer" "cx-util"))
               (:file "cx-layered-access-class" :depends-on ("cx-layered-function"))
               (:file "cx-layered-class" :depends-on ("cx-layered-access-class" "cx-partial-class"))))
