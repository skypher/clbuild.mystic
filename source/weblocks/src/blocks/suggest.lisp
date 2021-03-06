
(in-package :weblocks)

(export '(render-suggest))

(defun render-suggest (input-name localp &key value fetch-fn
		       (input-id (gensym))
		       (choices-id (gensym))
		       (format-fn #'format-suggest-list))
  "Renders a block that provides functionality similar to google-suggest.

input-name - the 'name' attribute of the input box.

localp - If nil, the completion will be done via an asynchronious call
to the server. Otherwise, must be set to a list of items which will be
sent to the client for local autocompletion. In this case, the value
of 'fetch-action' is ignored. Note, if JavaScript is turned off local
autocompletion will degrade to a simple drowdown.

value - can be used to set the value of the input box and/or dropdown.

fetch-fn - when 'localp' is false, a function that accepts a single
argument (the string entered by the user) and returns a list of items
that will be sent to the client in an appropriate format.

input-id, choices-id - optional IDs of input control and choice div,
respectively. If an ID isn't provided, it will be generated.

format-fn - a function used to format the results into html sent to
the client. Accepts a list of results."
  (let ((a-input-name (attributize-name input-name)))
    (if localp
	(with-html
	  (:select :id input-id :name a-input-name
		   (mapc (lambda (i)
			   (if (string-equal i value)
			       (htm (:option :selected "true" (str i)))
			       (htm (:option (str i)))))
			 localp))
	  (with-javascript (if value
			       "replaceDropdownWithSuggest('~A', '~A', '~A', '~A');"
			       "replaceDropdownWithSuggest('~A', '~A', '~A');")
	    input-id a-input-name choices-id value))
	(with-html
	  (:input :type "text" :id input-id :name a-input-name :class "suggest" :value value)
	  (:div :id choices-id :class "suggest" "")
	  (with-javascript "declareSuggest('~A', '~A', '~A', '~A');"
	    input-id choices-id
	    (make-action
	     (lambda (&rest keys)
	       (funcall format-fn (funcall fetch-fn (request-parameter a-input-name)))))
	    (session-name-string-pair))))))

(defun format-suggest-list (results)
  "Formats a list of results into
HTML that can later be sent to a suggest control on the client."
  (let ((*weblocks-output-stream* (make-string-output-stream)))
    (declare (special *weblocks-output-stream*))
    (with-html
      (:ul
       (mapc (lambda (res)
	       (htm (:li (str res))))
	     results)))
    (get-output-stream-string *weblocks-output-stream*)))

