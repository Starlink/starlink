;;;###autoload
(defmacro edstar-make-virtual (fun args &optional doc interactive)
"Purpose:
   Turn a function into a \"virtual\" function.

Synopsis:
   (edstar-make-virtual fun args &optional doc interactive)

Description:
   This macro allows the creation of a virtual function (c.f. edstar-virtual)
   from an existing normal (non-virtual) function. The starting function may
   have been created using defun, or may be a built-in emacs function.

   Once a function has been converted into a virtual function, its
   implementation may be locally overridden using edstar-override (q.v.).

Arguments:
   fun
      The name of the function which is to be made virtual.
   args
      The argument list. This must match the argumant list of the original
      function.
   doc
      An optional documentation string. If supplied, it will override the
      function's original documentation string, if any.
   interactive
      An optional \"(interactive)\" declaration. If supplied, it will override
      the function's original declaration, if any. It may often be necessary
      to supply this declaration because it may not be possible to obtain the
      original one from the function supplied (e.g. if it is not a lisp
      function). If the function is interactively-callable, but no
      \"(interactive)\" declaration can be obtained, then a default one is
      used, although this may not always be appropriate.

Notes:
   - If using autoloading, you must ensure that the function being made
   virtual is loaded before using this macro. If this is not done, subsequent
   autoloading may cause the new definition to be over-written.
"

;;; Local variables.
  (let (arg                     ; Argument name
        arglist                 ; List of individual arguments
        function                ; Lambda expression for user interface
        (rest nil)              ; Argument list contains &rest?
        (restlist nil)          ; Name of &rest argument list
        value)                  ; Function implementation

;;; Obtain the function implementation from the function symbol's function
;;; cell.
    (setq value (symbol-function fun))

;;; Obtain the documentation string, if not supplied as an argument.
    (if (not doc) (setq doc (documentation fun t)))

;;; Test if the function can be called interactively. If so, and an
;;; "(interactive)" declaration has not been given as an argument, then
;;; attempt to extract it from the function.
    (if (not interactive)
        (progn
          (setq interactive (commandp value))

;;; If the result is non-nil, but is not a list starting with 'interactive,
;;; then provide a default declaration.
          (if (and interactive
                   (not (and (listp interactive)
                             (equal (elt interactive 0) 'interactive))))
              (setq interactive '(interactive)))))

;;; Start building a list which will become a lambda expression to invoke
;;; the function. This will be the user-callable interface.
    (setq function (list 'lambda args))

;;; If available, append the documentation string.
    (if doc (setq function (append function (list doc))))

;;; Also append the "(interactive)" declaration, if available.
    (if interactive (setq function (append function (list interactive))))

;;; Now construct the list of arguments to be passed to the function.
;;; Initialise this to an empty list and loop to inspect each element of the
;;; argument list supplied.
    (setq arglist (list 'list))
    (while args
      (progn
        (setq arg (car args))

;;; Note if the &rest element is found. This flags the start of a trailing
;;; variable argument list. Discard this element.
        (if (equal arg '&rest )
            (setq rest t)

;;; Save the name of the argument that follows the &rest element. This
;;; represents a list of individual arguments (of unknown length).
          (if rest
              (setq restlist arg)

;;; All previous elements represent single arguments, apart from the &optional
;;; element (which is simply discarded). Form a list of the argument names.
            (if (not (equal arg '&optional))
                (setq arglist (append arglist (list arg))))))

;;; Return to consider the next argument list element.
        (setq args (cdr args))))

;;; Form an expression which evaluates to a list representing the complete set
;;; of arguments by concatenating the list of individual argument names and the
;;; (optional) variable argument list which follows.
    (setq arglist (list 'append arglist restlist))

;;; Complete the lambda expression which provides the user-interface by
;;; appending an element which contains a list which uses "apply" to invoke
;;; the function implementation on the complete argument list.
    (setq function (append function (list (list 'apply fun arglist))))

;;; We now evaluate the macro's return value, which consists of a list
;;; of expressions to be evaluated.
    (list 'progn

;;; The first expression sets the virtual function's value cell to the function
;;; implementation. The virtual function may subsequently be overridden by
;;; substituting a new implementation in place of this.
          (list 'set (list 'quote fun) (list 'quote value))

;;; The second expression sets the corresponding function cell to be the lambda
;;; expression which provides the user interface and invokes the function.
          (list 'fset (list 'quote fun) (list 'quote function))

;;; The final expression is the returned value when the macro is invoked. It
;;; evaluates to the function symbol being defined (i.e. like defun).
          (list 'quote fun))))
