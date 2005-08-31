;;;###autoload
(defmacro edstar-virtual (fun args &rest body)
"Purpose:
   Define a new \"virtual function\" for emacs.

Synopsis:
   (edstar-virtual fun args &rest body)

Description:
   This macro allows the creation of virtual functions. These may be used just
   like normal functions (created using defun), except that their lisp
   implementation may later be locally overridden. (This differs from simply
   using defun a second time to re-define the function, since this would
   modify the function permanently, with global scope, and destroy the original
   definition.)

   With a virtual function, the original global definition remains in place
   when the function is overridden and only the local definition (stored in a
   suitable local variable) is changed. This allows the function to be adapted
   for more specialised purposes on, say, a per-buffer basis, without affecting
   its use elsewhere.

   The term \"virtual function\" is object-oriented terminology for a function
   whose interface may be inherited from a base class by a more specialised
   class, but whose implementation may be overridden to provide more
   specialised (and appropriate) behaviour for a particular type of data. The
   use in emacs is very similar and fulfils the same purpose, except that the
   context within which the function is overridden will normally be an editing
   buffer or a (let...) form.

   To override a virtual function, you must create a suitable local variable
   with the same name as the function and then use edstar-override.

Arguments:
   fun
      The name of the virtual function.
   args
      The argument list.
   body
      The body of the function.

Notes:
   - This macro is used exactly like defun and takes the same arguments (but
   see below for differences associated with loading).

   - If using autoloading, you must ensure that any use of edstar-virtual
   is loaded before the virtual function it defines is overridden. If this
   is not done, subsequent autoloading may cause the new definition to be
   over-written.

   - Note that ;;;###autoload can be used to perform autoloading of a virtual
   function, but will result in the function's complete implementation being
   copied into the \"loaddefs.el\" file (unlike defun, which only generates
   an instruction to load the necessary file). This means that \"loaddefs.el\"
   must be updated if the function is changed. The need to do this can be
   minimised, however, if the function's implementation is encapsulated in a
   second (autoloaded) function.
"

;;; Local variables.
  (let (arg                     ; Argument name
        arglist                 ; List of individual arguments
        (doc nil)               ; Documentation string
        function                ; Lambda expression for user interface
        (interactive nil)       ; "(interactive)" declaration
        (rest nil)              ; Argument list contains &rest?
        (restlist nil)          ; Name of &rest argument list
        value)                  ; Lambda expression for function implementation

;;; Determine if the function body starts with a documentation string. If so,
;;; extract it, removing it from the function body.
    (if (stringp (car body))
        (progn
          (setq doc (car body))
          (setq body (cdr body))))

;;; Create a preliminary lambda expression to implement the function.
    (setq value (append (list 'lambda args) body))

;;; Test if this expression can be called interactively. If so, extract the
;;; "(interactive)" declaration, removing it from the start of the function
;;; body.
    (if (commandp value)
        (progn
          (setq interactive (car body))
          (setq body (cdr body))))

;;; Now form a new lambda expression to implement the function, this time
;;; without the "(interactive)" declaration.
    (setq value (append (list 'lambda args) body))

;;; Start building a list which will become a lambda expression to invoke
;;; the one created above. This second list will be the user-callable interface
;;; to the function's implementation.
    (setq function (list 'lambda args))

;;; If available, append the documentation string.
    (if doc (setq function (append function (list doc))))

;;; Also append the "(interactive)" declaration, if available.
    (if interactive (setq function (append function (list interactive))))

;;; Now construct the list of arguments to be passed to the lambda expression
;;; which implements the function. Initialise this to an empty list and loop
;;; to inspect each element of the argument list supplied.
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

;;; The first expression sets the virtual function's value cell to the lambda
;;; expression which implements the function. The virtual function may
;;; subsequently be overridden by substituting a new lambda expression in
;;; place of this.
          (list 'set (list 'quote fun) (list 'quote value))

;;; The second expression sets the corresponding function cell to be the lambda
;;; expression which provides the user interface and invokes the expression
;;; above.
          (list 'fset (list 'quote fun) (list 'quote function))

;;; The final expression is the returned value when the macro is invoked. It
;;; evaluates to the function symbol being defined (i.e. like defun).
          (list 'quote fun))))
