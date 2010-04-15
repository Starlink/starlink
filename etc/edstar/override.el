;;;###autoload
(defmacro edstar-override (fun args &rest body)
"Purpose:
   Override a \"virtual function\".

Synopsis:
   (edstar-override fun args &rest body)

Description:
   This macro allows a virtual function created with edstar-virtual (q.v.) to
   be overridden by giving it a new implementation (the calling interface
   remains the same).

   The usual way of using this macro is first to create a local version of a
   variable with the same name as the virtual function (either using let or
   make-local-variable) and then to provide a new local implementation of the
   function with edstar-override. The new implementation will then be in effect
   wherever the local variable is within scope, while the previous
   implementation will remain in effect elsewhere.

   Normally, you do not want the original global implementation of a virtual
   function to be affected by edstar-override. However, if you do not create a
   appropriate local variable first, you will be modifying whatever version of
   the variable is within scope at that point. If this is the global version,
   then the original implementation will be destroyed. This is a legitimate
   use of edstar-override.

Arguments:
   fun
      The name of the virtual function to be overridden.
   args
      The argument list. This should match the argument list given in the
      original invocation of edstar-virtual.
   body
      The body of the function. Any documentation string or \"(interactive)\"
      declaration is ignored, since the user-interface is defined by the
      original use of edstar-virtual and is not changed by edstar-override.

Notes:
   - This macro is used like defun and takes the same arguments.

   - You should not normally autoload an invocation of this macro, since its
   purpose is to override a virtual function (normally itself autoloaded) only
   at an appropriate point during execution. Normally, edstar-override will
   be invoked from the body of another function.
"

;;; Local variables.
  (let (value)                  ; Lambda expression for function implementation

;;; Determine if the function body starts with a documentation string. If so,
;;; remove it.
    (if (stringp (car body)) (setq body (cdr body)))

;;; Create a preliminary lambda expression to implement the function.
    (setq value (append (list 'lambda args) body))

;;; Test if this expression can be called interactively. If so, remove the
;;; "(interactive)" declaration from the start of the function body.
    (if (commandp value) (setq body (cdr body)))

;;; Now form a new lambda expression to implement the function, this time
;;; without the "(interactive)" declaration.
    (setq value (append (list 'lambda args) body))

;;; We now evaluate the macro's return value, which consists of a list
;;; of expressions to be evaluated.
    (list 'progn

;;; The first expression sets the virtual function's value cell to the lambda
;;; expression which implements the function, overriding the previous
;;; implementation. The function cell is not changed.
          (list 'set (list 'quote fun) (list 'quote value))

;;; The second expression is the returned value when the macro is invoked. It
;;; evaluates to the function symbol being overridden (i.e. like defun).
          (list 'quote fun))))
