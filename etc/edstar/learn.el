;;;###autoload (defvar edstar-macro-key nil "Key to be used for current keyboard macro")

;;;###autoload
(defun learn (key)
"
Purpose:
   Define a keyboard macro (learn sequence).

Description:
   The function records a sequence of key presses and/or commands (a keyboard
   macro) and binds them to a key, so that they may be re-invoked by pressing
   that key. The sequence of events in defining a macro is as follows:

   1) Unless you specify a key when you invoke the \"learn\" function (you
   will not normally do so), you are first prompted to press the key which
   will be used to invoke the macro. Choose any key, or key sequence (using
   the GOLD- key for example), that you do not subsequently want to use for
   its original purpose.

   2) You should then press the keys and/or enter the commands which you
   want invoked. These will be executed as you enter them, so you can see
   their effect.

   3) When your definition is complete, press the original key (the one
   you pressed in step 1). This will terminate the learn sequence and cause
   this key to invoke the macro you have defined when you next press it.
"
   (interactive "kPress the key for which you want to define a macro:")

;;; Save the key being defined.
   (setq edstar-macro-key key)

;;; Re-bind the key to the function which ends a macro definition.
   (global-set-key key 'edstar-end-learn)

;;; Start defining the macro.
   (start-kbd-macro nil)

;;; Issue a message about how to end the definition.
   (message (concat "Enter macro commands...  Press "
		    (key-description key) " to end macro definition")))


;;;###autoload
(defun edstar-end-learn ()
"
Purpose:
   End a keyboard macro definition (learn sequence).

Description:
   The function ends a keyboard macro definition begun by the \"learn\"
   command.

Notes:
   This function should not normally be invoked by a user except by pressing
   the macro kay at the end of defining a macro definition.

"
  (interactive)

;;; Finish defining the macro.
  (end-kbd-macro nil)

;;; Bind the macro key to the macro's definition.
  (global-set-key edstar-macro-key last-kbd-macro))
