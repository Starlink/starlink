
(defun convert-itcl ()
      "do the simple part to convert from itcl-1.5 to itcl-2.0"
      (interactive)

      (goto-char (point-min))
      (replace-string "itcl_class" "class")

      (goto-char (point-min))
      (replace-string "$this." "$w_.")

      (goto-char (point-min))
      (replace-string "{config}" "{args}")

      (goto-char (point-min))
      (replace-string "global " "global ::")

      (goto-char (point-min))
      (replace-regexp "[A-Za-z0-9_]+::constructor" "eval itk_initialize $args")

      (goto-char (point-min))
      (replace-regexp "\\(^[ \t]+\\)public \\([A-Za-z0-9_]+\\)" "\\1itk_option define -\\2 \\2 \\2")

      (goto-char (point-min))
      (replace-regexp "\\(^[ \\t]+\\)protected \\([A-Za-z0-9_]+\\)" "\\1protected variable \\2")

      (goto-char (point-min))
      (replace-regexp "\"\\(\\$this [^\"]+\\)\"" "[code \\1]")

      (goto-char (point-min))
      (replace-regexp "\"\\(\\+\\$this [^\"]+\\)\"" "\"+[code \\1]\"")

      (goto-char (point-min))
)
