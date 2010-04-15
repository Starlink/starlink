(provide 'env)
(defvar starfort-mode-placeholder-table (make-vector 4095 0))
(let (sym)
  (setq sym (intern "IOSTAT" starfort-mode-placeholder-table))
  (set sym "IOSTAT = {integer_var}")
  (setplist sym '((desc . "IOSTAT = {integer_var}")))

  (setq sym (intern "NOT_EQUAL" starfort-mode-placeholder-table))
  (set sym ".NE.")
  (setplist sym '((desc . ".NE.")))

  (setq sym (intern "GKS$DET" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARRAY_REF" starfort-mode-placeholder-table))
  (set sym '(("integer_array_ref" nil place) ("real_array_ref" nil place) ("logical_array_ref" nil place) ("complex_array_ref" nil place) ("char_array_ref" nil place)))
  (setplist sym '((desc . "Array reference")))

  (setq sym (intern "ELSE" starfort-mode-placeholder-table))
  (set sym "ELSE
{executable_statement}...	")
  (setplist sym '((desc . "ELSE {executable_statement}") (vert . t)))

  (setq sym (intern "ROUTINE_NOTES" starfort-mode-placeholder-table))
  (set sym "-  {noted_item}")
  (setplist sym '((vert . t)))

  (setq sym (intern "DELIM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DVAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MKSSCF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DTOW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BLOCK_DATA_PROLOGUE" starfort-mode-placeholder-table))
  (set sym "\\*	Name:
{routine_name}
\\
\\*	Purpose:
{routine_purpose}
\\
\\*	Language:
{routine_language}
\\
\\*	Type of Module:
BLOCK DATA
\\
\\*	Description:
{routine_description}
\\
\\*	[optional_block_data_items]...
Authors:
{original_author_entry}
\\
\\*	History:
{original_version_entry}
\\
\\*	Bugs:
{note_any_bugs_here}
\\")
  (setplist sym '((desc . "BLOCK DATA prologue template")))

  (setq sym (intern "INDEX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GLOBAL_ACCESS_MODE" starfort-mode-placeholder-table))
  (set sym '(("Read" "") ("Write" "") ("Read and Write" "")))

  (setq sym (intern "GKS$COFL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NUMBER" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR_CONS" starfort-mode-placeholder-table))
  (set sym "'{char}...'")
  (setplist sym '((desc . "Character constant")))

  (setq sym (intern "MESSAG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STATUS_ACCESS_MODE" starfort-mode-placeholder-table))
  (set sym '(("Given and Returned" "") ("Returned" "")))

  (setq sym (intern "ELOC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SUBROUTINE_SECTIONS" starfort-mode-placeholder-table))
  (set sym '(("Arguments" nil token) ("Examples" nil token) ("Pitfalls" nil token) ("Notes" nil token) ("Prior_Requirements" nil token) ("Side_Effects" nil token) ("Algorithm" nil token) ("Accuracy" nil token) ("Timing" nil token) ("Routines_Used" nil token) ("Deficiencies" nil token) ("Machine_Specifics" nil token) ("DIY_Prologue" nil token) ("References" nil token) ("Keywords" nil token) ("Copyright" nil token) ("Global_Constants" nil token) ("Global_Variables" nil token) ("Arguments_Given" nil token) ("Arguments_Given_and_Returned" nil token) ("Arguments_Returned" nil token) ("Status_Argument" nil token) ("External_References" nil token) ("Local_Constants" nil token) ("Local_Variables" nil token) ("Internal_References" nil token) ("Local_Data" nil token)))
  (setplist sym '((desc . "Menu of prologue sections for a subroutine")))

  (setq sym (intern "MSZSF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INDF2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INDF1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DELAY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NDIM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VEC2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VEC1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CCLIST" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NDFS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REFERENCES" starfort-mode-placeholder-table))
  (set sym "References:
{routine_references}...
\\")
  (setplist sym '((desc . "Bibliographic references")))

  (setq sym (intern "GKS$CLSW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MLDR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IMPLIED_DO_LIST" starfort-mode-placeholder-table))
  (set sym "({dlist}, {integer_var} = {integer_exp}, {integer_exp}[, {integer_exp}])")
  (setplist sym '((desc . "Implied do-list")))

  (setq sym (intern "TRUE" starfort-mode-placeholder-table))
  (set sym ".TRUE.")
  (setplist sym '((desc . "Logical true constant")))

  (setq sym (intern "CHARACTER_FIELD_DESCR" starfort-mode-placeholder-table))
  (set sym "A[width]")
  (setplist sym '((desc . "Aw")))

  (setq sym (intern "NDEC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Number of characters to the right of the decimal point.")))

  (setq sym (intern "BLANK_PAR" starfort-mode-placeholder-table))
  (set sym "BLANK = {char_var}")
  (setplist sym '((desc . "BLANK = {char_var}")))

  (setq sym (intern "TXCOLI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "KEYWORDS" starfort-mode-placeholder-table))
  (set sym "Keywords:
{routine_keywords}...
\\")
  (setplist sym '((desc . "Any keywords for bibliographic classification")))

  (setq sym (intern "NDAT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COLIND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCHX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCHH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XNAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WNAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCHD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PMIND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TNAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PNAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FUNCTION_DECLARATIONS" starfort-mode-placeholder-table))
  (set sym "
\\*	Type Definitions:
\\	IMPLICIT NONE              ! No implicit typing
\\
[global_constants]
[global_variables]
[arguments_given]
[arguments_given_and_returned]
[arguments_returned]
[status_argument]
[external_references]
[local_constants]
[local_variables]
[internal_references]
[local_data]")
  (setplist sym '((desc . "FUNCTION declarations template")))

  (setq sym (intern "PMCOLI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PLCOLI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NBND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FNAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOCNUM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DISPLY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NARR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WRITE_PARAMETER" starfort-mode-placeholder-table))
  (set sym '(("REC" "Specify the required record for direct access files" token) ("IOSTAT" "Specify a variable to receive any I/O error code" token) ("ERR" "Specify a statement to branch to if an error occurs" token)))
  (setplist sym '((sep . ", ") (head . ",")))

  (setq sym (intern "AVAIL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NBIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARITH_VAR" starfort-mode-placeholder-table))
  (set sym '(("integer_var" nil place) ("logical_var" nil place) ("real_var" nil place)))
  (setplist sym '((desc . "Numeric variable")))

  (setq sym (intern "MINS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTERNAL_DECLARATION_FILE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter the logical name of the file you wish to include which contains
type declarations for internal functions to be defined later.")))

  (setq sym (intern "SEGTM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DISPID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "\,\ {INTEGER_EXP}" starfort-mode-placeholder-table))
  (set sym ", {integer_exp}")

  (setq sym (intern "LREF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A symbolic name  is  a string of  letters,  digits,  and  the  special
characters dollar sign ($) and underscore (_).   The  first  character
in a symbolic name must be a letter.   The  symbolic  name can contain
a maximum of 31 characters. FORTRAN-77 limits the length of a symbolic
name to 6 characters") (desc . "Symbolic name") (sep . ", ")))

  (setq sym (intern "OLDFLG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INDEXS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOCAL_CONSTANT_SPECIFICATION" starfort-mode-placeholder-table))
  (set sym "{data_type} {constant_name}   ! [constant_description]
\\	PARAMETER ( {constant_name} = {cons} )")
  (setplist sym '((vert . t)))

  (setq sym (intern "DPSI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DO_INCREMENT" starfort-mode-placeholder-table))
  (set sym "{aelm} [{aop} {aelm}]...")
  (setplist sym '((head . ",")))

  (setq sym (intern "FACOLI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FUNCTION_PROLOGUE" starfort-mode-placeholder-table))
  (set sym "\\*	Name:
{routine_name}
\\
\\*	Purpose:
{routine_purpose}
\\
\\*	Language:
{routine_language}
\\
\\*	Invocation:
   RESULT = {routine_name}( [p_]... )
\\
\\*	Description:
{routine_description}
\\
\\*	[arguments]
Returned Value:
{routine_name} = {data_type}
{returned_value_description}
\\
\\*	[optional_function_items]...
Authors:
{original_author_entry}
\\
\\*	History:
{original_version_entry}
\\
\\*	Bugs:
{note_any_bugs_here}
\\")
  (setplist sym '((desc . "FUNCTION prologue template")))

  (setq sym (intern "VAL2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VAL1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "I_CODE" starfort-mode-placeholder-table))
  (set sym "[repeat]I[width][min_char]")
  (setplist sym '((desc . "I{width}")))

  (setq sym (intern "INDEX2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INDEX1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_EXAMPLE_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Describe what the example does. Be sure to explain any \"tricks\" which
may not be obvious to a beginner.")))

  (setq sym (intern "G_CODE" starfort-mode-placeholder-table))
  (set sym "[repeat]G{width}.{ndec}[E{int_cons}]")
  (setplist sym '((desc . "Gw.d[Ee]")))

  (setq sym (intern "RMSMIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "F_CODE" starfort-mode-placeholder-table))
  (set sym "[repeat]F{width}.{ndec}")
  (setplist sym '((desc . "Fw.d")))

  (setq sym (intern "SUBROUTINE_DECLARATIONS" starfort-mode-placeholder-table))
  (set sym "
\\*	Type Definitions:
\\	IMPLICIT NONE              ! No implicit typing
\\
[global_constants]
[global_variables]
[arguments_given]
[arguments_given_and_returned]
[arguments_returned]
[status_argument]
[external_references]
[local_constants]
[local_variables]
[internal_references]
[local_data]")
  (setplist sym '((desc . "SUBROUTINE declarations template")))

  (setq sym (intern "E_CODE" starfort-mode-placeholder-table))
  (set sym "[repeat]E{width}.{ndec}[E{int_cons}]")
  (setplist sym '((desc . "Ew.d[Ee]")))

  (setq sym (intern "D_CODE" starfort-mode-placeholder-table))
  (set sym "[repeat]D{width}.{ndec}")
  (setplist sym '((desc . "Dw.d")))

  (setq sym (intern "ACCESS_PAR" starfort-mode-placeholder-table))
  (set sym "ACCESS = {char_var}")
  (setplist sym '((desc . "ACCESS = {char_var}")))

  (setq sym (intern "SUBSTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PICNAM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EXTERNAL_REFERENCES" starfort-mode-placeholder-table))
  (set sym "\\*	External References:
\\	{external_function_specification}...
")
  (setplist sym '((desc . "Define external function references")))

  (setq sym (intern "CONSTANT_EXP" starfort-mode-placeholder-table))
  (set sym "{name} = {cons}")
  (setplist sym '((desc . "Constant expression") (sep . ", ")))

  (setq sym (intern "{UNSIGN_INT_CONS}\ *" starfort-mode-placeholder-table))
  (set sym "{unsign_int_cons} *")
  (setplist sym '((desc . "Constant value for Data element") (sep . ", ")))

  (setq sym (intern "COMMON_STMT" starfort-mode-placeholder-table))
  (set sym "COMMON /{common_blk_name}/ {common_elm}...")
  (setplist sym '((desc . "COMMON statement") (vert . t)))

  (setq sym (intern "RMSMAX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EXECUTABLE_STATEMENT" starfort-mode-placeholder-table))
  (set sym '(("DO" "DO loop" token) ("IF" "IF ( {logical_exp} ) {executable_statement}" token) ("IF_THEN" "IF ( {logical_exp} ) THEN ... END IF" token) ("WHILE" "DO WHILE loop (standard F77 version)" token) ("READ" "Internal READ statement" token) ("WRITE" "Internal WRITE statement" token) ("FORMAT" "FORMAT statement" token) ("CALL" "CALL statement" token) ("GO" "GO TO {lbl}" token) ("GO_COMPUTED" "Computed GO TO statement" token) ("DO_ENDDO" "DO ... ENDDO (non-standard, worth avoiding)" token) ("DO_WHILE" "DO WHILE ... ENDDO (non-standard, worth avoiding)" token) ("CONTINUE" "CONTINUE statement" token) ("IO_STMT" "I/O statement (use FIO_ and RIO_ routines if possible)" token) ("assignment_arith" nil token) ("assignment_log" nil token) ("assignment_char" nil token)))
  (setplist sym '((desc "FORTRAN executable statements") (vert . t)))

  (setq sym (intern "FILENM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BKWDS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCURS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LTOBS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WKTYP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IDMSF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SUBROUTINE_PROGRAM_MODULE" starfort-mode-placeholder-table))
  (set sym "SUBROUTINE {routine_name}( [p]... )
\\*+
\\*{subroutine_prologue}
\\*-
\\	{subroutine_declarations}
\\*.
\\
[status_check]
[executable_statement]...

END")
  (setplist sym '((desc . "SUBROUTINE program module")))

  (setq sym (intern "BLOCK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TPAR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FILDSC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_DEFICIENCIES" starfort-mode-placeholder-table))
  (set sym "-  {deficiency}")
  (setplist sym '((vert . t)))

  (setq sym (intern "FORM_OPTIONS" starfort-mode-placeholder-table))
  (set sym '(("FORMATTED" "A formatted file") ("UNFORMATTED" "An unformatted file")))
  (setplist sym '((desc . "Format options")))

  (setq sym (intern "ISTTY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YSTART" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PARAMETER_DEFAULT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((desc . "Normal default value for parameter") (help . "If the parameter normally has a default value and is not prompted for,
then indicate the default value here. The value should be placed between
square brackets to separate it from the preceding text, e.g. [3.8].
If the value cannot be given literally (because it is calculated at
run-time, for instance), then use a brief description such as [Number
of image pixels]. Alternatively, use empty brackets [], which indicates
that the way in which the default is calculated is discussed in the
associated description of the parameter's purpose.")))

  (setq sym (intern "MODCON" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NMEMB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XSTART" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARGUMENTS_GIVEN" starfort-mode-placeholder-table))
  (set sym "\\*	Arguments Given:
\\	{declaration_statement}...
")
  (setplist sym '((desc . "Define input arguments")))

  (setq sym (intern "NSTRT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MESS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LESS_THAN" starfort-mode-placeholder-table))
  (set sym ".LT.")
  (setplist sym '((desc . ".LT.")))

  (setq sym (intern "MESLEN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOBS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PROGRAM_MODULE" starfort-mode-placeholder-table))
  (set sym '(("A_task" "ADAM A_task (main routine of an ADAM application)" token) ("subroutine" "SUBROUTINE program module" token) ("function" "FUNCTION program module" token) ("block_data" "BLOCK DATA program module" token) ("monolith" "Top-level ADAM monolith routine" token)))
  (setplist sym '((vert . t) (desc . "Menu of different types of program module")))

  (setq sym (intern "MPLBTE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SUBSCR" starfort-mode-placeholder-table))
  (set sym "{arith_exp}")
  (setplist sym '((desc . "Subscript expression") (sep . ", ")))

  (setq sym (intern "ITTDEP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOC3" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SEQUENTIAL_PAR" starfort-mode-placeholder-table))
  (set sym "SEQUENTIAL = {char_var}")
  (setplist sym '((desc . "SEQUENTIAL = {char_var}")))

  (setq sym (intern "LOC2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOC1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MENU" starfort-mode-placeholder-table))
  (set sym '(("Program_Modules" nil token) ("Prologues" nil token) ("Prologue_Sections" nil token) ("Executable_Statement" nil token) ("ADAM_Constructs" nil token)))
  (setplist sym '((desc . "Main menu of program modules, prologues, statements, etc.")))

  (setq sym (intern "WKSTN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RECSZ" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ISTMP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LSE$BLOCK_COMMENT" starfort-mode-placeholder-table))
  (set sym "
*  [comment]")

  (setq sym (intern "PSTART" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REFERENCE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter a single bibliographic reference.")))

  (setq sym (intern "NSUBS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OUTARR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FUNCTION_PROGRAM_MODULE" starfort-mode-placeholder-table))
  (set sym "{function_type} FUNCTION {routine_name}( [p_]... )
\\*+
\\*{function_prologue}
\\*-
\\	{function_declarations}
\\*.
\\
[executable_statement]...

END")
  (setplist sym '((desc . "FUNCTION program module")))

  (setq sym (intern "TMIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FCTID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RECNO" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STOP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PMBUN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STR2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STR1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CTNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LSTAT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "JSTAT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PARRAY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TMAX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PRIOR_CONDITION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Describe any condition which must be satisfied before the routine is
invoked, but which may not be obvious from the information given
earlier.")))

  (setq sym (intern "ALGORITHMIC_STEP" starfort-mode-placeholder-table))
  (set sym '((help . "Describe each of the important steps in the routine's algorithm.
This information should augment that given in the \"Description\"
section, but will normally be of interest only to a programmer and
will not be extracted to appear in user documentation.  No entry need
be made here if the algorithm is trivial.")))

  (setq sym (intern "INCLUDE_INTERNAL_DEFINITIONS" starfort-mode-placeholder-table))
  (set sym "INCLUDE '{internal_definition_file}'")
  (setplist sym '((vert . t)))

  (setq sym (intern "OPTIONAL_A_TASK_ITEMS" starfort-mode-placeholder-table))
  (set sym '(("A_Task_Options" "Expands to a list of placeholders for all items below" token) ("Pitfalls" nil token) ("Notes" nil token) ("Prior_Requirements" nil token) ("Side_Effects" nil token) ("Algorithm" nil token) ("Accuracy" nil token) ("Timing" nil token) ("Implementation_Status" nil token) ("Routines_Used" nil token) ("Deficiencies" nil token) ("Machine_Specifics" nil token) ("DIY_Prologue" nil token) ("References" nil token) ("Keywords" nil token) ("Copyright" nil token)))
  (setplist sym '((vert . t)))

  (setq sym (intern "LSE$GENERIC" starfort-mode-placeholder-table))
  (set sym "	{executable_statement}...")

  (setq sym (intern "RMATP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RMATN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REAL_VAR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Real variable") (desc . "Real variable")))

  (setq sym (intern "BLKER" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NBYTE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DLIST" starfort-mode-placeholder-table))
  (set sym '(("array_ref" nil place) ("char_substring" nil place) ("implied_do_list" nil place)))

  (setq sym (intern "CLIST" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BADBIT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STAT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YLIMS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NDIMX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PDIMS" starfort-mode-placeholder-table))
  (set sym "( [pdim]... )")
  (setplist sym '((desc . "Array dimension expression") (head . "") (tail . "")))

  (setq sym (intern "GKS$MTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$LTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PLIND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "UBND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DISP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DATA_ELM" starfort-mode-placeholder-table))
  (set sym '(("var" nil place) ("array_name" nil place) ("array_ref" nil place) ("char_substring" nil place) ("implied_do_list" nil place)))
  (setplist sym '((desc . "Data element for DATA statement") (sep . ", ")))

  (setq sym (intern "CHAR_ARRAY_REF" starfort-mode-placeholder-table))
  (set sym "{char_array_name}( {subscr}... )")
  (setplist sym '((desc . "Character Array reference")))

  (setq sym (intern "NCOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IMAGE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DIMX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YTICK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XTICK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DIMU" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DIMS" starfort-mode-placeholder-table))
  (set sym "[lbound] : {ubound}")
  (setplist sym '((desc . "Dimension declarator") (sep . ", ")))

  (setq sym (intern "MALT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DIML" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CONTEXT_MESSAGE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter a brief message explaining what the routine was trying to
accomplish when the error occurred. For instance, a suitable contextual
error message from a routine called SQRT which takes the square root of
a spectrum might be:

   'SQRT: Error taking the square root of a spectrum.'

A contextual message will be qualified by error information reported
earlier (normally from lower-level software), so it need not be too
detailed.") (desc . "Text of a contextual error message")))

  (setq sym (intern "STATUS" starfort-mode-placeholder-table))
  (set sym "STATUS")
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PITFALLS" starfort-mode-placeholder-table))
  (set sym "Pitfalls:
{pitfall_description}...
\\")
  (setplist sym '((desc . "A description of any pitfalls for the unwary")))

  (setq sym (intern "STATUS_ARGUMENT" starfort-mode-placeholder-table))
  (set sym "\\*	Status:
\\	INTEGER STATUS             ! Global status
\\")
  (setplist sym '((desc . "Define the STATUS argument")))

  (setq sym (intern "RYIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTRINSIC_STMT" starfort-mode-placeholder-table))
  (set sym "INTRINSIC {intrinsic_func_name}")
  (setplist sym '((desc . "INTRINSIC statement") (vert . t)))

  (setq sym (intern "NCOMP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCONF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCOLS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DATA_VALUES" starfort-mode-placeholder-table))
  (set sym "[{unsign_int_cons} *] {cons}")
  (setplist sym '((sep . ", ")))

  (setq sym (intern "CHAR_EXP" starfort-mode-placeholder-table))
  (set sym "{char_elm}[//{char_elm}]...")
  (setplist sym '((desc . "Character Expression")))

  (setq sym (intern "NCOLI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MCOLI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MONOLITH_PROLOGUE" starfort-mode-placeholder-table))
  (set sym "\\*	Name:
{routine_name}
\\
\\*	Purpose:
Top-level ADAM monolith routine for the {routine_name} package.
\\
\\*	Language:
Starlink Fortran 77
\\
\\*	Invocation:
CALL {routine_name}( STATUS )
\\
\\*	Description:
This routine obtains the name of the current action and calls the
appropriate routine to perform the specified operation. An error
will be reported and STATUS will be set if the action name is not
recognised.
\\
\\*	Arguments:
STATUS = INTEGER (Given and Returned)
The global status.
\\
\\*	[optional_monolith_items]...
Authors:
{original_author_entry}
\\
\\*	History:
{original_version_entry}
\\
\\*	Bugs:
{note_any_bugs_here}
\\")
  (setplist sym '((desc . "MONOLITH prologue template")))

  (setq sym (intern "RMATPN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "D\[+\ OR\ -\]{N}\.\.\." starfort-mode-placeholder-table))
  (set sym "D[+ or -]{n}...")

  (setq sym (intern "RXIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GREATER_THAN_OR_EQUAL" starfort-mode-placeholder-table))
  (set sym ".GE.")
  (setplist sym '((desc . ".GE.")))

  (setq sym (intern "{AOP}\ {INT_ELM}" starfort-mode-placeholder-table))
  (set sym "{aop} {int_elm}")

  (setq sym (intern "CONS" starfort-mode-placeholder-table))
  (set sym '(("char_cons" nil place) ("int_cons" nil place) ("real_cons" nil place) ("double_cons" nil place) ("log_cons" nil place) ("cmplx_cons" nil place)))
  (setplist sym '((desc . "constant") (sep . ", ")))

  (setq sym (intern "COMP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ACCESS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COMM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MORLOC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FJUST" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NOTE_ANY_BUGS_HERE" starfort-mode-placeholder-table))
  (set sym "-  {description_of_bug}
{note_new_bugs_here}")

  (setq sym (intern "COLI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR_VAR_SUBSTRING" starfort-mode-placeholder-table))
  (set sym "{char_var}( [integer_exp]:[integer_exp] )")
  (setplist sym '((desc . "{char_var}( [integer_exp]:[integer_exp] )")))

  (setq sym (intern "COLA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TEXT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR_ELM" starfort-mode-placeholder-table))
  (set sym '(("char_cons" nil place) ("char_var" nil place) ("char_substring" nil place) ("char_exp" nil place) ("char_func_ref" nil place)))
  (setplist sym '((desc . "Character element")))

  (setq sym (intern "PARAMETER_ACCESS_MODE" starfort-mode-placeholder-table))
  (set sym '(("Read" "") ("Write" "") ("Read and Write" "")))

  (setq sym (intern "DIRECT_PAR" starfort-mode-placeholder-table))
  (set sym "DIRECT = {char_var}")
  (setplist sym '((desc . "DIRECT = {char_var}")))

  (setq sym (intern "ARITH_ARRAY_NAME" starfort-mode-placeholder-table))
  (set sym '(("integer_array_name" nil place) ("real_array_name" nil place) ("logical_array_name" nil place)))

  (setq sym (intern "PARLEN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EXISTS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "JUST" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR_FUNC_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Character function name") (desc . "Character function name")))

  (setq sym (intern "ROUTINE_EXAMPLE" starfort-mode-placeholder-table))
  (set sym "{routine_example_text}
{routine_example_description}")
  (setplist sym '((vert . t)))

  (setq sym (intern "NAMED_PAR" starfort-mode-placeholder-table))
  (set sym "NAMED = {logical_var}")
  (setplist sym '((desc . "NAMED = {logical_var}")))

  (setq sym (intern "FORMATTED_PAR" starfort-mode-placeholder-table))
  (set sym "FORMATTED = {char_var}")
  (setplist sym '((desc . "FORMATTED = {char_var}")))

  (setq sym (intern "DIY_PROLOGUE_TEXT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter the text of your personal prologue item.")))

  (setq sym (intern "PITFALL_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym "-  {pitfall}")
  (setplist sym '((vert . t)))

  (setq sym (intern "DEQX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XLEFT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FORMAT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ORIGINAL_VERSION_ENTRY" starfort-mode-placeholder-table))
  (set sym "{date} ({author_identifier}):
Original version.
{enter_changes_here}")

  (setq sym (intern "VLDNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STDNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CMPT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DEPS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CONSTANT_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a brief description of what the constant represents.") (head . "!")))

  (setq sym (intern "RECL_PAR" starfort-mode-placeholder-table))
  (set sym "RECL = {integer_var}")
  (setplist sym '((desc . "RECL = {integer_var}")))

  (setq sym (intern "IREAD_PARAMETER" starfort-mode-placeholder-table))
  (set sym '(("IOSTAT" "Specify a variable to receive any I/O error code" token) ("END" "Specify a statement to branch to on end-of-file condition" token) ("ERR" "Specify a statement to branch to if an error occurs" token)))
  (setplist sym '((sep . ", ") (head . ",")))

  (setq sym (intern "MSG_TOKEN_ROUTINE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Type the name of a MSG_ routine to be used to define a message token for
the subsequent error message.  If you are unfamiliar with the routines
available, then type SET or FMT followed by cntrl-E to obtain an
appropriate menu from which the full subroutine argument list can be
obtained.") (desc . "Name of a MSG_ routine for setting a message token")))

  (setq sym (intern "MBUFF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ALGORITHM_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym "-  {algorithmic_step}")
  (setplist sym '((vert . t)))

  (setq sym (intern "GKS$INTS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TEMP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FIELD_SPEC" starfort-mode-placeholder-table))
  (set sym '(("integer_field_descr" nil place) ("real_and_complex_field_descr" nil place) ("logical_field_descr" nil place) ("character_field_descr" nil place) ("edit_field_descr" nil place)))
  (setplist sym '((sep . ", ")))

  (setq sym (intern "AUTHOR_IDENTIFIER" starfort-mode-placeholder-table))
  (set sym '(lambda nil (or (getenv "EDSTAR_PERSONAL_USERID") (upcase (user-login-name)))))

  (setq sym (intern "CHARXP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NOTE_NEW_BUGS_HERE" starfort-mode-placeholder-table))
  (set sym "-  {description_of_bug}
{note_new_bugs_here}")

  (setq sym (intern "ACCURACY" starfort-mode-placeholder-table))
  (set sym "Accuracy:
{routine_accuracy}
\\")
  (setplist sym '((desc . "Details of the accuracy achieved")))

  (setq sym (intern "STRING" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LEN2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GLOBAL_VARIABLES" starfort-mode-placeholder-table))
  (set sym "\\*	Global Variables:
\\	{include_global_variables}...
")
  (setplist sym '((desc . "Define global variables")))

  (setq sym (intern "CHARSP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PLBUN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RTNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "{LOP}{LOG_ELM}" starfort-mode-placeholder-table))
  (set sym "{lop}{log_elm}")

  (setq sym (intern "GLOBAL_CONSTANTS_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a brief description of the contents of the global constants file.") (head . "!")))

  (setq sym (intern "ENTER_CHANGES_HERE" starfort-mode-placeholder-table))
  (set sym "{date} ({author_identifier}):
{changes}
{enter_further_changes_here}")

  (setq sym (intern "INCLUDE_GLOBAL_VARIABLES" starfort-mode-placeholder-table))
  (set sym "INCLUDE '{global_variables_file}' ! [global_variables_description]
\\*	{descriptions_of_global_variables_referenced}...
\\")
  (setplist sym '((vert . t) (head . "
")))

  (setq sym (intern "DECZ" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YSIZE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XSIZE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARGUMENTS_RETURNED" starfort-mode-placeholder-table))
  (set sym "\\*	Arguments Returned:
\\	{declaration_statement}...
")
  (setplist sym '((desc . "Define output arguments")))

  (setq sym (intern "ADDED" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TSIZE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CLIP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COMPLEX_VAR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A variable of type COMPLEX") (desc . "Complex variable")))

  (setq sym (intern "EXTERNAL_FUNCTION_SPECIFICATION" starfort-mode-placeholder-table))
  (set sym "[external_declaration]
{data_type} {external_name}   ! [external_description]	")
  (setplist sym '((vert . t)))

  (setq sym (intern "ERRNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARITH_EXP" starfort-mode-placeholder-table))
  (set sym "{aelm} [{aop} {aelm}]...")
  (setplist sym '((desc . "Arithmetic Expression")))

  (setq sym (intern "COPYRIGHT" starfort-mode-placeholder-table))
  (set sym "Copyright:
{routine_copyright}
\\")
  (setplist sym '((desc . "Copyright message")))

  (setq sym (intern "STATE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "START" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SLEN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CLEN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RDATA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ERRIND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CLASS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IDATA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTERNAL_DEFINITION_STATEMENT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A statement such as:

   FUNC( ARG ) = ( ARG - 6 ) ** 2

which defines an internal statement function.")))

  (setq sym (intern "STARLINK_FORTRAN_77" starfort-mode-placeholder-table))
  (set sym "Starlink Fortran 77")

  (setq sym (intern "DDATA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INIPOS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PRINT_FMT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A statement label of a FORMAT statement, or the name of an array,
array element, or character expression containing a run-time format.
An asterisk may be given to specify list-directed output.") (desc . "PRINT statement format specifier")))

  (setq sym (intern "DAYS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SIZE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DECLARATION_STATEMENT" starfort-mode-placeholder-table))
  (set sym "{data_type} {name}[dimensions]")
  (setplist sym '((desc . "Type Declaration") (vert . t)))

  (setq sym (intern "LOCAL_VARIABLES" starfort-mode-placeholder-table))
  (set sym "\\*	Local Variables:
\\	{local_variable_declaration}...
")
  (setplist sym '((desc . "Define local variables")))

  (setq sym (intern "OPTIONAL_MONOLITH_ITEMS" starfort-mode-placeholder-table))
  (set sym '(("Monolith_Options" "Expands to a list of placeholders for all items below" token) ("Pitfalls" nil token) ("Notes" nil token) ("Prior_Requirements" nil token) ("Side_Effects" nil token) ("Routines_Used" nil token) ("Deficiencies" nil token) ("DIY_Prologue" nil token) ("References" nil token) ("Keywords" nil token) ("Copyright" nil token)))
  (setplist sym '((vert . t)))

  (setq sym (intern "ENTER_NEW_AUTHORS_HERE" starfort-mode-placeholder-table))
  (set sym "{author_identifier}: {authors_name} ({affiliation})
{enter_new_authors_here}")

  (setq sym (intern "ROUTINES_USED" starfort-mode-placeholder-table))
  (set sym "External Routines Used:
{facility_or_package}...
\\")
  (setplist sym '((desc . "Details of any external routines used")))

  (setq sym (intern "BLANK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RRMS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LBND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SAVE_ELM" starfort-mode-placeholder-table))
  (set sym '(("var" nil place) ("array_name" nil place) ("common_block_ref" nil place)))
  (setplist sym '((sep . ", ")))

  (setq sym (intern "ARGUMENT_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a description of the function that the argument performs.")))

  (setq sym (intern "DATE" starfort-mode-placeholder-table))
  (set sym '(lambda nil (let (date (time (upcase (current-time-string)))) (setq date (concat (substring time 8 10) "-" (substring time 4 7) "-" (substring time 20 24))) (if (= (aref date 0) 32) (setq date (substring date 1))) date)))
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MAXPT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DATA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LASF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOGICAL_VAR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Logical variable") (desc . "Logical variable")))

  (setq sym (intern "ROUTINE_IMPLEMENTATION_STATUS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give details of the extent to which the current implementation of the
routine supports standard features of the environment and/or data system
in use. For instance, if the routine processes Starlink NDF data
structures, then you might say which components it can handle, whether it
supports \"bad\" pixels, what type of arithmetic is used, etc. It is best
to mention features which ARE supported, rather than listing those which
aren't, since the latter list would need to be updated whenever the
facilities offered by the environment are extended.")))

  (setq sym (intern "IZIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHXP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ICHNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IZOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GROUP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHUY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHUX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FACILITY_OR_PACKAGE" starfort-mode-placeholder-table))
  (set sym "{name_of_facility_or_package}:
{routine_used}...")
  (setplist sym '((vert . t)))

  (setq sym (intern "PLACE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHSP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOGICAL_FIELD_DESCR" starfort-mode-placeholder-table))
  (set sym "L{width}")
  (setplist sym '((desc . "Lw")))

  (setq sym (intern "LOG_CONS" starfort-mode-placeholder-table))
  (set sym '(("true" nil place) ("false" nil place)))
  (setplist sym '((desc . "Logical constant")))

  (setq sym (intern "IAXIS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TVALUE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COLON_FIELD" starfort-mode-placeholder-table))
  (set sym ":")
  (setplist sym '((desc . ":")))

  (setq sym (intern "IYIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RWINDO" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTERNALS_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a brief description of what the internal functions do.") (head . "!")))

  (setq sym (intern "BLOCKDATA_PROGRAM_MODULE" starfort-mode-placeholder-table))
  (set sym "BLOCK DATA {routine_name}
\\*+
\\*{block_data_prologue}
\\*-
\\	{block_data_declarations}
\\*.
\\
END")
  (setplist sym '((desc . "BLOCK DATA program module")))

  (setq sym (intern "RVALUE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "A_TASK_PROLOGUE" starfort-mode-placeholder-table))
  (set sym "\\*	Name:
{routine_name}
\\
\\*	Purpose:
{routine_purpose}
\\
\\*	Language:
{routine_language}
\\
\\*	Type of Module:
ADAM A-task
\\
\\*	Invocation:
CALL {routine_name}( STATUS )
\\
\\*	Arguments:
STATUS = INTEGER (Given and Returned)
The global status.
\\
\\*	Description:
{routine_description}
\\
\\*	[usage]
[ADAM_parameters]
[examples]
[optional_A_task_items]...
Authors:
{original_author_entry}
\\
\\*	History:
{original_version_entry}
\\
\\*	Bugs:
{note_any_bugs_here}
\\")
  (setplist sym '((desc . "ADAM A_task prologue template")))

  (setq sym (intern "SUBPROGRAM_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Subprogram name") (desc . "Subprogram name")))

  (setq sym (intern "OPTIONAL_FUNCTION_ITEMS" starfort-mode-placeholder-table))
  (set sym '(("Function_Options" "Expands to a list of placeholders for all items below" token) ("Examples" nil token) ("Pitfalls" nil token) ("Notes" nil token) ("Prior_Requirements" nil token) ("Side_Effects" nil token) ("Algorithm" nil token) ("Accuracy" nil token) ("Timing" nil token) ("Routines_Used" nil token) ("Deficiencies" nil token) ("Machine_Specifics" nil token) ("DIY_Prologue" nil token) ("References" nil token) ("Keywords" nil token) ("Copyright" nil token)))
  (setplist sym '((vert . t)))

  (setq sym (intern "SIGN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STATUS_ARGUMENT_SPEC" starfort-mode-placeholder-table))
  (set sym "STATUS = INTEGER ({status_access_mode})
   The global status.")

  (setq sym (intern "SGTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCHUNK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CLOSE_STATUS_OPTIONS" starfort-mode-placeholder-table))
  (set sym '(("KEEP" "Keep the file" token) ("DELETE" "Delete the file" token)))
  (setplist sym '((desc . "CLOSE statement STATUS options")))

  (setq sym (intern "ROUTINE_EXAMPLE_TEXT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter the text of an example which illustrates how the routine may be
used. It is often helpful to give several such examples, starting with
a very simple case and working gradually up to a rather complex case.
This is an opportunity to illustrate any special \"tricks\" which may make
a routine easy to use but are not obvious from the descriptions given
elsewhere.")))

  (setq sym (intern "NCHAR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SIDE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CMPLX_CONS_ELM" starfort-mode-placeholder-table))
  (set sym '(("real_cons" nil place) ("int_cons" nil place)))
  (setplist sym '((desc . "Complex*8 constant element")))

  (setq sym (intern "LVALUE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PRIMID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AXIS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "READ_STMT" starfort-mode-placeholder-table))
  (set sym "READ ( {unit}, [fmt], [read_parameter]... ) [io_elm]...")
  (setplist sym '((desc . "READ statement") (vert . t)))

  (setq sym (intern "GLOBAL_CONSTANTS_FILE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter the logical name of the global constants file you wish to include.")))

  (setq sym (intern "IXIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SGPR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ERRFIL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IVALUE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BLINKS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ICHUNK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MACHINE" starfort-mode-placeholder-table))
  (set sym "Machine")
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REJECT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MAXVAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$REGFL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "{AOP}\ {BOUND_ELM}" starfort-mode-placeholder-table))
  (set sym "{aop} {bound_elm}")

  (setq sym (intern "DATA_TYPE" starfort-mode-placeholder-table))
  (set sym '(("INTEGER" nil token) ("REAL" nil token) ("CHARACTER" nil token) ("LOGICAL" nil token) ("DOUBLE_PRECISION" nil token) ("WORD" nil token) ("BYTE" nil token) ("COMPLEX" nil token)))
  (setplist sym '((desc . "Data type")))

  (setq sym (intern "STYLID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$PREC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SGNA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_USED" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter the name of any external routine which is invoked by the routine you
are documenting.") (sep . ", ")))

  (setq sym (intern "MAXTNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DVALUE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CWINDO" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CVALUE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '(quote (help . "Any ascii character")))

  (setq sym (intern "ISDST" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ZOBS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "HIVAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SKDNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DIRECN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LCDNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NEWZON" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NYSUB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ISECT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ERRCLS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BLUE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BNDL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COMPLEX_ARRAY_REF" starfort-mode-placeholder-table))
  (set sym "{complex_array_name}( {subscr}... )")
  (setplist sym '((desc . "Complex Array reference")))

  (setq sym (intern "EQUAL" starfort-mode-placeholder-table))
  (set sym ".EQ.")
  (setplist sym '((desc . ".EQ.")))

  (setq sym (intern "BORDER" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PARCAN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IVAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YTOP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OBJTY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IARY3" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ITNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IARY2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IARY1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GREATER_THAN" starfort-mode-placeholder-table))
  (set sym ".GT.")
  (setplist sym '((desc . ".GT.")))

  (setq sym (intern "ISTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SEED" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SECS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ERROR_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A name identifying the error being reported in such a way that it is
unique within this routine.  In combination with the routine name, the
error report will then be associated with a globally unique character
string which may be used by the error system to identify it. It is
recommended that upper case alpha-numeric characters be used to construct
a meaningful name which describes the particular type of error being
reported (e.g. FAC_OPEN_NONAME).  A maximum of 15 characters in total
(including the subroutine name and underscores) is permitted.")))

  (setq sym (intern "NFRAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REAL_CONS" starfort-mode-placeholder-table))
  (set sym "[+ or -][n]....{n}...[E[+ or -]{n}...]")
  (setplist sym '((desc . "REAL constant")))

  (setq sym (intern "RMAT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CARRIAGECONTROL_PAR" starfort-mode-placeholder-table))
  (set sym "CARRIAGECONTROL = {char_var}")
  (setplist sym '((desc . "CARRIAGECONTROL = {char_var}")))

  (setq sym (intern "EXIST_PAR" starfort-mode-placeholder-table))
  (set sym "EXIST = {logical_var}")
  (setplist sym '((desc . "EXIST = {logical_var}")))

  (setq sym (intern "ROUTINE_COPYRIGHT" starfort-mode-placeholder-table))
  (set sym "Copyright (C) {year} Central Laboratory of the Research Councils")

  (setq sym (intern "QUAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AOPRMS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AMPRMS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ITEM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MIN_CHAR" starfort-mode-placeholder-table))
  (set sym ".[min]")

  (setq sym (intern "WKCAT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REAL_ARRAY_REF" starfort-mode-placeholder-table))
  (set sym "{real_array_name}( {subscr}... )")
  (setplist sym '((desc . "Real Array reference")))

  (setq sym (intern "DISCO" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XRIGHT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARGUMENTS_GIVEN_AND_RETURNED" starfort-mode-placeholder-table))
  (set sym "\\*	Arguments Given and Returned:
\\	{declaration_statement}...
")
  (setplist sym '((desc . "Define input/output arguments")))

  (setq sym (intern "PRIOR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OBJID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PAREP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ASFS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DIY_PROLOGUE_ITEM" starfort-mode-placeholder-table))
  (set sym "{DIY_prologue_heading}:
{DIY_prologue_text}
\\")
  (setplist sym '((desc . "Design your own additional prologue item") (vert . t)))

  (setq sym (intern "ARGUMENT_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A Fortran dummy argument name of up to 6 characters")))

  (setq sym (intern "YRMS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LSAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARGUMENTS" starfort-mode-placeholder-table))
  (set sym "Arguments:
[argument_spec]...
[status_argument_spec]
\\")
  (setplist sym '((desc . "Description of the routine's argument list") (head . "*")))

  (setq sym (intern "ZOB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RVIEWP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EAREA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ASEC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_KEYWORDS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter a keyword to be used for bibliographic classification purposes.") (sep . ", ")))

  (setq sym (intern "ISBAS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PARAM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MAXNUM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TRIGS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARG_" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Actual arguments must agree in order, number, and data type with
the dummy arguments with which they are associated.  Actual arguments
can be constants, variables, expressions, arrays, array elements,
character substrings, or subprogram names.") (desc . "Function argument") (sep . ", ")))

  (setq sym (intern "MAPPED" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "USAGE" starfort-mode-placeholder-table))
  (set sym "Usage:
{routine_name} {parameter_usage}
\\")
  (setplist sym '((desc . "Concise description of an A-task's usage")))

  (setq sym (intern "RYOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XYP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XYM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IYOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XYE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DYOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCHOIC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "E{INT_CONS}" starfort-mode-placeholder-table))
  (set sym "E{int_cons}")
  (setplist sym '((desc . "Number of characters in exponent")))

  (setq sym (intern "YPTS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TYPLST" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ISACC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COMMON_BLK_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Common block name")))

  (setq sym (intern "MXWRD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTERNAL_DEFINITIONS" starfort-mode-placeholder-table))
  (set sym "[internal_definition_statement]...
[include_internal_definitions]...")

  (setq sym (intern "UBOUND" starfort-mode-placeholder-table))
  (set sym "{bounds_exp}")
  (setplist sym '((desc . "Lower bound")))

  (setq sym (intern "ORIENT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CAPID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YPOS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MINUS" starfort-mode-placeholder-table))
  (set sym "-")
  (setplist sym '((desc . "-")))

  (setq sym (intern "ADAM_PARAMETERS" starfort-mode-placeholder-table))
  (set sym "ADAM Parameters:
[parameter_spec]...
\\")
  (setplist sym '((head . "*") (desc . "Description of the routine's ADAM parameters")))

  (setq sym (intern "YOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YSPLIT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WY2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DESCRIPTION_OF_BUG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A description of any bug which is known about but which has not yet been
fixed.  Known bugs should always be corrected as soon as possible, so
there should almost never be an entry under this heading.  In rare
circumstances, however, it may be impractical to correct a bug
immediately, perhaps because it has proved too difficult to trace or
because it arises from inadequate software or documentation supplied by
by another author.  If the routine can still function satisfactorily in
most circumstances, then the bug should be noted here and fixed at the
first opportunity.")))

  (setq sym (intern "XSPLIT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WY1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WX2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WX1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XLO" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LWIDTH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PROLOGUE_SECTIONS" starfort-mode-placeholder-table))
  (set sym '(("A_Task_Sections" nil token) ("Subroutine_Sections" nil token) ("Function_Sections" nil token) ("Block_Data_Sections" nil token) ("Monolith_Sections" nil token)))
  (setplist sym '((desc . "Menu for selecting individual prologue sections")))

  (setq sym (intern "BIAS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LBOUND" starfort-mode-placeholder-table))
  (set sym "{bounds_exp}")
  (setplist sym '((desc . "Upper bound") (tail . ":")))

  (setq sym (intern "YOPT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REAL_FUNC_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Real function name") (desc . "Real function name")))

  (setq sym (intern "MXACWK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ERR_PAR" starfort-mode-placeholder-table))
  (set sym "ERR = {lbl}")
  (setplist sym '((desc . "ERR = {lbl}")))

  (setq sym (intern "XHI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INQUIRE_PARAMETER" starfort-mode-placeholder-table))
  (set sym '(("file_par" nil token) ("unit_par" nil token) ("access_par" nil token) ("blank_par" nil token) ("carriagecontrol_par" nil token) ("direct_par" nil token) ("err_par" nil token) ("exist_par" nil token) ("form_par" nil token) ("formatted_par" nil token) ("iostat_par" nil token) ("name_par" nil token) ("named_par" nil token) ("nextrec_par" nil token) ("number_par" nil token) ("opened_par" nil token) ("recl_par" nil token) ("sequential_par" nil token) ("unformatted_par" nil token)))
  (setplist sym '((sep . ", ")))

  (setq sym (intern "YTICKD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BEPOCH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INUM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XTICKD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROICOL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NPTXI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DO_VAR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "An integer or real variable")))

  (setq sym (intern "TOPLBL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR_ARRAY_SUBSTRING" starfort-mode-placeholder-table))
  (set sym "{char_array_name}( {subscr}... )( [integer_exp]:[integer_exp] )")
  (setplist sym '((desc . "{char_array_name}( {subscr}... )( [integer_exp]:[integer_exp] )")))

  (setq sym (intern "VALUE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YOFF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_SIDE_EFFECTS" starfort-mode-placeholder-table))
  (set sym "-  {side_effect}")
  (setplist sym '((vert . t)))

  (setq sym (intern "IOFF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PRIOR_REQUIREMENTS" starfort-mode-placeholder-table))
  (set sym "Prior Requirements:
{routine_prior_requirements}...
\\")
  (setplist sym '((desc . "Prior requirements which must be satisfied")))

  (setq sym (intern "ROUTINE_NAME" starfort-mode-placeholder-table))
  (set sym '(lambda nil (let (name i dot) (if (setq name (buffer-file-name)) (progn (setq name (upcase (file-name-nondirectory (file-name-sans-versions name)))) (setq i (length name)) (while (and (> i 0) (not dot)) (setq dot (= 46 (elt name (setq i (- i 1)))))) (if dot (setq name (substring name 0 i))) name) nil))))
  (setplist sym '((help . "The full name of the routine.  Valid characters are letters, digits,
and the underscore character (_).  The first character must be a letter.
Normally, a routine name should consist of up to six characters, but to
reduce the chances of name clashes, the Starlink convention is to add a
three character \"facility prefix\" and to limit the length of the routine
name itself to five characters.  An underscore is used to separate the two
components, giving names such as SGS_IZONE or DAT_ANNUL.  A further
convention is that routines which are only to be called internally
within a facility are identified by appending a \"1\" to the facility
prefix, giving names such as FAC1_START.") (auto . t)))

  (setq sym (intern "NTICKS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AXVEC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EXTRN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FALSE" starfort-mode-placeholder-table))
  (set sym ".FALSE.")
  (setplist sym '((desc . "Logical false constant")))

  (setq sym (intern "IAMIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "UTC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHOSTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VIS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MFABTE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BLOCK_DATA_SECTIONS" starfort-mode-placeholder-table))
  (set sym '(("Notes" nil token) ("Side_Effects" nil token) ("Deficiencies" nil token) ("Machine_Specifics" nil token) ("DIY_Prologue" nil token) ("References" nil token) ("Keywords" nil token) ("Copyright" nil token) ("Global_Constants" nil token) ("Global_Variables" nil token) ("Local_Constants" nil token) ("Local_Variables" nil token) ("Global_Data" nil token)))
  (setplist sym '((desc . "Menu of prologue sections for a block data routine")))

  (setq sym (intern "PTXI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "UT1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NAMES" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TXP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OPSTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IMPLEMENTATION_STATUS" starfort-mode-placeholder-table))
  (set sym "Implementation Status:
{routine_implementation_status}
\\")
  (setplist sym '((desc . "Extent of standard features supported by the routine")))

  (setq sym (intern "TXJ" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TXI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GREEN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INQUIRE_STMT" starfort-mode-placeholder-table))
  (set sym "INQUIRE ( {inquire_parameter}... )")
  (setplist sym '((desc . "INQUIRE statement")))

  (setq sym (intern "NXSUB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTEGER_FIELD_DESCR" starfort-mode-placeholder-table))
  (set sym '(("i_code" nil place)))
  (setplist sym '((desc . "Iw, Iw.m")))

  (setq sym (intern "TUS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BOUND_ELM" starfort-mode-placeholder-table))
  (set sym '(("cons" nil place) ("p" nil place) ("common_var" nil place)))
  (setplist sym '((desc . "Bounds element")))

  (setq sym (intern "IYMDF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OPSTA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BEP1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BEP0" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INDF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VEC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SAVE_STMT" starfort-mode-placeholder-table))
  (set sym "SAVE {save_elm}...")
  (setplist sym '((desc . "SAVE statement") (vert . t)))

  (setq sym (intern "VALID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SZY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SZX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EXAMPLES" starfort-mode-placeholder-table))
  (set sym "Examples:
{routine_example}...
\\")
  (setplist sym '((desc . "Examples of how the routine can be used")))

  (setq sym (intern "ILPY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REAL_AND_COMPLEX_FIELD_DESCR" starfort-mode-placeholder-table))
  (set sym '(("f_code" nil place) ("e_code" nil place) ("d_code" nil place) ("g_code" nil place)))
  (setplist sym '((desc . "Fw.d, Ew.d, Dw.d, Gw.d, Ew.dEe, Gw.dEe")))

  (setq sym (intern "ACTION_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The name of one of the actions (i.e. commands) which the monolith
recognises. Remember to use upper case.") (auto . t)))

  (setq sym (intern "ILPX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YMIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IMIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TRN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTEGER_FUNC_REF" starfort-mode-placeholder-table))
  (set sym "{integer_func_name}( [arg_]... )")
  (setplist sym '((desc . "Integer FUNCTION reference")))

  (setq sym (intern "VAR" starfort-mode-placeholder-table))
  (set sym '(("integer_var" nil place) ("char_var" nil place) ("logical_var" nil place) ("real_var" nil place) ("complex_var" nil place)))
  (setplist sym '((desc . "Symbolic Name") (sep . ", ")))

  (setq sym (intern "VAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ORDER" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MXOPWK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "UID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REFB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REFA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOG_ELM" starfort-mode-placeholder-table))
  (set sym '(("log_cons" nil place) ("logical_var" nil place) ("logical_array_ref" nil place) ("relational_exp" nil place) ("logical_exp_paren" nil place) ("integer_exp_paren" nil place) ("integer_func_ref" nil place) ("logical_func_ref" nil place)))
  (setplist sym '((desc . "Logical element")))

  (setq sym (intern "TLR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INCLUDE_INTERNAL_DECLARATIONS" starfort-mode-placeholder-table))
  (set sym "INCLUDE '{internal_declaration_file}'   ! [internals_description]	")
  (setplist sym '((vert . t)))

  (setq sym (intern "STL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YMAX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SUB" starfort-mode-placeholder-table))
  (set sym "-")
  (setplist sym '((desc . "-")))

  (setq sym (intern "CLOSE_PARAMETER" starfort-mode-placeholder-table))
  (set sym '(("CLOSE_STATUS" "Specify whether to keep or delete the file (default=keep)" token) ("ERR" "Specify a statement to branch to if an error occurs" token) ("IOSTAT" "Specify a variable to receive any I/O error code" token)))
  (setplist sym '((desc . "CLOSE statement parameter") (sep . ", ") (head . ",")))

  (setq sym (intern "JDIM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOGICAL_EXP" starfort-mode-placeholder-table))
  (set sym "[.NOT.]{log_elm}[{lop}{log_elm}]...")
  (setplist sym '((desc . "Logical Expression")))

  (setq sym (intern "NSHIFT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SUBROUTINE_PROLOGUE" starfort-mode-placeholder-table))
  (set sym "\\*	Name:
{routine_name}
\\
\\*	Purpose:
{routine_purpose}
\\
\\*	Language:
{routine_language}
\\
\\*	Invocation:
CALL {routine_name}( [p]... )
\\
\\*	Description:
{routine_description}
\\
\\*	[arguments]
[optional_subroutine_items]...
Authors:
{original_author_entry}
\\
\\*	History:
{original_version_entry}
\\
\\*	Bugs:
{note_any_bugs_here}
\\")
  (setplist sym '((desc . "SUBROUTINE prologue template")))

  (setq sym (intern "INTERNAL_DECLARATION_STATEMENT" starfort-mode-placeholder-table))
  (set sym "{data_type} {internal_name}   ! [internal_description]	")
  (setplist sym '((vert . t)))

  (setq sym (intern "NPRIO" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ENTER_FURTHER_CHANGES_HERE" starfort-mode-placeholder-table))
  (set sym "{date} ({author_identifier}):
{changes}
{enter_further_changes_here}")

  (setq sym (intern "TDK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YLBL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TDB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TRANS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOCAL_VARIABLE_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a brief description of what the variable is used for.") (head . "!")))

  (setq sym (intern "PAIND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_PRIOR_REQUIREMENTS" starfort-mode-placeholder-table))
  (set sym "-  {prior_condition}")
  (setplist sym '((vert . t)))

  (setq sym (intern "ROP" starfort-mode-placeholder-table))
  (set sym '(("less_than" nil place) ("less_than_or_equal" nil place) ("equal" nil place) ("not_equal" nil place) ("greater_than" nil place) ("greater_than_or_equal" nil place)))
  (setplist sym '((desc . "Relational Operators")))

  (setq sym (intern "ADAM_CONSTRUCTS" starfort-mode-placeholder-table))
  (set sym '(("_OK_BLOCK" nil token) ("_BAD_BLOCK" nil token) ("_CHECK_STATUS" nil token) ("_ERROR_REPORT" nil token) ("_CLEANUP_CODE" nil token)))
  (setplist sym '((desc . "Menu of ADAM programming constructs")))

  (setq sym (intern "XRMS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FAIND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SET" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DATA_STMT" starfort-mode-placeholder-table))
  (set sym "DATA {data_elm} / {data_values}... /")
  (setplist sym '((desc . "DATA statement") (vert . t)))

  (setq sym (intern "ROB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RVALUES" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SEC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BASE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FUNCTION_SECTIONS" starfort-mode-placeholder-table))
  (set sym '(("Arguments" nil token) ("Examples" nil token) ("Pitfalls" nil token) ("Notes" nil token) ("Prior_Requirements" nil token) ("Side_Effects" nil token) ("Algorithm" nil token) ("Accuracy" nil token) ("Timing" nil token) ("Routines_Used" nil token) ("Deficiencies" nil token) ("Machine_Specifics" nil token) ("DIY_Prologue" nil token) ("References" nil token) ("Keywords" nil token) ("Copyright" nil token) ("Global_Constants" nil token) ("Global_Variables" nil token) ("Arguments_Given" nil token) ("Arguments_Given_and_Returned" nil token) ("Arguments_Returned" nil token) ("Status_Argument" nil token) ("External_References" nil token) ("Local_Constants" nil token) ("Local_Variables" nil token) ("Internal_References" nil token) ("Local_Data" nil token)))
  (setplist sym '((desc . "Menu of prologue sections for a function")))

  (setq sym (intern "RXOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IXOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FORM_PAR" starfort-mode-placeholder-table))
  (set sym "FORM = {char_var}")
  (setplist sym '((desc . "FORM = {char_var}")))

  (setq sym (intern "IHOUR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DXOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NPPMI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RFY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PYA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RFX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STATUS_CHECK" starfort-mode-placeholder-table))
  (set sym "\\*	Check inherited global status.
\\	IF ( STATUS .NE. SAI__OK ) RETURN
")

  (setq sym (intern "PARAMETER_TYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The HDS or ADAM data type of the parameter.  Normally this should be
specified in the same form in which it appears in the A-task interface
(.IFL) file, e.g:

   _INTEGER, _REAL, _CHAR, LITERAL, NDF, etc.

When the ADAM type \"UNIV\" is used to allow a variety of HDS data types
to be accepted, then a more descriptive type specification should
generally be given here.")))

  (setq sym (intern "NPPLI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PXA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INT_ELM" starfort-mode-placeholder-table))
  (set sym '(("int_cons" nil place) ("integer_var" nil place) ("integer_array_ref" nil place) ("integer_exp" nil place) ("integer_func_ref" nil place)))
  (setplist sym '((desc . "Integer element")))

  (setq sym (intern "XPTS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTERNAL_DEFINITION_FILE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter the logical name of the file you wish to include which contains
definitions of internal functions.")))

  (setq sym (intern "UNSIGN_INT_CONS" starfort-mode-placeholder-table))
  (set sym "{n}...")
  (setplist sym '((desc . "Unsigned Integer constant")))

  (setq sym (intern "MXPIX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PREC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PROMPT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "QMF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RED" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A numeric expression with a value that represents the position in a
direct access file of the record to be accessed.  The value must be
greater than or equal to one, and less than or equal to the maximum
number of record cells allowed in the file.")))

  (setq sym (intern "ERROR_CODE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The name of a global symbolic constant identifying an error condition.
Such constants are conventionally named FAC__ENAME, where FAC is a three
character prefix identifying the facility from which the error originates
and ENAME is the name of the error code (up to 5 characters).  Some
existing software does not follow this convention, but all new software
should. The value SAI__ERROR is available for general use.")))

  (setq sym (intern "PARAMETER_USAGE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter a list of parameters to show how the routine should be invoked. The
recommended method is to enter the names of all the positional parameters
in order, followed by \"KEYWORD=?\" for any other parameters which
require a keyword but are not normally defaulted (i.e. will be prompted
for if omitted from the comand line). If any of the positional parameters
are normally defaulted (i.e. are not prompted for), then this should be
indicated by enclosing them in square brackets. For example:

   PROG IN1 IN2 [IN3] OUT MODE=? START=?

Here, IN1, IN2, IN3 and OUT are all positional parameters (positions 1, 2,
3 & 4 respectively) with only IN3 being defaulted, while MODE and START
are non-defaulted keyword parameters. Defaulted keyword parameters (of
which there are sometimes a great number) need not appear.")))

  (setq sym (intern "GRPEXP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RAZ" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PSI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REGMOD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RAP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XPOS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SYSNAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RPNTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RAD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "POS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DIMENSIONS" starfort-mode-placeholder-table))
  (set sym "( {dims}... )")
  (setplist sym '((desc . "Array dimension expression")))

  (setq sym (intern "XOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARGUMENT_SPEC" starfort-mode-placeholder-table))
  (set sym "{argument_name}[dimensions] = {argument_data_type} ({argument_access_mode})
{argument_description}")
  (setplist sym '((vert . t)))

  (setq sym (intern "IPNTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BLANK_OPTIONS" starfort-mode-placeholder-table))
  (set sym '(("NULL" "Ignore blanks in numeric fields" token) ("ZERO" "Treat blanks in numeric fields as zeros" token)))
  (setplist sym '((desc . "Blank options")))

  (setq sym (intern "PNM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RLWMIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COMPLEX_ARRAY_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Complex array name") (desc . "Complex array name")))

  (setq sym (intern "SEGNAM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IZONID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DEFICIENCIES" starfort-mode-placeholder-table))
  (set sym "Implementation Deficiencies:
{routine_deficiencies}...
\\")
  (setplist sym '((desc . "Details of any implementation deficiencies")))

  (setq sym (intern "PMI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BACK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PMB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IGRP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PLI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NPPAI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IZONES" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ICONID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR_ASSIGN_ELM" starfort-mode-placeholder-table))
  (set sym '(("char_var" nil place) ("char_substring" nil place) ("char_array_ref" nil place)))
  (setplist sym '((desc . "Character assignment element")))

  (setq sym (intern "XOPT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NYV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NXY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NXV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOGICAL_ASSIGN_ELM" starfort-mode-placeholder-table))
  (set sym '(("logical_var" nil place) ("logical_array_ref" nil place)))
  (setplist sym '((desc . "Logical assignment element")))

  (setq sym (intern "PID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PHI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARRAY_NAME" starfort-mode-placeholder-table))
  (set sym '(("char_array_name" nil place) ("integer_array_name" nil place) ("real_array_name" nil place) ("complex_array_name" nil place) ("logical_array_name" nil place)))
  (setplist sym '((desc . "Array name")))

  (setq sym (intern "RLWMAX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ELONGM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTERNAL_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The name (up to 6 characters) of an internal function which is to be
defined later.")))

  (setq sym (intern "XNUM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Number of character positions to be passed over")))

  (setq sym (intern "PET" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PNTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REPEAT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Is the repeat count for the field descriptor.  If you omit this
value, the repeat count is assumed to be 1.") (desc . "Repeat count")))

  (setq sym (intern "HOURS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NUM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INVVAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IMPLICIT_NONE" starfort-mode-placeholder-table))
  (set sym "IMPLICIT NONE")
  (setplist sym '((desc . "IMPLICIT NONE")))

  (setq sym (intern "IHMSF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EPOCH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NTM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OLD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTERNAL_REFERENCES" starfort-mode-placeholder-table))
  (set sym "\\*	Internal References:
\\	{internal_declarations}

{internal_definitions}
")
  (setplist sym '((desc . "Define internal functions")))

  (setq sym (intern "PCI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NSG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PPAI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NPT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NPR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PAI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DOUBLE_CONS" starfort-mode-placeholder-table))
  (set sym "[+ or -][n]....{n}...[D[+ or -]{n}...]")
  (setplist sym '((desc . "DOUBLE PRECISION constant")))

  (setq sym (intern "GLOBAL_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The name of a global variable referenced in this routine.")))

  (setq sym (intern "NOT" starfort-mode-placeholder-table))
  (set sym ".NOT.")
  (setplist sym '((desc . ".NOT.")))

  (setq sym (intern "WIDTH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "External field width in characters")))

  (setq sym (intern "IFMT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A statement label of a FORMAT statement, or the name of an array,
array element, or character expression containing a run-time format.") (desc . "Format Specifier")))

  (setq sym (intern "INCLUDE_GLOBAL_CONSTANTS" starfort-mode-placeholder-table))
  (set sym "INCLUDE '{global_constants_file}'   ! [global_constants_description]	")
  (setplist sym '((vert . t)))

  (setq sym (intern "MOVED" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SENSIT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NR1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XOFF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NMX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NMT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NMS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCHDEV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NLW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NLT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ALGORITHM" starfort-mode-placeholder-table))
  (set sym "Algorithm:
{algorithm_description}...
\\")
  (setplist sym '((desc . "A technical description of the algorithm used")))

  (setq sym (intern "MTXBTE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IERR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOVAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GLOBAL_CONSTANTS" starfort-mode-placeholder-table))
  (set sym "\\*	Global Constants:
\\	[standard_SAE_constants]
[include_global_constants]...
")
  (setplist sym '((desc . "Define global constants")))

  (setq sym (intern "COMMON_VAR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Variable in a common block") (desc . "Common block variable")))

  (setq sym (intern "EXTERNAL_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The name of an external function which is referenced.") (auto . t)))

  (setq sym (intern "MSG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NIS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NHS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ASPECT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BADOK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MAXCHX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOSTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OFFSET" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OB2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OB1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MMX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NEW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MAXCHH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTEGER_EXP_PAREN" starfort-mode-placeholder-table))
  (set sym "( {integer_exp} )")
  (setplist sym '((desc . "Integer Expression in parentheses")))

  (setq sym (intern "NFI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SPLIT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LUN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REWIND_STMT" starfort-mode-placeholder-table))
  (set sym "REWIND ( UNIT = {unit}, [rewind_parameter]... )")
  (setplist sym '((desc . "REWIND statement") (vert . t)))

  (setq sym (intern "AELM" starfort-mode-placeholder-table))
  (set sym '(("int_cons" nil place) ("real_cons" nil place) ("double_cons" nil place) ("arith_var" nil place) ("numeric_array_ref" nil place) ("arith_exp_paren" nil place) ("arith_func_ref" nil place)))
  (setplist sym '((desc . "Arithmetic Element")))

  (setq sym (intern "ACCESS_OPTIONS" starfort-mode-placeholder-table))
  (set sym '(("SEQUENTIAL" "Sequential file access" token) ("DIRECT" "Direct (record-oriented) file access" token) ("APPEND" "Appended sequential access (non-standard, worth avoiding)" token)))
  (setplist sym '((desc . "Access options")))

  (setq sym (intern "NDP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$REGMOD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MXWKAS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "{AOP}\ {AELM}" starfort-mode-placeholder-table))
  (set sym "{aop} {aelm} ")

  (setq sym (intern "CENTER" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XMIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NCD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TXEXPY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TXEXPX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PLOT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AUTHORS_NAME" starfort-mode-placeholder-table))
  (set sym '(lambda nil (or (getenv "EDSTAR_PERSONAL_NAME") (user-full-name))))

  (setq sym (intern "NOTES" starfort-mode-placeholder-table))
  (set sym "Notes:
{routine_notes}...
\\")
  (setplist sym '((desc . "Notes which qualify earlier information")))

  (setq sym (intern "ND1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OPLEN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FORWD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IDNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOP" starfort-mode-placeholder-table))
  (set sym '(("and" nil place) ("not" nil place) ("or" nil place) ("neqv" nil place) ("eqv" nil place)))
  (setplist sym '((desc . "Logical Operators")))

  (setq sym (intern "WTOD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FABUN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_ACCURACY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "If important, give details of the numerical accuracy which the routine
achieves.")))

  (setq sym (intern "LOC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LVALUES" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "POSIT" starfort-mode-placeholder-table))
  (set sym "+")
  (setplist sym '((desc . "+")))

  (setq sym (intern "ARITH_FUNC_REF" starfort-mode-placeholder-table))
  (set sym "{arith_func_name}( [arg_]... )")
  (setplist sym '((desc . "Arithmetic function reference")))

  (setq sym (intern "XMAX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YEAR" starfort-mode-placeholder-table))
  (set sym '(lambda nil (substring (current-time-string) 20 24)))
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PICID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOCAL_CONSTANTS" starfort-mode-placeholder-table))
  (set sym "\\*	Local Constants:
\\	{local_constant_specification}...
")
  (setplist sym '((desc . "Define local constants")))

  (setq sym (intern "IDIM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PRINT_IO_ELM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The I/O list in a PRINT statement contains the names of variables,
arrays, array elements, character substrings, constants and expressions
from which data will be transferred.") (desc . "PRINT statement I/O list element") (sep . ", ")))

  (setq sym (intern "VIEWPT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CALL_STMT" starfort-mode-placeholder-table))
  (set sym "CALL {subprogram_name}( [arg]... )")
  (setplist sym '((desc . "CALL statement") (vert . t)))

  (setq sym (intern "PITFALL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Describe any particular problem which the inexperienced or unwary
user may encounter which may not be obvious from the information
given elsewhere.")))

  (setq sym (intern "INTEGER_ARRAY_REF" starfort-mode-placeholder-table))
  (set sym "{integer_array_name}( {subscr}... )")
  (setplist sym '((desc . "Integer Array reference")))

  (setq sym (intern "IGRP2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IGRP1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IDEG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LEN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Length in characters.")))

  (setq sym (intern "LDR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MACHINE_SPECIFICS" starfort-mode-placeholder-table))
  (set sym "{machine}-Specific Features Used:
{routine_machine_specifics}...
\\")
  (setplist sym '((desc . "Any machine-specific features used")))

  (setq sym (intern "YDAY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NSPACE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XLBL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LBL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The label of an executable statement.") (sep . ", ")))

  (setq sym (intern "YBOT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FILE_PAR" starfort-mode-placeholder-table))
  (set sym "FILE = {fi}")
  (setplist sym '((desc . "FILE = {fi}")))

  (setq sym (intern "WORDS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TXIND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NOTED_ITEM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter the text of a note which may be used to qualify or \"fine-tune\"
the descriptive information given earlier.  For instance, a note could
be used to describe how a special case will be handled by the routine, or
to describe particular conditions which the input data must satisfy.  Such
information may be essential for a precise description of how the routine
behaves, but might not be of initial interest to the general user who will
mainly be guided by the \"Description\" topic.

The content of the note may be extracted for use in user documentation,
so it should not contain items which are only of interest to some-one
reading the code (e.g. a maintenance programmer).")))

  (setq sym (intern "LAB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The label of a CONTINUE statement which ends the DO loop range. The
statement must physically follow in the same program unit.") (desc . "Do statement label") (auto . t)))

  (setq sym (intern "LABEL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOCAL_VARIABLE_DECLARATION" starfort-mode-placeholder-table))
  (set sym "{data_type} {name}[dimensions]   ! [local_variable_description]	")
  (setplist sym '((vert . t)))

  (setq sym (intern "DIY_PROLOGUE_HEADING" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter a heading for your personal prologue item.  This may be used to
cater for special circumstances which are not covered by any of the
standard prologue item headings.  Be careful not to duplicate one of
the standard headings.")))

  (setq sym (intern "VONOFF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IY2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IY1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ITT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IX2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IARY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IX1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "+\ OR\ -" starfort-mode-placeholder-table))
  (set sym '(("POSIT" nil place) ("MINUS" nil place)))
  (setplist sym '((desc . "Optional sign")))

  (setq sym (intern "IARG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SHIFT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REWIND_PARAMETER" starfort-mode-placeholder-table))
  (set sym '(("IOSTAT" "Specify a variable to receive any I/O error code" token) ("ERR" "Specify a statement to branch to if an error occurs" token)))
  (setplist sym '((sep . ", ") (head . ",")))

  (setq sym (intern "IPY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IPX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INT_CONS" starfort-mode-placeholder-table))
  (set sym "[+ or -]{n}...")
  (setplist sym '((desc . "Integer constant")))

  (setq sym (intern "GLOBAL_VARIABLE_PURPOSE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a description of the function that the global variable performs.")))

  (setq sym (intern "ARITH_EXP_PAREN" starfort-mode-placeholder-table))
  (set sym "( {arith_exp} )")
  (setplist sym '((desc . " Arithmetic Expression in parentheses")))

  (setq sym (intern "INV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MONOLITH_SECTIONS" starfort-mode-placeholder-table))
  (set sym '(("Pitfalls" nil token) ("Notes" nil token) ("Prior_Requirements" nil token) ("Side_Effects" nil token) ("Routines_Used" nil token) ("Deficiencies" nil token) ("DIY_Prologue" nil token) ("References" nil token) ("Keywords" nil token) ("Copyright" nil token)))
  (setplist sym '((desc . "Menu of prologue sections for an ADAM monolith routine")))

  (setq sym (intern "IVALUES" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EXTERNAL_STMT" starfort-mode-placeholder-table))
  (set sym "EXTERNAL {name}")
  (setplist sym '((desc . "EXTERNAL statement") (vert . t)))

  (setq sym (intern "TOPIC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PARAMETER_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The name of an ADAM parameter which the routine accesses.")))

  (setq sym (intern "NEWFLG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RETURNED_VALUE_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A description of the value returned via the function name.")))

  (setq sym (intern "COORD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_PURPOSE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A very brief (about one line) description of the service the routine
provides, e.g:

    \"Subtract a scalar from each pixel of an array.\"
or: \"Interpolate across a group of lines or columns in a 2-d array.\"

This description may be extracted to appear as part of the routine
heading in user documentation.")))

  (setq sym (intern "TEMP_STATUS" starfort-mode-placeholder-table))
  (set sym "TSTAT")
  (setplist sym '((desc . "Temporary status variable") (auto . t)))

  (setq sym (intern "ZOOMF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MEMTYP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MXOFFS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "HIGH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AFFILIATION" starfort-mode-placeholder-table))
  (set sym '(lambda nil (or (getenv "EDSTAR_PERSONAL_AFFILIATION") "STARLINK")))

  (setq sym (intern "MONTH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "HOB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TRNLOC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PARAMETER_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a description of the function that the parameter performs. Also
specify its default value in square brackets [...] if appropriate.")))

  (setq sym (intern "CONTEXTUAL_ERROR_REPORT" starfort-mode-placeholder-table))
  (set sym "\\*	If an error occurred, then report a contextual message.
\\	IF ( STATUS .NE. SAI__OK ) THEN
CALL ERR_REP( '{routine_name}_ERR',
\\     :	'{routine_name}: {context_message}.',
\\     :	STATUS )
END IF
")
  (setplist sym '((desc . "Report contextual information following an error")))

  (setq sym (intern "E\[+\ OR\ -\]{N}\.\.\." starfort-mode-placeholder-table))
  (set sym "E[+ or -]{n}...")
  (setplist sym '((desc . "Decimal exponent")))

  (setq sym (intern "WORK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTEGER_FUNC_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Integer function name") (desc . "Integer function name")))

  (setq sym (intern "PHIM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NWRD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARITH_ARRAY_REF" starfort-mode-placeholder-table))
  (set sym "{arith_array_name}( {subscr}... )")
  (setplist sym '((desc . "Arithmetic Array reference")))

  (setq sym (intern "GKS$TYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IO_ELM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The I/O list in an input or output statement contains the names of
variables,  arrays, array elements, and character substrings from
which or to which data will be transferred.  The I/O list in an output
statement can also contain constants and expressions to be output.") (desc . "I/O list element") (sep . ", ")))

  (setq sym (intern "DR1950" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "UNIT_PAR" starfort-mode-placeholder-table))
  (set sym "UNIT = {unit_value}")
  (setplist sym '((desc . "UNIT = {unit_value}")))

  (setq sym (intern "GKS$VIS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DD1950" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$TXP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "THETA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$SW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "THERE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DEFMOD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OPENED_PAR" starfort-mode-placeholder-table))
  (set sym "OPENED = {logical_var}")
  (setplist sym '((desc . "OPENED = {logical_var}")))

  (setq sym (intern "LOCAL_DATA" starfort-mode-placeholder-table))
  (set sym "\\*	Local Data:
\\	{data_stmt}...
")
  (setplist sym '((desc . "Define local data")))

  (setq sym (intern "RRESLT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GR2E" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "UNFORMATTED_PAR" starfort-mode-placeholder-table))
  (set sym "UNFORMATTED = {char_var}")
  (setplist sym '((desc . "UNFORMATTED = {char_var}")))

  (setq sym (intern "CONID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COMMON_BLOCK_REF" starfort-mode-placeholder-table))
  (set sym "/{common_blk_name}/")

  (setq sym (intern "GLOBAL_VARIABLES_FILE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter the logical name of the global variables file you wish to include.")))

  (setq sym (intern "NOMMS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COMNT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTEGER_VAR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Integer variable") (desc . "Integer variable")))

  (setq sym (intern "NOMLW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XNUMB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTTY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FOR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DEVNAM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FMT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A statement label of a FORMAT statement, or the name of an array,
array element, or character expression containing a run-time format.
An asterisk may also be used to specify list-directed formatting.") (desc . "Format Specifier")))

  (setq sym (intern "FWDS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GDP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ESW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NVLD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "THETA2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PERP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "THETA1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NPFAI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DR2000" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ETA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NVIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SIDE_EFFECT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Describe any side effect which the routine might have but which would
not be obvious from the information given elsewhere.")))

  (setq sym (intern "DYW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PGFLAG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EQV" starfort-mode-placeholder-table))
  (set sym ".EQV.")
  (setplist sym '((desc . ".EQV.")))

  (setq sym (intern "RELEASE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTOP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DD2000" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MEMSIY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IRESLT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DXW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MEMSIX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COLOR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LESS_THAN_OR_EQUAL" starfort-mode-placeholder-table))
  (set sym ".LE.")
  (setplist sym '((desc . ".LE.")))

  (setq sym (intern "EPS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VRTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ORIGINAL_AUTHOR_ENTRY" starfort-mode-placeholder-table))
  (set sym "{author_identifier}: {authors_name} ({affiliation})
{enter_new_authors_here}")

  (setq sym (intern "CHDNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FILE_SPEC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "File specification")))

  (setq sym (intern "EPJ" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INSTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARGUMENT_DATA_TYPE" starfort-mode-placeholder-table))
  (set sym '(("INTEGER" nil token) ("REAL" nil token) ("CHARACTER" nil token) ("LOGICAL" nil token) ("DOUBLE_PRECISION" nil token) ("WORD" nil token) ("BYTE" nil token) ("COMPLEX" nil token) ("PASSED_SUBROUTINE" nil token) ("PASSED_FUNCTION" nil token)))
  (setplist sym '((desc . "Subroutine/function data type description")))

  (setq sym (intern "EPB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DVH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ELX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NULL" starfort-mode-placeholder-table))
  (set sym "'NULL'")
  (setplist sym '((desc . "'NULL'")))

  (setq sym (intern "READ_PARAMETER" starfort-mode-placeholder-table))
  (set sym '(("REC" "Specify the required record for direct access files" token) ("IOSTAT" "Specify a variable to receive any I/O error code" token) ("END" "Specify a statement to branch to on end-of-file condition" token) ("ERR" "Specify a statement to branch to if an error occurs" token)))
  (setplist sym '((sep . ", ") (head . ",")))

  (setq sym (intern "DVB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EP1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EP0" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DRESLT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$TXAV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHECK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MESSAGE_TEXT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter the text of the error message. This should be as informative as
possible and should include information about the immediate context in
which the error occurred. In wording the message, be careful not to
make unjustified assumptions about the wider context within which your
routine may be called.
   Values may be included in the message text by means of previously
defined message tokens prefixed with a '^' character (e.g. ^TOKEN). It
is recommended that the ^STATUS token (which ADAM uses to represent the
VMS error message associated with the current STATUS value on VAX
machines) should not be used, partly on grounds of portability and partly
because it tends to be rather uninformative. The following are examples
of possible error reports:

   'All ^NSLOT slots allocated for new entries have been used up'
   'Unable to copy ^OBJECT to ^STRUCTURE.^COMPONENT'
   'A value of ^NSIGMA is not valid when specifying the clipping level'") (desc . "The text of an error report")))

  (setq sym (intern "MXDIM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DSL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NUM2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BUFLEN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NUM1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$TXAH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NVAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EDIT_FIELD_DESCR" starfort-mode-placeholder-table))
  (set sym '(("x_edit_field" nil place) ("t_edit_field" nil place) ("s_edit_field" nil place) ("b_edit_field" nil place) ("char_cons" nil place) ("colon_field" nil place)))
  (setplist sym '((desc . "'...',nX,Tn,TLn,TRn,nP,:,BN,BZ,S,SP,SS")))

  (setq sym (intern "PFAI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DSB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DPY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DPX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COLIA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TXBUN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTEGER_ARRAY_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Integer array name") (desc . "Integer array name")))

  (setq sym (intern "FAI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EHN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MORE_PARAMETERS" starfort-mode-placeholder-table))
  (set sym "{constant_exp}")
  (setplist sym '((desc . "A list of parameters") (vert . t) (sep . ", ")))

  (setq sym (intern "NTNR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STANDARD_SAE_CONSTANTS" starfort-mode-placeholder-table))
  (set sym "INCLUDE 'SAE_PAR'          ! Standard SAE constants	")

  (setq sym (intern "DPH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MKTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DPB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LNTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CVM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WKTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR_SUBSTRING" starfort-mode-placeholder-table))
  (set sym '(("char_var_substring" nil place) ("char_array_substring" nil place)))
  (setplist sym '((desc . "Character substring")))

  (setq sym (intern "DOB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PARAMETER_STMT" starfort-mode-placeholder-table))
  (set sym "PARAMETER ( {parameter_list} )")
  (setplist sym '((desc . "PARAMETER statement") (vert . t)))

  (setq sym (intern "DVALUES" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$MODE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NSUB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARITH_FUNC_NAME" starfort-mode-placeholder-table))
  (set sym '(("integer_func_name" nil place) ("real_func_name" nil place) ("logical_func_name" nil place)))
  (setplist sym '((desc . "Arithmetic function name")))

  (setq sym (intern "NSTD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "//{CHAR_ELM}" starfort-mode-placeholder-table))
  (set sym "//{char_elm}")

  (setq sym (intern "LSTYLE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IWKID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PDIM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((desc . "Array dimension size") (help . "The size of a parameter dimension. If the dimension does not have a fixed
size, then omit this value altogether. Note that in this latter case, the
number of commas remaining in the dimension expression should indicate the
number of parameter dimensions.") (sep . ", ")))

  (setq sym (intern "DIV" starfort-mode-placeholder-table))
  (set sym "/")
  (setplist sym '((desc . "/")))

  (setq sym (intern "DJM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CPY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CPX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DIM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TOKEN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHANGES" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Describe any changes made to the routine.")))

  (setq sym (intern "TPATH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NEXTREC_PAR" starfort-mode-placeholder-table))
  (set sym "NEXTREC = {integer_var}")
  (setplist sym '((desc . "NEXTREC = {integer_var}")))

  (setq sym (intern "GKS$DEFMOD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DET" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CVALUES" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHARS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BUF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BMTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DEC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a full description (in English) of what the routine does and how
it does it (if relevant).  This description should be comprehensible
to the user of the routine, e.g:

   \"This routine interpolates across a contiguous group of lines or
   columns in a 2-d array.  If the group abuts an edge of the array
   then interpolation is not possible, and the group is replaced by
   the duplication of the nearest line or column outside the group, as
   appropriate.  Should the flanking pixels of the group be bad (i.e.
   have the magic value), then the next adjacent pixel that is not
   bad is chosen to compute the interpolation.  Pseudo-Poissonian
   noise may be added to the interpolated pixels for cosmetic
   effect.\"

The description may be extracted for use in user documentation.")))

  (setq sym (intern "DEMPTY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NSKD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NAME_OF_FACILITY_OR_PACKAGE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter a name identifying the facility or package from which the external
routines are drawn, e.g. HDS, SGS, etc.")))

  (setq sym (intern "DAY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SHAPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ABORT_STMT" starfort-mode-placeholder-table))
  (set sym "99")

  (setq sym (intern "NAME_PAR" starfort-mode-placeholder-table))
  (set sym "NAME = {char_var}")
  (setplist sym '((desc . "NAME = {char_var}")))

  (setq sym (intern "LANGUAGE_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Describe the type of Fortran in which the routine is written, e.g:

    Fortran 77, Fortran 66, VAX Fortran, etc.

The use of \"Starlink Fortran 77\" following the standards set out in
the Starlink Programming Standards document (SGP/16) is strongl
encouraged.  A separate menu item is provided for this language
description.")))

  (setq sym (intern "DAP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WKID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LENGTH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TXALV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTERNAL_DECLARATIONS" starfort-mode-placeholder-table))
  (set sym "[internal_declaration_statement]...
[include_internal_declarations]...")

  (setq sym (intern "TANGLE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INDXLO" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VOFFON" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TXALH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CLOSE_STMT" starfort-mode-placeholder-table))
  (set sym "CLOSE ( UNIT = {unit}, [close_parameter]... )")
  (setplist sym '((desc . "CLOSE statement") (vert . t)))

  (setq sym (intern "ARRAY_DECL" starfort-mode-placeholder-table))
  (set sym "{array_name}( {dims}... )")
  (setplist sym '((desc . "Array declarator") (sep . ", ")))

  (setq sym (intern "CI2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CI1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PATH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "REAL_ARRAY_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Real array name") (desc . "Real array name")))

  (setq sym (intern "ROIID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INDXHI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PARAMETER_LIST" starfort-mode-placeholder-table))
  (set sym "{constant_exp},
\\     :[more_parameters]...	")
  (setplist sym '((desc . "  Parameter list")))

  (setq sym (intern "ZU" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ZR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YZ" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOGICAL_FUNC_REF" starfort-mode-placeholder-table))
  (set sym "{logical_func_name}( [arg_]... )")
  (setplist sym '((desc . "Logical FUNCTION reference")))

  (setq sym (intern "YU" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XZ" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Actual arguments must agree in order, number, and data type with
the dummy arguments with which they are associated.  Actual arguments
can be constants, variables, expressions, arrays, array elements,
character substrings, or subprogram names.") (desc . "Subroutine argument") (sep . ", ") (head . "(") (tail . ")")))

  (setq sym (intern "YP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NVOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "A_TASK_SECTIONS" starfort-mode-placeholder-table))
  (set sym '(("Usage" nil token) ("ADAM_Parameters" nil token) ("Examples" nil token) ("Pitfalls" nil token) ("Notes" nil token) ("Prior_Requirements" nil token) ("Side_Effects" nil token) ("Algorithm" nil token) ("Accuracy" nil token) ("Timing" nil token) ("Implementation_Status" nil token) ("Routines_Used" nil token) ("Deficiencies" nil token) ("Machine_Specifics" nil token) ("DIY_Prologue" nil token) ("References" nil token) ("Keywords" nil token) ("Copyright" nil token) ("Global_Constants" nil token) ("Global_Variables" nil token) ("External_References" nil token) ("Local_Constants" nil token) ("Local_Variables" nil token) ("Internal_References" nil token) ("Local_Data" nil token)))
  (setplist sym '((desc . "Menu of prologue sections for an ADAM A-task")))

  (setq sym (intern "YM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XU" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NEWY2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "APP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NEWY1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VERSION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "XL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "YC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NEWX2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AOP" starfort-mode-placeholder-table))
  (set sym '(("add" nil place) ("sub" nil place) ("mult" nil place) ("div" nil place) ("expo" nil place)))

  (setq sym (intern "XI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NEWX1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VU" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_MACHINE_SPECIFICS" starfort-mode-placeholder-table))
  (set sym "-  {machine_specific_feature}")
  (setplist sym '((vert . t)))

  (setq sym (intern "XC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "UV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AOB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MACHINE_SPECIFIC_FEATURE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Enter details of any machine-specific feature which may affect the
routine's portability, such as dependence on a particular type of
hardware or the use of operating-system routines.  You should also
include any extensions to the language description given under the
\"Language\" heading.  Thus, if you specified \"Starlink Fortran 77\" as
the language, then you should mention any language features whose use is
not sanctioned in the Starlink Programming Standards document (SGP/16).
Machine specific features should only be used where there is no
alternative.")))

  (setq sym (intern "BLOCK_DATA_DECLARATIONS" starfort-mode-placeholder-table))
  (set sym "
\\*	Type Definitions:
\\	IMPLICIT NONE              ! No implicit typing
\\
[global_constants]
[global_variables]
[local_constants]
[local_variables]
[global_data]")
  (setplist sym '((desc . "BLOCK DATA declarations template")))

  (setq sym (intern "Y2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "Y1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "Y0" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AND" starfort-mode-placeholder-table))
  (set sym ".AND.")
  (setplist sym '((desc . ".AND.")))

  (setq sym (intern "VC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "X2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TR" starfort-mode-placeholder-table))
  (set sym "TR")
  (setplist sym '((desc . "TR")))

  (setq sym (intern "VA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "X1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "X0" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ST" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TL" starfort-mode-placeholder-table))
  (set sym "TL")
  (setplist sym '((desc . "TL")))

  (setq sym (intern "SS" starfort-mode-placeholder-table))
  (set sym "SS")
  (setplist sym '((desc . "SS")))

  (setq sym (intern "RY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SP" starfort-mode-placeholder-table))
  (set sym "SP")
  (setplist sym '((desc . "SP")))

  (setq sym (intern "RX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "V2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "QY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "V1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IO_STMT" starfort-mode-placeholder-table))
  (set sym '(("OPEN" "OPEN statement" token) ("READ_STMT" "READ statement" token) ("WRITE_STMT" "WRITE statement" token) ("PRINT" "PRINT statement" token) ("CLOSE" "CLOSE statement" token) ("INQUIRE" "INQUIRE statement" token) ("REWIND" "REWIND statement" token)))
  (setplist sym '((desc . "I/O statement")))

  (setq sym (intern "CMPLX_CONS" starfort-mode-placeholder-table))
  (set sym "( {cmplx_cons_elm}, {cmplx_cons_elm} )")
  (setplist sym '((desc . "COMPLEX constants")))

  (setq sym (intern "QX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "P_" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Dummy argument.") (desc . "Function dummy argument") (sep . ", ")))

  (setq sym (intern "RM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WINDOW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EXPO" starfort-mode-placeholder-table))
  (set sym "**")
  (setplist sym '((desc . "**")))

  (setq sym (intern "PY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARGUMENT_ACCESS_MODE" starfort-mode-placeholder-table))
  (set sym '(("Given" "") ("Returned" "") ("Given and Returned" "")))

  (setq sym (intern "RD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR_FUNC_REF" starfort-mode-placeholder-table))
  (set sym "{char_func_name}( [arg_]... )")
  (setplist sym '((desc . "Character function reference")))

  (setq sym (intern "BAD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OR" starfort-mode-placeholder-table))
  (set sym ".OR.")
  (setplist sym '((desc . ".OR.")))

  (setq sym (intern "NY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "\.NOT\." starfort-mode-placeholder-table))
  (set sym ".NOT.")

  (setq sym (intern "NOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "R1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "R0" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NO" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VALUES" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "A_TASK_PROGRAM_MODULE" starfort-mode-placeholder-table))
  (set sym "SUBROUTINE {routine_name}( STATUS )
\\*+
\\*{a_task_prologue}
\\*-
\\	{a_task_declarations}
\\*.
\\
\\*	Check inherited global status.
\\	IF ( STATUS .NE. SAI__OK ) RETURN

[executable_statement]...

[contextual_error_report]
END")
  (setplist sym '((desc . "ADAM A_task (main routine of an ADAM application)")))

  (setq sym (intern "MM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ND" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PACK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ARITH_ASSIGN_ELM" starfort-mode-placeholder-table))
  (set sym '(("{arith_var}" "") ("{arith_array_ref}" "")))
  (setplist sym '((desc . "Arithmetic assignment element")))

  (setq sym (intern "NPIX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NORM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ABV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FORM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ADD" starfort-mode-placeholder-table))
  (set sym "+")
  (setplist sym '((desc . "+")))

  (setq sym (intern "IY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "JF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "HT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "HS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "JB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FONT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "HM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DEVNO" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DCUNIT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OUTSTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "K0" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AB2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GLOBAL_DATA" starfort-mode-placeholder-table))
  (set sym "\\*	Global Data:
\\	{data_stmt}...
")
  (setplist sym '((desc . "Define global data")))

  (setq sym (intern "AB1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "J2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NPEN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "J1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BOUNDS_EXP" starfort-mode-placeholder-table))
  (set sym "{bound_elm} [{aop} {bound_elm}]...")
  (setplist sym '((desc . "Dimension declaration bounds expression")))

  (setq sym (intern "FNUM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ACMODE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "I2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A character expression, numeric scalar memory reference, or numeric
array name reference whose value specifies the name of the file to be
inquired about")))

  (setq sym (intern "EQ" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "I1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTERNAL_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a brief description of what the internal function does.") (head . "!")))

  (setq sym (intern "FG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "V1950" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTRINSIC_FUNC_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The name of a Fortran intrinsic function.")))

  (setq sym (intern "R1950" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OPEN_PARAMETER" starfort-mode-placeholder-table))
  (set sym '(("ACCESS" "Specify sequential or direct file access (default=sequential)" token) ("BLANK" "Specify the interpretation of blanks in numeric fields" token) ("NO_CARRIAGE_CONTROL" "Turn off Fortran carriage control" token) ("ERR" "Specify a statement to branch to if an error occurs" token) ("FILE" "Specify the name of the file to be opened" token) ("FORM" "Specify whether the file is formatted or unformatted" token) ("IOSTAT" "Specify a variable to receive any I/O error code" token) ("READONLY" "Specify that file is to be read only (to allow sharing)" token) ("RECL" "Specify the length of records for direct access files" token) ("OPEN_STATUS" "Specify whether an old, new or scratch file is to be used" token)))
  (setplist sym '((sep . ", ")))

  (setq sym (intern "P1950" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NPCI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "EXTERNAL_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a brief description of what the external function does.") (head . "!")))

  (setq sym (intern "CS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BZ" starfort-mode-placeholder-table))
  (set sym "BZ")
  (setplist sym '((desc . "BZ")))

  (setq sym (intern "CR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PARAMETER_SPEC" starfort-mode-placeholder-table))
  (set sym "{parameter_name}[pdims] = {parameter_type} ({parameter_access_mode})
{parameter_description}
[parameter_default]")
  (setplist sym '((vert . t)))

  (setq sym (intern "NMEMAX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NPCD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "D1950" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BN" starfort-mode-placeholder-table))
  (set sym "BN")
  (setplist sym '((desc . "BN")))

  (setq sym (intern "CC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "AR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CB" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CA" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NVLUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "D1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "D0" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OPTIONAL_BLOCK_DATA_ITEMS" starfort-mode-placeholder-table))
  (set sym '(("Block_Data_Options" "Expands to a list of placeholders for all items below" token) ("Notes" nil token) ("Side_Effects" nil token) ("Deficiencies" nil token) ("Machine_Specifics" nil token) ("DIY_Prologue" nil token) ("References" nil token) ("Keywords" nil token) ("Copyright" nil token)))
  (setplist sym '((vert . t)))

  (setq sym (intern "AD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "UNIT_VALUE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The number of the logical unit to be inquired about.  The unit
does not have to exist, nor does it need to be connected to a
file.  If the unit is connected to a file, the inquiry encompasses
both the connection and the file.")))

  (setq sym (intern "LEVEL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "A_TASK_DECLARATIONS" starfort-mode-placeholder-table))
  (set sym "
\\*	Type Definitions:
\\	IMPLICIT NONE              ! No implicit typing
\\
\\*	Global Constants:
\\	INCLUDE 'SAE_PAR'          ! Standard SAE constants
\\	[include_global_constants]...

[global_variables]
\\*	Status:
\\	INTEGER STATUS             ! Global status
\\
[external_references]
[local_constants]
[local_variables]
[internal_references]
[local_data]")
  (setplist sym '((desc . "ADAM A_task declarations template")))

  (setq sym (intern "B2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "B1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "A2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "A1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COMMON_ELM" starfort-mode-placeholder-table))
  (set sym '(("var" nil place) ("array_name" nil place) ("array_decl" nil place)))
  (setplist sym '((desc . "Common list element") (sep . ", ")))

  (setq sym (intern "NEVAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ACTVAL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INMID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MINCHX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FUNCTION_TYPE" starfort-mode-placeholder-table))
  (set sym '(("INTEGER" nil token) ("REAL" nil token) ("CHARACTER_N" nil token) ("CHARACTER_*" nil token) ("LOGICAL" nil token) ("DOUBLE_PRECISION" nil token) ("WORD" nil token) ("BYTE" nil token) ("COMPLEX" nil token)))
  (setplist sym '((desc . "Function data type")))

  (setq sym (intern "OFFS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "V2000" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OPTIONAL_SUBROUTINE_ITEMS" starfort-mode-placeholder-table))
  (set sym '(("Subroutine_Options" "Expands to a list of placeholders for all items below" token) ("Examples" nil token) ("Pitfalls" nil token) ("Notes" nil token) ("Prior_Requirements" nil token) ("Side_Effects" nil token) ("Algorithm" nil token) ("Accuracy" nil token) ("Timing" nil token) ("Routines_Used" nil token) ("Deficiencies" nil token) ("Machine_Specifics" nil token) ("DIY_Prologue" nil token) ("References" nil token) ("Keywords" nil token) ("Copyright" nil token)))
  (setplist sym '((vert . t)))

  (setq sym (intern "LOCTR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "R2000" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "VLUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "P2000" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OPEN_STATUS_OPTIONS" starfort-mode-placeholder-table))
  (set sym '(("UNKNOWN" "Create a new file if one does not already exist" token) ("OLD" "Open an existing file" token) ("NEW" "Create a new file" tokn) ("SCRATCH" "Use a (new) scratch file" token)))
  (setplist sym '((desc . "STATUS options")))

  (setq sym (intern "MINCHH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MULT" starfort-mode-placeholder-table))
  (set sym "*")
  (setplist sym '((desc . "*")))

  (setq sym (intern "FILTER" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "D2000" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WKNAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "X_EDIT_FIELD" starfort-mode-placeholder-table))
  (set sym "{xnum}X")
  (setplist sym '((desc . "X")))

  (setq sym (intern "OPEN_STMT" starfort-mode-placeholder-table))
  (set sym "OPEN ( UNIT = {unit}, {open_parameter}... )")
  (setplist sym '((desc . "OPEN statement") (vert . t)))

  (setq sym (intern "IOSTAT_PAR" starfort-mode-placeholder-table))
  (set sym "IOSTAT = {integer_var}")
  (setplist sym '((desc . "IOSTAT = {integer_var}")))

  (setq sym (intern "TIMING" starfort-mode-placeholder-table))
  (set sym "Timing:
{routine_timing}
\\")
  (setplist sym '((desc . "Details of the routine's execution time")))

  (setq sym (intern "TSTRCT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "T_EDIT_FIELD" starfort-mode-placeholder-table))
  (set sym '(("T" nil place) ("TL" nil place) ("TR" nil place)))
  (setplist sym '((desc . "T edit descriptor")))

  (setq sym (intern "OUTID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "S_EDIT_FIELD" starfort-mode-placeholder-table))
  (set sym '(("S" nil place) ("SS" nil place) ("SP" nil place)))
  (setplist sym '((desc . "S, SS, SP")))

  (setq sym (intern "GDPL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SGDEL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RESLT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NMEM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IMPLICIT_STMT" starfort-mode-placeholder-table))
  (set sym '(("implicit_none" nil place)))
  (setplist sym '((desc . "IMPLICIT statement")))

  (setq sym (intern "ACTROU" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "JFLAG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IFLAG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NUMERIC_ARRAY_REF" starfort-mode-placeholder-table))
  (set sym "{arith_array_name}( {subscr}... )")
  (setplist sym '((desc . "Numeric Array reference")))

  (setq sym (intern "B_EDIT_FIELD" starfort-mode-placeholder-table))
  (set sym '(("BN" nil place) ("BZ" nil place)))
  (setplist sym '((desc . "BN, BZ")))

  (setq sym (intern "ROUTINE_LANGUAGE" starfort-mode-placeholder-table))
  (set sym '(("language_description" nil place) ("Starlink_Fortran_77" nil place)))

  (setq sym (intern "COMMENT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A comment describing the code which follows.  Comments should be entered
in lower case (to distinguish them from lines of code, in upper case),
correctly punctuated (including full stops), and with appropriate
capitalisation of letters at the beginning of sentences, etc.  References
to program variables should be in upper case.  Comments should begin in
column four.") (head . "
*")))

  (setq sym (intern "UNITS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "BMDSCR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NLEV" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOGICAL_EXP_PAREN" starfort-mode-placeholder-table))
  (set sym "( {logical_exp} )")
  (setplist sym '((desc . "Logical expression in parentheses")))

  (setq sym (intern "NUMBER_PAR" starfort-mode-placeholder-table))
  (set sym "NUMBER = {integer_var}")
  (setplist sym '((desc . "NUMBER = {integer_var}")))

  (setq sym (intern "ADAM_ACTION" starfort-mode-placeholder-table))
  (set sym "\\*	[comment]
\\	ELSE IF ( NAME .EQ. '{action_name}' ) THEN
CALL {action_name}( STATUS )
")
  (setplist sym '((desc . "Test for and execute an action in an ADAM monolith") (vert . t) (head . "
")))

  (setq sym (intern "WDAY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COLREP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "WTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NLCD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DATREF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ITYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FLAG" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DATREC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "FTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DTYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STYLI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "USER" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "STYLE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NITT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LSE$LINE_COMMENT" starfort-mode-placeholder-table))
  (set sym "
! [comment]")

  (setq sym (intern "IFILE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "An internal file specifier")))

  (setq sym (intern "LUTNUM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DEPTH" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CONSTANT_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "The name of the local constant to be defined.") (auto . t)))

  (setq sym (intern "NODENAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MONOLITH_PROGRAM_MODULE" starfort-mode-placeholder-table))
  (set sym "SUBROUTINE {routine_name}( STATUS )
\\*+
\\*{monolith_prologue}
\\*-
\\	{monolith_declarations}
\\*.
\\
\\*	Check inherited global status.
\\	IF ( STATUS .NE. SAI__OK ) RETURN

\\*	Get the action name.
\\	CALL TASK_GET_NAME( NAME, STATUS )

\\*	Test the action name against each valid value in turn, calling the
appropriate routine...
\\
\\*	[comment]
\\	IF ( NAME .EQ. '{action_name}' ) THEN
CALL {action_name}( STATUS )

[ADAM_action]...

\\*	If the action name is not recognised, then report an error.
\\	ELSE
STATUS = SAI__ERROR
CALL MSG_SETC( 'NAME', NAME )
CALL ERR_REP( '{routine_name}_ERR',
\\     :	'{routine_name}: The action name ''^NAME'' is ' //
\\     :	'not recognised by the {routine_name} monolith.',
\\     :	STATUS )
END IF

END")
  (setplist sym '((desc . "ADAM monolith program module")))

  (setq sym (intern "ITTNUM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOGICAL_ARRAY_REF" starfort-mode-placeholder-table))
  (set sym "{logical_array_name}( {subscr}... )")
  (setplist sym '((desc . "Logical Array reference")))

  (setq sym (intern "REPLY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "TYPE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "UNLOAD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$HIL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CMPLX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_TIMING" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give details of the processor time required by the routine (and/or the
elapsed time if relevant).  Since the absolute time will depend on the
type of machine in use, you should normally only give relative
information, describing how the timing depends on the mode of use, e.g:

   \"Processor time is approximately proportional to the square root
   of the number of elements in the DATA array.\"

This information may be omitted if the time requirement is not expected
to be significant in normal use.")))

  (setq sym (intern "FILE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LMORE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DYIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NBLOCK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SIDE_EFFECTS" starfort-mode-placeholder-table))
  (set sym "Side Effects:
{routine_side_effects}...
\\")
  (setplist sym '((desc . "Details of any side effects")))

  (setq sym (intern "EPS0" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DBUFSK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DEFICIENCY" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Describe any deficiency in the current implementation of the routine.
Examples are; limited efficiency, limited accuracy or circumstances in
which the routine does not function adequately.  In this latter case you
should be careful to describe what will actually happen if the routine
cannot cope.  It may be helpful to future developers if you indicate
how improvements might be made.")))

  (setq sym (intern "POSVEL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RESVEC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GKS$RELPRI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "INTEGER_EXP" starfort-mode-placeholder-table))
  (set sym "{int_elm} [{aop} {int_elm}]...")
  (setplist sym '((desc . "Integer expression")))

  (setq sym (intern "MOUT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IBLOCK" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ELSE_IF_THEN" starfort-mode-placeholder-table))
  (set sym "ELSE IF ( {logical_exp} ) THEN
{executable_statement}...	")
  (setplist sym '((desc . "ELSE IF ( {logical_exp} ) THEN") (vert . t)))

  (setq sym (intern "ANGLE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LWSC" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "OUTMID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ROUTINE_REFERENCES" starfort-mode-placeholder-table))
  (set sym "-  {reference}")
  (setplist sym '((vert . t)))

  (setq sym (intern "GKS$ESW" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DXIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "GLOBAL_VARIABLES_DESCRIPTION" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Give a brief description of the contents of the global variables file.") (head . "!")))

  (setq sym (intern "EXTERNAL_DECLARATION" starfort-mode-placeholder-table))
  (set sym "EXTERNAL {external_name}")

  (setq sym (intern "NUMCUR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOCTR2" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOCTR1" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COMENT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "COEFFS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CMODE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "USEDEF" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SYSTEM" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NYSUBD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NXSUBD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LUTLIS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MPAI" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NFPP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "Z" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SYSTAT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "Y" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "X" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "W" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "V" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "U" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "T" starfort-mode-placeholder-table))
  (set sym "T")
  (setplist sym '((desc . "T")))

  (setq sym (intern "S" starfort-mode-placeholder-table))
  (set sym "S")
  (setplist sym '((desc . "S")))

  (setq sym (intern "R" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "PROLOGUES" starfort-mode-placeholder-table))
  (set sym '(("A_Task_prologue" nil token) ("Subroutine_prologue" nil token) ("Function_prologue" nil token) ("Block_Data_prologue" nil token) ("Monolith_prologue" nil token)))
  (setplist sym '((desc . "Menu of prologue templates for different program modules")))

  (setq sym (intern "P" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Dummy argument.") (desc . "Subroutine dummy argument") (sep . ", ") (head . "(") (tail . ")")))

  (setq sym (intern "N" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "A decimal digit") (desc . "String of decimal digits")))

  (setq sym (intern "M" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR_VAR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Character variable") (desc . "Character variable")))

  (setq sym (intern "MONOLITH_DECLARATIONS" starfort-mode-placeholder-table))
  (set sym "
\\*	Type Definitions:
\\	IMPLICIT NONE              ! No implicit typing
\\
\\*	Global Constants:
\\	INCLUDE 'SAE_PAR'          ! Standard SAE constants
\\	INCLUDE 'PAR_PAR'          ! PAR_ public constants
\\
\\*	Status:
\\	INTEGER STATUS             ! Global status
\\
\\*	Local Variables:
\\	CHARACTER * ( PAR__SZNAM ) NAME ! Action name
\\")
  (setplist sym '((desc . "MONOLITH declarations")))

  (setq sym (intern "K" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "J" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "I" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "H" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "E" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DESCRIPTIONS_OF_GLOBAL_VARIABLES_REFERENCED" starfort-mode-placeholder-table))
  (set sym "{global_name}[dimensions] = {data_type} ({global_access_mode})
[global_variable_purpose]")
  (setplist sym '((vert . t)))

  (setq sym (intern "D" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DATMIN" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "C" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "ITTLIS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "B" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "A" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "SYMBOL" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "CHAR_ARRAY_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Character array name") (desc . "Character array name")))

  (setq sym (intern "NGDP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "LOGICAL_ARRAY_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Logical array name") (desc . "Logical array name")))

  (setq sym (intern "MEMDEP" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MODE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "/" starfort-mode-placeholder-table))
  (set sym "/")

  (setq sym (intern "LOGICAL_FUNC_NAME" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Logical function name") (desc . "Logical function name")))

  (setq sym (intern "NERR" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "NEQV" starfort-mode-placeholder-table))
  (set sym ".NEQV.")
  (setplist sym '((desc . ".NEQV.")))

  (setq sym (intern "MPMBTE" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "UNIT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "An integer expression with a value in the range of 0 through 99
that refers to a specific file or I/O device.  To avoid possible
clashes between the unit numbers used in separate parts of a program
these numbers should not be 'hard-wired', but should be allocated
when required by calling the routine FIO_GUNIT (and subsequently
deallocated by calling FIO_PUNIT).")))

  (setq sym (intern "LVIS" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "MEMID" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "RELATIONAL_EXP" starfort-mode-placeholder-table))
  (set sym '(("arith_relational_exp" nil token) ("char_relational_exp" nil token)))
  (setplist sym '((desc . "Relational Expression")))

  (setq sym (intern "ITTON" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "IWRITE_PARAMETER" starfort-mode-placeholder-table))
  (set sym '(("IOSTAT" "Specify a variable to receive any I/O error code" token) ("END" "Specify a statement to branch to on end-of-file condition" token) ("ERR" "Specify a statement to branch to if an error occurs" token)))
  (setplist sym '((sep . ", ") (head . ",")))

  (setq sym (intern "NENT" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DATMAX" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))

  (setq sym (intern "DEFINE_MESSAGE_TOKEN" starfort-mode-placeholder-table))
  (set sym "CALL MSG_{msg_token_routine}")
  (setplist sym '((vert . t)))

  (setq sym (intern "MMOD" starfort-mode-placeholder-table))
  (set sym 'nil)
  (setplist sym '((help . "Please supply an argument value.")))
)
(defvar starfort-mode-token-table (make-vector 4095 0))
(let (sym)
  (setq sym (intern "CLOSE_STATUS" starfort-mode-token-table))
  (set sym "STATUS = {close_status_options}")
  (setplist sym '((class . token) (desc . "STATUS = {close_status_options}")))

  (setq sym (intern "SLA_EPJ2D" starfort-mode-token-table))
  (set sym "SLA_EPJ2D( {epj} )")
  (setplist sym '((class . token) (desc . "Conversion of Julian Epoch to Modified Julian Date")))

  (setq sym (intern "VEC_MODI" starfort-mode-token-table))
  (set sym "VEC_MODI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two INTEGER vectorised arrays")))

  (setq sym (intern "VEC_MODD" starfort-mode-token-table))
  (set sym "VEC_MODD( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two DOUBLE PRECISION vectorised arrays")))

  (setq sym (intern "TRN__TYPIN" starfort-mode-token-table))
  (set sym "TRN__TYPIN")
  (setplist sym '((class . token) (desc . "Type invalid (error code)")))

  (setq sym (intern "VEC_MODB" starfort-mode-token-table))
  (set sym "VEC_MODB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two BYTE vectorised arrays")))

  (setq sym (intern "NUM_RTOW" starfort-mode-token-table))
  (set sym "NUM_RTOW( {num} )")
  (setplist sym '((class . token) (desc . "Convert a REAL number to WORD")))

  (setq sym (intern "NDF_NEWP" starfort-mode-token-table))
  (set sym "NDF_NEWP( {ftype}, {ndim}, {ubnd}, {place}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Create a new primitive NDF") (helpkey . "NDF_NEWP")))

  (setq sym (intern "AGD_ACTIV" starfort-mode-token-table))
  (set sym "AGD_ACTIV( {status} )")
  (setplist sym '((class . token) (desc . "Initialise IDI")))

  (setq sym (intern "NUM_BTOW" starfort-mode-token-table))
  (set sym "NUM_BTOW( {num} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE number to WORD")))

  (setq sym (intern "NUM_RTOR" starfort-mode-token-table))
  (set sym "NUM_RTOR( {num} )")
  (setplist sym '((class . token) (desc . "Convert a REAL number to REAL")))

  (setq sym (intern "NUM_BTOR" starfort-mode-token-table))
  (set sym "NUM_BTOR( {num} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE number to REAL")))

  (setq sym (intern "TRN__MIOPR" starfort-mode-token-table))
  (set sym "TRN__MIOPR")
  (setplist sym '((class . token) (desc . "Missing or invalid operator (error code)")))

  (setq sym (intern "NUM_RTOI" starfort-mode-token-table))
  (set sym "NUM_RTOI( {num} )")
  (setplist sym '((class . token) (desc . "Convert a REAL number to INTEGER")))

  (setq sym (intern "NUM_BTOI" starfort-mode-token-table))
  (set sym "NUM_BTOI( {num} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE number to INTEGER")))

  (setq sym (intern "NUM_RTOD" starfort-mode-token-table))
  (set sym "NUM_RTOD( {num} )")
  (setplist sym '((class . token) (desc . "Convert a REAL number to DOUBLE PRECISION")))

  (setq sym (intern "BLOCK_DATA_PROLOGUE" starfort-mode-token-table))
  (set sym "\\*+
\\*{block_data_prologue}
\\*-
\\	{block_data_declarations}
\\*.")
  (setplist sym '((class . token) (desc . "Block Data prologue")))

  (setq sym (intern "NUM_RTOB" starfort-mode-token-table))
  (set sym "NUM_RTOB( {num} )")
  (setplist sym '((class . token) (desc . "Convert a REAL number to BYTE")))

  (setq sym (intern "NUM_BTOD" starfort-mode-token-table))
  (set sym "NUM_BTOD( {num} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE number to DOUBLE PRECISION")))

  (setq sym (intern "NUM_BTOB" starfort-mode-token-table))
  (set sym "NUM_BTOB( {num} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE number to BYTE")))

  (setq sym (intern "CHR_SIMLR" starfort-mode-token-table))
  (set sym "CHR_SIMLR( {str1}, {str2} )")
  (setplist sym '((class . token) (desc . "Determine whether two strings are equal apart from case")))

  (setq sym (intern "TRN__MIOPA" starfort-mode-token-table))
  (set sym "TRN__MIOPA")
  (setplist sym '((class . token) (desc . "Missing or invalid operand (error code)")))

  (setq sym (intern "VEC_UWTOUW" starfort-mode-token-table))
  (set sym "VEC_UWTOUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD vectorised array to UNSIGNED WORD")))

  (setq sym (intern "PGVSTAND" starfort-mode-token-table))
  (set sym "PGVSTAND")
  (setplist sym '((class . token) (desc . "Set standard (default) viewport")))

  (setq sym (intern "AGD_ASSOC" starfort-mode-token-table))
  (set sym "AGD_ASSOC( {param}, {acmode}, {pname}, {memid}, {picid}, {dispid}, {xsize}, {ysize}, {xoff}, {yoff}, {status} )")
  (setplist sym '((class . token) (desc . "Associate a device with AGI and IDI")))

  (setq sym (intern "IIMBLM" starfort-mode-token-table))
  (set sym "IIMBLM( {dispid}, {memid}, {nmem}, {blinks}, {status} )")
  (setplist sym '((class . token) (desc . "Blink Memories")))

  (setq sym (intern "IICSCV" starfort-mode-token-table))
  (set sym "IICSCV( {dispid}, {numcur}, {lvis}, {status} )")
  (setplist sym '((class . token) (desc . "Set Cursor Visibility")))

  (setq sym (intern "PGLINE" starfort-mode-token-table))
  (set sym "PGLINE( {n}, {xpts}, {ypts} )")
  (setplist sym '((class . token) (desc . "Draw a polyline (curve defined by line-segments)")))

  (setq sym (intern "GRDITM" starfort-mode-token-table))
  (set sym "GRDITM( {wkid}, {mldr}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Read item from GKSM")))

  (setq sym (intern "CHR_FANDL" starfort-mode-token-table))
  (set sym "CHR_FANDL( {string}, {index1}, {index2} )")
  (setplist sym '((class . token) (desc . "Find the indices of the first and last non-blank characters")))

  (setq sym (intern "VEC_UWTOUB" starfort-mode-token-table))
  (set sym "VEC_UWTOUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD vectorised array to UNSIGNED BYTE")))

  (setq sym (intern "VEC_UBTOUW" starfort-mode-token-table))
  (set sym "VEC_UBTOUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE vectorised array to UNSIGNED WORD")))

  (setq sym (intern "GGTITM" starfort-mode-token-table))
  (set sym "GGTITM( {wkid}, {type}, {ldr} )")
  (setplist sym '((class . token) (desc . "Get item type from GKSM")))

  (setq sym (intern "SUBROUTINE_SECTIONS" starfort-mode-token-table))
  (set sym '(("SUBROUTINE_SECTIONS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "ASSIGNMENT_CHAR" starfort-mode-token-table))
  (set sym "{char_assign_elm} = {char_exp}")
  (setplist sym '((class . token) (desc . "Character ASSIGNMENT Statement")))

  (setq sym (intern "SLA_VN" starfort-mode-token-table))
  (set sym "SLA_VN( {v}, {uv}, {vm} )")
  (setplist sym '((class . token) (desc . "Normalises a 3-vector also giving the modulus")))

  (setq sym (intern "SGS_SARTX" starfort-mode-token-table))
  (set sym "SGS_SARTX( {r} )")
  (setplist sym '((class . token) (desc . "Set aspect ratio of text")))

  (setq sym (intern "MONOLITH_OPTIONS" starfort-mode-token-table))
  (set sym "[notes]
[prior_requirements]
[side_effects]
[routines_used]
[deficiencies]
[DIY_prologue_item]...
[references]
[keywords]
[copyright]")
  (setplist sym '((class . token) (desc . "Expanded list of all optional items")))

  (setq sym (intern "SLA_GMST" starfort-mode-token-table))
  (set sym "SLA_GMST( {ut1} )")
  (setplist sym '((class . token) (desc . "Conversion from universal time to sidereal time")))

  (setq sym (intern "VEC_UBTOUB" starfort-mode-token-table))
  (set sym "VEC_UBTOUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE vectorised array to UNSIGNED BYTE")))

  (setq sym (intern "SLA_NUTC" starfort-mode-token-table))
  (set sym "SLA_NUTC( {date}, {dpsi}, {deps}, {eps0} )")
  (setplist sym '((class . token) (desc . "Nutation: longitude & obliquity components and mean obliquity (IAU 1980 theory)")))

  (setq sym (intern "VEC_NEGW" starfort-mode-token-table))
  (set sym "VEC_NEGW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of WORD vectorised array")))

  (setq sym (intern "VEC_DTOW" starfort-mode-token-table))
  (set sym "VEC_DTOW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION vectorised array to WORD")))

  (setq sym (intern "NDF_TUNE" starfort-mode-token-table))
  (set sym "NDF_TUNE( {value}, {tpar}, {status} )")
  (setplist sym '((class . token) (desc . "Set an NDF_ system tuning parameter") (helpkey . "NDF_TUNE")))

  (setq sym (intern "VEC_NEGR" starfort-mode-token-table))
  (set sym "VEC_NEGR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of REAL vectorised array")))

  (setq sym (intern "AGI_TDDTW" starfort-mode-token-table))
  (set sym "AGI_TDDTW( {picid}, {nxy}, {dx}, {dy}, {wx}, {wy}, {status} )")
  (setplist sym '((class . token) (desc . "Transform double precision data to world coordinates")))

  (setq sym (intern "VEC_DTOR" starfort-mode-token-table))
  (set sym "VEC_DTOR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION vectorised array to REAL")))

  (setq sym (intern "ERR_LOAD" starfort-mode-token-table))
  (set sym "ERR_LOAD( {param}, {parlen}, {opstr}, {oplen}, {status} )")
  (setplist sym '((class . token) (desc . "Return error messages from the current error context") (helpkey . "ERR_LOAD")))

  (setq sym (intern "VEC_NEGI" starfort-mode-token-table))
  (set sym "VEC_NEGI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of INTEGER vectorised array")))

  (setq sym (intern "VEC_DTOI" starfort-mode-token-table))
  (set sym "VEC_DTOI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION vectorised array to INTEGER")))

  (setq sym (intern "VEC_NEGD" starfort-mode-token-table))
  (set sym "VEC_NEGD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of DOUBLE PRECISION vectorised array")))

  (setq sym (intern "VEC_DTOD" starfort-mode-token-table))
  (set sym "VEC_DTOD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION vectorised array to DOUBLE PRECISION")))

  (setq sym (intern "VEC_NEGB" starfort-mode-token-table))
  (set sym "VEC_NEGB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of BYTE vectorised array")))

  (setq sym (intern "VEC_DTOB" starfort-mode-token-table))
  (set sym "VEC_DTOB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION vectorised array to BYTE")))

  (setq sym (intern "IICRCP" starfort-mode-token-table))
  (set sym "IICRCP( {dispid}, {inmid}, {numcur}, {xc}, {yc}, {outmid}, {status} )")
  (setplist sym '((class . token) (desc . "Read Cursor Position")))

  (setq sym (intern "DAT_EXIST" starfort-mode-token-table))
  (set sym "DAT_EXIST( {param}, {mode}, {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Associate an existing data object with an ADAM parameter")))

  (setq sym (intern "REFERENCES" starfort-mode-token-table))
  (set sym '(("REFERENCES" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "IIZWZP" starfort-mode-token-table))
  (set sym "IIZWZP( {dispid}, {xoff}, {yoff}, {zoomf}, {status} )")
  (setplist sym '((class . token) (desc . "Write Display Zoom and Pan")))

  (setq sym (intern "AGD_DEASS" starfort-mode-token-table))
  (set sym "AGD_DEASS( {param}, {parcan}, {status} )")
  (setplist sym '((class . token) (desc . "Deassociate a device from AGI and IDI")))

  (setq sym (intern "SLA_PM" starfort-mode-token-table))
  (set sym "SLA_PM( {r0}, {d0}, {pr}, {pd}, {px}, {rv}, {ep0}, {ep1}, {r1}, {d1} )")
  (setplist sym '((class . token) (desc . "Apply corrections for proper motion to a star RA,Dec")))

  (setq sym (intern "IIZWZM" starfort-mode-token-table))
  (set sym "IIZWZM( {dispid}, {memid}, {nmem}, {zoomf}, {status} )")
  (setplist sym '((class . token) (desc . "Write Memory Zoom")))

  (setq sym (intern "NDF_ANORM" starfort-mode-token-table))
  (set sym "NDF_ANORM( {indf}, {iaxis}, {norm}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the logical value of an NDF axis normalisation flag") (helpkey . "NDF_ANORM")))

  (setq sym (intern "MSG_LOAD" starfort-mode-token-table))
  (set sym "MSG_LOAD( {param}, {text}, {opstr}, {oplen}, {status} )")
  (setplist sym '((class . token) (desc . "Expand and return a message") (helpkey . "MSG_LOAD")))

  (setq sym (intern "NDF_AFORM" starfort-mode-token-table))
  (set sym "NDF_AFORM( {indf}, {comp}, {iaxis}, {form}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the storage form of an NDF axis array") (helpkey . "NDF_AFORM")))

  (setq sym (intern "NUM_WTOUW" starfort-mode-token-table))
  (set sym "NUM_WTOUW( {num} )")
  (setplist sym '((class . token) (desc . "Convert a WORD number to UNSIGNED WORD")))

  (setq sym (intern "BLANK_PAR" starfort-mode-token-table))
  (set sym '(("BLANK_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NUM_RTOUW" starfort-mode-token-table))
  (set sym "NUM_RTOUW( {num} )")
  (setplist sym '((class . token) (desc . "Convert a REAL number to UNSIGNED WORD")))

  (setq sym (intern "SLA_RANORM" starfort-mode-token-table))
  (set sym "SLA_RANORM( {angle} )")
  (setplist sym '((class . token) (desc . "Normalise angle into range 0-2 pi (single precision)")))

  (setq sym (intern "NUM_NEGUW" starfort-mode-token-table))
  (set sym "NUM_NEGUW( {num} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of UNSIGNED WORD number")))

  (setq sym (intern "KEYWORDS" starfort-mode-token-table))
  (set sym '(("KEYWORDS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VAL_EXPUW" starfort-mode-token-table))
  (set sym "VAL_EXPUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of UNSIGNED WORD value")))

  (setq sym (intern "NUM_ITOUW" starfort-mode-token-table))
  (set sym "NUM_ITOUW( {num} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER number to UNSIGNED WORD")))

  (setq sym (intern "NUM_DTOUW" starfort-mode-token-table))
  (set sym "NUM_DTOUW( {num} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION number to UNSIGNED WORD")))

  (setq sym (intern "DAT__SZTYP" starfort-mode-token-table))
  (set sym "DAT__SZTYP")
  (setplist sym '((class . token) (desc . "Size of HDS data type string (symbolic constant)")))

  (setq sym (intern "NUM_WTOUB" starfort-mode-token-table))
  (set sym "NUM_WTOUB( {num} )")
  (setplist sym '((class . token) (desc . "Convert a WORD number to UNSIGNED BYTE")))

  (setq sym (intern "NUM_BTOUW" starfort-mode-token-table))
  (set sym "NUM_BTOUW( {num} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE number to UNSIGNED WORD")))

  (setq sym (intern "NUM_RTOUB" starfort-mode-token-table))
  (set sym "NUM_RTOUB( {num} )")
  (setplist sym '((class . token) (desc . "Convert a REAL number to UNSIGNED BYTE")))

  (setq sym (intern "PGCONX" starfort-mode-token-table))
  (set sym "PGCONX( {a}, {idim}, {jdim}, {i1}, {i2}, {j1}, {j2}, {c}, {nc}, {plot} )")
  (setplist sym '((class . token) (desc . "Contour map of a 2D data array (non-rectangular)")))

  (setq sym (intern "PGCONT" starfort-mode-token-table))
  (set sym "PGCONT( {a}, {idim}, {jdim}, {i1}, {i2}, {j1}, {j2}, {c}, {nc}, {tr} )")
  (setplist sym '((class . token) (desc . "Contour map of a 2D data array (contour-following)")))

  (setq sym (intern "NUM_NEGUB" starfort-mode-token-table))
  (set sym "NUM_NEGUB( {num} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of UNSIGNED BYTE number")))

  (setq sym (intern "PGCONS" starfort-mode-token-table))
  (set sym "PGCONS( {a}, {idim}, {jdim}, {i1}, {i2}, {j1}, {j2}, {c}, {nc}, {tr} )")
  (setplist sym '((class . token) (desc . "Contour map of a 2D data array (fast algorithm)")))

  (setq sym (intern "VAL_EXPUB" starfort-mode-token-table))
  (set sym "VAL_EXPUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of UNSIGNED BYTE value")))

  (setq sym (intern "SGS_DISCU" starfort-mode-token-table))
  (set sym "SGS_DISCU")
  (setplist sym '((class . token) (desc . "Disable sample cursor")))

  (setq sym (intern "NUM_ITOUB" starfort-mode-token-table))
  (set sym "NUM_ITOUB( {num} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER number to UNSIGNED BYTE")))

  (setq sym (intern "SGS_MARKL" starfort-mode-token-table))
  (set sym "SGS_MARKL( {mtype} )")
  (setplist sym '((class . token) (desc . "Draw marker at end of polyline")))

  (setq sym (intern "NDF_ANNUL" starfort-mode-token-table))
  (set sym "NDF_ANNUL( {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Annul an NDF identifier") (helpkey . "NDF_ANNUL")))

  (setq sym (intern "VAL_MODW" starfort-mode-token-table))
  (set sym "VAL_MODW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two WORD values")))

  (setq sym (intern "NUM_DTOUB" starfort-mode-token-table))
  (set sym "NUM_DTOUB( {num} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION number to UNSIGNED BYTE")))

  (setq sym (intern "NUM_BTOUB" starfort-mode-token-table))
  (set sym "NUM_BTOUB( {num} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE number to UNSIGNED BYTE")))

  (setq sym (intern "VAL_MODR" starfort-mode-token-table))
  (set sym "VAL_MODR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two REAL values")))

  (setq sym (intern "SGS_OPEN" starfort-mode-token-table))
  (set sym "SGS_OPEN( {wkstn}, {izonid}, {status} )")
  (setplist sym '((class . token) (desc . "Open graphics (including GKS if necessary)")))

  (setq sym (intern "PGCONB" starfort-mode-token-table))
  (set sym "PGCONB( {a}, {idim}, {jdim}, {i1}, {i2}, {j1}, {j2}, {c}, {nc}, {tr}, {blank} )")
  (setplist sym '((class . token) (desc . "Contour map of a 2D data array, with blanking")))

  (setq sym (intern "SLA_INTIN" starfort-mode-token-table))
  (set sym "SLA_INTIN( {string}, {nstrt}, {ireslt}, {jflag} )")
  (setplist sym '((class . token) (desc . "Convert free-format input into integer")))

  (setq sym (intern "VAL_MODI" starfort-mode-token-table))
  (set sym "VAL_MODI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two INTEGER values")))

  (setq sym (intern "ARY_UNMAP" starfort-mode-token-table))
  (set sym "ARY_UNMAP( {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Unmap an array")))

  (setq sym (intern "DAT__TRUNC" starfort-mode-token-table))
  (set sym "DAT__TRUNC")
  (setplist sym '((class . token) (desc . "Text truncated (error code)")))

  (setq sym (intern "VAL_MODD" starfort-mode-token-table))
  (set sym "VAL_MODD( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two DOUBLE PRECISION values")))

  (setq sym (intern "IIZWSC" starfort-mode-token-table))
  (set sym "IIZWSC( {dispid}, {memid}, {nmem}, {xoff}, {yoff}, {status} )")
  (setplist sym '((class . token) (desc . "Write Memory Scroll")))

  (setq sym (intern "VAL_MODB" starfort-mode-token-table))
  (set sym "VAL_MODB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two BYTE values")))

  (setq sym (intern "IIRWRI" starfort-mode-token-table))
  (set sym "IIRWRI( {dispid}, {memid}, {roiid}, {xmin}, {ymin}, {xmax}, {ymax}, {status} )")
  (setplist sym '((class . token) (desc . "Write Rectangular Region of Interest")))

  (setq sym (intern "IIDIAG" starfort-mode-token-table))
  (set sym "IIDIAG( {dispid}, {outid}, {status} )")
  (setplist sym '((class . token) (desc . "Diagnostic Routine")))

  (setq sym (intern "PGTEXT" starfort-mode-token-table))
  (set sym "PGTEXT( {x}, {y}, {text} )")
  (setplist sym '((class . token) (desc . "Write text (horizontal, left-justified)")))

  (setq sym (intern "NUM_TANR" starfort-mode-token-table))
  (set sym "NUM_TANR( {num} )")
  (setplist sym '((class . token) (desc . "Tangent function of REAL number (radians)")))

  (setq sym (intern "NUM_SINR" starfort-mode-token-table))
  (set sym "NUM_SINR( {num} )")
  (setplist sym '((class . token) (desc . "Sine function of REAL number (radians)")))

  (setq sym (intern "TRN__DIMIN" starfort-mode-token-table))
  (set sym "TRN__DIMIN")
  (setplist sym '((class . token) (desc . "Dimensions invalid (error code)")))

  (setq sym (intern "NUM_TAND" starfort-mode-token-table))
  (set sym "NUM_TAND( {num} )")
  (setplist sym '((class . token) (desc . "Tangent function of DOUBLE PRECISION number (radians)")))

  (setq sym (intern "NUM_SIND" starfort-mode-token-table))
  (set sym "NUM_SIND( {num} )")
  (setplist sym '((class . token) (desc . "Sine function of DOUBLE PRECISION number (radians)")))

  (setq sym (intern "PGPAPER" starfort-mode-token-table))
  (set sym "PGPAPER( {width}, {aspect} )")
  (setplist sym '((class . token) (desc . "Change the size of the view surface")))

  (setq sym (intern "SLA_DR2TF" starfort-mode-token-table))
  (set sym "SLA_DR2TF( {ndp}, {angle}, {sign}, {ihmsf} )")
  (setplist sym '((class . token) (desc . "Convert an angle in radians to hours, minutes, seconds")))

  (setq sym (intern "SLA_CR2TF" starfort-mode-token-table))
  (set sym "SLA_CR2TF( {ndp}, {angle}, {sign}, {ihmsf} )")
  (setplist sym '((class . token) (desc . "Convert an angle in radians into hours, minutes, seconds")))

  (setq sym (intern "SGS_CIRCL" starfort-mode-token-table))
  (set sym "SGS_CIRCL( {x}, {y}, {r} )")
  (setplist sym '((class . token) (desc . "Draw circle")))

  (setq sym (intern "GQCNTN" starfort-mode-token-table))
  (set sym "GQCNTN( {errind}, {ctnr} )")
  (setplist sym '((class . token) (desc . "Inquire current normalization transformation number")))

  (setq sym (intern "MAG_CANCL" starfort-mode-token-table))
  (set sym "MAG_CANCL( {param}, {status} )")
  (setplist sym '((class . token) (desc . "Close tape device")))

  (setq sym (intern "AGD_DEACT" starfort-mode-token-table))
  (set sym "AGD_DEACT( {status} )")
  (setplist sym '((class . token) (desc . "Close down IDI")))

  (setq sym (intern "VAL_SIGNW" starfort-mode-token-table))
  (set sym "VAL_SIGNW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one WORD value to another")))

  (setq sym (intern "VAL_SIGNR" starfort-mode-token-table))
  (set sym "VAL_SIGNR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one REAL value to another")))

  (setq sym (intern "VAL_SQRTUW" starfort-mode-token-table))
  (set sym "VAL_SQRTUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of UNSIGNED WORD value")))

  (setq sym (intern "FIO_STOP" starfort-mode-token-table))
  (set sym "FIO_STOP( {status} )")
  (setplist sym '((class . token) (desc . "Close down FIO")))

  (setq sym (intern "VAL_SIGNI" starfort-mode-token-table))
  (set sym "VAL_SIGNI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one INTEGER value to another")))

  (setq sym (intern "CLOSE" starfort-mode-token-table))
  (set sym '(("CLOSE_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "PSX_RAND" starfort-mode-token-table))
  (set sym "PSX_RAND( {inum}, {maxnum}, {fnum}, {status} )")
  (setplist sym '((class . token) (desc . "Generate a random number")))

  (setq sym (intern "VAL_SIGND" starfort-mode-token-table))
  (set sym "VAL_SIGND( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one DOUBLE PRECISION value to another")))

  (setq sym (intern "VAL_SIGNB" starfort-mode-token-table))
  (set sym "VAL_SIGNB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one BYTE value to another")))

  (setq sym (intern "VAL_NEGW" starfort-mode-token-table))
  (set sym "VAL_NEGW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of WORD value")))

  (setq sym (intern "VAL_DTOW" starfort-mode-token-table))
  (set sym "VAL_DTOW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION value to WORD")))

  (setq sym (intern "ERR_STAT" starfort-mode-token-table))
  (set sym "ERR_STAT( {status} )")
  (setplist sym '((class . token) (desc . "Inquire the last reported error status") (helpkey . "ERR_STAT")))

  (setq sym (intern "VAL_SQRTUB" starfort-mode-token-table))
  (set sym "VAL_SQRTUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of UNSIGNED BYTE value")))

  (setq sym (intern "VAL_NEGR" starfort-mode-token-table))
  (set sym "VAL_NEGR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of REAL value")))

  (setq sym (intern "FUNCTION_PROLOGUE" starfort-mode-token-table))
  (set sym "\\*+
\\*{function_prologue}
\\*-
\\	{function_declarations}
\\*.")
  (setplist sym '((class . token) (desc . "Function prologue")))

  (setq sym (intern "VAL_DTOR" starfort-mode-token-table))
  (set sym "VAL_DTOR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION value to REAL")))

  (setq sym (intern "CMP__NOMAP" starfort-mode-token-table))
  (set sym "CMP__NOMAP")
  (setplist sym '((class . token) (desc . "Not mapped (error code)")))

  (setq sym (intern "ARY_RESET" starfort-mode-token-table))
  (set sym "ARY_RESET( {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Reset an array to an undefined state")))

  (setq sym (intern "HDS_SHOW" starfort-mode-token-table))
  (set sym "HDS_SHOW( {topic}, {status} )")
  (setplist sym '((class . token) (desc . "Show HDS statistics")))

  (setq sym (intern "GWM_WSETL" starfort-mode-token-table))
  (set sym "GWM_WSETL( {option}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Set a logical window option")))

  (setq sym (intern "VAL_NEGI" starfort-mode-token-table))
  (set sym "VAL_NEGI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of INTEGER value")))

  (setq sym (intern "GWM_WSETI" starfort-mode-token-table))
  (set sym "GWM_WSETI( {option}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Set an integer window option")))

  (setq sym (intern "VAL_DTOI" starfort-mode-token-table))
  (set sym "VAL_DTOI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION value to INTEGER")))

  (setq sym (intern "DAT__ISMAP" starfort-mode-token-table))
  (set sym "DAT__ISMAP")
  (setplist sym '((class . token) (desc . "Data currently mapped (error code)")))

  (setq sym (intern "VEC_MINW" starfort-mode-token-table))
  (set sym "VEC_MINW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two WORD vectorised arrays")))

  (setq sym (intern "VAL_NEGD" starfort-mode-token-table))
  (set sym "VAL_NEGD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of DOUBLE PRECISION value")))

  (setq sym (intern "VAL_DTOD" starfort-mode-token-table))
  (set sym "VAL_DTOD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION value to DOUBLE PRECISION")))

  (setq sym (intern "GWM_WSETC" starfort-mode-token-table))
  (set sym "GWM_WSETC( {option}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Set an character string window option")))

  (setq sym (intern "VEC_MINR" starfort-mode-token-table))
  (set sym "VEC_MINR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two REAL vectorised arrays")))

  (setq sym (intern "VAL_NEGB" starfort-mode-token-table))
  (set sym "VAL_NEGB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of BYTE value")))

  (setq sym (intern "VAL_DTOB" starfort-mode-token-table))
  (set sym "VAL_DTOB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION value to BYTE")))

  (setq sym (intern "VEC_MINI" starfort-mode-token-table))
  (set sym "VEC_MINI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two INTEGER vectorised arrays")))

  (setq sym (intern "VEC_MIND" starfort-mode-token-table))
  (set sym "VEC_MIND( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two DOUBLE PRECISION vectorised arrays")))

  (setq sym (intern "VEC_MINB" starfort-mode-token-table))
  (set sym "VEC_MINB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two BYTE vectorised arrays")))

  (setq sym (intern "IIDERR" starfort-mode-token-table))
  (set sym "IIDERR( {status}, {messag}, {meslen} )")
  (setplist sym '((class . token) (desc . "Get Error")))

  (setq sym (intern "HDS_CLOSE" starfort-mode-token-table))
  (set sym "HDS_CLOSE( {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Close container file")))

  (setq sym (intern "ARY_VERFY" starfort-mode-token-table))
  (set sym "ARY_VERFY( {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Verify that an array's data structure is correctly constructed")))

  (setq sym (intern "TRN__PRCIN" starfort-mode-token-table))
  (set sym "TRN__PRCIN")
  (setplist sym '((class . token) (desc . "Precision invalid (error code)")))

  (setq sym (intern "ACCESS_PAR" starfort-mode-token-table))
  (set sym '(("ACCESS_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VEC_IDVUW" starfort-mode-token-table))
  (set sym "VEC_IDVUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one UNSIGNED WORD vectorised array by another")))

  (setq sym (intern "TRN_TRNR" starfort-mode-token-table))
  (set sym "TRN_TRNR( {bad}, {nd1}, {ncin}, {ndat}, {rdata}, {id}, {nr1}, {ncout}, {rreslt}, {status} )")
  (setplist sym '((class . token) (desc . "Transform general REAL coordinate data")))

  (setq sym (intern "ARY_STYPE" starfort-mode-token-table))
  (set sym "ARY_STYPE( {ftype}, {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Set a new type for an array")))

  (setq sym (intern "EXTERNAL_REFERENCES" starfort-mode-token-table))
  (set sym '(("EXTERNAL_REFERENCES" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "TRN_TRNI" starfort-mode-token-table))
  (set sym "TRN_TRNI( {bad}, {nd1}, {ncin}, {ndat}, {idata}, {id}, {nr1}, {ncout}, {ireslt}, {status} )")
  (setplist sym '((class . token) (desc . "Transform general INTEGER coordinate data")))

  (setq sym (intern "ERR_MARK" starfort-mode-token-table))
  (set sym "ERR_MARK")
  (setplist sym '((class . token) (desc . "Mark (start) a new error context") (helpkey . "ERR_MARK")))

  (setq sym (intern "ARY_FTYPE" starfort-mode-token-table))
  (set sym "ARY_FTYPE( {iary}, {ftype}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the full data type of an array")))

  (setq sym (intern "TRN_TRND" starfort-mode-token-table))
  (set sym "TRN_TRND( {bad}, {nd1}, {ncin}, {ndat}, {ddata}, {id}, {nr1}, {ncout}, {dreslt}, {status} )")
  (setplist sym '((class . token) (desc . "Transform general DOUBLE PRECISION coordinate data")))

  (setq sym (intern "DELETE" starfort-mode-token-table))
  (set sym "'DELETE'")
  (setplist sym '((class . token) (desc . "'DELETE'")))

  (setq sym (intern "VEC_IDVUB" starfort-mode-token-table))
  (set sym "VEC_IDVUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one UNSIGNED BYTE vectorised array by another")))

  (setq sym (intern "NUM_ATNDR" starfort-mode-token-table))
  (set sym "NUM_ATNDR( {num} )")
  (setplist sym '((class . token) (desc . "Inverse tangent function of REAL number (degrees)")))

  (setq sym (intern "IIDENC" starfort-mode-token-table))
  (set sym "IIDENC( {dispid}, {status} )")
  (setplist sym '((class . token) (desc . "Enable configuration")))

  (setq sym (intern "PGLCUR" starfort-mode-token-table))
  (set sym "PGLCUR( {maxpt}, {npt}, {x}, {y} )")
  (setplist sym '((class . token) (desc . "Draw a line using the cursor")))

  (setq sym (intern "NDF_UNMAP" starfort-mode-token-table))
  (set sym "NDF_UNMAP( {indf}, {comp}, {status} )")
  (setplist sym '((class . token) (desc . "Unmap an NDF or a mapped NDF array") (helpkey . "NDF_UNMAP")))

  (setq sym (intern "CHR_APPND" starfort-mode-token-table))
  (set sym "CHR_APPND( {str1}, {str2}, {len2} )")
  (setplist sym '((class . token) (desc . "Copy one string into another (ignoring trailing blanks)")))

  (setq sym (intern "EXECUTABLE_STATEMENT" starfort-mode-token-table))
  (set sym "[executable_statement]...")
  (setplist sym '((class . token) (desc . "Placeholder for executable statements")))

  (setq sym (intern "NUM_ATNDD" starfort-mode-token-table))
  (set sym "NUM_ATNDD( {num} )")
  (setplist sym '((class . token) (desc . "Inverse tangent function of DOUBLE PRECISION number (degrees)")))

  (setq sym (intern "DO_ENDDO" starfort-mode-token-table))
  (set sym "DO {do_var} = {arith_exp}, {arith_exp}, [do_increment]
{executable_statement}...
END DO	")
  (setplist sym '((class . token) (desc . "Indexed DO loop")))

  (setq sym (intern "GSVLM" starfort-mode-token-table))
  (set sym "GSVLM( {wkid}, {idnr}, {gks$mode}, {gks$esw} )")
  (setplist sym '((class . token) (desc . "Set valuator mode")))

  (setq sym (intern "ARY_IMPRT" starfort-mode-token-table))
  (set sym "ARY_IMPRT( {loc}, {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Import an array into the ARY_ system from HDS")))

  (setq sym (intern "DAT_WHERE" starfort-mode-token-table))
  (set sym "DAT_WHERE( {loc}, {block}, {offset}, {status} )")
  (setplist sym '((class . token) (desc . "Find position of primitive in HDS file")))

  (setq sym (intern "ASSIGNMENT_LOG" starfort-mode-token-table))
  (set sym "{logical_assign_elm} = {logical_exp}")
  (setplist sym '((class . token) (desc . "Logical ASSIGNMENT statement")))

  (setq sym (intern "DAT_THERE" starfort-mode-token-table))
  (set sym "DAT_THERE( {loc}, {name}, {reply}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire component existence")))

  (setq sym (intern "PSX_ASCTIME" starfort-mode-token-table))
  (set sym "PSX_ASCTIME( {tstrct}, {string}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a time structure to a character string")))

  (setq sym (intern "SLA_DR2AF" starfort-mode-token-table))
  (set sym "SLA_DR2AF( {ndp}, {angle}, {sign}, {idmsf} )")
  (setplist sym '((class . token) (desc . "Convert an angle in radians to degrees, arcminutes, arcseconds")))

  (setq sym (intern "SLA_CR2AF" starfort-mode-token-table))
  (set sym "SLA_CR2AF( {ndp}, {angle}, {sign}, {idmsf} )")
  (setplist sym '((class . token) (desc . "Convert an angle in radians into degrees, arcminutes, arcseconds")))

  (setq sym (intern "BLOCK" starfort-mode-token-table))
  (set sym '(("BLOCKDATA_PROGRAM_MODULE" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "GSVIS" starfort-mode-token-table))
  (set sym "GSVIS( {sgna}, {gks$vis} )")
  (setplist sym '((class . token) (desc . "Set visibility")))

  (setq sym (intern "GSTXR" starfort-mode-token-table))
  (set sym "GSTXR( {wkid}, {txi}, {font}, {gks$prec}, {chxp}, {chsp}, {coli} )")
  (setplist sym '((class . token) (desc . "Set text representation")))

  (setq sym (intern "GSTXP" starfort-mode-token-table))
  (set sym "GSTXP( {gks$txp} )")
  (setplist sym '((class . token) (desc . "Set text path")))

  (setq sym (intern "PSX_UNAME" starfort-mode-token-table))
  (set sym "PSX_UNAME( {sysname}, {nodename}, {release}, {version}, {machine}, {status} )")
  (setplist sym '((class . token) (desc . "Gets information about the host computer system")))

  (setq sym (intern "MSG_IFSET" starfort-mode-token-table))
  (set sym "MSG_IFSET( {filter}, {status} )")
  (setplist sym '((class . token) (desc . "Set the filter level for conditional message output") (helpkey . "MSG_IFSET")))

  (setq sym (intern "CHR_UPPER" starfort-mode-token-table))
  (set sym "CHR_UPPER( {char} )")
  (setplist sym '((class . token) (desc . "Give upper case equivalent of a character")))

  (setq sym (intern "GSTXI" starfort-mode-token-table))
  (set sym "GSTXI( {txi} )")
  (setplist sym '((class . token) (desc . "Set text index")))

  (setq sym (intern "PGLDEV" starfort-mode-token-table))
  (set sym "PGLDEV")
  (setplist sym '((class . token) (desc . "List available device types")))

  (setq sym (intern "ARY_CMPLX" starfort-mode-token-table))
  (set sym "ARY_CMPLX( {iary}, {cmplx}, {status} )")
  (setplist sym '((class . token) (desc . "Determine whether an array holds complex values")))

  (setq sym (intern "ERR_ANNUL" starfort-mode-token-table))
  (set sym "ERR_ANNUL( {status} )")
  (setplist sym '((class . token) (desc . "Annul the contents of the current error context") (helpkey . "ERR_ANNUL")))

  (setq sym (intern "VAL_UWTOW" starfort-mode-token-table))
  (set sym "VAL_UWTOW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD value to WORD")))

  (setq sym (intern "VAL_UWTOR" starfort-mode-token-table))
  (set sym "VAL_UWTOR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD value to REAL")))

  (setq sym (intern "IIRSRV" starfort-mode-token-table))
  (set sym "IIRSRV( {dispid}, {roiid}, {lvis}, {status} )")
  (setplist sym '((class . token) (desc . "Set Visibility Rectangular Region of Interest")))

  (setq sym (intern "ARGUMENTS_GIVEN" starfort-mode-token-table))
  (set sym '(("ARGUMENTS_GIVEN" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "IIZRZP" starfort-mode-token-table))
  (set sym "IIZRZP( {dispid}, {xoff}, {yoff}, {zoomf}, {status} )")
  (setplist sym '((class . token) (desc . "Read Display Zoom and Pan")))

  (setq sym (intern "VAL_UWTOI" starfort-mode-token-table))
  (set sym "VAL_UWTOI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD value to INTEGER")))

  (setq sym (intern "ARY_NOACC" starfort-mode-token-table))
  (set sym "ARY_NOACC( {access}, {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Disable a specified type of access to an array")))

  (setq sym (intern "TRN__EXPUD" starfort-mode-token-table))
  (set sym "TRN__EXPUD")
  (setplist sym '((class . token) (desc . "Expression undefined (error code)")))

  (setq sym (intern "VAL_UWTOD" starfort-mode-token-table))
  (set sym "VAL_UWTOD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD value to DOUBLE PRECISION")))

  (setq sym (intern "VAL_UWTOB" starfort-mode-token-table))
  (set sym "VAL_UWTOB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD value to BYTE")))

  (setq sym (intern "GO_COMPUTED" starfort-mode-token-table))
  (set sym "GO TO ( {lbl}... ), {arith_exp}")
  (setplist sym '((class . token) (desc . "GO TO ( {lbl}... ), {arith_exp}")))

  (setq sym (intern "GECLKS" starfort-mode-token-table))
  (set sym "GECLKS")
  (setplist sym '((class . token) (desc . "Emergency close GKS")))

  (setq sym (intern "VEC_MULUW" starfort-mode-token-table))
  (set sym "VEC_MULUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two UNSIGNED WORD vectorised arrays")))

  (setq sym (intern "NUM_SUBUW" starfort-mode-token-table))
  (set sym "NUM_SUBUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Subtract one UNSIGNED WORD number from another")))

  (setq sym (intern "SLA_RVEROT" starfort-mode-token-table))
  (set sym "SLA_RVEROT( {phi}, {ra}, {da}, {st} )")
  (setplist sym '((class . token) (desc . "Velocity component in a given direction due to Earth rotation")))

  (setq sym (intern "AGI_SROOT" starfort-mode-token-table))
  (set sym "AGI_SROOT( {status} )")
  (setplist sym '((class . token) (desc . "Select the root picture for searching")))

  (setq sym (intern "GSCLIP" starfort-mode-token-table))
  (set sym "GSCLIP( {gks$clsw} )")
  (setplist sym '((class . token) (desc . "Set clipping indicator")))

  (setq sym (intern "GQCLIP" starfort-mode-token-table))
  (set sym "GQCLIP( {errind}, {clip} )")
  (setplist sym '((class . token) (desc . "Inquire clipping indicator")))

  (setq sym (intern "NDF_RESET" starfort-mode-token-table))
  (set sym "NDF_RESET( {indf}, {comp}, {status} )")
  (setplist sym '((class . token) (desc . "Reset an NDF component to an undefined state") (helpkey . "NDF_RESET")))

  (setq sym (intern "AGI_RCSP" starfort-mode-token-table))
  (set sym "AGI_RCSP( {pname}, {pstart}, {x}, {y}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Recall succeeding picture embracing a position")))

  (setq sym (intern "PGTBOX" starfort-mode-token-table))
  (set sym "PGTBOX( {xopt}, {xtickd}, {nxsubd}, {yopt}, {ytickd}, {nysubd} )")
  (setplist sym '((class . token) (desc . "Draw a box and optionally write HH MM SS style numeric labelling")))

  (setq sym (intern "CTYP" starfort-mode-token-table))
  (set sym "CHARACTER * ( DAT__SZTYP )")
  (setplist sym '((class . token) (alias . t)))

  (setq sym (intern "VEC_LOGW" starfort-mode-token-table))
  (set sym "VEC_LOGW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of WORD vectorised array")))

  (setq sym (intern "VAL_MINW" starfort-mode-token-table))
  (set sym "VAL_MINW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two WORD values")))

  (setq sym (intern "VEC_MULUB" starfort-mode-token-table))
  (set sym "VEC_MULUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two UNSIGNED BYTE vectorised arrays")))

  (setq sym (intern "NUM_SUBUB" starfort-mode-token-table))
  (set sym "NUM_SUBUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Subtract one UNSIGNED BYTE number from another")))

  (setq sym (intern "VEC_LOGR" starfort-mode-token-table))
  (set sym "VEC_LOGR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of REAL vectorised array")))

  (setq sym (intern "VAL_MINR" starfort-mode-token-table))
  (set sym "VAL_MINR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two REAL values")))

  (setq sym (intern "CHR_LOWER" starfort-mode-token-table))
  (set sym "CHR_LOWER( {char} )")
  (setplist sym '((class . token) (desc . "Give lower case equivalent of a character")))

  (setq sym (intern "SEQUENTIAL_PAR" starfort-mode-token-table))
  (set sym '(("SEQUENTIAL_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SLA_UNPCD" starfort-mode-token-table))
  (set sym "SLA_UNPCD( {disco}, {x}, {y} )")
  (setplist sym '((class . token) (desc . "Remove pincushion/barrel distortion from a distorted [x,y] to give tangent-plane [x,y].")))

  (setq sym (intern "MENU" starfort-mode-token-table))
  (set sym '(("MENU" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "IIZRSZ" starfort-mode-token-table))
  (set sym "IIZRSZ( {dispid}, {memid}, {xoff}, {yoff}, {zoomf}, {status} )")
  (setplist sym '((class . token) (desc . "Read Memory Scroll and Zoom")))

  (setq sym (intern "VEC_LOGI" starfort-mode-token-table))
  (set sym "VEC_LOGI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of INTEGER vectorised array")))

  (setq sym (intern "VAL_MINI" starfort-mode-token-table))
  (set sym "VAL_MINI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two INTEGER values")))

  (setq sym (intern "AGI_RCPP" starfort-mode-token-table))
  (set sym "AGI_RCPP( {pname}, {pstart}, {x}, {y}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Recall preceding picture embracing a position")))

  (setq sym (intern "VEC_LOGD" starfort-mode-token-table))
  (set sym "VEC_LOGD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of DOUBLE PRECISION vectorised array")))

  (setq sym (intern "VAL_MIND" starfort-mode-token-table))
  (set sym "VAL_MIND( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two DOUBLE PRECISION values")))

  (setq sym (intern "IIDCLO" starfort-mode-token-table))
  (set sym "IIDCLO( {dispid}, {status} )")
  (setplist sym '((class . token) (desc . "Close Display")))

  (setq sym (intern "VEC_LOGB" starfort-mode-token-table))
  (set sym "VEC_LOGB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of BYTE vectorised array")))

  (setq sym (intern "VAL_MINB" starfort-mode-token-table))
  (set sym "VAL_MINB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two BYTE values")))

  (setq sym (intern "SLA_VXV" starfort-mode-token-table))
  (set sym "SLA_VXV( {va}, {vb}, {vc} )")
  (setplist sym '((class . token) (desc . "Vector product of two 3-vectors (single precision)")))

  (setq sym (intern "SGS_CANCL" starfort-mode-token-table))
  (set sym "SGS_CANCL( {param}, {status} )")
  (setplist sym '((class . token) (desc . "Close graphics workstation and cancel parameter")))

  (setq sym (intern "NUM_ATN2R" starfort-mode-token-table))
  (set sym "NUM_ATN2R( {num} )")
  (setplist sym '((class . token) (desc . "Fortran ATAN2 function of REAL number (radians)")))

  (setq sym (intern "GSSTM" starfort-mode-token-table))
  (set sym "GSSTM( {wkid}, {idnr}, {gks$mode}, {gks$esw} )")
  (setplist sym '((class . token) (desc . "Set string mode")))

  (setq sym (intern "SLA_EPB2D" starfort-mode-token-table))
  (set sym "SLA_EPB2D( {epb} )")
  (setplist sym '((class . token) (desc . "Conversion of Besselian Epoch to Modified Julian Date")))

  (setq sym (intern "NDF_MTYPN" starfort-mode-token-table))
  (set sym "NDF_MTYPN( {typlst}, {n}, {ndfs}, {comp}, {itype}, {dtype}, {status} )")
  (setplist sym '((class . token) (desc . "Match the types of the array components of a number of NDFs") (helpkey . "NDF_MTYPN")))

  (setq sym (intern "IIRRRI" starfort-mode-token-table))
  (set sym "IIRRRI( {dispid}, {inmid}, {roiid}, {xmin}, {ymin}, {xmax}, {ymax}, {outmid}, {status} )")
  (setplist sym '((class . token) (desc . "Read Rectangular Region of Interest")))

  (setq sym (intern "NDF_STYPE" starfort-mode-token-table))
  (set sym "NDF_STYPE( {ftype}, {indf}, {comp}, {status} )")
  (setplist sym '((class . token) (desc . "Set a new type for an NDF array component") (helpkey . "NDF_STYPE")))

  (setq sym (intern "VAL_PWRUW" starfort-mode-token-table))
  (set sym "VAL_PWRUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (UNSIGNED WORD values)")))

  (setq sym (intern "NUM_ATN2D" starfort-mode-token-table))
  (set sym "NUM_ATN2D( {num} )")
  (setplist sym '((class . token) (desc . "Fortran ATAN2 function of DOUBLE PRECISION number (radians)")))

  (setq sym (intern "NDF_MTYPE" starfort-mode-token-table))
  (set sym "NDF_MTYPE( {typlst}, {indf1}, {indf2}, {comp}, {itype}, {dtype}, {status} )")
  (setplist sym '((class . token) (desc . "Match the types of the array components of a pair of NDFs") (helpkey . "NDF_MTYPE")))

  (setq sym (intern "NUM_ITOW" starfort-mode-token-table))
  (set sym "NUM_ITOW( {num} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER number to WORD")))

  (setq sym (intern "AGI_RCLP" starfort-mode-token-table))
  (set sym "AGI_RCLP( {pname}, {x}, {y}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Recall last picture embracing a position")))

  (setq sym (intern "NUM_ITOR" starfort-mode-token-table))
  (set sym "NUM_ITOR( {num} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER number to REAL")))

  (setq sym (intern "NDF_FTYPE" starfort-mode-token-table))
  (set sym "NDF_FTYPE( {indf}, {comp}, {ftype}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the full data type of an NDF array component") (helpkey . "NDF_FTYPE")))

  (setq sym (intern "NDF_ATYPE" starfort-mode-token-table))
  (set sym "NDF_ATYPE( {indf}, {comp}, {iaxis}, {type}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the numeric type of an NDF axis array") (helpkey . "NDF_ATYPE")))

  (setq sym (intern "SGS_SAMCU" starfort-mode-token-table))
  (set sym "SGS_SAMCU( {x}, {y} )")
  (setplist sym '((class . token) (desc . "Sample cursor")))

  (setq sym (intern "VAL_PWRUB" starfort-mode-token-table))
  (set sym "VAL_PWRUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (UNSIGNED BYTE values)")))

  (setq sym (intern "NUM_ITOI" starfort-mode-token-table))
  (set sym "NUM_ITOI( {num} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER number to INTEGER")))

  (setq sym (intern "NUM_ITOD" starfort-mode-token-table))
  (set sym "NUM_ITOD( {num} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER number to DOUBLE PRECISION")))

  (setq sym (intern "NUM_ITOB" starfort-mode-token-table))
  (set sym "NUM_ITOB( {num} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER number to BYTE")))

  (setq sym (intern "SLA_PVOBS" starfort-mode-token-table))
  (set sym "SLA_PVOBS( {p}, {h}, {stl}, {pv} )")
  (setplist sym '((class . token) (desc . "Position and velocity of an observing station")))

  (setq sym (intern "SLA_GRESID" starfort-mode-token-table))
  (set sym "SLA_GRESID( {s} )")
  (setplist sym '((class . token) (desc . "Generate pseudo-random normal deviate ( = 'Gaussian residual')")))

  (setq sym (intern "ERR_LEVEL" starfort-mode-token-table))
  (set sym "ERR_LEVEL( {level} )")
  (setplist sym '((class . token) (desc . "Inquire the current error context level") (helpkey . "ERR_LEVEL")))

  (setq sym (intern "DAT__GRPIN" starfort-mode-token-table))
  (set sym "DAT__GRPIN")
  (setplist sym '((class . token) (desc . "Group invalid (error code)")))

  (setq sym (intern "NUM_ACSDR" starfort-mode-token-table))
  (set sym "NUM_ACSDR( {num} )")
  (setplist sym '((class . token) (desc . "Inverse cosine function of REAL number (degrees)")))

  (setq sym (intern "TRN_TR2R" starfort-mode-token-table))
  (set sym "TRN_TR2R( {bad}, {nxy}, {rxin}, {ryin}, {id}, {rxout}, {ryout}, {status} )")
  (setplist sym '((class . token) (desc . "Transform 2-dimensional REAL coordinate data")))

  (setq sym (intern "NDF_IMPRT" starfort-mode-token-table))
  (set sym "NDF_IMPRT( {loc}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Import an NDF into the NDF_ system from HDS") (helpkey . "NDF_IMPRT")))

  (setq sym (intern "PASSED_FUNCTION" starfort-mode-token-table))
  (set sym "FUNCTION")
  (setplist sym '((class . token) (desc . "Function passed as an argument")))

  (setq sym (intern "SLA_RVGALC" starfort-mode-token-table))
  (set sym "SLA_RVGALC( {r2000}, {d2000} )")
  (setplist sym '((class . token) (desc . "Velocity component in a given direction due to galactic rotation")))

  (setq sym (intern "AGI_ILAB" starfort-mode-token-table))
  (set sym "AGI_ILAB( {picid}, {label}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire label of a picture")))

  (setq sym (intern "DAT_UPDAT" starfort-mode-token-table))
  (set sym "DAT_UPDAT( {param}, {status} )")
  (setplist sym '((class . token) (desc . "Force update of an HDS file via the ADAM parameter system")))

  (setq sym (intern "TRN_TR1R" starfort-mode-token-table))
  (set sym "TRN_TR1R( {bad}, {nx}, {rxin}, {id}, {rxout}, {status} )")
  (setplist sym '((class . token) (desc . "Transform 1-dimensional REAL coordinate data")))

  (setq sym (intern "NUM_ACSDD" starfort-mode-token-table))
  (set sym "NUM_ACSDD( {num} )")
  (setplist sym '((class . token) (desc . "Inverse cosine function of DOUBLE PRECISION number (degrees)")))

  (setq sym (intern "TRN_TR2I" starfort-mode-token-table))
  (set sym "TRN_TR2I( {bad}, {nxy}, {ixin}, {iyin}, {id}, {ixout}, {iyout}, {status} )")
  (setplist sym '((class . token) (desc . "Transform 2-dimensional INTEGER coordinate data")))

  (setq sym (intern "MONOLITH" starfort-mode-token-table))
  (set sym '(("MONOLITH_PROGRAM_MODULE" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "AGI_RCFP" starfort-mode-token-table))
  (set sym "AGI_RCFP( {pname}, {x}, {y}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Recall first picture embracing a position")))

  (setq sym (intern "TRN_TR2D" starfort-mode-token-table))
  (set sym "TRN_TR2D( {bad}, {nxy}, {dxin}, {dyin}, {id}, {dxout}, {dyout}, {status} )")
  (setplist sym '((class . token) (desc . "Transform 2-dimensional DOUBLE PRECISION coordinate data")))

  (setq sym (intern "TRN_TR1I" starfort-mode-token-table))
  (set sym "TRN_TR1I( {bad}, {nx}, {ixin}, {id}, {ixout}, {status} )")
  (setplist sym '((class . token) (desc . "Transform 1-dimensional INTEGER coordinate data")))

  (setq sym (intern "GSSKM" starfort-mode-token-table))
  (set sym "GSSKM( {wkid}, {idnr}, {gks$mode}, {gks$esw} )")
  (setplist sym '((class . token) (desc . "Set stroke mode")))

  (setq sym (intern "TRN_TR1D" starfort-mode-token-table))
  (set sym "TRN_TR1D( {bad}, {nx}, {dxin}, {id}, {dxout}, {status} )")
  (setplist sym '((class . token) (desc . "Transform 1-dimensional DOUBLE PRECISION coordinate data")))

  (setq sym (intern "GCRSG" starfort-mode-token-table))
  (set sym "GCRSG( {sgna} )")
  (setplist sym '((class . token) (desc . "Create segment")))

  (setq sym (intern "EMS_STAT" starfort-mode-token-table))
  (set sym "EMS_STAT( {status} )")
  (setplist sym '((class . token) (desc . "Inquire the last reported error status") (helpkey . "EMS_STAT")))

  (setq sym (intern "GSCHXP" starfort-mode-token-table))
  (set sym "GSCHXP( {chxp} )")
  (setplist sym '((class . token) (desc . "Set character expansion factor")))

  (setq sym (intern "RIO_OPEN" starfort-mode-token-table))
  (set sym "RIO_OPEN( {file}, {acmode}, {form}, {recsz}, {fd}, {status} )")
  (setplist sym '((class . token) (desc . "Open a direct access file")))

  (setq sym (intern "GQCHXP" starfort-mode-token-table))
  (set sym "GQCHXP( {errind}, {chxp} )")
  (setplist sym '((class . token) (desc . "Inquire character expansion factor")))

  (setq sym (intern "MAG_MOVE" starfort-mode-token-table))
  (set sym "MAG_MOVE( {td}, {file}, {start}, {block}, {status} )")
  (setplist sym '((class . token) (desc . "Move to a specified file and block on a tape")))

  (setq sym (intern "GSSGT" starfort-mode-token-table))
  (set sym "GSSGT( {sgna}, {m} )")
  (setplist sym '((class . token) (desc . "Set segment transformation")))

  (setq sym (intern "SLA_RVLSR" starfort-mode-token-table))
  (set sym "SLA_RVLSR( {r2000}, {d2000} )")
  (setplist sym '((class . token) (desc . "Velocity component in a given direction due to the Sun's motion with respect to the Local Standard of Rest")))

  (setq sym (intern "IIDAMY" starfort-mode-token-table))
  (set sym "IIDAMY( {dispid}, {xsize}, {ysize}, {memdep}, {memtyp}, {memid}, {status} )")
  (setplist sym '((class . token) (desc . "Allocate Memory")))

  (setq sym (intern "GSSGP" starfort-mode-token-table))
  (set sym "GSSGP( {sgna}, {prior} )")
  (setplist sym '((class . token) (desc . "Set segment priority")))

  (setq sym (intern "_CLEANUP_CODE" starfort-mode-token-table))
  (set sym "
\\*	Begin a new error reporting context.
\\	CALL ERR_BEGIN( STATUS )

\\*	[comment]
\\	{executable_statement}...

\\*	End the error reporting context.
\\	CALL ERR_END( STATUS )")
  (setplist sym '((class . token) (desc . "Cleaning up code; a new error reporting context")))

  (setq sym (intern "A_TASK" starfort-mode-token-table))
  (set sym '(("A_TASK_PROGRAM_MODULE" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NDF_CMPLX" starfort-mode-token-table))
  (set sym "NDF_CMPLX( {indf}, {comp}, {cmplx}, {status} )")
  (setplist sym '((class . token) (desc . "Determine whether an NDF array component holds complex values") (helpkey . "NDF_CMPLX")))

  (setq sym (intern "IICINC" starfort-mode-token-table))
  (set sym "IICINC( {dispid}, {memid}, {numcur}, {shape}, {color}, {xc}, {yc}, {status} )")
  (setplist sym '((class . token) (desc . "Initialize Cursor")))

  (setq sym (intern "GSCHUP" starfort-mode-token-table))
  (set sym "GSCHUP( {chux}, {chuy} )")
  (setplist sym '((class . token) (desc . "Set character up vector")))

  (setq sym (intern "GQCHUP" starfort-mode-token-table))
  (set sym "GQCHUP( {errind}, {chux}, {chuy} )")
  (setplist sym '((class . token) (desc . "Inquire character up vector")))

  (setq sym (intern "PGSHLS" starfort-mode-token-table))
  (set sym "PGSHLS( {ci}, {ch}, {cl}, {cs} )")
  (setplist sym '((class . token) (desc . "Set color representation using HLS system")))

  (setq sym (intern "DAT_GETR" starfort-mode-token-table))
  (set sym "DAT_GETR( {loc}, {ndim}, {dim}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive as REAL")))

  (setq sym (intern "NDF_NOACC" starfort-mode-token-table))
  (set sym "NDF_NOACC( {access}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Disable a specified type of access to an NDF") (helpkey . "NDF_NOACC")))

  (setq sym (intern "DAT_GETL" starfort-mode-token-table))
  (set sym "DAT_GETL( {loc}, {ndim}, {dim}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive as LOGICAL")))

  (setq sym (intern "IMPLICIT" starfort-mode-token-table))
  (set sym '(("IMPLICIT_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "GSCHSP" starfort-mode-token-table))
  (set sym "GSCHSP( {chsp} )")
  (setplist sym '((class . token) (desc . "Set character spacing")))

  (setq sym (intern "DAT_GETI" starfort-mode-token-table))
  (set sym "DAT_GETI( {loc}, {ndim}, {dim}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive as INTEGER")))

  (setq sym (intern "GQCHSP" starfort-mode-token-table))
  (set sym "GQCHSP( {errind}, {chsp} )")
  (setplist sym '((class . token) (desc . "Inquire character spacing")))

  (setq sym (intern "SLA_GEOC" starfort-mode-token-table))
  (set sym "SLA_GEOC( {p}, {h}, {r}, {z} )")
  (setplist sym '((class . token) (desc . "Convert geodetic position to geocentric")))

  (setq sym (intern "DAT_GETD" starfort-mode-token-table))
  (set sym "DAT_GETD( {loc}, {ndim}, {dim}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive as DOUBLE PRECISION")))

  (setq sym (intern "VEC_SUBW" starfort-mode-token-table))
  (set sym "VEC_SUBW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one WORD vectorised array from another")))

  (setq sym (intern "DAT_GETC" starfort-mode-token-table))
  (set sym "DAT_GETC( {loc}, {ndim}, {dim}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive as CHARACTER")))

  (setq sym (intern "VEC_SUBR" starfort-mode-token-table))
  (set sym "VEC_SUBR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one REAL vectorised array from another")))

  (setq sym (intern "SGS_TXR" starfort-mode-token-table))
  (set sym "SGS_TXR( {x}, {y}, {r}, {nfi}, {ndp} )")
  (setplist sym '((class . token) (desc . "Begin new text with a real")))

  (setq sym (intern "VEC_SUBI" starfort-mode-token-table))
  (set sym "VEC_SUBI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one INTEGER vectorised array from another")))

  (setq sym (intern "DAT_SHAPE" starfort-mode-token-table))
  (set sym "DAT_SHAPE( {loc}, {ndimx}, {dim}, {ndim}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire object shape")))

  (setq sym (intern "EMS_MARK" starfort-mode-token-table))
  (set sym "EMS_MARK")
  (setplist sym '((class . token) (desc . "Start a new error context") (helpkey . "EMS_MARK")))

  (setq sym (intern "SLA_VDV" starfort-mode-token-table))
  (set sym "SLA_VDV( {va}, {vb} )")
  (setplist sym '((class . token) (desc . "Scalar product of two 3-vectors (single precision)")))

  (setq sym (intern "VAL_LOGW" starfort-mode-token-table))
  (set sym "VAL_LOGW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of WORD value")))

  (setq sym (intern "SGS_TXI" starfort-mode-token-table))
  (set sym "SGS_TXI( {x}, {y}, {i}, {nfi} )")
  (setplist sym '((class . token) (desc . "Begin text with an integer")))

  (setq sym (intern "VEC_SUBD" starfort-mode-token-table))
  (set sym "VEC_SUBD( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one DOUBLE PRECISION vectorised array from another")))

  (setq sym (intern "VEC_SUBB" starfort-mode-token-table))
  (set sym "VEC_SUBB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one BYTE vectorised array from another")))

  (setq sym (intern "VAL_LOGR" starfort-mode-token-table))
  (set sym "VAL_LOGR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of REAL value")))

  (setq sym (intern "GRP_SETSZ" starfort-mode-token-table))
  (set sym "GRP_SETSZ( {igrp}, {size}, {status} )")
  (setplist sym '((class . token) (desc . "Reduce the size of a group")))

  (setq sym (intern "GRSGWK" starfort-mode-token-table))
  (set sym "GRSGWK( {wkid} )")
  (setplist sym '((class . token) (desc . "Redraw all segments on workstation")))

  (setq sym (intern "FIO_TEST" starfort-mode-token-table))
  (set sym "FIO_TEST( {errcls}, {status} )")
  (setplist sym '((class . token) (desc . "Test if an FIO status value belongs to a certain class of errors")))

  (setq sym (intern "TRN_ANNUL" starfort-mode-token-table))
  (set sym "TRN_ANNUL( {id}, {status} )")
  (setplist sym '((class . token) (desc . "Annul compiled mapping")))

  (setq sym (intern "GQSGWK" starfort-mode-token-table))
  (set sym "GQSGWK( {wkid}, {n}, {errind}, {ol}, {segnam} )")
  (setplist sym '((class . token) (desc . "Inquire set member of segment names on workstation")))

  (setq sym (intern "VAL_LOGI" starfort-mode-token-table))
  (set sym "VAL_LOGI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of INTEGER value")))

  (setq sym (intern "SLA_XY2XY" starfort-mode-token-table))
  (set sym "SLA_XY2XY( {x1}, {y1}, {coeffs}, {x2}, {y2} )")
  (setplist sym '((class . token) (desc . "Transform one [X,Y] into another using a linear model of the type produced by the SLA_FITXY routine.")))

  (setq sym (intern "VAL_LOGD" starfort-mode-token-table))
  (set sym "VAL_LOGD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of DOUBLE PRECISION value")))

  (setq sym (intern "VAL_LOGB" starfort-mode-token-table))
  (set sym "VAL_LOGB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of BYTE value")))

  (setq sym (intern "GQSGUS" starfort-mode-token-table))
  (set sym "GQSGUS( {n}, {errind}, {ol}, {segnam} )")
  (setplist sym '((class . token) (desc . "Inquire set member of segment names in use")))

  (setq sym (intern "TRN__NOID" starfort-mode-token-table))
  (set sym "TRN__NOID")
  (setplist sym '((class . token) (desc . "Null identifier value (symbolic constant)")))

  (setq sym (intern "SLA_PRENUT" starfort-mode-token-table))
  (set sym "SLA_PRENUT( {epoch}, {date}, {rmatpn} )")
  (setplist sym '((class . token) (desc . "Form the matrix of precession and nutation (IAU1976/FK5)")))

  (setq sym (intern "GDSGWK" starfort-mode-token-table))
  (set sym "GDSGWK( {wkid}, {sgna} )")
  (setplist sym '((class . token) (desc . "Delete segment from workstation")))

  (setq sym (intern "NUM_ACOSR" starfort-mode-token-table))
  (set sym "NUM_ACOSR( {num} )")
  (setplist sym '((class . token) (desc . "Inverse cosine function of REAL number (radians)")))

  (setq sym (intern "DAT__SZMOD" starfort-mode-token-table))
  (set sym "DAT__SZMOD")
  (setplist sym '((class . token) (desc . "Size of HDS access mode string (symbolic constant)")))

  (setq sym (intern "PGVPORT" starfort-mode-token-table))
  (set sym "PGVPORT( {xleft}, {xright}, {ybot}, {ytop} )")
  (setplist sym '((class . token) (desc . "Set viewport (normalized device coordinates)")))

  (setq sym (intern "CMP_TYPE" starfort-mode-token-table))
  (set sym "CMP_TYPE( {loc}, {name}, {type}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire component type")))

  (setq sym (intern "NUM_ACOSD" starfort-mode-token-table))
  (set sym "NUM_ACOSD( {num} )")
  (setplist sym '((class . token) (desc . "Inverse cosine function of DOUBLE PRECISION number (radians)")))

  (setq sym (intern "SGS_TPZ" starfort-mode-token-table))
  (set sym "SGS_TPZ( {izin}, {xin}, {yin}, {izout}, {xout}, {yout}, {status} )")
  (setplist sym '((class . token) (desc . "Transform position to new zone")))

  (setq sym (intern "BYTE" starfort-mode-token-table))
  (set sym "BYTE")
  (setplist sym '((class . token) (desc . "BYTE data type")))

  (setq sym (intern "MAG_MOUNT" starfort-mode-token-table))
  (set sym "MAG_MOUNT( {param}, {mode}, {status} )")
  (setplist sym '((class . token) (desc . "Mount a tape on a drive")))

  (setq sym (intern "TRN_STOK" starfort-mode-token-table))
  (set sym "TRN_STOK( {token}, {tvalue}, {text}, {nsubs}, {status} )")
  (setplist sym '((class . token) (desc . "Substitute text for a token")))

  (setq sym (intern "PGQWIN" starfort-mode-token-table))
  (set sym "PGQWIN( {x1}, {x2}, {y1}, {y2} )")
  (setplist sym '((class . token) (desc . "Inquire window boundary coordinates")))

  (setq sym (intern "VEC_MAXW" starfort-mode-token-table))
  (set sym "VEC_MAXW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two WORD vectorised arrays")))

  (setq sym (intern "SLA_SVD" starfort-mode-token-table))
  (set sym "SLA_SVD( {m}, {n}, {mp}, {np}, {a}, {w}, {v}, {work}, {jstat} )")
  (setplist sym '((class . token) (desc . "Singular value decomposition (double precision)")))

  (setq sym (intern "VEC_MAXR" starfort-mode-token-table))
  (set sym "VEC_MAXR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two REAL vectorised arrays")))

  (setq sym (intern "SGS_SPREC" starfort-mode-token-table))
  (set sym "SGS_SPREC( {npr} )")
  (setplist sym '((class . token) (desc . "Set precision of text")))

  (setq sym (intern "DAT__SZNAM" starfort-mode-token-table))
  (set sym "DAT__SZNAM")
  (setplist sym '((class . token) (desc . "Size of HDS name string (symbolic constant)")))

  (setq sym (intern "VEC_MAXI" starfort-mode-token-table))
  (set sym "VEC_MAXI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two INTEGER vectorised arrays")))

  (setq sym (intern "NDF_AUNMP" starfort-mode-token-table))
  (set sym "NDF_AUNMP( {indf}, {comp}, {iaxis}, {status} )")
  (setplist sym '((class . token) (desc . "Unmap an NDF axis array") (helpkey . "NDF_AUNMP")))

  (setq sym (intern "VEC_DIVW" starfort-mode-token-table))
  (set sym "VEC_DIVW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one WORD vectorised array by another")))

  (setq sym (intern "VEC_MAXD" starfort-mode-token-table))
  (set sym "VEC_MAXD( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two DOUBLE PRECISION vectorised arrays")))

  (setq sym (intern "VEC_MAXB" starfort-mode-token-table))
  (set sym "VEC_MAXB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two BYTE vectorised arrays")))

  (setq sym (intern "VEC_DIVR" starfort-mode-token-table))
  (set sym "VEC_DIVR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one REAL vectorised array by another")))

  (setq sym (intern "ARY_DELET" starfort-mode-token-table))
  (set sym "ARY_DELET( {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Delete an array")))

  (setq sym (intern "GSPMR" starfort-mode-token-table))
  (set sym "GSPMR( {wkid}, {pmi}, {gks$mtype}, {mszsf}, {coli} )")
  (setplist sym '((class . token) (desc . "Set polymarker representation")))

  (setq sym (intern "VEC_DIVI" starfort-mode-token-table))
  (set sym "VEC_DIVI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one INTEGER vectorised array by another")))

  (setq sym (intern "GSPLR" starfort-mode-token-table))
  (set sym "GSPLR( {wkid}, {pli}, {gks$ltype}, {lwidth}, {coli} )")
  (setplist sym '((class . token) (desc . "Set polyline representation")))

  (setq sym (intern "DAT__SZLOC" starfort-mode-token-table))
  (set sym "DAT__SZLOC")
  (setplist sym '((class . token) (desc . "Size of HDS locator (symbolic constant)")))

  (setq sym (intern "GSPMI" starfort-mode-token-table))
  (set sym "GSPMI( {pmi} )")
  (setplist sym '((class . token) (desc . "Set polymarker index")))

  (setq sym (intern "VEC_DIVD" starfort-mode-token-table))
  (set sym "VEC_DIVD( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one DOUBLE PRECISION vectorised array by another")))

  (setq sym (intern "VEC_DIVB" starfort-mode-token-table))
  (set sym "VEC_DIVB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one BYTE vectorised array by another")))

  (setq sym (intern "GSPLI" starfort-mode-token-table))
  (set sym "GSPLI( {pli} )")
  (setplist sym '((class . token) (desc . "Set polyline index")))

  (setq sym (intern "VEC_WTOUW" starfort-mode-token-table))
  (set sym "VEC_WTOUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD vectorised array to UNSIGNED WORD")))

  (setq sym (intern "VEC_SQRTUW" starfort-mode-token-table))
  (set sym "VEC_SQRTUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of UNSIGNED WORD vectorised array")))

  (setq sym (intern "VEC_RTOUW" starfort-mode-token-table))
  (set sym "VEC_RTOUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL vectorised array to UNSIGNED WORD")))

  (setq sym (intern "STATUS" starfort-mode-token-table))
  (set sym '(("STATUS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VEC_NEGUW" starfort-mode-token-table))
  (set sym "VEC_NEGUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of UNSIGNED WORD vectorised array")))

  (setq sym (intern "PITFALLS" starfort-mode-token-table))
  (set sym '(("PITFALLS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VEC_ITOUW" starfort-mode-token-table))
  (set sym "VEC_ITOUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER vectorised array to UNSIGNED WORD")))

  (setq sym (intern "TRN__MIDIN" starfort-mode-token-table))
  (set sym "TRN__MIDIN")
  (setplist sym '((class . token) (desc . "Compiled mapping identifier invalid (error code)")))

  (setq sym (intern "VEC_DTOUW" starfort-mode-token-table))
  (set sym "VEC_DTOUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION vectorised array to UNSIGNED WORD")))

  (setq sym (intern "VEC_WTOUB" starfort-mode-token-table))
  (set sym "VEC_WTOUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD vectorised array to UNSIGNED BYTE")))

  (setq sym (intern "VEC_BTOUW" starfort-mode-token-table))
  (set sym "VEC_BTOUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE vectorised array to UNSIGNED WORD")))

  (setq sym (intern "STATUS_ARGUMENT" starfort-mode-token-table))
  (set sym '(("STATUS_ARGUMENT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VEC_SQRTUB" starfort-mode-token-table))
  (set sym "VEC_SQRTUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of UNSIGNED BYTE vectorised array")))

  (setq sym (intern "VEC_RTOUB" starfort-mode-token-table))
  (set sym "VEC_RTOUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL vectorised array to UNSIGNED BYTE")))

  (setq sym (intern "DAT__PRMAP" starfort-mode-token-table))
  (set sym "DAT__PRMAP")
  (setplist sym '((class . token) (desc . "Primitive data mapped (error code)")))

  (setq sym (intern "VEC_NEGUB" starfort-mode-token-table))
  (set sym "VEC_NEGUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of UNSIGNED BYTE vectorised array")))

  (setq sym (intern "NDF_MBND" starfort-mode-token-table))
  (set sym "NDF_MBND( {option}, {indf1}, {indf2}, {status} )")
  (setplist sym '((class . token) (desc . "Match the pixel-index bounds of a pair of NDFs") (helpkey . "NDF_MBND")))

  (setq sym (intern "NDF_GTUNE" starfort-mode-token-table))
  (set sym "NDF_GTUNE( {tpar}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the value of an NDF_ system tuning parameter") (helpkey . "NDF_GTUNE")))

  (setq sym (intern "VEC_ITOUB" starfort-mode-token-table))
  (set sym "VEC_ITOUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER vectorised array to UNSIGNED BYTE")))

  (setq sym (intern "GRP_SETCS" starfort-mode-token-table))
  (set sym "GRP_SETCS( {igrp}, {sensit}, {status} )")
  (setplist sym '((class . token) (desc . "Establish the case sensitivity of a group")))

  (setq sym (intern "VEC_DTOUB" starfort-mode-token-table))
  (set sym "VEC_DTOUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION vectorised array to UNSIGNED BYTE")))

  (setq sym (intern "VEC_BTOUB" starfort-mode-token-table))
  (set sym "VEC_BTOUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE vectorised array to UNSIGNED BYTE")))

  (setq sym (intern "PGRNGE" starfort-mode-token-table))
  (set sym "PGRNGE( {x1}, {x2}, {xlo}, {xhi} )")
  (setplist sym '((class . token) (desc . "Choose axis limits")))

  (setq sym (intern "EXTERNAL" starfort-mode-token-table))
  (set sym '(("EXTERNAL_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "BLOCK_DATA" starfort-mode-token-table))
  (set sym '(("BLOCKDATA_PROGRAM_MODULE" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VEC_DIMW" starfort-mode-token-table))
  (set sym "VEC_DIMW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two WORD vectorised arrays")))

  (setq sym (intern "VEC_DIMR" starfort-mode-token-table))
  (set sym "VEC_DIMR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two REAL vectorised arrays")))

  (setq sym (intern "GRP_GETCS" starfort-mode-token-table))
  (set sym "GRP_GETCS( {igrp}, {sensit}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the case sensitivity of a group")))

  (setq sym (intern "DAT_ANNUL" starfort-mode-token-table))
  (set sym "DAT_ANNUL( {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Annul locator")))

  (setq sym (intern "NUM_PWRW" starfort-mode-token-table))
  (set sym "NUM_PWRW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Raise to a power (WORD numbers)")))

  (setq sym (intern "GRP_SETCC" starfort-mode-token-table))
  (set sym "GRP_SETCC( {igrp}, {cclist}, {cc}, {status} )")
  (setplist sym '((class . token) (desc . "Sets requested control characters for the specified group")))

  (setq sym (intern "NUM_PWRR" starfort-mode-token-table))
  (set sym "NUM_PWRR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Raise to a power (REAL numbers)")))

  (setq sym (intern "NUM_LG10W" starfort-mode-token-table))
  (set sym "NUM_LG10W( {num} )")
  (setplist sym '((class . token) (desc . "Common logarithm of WORD number")))

  (setq sym (intern "NDF_MAPZ" starfort-mode-token-table))
  (set sym "NDF_MAPZ( {indf}, {comp}, {type}, {mmod}, {rpntr}, {ipntr}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain complex mapped access to an array component of an NDF") (helpkey . "NDF_MAPZ")))

  (setq sym (intern "VEC_DIMI" starfort-mode-token-table))
  (set sym "VEC_DIMI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two INTEGER vectorised arrays")))

  (setq sym (intern "VAL_SUBW" starfort-mode-token-table))
  (set sym "VAL_SUBW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one WORD value from another")))

  (setq sym (intern "MONOLITH_PROLOGUE" starfort-mode-token-table))
  (set sym "\\*+
\\*	{monolith_prologue}
\\*-
\\	{monolith_declarations}
\\*.")
  (setplist sym '((class . token) (desc . "ADAM monolith routine prologue")))

  (setq sym (intern "NUM_LG10R" starfort-mode-token-table))
  (set sym "NUM_LG10R( {num} )")
  (setplist sym '((class . token) (desc . "Common logarithm of REAL number")))

  (setq sym (intern "MSG_OUTIF" starfort-mode-token-table))
  (set sym "MSG_OUTIF( {prior}, {param}, {text}, {status} )")
  (setplist sym '((class . token) (desc . "Conditionally deliver the text of a message to the user") (helpkey . "MSG_OUTIF")))

  (setq sym (intern "DAT__ACCON" starfort-mode-token-table))
  (set sym "DAT__ACCON")
  (setplist sym '((class . token) (desc . "Access conflict (error code)")))

  (setq sym (intern "VEC_DIMD" starfort-mode-token-table))
  (set sym "VEC_DIMD( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two DOUBLE PRECISION vectorised arrays")))

  (setq sym (intern "VAL_SUBR" starfort-mode-token-table))
  (set sym "VAL_SUBR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one REAL value from another")))

  (setq sym (intern "VEC_DIMB" starfort-mode-token-table))
  (set sym "VEC_DIMB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two BYTE vectorised arrays")))

  (setq sym (intern "NUM_PWRI" starfort-mode-token-table))
  (set sym "NUM_PWRI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Raise to a power (INTEGER numbers)")))

  (setq sym (intern "GRP_GETCC" starfort-mode-token-table))
  (set sym "GRP_GETCC( {igrp}, {cclist}, {cc}, {status} )")
  (setplist sym '((class . token) (desc . "Returns requested control characters for the specified group")))

  (setq sym (intern "SGS_OPNWK" starfort-mode-token-table))
  (set sym "SGS_OPNWK( {wkstn}, {izonid}, {status} )")
  (setplist sym '((class . token) (desc . "Open workstation")))

  (setq sym (intern "NUM_PWRD" starfort-mode-token-table))
  (set sym "NUM_PWRD( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Raise to a power (DOUBLE PRECISION numbers)")))

  (setq sym (intern "NUM_LG10I" starfort-mode-token-table))
  (set sym "NUM_LG10I( {num} )")
  (setplist sym '((class . token) (desc . "Common logarithm of INTEGER number")))

  (setq sym (intern "VAL_SUBI" starfort-mode-token-table))
  (set sym "VAL_SUBI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one INTEGER value from another")))

  (setq sym (intern "NUM_PWRB" starfort-mode-token-table))
  (set sym "NUM_PWRB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Raise to a power (BYTE numbers)")))

  (setq sym (intern "GSPAR" starfort-mode-token-table))
  (set sym "GSPAR( {wkid}, {pai}, {dx}, {dy}, {dimx}, {colia} )")
  (setplist sym '((class . token) (desc . "Set pattern representation")))

  (setq sym (intern "NUM_LG10D" starfort-mode-token-table))
  (set sym "NUM_LG10D( {num} )")
  (setplist sym '((class . token) (desc . "Common logarithm of DOUBLE PRECISION number")))

  (setq sym (intern "VAL_SUBD" starfort-mode-token-table))
  (set sym "VAL_SUBD( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one DOUBLE PRECISION value from another")))

  (setq sym (intern "NUM_LG10B" starfort-mode-token-table))
  (set sym "NUM_LG10B( {num} )")
  (setplist sym '((class . token) (desc . "Common logarithm of BYTE number")))

  (setq sym (intern "SGS_OPOLY" starfort-mode-token-table))
  (set sym "SGS_OPOLY")
  (setplist sym '((class . token) (desc . "Output buffered polyline")))

  (setq sym (intern "VAL_SUBB" starfort-mode-token-table))
  (set sym "VAL_SUBB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one BYTE value from another")))

  (setq sym (intern "SLA_SEP" starfort-mode-token-table))
  (set sym "SLA_SEP( {a1}, {b1}, {a2}, {b2} )")
  (setplist sym '((class . token) (desc . "Angle between two points on a sphere")))

  (setq sym (intern "MSG_IFLEV" starfort-mode-token-table))
  (set sym "MSG_IFLEV( {filter} )")
  (setplist sym '((class . token) (desc . "Return the current filter level for conditional message output") (helpkey . "MSG_IFLEV")))

  (setq sym (intern "NDF_CPUT" starfort-mode-token-table))
  (set sym "NDF_CPUT( {value}, {indf}, {comp}, {status} )")
  (setplist sym '((class . token) (desc . "Assign a value to an NDF character component") (helpkey . "NDF_CPUT")))

  (setq sym (intern "ARITH_RELATIONAL_EXP" starfort-mode-token-table))
  (set sym "{arith_exp} {rop} {arith_exp}")
  (setplist sym '((class . token) (desc . "Arithmetic relational expression")))

  (setq sym (intern "SGS_BPOLY" starfort-mode-token-table))
  (set sym "SGS_BPOLY( {x}, {y} )")
  (setplist sym '((class . token) (desc . "Begin a polyline")))

  (setq sym (intern "NUM_ADDUW" starfort-mode-token-table))
  (set sym "NUM_ADDUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Add two UNSIGNED WORD numbers")))

  (setq sym (intern "NDF_CREP" starfort-mode-token-table))
  (set sym "NDF_CREP( {param}, {ftype}, {ndim}, {ubnd}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Create a new primitive NDF via the ADAM parameter system") (helpkey . "NDF_CREP")))

  (setq sym (intern "SGS_APOLY" starfort-mode-token-table))
  (set sym "SGS_APOLY( {x}, {y} )")
  (setplist sym '((class . token) (desc . "Append a new line to a polyline")))

  (setq sym (intern "SLA_GE50" starfort-mode-token-table))
  (set sym "SLA_GE50( {dl}, {db}, {dr}, {dd} )")
  (setplist sym '((class . token) (desc . "Transformation from IAU 1958 galactic coordinates to B1950.0 'FK4' equatorial coordinates")))

  (setq sym (intern "SLA_RANDOM" starfort-mode-token-table))
  (set sym "SLA_RANDOM( {seed} )")
  (setplist sym '((class . token) (desc . "Generate pseudo-random real number in the range 0 <= X < 1.")))

  (setq sym (intern "NUM_INTW" starfort-mode-token-table))
  (set sym "NUM_INTW( {num} )")
  (setplist sym '((class . token) (desc . "Truncate WORD number to an integer")))

  (setq sym (intern "NDF_SQMF" starfort-mode-token-table))
  (set sym "NDF_SQMF( {qmf}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Set a new logical value for an NDF's quality masking flag") (helpkey . "NDF_SQMF")))

  (setq sym (intern "NUM_ASNDR" starfort-mode-token-table))
  (set sym "NUM_ASNDR( {num} )")
  (setplist sym '((class . token) (desc . "Inverse sine function of REAL number (degrees)")))

  (setq sym (intern "IDI_CLRFG" starfort-mode-token-table))
  (set sym "IDI_CLRFG( {iflag} )")
  (setplist sym '((class . token) (desc . "Set clear flag")))

  (setq sym (intern "AGI_ISAMP" starfort-mode-token-table))
  (set sym "AGI_ISAMP( {picid}, {lsame}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire if two pictures are the same")))

  (setq sym (intern "NUM_INTR" starfort-mode-token-table))
  (set sym "NUM_INTR( {num} )")
  (setplist sym '((class . token) (desc . "Truncate REAL number to an integer")))

  (setq sym (intern "DAT__FATAL" starfort-mode-token-table))
  (set sym "DAT__FATAL")
  (setplist sym '((class . token) (desc . "Fatal internal error (error code)")))

  (setq sym (intern "TRN__POSDT" starfort-mode-token-table))
  (set sym "TRN__POSDT")
  (setplist sym '((class . token) (desc . "'Reflection is absent' classification (symbolic constant)")))

  (setq sym (intern "SLA_PXY" starfort-mode-token-table))
  (set sym "SLA_PXY( {np}, {xye}, {xym}, {coeffs}, {xyp}, {xrms}, {yrms}, {rrms} )")
  (setplist sym '((class . token) (desc . "compute the array of PREDICTED coordinates and the RMS residuals")))

  (setq sym (intern "NUM_ADDUB" starfort-mode-token-table))
  (set sym "NUM_ADDUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Add two UNSIGNED BYTE numbers")))

  (setq sym (intern "NUM_INTI" starfort-mode-token-table))
  (set sym "NUM_INTI( {num} )")
  (setplist sym '((class . token) (desc . "Truncate INTEGER number to an integer")))

  (setq sym (intern "AGI_ISAMD" starfort-mode-token-table))
  (set sym "AGI_ISAMD( {picid}, {lsame}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire if pictures are on same device")))

  (setq sym (intern "NUM_ASNDD" starfort-mode-token-table))
  (set sym "NUM_ASNDD( {num} )")
  (setplist sym '((class . token) (desc . "Inverse sine function of DOUBLE PRECISION number (degrees)")))

  (setq sym (intern "NUM_INTD" starfort-mode-token-table))
  (set sym "NUM_INTD( {num} )")
  (setplist sym '((class . token) (desc . "Truncate DOUBLE PRECISION number to an integer")))

  (setq sym (intern "ACCESS" starfort-mode-token-table))
  (set sym "ACCESS = {access_options}")
  (setplist sym '((class . token) (desc . "ACCESS = {access_options}")))

  (setq sym (intern "NUM_UBTOW" starfort-mode-token-table))
  (set sym "NUM_UBTOW( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE number to WORD")))

  (setq sym (intern "NUM_INTB" starfort-mode-token-table))
  (set sym "NUM_INTB( {num} )")
  (setplist sym '((class . token) (desc . "Truncate BYTE number to an integer")))

  (setq sym (intern "NUM_UBTOR" starfort-mode-token-table))
  (set sym "NUM_UBTOR( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE number to REAL")))

  (setq sym (intern "DAT_RETYP" starfort-mode-token-table))
  (set sym "DAT_RETYP( {loc}, {type}, {status} )")
  (setplist sym '((class . token) (desc . "Change object type")))

  (setq sym (intern "DAT__OBJNF" starfort-mode-token-table))
  (set sym "DAT__OBJNF")
  (setplist sym '((class . token) (desc . "Object not found (error code)")))

  (setq sym (intern "SLA_AMPQK" starfort-mode-token-table))
  (set sym "SLA_AMPQK( {ra}, {da}, {amprms}, {rm}, {dm} )")
  (setplist sym '((class . token) (desc . "Convert star RA,Dec from geocentric apparent to mean place")))

  (setq sym (intern "NUM_UBTOI" starfort-mode-token-table))
  (set sym "NUM_UBTOI( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE number to INTEGER")))

  (setq sym (intern "NDF_MBAD" starfort-mode-token-table))
  (set sym "NDF_MBAD( {badok}, {indf1}, {indf2}, {comp}, {check}, {bad}, {status} )")
  (setplist sym '((class . token) (desc . "Merge the bad-pixel flags of the array components of a pair of NDFs") (helpkey . "NDF_MBAD")))

  (setq sym (intern "SLA_SVDSOL" starfort-mode-token-table))
  (set sym "SLA_SVDSOL( {m}, {n}, {mp}, {np}, {b}, {u}, {w}, {v}, {work}, {x} )")
  (setplist sym '((class . token) (desc . "From a given vector and the SVD of a matrix (as obtained from the SVD routine), obtain the solution vector.")))

  (setq sym (intern "VAL_MAXW" starfort-mode-token-table))
  (set sym "VAL_MAXW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two WORD values")))

  (setq sym (intern "NUM_UBTOD" starfort-mode-token-table))
  (set sym "NUM_UBTOD( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE number to DOUBLE PRECISION")))

  (setq sym (intern "GRP_PTYPE" starfort-mode-token-table))
  (set sym "GRP_PTYPE( {igrp}, {type}, {status} )")
  (setplist sym '((class . token) (desc . "Associate a new type string with a group")))

  (setq sym (intern "VAL_INTUW" starfort-mode-token-table))
  (set sym "VAL_INTUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate UNSIGNED WORD value to an integer")))

  (setq sym (intern "NUM_UBTOB" starfort-mode-token-table))
  (set sym "NUM_UBTOB( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE number to BYTE")))

  (setq sym (intern "VAL_MAXR" starfort-mode-token-table))
  (set sym "VAL_MAXR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two REAL values")))

  (setq sym (intern "DAT_COERC" starfort-mode-token-table))
  (set sym "DAT_COERC( {loc1}, {ndim}, {loc2}, {status} )")
  (setplist sym '((class . token) (desc . "Coerce object shape")))

  (setq sym (intern "DAT_PUTVR" starfort-mode-token-table))
  (set sym "DAT_PUTVR( {loc}, {el}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL data to a primitive as if vectorised")))

  (setq sym (intern "GRP_PURGE" starfort-mode-token-table))
  (set sym "GRP_PURGE( {igrp1}, {igrp2}, {status} )")
  (setplist sym '((class . token) (desc . "Purge duplicate entries from a group")))

  (setq sym (intern "GRP_GTYPE" starfort-mode-token-table))
  (set sym "GRP_GTYPE( {igrp}, {type}, {status} )")
  (setplist sym '((class . token) (desc . "Retrieve the type string stored with a group")))

  (setq sym (intern "VAL_MAXI" starfort-mode-token-table))
  (set sym "VAL_MAXI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two INTEGER values")))

  (setq sym (intern "DAT_PUTVL" starfort-mode-token-table))
  (set sym "DAT_PUTVL( {loc}, {el}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL data to a primitive as if vectorised")))

  (setq sym (intern "VAL_DIVW" starfort-mode-token-table))
  (set sym "VAL_DIVW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one WORD value by another")))

  (setq sym (intern "NUM_ABSUW" starfort-mode-token-table))
  (set sym "NUM_ABSUW( {num} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of UNSIGNED WORD number")))

  (setq sym (intern "SLA_WAIT" starfort-mode-token-table))
  (set sym "SLA_WAIT( {delay} )")
  (setplist sym '((class . token) (desc . "Interval wait")))

  (setq sym (intern "DAT_PUTVI" starfort-mode-token-table))
  (set sym "DAT_PUTVI( {loc}, {el}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER data to a primitive as if vectorised")))

  (setq sym (intern "VAL_MAXD" starfort-mode-token-table))
  (set sym "VAL_MAXD( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two DOUBLE PRECISION values")))

  (setq sym (intern "NDF_DELET" starfort-mode-token-table))
  (set sym "NDF_DELET( {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Delete an NDF") (helpkey . "NDF_DELET")))

  (setq sym (intern "DAT_TYPE" starfort-mode-token-table))
  (set sym "DAT_TYPE( {loc}, {type}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire object type")))

  (setq sym (intern "DAT_GETVR" starfort-mode-token-table))
  (set sym "DAT_GETVR( {loc}, {elx}, {rvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive as REAL as if vectorised")))

  (setq sym (intern "VAL_MAXB" starfort-mode-token-table))
  (set sym "VAL_MAXB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two BYTE values")))

  (setq sym (intern "DAT__OBJIN" starfort-mode-token-table))
  (set sym "DAT__OBJIN")
  (setplist sym '((class . token) (desc . "Object invalid (error code)")))

  (setq sym (intern "ARY_FORM" starfort-mode-token-table))
  (set sym "ARY_FORM( {iary}, {form}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the storage form of an array")))

  (setq sym (intern "VAL_DIVR" starfort-mode-token-table))
  (set sym "VAL_DIVR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one REAL value by another")))

  (setq sym (intern "CMP_MODC" starfort-mode-token-table))
  (set sym "CMP_MODC( {loc}, {name}, {len}, {ndim}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain _CHAR component")))

  (setq sym (intern "VAL_INTUB" starfort-mode-token-table))
  (set sym "VAL_INTUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate UNSIGNED BYTE value to an integer")))

  (setq sym (intern "DAT_PUTVD" starfort-mode-token-table))
  (set sym "DAT_PUTVD( {loc}, {el}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION data to a primitive as if vectorised")))

  (setq sym (intern "GQASWK" starfort-mode-token-table))
  (set sym "GQASWK( {sgna}, {n}, {errind}, {ol}, {wkid} )")
  (setplist sym '((class . token) (desc . "Inquire set member of associated workstations")))

  (setq sym (intern "DAT_PUTVC" starfort-mode-token-table))
  (set sym "DAT_PUTVC( {loc}, {el}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER data to a primitive as if vectorised")))

  (setq sym (intern "GCLWK" starfort-mode-token-table))
  (set sym "GCLWK( {wkid} )")
  (setplist sym '((class . token) (desc . "Close workstation")))

  (setq sym (intern "DAT_GETVL" starfort-mode-token-table))
  (set sym "DAT_GETVL( {loc}, {elx}, {lvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive as LOGICAL as if vectorised")))

  (setq sym (intern "PSX_CTIME" starfort-mode-token-table))
  (set sym "PSX_CTIME( {nticks}, {string}, {status} )")
  (setplist sym '((class . token) (desc . "Convert the calendar time to a character string")))

  (setq sym (intern "DAT_GETVI" starfort-mode-token-table))
  (set sym "DAT_GETVI( {loc}, {elx}, {ivalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive as INTEGER as if vectorised")))

  (setq sym (intern "VAL_DIVI" starfort-mode-token-table))
  (set sym "VAL_DIVI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one INTEGER value by another")))

  (setq sym (intern "CHR_FIWS" starfort-mode-token-table))
  (set sym "CHR_FIWS( {string}, {index}, {status} )")
  (setplist sym '((class . token) (desc . "Find next start of word")))

  (setq sym (intern "DIRECT_PAR" starfort-mode-token-table))
  (set sym '(("DIRECT_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SLA_RCC" starfort-mode-token-table))
  (set sym "SLA_RCC( {tdb}, {ut1}, {wl}, {u}, {v} )")
  (setplist sym '((class . token) (desc . "Relativistic clock correction")))

  (setq sym (intern "MAG_WTM" starfort-mode-token-table))
  (set sym "MAG_WTM( {td}, {status} )")
  (setplist sym '((class . token) (desc . "Write a tape mark")))

  (setq sym (intern "DAT_GETVD" starfort-mode-token-table))
  (set sym "DAT_GETVD( {loc}, {elx}, {dvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive as DOUBLE PRECISION as if vectorised")))

  (setq sym (intern "DAT_GETVC" starfort-mode-token-table))
  (set sym "DAT_GETVC( {loc}, {elx}, {cvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive as CHARACTER as if vectorised")))

  (setq sym (intern "VAL_DIVD" starfort-mode-token-table))
  (set sym "VAL_DIVD( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one DOUBLE PRECISION value by another")))

  (setq sym (intern "GERLOG" starfort-mode-token-table))
  (set sym "GERLOG( {errnr}, {fctid}, {errfil} )")
  (setplist sym '((class . token) (desc . "Error logging")))

  (setq sym (intern "TRN_PAR" starfort-mode-token-table))
  (set sym "TRN_PAR")
  (setplist sym '((class . token) (desc . "TRN__ symbolic constant definitions (include file)")))

  (setq sym (intern "VAL_DIVB" starfort-mode-token-table))
  (set sym "VAL_DIVB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one BYTE value by another")))

  (setq sym (intern "NUM_ABSUB" starfort-mode-token-table))
  (set sym "NUM_ABSUB( {num} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of UNSIGNED BYTE number")))

  (setq sym (intern "DAT_FIND" starfort-mode-token-table))
  (set sym "DAT_FIND( {loc1}, {name}, {loc2}, {status} )")
  (setplist sym '((class . token) (desc . "Find named component")))

  (setq sym (intern "IIISTI" starfort-mode-token-table))
  (set sym "IIISTI( {dispid}, {status} )")
  (setplist sym '((class . token) (desc . "Stop Interactive Input")))

  (setq sym (intern "SGS_IPLXY" starfort-mode-token-table))
  (set sym "SGS_IPLXY( {x}, {y} )")
  (setplist sym '((class . token) (desc . "Inquire polyline x & y")))

  (setq sym (intern "CHR_FIWE" starfort-mode-token-table))
  (set sym "CHR_FIWE( {string}, {index}, {status} )")
  (setplist sym '((class . token) (desc . "Find next end of word")))

  (setq sym (intern "NDF_COPY" starfort-mode-token-table))
  (set sym "NDF_COPY( {indf1}, {place}, {indf2}, {status} )")
  (setplist sym '((class . token) (desc . "Copy an NDF to a new location") (helpkey . "NDF_COPY")))

  (setq sym (intern "GCLSG" starfort-mode-token-table))
  (set sym "GCLSG")
  (setplist sym '((class . token) (desc . "Close segment")))

  (setq sym (intern "SLA_GALSUP" starfort-mode-token-table))
  (set sym "SLA_GALSUP( {dl}, {db}, {dsl}, {dsb} )")
  (setplist sym '((class . token) (desc . "Transformation from IAU 1958 galactic coordinates to de Vaucouleurs supergalactic coordinates")))

  (setq sym (intern "NAMED_PAR" starfort-mode-token-table))
  (set sym '(("NAMED_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "FORMATTED_PAR" starfort-mode-token-table))
  (set sym '(("FORMATTED_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "FORMAT" starfort-mode-token-table))
  (set sym '(lambda nil (let (indent) (starfort-indent-line) (setq indent (current-column)) (delete-region (save-excursion (beginning-of-line) (point)) (point)) (insert " {lbl}") (insert-char 32 (- indent 6)) (insert "FORMAT ( [/]... {field_spec}... [/]... )")) t))
  (setplist sym '((class . token) (desc . "FORMAT statement")))

  (setq sym (intern "DAT_PUTNR" starfort-mode-token-table))
  (set sym "DAT_PUTNR( {loc}, {ndim}, {dimx}, {rvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL data to an array primitive")))

  (setq sym (intern "DAT_PUTNL" starfort-mode-token-table))
  (set sym "DAT_PUTNL( {loc}, {ndim}, {dimx}, {lvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL data to an array primitive")))

  (setq sym (intern "DAT_PUTNI" starfort-mode-token-table))
  (set sym "DAT_PUTNI( {loc}, {ndim}, {dimx}, {ivalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER data to an array primitive")))

  (setq sym (intern "FIO_RWIND" starfort-mode-token-table))
  (set sym "FIO_RWIND( {fd}, {status} )")
  (setplist sym '((class . token) (desc . "Rewind a sequential file")))

  (setq sym (intern "DAT_GETNR" starfort-mode-token-table))
  (set sym "DAT_GETNR( {loc}, {ndim}, {dimx}, {rvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read array primitive as REAL")))

  (setq sym (intern "RECL_PAR" starfort-mode-token-table))
  (set sym '(("RECL_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "DAT_PUTND" starfort-mode-token-table))
  (set sym "DAT_PUTND( {loc}, {ndim}, {dimx}, {dvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION data to an array primitive")))

  (setq sym (intern "DAT_PUTNC" starfort-mode-token-table))
  (set sym "DAT_PUTNC( {loc}, {ndim}, {dimx}, {cvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER data to an array primitive")))

  (setq sym (intern "VAL_DIMW" starfort-mode-token-table))
  (set sym "VAL_DIMW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two WORD values")))

  (setq sym (intern "SLA_DVXV" starfort-mode-token-table))
  (set sym "SLA_DVXV( {va}, {vb}, {vc} )")
  (setplist sym '((class . token) (desc . "Vector product of two 3-vectors (double precision)")))

  (setq sym (intern "DAT_GETNL" starfort-mode-token-table))
  (set sym "DAT_GETNL( {loc}, {ndim}, {dimx}, {lvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read array primitive as LOGICAL")))

  (setq sym (intern "DAT_GETNI" starfort-mode-token-table))
  (set sym "DAT_GETNI( {loc}, {ndim}, {dimx}, {ivalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read array primitive as INTEGER")))

  (setq sym (intern "VAL_DIMR" starfort-mode-token-table))
  (set sym "VAL_DIMR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two REAL values")))

  (setq sym (intern "DAT_GETND" starfort-mode-token-table))
  (set sym "DAT_GETND( {loc}, {ndim}, {dimx}, {dvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read array primitive as DOUBLE PRECISION")))

  (setq sym (intern "DAT_GETNC" starfort-mode-token-table))
  (set sym "DAT_GETNC( {loc}, {ndim}, {dimx}, {cvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read array primitive as CHARACTER")))

  (setq sym (intern "DAT_MOVE" starfort-mode-token-table))
  (set sym "DAT_MOVE( {loc1}, {loc2}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Move object")))

  (setq sym (intern "ARY_CLONE" starfort-mode-token-table))
  (set sym "ARY_CLONE( {iary1}, {iary2}, {status} )")
  (setplist sym '((class . token) (desc . "Clone an array identifier")))

  (setq sym (intern "CMOD" starfort-mode-token-table))
  (set sym "CHARACTER * ( DAT__SZMOD )")
  (setplist sym '((class . token) (alias . t)))

  (setq sym (intern "VAL_DIMI" starfort-mode-token-table))
  (set sym "VAL_DIMI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two INTEGER values")))

  (setq sym (intern "SLA_MOON" starfort-mode-token-table))
  (set sym "SLA_MOON( {iy}, {id}, {fd}, {posvel} )")
  (setplist sym '((class . token) (desc . "Approximate geocentric position and velocity of the moon")))

  (setq sym (intern "VAL_DIMD" starfort-mode-token-table))
  (set sym "VAL_DIMD( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two DOUBLE PRECISION values")))

  (setq sym (intern "AGI_ICOM" starfort-mode-token-table))
  (set sym "AGI_ICOM( {coment}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire comment for the current picture")))

  (setq sym (intern "VAL_DIMB" starfort-mode-token-table))
  (set sym "VAL_DIMB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two BYTE values")))

  (setq sym (intern "DAT_UNMAP" starfort-mode-token-table))
  (set sym "DAT_UNMAP( {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Unmap object")))

  (setq sym (intern "GCLKS" starfort-mode-token-table))
  (set sym "GCLKS")
  (setplist sym '((class . token) (desc . "Close GKS")))

  (setq sym (intern "SLA_DFLTIN" starfort-mode-token-table))
  (set sym "SLA_DFLTIN( {string}, {nstrt}, {dreslt}, {jflag} )")
  (setplist sym '((class . token) (desc . "Convert free-format input into double precision floating point")))

  (setq sym (intern "ERR_FLUSH" starfort-mode-token-table))
  (set sym "ERR_FLUSH( {status} )")
  (setplist sym '((class . token) (desc . "Flush the current error context") (helpkey . "ERR_FLUSH")))

  (setq sym (intern "DAT_NEW1R" starfort-mode-token-table))
  (set sym "DAT_NEW1R( {loc}, {name}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Create vector _REAL component")))

  (setq sym (intern "ACCURACY" starfort-mode-token-table))
  (set sym '(("ACCURACY" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "DAT_NEW1L" starfort-mode-token-table))
  (set sym "DAT_NEW1L( {loc}, {name}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Create vector _LOGICAL component")))

  (setq sym (intern "PAR_PUTVR" starfort-mode-token-table))
  (set sym "PAR_PUTVR( {param}, {nval}, {rvalues}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL parameter values as if vectorised")))

  (setq sym (intern "DAT_NEW0R" starfort-mode-token-table))
  (set sym "DAT_NEW0R( {loc}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Create scalar _REAL component")))

  (setq sym (intern "DAT_NEW1I" starfort-mode-token-table))
  (set sym "DAT_NEW1I( {loc}, {name}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Create vector _INTEGER component")))

  (setq sym (intern "CHR_FILL" starfort-mode-token-table))
  (set sym "CHR_FILL( {char}, {string} )")
  (setplist sym '((class . token) (desc . "Fill a string with a given character")))

  (setq sym (intern "TRN_NEW" starfort-mode-token-table))
  (set sym "TRN_NEW( {nvin}, {nvout}, {for}, {inv}, {prec}, {comm}, {eloc}, {name}, {loctr}, {status} )")
  (setplist sym '((class . token) (desc . "Create new transformation")))

  (setq sym (intern "SLA_NUT" starfort-mode-token-table))
  (set sym "SLA_NUT( {date}, {rmatn} )")
  (setplist sym '((class . token) (desc . "Form the matrix of nutation for a given date (IAU 1980 theory)")))

  (setq sym (intern "PAR_PUTVL" starfort-mode-token-table))
  (set sym "PAR_PUTVL( {param}, {nval}, {lvalues}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL parameter values as if vectorised")))

  (setq sym (intern "DAT_NEW1D" starfort-mode-token-table))
  (set sym "DAT_NEW1D( {loc}, {name}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Create vector _DOUBLE component")))

  (setq sym (intern "DAT_NEW0L" starfort-mode-token-table))
  (set sym "DAT_NEW0L( {loc}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Create scalar _LOGICAL component")))

  (setq sym (intern "DAT_NEW1C" starfort-mode-token-table))
  (set sym "DAT_NEW1C( {loc}, {name}, {len}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Create vector _CHAR component")))

  (setq sym (intern "PAR_PUTVI" starfort-mode-token-table))
  (set sym "PAR_PUTVI( {param}, {nval}, {ivalues}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER parameter values as if vectorised")))

  (setq sym (intern "DAT_NEW0I" starfort-mode-token-table))
  (set sym "DAT_NEW0I( {loc}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Create scalar _INTEGER component")))

  (setq sym (intern "PAR_GETVR" starfort-mode-token-table))
  (set sym "PAR_GETVR( {param}, {maxval}, {rvalue}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Get REAL parameter values as if vectorised")))

  (setq sym (intern "SLA_EPCO" starfort-mode-token-table))
  (set sym "SLA_EPCO( {k0}, {k}, {e} )")
  (setplist sym '((class . token) (desc . "Convert an epoch into the appropriate form - 'B' or 'J'")))

  (setq sym (intern "PAR_PUTVD" starfort-mode-token-table))
  (set sym "PAR_PUTVD( {param}, {nval}, {dvalues}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION parameter values as if vectorised")))

  (setq sym (intern "DAT_NEW0D" starfort-mode-token-table))
  (set sym "DAT_NEW0D( {loc}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Create scalar _DOUBLE component")))

  (setq sym (intern "PAR_PUTVC" starfort-mode-token-table))
  (set sym "PAR_PUTVC( {param}, {nval}, {cvalues}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER parameter values as if vectorised")))

  (setq sym (intern "DAT_NEW0C" starfort-mode-token-table))
  (set sym "DAT_NEW0C( {loc}, {name}, {len}, {status} )")
  (setplist sym '((class . token) (desc . "Create scalar _CHAR component")))

  (setq sym (intern "NDF_XNAME" starfort-mode-token-table))
  (set sym "NDF_XNAME( {indf}, {n}, {xname}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the name of the N'th extension in an NDF") (helpkey . "NDF_XNAME")))

  (setq sym (intern "DAT__TYPIN" starfort-mode-token-table))
  (set sym "DAT__TYPIN")
  (setplist sym '((class . token) (desc . "Type invalid (error code)")))

  (setq sym (intern "CNAM" starfort-mode-token-table))
  (set sym "CHARACTER * ( DAT__SZNAM )")
  (setplist sym '((class . token) (alias . t)))

  (setq sym (intern "PAR_GETVL" starfort-mode-token-table))
  (set sym "PAR_GETVL( {param}, {maxval}, {lvalue}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Get LOGICAL parameter values as if vectorised")))

  (setq sym (intern "SLA_DEULER" starfort-mode-token-table))
  (set sym "SLA_DEULER( {order}, {phi}, {theta}, {psi}, {rmat} )")
  (setplist sym '((class . token) (desc . "Form a rotation matrix from the Euler angles - three successive rotations about specified Cartesian axes.")))

  (setq sym (intern "TRN_COMP" starfort-mode-token-table))
  (set sym "TRN_COMP( {loctr}, {forwd}, {id}, {status} )")
  (setplist sym '((class . token) (desc . "Compile transformation")))

  (setq sym (intern "READONLY" starfort-mode-token-table))
  (set sym "READONLY")
  (setplist sym '((class . token) (desc . "READONLY")))

  (setq sym (intern "GLOBAL_VARIABLES" starfort-mode-token-table))
  (set sym '(("GLOBAL_VARIABLES" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "PAR_GETVI" starfort-mode-token-table))
  (set sym "PAR_GETVI( {param}, {maxval}, {ivalue}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Get INTEGER parameter values as if vectorised")))

  (setq sym (intern "PAR_GETVD" starfort-mode-token-table))
  (set sym "PAR_GETVD( {param}, {maxval}, {dvalue}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Get DOUBLE PRECISION parameter values as if vectorised")))

  (setq sym (intern "PAR_GETVC" starfort-mode-token-table))
  (set sym "PAR_GETVC( {param}, {maxval}, {cvalue}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Get CHARACTER parameter values as if vectorised")))

  (setq sym (intern "FUNCTION_OPTIONS" starfort-mode-token-table))
  (set sym "[examples]
[pitfalls]
[notes]
[prior_requirements]
[side_effects]
[algorithm]
[accuracy]
[timing]
[routines_used]
[deficiencies]
[machine_specifics]
[DIY_prologue_item]...
[references]
[keywords]
[copyright]")
  (setplist sym '((class . token) (desc . "Expanded list of all optional items")))

  (setq sym (intern "SLA_PCD" starfort-mode-token-table))
  (set sym "SLA_PCD( {disco}, {x}, {y} )")
  (setplist sym '((class . token) (desc . "Apply pincushion/barrel distortion to a tangent-plane [x,y].")))

  (setq sym (intern "SLA_MXV" starfort-mode-token-table))
  (set sym "SLA_MXV( {rm}, {va}, {vb} )")
  (setplist sym '((class . token) (desc . "Performs the 3-D forward unitary transformation: vector VB = matrix RM vector VA")))

  (setq sym (intern "CLOC" starfort-mode-token-table))
  (set sym "CHARACTER * ( DAT__SZLOC )")
  (setplist sym '((class . token) (alias . t)))

  (setq sym (intern "NDF_CMSG" starfort-mode-token-table))
  (set sym "NDF_CMSG( {token}, {indf}, {comp}, {status} )")
  (setplist sym '((class . token) (desc . "Assign the value of an NDF character component to a message token") (helpkey . "NDF_CMSG")))

  (setq sym (intern "ARY_OFFS" starfort-mode-token-table))
  (set sym "ARY_OFFS( {iary1}, {iary2}, {mxoffs}, {offs}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the pixel offset between two arrays")))

  (setq sym (intern "SLA_MXM" starfort-mode-token-table))
  (set sym "SLA_MXM( {a}, {b}, {c} )")
  (setplist sym '((class . token) (desc . "Product of two 3x3 matrices: matrix C = matrix A x matrix B")))

  (setq sym (intern "CHR_MOVE" starfort-mode-token-table))
  (set sym "CHR_MOVE( {str1}, {str2} )")
  (setplist sym '((class . token) (desc . "Move one string into another (ignoring trailing blanks)")))

  (setq sym (intern "GSLCM" starfort-mode-token-table))
  (set sym "GSLCM( {wkid}, {idnr}, {gks$mode}, {gks$esw} )")
  (setplist sym '((class . token) (desc . "Set locator mode")))

  (setq sym (intern "ARGUMENTS_RETURNED" starfort-mode-token-table))
  (set sym '(("ARGUMENTS_RETURNED" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VEC_SUBUW" starfort-mode-token-table))
  (set sym "VEC_SUBUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one UNSIGNED WORD vectorised array from another")))

  (setq sym (intern "PAR_PUTNR" starfort-mode-token-table))
  (set sym "PAR_PUTNR( {param}, {ndim}, {dimx}, {rvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL N-dimensional parameter values")))

  (setq sym (intern "SGS_WIDEN" starfort-mode-token-table))
  (set sym "SGS_WIDEN( {wkstn}, {itype}, {iconid}, {status} )")
  (setplist sym '((class . token) (desc . "Translate SGS workstation name to GKS")))

  (setq sym (intern "PAR_PUTNL" starfort-mode-token-table))
  (set sym "PAR_PUTNL( {param}, {ndim}, {dimx}, {lvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL N-dimensional parameter values")))

  (setq sym (intern "NUM_MAXUW" starfort-mode-token-table))
  (set sym "NUM_MAXUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Maximum of two UNSIGNED WORD numbers")))

  (setq sym (intern "PAR_PUTNI" starfort-mode-token-table))
  (set sym "PAR_PUTNI( {param}, {ndim}, {dimx}, {ivalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER N-dimensional parameter values")))

  (setq sym (intern "PAR_GETNR" starfort-mode-token-table))
  (set sym "PAR_GETNR( {param}, {ndim}, {dimx}, {rvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read REAL N-dimensional parameter value")))

  (setq sym (intern "PAR_PUTND" starfort-mode-token-table))
  (set sym "PAR_PUTND( {param}, {ndim}, {dimx}, {dvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION N-dimensional parameter values")))

  (setq sym (intern "VEC_SUBUB" starfort-mode-token-table))
  (set sym "VEC_SUBUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one UNSIGNED BYTE vectorised array from another")))

  (setq sym (intern "NUM_ATANR" starfort-mode-token-table))
  (set sym "NUM_ATANR( {num} )")
  (setplist sym '((class . token) (desc . "Inverse tangent function of REAL number (radians)")))

  (setq sym (intern "NUM_ASINR" starfort-mode-token-table))
  (set sym "NUM_ASINR( {num} )")
  (setplist sym '((class . token) (desc . "Inverse sine function of REAL number (radians)")))

  (setq sym (intern "PAR_PUTNC" starfort-mode-token-table))
  (set sym "PAR_PUTNC( {param}, {ndim}, {dimx}, {cvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER N-dimensional parameter values")))

  (setq sym (intern "MSG_SETR" starfort-mode-token-table))
  (set sym "MSG_SETR( {token}, {rvalue} )")
  (setplist sym '((class . token) (desc . "Assign a REAL value to a message token (concise)") (helpkey . "MSG_SETR")))

  (setq sym (intern "GRP_INFOI" starfort-mode-token-table))
  (set sym "GRP_INFOI( {igrp}, {index}, {item}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Retrieve an item of integer information about a name")))

  (setq sym (intern "PAR_GETNL" starfort-mode-token-table))
  (set sym "PAR_GETNL( {param}, {ndim}, {dimx}, {lvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read LOGICAL N-dimensional parameter value")))

  (setq sym (intern "IIRINR" starfort-mode-token-table))
  (set sym "IIRINR( {dispid}, {memid}, {roicol}, {xmin}, {ymin}, {xmax}, {ymax}, {roiid}, {status} )")
  (setplist sym '((class . token) (desc . "Initialize Rectangular Region of Interest")))

  (setq sym (intern "VEC_RTOW" starfort-mode-token-table))
  (set sym "VEC_RTOW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL vectorised array to WORD")))

  (setq sym (intern "NDF_TEMP" starfort-mode-token-table))
  (set sym "NDF_TEMP( {place}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain a placeholder for a temporary NDF") (helpkey . "NDF_TEMP")))

  (setq sym (intern "DAT_RESET" starfort-mode-token-table))
  (set sym "DAT_RESET( {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Reset object state")))

  (setq sym (intern "VEC_BTOW" starfort-mode-token-table))
  (set sym "VEC_BTOW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE vectorised array to WORD")))

  (setq sym (intern "PAR_GETNI" starfort-mode-token-table))
  (set sym "PAR_GETNI( {param}, {ndim}, {dimx}, {ivalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read INTEGER N-dimensional parameter value")))

  (setq sym (intern "MSG_SETL" starfort-mode-token-table))
  (set sym "MSG_SETL( {token}, {lvalue} )")
  (setplist sym '((class . token) (desc . "Assign a LOGICAL value to a message token (concise)") (helpkey . "MSG_SETL")))

  (setq sym (intern "GRP_INFOC" starfort-mode-token-table))
  (set sym "GRP_INFOC( {igrp}, {index}, {item}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Retrieve an item of character information about a name")))

  (setq sym (intern "VEC_RTOR" starfort-mode-token-table))
  (set sym "VEC_RTOR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL vectorised array to REAL")))

  (setq sym (intern "VEC_BTOR" starfort-mode-token-table))
  (set sym "VEC_BTOR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE vectorised array to REAL")))

  (setq sym (intern "PAR_GETND" starfort-mode-token-table))
  (set sym "PAR_GETND( {param}, {ndim}, {dimx}, {dvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read DOUBLE PRECISION N-dimensional parameter value")))

  (setq sym (intern "MSG_SETI" starfort-mode-token-table))
  (set sym "MSG_SETI( {token}, {ivalue} )")
  (setplist sym '((class . token) (desc . "Assign an INTEGER value to a message token (concise)") (helpkey . "MSG_SETI")))

  (setq sym (intern "PAR_GETNC" starfort-mode-token-table))
  (set sym "PAR_GETNC( {param}, {ndim}, {dimx}, {cvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read CHARACTER N-dimensional parameter value")))

  (setq sym (intern "GDAWK" starfort-mode-token-table))
  (set sym "GDAWK( {wkid} )")
  (setplist sym '((class . token) (desc . "Deactivate workstation")))

  (setq sym (intern "SLA_OBS" starfort-mode-token-table))
  (set sym "SLA_OBS( {n}, {c}, {name}, {w}, {p}, {h} )")
  (setplist sym '((class . token) (desc . "Parameters of selected groundbased observing stations")))

  (setq sym (intern "NUM_MAXUB" starfort-mode-token-table))
  (set sym "NUM_MAXUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Maximum of two UNSIGNED BYTE numbers")))

  (setq sym (intern "NUM_ATAND" starfort-mode-token-table))
  (set sym "NUM_ATAND( {num} )")
  (setplist sym '((class . token) (desc . "Inverse tangent function of DOUBLE PRECISION number (radians)")))

  (setq sym (intern "NUM_ASIND" starfort-mode-token-table))
  (set sym "NUM_ASIND( {num} )")
  (setplist sym '((class . token) (desc . "Inverse sine function of DOUBLE PRECISION number (radians)")))

  (setq sym (intern "MSG_SETD" starfort-mode-token-table))
  (set sym "MSG_SETD( {token}, {dvalue} )")
  (setplist sym '((class . token) (desc . "Assign a DOUBLE PRECISION value to a message token (concise)") (helpkey . "MSG_SETD")))

  (setq sym (intern "GKS_ASSOC" starfort-mode-token-table))
  (set sym "GKS_ASSOC( {param}, {acmode}, {wkid}, {status} )")
  (setplist sym '((class . token) (desc . "Associate graphics workstation with parameter and open it")))

  (setq sym (intern "MSG_SETC" starfort-mode-token-table))
  (set sym "MSG_SETC( {token}, {cvalue} )")
  (setplist sym '((class . token) (desc . "Assign a CHARACTER value to a message token (concise)") (helpkey . "MSG_SETC")))

  (setq sym (intern "VEC_RTOI" starfort-mode-token-table))
  (set sym "VEC_RTOI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL vectorised array to INTEGER")))

  (setq sym (intern "DAT_NEWC" starfort-mode-token-table))
  (set sym "DAT_NEWC( {loc}, {name}, {len}, {ndim}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Create _CHAR component")))

  (setq sym (intern "GRQVL" starfort-mode-token-table))
  (set sym "GRQVL( {wkid}, {vldnr}, {stat}, {val} )")
  (setplist sym '((class . token) (desc . "Request valuator")))

  (setq sym (intern "VEC_BTOI" starfort-mode-token-table))
  (set sym "VEC_BTOI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE vectorised array to INTEGER")))

  (setq sym (intern "VEC_RTOD" starfort-mode-token-table))
  (set sym "VEC_RTOD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL vectorised array to DOUBLE PRECISION")))

  (setq sym (intern "VEC_RTOB" starfort-mode-token-table))
  (set sym "VEC_RTOB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL vectorised array to BYTE")))

  (setq sym (intern "VEC_BTOD" starfort-mode-token-table))
  (set sym "VEC_BTOD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE vectorised array to DOUBLE PRECISION")))

  (setq sym (intern "SLA_OAP" starfort-mode-token-table))
  (set sym "SLA_OAP( {type}, {ob1}, {ob2}, {date}, {dut}, {elongm}, {phim}, {hm}, {xp}, {yp}, {tdk}, {pmb}, {rh}, {wl}, {tlr}, {rap}, {dap} )")
  (setplist sym '((class . token) (desc . "Observed to apparent place")))

  (setq sym (intern "COPYRIGHT" starfort-mode-token-table))
  (set sym '(("COPYRIGHT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VEC_BTOB" starfort-mode-token-table))
  (set sym "VEC_BTOB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE vectorised array to BYTE")))

  (setq sym (intern "PGLABEL" starfort-mode-token-table))
  (set sym "PGLABEL( {xlbl}, {ylbl}, {toplbl} )")
  (setplist sym '((class . token) (desc . "Write labels for x-axis, y-axis, and top of plot")))

  (setq sym (intern "GRQST" starfort-mode-token-table))
  (set sym "GRQST( {wkid}, {stdnr}, {stat}, {lostr}, {str} )")
  (setplist sym '((class . token) (desc . "Request string")))

  (setq sym (intern "GKS_GSTAT" starfort-mode-token-table))
  (set sym "GKS_GSTAT( {status} )")
  (setplist sym '((class . token) (desc . "Inquire if GKS has reported an error")))

  (setq sym (intern "GRQSK" starfort-mode-token-table))
  (set sym "GRQSK( {wkid}, {skdnr}, {n}, {stat}, {tnr}, {np}, {px}, {py} )")
  (setplist sym '((class . token) (desc . "Request stroke")))

  (setq sym (intern "GRP_REMOV" starfort-mode-token-table))
  (set sym "GRP_REMOV( {igrp1}, {name}, {igrp2}, {status} )")
  (setplist sym '((class . token) (desc . "Remove all occurrences of a given name from a group")))

  (setq sym (intern "AGI_CANCL" starfort-mode-token-table))
  (set sym "AGI_CANCL( {param}, {status} )")
  (setplist sym '((class . token) (desc . "Cancel the ADAM device parameter")))

  (setq sym (intern "SLA_DVDV" starfort-mode-token-table))
  (set sym "SLA_DVDV( {va}, {vb} )")
  (setplist sym '((class . token) (desc . "Scalar product of two 3-vectors (double precision)")))

  (setq sym (intern "STARLINK_FORTRAN_77" starfort-mode-token-table))
  (set sym '(("STARLINK_FORTRAN_77" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "IIIQID" starfort-mode-token-table))
  (set sym "IIIQID( {dispid}, {intty}, {intid}, {messag}, {meslen}, {status} )")
  (setplist sym '((class . token) (desc . "Query Interactor Description")))

  (setq sym (intern "ERR_RLSE" starfort-mode-token-table))
  (set sym "ERR_RLSE")
  (setplist sym '((class . token) (desc . "Release (end) the current error context") (helpkey . "ERR_RLSE")))

  (setq sym (intern "EMS_SYSER" starfort-mode-token-table))
  (set sym "EMS_SYSER( {token}, {systat} )")
  (setplist sym '((class . token) (desc . "Assign an operating system error message to a token") (helpkey . "EMS_SYSER")))

  (setq sym (intern "DIY_PROLOGUE" starfort-mode-token-table))
  (set sym '(("DIY_PROLOGUE_ITEM" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "MSG_IFGET" starfort-mode-token-table))
  (set sym "MSG_IFGET( {pname}, {status} )")
  (setplist sym '((class . token) (desc . "Get the filter level for conditional message output from the ADAM parameter system") (helpkey . "MSG_IFGET")))

  (setq sym (intern "TRN__CONIN" starfort-mode-token-table))
  (set sym "TRN__CONIN")
  (setplist sym '((class . token) (desc . "Constant syntax invalid (error code)")))

  (setq sym (intern "DECLARATION_STATEMENT" starfort-mode-token-table))
  (set sym '(("DECLARATION_STATEMENT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "LOCAL_VARIABLES" starfort-mode-token-table))
  (set sym '(("LOCAL_VARIABLES" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NDF_CLONE" starfort-mode-token-table))
  (set sym "NDF_CLONE( {indf1}, {indf2}, {status} )")
  (setplist sym '((class . token) (desc . "Clone an NDF identifier") (helpkey . "NDF_CLONE")))

  (setq sym (intern "ARY_ISTMP" starfort-mode-token-table))
  (set sym "ARY_ISTMP( {iary}, {temp}, {status} )")
  (setplist sym '((class . token) (desc . "Determine if an array is temporary")))

  (setq sym (intern "FIO_SERR" starfort-mode-token-table))
  (set sym "FIO_SERR( {iostat}, {status} )")
  (setplist sym '((class . token) (desc . "Set error status")))

  (setq sym (intern "HDS_STATE" starfort-mode-token-table))
  (set sym "HDS_STATE( {state}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire the current state of HDS")))

  (setq sym (intern "HDS_START" starfort-mode-token-table))
  (set sym "HDS_START( {status} )")
  (setplist sym '((class . token) (desc . "Start up HDS")))

  (setq sym (intern "BLANK" starfort-mode-token-table))
  (set sym "BLANK = {blank_options}")
  (setplist sym '((class . token) (desc . "BLANK = {blank_options}")))

  (setq sym (intern "ROUTINES_USED" starfort-mode-token-table))
  (set sym '(("ROUTINES_USED" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "DAT__FILWR" starfort-mode-token-table))
  (set sym "DAT__FILWR")
  (setplist sym '((class . token) (desc . "File write (error code)")))

  (setq sym (intern "TRN__CONDT" starfort-mode-token-table))
  (set sym "TRN__CONDT")
  (setplist sym '((class . token) (desc . "'Constant scale factor' classification (symbolic constant)")))

  (setq sym (intern "GERHND" starfort-mode-token-table))
  (set sym "GERHND( {errnr}, {fctid}, {errfil} )")
  (setplist sym '((class . token) (desc . "Error handling")))

  (setq sym (intern "DAT_PUT1R" starfort-mode-token-table))
  (set sym "DAT_PUT1R( {loc}, {el}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL data to a vector primitive")))

  (setq sym (intern "DAT_PUT1L" starfort-mode-token-table))
  (set sym "DAT_PUT1L( {loc}, {el}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL data to a vector primitive")))

  (setq sym (intern "GRQLC" starfort-mode-token-table))
  (set sym "GRQLC( {wkid}, {lcdnr}, {stat}, {tnr}, {px}, {py} )")
  (setplist sym '((class . token) (desc . "Request locator")))

  (setq sym (intern "DAT_PUT0R" starfort-mode-token-table))
  (set sym "DAT_PUT0R( {loc}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL data to a scalar primitive")))

  (setq sym (intern "DAT_PUT1I" starfort-mode-token-table))
  (set sym "DAT_PUT1I( {loc}, {el}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER data to a vector primitive")))

  (setq sym (intern "NDF_BEGIN" starfort-mode-token-table))
  (set sym "NDF_BEGIN")
  (setplist sym '((class . token) (desc . "Begin a new NDF context") (helpkey . "NDF_BEGIN")))

  (setq sym (intern "DAT_GET1R" starfort-mode-token-table))
  (set sym "DAT_GET1R( {loc}, {elx}, {rvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read vector primitive as REAL")))

  (setq sym (intern "VAL_LOGUW" starfort-mode-token-table))
  (set sym "VAL_LOGUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of UNSIGNED WORD value")))

  (setq sym (intern "DAT_PUT1D" starfort-mode-token-table))
  (set sym "DAT_PUT1D( {loc}, {el}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION data to a vector primitive")))

  (setq sym (intern "DAT_PUT0L" starfort-mode-token-table))
  (set sym "DAT_PUT0L( {loc}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL data to a scalar primitive")))

  (setq sym (intern "DAT_PUT1C" starfort-mode-token-table))
  (set sym "DAT_PUT1C( {loc}, {el}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER data to a vector primitive")))

  (setq sym (intern "MSG_RENEW" starfort-mode-token-table))
  (set sym "MSG_RENEW")
  (setplist sym '((class . token) (desc . "Renew any annulled message tokens in the current context") (helpkey . "MSG_RENEW")))

  (setq sym (intern "DATA" starfort-mode-token-table))
  (set sym '(("DATA_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "DAT_PUT0I" starfort-mode-token-table))
  (set sym "DAT_PUT0I( {loc}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER data to a scalar primitive")))

  (setq sym (intern "DAT_GET1L" starfort-mode-token-table))
  (set sym "DAT_GET1L( {loc}, {elx}, {lvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read vector primitive as LOGICAL")))

  (setq sym (intern "DAT_GET0R" starfort-mode-token-table))
  (set sym "DAT_GET0R( {loc}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read scalar primitive as REAL")))

  (setq sym (intern "DAT_GET1I" starfort-mode-token-table))
  (set sym "DAT_GET1I( {loc}, {elx}, {ivalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read vector primitive as INTEGER")))

  (setq sym (intern "DAT_PUT0D" starfort-mode-token-table))
  (set sym "DAT_PUT0D( {loc}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION data to a scalar primitive")))

  (setq sym (intern "DAT_PUT0C" starfort-mode-token-table))
  (set sym "DAT_PUT0C( {loc}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER data to a scalar primitive")))

  (setq sym (intern "DAT_GET1D" starfort-mode-token-table))
  (set sym "DAT_GET1D( {loc}, {elx}, {dvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read vector primitive as DOUBLE PRECISION")))

  (setq sym (intern "DAT_GET0L" starfort-mode-token-table))
  (set sym "DAT_GET0L( {loc}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read scalar primitive as LOGICAL")))

  (setq sym (intern "DAT_GET1C" starfort-mode-token-table))
  (set sym "DAT_GET1C( {loc}, {elx}, {cvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read vector primitive as CHARACTER")))

  (setq sym (intern "SLA_EULER" starfort-mode-token-table))
  (set sym "SLA_EULER( {order}, {phi}, {theta}, {psi}, {rmat} )")
  (setplist sym '((class . token) (desc . "Form a rotation matrix from the Euler angles - three successive rotations about specified Cartesian axes.")))

  (setq sym (intern "NUM_DIVUW" starfort-mode-token-table))
  (set sym "NUM_DIVUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one UNSIGNED WORD number by another")))

  (setq sym (intern "DAT_GET0I" starfort-mode-token-table))
  (set sym "DAT_GET0I( {loc}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read scalar primitive as INTEGER")))

  (setq sym (intern "DAT_GET0D" starfort-mode-token-table))
  (set sym "DAT_GET0D( {loc}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read scalar primitive as DOUBLE PRECISION")))

  (setq sym (intern "NDF_CLEN" starfort-mode-token-table))
  (set sym "NDF_CLEN( {indf}, {comp}, {length}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the length of an NDF character component") (helpkey . "NDF_CLEN")))

  (setq sym (intern "DAT_GET0C" starfort-mode-token-table))
  (set sym "DAT_GET0C( {loc}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read scalar primitive as CHARACTER")))

  (setq sym (intern "VAL_LOGUB" starfort-mode-token-table))
  (set sym "VAL_LOGUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of UNSIGNED BYTE value")))

  (setq sym (intern "DAT__NAMIN" starfort-mode-token-table))
  (set sym "DAT__NAMIN")
  (setplist sym '((class . token) (desc . "Name invalid (error code)")))

  (setq sym (intern "CHR_INSET" starfort-mode-token-table))
  (set sym "CHR_INSET( {set}, {string} )")
  (setplist sym '((class . token) (desc . "Determine whether a string is a member of a set")))

  (setq sym (intern "IDI_ASSOC" starfort-mode-token-table))
  (set sym "IDI_ASSOC( {pname}, {acmode}, {dispid}, {status} )")
  (setplist sym '((class . token) (desc . "Open IDI in the ADAM environment")))

  (setq sym (intern "GWM_OPEN" starfort-mode-token-table))
  (set sym "GWM_OPEN( {disply}, {usedef}, {status} )")
  (setplist sym '((class . token) (desc . "Establish the X client-server connection")))

  (setq sym (intern "DAT__FILRD" starfort-mode-token-table))
  (set sym "DAT__FILRD")
  (setplist sym '((class . token) (desc . "File read error (error code)")))

  (setq sym (intern "DAT__DIMIN" starfort-mode-token-table))
  (set sym "DAT__DIMIN")
  (setplist sym '((class . token) (desc . "Dimensions invalid (error code)")))

  (setq sym (intern "DAT__FILPR" starfort-mode-token-table))
  (set sym "DAT__FILPR")
  (setplist sym '((class . token) (desc . "File protected (error code)")))

  (setq sym (intern "NUM_DIVUB" starfort-mode-token-table))
  (set sym "NUM_DIVUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one UNSIGNED BYTE number by another")))

  (setq sym (intern "SLA_MAP" starfort-mode-token-table))
  (set sym "SLA_MAP( {rm}, {dm}, {pr}, {pd}, {px}, {rv}, {eq}, {date}, {ra}, {da} )")
  (setplist sym '((class . token) (desc . "Transform star RA,Dec from mean place to geocentric apparent")))

  (setq sym (intern "NDF_SBB" starfort-mode-token-table))
  (set sym "NDF_SBB( {badbit}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Set a bad-bits mask value for the quality component of an NDF") (helpkey . "NDF_SBB")))

  (setq sym (intern "DAT__FILNX" starfort-mode-token-table))
  (set sym "DAT__FILNX")
  (setplist sym '((class . token) (desc . "File not extended (error code)")))

  (setq sym (intern "NDF_BLOCK" starfort-mode-token-table))
  (set sym "NDF_BLOCK( {indf1}, {ndim}, {mxdim}, {iblock}, {indf2}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain an NDF section containing a block of adjacent pixels") (helpkey . "NDF_BLOCK")))

  (setq sym (intern "NDF_SIZE" starfort-mode-token-table))
  (set sym "NDF_SIZE( {indf}, {npix}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the size of an NDF") (helpkey . "NDF_SIZE")))

  (setq sym (intern "NDF_ASTYP" starfort-mode-token-table))
  (set sym "NDF_ASTYP( {type}, {indf}, {comp}, {iaxis}, {status} )")
  (setplist sym '((class . token) (desc . "Set a new numeric type for an NDF axis array") (helpkey . "NDF_ASTYP")))

  (setq sym (intern "TRN_INV" starfort-mode-token-table))
  (set sym "TRN_INV( {loctr}, {status} )")
  (setplist sym '((class . token) (desc . "Invert transformation")))

  (setq sym (intern "GRQCH" starfort-mode-token-table))
  (set sym "GRQCH( {wkid}, {chdnr}, {stat}, {chnr} )")
  (setplist sym '((class . token) (desc . "Request choice")))

  (setq sym (intern "DAT__FILMP" starfort-mode-token-table))
  (set sym "DAT__FILMP")
  (setplist sym '((class . token) (desc . "File mapping error (error code)")))

  (setq sym (intern "AGI_OPEN" starfort-mode-token-table))
  (set sym "AGI_OPEN( {wkname}, {acmode}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Open an AGI device in a non-ADAM environment")))

  (setq sym (intern "DAT__FILNF" starfort-mode-token-table))
  (set sym "DAT__FILNF")
  (setplist sym '((class . token) (desc . "File not found (error code)")))

  (setq sym (intern "DAT__FILND" starfort-mode-token-table))
  (set sym "DAT__FILND")
  (setplist sym '((class . token) (desc . "File not deleted (error code)")))

  (setq sym (intern "MSG_OUT" starfort-mode-token-table))
  (set sym "MSG_OUT( {param}, {text}, {status} )")
  (setplist sym '((class . token) (desc . "Output a message") (helpkey . "MSG_OUT")))

  (setq sym (intern "VAL_RTOW" starfort-mode-token-table))
  (set sym "VAL_RTOW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL value to WORD")))

  (setq sym (intern "VAL_BTOW" starfort-mode-token-table))
  (set sym "VAL_BTOW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE value to WORD")))

  (setq sym (intern "NDF_QMF" starfort-mode-token-table))
  (set sym "NDF_QMF( {indf}, {qmf}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the logical value of an NDF's quality masking flag") (helpkey . "NDF_QMF")))

  (setq sym (intern "SLA_ETRMS" starfort-mode-token-table))
  (set sym "SLA_ETRMS( {ep}, {ev} )")
  (setplist sym '((class . token) (desc . "Compute the E-terms (elliptic component of annual aberration) vector")))

  (setq sym (intern "PSX_FREE" starfort-mode-token-table))
  (set sym "PSX_FREE( {pntr}, {status} )")
  (setplist sym '((class . token) (desc . "Free virtual memory")))

  (setq sym (intern "VAL_RTOR" starfort-mode-token-table))
  (set sym "VAL_RTOR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL value to REAL")))

  (setq sym (intern "GQPXAD" starfort-mode-token-table))
  (set sym "GQPXAD( {wkid}, {px}, {py}, {qx}, {qy}, {errind}, {n}, {m} )")
  (setplist sym '((class . token) (desc . "Inquire pixel array dimensions")))

  (setq sym (intern "LOGICAL" starfort-mode-token-table))
  (set sym "LOGICAL")
  (setplist sym '((class . token) (desc . "LOGICAL data type")))

  (setq sym (intern "VAL_BTOR" starfort-mode-token-table))
  (set sym "VAL_BTOR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE value to REAL")))

  (setq sym (intern "GRP_INDEX" starfort-mode-token-table))
  (set sym "GRP_INDEX( {name}, {igrp}, {start}, {index}, {status} )")
  (setplist sym '((class . token) (desc . "Searches for a given name and if found, returns its index")))

  (setq sym (intern "MAG_SET" starfort-mode-token-table))
  (set sym "MAG_SET( {td}, {file}, {start}, {block}, {status} )")
  (setplist sym '((class . token) (desc . "Set current tape file/block positions")))

  (setq sym (intern "A_TASK_PROLOGUE" starfort-mode-token-table))
  (set sym "\\*+
\\*{a_task_prologue}
\\*-
\\	{a_task_declarations}
\\*.")
  (setplist sym '((class . token) (desc . "ADAM A-task prologue")))

  (setq sym (intern "VAL_RTOI" starfort-mode-token-table))
  (set sym "VAL_RTOI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL value to INTEGER")))

  (setq sym (intern "VAL_BTOI" starfort-mode-token-table))
  (set sym "VAL_BTOI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE value to INTEGER")))

  (setq sym (intern "HDS_GROUP" starfort-mode-token-table))
  (set sym "HDS_GROUP( {loc}, {group}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire locator group")))

  (setq sym (intern "GRP_DELET" starfort-mode-token-table))
  (set sym "GRP_DELET( {igrp}, {status} )")
  (setplist sym '((class . token) (desc . "Delete a group from the GRP system")))

  (setq sym (intern "VAL_RTOD" starfort-mode-token-table))
  (set sym "VAL_RTOD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL value to DOUBLE PRECISION")))

  (setq sym (intern "VAL_RTOB" starfort-mode-token-table))
  (set sym "VAL_RTOB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL value to BYTE")))

  (setq sym (intern "VAL_BTOD" starfort-mode-token-table))
  (set sym "VAL_BTOD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE value to DOUBLE PRECISION")))

  (setq sym (intern "DAT__FILIN" starfort-mode-token-table))
  (set sym "DAT__FILIN")
  (setplist sym '((class . token) (desc . "File invalid (error code)")))

  (setq sym (intern "VAL_BTOB" starfort-mode-token-table))
  (set sym "VAL_BTOB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE value to BYTE")))

  (setq sym (intern "UNKNOWN" starfort-mode-token-table))
  (set sym "'UNKNOWN'")
  (setplist sym '((class . token) (desc . "'UNKNOWN'")))

  (setq sym (intern "PAR_PUT1R" starfort-mode-token-table))
  (set sym "PAR_PUT1R( {param}, {nval}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL vector parameter values")))

  (setq sym (intern "TRN__MXCLS" starfort-mode-token-table))
  (set sym "TRN__MXCLS")
  (setplist sym '((class . token) (desc . "Max. size of classification array (symbolic constant)")))

  (setq sym (intern "GKS_DEACT" starfort-mode-token-table))
  (set sym "GKS_DEACT( {status} )")
  (setplist sym '((class . token) (desc . "Deactivate ADAM GKS after use by an application")))

  (setq sym (intern "PAR_PUT1L" starfort-mode-token-table))
  (set sym "PAR_PUT1L( {param}, {nval}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL vector parameter values")))

  (setq sym (intern "ARY_TYPE" starfort-mode-token-table))
  (set sym "ARY_TYPE( {iary}, {type}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the numeric type of an array")))

  (setq sym (intern "PAR_PUT0R" starfort-mode-token-table))
  (set sym "PAR_PUT0R( {param}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL scalar parameter value")))

  (setq sym (intern "PAR_PUT1I" starfort-mode-token-table))
  (set sym "PAR_PUT1I( {param}, {nval}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER vector parameter values")))

  (setq sym (intern "GQWKT" starfort-mode-token-table))
  (set sym "GQWKT( {wkid}, {errind}, {tus}, {rwindo}, {cwindo}, {rviewp} )")
  (setplist sym '((class . token) (desc . "Inquire workstation transformation")))

  (setq sym (intern "CHR_LTOC" starfort-mode-token-table))
  (set sym "CHR_LTOC( {lvalue}, {cvalue}, {nchar} )")
  (setplist sym '((class . token) (desc . "Encode a logical value as a character string")))

  (setq sym (intern "GQWKS" starfort-mode-token-table))
  (set sym "GQWKS( {wkid}, {errind}, {state} )")
  (setplist sym '((class . token) (desc . "Inquire workstation state")))

  (setq sym (intern "CHR_DTOC" starfort-mode-token-table))
  (set sym "CHR_DTOC( {dvalue}, {cvalue}, {nchar} )")
  (setplist sym '((class . token) (desc . "Encode a double precision number as a character string")))

  (setq sym (intern "PAR_GET1R" starfort-mode-token-table))
  (set sym "PAR_GET1R( {param}, {maxval}, {rvalue}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Read REAL vector parameter value")))

  (setq sym (intern "PAR_PUT1D" starfort-mode-token-table))
  (set sym "PAR_PUT1D( {param}, {nval}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION vector parameter values")))

  (setq sym (intern "PAR_PUT0L" starfort-mode-token-table))
  (set sym "PAR_PUT0L( {param}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL scalar parameter value")))

  (setq sym (intern "PAR_PUT1C" starfort-mode-token-table))
  (set sym "PAR_PUT1C( {param}, {nval}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER vector parameter values")))

  (setq sym (intern "GQWKM" starfort-mode-token-table))
  (set sym "GQWKM( {errind}, {mxopwk}, {mxacwk}, {mxwkas} )")
  (setplist sym '((class . token) (desc . "Inquire workstation maximum numbers")))

  (setq sym (intern "PAR_PUT0I" starfort-mode-token-table))
  (set sym "PAR_PUT0I( {param}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER scalar parameter value")))

  (setq sym (intern "PAR_GET1L" starfort-mode-token-table))
  (set sym "PAR_GET1L( {param}, {maxval}, {lvalue}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Read LOGICAL vector parameter value")))

  (setq sym (intern "SLA_DSEP" starfort-mode-token-table))
  (set sym "SLA_DSEP( {a1}, {b1}, {a2}, {b2} )")
  (setplist sym '((class . token) (desc . "Angle between two points on a sphere")))

  (setq sym (intern "PAR_GET0R" starfort-mode-token-table))
  (set sym "PAR_GET0R( {param}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read REAL scalar parameter value")))

  (setq sym (intern "PAR_GET1I" starfort-mode-token-table))
  (set sym "PAR_GET1I( {param}, {maxval}, {ivalue}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Read INTEGER vector parameter value")))

  (setq sym (intern "AGI_IBASE" starfort-mode-token-table))
  (set sym "AGI_IBASE( {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire base picture for current device")))

  (setq sym (intern "PAR_PUT0D" starfort-mode-token-table))
  (set sym "PAR_PUT0D( {param}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION scalar parameter value")))

  (setq sym (intern "PAR_PUT0C" starfort-mode-token-table))
  (set sym "PAR_PUT0C( {param}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER scalar parameter value")))

  (setq sym (intern "TRN__TOKIN" starfort-mode-token-table))
  (set sym "TRN__TOKIN")
  (setplist sym '((class . token) (desc . "Token name invalid (error code)")))

  (setq sym (intern "PAR_GET1D" starfort-mode-token-table))
  (set sym "PAR_GET1D( {param}, {maxval}, {dvalue}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Read DOUBLE PRECISION vector parameter value")))

  (setq sym (intern "PAR_GET0L" starfort-mode-token-table))
  (set sym "PAR_GET0L( {param}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read LOGICAL scalar parameter value")))

  (setq sym (intern "ARY_FIND" starfort-mode-token-table))
  (set sym "ARY_FIND( {loc}, {name}, {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Find an array in an HDS structure and import it into the ARY_ system")))

  (setq sym (intern "PAR_GET1C" starfort-mode-token-table))
  (set sym "PAR_GET1C( {param}, {maxval}, {cvalue}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Read CHARACTER vector parameter value")))

  (setq sym (intern "GQWKC" starfort-mode-token-table))
  (set sym "GQWKC( {wkid}, {errind}, {conid}, {wtype} )")
  (setplist sym '((class . token) (desc . "Inquire workstation connection and type")))

  (setq sym (intern "PAR_GET0I" starfort-mode-token-table))
  (set sym "PAR_GET0I( {param}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read INTEGER scalar parameter value")))

  (setq sym (intern "TRN__OPCIN" starfort-mode-token-table))
  (set sym "TRN__OPCIN")
  (setplist sym '((class . token) (desc . "Operation code invalid (error code)")))

  (setq sym (intern "PAR_GET0D" starfort-mode-token-table))
  (set sym "PAR_GET0D( {param}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read DOUBLE PRECISION scalar parameter value")))

  (setq sym (intern "GQPTXR" starfort-mode-token-table))
  (set sym "GQPTXR( {wtype}, {ptxi}, {errind}, {font}, {prec}, {charxp}, {charsp}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire predefined text representation")))

  (setq sym (intern "PAR_GET0C" starfort-mode-token-table))
  (set sym "PAR_GET0C( {param}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read CHARACTER scalar parameter value")))

  (setq sym (intern "NDF_CINP" starfort-mode-token-table))
  (set sym "NDF_CINP( {param}, {indf}, {comp}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain an NDF character component value via the ADAM parameter system") (helpkey . "NDF_CINP")))

  (setq sym (intern "DAT__FILCR" starfort-mode-token-table))
  (set sym "DAT__FILCR")
  (setplist sym '((class . token) (desc . "File create error (error code)")))

  (setq sym (intern "READ_STMT" starfort-mode-token-table))
  (set sym '(("READ_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "DAT__FILCL" starfort-mode-token-table))
  (set sym "DAT__FILCL")
  (setplist sym '((class . token) (desc . "File close error (error code)")))

  (setq sym (intern "NDF_ISTMP" starfort-mode-token-table))
  (set sym "NDF_ISTMP( {indf}, {istmp}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire if an NDF is temporary") (helpkey . "NDF_ISTMP")))

  (setq sym (intern "MAG_REW" starfort-mode-token-table))
  (set sym "MAG_REW( {td}, {status} )")
  (setplist sym '((class . token) (desc . "Rewind a tape")))

  (setq sym (intern "DAT__FILCK" starfort-mode-token-table))
  (set sym "DAT__FILCK")
  (setplist sym '((class . token) (desc . "File locking error (error code)")))

  (setq sym (intern "SLA_KBJ" starfort-mode-token-table))
  (set sym "SLA_KBJ( {jb}, {e}, {k}, {j} )")
  (setplist sym '((class . token) (desc . "Select epoch prefix 'B' or 'J'")))

  (setq sym (intern "GQVLS" starfort-mode-token-table))
  (set sym "GQVLS( {wkid}, {vldnr}, {mldr}, {errind}, {mode}, {esw}, {ival}, {pet}, {earea}, {loval}, {hival}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Inquire valuator device state")))

  (setq sym (intern "PGRECT" starfort-mode-token-table))
  (set sym "PGRECT( {x1}, {x2}, {y1}, {y2} )")
  (setplist sym '((class . token) (desc . "Draw a rectangle, using fill-area attributes")))

  (setq sym (intern "EMS_SETR" starfort-mode-token-table))
  (set sym "EMS_SETR( {token}, {rvalue} )")
  (setplist sym '((class . token) (desc . "Assign a REAL value to a message token (concise)") (helpkey . "EMS_SETR")))

  (setq sym (intern "EMS_SETL" starfort-mode-token-table))
  (set sym "EMS_SETL( {token}, {lvalue} )")
  (setplist sym '((class . token) (desc . "Assign a LOGICAL value to a message token (concise)") (helpkey . "EMS_SETL")))

  (setq sym (intern "EMS_SETI" starfort-mode-token-table))
  (set sym "EMS_SETI( {token}, {ivalue} )")
  (setplist sym '((class . token) (desc . "Assign an INTEGER value to a message token (concise)") (helpkey . "EMS_SETI")))

  (setq sym (intern "PARAMETER" starfort-mode-token-table))
  (set sym '(("PARAMETER_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "EMS_SETD" starfort-mode-token-table))
  (set sym "EMS_SETD( {token}, {dvalue} )")
  (setplist sym '((class . token) (desc . "Assign a DOUBLE PRECISION value to a message token (concise)") (helpkey . "EMS_SETD")))

  (setq sym (intern "EMS_SETC" starfort-mode-token-table))
  (set sym "EMS_SETC( {token}, {cvalue} )")
  (setplist sym '((class . token) (desc . "Assign a CHARACTER value to a message token (concise)") (helpkey . "EMS_SETC")))

  (setq sym (intern "VAL_MODUW" starfort-mode-token-table))
  (set sym "VAL_MODUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two UNSIGNED WORD values")))

  (setq sym (intern "TRN__NVRIN" starfort-mode-token-table))
  (set sym "TRN__NVRIN")
  (setplist sym '((class . token) (desc . "Number of variables invalid (error code)")))

  (setq sym (intern "GQTXX" starfort-mode-token-table))
  (set sym "GQTXX( {wkid}, {px}, {py}, {str}, {errind}, {cpx}, {cpy}, {txexpx}, {txexpy} )")
  (setplist sym '((class . token) (desc . "Inquire text extent")))

  (setq sym (intern "MAG_POS" starfort-mode-token-table))
  (set sym "MAG_POS( {td}, {file}, {start}, {block}, {moved}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire current tape file/block position")))

  (setq sym (intern "GQTXR" starfort-mode-token-table))
  (set sym "GQTXR( {wkid}, {txi}, {gks$type}, {errind}, {font}, {prec}, {charxp}, {charsp}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire text representation")))

  (setq sym (intern "GQTXP" starfort-mode-token-table))
  (set sym "GQTXP( {errind}, {txp} )")
  (setplist sym '((class . token) (desc . "Inquire text path")))

  (setq sym (intern "GQTXI" starfort-mode-token-table))
  (set sym "GQTXI( {errind}, {index} )")
  (setplist sym '((class . token) (desc . "Inquire text index")))

  (setq sym (intern "VAL_MODUB" starfort-mode-token-table))
  (set sym "VAL_MODUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two UNSIGNED BYTE values")))

  (setq sym (intern "GQTXF" starfort-mode-token-table))
  (set sym "GQTXF( {wtype}, {n}, {errind}, {nfpp}, {font}, {prec}, {nchh}, {minchh}, {maxchh}, {nchx}, {minchx}, {maxchx}, {nptxi} )")
  (setplist sym '((class . token) (desc . "Inquire text facilities")))

  (setq sym (intern "NDF_ASSOC" starfort-mode-token-table))
  (set sym "NDF_ASSOC( {param}, {mode}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Associate an existing NDF with an ADAM parameter") (helpkey . "NDF_ASSOC")))

  (setq sym (intern "EMS_FIOER" starfort-mode-token-table))
  (set sym "EMS_FIOER( {token}, {iostat} )")
  (setplist sym '((class . token) (desc . "Assign a Fortran I/O error message to a token") (helpkey . "EMS_FIOER")))

  (setq sym (intern "NDF_XSTAT" starfort-mode-token-table))
  (set sym "NDF_XSTAT( {indf}, {xname}, {there}, {status} )")
  (setplist sym '((class . token) (desc . "Determine if a named NDF extension exists") (helpkey . "NDF_XSTAT")))

  (setq sym (intern "DAT_DREP" starfort-mode-token-table))
  (set sym "DAT_DREP( {loc}, {format}, {order}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain primitive data representation information")))

  (setq sym (intern "VEC_LG10W" starfort-mode-token-table))
  (set sym "VEC_LG10W( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of WORD vectorised array")))

  (setq sym (intern "DAT_NAME" starfort-mode-token-table))
  (set sym "DAT_NAME( {loc}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire object name")))

  (setq sym (intern "VEC_LG10R" starfort-mode-token-table))
  (set sym "VEC_LG10R( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of REAL vectorised array")))

  (setq sym (intern "PGBBUF" starfort-mode-token-table))
  (set sym "PGBBUF")
  (setplist sym '((class . token) (desc . "Begin batch of output (buffer)")))

  (setq sym (intern "PSX_GETUID" starfort-mode-token-table))
  (set sym "PSX_GETUID( {uid}, {status} )")
  (setplist sym '((class . token) (desc . "Gets the real user ID")))

  (setq sym (intern "NDF_ASTAT" starfort-mode-token-table))
  (set sym "NDF_ASTAT( {indf}, {comp}, {iaxis}, {state}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the state of an NDF axis component (defined or undefined)") (helpkey . "NDF_ASTAT")))

  (setq sym (intern "VEC_LG10I" starfort-mode-token-table))
  (set sym "VEC_LG10I( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of INTEGER vectorised array")))

  (setq sym (intern "DAT_RENAM" starfort-mode-token-table))
  (set sym "DAT_RENAM( {loc}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Rename object")))

  (setq sym (intern "VEC_LG10D" starfort-mode-token-table))
  (set sym "VEC_LG10D( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of DOUBLE PRECISION vectorised array")))

  (setq sym (intern "VEC_LG10B" starfort-mode-token-table))
  (set sym "VEC_LG10B( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of BYTE vectorised array")))

  (setq sym (intern "GSFAR" starfort-mode-token-table))
  (set sym "GSFAR( {wkid}, {fai}, {gks$ints}, {styli}, {coli} )")
  (setplist sym '((class . token) (desc . "Set fill area representation")))

  (setq sym (intern "ERR_BEGIN" starfort-mode-token-table))
  (set sym "ERR_BEGIN( {status} )")
  (setplist sym '((class . token) (desc . "Create a new error reporting environment") (helpkey . "ERR_BEGIN")))

  (setq sym (intern "EMS_RLSE" starfort-mode-token-table))
  (set sym "EMS_RLSE")
  (setplist sym '((class . token) (desc . "Release (end) an error context") (helpkey . "EMS_RLSE")))

  (setq sym (intern "NUM_SQRTW" starfort-mode-token-table))
  (set sym "NUM_SQRTW( {num} )")
  (setplist sym '((class . token) (desc . "Square root of WORD number")))

  (setq sym (intern "NUM_IDVW" starfort-mode-token-table))
  (set sym "NUM_IDVW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one WORD number by another")))

  (setq sym (intern "GSFAI" starfort-mode-token-table))
  (set sym "GSFAI( {fai} )")
  (setplist sym '((class . token) (desc . "Set fill area index")))

  (setq sym (intern "NUM_SQRTR" starfort-mode-token-table))
  (set sym "NUM_SQRTR( {num} )")
  (setplist sym '((class . token) (desc . "Square root of REAL number")))

  (setq sym (intern "VEC_ADDUW" starfort-mode-token-table))
  (set sym "VEC_ADDUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Add two UNSIGNED WORD vectorised arrays")))

  (setq sym (intern "DIRECT" starfort-mode-token-table))
  (set sym "'DIRECT'")
  (setplist sym '((class . token) (desc . "'DIRECT'")))

  (setq sym (intern "NUM_IDVR" starfort-mode-token-table))
  (set sym "NUM_IDVR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one REAL number by another")))

  (setq sym (intern "NUM_SQRTI" starfort-mode-token-table))
  (set sym "NUM_SQRTI( {num} )")
  (setplist sym '((class . token) (desc . "Square root of INTEGER number")))

  (setq sym (intern "NUM_IDVI" starfort-mode-token-table))
  (set sym "NUM_IDVI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one INTEGER number by another")))

  (setq sym (intern "NUM_SQRTD" starfort-mode-token-table))
  (set sym "NUM_SQRTD( {num} )")
  (setplist sym '((class . token) (desc . "Square root of DOUBLE PRECISION number")))

  (setq sym (intern "NDF_MSG" starfort-mode-token-table))
  (set sym "NDF_MSG( {token}, {indf} )")
  (setplist sym '((class . token) (desc . "Assign the name of an NDF to a message token") (helpkey . "NDF_MSG")))

  (setq sym (intern "NUM_SQRTB" starfort-mode-token-table))
  (set sym "NUM_SQRTB( {num} )")
  (setplist sym '((class . token) (desc . "Square root of BYTE number")))

  (setq sym (intern "SLA_REFRO" starfort-mode-token-table))
  (set sym "SLA_REFRO( {zobs}, {hm}, {tdk}, {pmb}, {rh}, {wl}, {phi}, {tlr}, {eps}, {ref} )")
  (setplist sym '((class . token) (desc . "Atmospheric refraction for radio and optical wavelengths")))

  (setq sym (intern "NUM_IDVD" starfort-mode-token-table))
  (set sym "NUM_IDVD( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one DOUBLE PRECISION number by another")))

  (setq sym (intern "MAG_DISM" starfort-mode-token-table))
  (set sym "MAG_DISM( {param}, {unload}, {status} )")
  (setplist sym '((class . token) (desc . "Dismount a tape on a drive")))

  (setq sym (intern "TRN_ERR" starfort-mode-token-table))
  (set sym "TRN_ERR")
  (setplist sym '((class . token) (desc . "TRN__ error code definitions (include file)")))

  (setq sym (intern "NUM_IDVB" starfort-mode-token-table))
  (set sym "NUM_IDVB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one BYTE number by another")))

  (setq sym (intern "NDF_QMASK" starfort-mode-token-table))
  (set sym "NDF_QMASK( {qual}, {badbit} )")
  (setplist sym '((class . token) (desc . "Combine an NDF quality value with a bad-bits mask to give a logical result") (helpkey . "NDF_QMASK")))

  (setq sym (intern "BLOCK_DATA_OPTIONS" starfort-mode-token-table))
  (set sym "[notes]
[side_effects]
[deficiencies]
[machine_specifics]
[DIY_prologue_item]...
[references]
[keywords]
[copyright]")
  (setplist sym '((class . token) (desc . "Expanded list of all optional items")))

  (setq sym (intern "VEC_ADDUB" starfort-mode-token-table))
  (set sym "VEC_ADDUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Add two UNSIGNED BYTE vectorised arrays")))

  (setq sym (intern "SLA_PRECES" starfort-mode-token-table))
  (set sym "SLA_PRECES( {system}, {ep0}, {ep1}, {ra}, {dc} )")
  (setplist sym '((class . token) (desc . "Precession - either FK4 (Bessel-Newcomb, pre-IAU1976) or FK5 (Fricke, post-IAU1976) as required.")))

  (setq sym (intern "VEC_UBTOW" starfort-mode-token-table))
  (set sym "VEC_UBTOW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE vectorised array to WORD")))

  (setq sym (intern "GQSTS" starfort-mode-token-table))
  (set sym "GQSTS( {wkid}, {stdnr}, {mldr}, {errind}, {mode}, {esw}, {lostr}, {istr}, {pet}, {earea}, {buflen}, {inipos}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Inquire string device state")))

  (setq sym (intern "ARY_NEWP" starfort-mode-token-table))
  (set sym "ARY_NEWP( {ftype}, {ndim}, {ubnd}, {place}, {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Create a new primitive array")))

  (setq sym (intern "VEC_UBTOR" starfort-mode-token-table))
  (set sym "VEC_UBTOR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE vectorised array to REAL")))

  (setq sym (intern "VEC_UBTOI" starfort-mode-token-table))
  (set sym "VEC_UBTOI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE vectorised array to INTEGER")))

  (setq sym (intern "HDS_RUN" starfort-mode-token-table))
  (set sym "HDS_RUN( {app}, {status} )")
  (setplist sym '((class . token) (desc . "Run an HDS application subroutine")))

  (setq sym (intern "NDF_NEW" starfort-mode-token-table))
  (set sym "NDF_NEW( {ftype}, {ndim}, {lbnd}, {ubnd}, {place}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Create a new simple NDF") (helpkey . "NDF_NEW")))

  (setq sym (intern "VEC_UBTOD" starfort-mode-token-table))
  (set sym "VEC_UBTOD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE vectorised array to DOUBLE PRECISION")))

  (setq sym (intern "VEC_UBTOB" starfort-mode-token-table))
  (set sym "VEC_UBTOB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE vectorised array to BYTE")))

  (setq sym (intern "AGI_PDEL" starfort-mode-token-table))
  (set sym "AGI_PDEL( {status} )")
  (setplist sym '((class . token) (desc . "Delete all the pictures on the current device")))

  (setq sym (intern "NUM_WTOW" starfort-mode-token-table))
  (set sym "NUM_WTOW( {num} )")
  (setplist sym '((class . token) (desc . "Convert a WORD number to WORD")))

  (setq sym (intern "NUM_WTOR" starfort-mode-token-table))
  (set sym "NUM_WTOR( {num} )")
  (setplist sym '((class . token) (desc . "Convert a WORD number to REAL")))

  (setq sym (intern "VEC_ABSUW" starfort-mode-token-table))
  (set sym "VEC_ABSUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of UNSIGNED WORD vectorised array")))

  (setq sym (intern "NUM_WTOI" starfort-mode-token-table))
  (set sym "NUM_WTOI( {num} )")
  (setplist sym '((class . token) (desc . "Convert a WORD number to INTEGER")))

  (setq sym (intern "NDF_CGET" starfort-mode-token-table))
  (set sym "NDF_CGET( {indf}, {comp}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the value of an NDF character component") (helpkey . "NDF_CGET")))

  (setq sym (intern "NUM_WTOD" starfort-mode-token-table))
  (set sym "NUM_WTOD( {num} )")
  (setplist sym '((class . token) (desc . "Convert a WORD number to DOUBLE PRECISION")))

  (setq sym (intern "DAT_INDEX" starfort-mode-token-table))
  (set sym "DAT_INDEX( {loc1}, {index}, {loc2}, {status} )")
  (setplist sym '((class . token) (desc . "Index into component list")))

  (setq sym (intern "NUM_WTOB" starfort-mode-token-table))
  (set sym "NUM_WTOB( {num} )")
  (setplist sym '((class . token) (desc . "Convert a WORD number to BYTE")))

  (setq sym (intern "NDF_ACPUT" starfort-mode-token-table))
  (set sym "NDF_ACPUT( {value}, {indf}, {comp}, {iaxis}, {status} )")
  (setplist sym '((class . token) (desc . "Assign a value to an NDF axis character component") (helpkey . "NDF_ACPUT")))

  (setq sym (intern "SLA_DMXV" starfort-mode-token-table))
  (set sym "SLA_DMXV( {dm}, {va}, {vb} )")
  (setplist sym '((class . token) (desc . "Performs the 3-D forward unitary transformation: vector VB = matrix DM vector VA")))

  (setq sym (intern "DAT_STRUC" starfort-mode-token-table))
  (set sym "DAT_STRUC( {loc}, {reply}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire object structure")))

  (setq sym (intern "DAT_DELET" starfort-mode-token-table))
  (set sym "DAT_DELET( {param}, {status} )")
  (setplist sym '((class . token) (desc . "Delete an object via the ADAM parameter system")))

  (setq sym (intern "VEC_ABSUB" starfort-mode-token-table))
  (set sym "VEC_ABSUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of UNSIGNED BYTE vectorised array")))

  (setq sym (intern "DAT_ALTER" starfort-mode-token-table))
  (set sym "DAT_ALTER( {loc}, {ndim}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Alter object size")))

  (setq sym (intern "SLA_DMXM" starfort-mode-token-table))
  (set sym "SLA_DMXM( {a}, {b}, {c} )")
  (setplist sym '((class . token) (desc . "Product of two 3x3 matrices: matrix C = matrix A x matrix B")))

  (setq sym (intern "PGQINF" starfort-mode-token-table))
  (set sym "PGQINF( {item}, {value}, {length} )")
  (setplist sym '((class . token) (desc . "Inquire PGPLOT general information")))

  (setq sym (intern "GWM_EXIST" starfort-mode-token-table))
  (set sym "GWM_EXIST( {wname}, {exists}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire if a GWM window of the given name exists")))

  (setq sym (intern "KEEP" starfort-mode-token-table))
  (set sym "'KEEP'")
  (setplist sym '((class . token) (desc . "'KEEP'")))

  (setq sym (intern "ARY_DUPE" starfort-mode-token-table))
  (set sym "ARY_DUPE( {iary1}, {place}, {iary2}, {status} )")
  (setplist sym '((class . token) (desc . "Duplicate an array")))

  (setq sym (intern "GQSKS" starfort-mode-token-table))
  (set sym "GQSKS( {wkid}, {skdnr}, {gks$type}, {n}, {mldr}, {errind}, {mode}, {esw}, {itnr}, {no}, {px}, {py}, {pet}, {earea}, {buflen}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Inquire stroke device state")))

  (setq sym (intern "TRN_JOIN" starfort-mode-token-table))
  (set sym "TRN_JOIN( {loctr1}, {loctr2}, {eloc}, {name}, {loctr}, {status} )")
  (setplist sym '((class . token) (desc . "Concatenate transformations")))

  (setq sym (intern "AGI_IPOBS" starfort-mode-token-table))
  (set sym "AGI_IPOBS( {picid}, {lobs}, {status} )")
  (setplist sym '((class . token) (desc . "Is current picture obscured by another?")))

  (setq sym (intern "SLA_EVP" starfort-mode-token-table))
  (set sym "SLA_EVP( {date}, {deqx}, {dvb}, {dpb}, {dvh}, {dph} )")
  (setplist sym '((class . token) (desc . "Barycentric and heliocentric velocity and position of the Earth")))

  (setq sym (intern "NDF_LOC" starfort-mode-token-table))
  (set sym "NDF_LOC( {indf}, {mode}, {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain an HDS locator for an NDF") (helpkey . "NDF_LOC")))

  (setq sym (intern "GRP_PUT" starfort-mode-token-table))
  (set sym "GRP_PUT( {igrp}, {size}, {names}, {index}, {status} )")
  (setplist sym '((class . token) (desc . "Put a given set of literal names into a group")))

  (setq sym (intern "CARRIAGECONTROL_PAR" starfort-mode-token-table))
  (set sym '(("CARRIAGECONTROL_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "EXIST_PAR" starfort-mode-token-table))
  (set sym '(("EXIST_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "GSCHM" starfort-mode-token-table))
  (set sym "GSCHM( {wkid}, {idnr}, {gks$mode}, {gks$esw} )")
  (setplist sym '((class . token) (desc . "Set choice mode")))

  (setq sym (intern "VEC_ITOW" starfort-mode-token-table))
  (set sym "VEC_ITOW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER vectorised array to WORD")))

  (setq sym (intern "GSCHH" starfort-mode-token-table))
  (set sym "GSCHH( {chh} )")
  (setplist sym '((class . token) (desc . "Set character height")))

  (setq sym (intern "GQSGP" starfort-mode-token-table))
  (set sym "GQSGP( {wtype}, {errind}, {nsg} )")
  (setplist sym '((class . token) (desc . "Inquire number of segment priorities supported")))

  (setq sym (intern "SLA_REFCO" starfort-mode-token-table))
  (set sym "SLA_REFCO( {hm}, {tdk}, {pmb}, {rh}, {wl}, {phi}, {tlr}, {eps}, {refa}, {refb} )")
  (setplist sym '((class . token) (desc . "Determine constants A and B in atmospheric refraction model dZ = A tan Z + B tan3 Z.")))

  (setq sym (intern "PGPOINT" starfort-mode-token-table))
  (set sym "PGPOINT( {n}, {xpts}, {ypts}, {symbol} )")
  (setplist sym '((class . token) (desc . "Draw one or more graph markers")))

  (setq sym (intern "VEC_ITOR" starfort-mode-token-table))
  (set sym "VEC_ITOR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER vectorised array to REAL")))

  (setq sym (intern "NUM_ADDW" starfort-mode-token-table))
  (set sym "NUM_ADDW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Add two WORD numbers")))

  (setq sym (intern "VEC_ITOI" starfort-mode-token-table))
  (set sym "VEC_ITOI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER vectorised array to INTEGER")))

  (setq sym (intern "MAG_ANNUL" starfort-mode-token-table))
  (set sym "MAG_ANNUL( {td}, {status} )")
  (setplist sym '((class . token) (desc . "Annul tape descriptor, releasing any associated tape drive")))

  (setq sym (intern "NUM_ADDR" starfort-mode-token-table))
  (set sym "NUM_ADDR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Add two REAL numbers")))

  (setq sym (intern "NDF_MAP" starfort-mode-token-table))
  (set sym "NDF_MAP( {indf}, {comp}, {type}, {mmod}, {pntr}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain mapped access to an array component of an NDF") (helpkey . "NDF_MAP")))

  (setq sym (intern "GQSGA" starfort-mode-token-table))
  (set sym "GQSGA( {sgna}, {errind}, {segtm}, {vis}, {high}, {sgpr}, {det} )")
  (setplist sym '((class . token) (desc . "Inquire segment attributes")))

  (setq sym (intern "NUM_ABSW" starfort-mode-token-table))
  (set sym "NUM_ABSW( {num} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of WORD number")))

  (setq sym (intern "VEC_ITOD" starfort-mode-token-table))
  (set sym "VEC_ITOD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER vectorised array to DOUBLE PRECISION")))

  (setq sym (intern "TRN__NMVMM" starfort-mode-token-table))
  (set sym "TRN__NMVMM")
  (setplist sym '((class . token) (desc . "Number of module variables mis-matched (error code)")))

  (setq sym (intern "SLA_DTP2S" starfort-mode-token-table))
  (set sym "SLA_DTP2S( {xi}, {eta}, {raz}, {decz}, {ra}, {dec} )")
  (setplist sym '((class . token) (desc . "Transform tangent plane coordinates into spherical")))

  (setq sym (intern "WRITE" starfort-mode-token-table))
  (set sym "WRITE ( {ifile}, {ifmt}, [iwrite_parameter]... ) [io_elm]...")
  (setplist sym '((class . token) (desc . "Internal WRITE statement")))

  (setq sym (intern "VEC_ITOB" starfort-mode-token-table))
  (set sym "VEC_ITOB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER vectorised array to BYTE")))

  (setq sym (intern "NUM_ABSR" starfort-mode-token-table))
  (set sym "NUM_ABSR( {num} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of REAL number")))

  (setq sym (intern "NUM_ADDI" starfort-mode-token-table))
  (set sym "NUM_ADDI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Add two INTEGER numbers")))

  (setq sym (intern "TRN_CLOSE" starfort-mode-token-table))
  (set sym "TRN_CLOSE( {status} )")
  (setplist sym '((class . token) (desc . "Close the TRANSFORM facility")))

  (setq sym (intern "NUM_ADDD" starfort-mode-token-table))
  (set sym "NUM_ADDD( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Add two DOUBLE PRECISION numbers")))

  (setq sym (intern "NUM_ADDB" starfort-mode-token-table))
  (set sym "NUM_ADDB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Add two BYTE numbers")))

  (setq sym (intern "NUM_ABSI" starfort-mode-token-table))
  (set sym "NUM_ABSI( {num} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of INTEGER number")))

  (setq sym (intern "SLA_EPJ" starfort-mode-token-table))
  (set sym "SLA_EPJ( {date} )")
  (setplist sym '((class . token) (desc . "Conversion of Modified Julian Date to Julian Epoch")))

  (setq sym (intern "NUM_ABSD" starfort-mode-token-table))
  (set sym "NUM_ABSD( {num} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of DOUBLE PRECISION number")))

  (setq sym (intern "NUM_ABSB" starfort-mode-token-table))
  (set sym "NUM_ABSB( {num} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of BYTE number")))

  (setq sym (intern "GRP_OWN" starfort-mode-token-table))
  (set sym "GRP_OWN( {igrp1}, {igrp2}, {status} )")
  (setplist sym '((class . token) (desc . "Returns the identifier of the group which owns the specified group")))

  (setq sym (intern "SLA_EPB" starfort-mode-token-table))
  (set sym "SLA_EPB( {date} )")
  (setplist sym '((class . token) (desc . "Conversion of Modified Julian Date to Besselian Epoch")))

  (setq sym (intern "GSASF" starfort-mode-token-table))
  (set sym "GSASF( {asfs} )")
  (setplist sym '((class . token) (desc . "Set aspect source flags")))

  (setq sym (intern "SLA_DVN" starfort-mode-token-table))
  (set sym "SLA_DVN( {v}, {uv}, {vm} )")
  (setplist sym '((class . token) (desc . "Normalises a 3-vector also giving the modulus")))

  (setq sym (intern "A_TASK_OPTIONS" starfort-mode-token-table))
  (set sym "[pitfalls]
[notes]
[prior_requirements]
[side_effects]
[algorithm]
[accuracy]
[timing]
[implementation_status]
[routines_used]
[deficiencies]
[machine_specifics]
[DIY_prologue_item]...
[references]
[keywords]
[copyright]")
  (setplist sym '((class . token) (desc . "Expanded list of all optional items")))

  (setq sym (intern "ARGUMENTS_GIVEN_AND_RETURNED" starfort-mode-token-table))
  (set sym '(("ARGUMENTS_GIVEN_AND_RETURNED" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SLA_DTT" starfort-mode-token-table))
  (set sym "SLA_DTT( {utc} )")
  (setplist sym '((class . token) (desc . "Increment to be applied to Coordinated Universal Time UTC to give Terrestrial Dynamical Time TDT (formerly Ephemeris Time ET)")))

  (setq sym (intern "SLA_TP2S" starfort-mode-token-table))
  (set sym "SLA_TP2S( {xi}, {eta}, {raz}, {decz}, {ra}, {dec} )")
  (setplist sym '((class . token) (desc . "Transform tangent plane coordinates into spherical")))

  (setq sym (intern "TRN_STOKR" starfort-mode-token-table))
  (set sym "TRN_STOKR( {token}, {rvalue}, {text}, {nsubs}, {status} )")
  (setplist sym '((class . token) (desc . "Substitute a REAL value for a token")))

  (setq sym (intern "PRINT" starfort-mode-token-table))
  (set sym "PRINT {print_fmt}, [print_io_elm]...")
  (setplist sym '((class . token) (desc . "PRINT statement")))

  (setq sym (intern "AGI_TWTOD" starfort-mode-token-table))
  (set sym "AGI_TWTOD( {picid}, {nxy}, {wx}, {wy}, {dx}, {dy}, {status} )")
  (setplist sym '((class . token) (desc . "Transform world to data coordinates")))

  (setq sym (intern "SGS_ZPART" starfort-mode-token-table))
  (set sym "SGS_ZPART( {nx}, {ny}, {izones}, {status} )")
  (setplist sym '((class . token) (desc . "Partition a zone")))

  (setq sym (intern "TRN_GTNVC" starfort-mode-token-table))
  (set sym "TRN_GTNVC( {id}, {nvin}, {nvout}, {status} )")
  (setplist sym '((class . token) (desc . "Get numbers of compiled variables")))

  (setq sym (intern "TRN_STOKI" starfort-mode-token-table))
  (set sym "TRN_STOKI( {token}, {ivalue}, {text}, {nsubs}, {status} )")
  (setplist sym '((class . token) (desc . "Substitute a INTEGER value for a token")))

  (setq sym (intern "DAT_VEC" starfort-mode-token-table))
  (set sym "DAT_VEC( {loc1}, {loc2}, {status} )")
  (setplist sym '((class . token) (desc . "Vectorise object")))

  (setq sym (intern "GQPXA" starfort-mode-token-table))
  (set sym "GQPXA( {wkid}, {px}, {py}, {dx}, {dy}, {dimx}, {errind}, {invval}, {colia} )")
  (setplist sym '((class . token) (desc . "Inquire pixel array")))

  (setq sym (intern "TRN_STOKD" starfort-mode-token-table))
  (set sym "TRN_STOKD( {token}, {dvalue}, {text}, {nsubs}, {status} )")
  (setplist sym '((class . token) (desc . "Substitute a DOUBLE PRECISION value for a token")))

  (setq sym (intern "SGS_ENSCU" starfort-mode-token-table))
  (set sym "SGS_ENSCU")
  (setplist sym '((class . token) (desc . "Enable sample cursor")))

  (setq sym (intern "NDF_SECT" starfort-mode-token-table))
  (set sym "NDF_SECT( {indf1}, {ndim}, {lbnd}, {ubnd}, {indf2}, {status} )")
  (setplist sym '((class . token) (desc . "Create an NDF section") (helpkey . "NDF_SECT")))

  (setq sym (intern "ARY_NDIM" starfort-mode-token-table))
  (set sym "ARY_NDIM( {iary}, {ndim}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire the dimensionality of an array")))

  (setq sym (intern "GQPPMR" starfort-mode-token-table))
  (set sym "GQPPMR( {wtype}, {pmi}, {errind}, {mktype}, {mksscf}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire predefined polymarker representation")))

  (setq sym (intern "PGPOLY" starfort-mode-token-table))
  (set sym "PGPOLY( {n}, {xpts}, {ypts} )")
  (setplist sym '((class . token) (desc . "Fill a polygonal area with shading")))

  (setq sym (intern "CMP_MAPV" starfort-mode-token-table))
  (set sym "CMP_MAPV( {loc}, {name}, {type}, {mode}, {pntr}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Map vectorised component")))

  (setq sym (intern "GQPPLR" starfort-mode-token-table))
  (set sym "GQPPLR( {wtype}, {pli}, {errind}, {lntype}, {lwidth}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire predefined polyline representation")))

  (setq sym (intern "CMP_MAPN" starfort-mode-token-table))
  (set sym "CMP_MAPN( {loc}, {name}, {type}, {mode}, {ndim}, {pntr}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Map array component")))

  (setq sym (intern "SAVE" starfort-mode-token-table))
  (set sym '(("SAVE_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "FIO_PUNIT" starfort-mode-token-table))
  (set sym "FIO_PUNIT( {unit}, {status} )")
  (setplist sym '((class . token) (desc . "Release a unit number")))

  (setq sym (intern "NUM_SIGNUW" starfort-mode-token-table))
  (set sym "NUM_SIGNUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one UNSIGNED WORD number to another")))

  (setq sym (intern "ARGUMENTS" starfort-mode-token-table))
  (set sym '(("ARGUMENTS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NDF_ASNRM" starfort-mode-token-table))
  (set sym "NDF_ASNRM( {norm}, {indf}, {iaxis}, {status} )")
  (setplist sym '((class . token) (desc . "Set a new logical value for an NDF axis normalisation flag") (helpkey . "NDF_ASNRM")))

  (setq sym (intern "IIIGSE" starfort-mode-token-table))
  (set sym "IIIGSE( {dispid}, {neval}, {string}, {slen}, {status} )")
  (setplist sym '((class . token) (desc . "Get String Evaluator")))

  (setq sym (intern "FIO_GUNIT" starfort-mode-token-table))
  (set sym "FIO_GUNIT( {unit}, {status} )")
  (setplist sym '((class . token) (desc . "Get a unit number")))

  (setq sym (intern "PSX_GETPPID" starfort-mode-token-table))
  (set sym "PSX_GETPPID( {pid}, {status} )")
  (setplist sym '((class . token) (desc . "Gets the process ID of the parent process")))

  (setq sym (intern "ARY_ISMAP" starfort-mode-token-table))
  (set sym "ARY_ISMAP( {iary}, {mapped}, {status} )")
  (setplist sym '((class . token) (desc . "Determine if an array is currently mapped")))

  (setq sym (intern "IIIGRE" starfort-mode-token-table))
  (set sym "IIIGRE( {dispid}, {neval}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Get Real Evaluator")))

  (setq sym (intern "VEC_MAXUW" starfort-mode-token-table))
  (set sym "VEC_MAXUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two UNSIGNED WORD vectorised arrays")))

  (setq sym (intern "NUM_SIGNUB" starfort-mode-token-table))
  (set sym "NUM_SIGNUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one UNSIGNED BYTE number to another")))

  (setq sym (intern "DAT__BOUND" starfort-mode-token-table))
  (set sym "DAT__BOUND")
  (setplist sym '((class . token) (desc . "Outside object bounds (error code)")))

  (setq sym (intern "NUM_MINUW" starfort-mode-token-table))
  (set sym "NUM_MINUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Minimum of two UNSIGNED WORD numbers")))

  (setq sym (intern "ERR_REP" starfort-mode-token-table))
  (set sym "ERR_REP( {param}, {text}, {status} )")
  (setplist sym '((class . token) (desc . "Report an error message") (helpkey . "ERR_REP")))

  (setq sym (intern "TRN__DUVAR" starfort-mode-token-table))
  (set sym "TRN__DUVAR")
  (setplist sym '((class . token) (desc . "Duplicate variable name (error code)")))

  (setq sym (intern "NUM_NINTW" starfort-mode-token-table))
  (set sym "NUM_NINTW( {num} )")
  (setplist sym '((class . token) (desc . "Nearest integer to WORD number")))

  (setq sym (intern "USAGE" starfort-mode-token-table))
  (set sym '(("USAGE" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "FIO_REP" starfort-mode-token-table))
  (set sym "FIO_REP( {unit}, {fname}, {iostat}, {mess}, {status} )")
  (setplist sym '((class . token) (desc . "Report error from FORTRAN I/O statements")))

  (setq sym (intern "NUM_NINTR" starfort-mode-token-table))
  (set sym "NUM_NINTR( {num} )")
  (setplist sym '((class . token) (desc . "Nearest integer to REAL number")))

  (setq sym (intern "CHR_SWAP" starfort-mode-token-table))
  (set sym "CHR_SWAP( {v1}, {v2} )")
  (setplist sym '((class . token) (desc . "Swap two single-character variables")))

  (setq sym (intern "SLA_DMAT" starfort-mode-token-table))
  (set sym "SLA_DMAT( {n}, {a}, {y}, {d}, {jf}, {iw} )")
  (setplist sym '((class . token) (desc . "Matrix inversion & solution of simultaneous equations")))

  (setq sym (intern "VEC_MAXUB" starfort-mode-token-table))
  (set sym "VEC_MAXUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two UNSIGNED BYTE vectorised arrays")))

  (setq sym (intern "SLA_EG50" starfort-mode-token-table))
  (set sym "SLA_EG50( {dr}, {dd}, {dl}, {db} )")
  (setplist sym '((class . token) (desc . "Transformation from B1950.0 'FK4' equatorial coordinates to IAU 1958 galactic coordinates")))

  (setq sym (intern "NUM_NINTI" starfort-mode-token-table))
  (set sym "NUM_NINTI( {num} )")
  (setplist sym '((class . token) (desc . "Nearest integer to INTEGER number")))

  (setq sym (intern "NUM_MINUB" starfort-mode-token-table))
  (set sym "NUM_MINUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Minimum of two UNSIGNED BYTE numbers")))

  (setq sym (intern "COMPLEX" starfort-mode-token-table))
  (set sym "COMPLEX")
  (setplist sym '((class . token) (desc . "COMPLEX data type")))

  (setq sym (intern "NUM_NINTD" starfort-mode-token-table))
  (set sym "NUM_NINTD( {num} )")
  (setplist sym '((class . token) (desc . "Nearest integer to DOUBLE PRECISION number")))

  (setq sym (intern "NUM_NINTB" starfort-mode-token-table))
  (set sym "NUM_NINTB( {num} )")
  (setplist sym '((class . token) (desc . "Nearest integer to BYTE number")))

  (setq sym (intern "GQPMR" starfort-mode-token-table))
  (set sym "GQPMR( {wkid}, {pmi}, {gks$type}, {errind}, {mktype}, {mksscf}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire polymarker representation")))

  (setq sym (intern "AGI_TWTDD" starfort-mode-token-table))
  (set sym "AGI_TWTDD( {picid}, {nxy}, {wx}, {wy}, {dx}, {dy}, {status} )")
  (setplist sym '((class . token) (desc . "Transform double precision world to data coordinates")))

  (setq sym (intern "TRN__CMTOF" starfort-mode-token-table))
  (set sym "TRN__CMTOF")
  (setplist sym '((class . token) (desc . "Compiled mapping table overflow (error code)")))

  (setq sym (intern "SLA_ECOR" starfort-mode-token-table))
  (set sym "SLA_ECOR( {rm}, {dm}, {iy}, {id}, {fd}, {rv}, {tl} )")
  (setplist sym '((class . token) (desc . "Component of Earth orbit velocity and heliocentric light time in a given direction")))

  (setq sym (intern "GQPLR" starfort-mode-token-table))
  (set sym "GQPLR( {wkid}, {pli}, {gks$type}, {errind}, {lntype}, {lwidth}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire polyline representation")))

  (setq sym (intern "GQPMI" starfort-mode-token-table))
  (set sym "GQPMI( {errind}, {index} )")
  (setplist sym '((class . token) (desc . "Inquire polymarker index")))

  (setq sym (intern "EMS_REP" starfort-mode-token-table))
  (set sym "EMS_REP( {param}, {text}, {status} )")
  (setplist sym '((class . token) (desc . "Report an error message") (helpkey . "EMS_REP")))

  (setq sym (intern "IIIGLE" starfort-mode-token-table))
  (set sym "IIIGLE( {dispid}, {neval}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Get Logical Evaluator")))

  (setq sym (intern "GQPMF" starfort-mode-token-table))
  (set sym "GQPMF( {wtype}, {n}, {errind}, {nmt}, {mt}, {nms}, {nomms}, {rmsmin}, {rmsmax}, {nppmi} )")
  (setplist sym '((class . token) (desc . "Inquire polymarker facilities")))

  (setq sym (intern "IIIGLD" starfort-mode-token-table))
  (set sym "IIIGLD( {dispid}, {locnum}, {dx}, {dy}, {status} )")
  (setplist sym '((class . token) (desc . "Get Locator Displacement")))

  (setq sym (intern "GQPLI" starfort-mode-token-table))
  (set sym "GQPLI( {errind}, {index} )")
  (setplist sym '((class . token) (desc . "Inquire plyline index")))

  (setq sym (intern "PSX_GETPID" starfort-mode-token-table))
  (set sym "PSX_GETPID( {pid}, {status} )")
  (setplist sym '((class . token) (desc . "Gets the process ID")))

  (setq sym (intern "NDF_ACMSG" starfort-mode-token-table))
  (set sym "NDF_ACMSG( {token}, {indf}, {comp}, {iaxis}, {status} )")
  (setplist sym '((class . token) (desc . "Assign the value of an NDF axis character component to a message token") (helpkey . "NDF_ACMSG")))

  (setq sym (intern "GQPLF" starfort-mode-token-table))
  (set sym "GQPLF( {wtype}, {n}, {errind}, {nlt}, {lt}, {nlw}, {nomlw}, {rlwmin}, {rlwmax}, {nppli} )")
  (setplist sym '((class . token) (desc . "Inquire polyline facilities")))

  (setq sym (intern "CALL" starfort-mode-token-table))
  (set sym '(("CALL_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "GQPPAR" starfort-mode-token-table))
  (set sym "GQPPAR( {wtype}, {ppai}, {nmx}, {mmx}, {errind}, {n}, {m}, {parray} )")
  (setplist sym '((class . token) (desc . "Inquire predefined pattern representation")))

  (setq sym (intern "VAL_IDVUW" starfort-mode-token-table))
  (set sym "VAL_IDVUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one UNSIGNED WORD value by another")))

  (setq sym (intern "IIIGIE" starfort-mode-token-table))
  (set sym "IIIGIE( {dispid}, {neval}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Get Integer Evaluator")))

  (setq sym (intern "GQOPS" starfort-mode-token-table))
  (set sym "GQOPS( {opsta} )")
  (setplist sym '((class . token) (desc . "Inquire operating state value")))

  (setq sym (intern "VAL_ITOW" starfort-mode-token-table))
  (set sym "VAL_ITOW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER value to WORD")))

  (setq sym (intern "MAG_JUMP" starfort-mode-token-table))
  (set sym "MAG_JUMP( {td}, {nblock}, {status} )")
  (setplist sym '((class . token) (desc . "Skip a specified number of physical blocks")))

  (setq sym (intern "VAL_ITOR" starfort-mode-token-table))
  (set sym "VAL_ITOR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER value to REAL")))

  (setq sym (intern "NUM_DIMUW" starfort-mode-token-table))
  (set sym "NUM_DIMUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Positive difference of two UNSIGNED WORD numbers")))

  (setq sym (intern "NDF_SBND" starfort-mode-token-table))
  (set sym "NDF_SBND( {ndim}, {lbnd}, {ubnd}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Set new pixel-index bounds for an NDF") (helpkey . "NDF_SBND")))

  (setq sym (intern "SGS_STXJ" starfort-mode-token-table))
  (set sym "SGS_STXJ( {txj} )")
  (setplist sym '((class . token) (desc . "Set text justification")))

  (setq sym (intern "VAL_ITOI" starfort-mode-token-table))
  (set sym "VAL_ITOI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER value to INTEGER")))

  (setq sym (intern "VAL_IDVUB" starfort-mode-token-table))
  (set sym "VAL_IDVUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one UNSIGNED BYTE value by another")))

  (setq sym (intern "GKS_PAR" starfort-mode-token-table))
  (set sym "GKS_PAR")
  (setplist sym '((class . token) (desc . "GKS symbolic constant definitions (include file)")))

  (setq sym (intern "VAL_ITOD" starfort-mode-token-table))
  (set sym "VAL_ITOD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER value to DOUBLE PRECISION")))

  (setq sym (intern "NDF_BB" starfort-mode-token-table))
  (set sym "NDF_BB( {indf}, {badbit}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the bad-bits mask value for the quality component of an NDF") (helpkey . "NDF_BB")))

  (setq sym (intern "VAL_ITOB" starfort-mode-token-table))
  (set sym "VAL_ITOB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER value to BYTE")))

  (setq sym (intern "GINVL" starfort-mode-token-table))
  (set sym "GINVL( {wkid}, {vldnr}, {ival}, {pet}, {xmin}, {xmax}, {ymin}, {ymax}, {loval}, {hival}, {il}, {ca} )")
  (setplist sym '((class . token) (desc . "Initialise valuator")))

  (setq sym (intern "CHR_CTOR" starfort-mode-token-table))
  (set sym "CHR_CTOR( {cvalue}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read a real number from a character string")))

  (setq sym (intern "SLA_DAT" starfort-mode-token-table))
  (set sym "SLA_DAT( {utc} )")
  (setplist sym '((class . token) (desc . "Increment to be applied to Coordinated Universal Time UTC to give International Atomic Time TAI")))

  (setq sym (intern "NUM_DIMUB" starfort-mode-token-table))
  (set sym "NUM_DIMUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Positive difference of two UNSIGNED BYTE numbers")))

  (setq sym (intern "DAT_CLONE" starfort-mode-token-table))
  (set sym "DAT_CLONE( {loc1}, {loc2}, {status} )")
  (setplist sym '((class . token) (desc . "Clone locator")))

  (setq sym (intern "ARY_STATE" starfort-mode-token-table))
  (set sym "ARY_STATE( {iary}, {state}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the state of an array (defined or undefined)")))

  (setq sym (intern "CHR_CTOL" starfort-mode-token-table))
  (set sym "CHR_CTOL( {cvalue}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read a logical value from a character string")))

  (setq sym (intern "CHR_CTOI" starfort-mode-token-table))
  (set sym "CHR_CTOI( {cvalue}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read an integer number from a character string")))

  (setq sym (intern "NUM_TANHR" starfort-mode-token-table))
  (set sym "NUM_TANHR( {num} )")
  (setplist sym '((class . token) (desc . "Hyperbolic tangent function of REAL number")))

  (setq sym (intern "NUM_SINHR" starfort-mode-token-table))
  (set sym "NUM_SINHR( {num} )")
  (setplist sym '((class . token) (desc . "Hyperbolic sine function of REAL number")))

  (setq sym (intern "CHR_CTOD" starfort-mode-token-table))
  (set sym "CHR_CTOD( {cvalue}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read a double precision number from a character string")))

  (setq sym (intern "GINST" starfort-mode-token-table))
  (set sym "GINST( {wkid}, {stdnr}, {istr}, {pet}, {xmin}, {xmax}, {ymin}, {ymax}, {buflen}, {inipos}, {il}, {ca} )")
  (setplist sym '((class . token) (desc . "Initialise string")))

  (setq sym (intern "CHR_CTOC" starfort-mode-token-table))
  (set sym "CHR_CTOC( {cvalue}, {string}, {nchar} )")
  (setplist sym '((class . token) (desc . "Write a character value into a string (in concise format)")))

  (setq sym (intern "GQPCR" starfort-mode-token-table))
  (set sym "GQPCR( {wtype}, {pci}, {errind}, {red}, {green}, {blue} )")
  (setplist sym '((class . token) (desc . "Inquire predefined colour representation")))

  (setq sym (intern "FIO_READ" starfort-mode-token-table))
  (set sym "FIO_READ( {fd}, {buf}, {nchar}, {status} )")
  (setplist sym '((class . token) (desc . "Read sequential record")))

  (setq sym (intern "SGS_SFONT" starfort-mode-token-table))
  (set sym "SGS_SFONT( {nf} )")
  (setplist sym '((class . token) (desc . "Set font of text")))

  (setq sym (intern "PGADVANCE" starfort-mode-token-table))
  (set sym "PGADVANCE")
  (setplist sym '((class . token) (desc . "Advance to new page")))

  (setq sym (intern "GINSK" starfort-mode-token-table))
  (set sym "GINSK( {wkid}, {skdnr}, {tnr}, {mp}, {ipx}, {ipy}, {pet}, {xmin}, {xmax}, {ymin}, {ymax}, {buflen}, {il}, {ca} )")
  (setplist sym '((class . token) (desc . "Initialise stroke")))

  (setq sym (intern "NUM_TANHD" starfort-mode-token-table))
  (set sym "NUM_TANHD( {num} )")
  (setplist sym '((class . token) (desc . "Hyperbolic tangent function of DOUBLE PRECISION number")))

  (setq sym (intern "NUM_SINHD" starfort-mode-token-table))
  (set sym "NUM_SINHD( {num} )")
  (setplist sym '((class . token) (desc . "Hyperbolic sine function of DOUBLE PRECISION number")))

  (setq sym (intern "SLA_SUBET" starfort-mode-token-table))
  (set sym "SLA_SUBET( {rc}, {dc}, {eq}, {rm}, {dm} )")
  (setplist sym '((class . token) (desc . "Remove the E-terms (elliptic component of annual aberration) from a pre IAU 1976 catalogue RA,Dec to give a mean place")))

  (setq sym (intern "IIGTXT" starfort-mode-token-table))
  (set sym "IIGTXT( {dispid}, {memid}, {text}, {xpos}, {ypos}, {tpath}, {tangle}, {color}, {tsize}, {status} )")
  (setplist sym '((class . token) (desc . "Plot text")))

  (setq sym (intern "CHARACTER_N" starfort-mode-token-table))
  (set sym "CHARACTER * {len}")
  (setplist sym '((class . token) (desc . "CHARACTER * N data type")))

  (setq sym (intern "GRP_NEW" starfort-mode-token-table))
  (set sym "GRP_NEW( {type}, {igrp}, {status} )")
  (setplist sym '((class . token) (desc . "Create a new empty group")))

  (setq sym (intern "GQPAR" starfort-mode-token-table))
  (set sym "GQPAR( {wkid}, {pai}, {gks$type}, {nmx}, {mmx}, {errind}, {n}, {m}, {parray} )")
  (setplist sym '((class . token) (desc . "Inquire pattern representation")))

  (setq sym (intern "SGS_BOX" starfort-mode-token-table))
  (set sym "SGS_BOX( {x1}, {x2}, {y1}, {y2} )")
  (setplist sym '((class . token) (desc . "Draw a box")))

  (setq sym (intern "VEC_DIVUW" starfort-mode-token-table))
  (set sym "VEC_DIVUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one UNSIGNED WORD vectorised array by another")))

  (setq sym (intern "PROLOGUE_SECTIONS" starfort-mode-token-table))
  (set sym '(("PROLOGUE_SECTIONS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NUM_TANDR" starfort-mode-token-table))
  (set sym "NUM_TANDR( {num} )")
  (setplist sym '((class . token) (desc . "Tangent function of REAL number (degrees)")))

  (setq sym (intern "NUM_SINDR" starfort-mode-token-table))
  (set sym "NUM_SINDR( {num} )")
  (setplist sym '((class . token) (desc . "Sine function of REAL number (degrees)")))

  (setq sym (intern "GQPAF" starfort-mode-token-table))
  (set sym "GQPAF( {wtype}, {errind}, {nppai} )")
  (setplist sym '((class . token) (desc . "Inquire pattern facilities")))

  (setq sym (intern "NUM_NINTUW" starfort-mode-token-table))
  (set sym "NUM_NINTUW( {num} )")
  (setplist sym '((class . token) (desc . "Nearest integer to UNSIGNED WORD number")))

  (setq sym (intern "SGS_ANNUL" starfort-mode-token-table))
  (set sym "SGS_ANNUL( {izonid}, {status} )")
  (setplist sym '((class . token) (desc . "Close graphics workstation without cancelling parameter")))

  (setq sym (intern "IIIENI" starfort-mode-token-table))
  (set sym "IIIENI( {dispid}, {intty}, {intid}, {objty}, {objid}, {intop}, {extrn}, {status} )")
  (setplist sym '((class . token) (desc . "Enable interaction")))

  (setq sym (intern "NUM_TANDD" starfort-mode-token-table))
  (set sym "NUM_TANDD( {num} )")
  (setplist sym '((class . token) (desc . "Tangent function of DOUBLE PRECISION number (degrees)")))

  (setq sym (intern "NUM_SINDD" starfort-mode-token-table))
  (set sym "NUM_SINDD( {num} )")
  (setplist sym '((class . token) (desc . "Sine function of DOUBLE PRECISION number (degrees)")))

  (setq sym (intern "NDF_SAME" starfort-mode-token-table))
  (set sym "NDF_SAME( {indf1}, {indf2}, {same}, {isect}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire if two NDFs are part of the same base NDF") (helpkey . "NDF_SAME")))

  (setq sym (intern "VEC_DIVUB" starfort-mode-token-table))
  (set sym "VEC_DIVUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one UNSIGNED BYTE vectorised array by another")))

  (setq sym (intern "TRN__VERMM" starfort-mode-token-table))
  (set sym "TRN__VERMM")
  (setplist sym '((class . token) (desc . "Software version mis-match (error code)")))

  (setq sym (intern "DOUBLE_PRECISION" starfort-mode-token-table))
  (set sym "DOUBLE PRECISION")
  (setplist sym '((class . token) (desc . "DOUBLE PRECISION data type")))

  (setq sym (intern "ERR_PAR" starfort-mode-token-table))
  (set sym '(("ERR_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "CHARACTER_*" starfort-mode-token-table))
  (set sym "CHARACTER * ( * )")
  (setplist sym '((class . token) (desc . "CHARACTER * ( * ) data type")))

  (setq sym (intern "SUBROUTINE" starfort-mode-token-table))
  (set sym '(("SUBROUTINE_PROGRAM_MODULE" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NUM_NINTUB" starfort-mode-token-table))
  (set sym "NUM_NINTUB( {num} )")
  (setplist sym '((class . token) (desc . "Nearest integer to UNSIGNED BYTE number")))

  (setq sym (intern "HDS_OPEN" starfort-mode-token-table))
  (set sym "HDS_OPEN( {file}, {mode}, {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Open container file")))

  (setq sym (intern "HDS_NEW" starfort-mode-token-table))
  (set sym "HDS_NEW( {file}, {name}, {type}, {ndim}, {dim}, {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Create container file")))

  (setq sym (intern "MAG_DEAL" starfort-mode-token-table))
  (set sym "MAG_DEAL( {param}, {status} )")
  (setplist sym '((class . token) (desc . "De-allocate a tape drive")))

  (setq sym (intern "IIIEIW" starfort-mode-token-table))
  (set sym "IIIEIW( {dispid}, {trigs}, {status} )")
  (setplist sym '((class . token) (desc . "Execute interaction and wait")))

  (setq sym (intern "SLA_SVDCOV" starfort-mode-token-table))
  (set sym "SLA_SVDCOV( {n}, {np}, {nc}, {w}, {v}, {work}, {cvm} )")
  (setplist sym '((class . token) (desc . "From the W and V matrices from the SVD factorisation of a matrix (as obtained from the SLA_SVD routine), obtain the covariance matrix.")))

  (setq sym (intern "SLA_AOP" starfort-mode-token-table))
  (set sym "SLA_AOP( {rap}, {dap}, {date}, {dut}, {elongm}, {phim}, {hm}, {xp}, {yp}, {tdk}, {pmb}, {rh}, {wl}, {tlr}, {aob}, {zob}, {hob}, {dob}, {rob} )")
  (setplist sym '((class . token) (desc . "Apparent to observed place, for optical sources distant from the solar system.")))

  (setq sym (intern "VEC_PWRW" starfort-mode-token-table))
  (set sym "VEC_PWRW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (WORD vectorised arrays)")))

  (setq sym (intern "GINLC" starfort-mode-token-table))
  (set sym "GINLC( {wkid}, {lcdnr}, {tnr}, {ipx}, {ipy}, {pet}, {xmin}, {xmax}, {ymin}, {ymax}, {il}, {ca} )")
  (setplist sym '((class . token) (desc . "Initialise locator")))

  (setq sym (intern "NDF_SBAD" starfort-mode-token-table))
  (set sym "NDF_SBAD( {bad}, {indf}, {comp}, {status} )")
  (setplist sym '((class . token) (desc . "Set the bad-pixel flag for an NDF array component") (helpkey . "NDF_SBAD")))

  (setq sym (intern "VEC_PWRR" starfort-mode-token-table))
  (set sym "VEC_PWRR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (REAL vectorised arrays)")))

  (setq sym (intern "VAL_MULUW" starfort-mode-token-table))
  (set sym "VAL_MULUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two UNSIGNED WORD values")))

  (setq sym (intern "SGS_ARC" starfort-mode-token-table))
  (set sym "SGS_ARC( {x}, {y}, {r}, {theta1}, {theta2} )")
  (setplist sym '((class . token) (desc . "Draw arc of a circle")))

  (setq sym (intern "DAT_CCTYP" starfort-mode-token-table))
  (set sym "DAT_CCTYP( {size}, {type} )")
  (setplist sym '((class . token) (desc . "Create type string")))

  (setq sym (intern "DAT_MAPV" starfort-mode-token-table))
  (set sym "DAT_MAPV( {loc}, {type}, {mode}, {pntr}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Map vectorised primitive")))

  (setq sym (intern "SLA_AMP" starfort-mode-token-table))
  (set sym "SLA_AMP( {ra}, {da}, {date}, {eq}, {rm}, {dm} )")
  (setplist sym '((class . token) (desc . "Convert star RA,Dec from geocentric apparent to mean place")))

  (setq sym (intern "VEC_PWRI" starfort-mode-token-table))
  (set sym "VEC_PWRI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (INTEGER vectorised arrays)")))

  (setq sym (intern "DAT_PUT" starfort-mode-token-table))
  (set sym "DAT_PUT( {loc}, {type}, {ndim}, {dim}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Write primitive")))

  (setq sym (intern "DAT_MAPR" starfort-mode-token-table))
  (set sym "DAT_MAPR( {loc}, {mode}, {ndim}, {dim}, {pntr}, {status} )")
  (setplist sym '((class . token) (desc . "Map primitive as REAL")))

  (setq sym (intern "VEC_PWRD" starfort-mode-token-table))
  (set sym "VEC_PWRD( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (DOUBLE PRECISION vectorised arrays)")))

  (setq sym (intern "DAT_MAPN" starfort-mode-token-table))
  (set sym "DAT_MAPN( {loc}, {type}, {mode}, {ndim}, {pntr}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Map array primitive")))

  (setq sym (intern "SLA_DJCL" starfort-mode-token-table))
  (set sym "SLA_DJCL( {djm}, {iy}, {im}, {id}, {fd}, {j} )")
  (setplist sym '((class . token) (desc . "Modified Julian Date to Gregorian year, month, day, and fraction of a day.")))

  (setq sym (intern "VEC_PWRB" starfort-mode-token-table))
  (set sym "VEC_PWRB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (BYTE vectorised arrays)")))

  (setq sym (intern "DAT_MAPL" starfort-mode-token-table))
  (set sym "DAT_MAPL( {loc}, {mode}, {ndim}, {dim}, {pntr}, {status} )")
  (setplist sym '((class . token) (desc . "Map primitive as LOGICAL")))

  (setq sym (intern "DAT_MAPI" starfort-mode-token-table))
  (set sym "DAT_MAPI( {loc}, {mode}, {ndim}, {dim}, {pntr}, {status} )")
  (setplist sym '((class . token) (desc . "Map primitive as INTEGER")))

  (setq sym (intern "VAL_MULUB" starfort-mode-token-table))
  (set sym "VAL_MULUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two UNSIGNED BYTE values")))

  (setq sym (intern "DAT_REF" starfort-mode-token-table))
  (set sym "DAT_REF( {loc}, {ref}, {lref}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain a reference for an HDS object")))

  (setq sym (intern "DAT_MAPD" starfort-mode-token-table))
  (set sym "DAT_MAPD( {loc}, {mode}, {ndim}, {dim}, {pntr}, {status} )")
  (setplist sym '((class . token) (desc . "Map primitive as DOUBLE PRECISION")))

  (setq sym (intern "PGQCOL" starfort-mode-token-table))
  (set sym "PGQCOL( {ci1}, {ci2} )")
  (setplist sym '((class . token) (desc . "Inquire color capability")))

  (setq sym (intern "DAT_MAPC" starfort-mode-token-table))
  (set sym "DAT_MAPC( {loc}, {mode}, {ndim}, {dim}, {pntr}, {status} )")
  (setplist sym '((class . token) (desc . "Map primitive as CHARACTER")))

  (setq sym (intern "GQLWK" starfort-mode-token-table))
  (set sym "GQLWK( {wtype}, {errind}, {mplbte}, {mpmbte}, {mtxbte}, {mfabte}, {mpai}, {mcoli} )")
  (setplist sym '((class . token) (desc . "Inquire maximum number of workstation state tables")))

  (setq sym (intern "GQACWK" starfort-mode-token-table))
  (set sym "GQACWK( {n}, {errind}, {ol}, {wkid} )")
  (setplist sym '((class . token) (desc . "Inquire set member of active workstations")))

  (setq sym (intern "PSX_MALLOC" starfort-mode-token-table))
  (set sym "PSX_MALLOC( {size}, {pntr}, {status} )")
  (setplist sym '((class . token) (desc . "Allocate virtual memory")))

  (setq sym (intern "NDF_ACLEN" starfort-mode-token-table))
  (set sym "NDF_ACLEN( {indf}, {comp}, {iaxis}, {length}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the length of an NDF axis character component") (helpkey . "NDF_ACLEN")))

  (setq sym (intern "PGIDEN" starfort-mode-token-table))
  (set sym "PGIDEN")
  (setplist sym '((class . token) (desc . "Write username, date, and time at bottom of plot")))

  (setq sym (intern "MAG_SKIP" starfort-mode-token-table))
  (set sym "MAG_SKIP( {td}, {ntm}, {status} )")
  (setplist sym '((class . token) (desc . "Skip a specified number of tape marks")))

  (setq sym (intern "VEC_INTW" starfort-mode-token-table))
  (set sym "VEC_INTW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate WORD vectorised array to an integer")))

  (setq sym (intern "SGS_CUVIS" starfort-mode-token-table))
  (set sym "SGS_CUVIS( {vis} )")
  (setplist sym '((class . token) (desc . "Set cursor visibility")))

  (setq sym (intern "VEC_INTR" starfort-mode-token-table))
  (set sym "VEC_INTR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate REAL vectorised array to an integer")))

  (setq sym (intern "PRIOR_REQUIREMENTS" starfort-mode-token-table))
  (set sym '(("PRIOR_REQUIREMENTS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "ARY_PLACE" starfort-mode-token-table))
  (set sym "ARY_PLACE( {loc}, {name}, {place}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain an array placeholder")))

  (setq sym (intern "GSPMCI" starfort-mode-token-table))
  (set sym "GSPMCI( {pmcoli} )")
  (setplist sym '((class . token) (desc . "Set polymarker colour index")))

  (setq sym (intern "SLA_DCS2C" starfort-mode-token-table))
  (set sym "SLA_DCS2C( {a}, {b}, {v} )")
  (setplist sym '((class . token) (desc . "Spherical coordinates to direction cosines (double precision)")))

  (setq sym (intern "VEC_INTI" starfort-mode-token-table))
  (set sym "VEC_INTI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate INTEGER vectorised array to an integer")))

  (setq sym (intern "GQPMCI" starfort-mode-token-table))
  (set sym "GQPMCI( {errind}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire polymarker colour index")))

  (setq sym (intern "VEC_INTD" starfort-mode-token-table))
  (set sym "VEC_INTD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate DOUBLE PRECISION vectorised array to an integer")))

  (setq sym (intern "GSHLIT" starfort-mode-token-table))
  (set sym "GSHLIT( {sgna}, {gks$hil} )")
  (setplist sym '((class . token) (desc . "Set highlighting")))

  (setq sym (intern "GINCH" starfort-mode-token-table))
  (set sym "GINCH( {wkid}, {chdnr}, {ichnr}, {pet}, {xmin}, {xmax}, {ymin}, {ymax}, {il}, {ca} )")
  (setplist sym '((class . token) (desc . "Initialise choice")))

  (setq sym (intern "VEC_INTB" starfort-mode-token-table))
  (set sym "VEC_INTB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate BYTE vectorised array to an integer")))

  (setq sym (intern "ZERO" starfort-mode-token-table))
  (set sym "'ZERO'")
  (setplist sym '((class . token) (desc . "'ZERO'")))

  (setq sym (intern "PAR_DEFNR" starfort-mode-token-table))
  (set sym "PAR_DEFNR( {param}, {ndim}, {dimx}, {rvalue}, {dims}, {status} )")
  (setplist sym '((class . token) (desc . "Set REAL N-dimensional dynamic default parameter value")))

  (setq sym (intern "PAR_DEFNL" starfort-mode-token-table))
  (set sym "PAR_DEFNL( {param}, {ndim}, {dimx}, {lvalue}, {dims}, {status} )")
  (setplist sym '((class . token) (desc . "Set LOGICAL N-dimensional dynamic default parameter value")))

  (setq sym (intern "TRN_APND" starfort-mode-token-table))
  (set sym "TRN_APND( {loctr1}, {loctr2}, {status} )")
  (setplist sym '((class . token) (desc . "Append transformation")))

  (setq sym (intern "PAR_DEFNI" starfort-mode-token-table))
  (set sym "PAR_DEFNI( {param}, {ndim}, {dimx}, {ivalue}, {dims}, {status} )")
  (setplist sym '((class . token) (desc . "Set INTEGER N-dimensional dynamic default parameter value")))

  (setq sym (intern "BLOCK_DATA_SECTIONS" starfort-mode-token-table))
  (set sym '(("BLOCK_DATA_SECTIONS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "PAR_DEFND" starfort-mode-token-table))
  (set sym "PAR_DEFND( {param}, {ndim}, {dimx}, {dvalue}, {dims}, {status} )")
  (setplist sym '((class . token) (desc . "Set DOUBLE PRECISION N-dimensional dynamic default parameter value")))

  (setq sym (intern "NDF_STATE" starfort-mode-token-table))
  (set sym "NDF_STATE( {indf}, {comp}, {state}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the state of an NDF component (defined or undefined)") (helpkey . "NDF_STATE")))

  (setq sym (intern "PAR_DEFNC" starfort-mode-token-table))
  (set sym "PAR_DEFNC( {param}, {ndim}, {dimx}, {cvalue}, {dims}, {status} )")
  (setplist sym '((class . token) (desc . "Set CHARACTER N-dimensional dynamic default parameter value")))

  (setq sym (intern "SLA_CS2C" starfort-mode-token-table))
  (set sym "SLA_CS2C( {a}, {b}, {v} )")
  (setplist sym '((class . token) (desc . "Spherical coordinates to direction cosines (single precision)")))

  (setq sym (intern "TRN__UNIDT" starfort-mode-token-table))
  (set sym "TRN__UNIDT")
  (setplist sym '((class . token) (desc . "'Volumes/areas preserved' classification (symbolic constant)")))

  (setq sym (intern "IMPLEMENTATION_STATUS" starfort-mode-token-table))
  (set sym '(("IMPLEMENTATION_STATUS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "FIO_FNAME" starfort-mode-token-table))
  (set sym "FIO_FNAME( {fd}, {fname}, {status} )")
  (setplist sym '((class . token) (desc . "Get the full file name of a file")))

  (setq sym (intern "GQMDS" starfort-mode-token-table))
  (set sym "GQMDS( {wtype}, {errind}, {dcunit}, {rx}, {ry}, {lx}, {ly} )")
  (setplist sym '((class . token) (desc . "Inquire maximum display surface size")))

  (setq sym (intern "PSX_CALLOC" starfort-mode-token-table))
  (set sym "PSX_CALLOC( {nmemb}, {type}, {pntr}, {status} )")
  (setplist sym '((class . token) (desc . "Allocate space for several objects of specified type")))

  (setq sym (intern "PGHIST" starfort-mode-token-table))
  (set sym "PGHIST( {n}, {data}, {datmin}, {datmax}, {nbin}, {pgflag} )")
  (setplist sym '((class . token) (desc . "Histogram of unbinned data")))

  (setq sym (intern "_CHECK_STATUS" starfort-mode-token-table))
  (set sym "IF ( STATUS .NE. SAI__OK ) GO TO {abort_stmt}	")
  (setplist sym '((class . token) (desc . "Abort if STATUS is not OK")))

  (setq sym (intern "GSPLCI" starfort-mode-token-table))
  (set sym "GSPLCI( {plcoli} )")
  (setplist sym '((class . token) (desc . "Set polyline colour index")))

  (setq sym (intern "GQPLCI" starfort-mode-token-table))
  (set sym "GQPLCI( {errind}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire polyline colour index")))

  (setq sym (intern "DAT_COPY" starfort-mode-token-table))
  (set sym "DAT_COPY( {loc1}, {loc2}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Copy object")))

  (setq sym (intern "SGS_MARK" starfort-mode-token-table))
  (set sym "SGS_MARK( {x}, {y}, {mtype} )")
  (setplist sym '((class . token) (desc . "Draw marker")))

  (setq sym (intern "NDF_END" starfort-mode-token-table))
  (set sym "NDF_END( {status} )")
  (setplist sym '((class . token) (desc . "End the current NDF context") (helpkey . "NDF_END")))

  (setq sym (intern "EXAMPLES" starfort-mode-token-table))
  (set sym '(("EXAMPLES" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "PGCURSE" starfort-mode-token-table))
  (set sym "PGCURSE( {x}, {y}, {ch} )")
  (setplist sym '((class . token) (desc . "Read cursor position")))

  (setq sym (intern "DAT_ASSOC" starfort-mode-token-table))
  (set sym "DAT_ASSOC( {param}, {mode}, {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Return a locator associated with an ADAM parameter")))

  (setq sym (intern "DAT_PAR" starfort-mode-token-table))
  (set sym "DAT_PAR")
  (setplist sym '((class . token) (desc . "DAT__ symbolic constant definitions (include file)")))

  (setq sym (intern "TRN__CMPER" starfort-mode-token-table))
  (set sym "TRN__CMPER")
  (setplist sym '((class . token) (desc . "Compilation error (error code)")))

  (setq sym (intern "SGS_LINE" starfort-mode-token-table))
  (set sym "SGS_LINE( {x1}, {y1}, {x2}, {y2} )")
  (setplist sym '((class . token) (desc . "Begin polyline with a single line")))

  (setq sym (intern "RIO_CLOSE" starfort-mode-token-table))
  (set sym "RIO_CLOSE( {fd}, {status} )")
  (setplist sym '((class . token) (desc . "Close a direct access file")))

  (setq sym (intern "AGI_IWOCO" starfort-mode-token-table))
  (set sym "AGI_IWOCO( {wx1}, {wx2}, {wy1}, {wy2}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire world coordinates of current picture")))

  (setq sym (intern "VAL_PWRW" starfort-mode-token-table))
  (set sym "VAL_PWRW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (WORD values)")))

  (setq sym (intern "GQLCS" starfort-mode-token-table))
  (set sym "GQLCS( {wkid}, {lcdnr}, {gks$type}, {mldr}, {errind}, {mode}, {esw}, {itnr}, {ilpx}, {ilpy}, {pet}, {earea}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Inquire locator device state")))

  (setq sym (intern "AGI_RCS" starfort-mode-token-table))
  (set sym "AGI_RCS( {pname}, {pstart}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Recall succeeding picture of specified name")))

  (setq sym (intern "VAL_PWRR" starfort-mode-token-table))
  (set sym "VAL_PWRR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (REAL values)")))

  (setq sym (intern "AGI_RCP" starfort-mode-token-table))
  (set sym "AGI_RCP( {pname}, {pstart}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Recall preceding picture of specified name")))

  (setq sym (intern "FIO_CLOSE" starfort-mode-token-table))
  (set sym "FIO_CLOSE( {fd}, {status} )")
  (setplist sym '((class . token) (desc . "Close a sequential file")))

  (setq sym (intern "AGI_RCL" starfort-mode-token-table))
  (set sym "AGI_RCL( {pname}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Recall last piture of specified name")))

  (setq sym (intern "PGGRAY" starfort-mode-token-table))
  (set sym "PGGRAY( {a}, {idim}, {jdim}, {i1}, {i2}, {j1}, {j2}, {fg}, {bg}, {tr} )")
  (setplist sym '((class . token) (desc . "Gray-scale map of a 2D data array")))

  (setq sym (intern "RECL" starfort-mode-token-table))
  (set sym "RECL = {arith_exp}")
  (setplist sym '((class . token) (desc . "RECL = {arith_exp}")))

  (setq sym (intern "VAL_PWRI" starfort-mode-token-table))
  (set sym "VAL_PWRI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (INTEGER values)")))

  (setq sym (intern "VEC_SQRTW" starfort-mode-token-table))
  (set sym "VEC_SQRTW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of WORD vectorised array")))

  (setq sym (intern "AGI_RCF" starfort-mode-token-table))
  (set sym "AGI_RCF( {pname}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Recall first picture of specified name")))

  (setq sym (intern "VAL_PWRD" starfort-mode-token-table))
  (set sym "VAL_PWRD( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (DOUBLE PRECISION values)")))

  (setq sym (intern "SLA_DCMPF" starfort-mode-token-table))
  (set sym "SLA_DCMPF( {coeffs}, {xz}, {yz}, {xs}, {ys}, {perp}, {orient} )")
  (setplist sym '((class . token) (desc . "Decompose an [X,Y] linear fit into its constituent parameters: zero points, scales, nonperpendicularity and orientation.")))

  (setq sym (intern "VEC_SQRTR" starfort-mode-token-table))
  (set sym "VEC_SQRTR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of REAL vectorised array")))

  (setq sym (intern "VAL_PWRB" starfort-mode-token-table))
  (set sym "VAL_PWRB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (BYTE values)")))

  (setq sym (intern "SLA_RVLG" starfort-mode-token-table))
  (set sym "SLA_RVLG( {r2000}, {d2000} )")
  (setplist sym '((class . token) (desc . "Velocity component due to galactic rotation and mean motion of the local group.")))

  (setq sym (intern "NUM_EXPUW" starfort-mode-token-table))
  (set sym "NUM_EXPUW( {num} )")
  (setplist sym '((class . token) (desc . "Exponential function of UNSIGNED WORD number")))

  (setq sym (intern "NDF_NCHNK" starfort-mode-token-table))
  (set sym "NDF_NCHNK( {indf}, {mxpix}, {nchunk}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the number of chunks of contiguous pixels in an NDF") (helpkey . "NDF_NCHNK")))

  (setq sym (intern "REAL" starfort-mode-token-table))
  (set sym "REAL")
  (setplist sym '((class . token) (desc . "REAL data type")))

  (setq sym (intern "SGS_SETCU" starfort-mode-token-table))
  (set sym "SGS_SETCU( {x}, {y} )")
  (setplist sym '((class . token) (desc . "Set cursor position")))

  (setq sym (intern "VEC_SQRTI" starfort-mode-token-table))
  (set sym "VEC_SQRTI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of INTEGER vectorised array")))

  (setq sym (intern "CHR_COPY" starfort-mode-token-table))
  (set sym "CHR_COPY( {instr}, {flag}, {outstr}, {lstat} )")
  (setplist sym '((class . token) (desc . "Copy one string to another, checking for truncation")))

  (setq sym (intern "VEC_SQRTD" starfort-mode-token-table))
  (set sym "VEC_SQRTD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of DOUBLE PRECISION vectorised array")))

  (setq sym (intern "VEC_SQRTB" starfort-mode-token-table))
  (set sym "VEC_SQRTB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of BYTE vectorised array")))

  (setq sym (intern "READ" starfort-mode-token-table))
  (set sym "READ ( {ifile}, {ifmt}, [iread_parameter]... ) [io_elm]...")
  (setplist sym '((class . token) (desc . "Formatted internal read statement")))

  (setq sym (intern "GQOPWK" starfort-mode-token-table))
  (set sym "GQOPWK( {n}, {errind}, {ol}, {wkid} )")
  (setplist sym '((class . token) (desc . "Inquire set member of open workstations")))

  (setq sym (intern "NUM_AT2DR" starfort-mode-token-table))
  (set sym "NUM_AT2DR( {num} )")
  (setplist sym '((class . token) (desc . "Fortran ATAN2 function of REAL number (degrees)")))

  (setq sym (intern "TRN__DIAG" starfort-mode-token-table))
  (set sym "TRN__DIAG")
  (setplist sym '((class . token) (desc . "'Preserves axes' classification (symbolic constant)")))

  (setq sym (intern "NUM_EXPUB" starfort-mode-token-table))
  (set sym "NUM_EXPUB( {num} )")
  (setplist sym '((class . token) (desc . "Exponential function of UNSIGNED BYTE number")))

  (setq sym (intern "VAL_INTW" starfort-mode-token-table))
  (set sym "VAL_INTW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate WORD value to an integer")))

  (setq sym (intern "TRN__NTVMM" starfort-mode-token-table))
  (set sym "TRN__NTVMM")
  (setplist sym '((class . token) (desc . "Number of transformation variables mis-matched (error code)")))

  (setq sym (intern "NDF_DIM" starfort-mode-token-table))
  (set sym "NDF_DIM( {indf}, {ndimx}, {dim}, {ndim}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire the dimension sizes of an NDF") (helpkey . "NDF_DIM")))

  (setq sym (intern "DAT_MSG" starfort-mode-token-table))
  (set sym "DAT_MSG( {token}, {loc} )")
  (setplist sym '((class . token) (desc . "Assign the name of an HDS object to a message token")))

  (setq sym (intern "CMP_SIZE" starfort-mode-token-table))
  (set sym "CMP_SIZE( {loc}, {name}, {size}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire component size")))

  (setq sym (intern "VAL_INTR" starfort-mode-token-table))
  (set sym "VAL_INTR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate REAL value to an integer")))

  (setq sym (intern "NDF_PLACE" starfort-mode-token-table))
  (set sym "NDF_PLACE( {loc}, {name}, {place}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain an NDF placeholder") (helpkey . "NDF_PLACE")))

  (setq sym (intern "NUM_AT2DD" starfort-mode-token-table))
  (set sym "NUM_AT2DD( {num} )")
  (setplist sym '((class . token) (desc . "Fortran ATAN2 function of DOUBLE PRECISION number (degrees)")))

  (setq sym (intern "CMP_MOD" starfort-mode-token-table))
  (set sym "CMP_MOD( {loc}, {name}, {type}, {ndim}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain component")))

  (setq sym (intern "VAL_INTI" starfort-mode-token-table))
  (set sym "VAL_INTI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate INTEGER value to an integer")))

  (setq sym (intern "GIITM" starfort-mode-token-table))
  (set sym "GIITM( {type}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Interpret item")))

  (setq sym (intern "SUBROUTINE_PROLOGUE" starfort-mode-token-table))
  (set sym "\\*+
\\*{subroutine_prologue}
\\*-
\\	{subroutine_declarations}
\\*.")
  (setplist sym '((class . token) (desc . "Subroutine prologue")))

  (setq sym (intern "VAL_INTD" starfort-mode-token-table))
  (set sym "VAL_INTD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate DOUBLE PRECISION value to an integer")))

  (setq sym (intern "VAL_INTB" starfort-mode-token-table))
  (set sym "VAL_INTB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate BYTE value to an integer")))

  (setq sym (intern "GQOPSG" starfort-mode-token-table))
  (set sym "GQOPSG( {errind}, {segnam} )")
  (setplist sym '((class . token) (desc . "Inquire name of open segment")))

  (setq sym (intern "VAL_SIGNUW" starfort-mode-token-table))
  (set sym "VAL_SIGNUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one UNSIGNED WORD value to another")))

  (setq sym (intern "WRITE_STMT" starfort-mode-token-table))
  (set sym "WRITE ( {unit}, [fmt], [write_parameter]... ) [io_elm]...")
  (setplist sym '((class . token) (desc . "WRITE statement")))

  (setq sym (intern "DAT_NEW" starfort-mode-token-table))
  (set sym "DAT_NEW( {loc}, {name}, {type}, {ndim}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Create component")))

  (setq sym (intern "CHR_DELIM" starfort-mode-token-table))
  (set sym "CHR_DELIM( {string}, {delim}, {index1}, {index2} )")
  (setplist sym '((class . token) (desc . "Locate indices to substring with given delimiter character")))

  (setq sym (intern "CHR_INDEX" starfort-mode-token-table))
  (set sym "CHR_INDEX( {string}, {substr} )")
  (setplist sym '((class . token) (desc . "Find the index of a substring in a string")))

  (setq sym (intern "VAL_SIGNUB" starfort-mode-token-table))
  (set sym "VAL_SIGNUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one UNSIGNED BYTE value to another")))

  (setq sym (intern "ARY_MSG" starfort-mode-token-table))
  (set sym "ARY_MSG( {token}, {iary} )")
  (setplist sym '((class . token) (desc . "Assign the name of an array to a message token")))

  (setq sym (intern "IIGPLY" starfort-mode-token-table))
  (set sym "IIGPLY( {dispid}, {memid}, {x}, {y}, {nxy}, {color}, {lstyle}, {status} )")
  (setplist sym '((class . token) (desc . "Polyline")))

  (setq sym (intern "PSX_LOCALTIME" starfort-mode-token-table))
  (set sym "PSX_LOCALTIME( {nticks}, {secs}, {mins}, {hours}, {day}, {month}, {year}, {wday}, {yday}, {isdst}, {tstrct}, {status} )")
  (setplist sym '((class . token) (desc . "Convert the value returned by PSX_TIME to individual values")))

  (setq sym (intern "ARY_SSECT" starfort-mode-token-table))
  (set sym "ARY_SSECT( {iary1}, {iary2}, {iary3}, {status} )")
  (setplist sym '((class . token) (desc . "Create a similar array section to an existing one")))

  (setq sym (intern "DAT_TEMP" starfort-mode-token-table))
  (set sym "DAT_TEMP( {type}, {ndim}, {dim}, {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Create temporary object")))

  (setq sym (intern "ADAM_CONSTRUCTS" starfort-mode-token-table))
  (set sym '(("ADAM_CONSTRUCTS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SGS_SPEN" starfort-mode-token-table))
  (set sym "SGS_SPEN( {npen} )")
  (setplist sym '((class . token) (desc . "Select pen")))

  (setq sym (intern "INTRINSIC" starfort-mode-token-table))
  (set sym '(("INTRINSIC_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SLA_ECMAT" starfort-mode-token-table))
  (set sym "SLA_ECMAT( {date}, {rmat} )")
  (setplist sym '((class . token) (desc . "Form the equatorial to ecliptic rotation matrix (IAU 1980 theory)")))

  (setq sym (intern "NDF_AMAP" starfort-mode-token-table))
  (set sym "NDF_AMAP( {indf}, {comp}, {iaxis}, {type}, {mmod}, {pntr}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain mapped access to an NDF axis array") (helpkey . "NDF_AMAP")))

  (setq sym (intern "VAL_WTOUW" starfort-mode-token-table))
  (set sym "VAL_WTOUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD value to UNSIGNED WORD")))

  (setq sym (intern "PAR_DEF1R" starfort-mode-token-table))
  (set sym "PAR_DEF1R( {param}, {nval}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Set REAL dynamic default vector parameter value")))

  (setq sym (intern "HDS_ERASE" starfort-mode-token-table))
  (set sym "HDS_ERASE( {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Erase container file")))

  (setq sym (intern "NUM_SIGNW" starfort-mode-token-table))
  (set sym "NUM_SIGNW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one WORD number to another")))

  (setq sym (intern "ARY_NEW" starfort-mode-token-table))
  (set sym "ARY_NEW( {ftype}, {ndim}, {lbnd}, {ubnd}, {place}, {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Create a new simple array")))

  (setq sym (intern "VAL_RTOUW" starfort-mode-token-table))
  (set sym "VAL_RTOUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL value to UNSIGNED WORD")))

  (setq sym (intern "PAR_DEF1L" starfort-mode-token-table))
  (set sym "PAR_DEF1L( {param}, {nval}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Set LOGICAL dynamic default vector parameter value")))

  (setq sym (intern "NUM_SIGNR" starfort-mode-token-table))
  (set sym "NUM_SIGNR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one REAL number to another")))

  (setq sym (intern "PAR_DEF0R" starfort-mode-token-table))
  (set sym "PAR_DEF0R( {param}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Set REAL dynamic default scalar parameter value")))

  (setq sym (intern "DAT__CONER" starfort-mode-token-table))
  (set sym "DAT__CONER")
  (setplist sym '((class . token) (desc . "Conversion error (error code)")))

  (setq sym (intern "PAR_DEF1I" starfort-mode-token-table))
  (set sym "PAR_DEF1I( {param}, {nval}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Set INTEGER dynamic default vector parameter value")))

  (setq sym (intern "VAL_NEGUW" starfort-mode-token-table))
  (set sym "VAL_NEGUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of UNSIGNED WORD value")))

  (setq sym (intern "SGS_SUPTX" starfort-mode-token-table))
  (set sym "SGS_SUPTX( {xu}, {yu} )")
  (setplist sym '((class . token) (desc . "Set up vector of text")))

  (setq sym (intern "VAL_ITOUW" starfort-mode-token-table))
  (set sym "VAL_ITOUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER value to UNSIGNED WORD")))

  (setq sym (intern "PAR_DEF1D" starfort-mode-token-table))
  (set sym "PAR_DEF1D( {param}, {nval}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Set DOUBLE PRECISION dynamic default vector parameter value")))

  (setq sym (intern "PAR_DEF0L" starfort-mode-token-table))
  (set sym "PAR_DEF0L( {param}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Set LOGICAL dynamic default scalar parameter value")))

  (setq sym (intern "PAR_DEF1C" starfort-mode-token-table))
  (set sym "PAR_DEF1C( {param}, {nval}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Set CHARACTER dynamic default vector parameter value")))

  (setq sym (intern "NUM_SIGNI" starfort-mode-token-table))
  (set sym "NUM_SIGNI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one INTEGER number to another")))

  (setq sym (intern "PAR_DEF0I" starfort-mode-token-table))
  (set sym "PAR_DEF0I( {param}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Set INTEGER dynamic default scalar parameter value")))

  (setq sym (intern "VAL_DTOUW" starfort-mode-token-table))
  (set sym "VAL_DTOUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION value to UNSIGNED WORD")))

  (setq sym (intern "VAL_WTOUB" starfort-mode-token-table))
  (set sym "VAL_WTOUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD value to UNSIGNED BYTE")))

  (setq sym (intern "VAL_BTOUW" starfort-mode-token-table))
  (set sym "VAL_BTOUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE value to UNSIGNED WORD")))

  (setq sym (intern "NUM_SIGND" starfort-mode-token-table))
  (set sym "NUM_SIGND( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one DOUBLE PRECISION number to another")))

  (setq sym (intern "PAR_DEF0D" starfort-mode-token-table))
  (set sym "PAR_DEF0D( {param}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Set DOUBLE PRECISION dynamic default scalar parameter value")))

  (setq sym (intern "PAR_DEF0C" starfort-mode-token-table))
  (set sym "PAR_DEF0C( {param}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Set CHARACTER dynamic default scalar parameter value")))

  (setq sym (intern "NUM_SIGNB" starfort-mode-token-table))
  (set sym "NUM_SIGNB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one BYTE number to another")))

  (setq sym (intern "CHR_TERM" starfort-mode-token-table))
  (set sym "CHR_TERM( {length}, {string} )")
  (setplist sym '((class . token) (desc . "Terminate string by padding out with blanks")))

  (setq sym (intern "VAL_RTOUB" starfort-mode-token-table))
  (set sym "VAL_RTOUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a REAL value to UNSIGNED BYTE")))

  (setq sym (intern "SLA_ECLEQ" starfort-mode-token-table))
  (set sym "SLA_ECLEQ( {dl}, {db}, {date}, {dr}, {dd} )")
  (setplist sym '((class . token) (desc . "Transformation from ecliptic coordinates to J2000.0 equatorial coordinates")))

  (setq sym (intern "FUNCTION_SECTIONS" starfort-mode-token-table))
  (set sym '(("FUNCTION_SECTIONS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SLA_ADDET" starfort-mode-token-table))
  (set sym "SLA_ADDET( {rm}, {dm}, {eq}, {rc}, {dc} )")
  (setplist sym '((class . token) (desc . "Add the E-terms to a pre IAU 1976 mean place to conform to the old catalogue convention")))

  (setq sym (intern "SLA_SMAT" starfort-mode-token-table))
  (set sym "SLA_SMAT( {n}, {a}, {y}, {d}, {jf}, {iw} )")
  (setplist sym '((class . token) (desc . "Matrix inversion & solution of simultaneous equations")))

  (setq sym (intern "VAL_NEGUB" starfort-mode-token-table))
  (set sym "VAL_NEGUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of UNSIGNED BYTE value")))

  (setq sym (intern "VAL_ITOUB" starfort-mode-token-table))
  (set sym "VAL_ITOUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an INTEGER value to UNSIGNED BYTE")))

  (setq sym (intern "NDF_ACGET" starfort-mode-token-table))
  (set sym "NDF_ACGET( {indf}, {comp}, {iaxis}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the value of an NDF axis character component") (helpkey . "NDF_ACGET")))

  (setq sym (intern "VAL_DTOUB" starfort-mode-token-table))
  (set sym "VAL_DTOUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION value to UNSIGNED BYTE")))

  (setq sym (intern "AGI_MORE" starfort-mode-token-table))
  (set sym "AGI_MORE( {picid}, {acmode}, {morloc}, {status} )")
  (setplist sym '((class . token) (desc . "Return an HDS locator to a MORE structure")))

  (setq sym (intern "VAL_BTOUB" starfort-mode-token-table))
  (set sym "VAL_BTOUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a BYTE value to UNSIGNED BYTE")))

  (setq sym (intern "FORM_PAR" starfort-mode-token-table))
  (set sym '(("FORM_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SLA_DTF2R" starfort-mode-token-table))
  (set sym "SLA_DTF2R( {ihour}, {imin}, {sec}, {rad}, {j} )")
  (setplist sym '((class . token) (desc . "Convert hours, minutes, seconds to radians")))

  (setq sym (intern "SLA_CTF2R" starfort-mode-token-table))
  (set sym "SLA_CTF2R( {ihour}, {imin}, {sec}, {rad}, {j} )")
  (setplist sym '((class . token) (desc . "Convert hours, minutes, seconds to radians")))

  (setq sym (intern "DAT_MAP" starfort-mode-token-table))
  (set sym "DAT_MAP( {loc}, {type}, {mode}, {ndim}, {dim}, {pntr}, {status} )")
  (setplist sym '((class . token) (desc . "Map primitive")))

  (setq sym (intern "DAT_SLICE" starfort-mode-token-table))
  (set sym "DAT_SLICE( {loc1}, {ndim}, {diml}, {dimu}, {loc2}, {status} )")
  (setplist sym '((class . token) (desc . "Locate slice")))

  (setq sym (intern "DAT__NOLOC" starfort-mode-token-table))
  (set sym "DAT__NOLOC")
  (setplist sym '((class . token) (desc . "Null locator value (symbolic constant)")))

  (setq sym (intern "CMP_LEN" starfort-mode-token-table))
  (set sym "CMP_LEN( {loc}, {name}, {len}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire component precision")))

  (setq sym (intern "SLA_DTF2D" starfort-mode-token-table))
  (set sym "SLA_DTF2D( {ihour}, {imin}, {sec}, {days}, {j} )")
  (setplist sym '((class . token) (desc . "Convert hours, minutes, seconds to days")))

  (setq sym (intern "SLA_CTF2D" starfort-mode-token-table))
  (set sym "SLA_CTF2D( {ihour}, {imin}, {sec}, {days}, {j} )")
  (setplist sym '((class . token) (desc . "Convert hours, minutes, seconds to days")))

  (setq sym (intern "HDS_FREE" starfort-mode-token-table))
  (set sym "HDS_FREE( {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Free container file")))

  (setq sym (intern "DAT__COMEX" starfort-mode-token-table))
  (set sym "DAT__COMEX")
  (setplist sym '((class . token) (desc . "Component already exists (error code)")))

  (setq sym (intern "NUM_EXPW" starfort-mode-token-table))
  (set sym "NUM_EXPW( {num} )")
  (setplist sym '((class . token) (desc . "Exponential function of WORD number")))

  (setq sym (intern "NUM_EXPR" starfort-mode-token-table))
  (set sym "NUM_EXPR( {num} )")
  (setplist sym '((class . token) (desc . "Exponential function of REAL number")))

  (setq sym (intern "NDF_PROP" starfort-mode-token-table))
  (set sym "NDF_PROP( {indf1}, {clist}, {param}, {indf2}, {status} )")
  (setplist sym '((class . token) (desc . "Propagate NDF information to create a new NDF via the ADAM parameter system") (helpkey . "NDF_PROP")))

  (setq sym (intern "REC" starfort-mode-token-table))
  (set sym "REC = {rec}")
  (setplist sym '((class . token) (desc . "REC = {rec}")))

  (setq sym (intern "DAT_CCOPY" starfort-mode-token-table))
  (set sym "DAT_CCOPY( {loc1}, {loc2}, {name}, {loc3}, {status} )")
  (setplist sym '((class . token) (desc . "Copy one structure level")))

  (setq sym (intern "GRP_GET" starfort-mode-token-table))
  (set sym "GRP_GET( {igrp}, {index}, {size}, {names}, {status} )")
  (setplist sym '((class . token) (desc . "Returns a set of names contained in a group")))

  (setq sym (intern "SLA_CLDJ" starfort-mode-token-table))
  (set sym "SLA_CLDJ( {iy}, {im}, {id}, {djm}, {j} )")
  (setplist sym '((class . token) (desc . "Gregorian Calendar to Modified Julian Date")))

  (setq sym (intern "NUM_EXPI" starfort-mode-token-table))
  (set sym "NUM_EXPI( {num} )")
  (setplist sym '((class . token) (desc . "Exponential function of INTEGER number")))

  (setq sym (intern "PGHI2D" starfort-mode-token-table))
  (set sym "PGHI2D( {data}, {nxv}, {nyv}, {ix1}, {ix2}, {iy1}, {iy2}, {x}, {ioff}, {bias}, {center}, {ylims} )")
  (setplist sym '((class . token) (desc . "Cross-sections through a 2D data array")))

  (setq sym (intern "NUM_EXPD" starfort-mode-token-table))
  (set sym "NUM_EXPD( {num} )")
  (setplist sym '((class . token) (desc . "Exponential function of DOUBLE PRECISION number")))

  (setq sym (intern "DAT_LEN" starfort-mode-token-table))
  (set sym "DAT_LEN( {loc}, {len}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire primitive precision")))

  (setq sym (intern "TRN__DELIN" starfort-mode-token-table))
  (set sym "TRN__DELIN")
  (setplist sym '((class . token) (desc . "Delimiting comma invalid (error code)")))

  (setq sym (intern "SGS_REQCU" starfort-mode-token-table))
  (set sym "SGS_REQCU( {x}, {y}, {n} )")
  (setplist sym '((class . token) (desc . "Request cursor position")))

  (setq sym (intern "NUM_EXPB" starfort-mode-token-table))
  (set sym "NUM_EXPB( {num} )")
  (setplist sym '((class . token) (desc . "Exponential function of BYTE number")))

  (setq sym (intern "PGFUNY" starfort-mode-token-table))
  (set sym "PGFUNY( {fx}, {n}, {ymin}, {ymax}, {pgflag} )")
  (setplist sym '((class . token) (desc . "Function defined by X = F(Y)")))

  (setq sym (intern "PGFUNX" starfort-mode-token-table))
  (set sym "PGFUNX( {fy}, {n}, {xmin}, {xmax}, {pgflag} )")
  (setplist sym '((class . token) (desc . "Function defined by Y = F(X)")))

  (setq sym (intern "CMP__ISMAP" starfort-mode-token-table))
  (set sym "CMP__ISMAP")
  (setplist sym '((class . token) (desc . "Data currently mapped (error code)")))

  (setq sym (intern "DAT__MXDIM" starfort-mode-token-table))
  (set sym "DAT__MXDIM")
  (setplist sym '((class . token) (desc . "Maximum number of HDS object dimensions (symbolic constant)")))

  (setq sym (intern "CHR_LEN" starfort-mode-token-table))
  (set sym "CHR_LEN( {string} )")
  (setplist sym '((class . token) (desc . "Find the used length of a string")))

  (setq sym (intern "PGFUNT" starfort-mode-token-table))
  (set sym "PGFUNT( {fx}, {fy}, {n}, {tmin}, {tmax}, {pgflag} )")
  (setplist sym '((class . token) (desc . "Function defined by X = F(T), Y = G(T)")))

  (setq sym (intern "DAT_NCOMP" starfort-mode-token-table))
  (set sym "DAT_NCOMP( {loc}, {ncomp}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire number of components")))

  (setq sym (intern "ARY_MAPZ" starfort-mode-token-table))
  (set sym "ARY_MAPZ( {iary}, {type}, {mmod}, {rpntr}, {ipntr}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain complex mapped access to an array")))

  (setq sym (intern "NDF_MBNDN" starfort-mode-token-table))
  (set sym "NDF_MBNDN( {option}, {n}, {ndfs}, {status} )")
  (setplist sym '((class . token) (desc . "Match the pixel-index bounds of a number of NDFs") (helpkey . "NDF_MBNDN")))

  (setq sym (intern "SGS_REQCH" starfort-mode-token-table))
  (set sym "SGS_REQCH( {n} )")
  (setplist sym '((class . token) (desc . "Request choice")))

  (setq sym (intern "ARY_MAP" starfort-mode-token-table))
  (set sym "ARY_MAP( {iary}, {type}, {mmod}, {pntr}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain mapped access to an array")))

  (setq sym (intern "TRN_PTCL" starfort-mode-token-table))
  (set sym "TRN_PTCL( {class}, {loctr}, {status} )")
  (setplist sym '((class . token) (desc . "Put classification")))

  (setq sym (intern "DAT_CLEN" starfort-mode-token-table))
  (set sym "DAT_CLEN( {loc}, {clen}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain character string length")))

  (setq sym (intern "TRN__INDEP" starfort-mode-token-table))
  (set sym "TRN__INDEP")
  (setplist sym '((class . token) (desc . "'Preserves axis independence' classification (symbolic constant)")))

  (setq sym (intern "HDS_TRACE" starfort-mode-token-table))
  (set sym "HDS_TRACE( {loc}, {nlev}, {path}, {file}, {status} )")
  (setplist sym '((class . token) (desc . "Trace object path")))

  (setq sym (intern "VAL_NINTUW" starfort-mode-token-table))
  (set sym "VAL_NINTUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to UNSIGNED WORD value")))

  (setq sym (intern "NDF_BASE" starfort-mode-token-table))
  (set sym "NDF_BASE( {indf1}, {indf2}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain an identifier for a base NDF") (helpkey . "NDF_BASE")))

  (setq sym (intern "NDF_BAD" starfort-mode-token-table))
  (set sym "NDF_BAD( {indf}, {comp}, {check}, {bad}, {status} )")
  (setplist sym '((class . token) (desc . "Determine if an NDF array component may contain bad pixels") (helpkey . "NDF_BAD")))

  (setq sym (intern "CHR_RTOC" starfort-mode-token-table))
  (set sym "CHR_RTOC( {rvalue}, {cvalue}, {nchar} )")
  (setplist sym '((class . token) (desc . "Encode a real number as a character string")))

  (setq sym (intern "PGNUMB" starfort-mode-token-table))
  (set sym "PGNUMB( {mm}, {pp}, {form}, {string}, {nc} )")
  (setplist sym '((class . token) (desc . "Convert a number into a plottable character string")))

  (setq sym (intern "PSX_GETGID" starfort-mode-token-table))
  (set sym "PSX_GETGID( {gid}, {status} )")
  (setplist sym '((class . token) (desc . "Gets the real group ID")))

  (setq sym (intern "SGS_CLRZ" starfort-mode-token-table))
  (set sym "SGS_CLRZ")
  (setplist sym '((class . token) (desc . "Clear zone")))

  (setq sym (intern "DEFICIENCIES" starfort-mode-token-table))
  (set sym '(("DEFICIENCIES" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "FIO_ACTIV" starfort-mode-token-table))
  (set sym "FIO_ACTIV( {status} )")
  (setplist sym '((class . token) (desc . "Initialise FIO library for ADAM application")))

  (setq sym (intern "VAL_NINTUB" starfort-mode-token-table))
  (set sym "VAL_NINTUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to UNSIGNED BYTE value")))

  (setq sym (intern "VEC_MINUW" starfort-mode-token-table))
  (set sym "VEC_MINUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two UNSIGNED WORD vectorised arrays")))

  (setq sym (intern "DAT_SIZE" starfort-mode-token-table))
  (set sym "DAT_SIZE( {loc}, {size}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire object size")))

  (setq sym (intern "VEC_NINTW" starfort-mode-token-table))
  (set sym "VEC_NINTW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to WORD vectorised array")))

  (setq sym (intern "VEC_NINTR" starfort-mode-token-table))
  (set sym "VEC_NINTR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to REAL vectorised array")))

  (setq sym (intern "NDF_NBLOC" starfort-mode-token-table))
  (set sym "NDF_NBLOC( {indf}, {ndim}, {mxdim}, {nblock}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the number of blocks of adjacent pixels in an NDF") (helpkey . "NDF_NBLOC")))

  (setq sym (intern "PGWNAD" starfort-mode-token-table))
  (set sym "PGWNAD( {x1}, {x2}, {y1}, {y2} )")
  (setplist sym '((class . token) (desc . "Set window and adjust viewport to same aspect ratio")))

  (setq sym (intern "TRN__CLSIN" starfort-mode-token-table))
  (set sym "TRN__CLSIN")
  (setplist sym '((class . token) (desc . "Classification information invalid (error code)")))

  (setq sym (intern "RIO_ASSOC" starfort-mode-token-table))
  (set sym "RIO_ASSOC( {pname}, {acmode}, {form}, {recsz}, {fd}, {status} )")
  (setplist sym '((class . token) (desc . "Create/open a direct access file associated with a parameter")))

  (setq sym (intern "GKS_ERR" starfort-mode-token-table))
  (set sym "GKS_ERR")
  (setplist sym '((class . token) (desc . "GKS symbolic error code definitions (include file)")))

  (setq sym (intern "VEC_NINTI" starfort-mode-token-table))
  (set sym "VEC_NINTI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to INTEGER vectorised array")))

  (setq sym (intern "VEC_MINUB" starfort-mode-token-table))
  (set sym "VEC_MINUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two UNSIGNED BYTE vectorised arrays")))

  (setq sym (intern "GQEWK" starfort-mode-token-table))
  (set sym "GQEWK( {n}, {errind}, {number}, {wktyp} )")
  (setplist sym '((class . token) (desc . "Inquire list element of available workstation types")))

  (setq sym (intern "VEC_NINTD" starfort-mode-token-table))
  (set sym "VEC_NINTD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to DOUBLE PRECISION vectorised array")))

  (setq sym (intern "VEC_NINTB" starfort-mode-token-table))
  (set sym "VEC_NINTB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to BYTE vectorised array")))

  (setq sym (intern "FIO_ASSOC" starfort-mode-token-table))
  (set sym "FIO_ASSOC( {pname}, {acmode}, {form}, {recsz}, {fd}, {status} )")
  (setplist sym '((class . token) (desc . "Create/open a sequential file associated with a parameter")))

  (setq sym (intern "DAT__UNSET" starfort-mode-token-table))
  (set sym "DAT__UNSET")
  (setplist sym '((class . token) (desc . "Primitive data undefined (error code)")))

  (setq sym (intern "SEQUENTIAL" starfort-mode-token-table))
  (set sym "'SEQUENTIAL'")
  (setplist sym '((class . token) (desc . "'SEQUENTIAL'")))

  (setq sym (intern "NUM_UWTOW" starfort-mode-token-table))
  (set sym "NUM_UWTOW( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD number to WORD")))

  (setq sym (intern "GQGDP" starfort-mode-token-table))
  (set sym "GQGDP( {wtype}, {gdp}, {errind}, {nbnd}, {bndl} )")
  (setplist sym '((class . token) (desc . "Inquire generalised drawing primitive")))

  (setq sym (intern "NUM_UWTOR" starfort-mode-token-table))
  (set sym "NUM_UWTOR( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD number to REAL")))

  (setq sym (intern "NUM_UWTOI" starfort-mode-token-table))
  (set sym "NUM_UWTOI( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD number to INTEGER")))

  (setq sym (intern "ARY_ISBAS" starfort-mode-token-table))
  (set sym "ARY_ISBAS( {iary}, {base}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire if an array is a base array")))

  (setq sym (intern "PSX_GETENV" starfort-mode-token-table))
  (set sym "PSX_GETENV( {name}, {trans}, {status} )")
  (setplist sym '((class . token) (desc . "Translate an environment variable")))

  (setq sym (intern "NUM_UWTOD" starfort-mode-token-table))
  (set sym "NUM_UWTOD( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD number to DOUBLE PRECISION")))

  (setq sym (intern "TRN_PRFX" starfort-mode-token-table))
  (set sym "TRN_PRFX( {loctr1}, {loctr2}, {status} )")
  (setplist sym '((class . token) (desc . "Prefix transformation")))

  (setq sym (intern "GQPFAR" starfort-mode-token-table))
  (set sym "GQPFAR( {wtype}, {pfai}, {errind}, {style}, {stylid}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire predefined fill area representation")))

  (setq sym (intern "NUM_UWTOB" starfort-mode-token-table))
  (set sym "NUM_UWTOB( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD number to BYTE")))

  (setq sym (intern "PGOLIN" starfort-mode-token-table))
  (set sym "PGOLIN( {maxpt}, {npt}, {x}, {y}, {symbol} )")
  (setplist sym '((class . token) (desc . "Mark a set of points using the cursor")))

  (setq sym (intern "CHR_DCWRD" starfort-mode-token-table))
  (set sym "CHR_DCWRD( {string}, {mxwrd}, {nwrd}, {start}, {stop}, {words}, {lstat} )")
  (setplist sym '((class . token) (desc . "Return all the words in a character string")))

  (setq sym (intern "PSX_SRAND" starfort-mode-token-table))
  (set sym "PSX_SRAND( {seed}, {status} )")
  (setplist sym '((class . token) (desc . "Set the seed for the random number generator")))

  (setq sym (intern "OLD" starfort-mode-token-table))
  (set sym "'OLD'")
  (setplist sym '((class . token) (desc . "'OLD'")))

  (setq sym (intern "VEC_DIMUW" starfort-mode-token-table))
  (set sym "VEC_DIMUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two UNSIGNED WORD vectorised arrays")))

  (setq sym (intern "INTERNAL_REFERENCES" starfort-mode-token-table))
  (set sym '(("INTERNAL_REFERENCES" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "GSWKWN" starfort-mode-token-table))
  (set sym "GSWKWN( {wkid}, {xmin}, {xmax}, {ymin}, {ymax} )")
  (setplist sym '((class . token) (desc . "Set workstation window")))

  (setq sym (intern "PASSED_SUBROUTINE" starfort-mode-token-table))
  (set sym "SUBROUTINE")
  (setplist sym '((class . token) (desc . "Subroutine passed as an argument")))

  (setq sym (intern "CHR_SIZE" starfort-mode-token-table))
  (set sym "CHR_SIZE( {string} )")
  (setplist sym '((class . token) (desc . "Find the declared size of a string")))

  (setq sym (intern "APPEND" starfort-mode-token-table))
  (set sym "'APPEND'")
  (setplist sym '((class . token) (desc . "'APPEND'")))

  (setq sym (intern "GSWKVP" starfort-mode-token-table))
  (set sym "GSWKVP( {wkid}, {xmin}, {xmax}, {ymin}, {ymax} )")
  (setplist sym '((class . token) (desc . "Set workstation viewport")))

  (setq sym (intern "ARY_COPY" starfort-mode-token-table))
  (set sym "ARY_COPY( {iary1}, {place}, {iary2}, {status} )")
  (setplist sym '((class . token) (desc . "Copy an array to a new location")))

  (setq sym (intern "VEC_DIMUB" starfort-mode-token-table))
  (set sym "VEC_DIMUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two UNSIGNED BYTE vectorised arrays")))

  (setq sym (intern "GRP_GRPSZ" starfort-mode-token-table))
  (set sym "GRP_GRPSZ( {igrp}, {size}, {status} )")
  (setplist sym '((class . token) (desc . "Returns the number of names in a group")))

  (setq sym (intern "GQDVL" starfort-mode-token-table))
  (set sym "GQDVL( {wtype}, {devno}, {n}, {mldr}, {errind}, {dval}, {ol}, {pet}, {earea}, {loval}, {hival}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Inquire default valuator device data")))

  (setq sym (intern "TRN_GTCLC" starfort-mode-token-table))
  (set sym "TRN_GTCLC( {id}, {class}, {status} )")
  (setplist sym '((class . token) (desc . "Get compiled classification")))

  (setq sym (intern "GUWK" starfort-mode-token-table))
  (set sym "GUWK( {wkid}, {gks$regfl} )")
  (setplist sym '((class . token) (desc . "Update workstation")))

  (setq sym (intern "SGS_FLUSH" starfort-mode-token-table))
  (set sym "SGS_FLUSH")
  (setplist sym '((class . token) (desc . "Flush buffers")))

  (setq sym (intern "GQDST" starfort-mode-token-table))
  (set sym "GQDST( {wtype}, {devno}, {n}, {mldr}, {errind}, {mbuff}, {ol}, {pet}, {earea}, {buflen}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Inquire default string device data")))

  (setq sym (intern "GQDSK" starfort-mode-token-table))
  (set sym "GQDSK( {wtype}, {devno}, {n}, {mldr}, {errind}, {dbufsk}, {ol}, {pet}, {earea}, {buflen}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Inquire default stroke device data")))

  (setq sym (intern "ARY_ISACC" starfort-mode-token-table))
  (set sym "ARY_ISACC( {iary}, {access}, {isacc}, {status} )")
  (setplist sym '((class . token) (desc . "Determine whether a specified type of array access is available")))

  (setq sym (intern "ALGORITHM" starfort-mode-token-table))
  (set sym '(("ALGORITHM" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NUM_PWRUW" starfort-mode-token-table))
  (set sym "NUM_PWRUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Raise to a power (UNSIGNED WORD numbers)")))

  (setq sym (intern "GQFAR" starfort-mode-token-table))
  (set sym "GQFAR( {wkid}, {fai}, {gks$type}, {errind}, {style}, {stylid}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire fill area representation")))

  (setq sym (intern "GLOBAL_CONSTANTS" starfort-mode-token-table))
  (set sym '(("GLOBAL_CONSTANTS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "ERR_END" starfort-mode-token-table))
  (set sym "ERR_END( {status} )")
  (setplist sym '((class . token) (desc . "End the current error reporting environment") (helpkey . "ERR_END")))

  (setq sym (intern "GQFAI" starfort-mode-token-table))
  (set sym "GQFAI( {errind}, {index} )")
  (setplist sym '((class . token) (desc . "Inquire fill area index")))

  (setq sym (intern "AGP_SVIEW" starfort-mode-token-table))
  (set sym "AGP_SVIEW( {picnam}, {coment}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Save the current PGPLOT viewport in the database")))

  (setq sym (intern "GQFAF" starfort-mode-token-table))
  (set sym "GQFAF( {wtype}, {ni}, {nh}, {errind}, {nis}, {is}, {nhs}, {hs}, {npfai} )")
  (setplist sym '((class . token) (desc . "Inquire fill area facilities")))

  (setq sym (intern "AGP_NVIEW" starfort-mode-token-table))
  (set sym "AGP_NVIEW( {status} )")
  (setplist sym '((class . token) (desc . "Create a new PGPLOT viewport from the current picture")))

  (setq sym (intern "NUM_PWRUB" starfort-mode-token-table))
  (set sym "NUM_PWRUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Raise to a power (UNSIGNED BYTE numbers)")))

  (setq sym (intern "MSG_BLANK" starfort-mode-token-table))
  (set sym "MSG_BLANK( {status} )")
  (setplist sym '((class . token) (desc . "Output a blank line") (helpkey . "MSG_BLANK")))

  (setq sym (intern "NDF_SSARY" starfort-mode-token-table))
  (set sym "NDF_SSARY( {iary1}, {indf}, {iary2}, {status} )")
  (setplist sym '((class . token) (desc . "Create an array section, using an NDF section as a template") (helpkey . "NDF_SSARY")))

  (setq sym (intern "GRP_GROUP" starfort-mode-token-table))
  (set sym "GRP_GROUP( {param}, {igrp1}, {igrp2}, {size}, {added}, {flag}, {status} )")
  (setplist sym '((class . token) (desc . "Append a list of names obtained from the environment to a previously created group")))

  (setq sym (intern "NUM_MULW" starfort-mode-token-table))
  (set sym "NUM_MULW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Multiply two WORD numbers")))

  (setq sym (intern "GACWK" starfort-mode-token-table))
  (set sym "GACWK( {wkid} )")
  (setplist sym '((class . token) (desc . "Activate workstation")))

  (setq sym (intern "NUM_MULR" starfort-mode-token-table))
  (set sym "NUM_MULR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Multiply two REAL numbers")))

  (setq sym (intern "NUM_MULI" starfort-mode-token-table))
  (set sym "NUM_MULI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Multiply two INTEGER numbers")))

  (setq sym (intern "AGI_ANNUL" starfort-mode-token-table))
  (set sym "AGI_ANNUL( {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Annul the given picture identifier")))

  (setq sym (intern "NUM_MULD" starfort-mode-token-table))
  (set sym "NUM_MULD( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Multiply two DOUBLE PRECISION numbers")))

  (setq sym (intern "NEW" starfort-mode-token-table))
  (set sym "'NEW'")
  (setplist sym '((class . token) (desc . "'NEW'")))

  (setq sym (intern "NUM_MULB" starfort-mode-token-table))
  (set sym "NUM_MULB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Multiply two BYTE numbers")))

  (setq sym (intern "GWM_GETCI" starfort-mode-token-table))
  (set sym "GWM_GETCI( {wname}, {idim}, {indexs}, {ncols}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire the number of colours and the colour indices allocated to the given window")))

  (setq sym (intern "GACTM" starfort-mode-token-table))
  (set sym "GACTM( {min}, {x0}, {y0}, {dx}, {dy}, {phi}, {fx}, {fy}, {gks$sw}, {mout} )")
  (setplist sym '((class . token) (desc . "Accumulate transformation matrix")))

  (setq sym (intern "EMS_END" starfort-mode-token-table))
  (set sym "EMS_END( {status} )")
  (setplist sym '((class . token) (desc . "End the current error reporting environment") (helpkey . "EMS_END")))

  (setq sym (intern "GKS_CANCL" starfort-mode-token-table))
  (set sym "GKS_CANCL( {param}, {status} )")
  (setplist sym '((class . token) (desc . "Close graphics workstation and cancel parameter")))

  (setq sym (intern "CHR_RTOAN" starfort-mode-token-table))
  (set sym "CHR_RTOAN( {rvalue}, {units}, {string}, {length} )")
  (setplist sym '((class . token) (desc . "Write a real number into a string as hr/deg:min:sec")))

  (setq sym (intern "VAL_SUBUW" starfort-mode-token-table))
  (set sym "VAL_SUBUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one UNSIGNED WORD value from another")))

  (setq sym (intern "GQDLC" starfort-mode-token-table))
  (set sym "GQDLC( {wtype}, {devno}, {n}, {mldr}, {errind}, {dpx}, {dpy}, {ol}, {pet}, {earea}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Inquire default locator device data")))

  (setq sym (intern "GQECI" starfort-mode-token-table))
  (set sym "GQECI( {wkid}, {n}, {errind}, {ol}, {colind} )")
  (setplist sym '((class . token) (desc . "Inquire list element of colour indices")))

  (setq sym (intern "NUM_COSHR" starfort-mode-token-table))
  (set sym "NUM_COSHR( {num} )")
  (setplist sym '((class . token) (desc . "Hyperbolic cosine function of REAL number")))

  (setq sym (intern "PGNCURSE" starfort-mode-token-table))
  (set sym "PGNCURSE( {maxpt}, {npt}, {x}, {y}, {symbol} )")
  (setplist sym '((class . token) (desc . "Mark a set of points using the cursor")))

  (setq sym (intern "SGS_CLSWK" starfort-mode-token-table))
  (set sym "SGS_CLSWK( {izonid}, {status} )")
  (setplist sym '((class . token) (desc . "Close workstation")))

  (setq sym (intern "NUM_COSHD" starfort-mode-token-table))
  (set sym "NUM_COSHD( {num} )")
  (setplist sym '((class . token) (desc . "Hyperbolic cosine function of DOUBLE PRECISION number")))

  (setq sym (intern "VAL_SUBUB" starfort-mode-token-table))
  (set sym "VAL_SUBUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Subtract one UNSIGNED BYTE value from another")))

  (setq sym (intern "GRP_GRPEX" starfort-mode-token-table))
  (set sym "GRP_GRPEX( {grpexp}, {igrp1}, {igrp2}, {size}, {added}, {flag}, {status} )")
  (setplist sym '((class . token) (desc . "Append a list of names contained within a supplied group expression to a previously created group")))

  (setq sym (intern "REWIND" starfort-mode-token-table))
  (set sym '(("REWIND_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NOTES" starfort-mode-token-table))
  (set sym '(("NOTES" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VEC_IDVW" starfort-mode-token-table))
  (set sym "VEC_IDVW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one WORD vectorised array by another")))

  (setq sym (intern "NUM_COSDR" starfort-mode-token-table))
  (set sym "NUM_COSDR( {num} )")
  (setplist sym '((class . token) (desc . "Cosine function of REAL number (degrees)")))

  (setq sym (intern "PGSLW" starfort-mode-token-table))
  (set sym "PGSLW( {lw} )")
  (setplist sym '((class . token) (desc . "Set line width")))

  (setq sym (intern "GSWN" starfort-mode-token-table))
  (set sym "GSWN( {tnr}, {xmin}, {xmax}, {ymin}, {ymax} )")
  (setplist sym '((class . token) (desc . "Set window")))

  (setq sym (intern "VEC_IDVR" starfort-mode-token-table))
  (set sym "VEC_IDVR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one REAL vectorised array by another")))

  (setq sym (intern "PGSLS" starfort-mode-token-table))
  (set sym "PGSLS( {ls} )")
  (setplist sym '((class . token) (desc . "Set line style")))

  (setq sym (intern "GSVP" starfort-mode-token-table))
  (set sym "GSVP( {trn}, {xmin}, {xmax}, {ymin}, {ymax} )")
  (setplist sym '((class . token) (desc . "Set viewport")))

  (setq sym (intern "GQWKDU" starfort-mode-token-table))
  (set sym "GQWKDU( {wkid}, {errind}, {defmod}, {regmod}, {dempty}, {nframe} )")
  (setplist sym '((class . token) (desc . "Inquire workstation deferral and update states")))

  (setq sym (intern "FIO_READF" starfort-mode-token-table))
  (set sym "FIO_READF( {fd}, {buf}, {status} )")
  (setplist sym '((class . token) (desc . "Fast read sequential record")))

  (setq sym (intern "SGS_SHTX" starfort-mode-token-table))
  (set sym "SGS_SHTX( {ht} )")
  (setplist sym '((class . token) (desc . "Set height of text")))

  (setq sym (intern "VEC_IDVI" starfort-mode-token-table))
  (set sym "VEC_IDVI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one INTEGER vectorised array by another")))

  (setq sym (intern "GQDDS" starfort-mode-token-table))
  (set sym "GQDDS( {wtype}, {errind}, {defmod}, {regmod} )")
  (setplist sym '((class . token) (desc . "Inquire default deferral state values")))

  (setq sym (intern "NUM_COSDD" starfort-mode-token-table))
  (set sym "NUM_COSDD( {num} )")
  (setplist sym '((class . token) (desc . "Cosine function of DOUBLE PRECISION number (degrees)")))

  (setq sym (intern "FIO_DEACT" starfort-mode-token-table))
  (set sym "FIO_DEACT( {status} )")
  (setplist sym '((class . token) (desc . "Deactivate FIO")))

  (setq sym (intern "VEC_IDVD" starfort-mode-token-table))
  (set sym "VEC_IDVD( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one DOUBLE PRECISION vectorised array by another")))

  (setq sym (intern "SLA_FITXY" starfort-mode-token-table))
  (set sym "SLA_FITXY( {itype}, {np}, {xye}, {xym}, {coeffs}, {j} )")
  (setplist sym '((class . token) (desc . "Fit a linear model to relate two sets of [X,Y] coordinates.")))

  (setq sym (intern "VEC_IDVB" starfort-mode-token-table))
  (set sym "VEC_IDVB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one BYTE vectorised array by another")))

  (setq sym (intern "NDF_ISBAS" starfort-mode-token-table))
  (set sym "NDF_ISBAS( {indf}, {isbas}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire if an NDF is a base NDF") (helpkey . "NDF_ISBAS")))

  (setq sym (intern "NDF_MAPQL" starfort-mode-token-table))
  (set sym "NDF_MAPQL( {indf}, {pntr}, {el}, {bad}, {status} )")
  (setplist sym '((class . token) (desc . "Map the quality component of an NDF as an array of logical values") (helpkey . "NDF_MAPQL")))

  (setq sym (intern "GQWKCL" starfort-mode-token-table))
  (set sym "GQWKCL( {wtype}, {errind}, {vrtype} )")
  (setplist sym '((class . token) (desc . "Inquire workstation classification")))

  (setq sym (intern "ARY_TEMP" starfort-mode-token-table))
  (set sym "ARY_TEMP( {place}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain a placeholder for a temporary array")))

  (setq sym (intern "GQDCH" starfort-mode-token-table))
  (set sym "GQDCH( {wtype}, {devno}, {n}, {mldr}, {errind}, {malt}, {ol}, {pet}, {earea}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Inquire default choice device state")))

  (setq sym (intern "GQWKCA" starfort-mode-token-table))
  (set sym "GQWKCA( {wtype}, {errind}, {wkcat} )")
  (setplist sym '((class . token) (desc . "Inquire workstation category")))

  (setq sym (intern "TRN__MLPAR" starfort-mode-token-table))
  (set sym "TRN__MLPAR")
  (setplist sym '((class . token) (desc . "Missing left parenthesis (error code)")))

  (setq sym (intern "GSPARF" starfort-mode-token-table))
  (set sym "GSPARF( {rfx}, {rfy} )")
  (setplist sym '((class . token) (desc . "Set pattern reference point")))

  (setq sym (intern "GQPARF" starfort-mode-token-table))
  (set sym "GQPARF( {errind}, {rfx}, {rfy} )")
  (setplist sym '((class . token) (desc . "Inquire pattern reference point")))

  (setq sym (intern "CMP_ERR" starfort-mode-token-table))
  (set sym "CMP_ERR")
  (setplist sym '((class . token) (desc . "CMP__ error code definitions (include file)")))

  (setq sym (intern "DAT_GET" starfort-mode-token-table))
  (set sym "DAT_GET( {loc}, {type}, {ndim}, {dim}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive")))

  (setq sym (intern "NDF_XNEW" starfort-mode-token-table))
  (set sym "NDF_XNEW( {indf}, {xname}, {type}, {ndim}, {dim}, {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Create a new extension in an NDF") (helpkey . "NDF_XNEW")))

  (setq sym (intern "GQCHS" starfort-mode-token-table))
  (set sym "GQCHS( {wkid}, {chdnr}, {mldr}, {errind}, {mode}, {esw}, {ichnr}, {pet}, {earea}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Inquire choice device state")))

  (setq sym (intern "FIO_OPEN" starfort-mode-token-table))
  (set sym "FIO_OPEN( {file}, {acmode}, {form}, {recsz}, {fd}, {status} )")
  (setplist sym '((class . token) (desc . "Create/open a sequential file")))

  (setq sym (intern "LOCAL_CONSTANTS" starfort-mode-token-table))
  (set sym '(("LOCAL_CONSTANTS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VEC_WTOW" starfort-mode-token-table))
  (set sym "VEC_WTOW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD vectorised array to WORD")))

  (setq sym (intern "PGSFS" starfort-mode-token-table))
  (set sym "PGSFS( {fs} )")
  (setplist sym '((class . token) (desc . "Set fill-area style")))

  (setq sym (intern "PGQVP" starfort-mode-token-table))
  (set sym "PGQVP( {units}, {x1}, {x2}, {y1}, {y2} )")
  (setplist sym '((class . token) (desc . "Inquire viewport size and position")))

  (setq sym (intern "GQCHH" starfort-mode-token-table))
  (set sym "GQCHH( {errind}, {chh} )")
  (setplist sym '((class . token) (desc . "Inquire character height")))

  (setq sym (intern "VEC_WTOR" starfort-mode-token-table))
  (set sym "VEC_WTOR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD vectorised array to REAL")))

  (setq sym (intern "SGS_SELCH" starfort-mode-token-table))
  (set sym "SGS_SELCH( {nchdev} )")
  (setplist sym '((class . token) (desc . "Select choice device")))

  (setq sym (intern "VEC_WTOI" starfort-mode-token-table))
  (set sym "VEC_WTOI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD vectorised array to INTEGER")))

  (setq sym (intern "PGPAGE" starfort-mode-token-table))
  (set sym "PGPAGE")
  (setplist sym '((class . token) (desc . "Advance to new page")))

  (setq sym (intern "PGRND" starfort-mode-token-table))
  (set sym "PGRND( {x}, {nsub} )")
  (setplist sym '((class . token) (desc . "Find the smallest ROUND number greater than x")))

  (setq sym (intern "DAT_ERR" starfort-mode-token-table))
  (set sym "DAT_ERR")
  (setplist sym '((class . token) (desc . "DAT__ error code definitions (include file)")))

  (setq sym (intern "VEC_WTOD" starfort-mode-token-table))
  (set sym "VEC_WTOD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD vectorised array to DOUBLE PRECISION")))

  (setq sym (intern "GSPA" starfort-mode-token-table))
  (set sym "GSPA( {szx}, {szy} )")
  (setplist sym '((class . token) (desc . "Set pattern size")))

  (setq sym (intern "VEC_WTOB" starfort-mode-token-table))
  (set sym "VEC_WTOB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD vectorised array to BYTE")))

  (setq sym (intern "DAT_STATE" starfort-mode-token-table))
  (set sym "DAT_STATE( {loc}, {reply}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire object state")))

  (setq sym (intern "TRN__NEGDT" starfort-mode-token-table))
  (set sym "TRN__NEGDT")
  (setplist sym '((class . token) (desc . "'Reflection is present' classification (symbolic constant)")))

  (setq sym (intern "PGSCR" starfort-mode-token-table))
  (set sym "PGSCR( {ci}, {cr}, {cg}, {cb} )")
  (setplist sym '((class . token) (desc . "Set color representation")))

  (setq sym (intern "NUM_LG10UW" starfort-mode-token-table))
  (set sym "NUM_LG10UW( {num} )")
  (setplist sym '((class . token) (desc . "Common logarithm of UNSIGNED WORD number")))

  (setq sym (intern "NDF_ISACC" starfort-mode-token-table))
  (set sym "NDF_ISACC( {indf}, {access}, {isacc}, {status} )")
  (setplist sym '((class . token) (desc . "Determine whether a specified type of NDF access is available") (helpkey . "NDF_ISACC")))

  (setq sym (intern "GSMK" starfort-mode-token-table))
  (set sym "GSMK( {gks$mtype} )")
  (setplist sym '((class . token) (desc . "Set marker type")))

  (setq sym (intern "PGSCI" starfort-mode-token-table))
  (set sym "PGSCI( {ci} )")
  (setplist sym '((class . token) (desc . "Set color index")))

  (setq sym (intern "IDI_CANCL" starfort-mode-token-table))
  (set sym "IDI_CANCL( {pname}, {status} )")
  (setplist sym '((class . token) (desc . "Cancel the ADAM device parameter")))

  (setq sym (intern "PGSCH" starfort-mode-token-table))
  (set sym "PGSCH( {size} )")
  (setplist sym '((class . token) (desc . "Set character height")))

  (setq sym (intern "SGS_INCHO" starfort-mode-token-table))
  (set sym "SGS_INCHO( {nchoic}, {n} )")
  (setplist sym '((class . token) (desc . "Inquire number of choices")))

  (setq sym (intern "PGSCF" starfort-mode-token-table))
  (set sym "PGSCF( {if} )")
  (setplist sym '((class . token) (desc . "Set character font")))

  (setq sym (intern "GSLN" starfort-mode-token-table))
  (set sym "GSLN( {gks$ltype} )")
  (setplist sym '((class . token) (desc . "Set linetype")))

  (setq sym (intern "MACHINE_SPECIFICS" starfort-mode-token-table))
  (set sym '(("MACHINE_SPECIFICS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "GQASF" starfort-mode-token-table))
  (set sym "GQASF( {errind}, {lasf} )")
  (setplist sym '((class . token) (desc . "Inquire aspect source flags")))

  (setq sym (intern "VEC_SIGNUW" starfort-mode-token-table))
  (set sym "VEC_SIGNUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one UNSIGNED WORD vectorised array to another")))

  (setq sym (intern "NUM_LG10UB" starfort-mode-token-table))
  (set sym "NUM_LG10UB( {num} )")
  (setplist sym '((class . token) (desc . "Common logarithm of UNSIGNED BYTE number")))

  (setq sym (intern "NDF_XLOC" starfort-mode-token-table))
  (set sym "NDF_XLOC( {indf}, {xname}, {mode}, {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain access to a named NDF extension via an HDS locator") (helpkey . "NDF_XLOC")))

  (setq sym (intern "VEC_SIGNUB" starfort-mode-token-table))
  (set sym "VEC_SIGNUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one UNSIGNED BYTE vectorised array to another")))

  (setq sym (intern "FILE_PAR" starfort-mode-token-table))
  (set sym '(("FILE_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "AGD_SWIND" starfort-mode-token-table))
  (set sym "AGD_SWIND( {dispid}, {memid}, {xsize}, {ysize}, {xoff}, {yoff}, {pname}, {coment}, {wx1}, {wx2}, {wy1}, {wy2}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Save an IDI window in the database")))

  (setq sym (intern "GOPWK" starfort-mode-token-table))
  (set sym "GOPWK( {wkid}, {conid}, {wtype} )")
  (setplist sym '((class . token) (desc . "Open workstation")))

  (setq sym (intern "PSX_TTYNAME" starfort-mode-token-table))
  (set sym "PSX_TTYNAME( {fildsc}, {tname}, {status} )")
  (setplist sym '((class . token) (desc . "Get the name of the terminal")))

  (setq sym (intern "PGQLW" starfort-mode-token-table))
  (set sym "PGQLW( {lw} )")
  (setplist sym '((class . token) (desc . "Inquire line width")))

  (setq sym (intern "AGD_NWIND" starfort-mode-token-table))
  (set sym "AGD_NWIND( {memid}, {dispid}, {xsize}, {ysize}, {xoff}, {yoff}, {status} )")
  (setplist sym '((class . token) (desc . "Define an IDI window from the current picture")))

  (setq sym (intern "PGQLS" starfort-mode-token-table))
  (set sym "PGQLS( {ls} )")
  (setplist sym '((class . token) (desc . "Inquire line style")))

  (setq sym (intern "WHILE" starfort-mode-token-table))
  (set sym '(lambda nil (let (indent) (starfort-indent-line) (setq indent (point)) (insert "CONTINUE             ! Start of 'DO WHILE' loop") (starfort-indent-line) (starfort-break-line (quote code)) (insert "IF ( {logical_exp} ) THEN") (starfort-break-line (quote code)) (insert "{executable_statement}...") (starfort-break-line (quote code)) (insert "GO TO {lab}") (starfort-break-line (quote code)) (insert "END IF") (starfort-indent-line) (save-excursion (goto-char indent) (setq indent (current-column)) (delete-region (save-excursion (beginning-of-line) (point)) (point)) (insert " {lab}") (insert-char 32 (- indent 6)))) t))
  (setplist sym '((class . token) (desc . "DO WHILE loop implemented in Standard Fortran 77")))

  (setq sym (intern "SGS_ZONE" starfort-mode-token-table))
  (set sym "SGS_ZONE( {x1}, {x2}, {y1}, {y2}, {izonid}, {status} )")
  (setplist sym '((class . token) (desc . "Create a zone")))

  (setq sym (intern "VEC_ADDW" starfort-mode-token-table))
  (set sym "VEC_ADDW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Add two WORD vectorised arrays")))

  (setq sym (intern "VEC_ADDR" starfort-mode-token-table))
  (set sym "VEC_ADDR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Add two REAL vectorised arrays")))

  (setq sym (intern "DAT_CELL" starfort-mode-token-table))
  (set sym "DAT_CELL( {loc1}, {ndim}, {sub}, {loc2}, {status} )")
  (setplist sym '((class . token) (desc . "Locate cell")))

  (setq sym (intern "GSDS" starfort-mode-token-table))
  (set sym "GSDS( {wkid}, {gks$defmod}, {gks$regmod} )")
  (setplist sym '((class . token) (desc . "Set deferral state")))

  (setq sym (intern "VEC_ABSW" starfort-mode-token-table))
  (set sym "VEC_ABSW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of WORD vectorised array")))

  (setq sym (intern "NDF_ACRE" starfort-mode-token-table))
  (set sym "NDF_ACRE( {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Ensure that an axis coordinate system exists for an NDF") (helpkey . "NDF_ACRE")))

  (setq sym (intern "VEC_ABSR" starfort-mode-token-table))
  (set sym "VEC_ABSR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of REAL vectorised array")))

  (setq sym (intern "VEC_ADDI" starfort-mode-token-table))
  (set sym "VEC_ADDI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Add two INTEGER vectorised arrays")))

  (setq sym (intern "GSCR" starfort-mode-token-table))
  (set sym "GSCR( {wkid}, {coli}, {cr}, {cg}, {cb} )")
  (setplist sym '((class . token) (desc . "Set color representation")))

  (setq sym (intern "VEC_ADDD" starfort-mode-token-table))
  (set sym "VEC_ADDD( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Add two DOUBLE PRECISION vectorised arrays")))

  (setq sym (intern "ARY_VALID" starfort-mode-token-table))
  (set sym "ARY_VALID( {iary}, {valid}, {status} )")
  (setplist sym '((class . token) (desc . "Determine whether an array identifier is valid")))

  (setq sym (intern "GSVPIP" starfort-mode-token-table))
  (set sym "GSVPIP( {tnr}, {rtnr}, {gks$relpri} )")
  (setplist sym '((class . token) (desc . "Set viewport input priority")))

  (setq sym (intern "VEC_ADDB" starfort-mode-token-table))
  (set sym "VEC_ADDB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Add two BYTE vectorised arrays")))

  (setq sym (intern "VEC_ABSI" starfort-mode-token-table))
  (set sym "VEC_ABSI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of INTEGER vectorised array")))

  (setq sym (intern "TRN_GTNV" starfort-mode-token-table))
  (set sym "TRN_GTNV( {loctr}, {nvin}, {nvout}, {status} )")
  (setplist sym '((class . token) (desc . "Get numbers of variables")))

  (setq sym (intern "VEC_EXPUW" starfort-mode-token-table))
  (set sym "VEC_EXPUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of UNSIGNED WORD vectorised array")))

  (setq sym (intern "VEC_ABSD" starfort-mode-token-table))
  (set sym "VEC_ABSD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of DOUBLE PRECISION vectorised array")))

  (setq sym (intern "VEC_ABSB" starfort-mode-token-table))
  (set sym "VEC_ABSB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of BYTE vectorised array")))

  (setq sym (intern "SGS_WNAME" starfort-mode-token-table))
  (set sym "SGS_WNAME( {actrou}, {iarg}, {status} )")
  (setplist sym '((class . token) (desc . "Generate list of workstation names")))

  (setq sym (intern "GQPX" starfort-mode-token-table))
  (set sym "GQPX( {wkid}, {px}, {py}, {errind}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire pixel")))

  (setq sym (intern "ERR_SYSER" starfort-mode-token-table))
  (set sym "ERR_SYSER( {token}, {systat} )")
  (setplist sym '((class . token) (desc . "Assign an operating system error message to a token") (helpkey . "ERR_SYSER")))

  (setq sym (intern "PGQFS" starfort-mode-token-table))
  (set sym "PGQFS( {fs} )")
  (setplist sym '((class . token) (desc . "Inquire fill-area style")))

  (setq sym (intern "IIMWMY" starfort-mode-token-table))
  (set sym "IIMWMY( {dispid}, {memid}, {image}, {npix}, {depth}, {pack}, {xstart}, {ystart}, {status} )")
  (setplist sym '((class . token) (desc . "Write Memory")))

  (setq sym (intern "VAL_IDVW" starfort-mode-token-table))
  (set sym "VAL_IDVW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one WORD value by another")))

  (setq sym (intern "SGS_CLRFG" starfort-mode-token-table))
  (set sym "SGS_CLRFG( {iflag} )")
  (setplist sym '((class . token) (desc . "Set clear screen flag")))

  (setq sym (intern "VAL_IDVR" starfort-mode-token-table))
  (set sym "VAL_IDVR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one REAL value by another")))

  (setq sym (intern "PAR_STATE" starfort-mode-token-table))
  (set sym "PAR_STATE( {param}, {state}, {status} )")
  (setplist sym '((class . token) (desc . "Return the current state of a parameter")))

  (setq sym (intern "VEC_EXPUB" starfort-mode-token-table))
  (set sym "VEC_EXPUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of UNSIGNED BYTE vectorised array")))

  (setq sym (intern "ARY_SIZE" starfort-mode-token-table))
  (set sym "ARY_SIZE( {iary}, {npix}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the size of an array")))

  (setq sym (intern "VAL_IDVI" starfort-mode-token-table))
  (set sym "VAL_IDVI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one INTEGER value by another")))

  (setq sym (intern "GQNT" starfort-mode-token-table))
  (set sym "GQNT( {ntnr}, {errind}, {window}, {viewpt} )")
  (setplist sym '((class . token) (desc . "Inquire normalisation transformation")))

  (setq sym (intern "CHR_RMBLK" starfort-mode-token-table))
  (set sym "CHR_RMBLK( {string} )")
  (setplist sym '((class . token) (desc . "Remove all blanks from a string in situ")))

  (setq sym (intern "SLA_EARTH" starfort-mode-token-table))
  (set sym "SLA_EARTH( {iy}, {id}, {fd}, {posvel} )")
  (setplist sym '((class . token) (desc . "Approximate heliocentric position and velocity of the Earth")))

  (setq sym (intern "GQPA" starfort-mode-token-table))
  (set sym "GQPA( {errind}, {szx}, {szy} )")
  (setplist sym '((class . token) (desc . "Inquire pattern size")))

  (setq sym (intern "VAL_IDVD" starfort-mode-token-table))
  (set sym "VAL_IDVD( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one DOUBLE PRECISION value by another")))

  (setq sym (intern "NDF_AREST" starfort-mode-token-table))
  (set sym "NDF_AREST( {indf}, {comp}, {iaxis}, {status} )")
  (setplist sym '((class . token) (desc . "Reset an NDF axis component to an undefined state") (helpkey . "NDF_AREST")))

  (setq sym (intern "VAL_IDVB" starfort-mode-token-table))
  (set sym "VAL_IDVB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one BYTE value by another")))

  (setq sym (intern "GRP_LISTF" starfort-mode-token-table))
  (set sym "GRP_LISTF( {filenm}, {indxlo}, {indxhi}, {comnt}, {igrp}, {status} )")
  (setplist sym '((class . token) (desc . "Write names to a specified text file")))

  (setq sym (intern "CONTINUE" starfort-mode-token-table))
  (set sym "\\ [lbl] CONTINUE	")
  (setplist sym '((class . token) (desc . "CONTINUE statement")))

  (setq sym (intern "PGQCR" starfort-mode-token-table))
  (set sym "PGQCR")
  (setplist sym '((class . token) (desc . "Inquire color representation")))

  (setq sym (intern "SLA_DAV2M" starfort-mode-token-table))
  (set sym "SLA_DAV2M( {axvec}, {rmat} )")
  (setplist sym '((class . token) (desc . "Form the rotation matrix corresponding to a given axial vector.")))

  (setq sym (intern "SGS_CLRBL" starfort-mode-token-table))
  (set sym "SGS_CLRBL( {x1}, {x2}, {y1}, {y2} )")
  (setplist sym '((class . token) (desc . "Clear block")))

  (setq sym (intern "GQMK" starfort-mode-token-table))
  (set sym "GQMK( {errind}, {mtype} )")
  (setplist sym '((class . token) (desc . "Inquire markertype")))

  (setq sym (intern "GOPKS" starfort-mode-token-table))
  (set sym "GOPKS( {errfil} )")
  (setplist sym '((class . token) (desc . "Open GKS")))

  (setq sym (intern "PGQCI" starfort-mode-token-table))
  (set sym "PGQCI( {ci} )")
  (setplist sym '((class . token) (desc . "Inquire color index")))

  (setq sym (intern "PGQCH" starfort-mode-token-table))
  (set sym "PGQCH( {size} )")
  (setplist sym '((class . token) (desc . "Inquire character height")))

  (setq sym (intern "MAG_JEOV" starfort-mode-token-table))
  (set sym "MAG_JEOV( {td}, {status} )")
  (setplist sym '((class . token) (desc . "Jump over an EOV condition (2 consecutive tape marks)")))

  (setq sym (intern "MONOLITH_SECTIONS" starfort-mode-token-table))
  (set sym '(("MONOLITH_SECTIONS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "PGQCF" starfort-mode-token-table))
  (set sym "PGQCF( {if} )")
  (setplist sym '((class . token) (desc . "Inquire character font")))

  (setq sym (intern "GQLN" starfort-mode-token-table))
  (set sym "GQLN( {errind}, {ltype} )")
  (setplist sym '((class . token) (desc . "Inquire linetype")))

  (setq sym (intern "GQLI" starfort-mode-token-table))
  (set sym "GQLI( {wtype}, {errind}, {nlcd}, {nskd}, {nvld}, {nchd}, {npcd}, {nstd} )")
  (setplist sym '((class . token) (desc . "Inquire number of available logical input devices")))

  (setq sym (intern "VAL_WTOW" starfort-mode-token-table))
  (set sym "VAL_WTOW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD value to WORD")))

  (setq sym (intern "PGETXT" starfort-mode-token-table))
  (set sym "PGETXT")
  (setplist sym '((class . token) (desc . "Erase text from graphics display")))

  (setq sym (intern "PSX_TIME" starfort-mode-token-table))
  (set sym "PSX_TIME( {nticks}, {status} )")
  (setplist sym '((class . token) (desc . "Get the current calendar time")))

  (setq sym (intern "VAL_WTOR" starfort-mode-token-table))
  (set sym "VAL_WTOR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD value to REAL")))

  (setq sym (intern "TRN__DSTIN" starfort-mode-token-table))
  (set sym "TRN__DSTIN")
  (setplist sym '((class . token) (desc . "Definition status invalid (error code)")))

  (setq sym (intern "SGS_ATXR" starfort-mode-token-table))
  (set sym "SGS_ATXR( {r}, {nfi}, {ndp} )")
  (setplist sym '((class . token) (desc . "Append to text a real number")))

  (setq sym (intern "PGBEGIN" starfort-mode-token-table))
  (set sym "PGBEGIN( {unit}, {file}, {nxsub}, {nysub} )")
  (setplist sym '((class . token) (desc . "Begin PGPLOT, open output device")))

  (setq sym (intern "SLA_AV2M" starfort-mode-token-table))
  (set sym "SLA_AV2M( {axvec}, {rmat} )")
  (setplist sym '((class . token) (desc . "Form the rotation matrix corresponding to a given axial vector.")))

  (setq sym (intern "SGS_ATXL" starfort-mode-token-table))
  (set sym "SGS_ATXL( {string} )")
  (setplist sym '((class . token) (desc . "Append to text left justified")))

  (setq sym (intern "VAL_WTOI" starfort-mode-token-table))
  (set sym "VAL_WTOI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD value to INTEGER")))

  (setq sym (intern "NDF_CHUNK" starfort-mode-token-table))
  (set sym "NDF_CHUNK( {indf1}, {mxpix}, {ichunk}, {indf2}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain an NDF section containing a chunk of contiguous pixels") (helpkey . "NDF_CHUNK")))

  (setq sym (intern "DAT_DEF" starfort-mode-token-table))
  (set sym "DAT_DEF( {param}, {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Set an ADAM parameter system default to be a data object")))

  (setq sym (intern "DAT__MODIN" starfort-mode-token-table))
  (set sym "DAT__MODIN")
  (setplist sym '((class . token) (desc . "Mode invalid (error code)")))

  (setq sym (intern "SGS_ATXI" starfort-mode-token-table))
  (set sym "SGS_ATXI( {i}, {nfi} )")
  (setplist sym '((class . token) (desc . "Append to text an integer")))

  (setq sym (intern "VAL_WTOD" starfort-mode-token-table))
  (set sym "VAL_WTOD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD value to DOUBLE PRECISION")))

  (setq sym (intern "UNFORMATTED" starfort-mode-token-table))
  (set sym "'UNFORMATTED'")
  (setplist sym '((class . token) (desc . "'UNFORMATTED'")))

  (setq sym (intern "VAL_WTOB" starfort-mode-token-table))
  (set sym "VAL_WTOB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert a WORD value to BYTE")))

  (setq sym (intern "SGS_ITXB" starfort-mode-token-table))
  (set sym "SGS_ITXB( {x}, {y}, {n}, {dx}, {dy} )")
  (setplist sym '((class . token) (desc . "Inquire text buffer")))

  (setq sym (intern "SGS_ITXA" starfort-mode-token-table))
  (set sym "SGS_ITXA( {nf}, {npr}, {ht}, {ar}, {xu}, {yu}, {sp}, {txj} )")
  (setplist sym '((class . token) (desc . "Inquire text attributes")))

  (setq sym (intern "SGS_ATXB" starfort-mode-token-table))
  (set sym "SGS_ATXB( {string}, {nspace} )")
  (setplist sym '((class . token) (desc . "Append to text with blanks")))

  (setq sym (intern "ARY_DIM" starfort-mode-token-table))
  (set sym "ARY_DIM( {iary}, {ndimx}, {dim}, {ndim}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire the dimension sizes of an array")))

  (setq sym (intern "SLA_DRANRM" starfort-mode-token-table))
  (set sym "SLA_DRANRM( {angle} )")
  (setplist sym '((class . token) (desc . "Normalise angle into range 0-2 pi (double precision)")))

  (setq sym (intern "TRN_GTCL" starfort-mode-token-table))
  (set sym "TRN_GTCL( {loctr}, {forwd}, {class}, {status} )")
  (setplist sym '((class . token) (desc . "Get classification")))

  (setq sym (intern "SGS_SELZ" starfort-mode-token-table))
  (set sym "SGS_SELZ( {izonid}, {status} )")
  (setplist sym '((class . token) (desc . "Select zone")))

  (setq sym (intern "VEC_NINTUW" starfort-mode-token-table))
  (set sym "VEC_NINTUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to UNSIGNED WORD vectorised array")))

  (setq sym (intern "AGI_END" starfort-mode-token-table))
  (set sym "AGI_END( {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Mark the end of an AGI scope")))

  (setq sym (intern "SGS_CLOSE" starfort-mode-token-table))
  (set sym "SGS_CLOSE")
  (setplist sym '((class . token) (desc . "Close all graphics (including GKS)")))

  (setq sym (intern "CHR_ITOC" starfort-mode-token-table))
  (set sym "CHR_ITOC( {ivalue}, {cvalue}, {nchar} )")
  (setplist sym '((class . token) (desc . "Encode an integer number as a character string")))

  (setq sym (intern "VEC_SIGNW" starfort-mode-token-table))
  (set sym "VEC_SIGNW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one WORD vectorised array to another")))

  (setq sym (intern "SLA_DBJIN" starfort-mode-token-table))
  (set sym "SLA_DBJIN( {string}, {nstrt}, {dreslt}, {j1}, {j2} )")
  (setplist sym '((class . token) (desc . "Convert free-format input into double precision floating point, using DFLTIN but with special syntax extensions.")))

  (setq sym (intern "GTX" starfort-mode-token-table))
  (set sym "GTX( {px}, {py}, {chars} )")
  (setplist sym '((class . token) (desc . "Text")))

  (setq sym (intern "VEC_SIGNR" starfort-mode-token-table))
  (set sym "VEC_SIGNR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one REAL vectorised array to another")))

  (setq sym (intern "GQCR" starfort-mode-token-table))
  (set sym "GQCR( {wkid}, {coli}, {gks$type}, {errind}, {red}, {green}, {blue} )")
  (setplist sym '((class . token) (desc . "Inquire colour representation")))

  (setq sym (intern "FORMATTED" starfort-mode-token-table))
  (set sym "'FORMATTED'")
  (setplist sym '((class . token) (desc . "'FORMATTED'")))

  (setq sym (intern "VEC_NINTUB" starfort-mode-token-table))
  (set sym "VEC_NINTUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to UNSIGNED BYTE vectorised array")))

  (setq sym (intern "VEC_SIGNI" starfort-mode-token-table))
  (set sym "VEC_SIGNI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one INTEGER vectorised array to another")))

  (setq sym (intern "VEC_SIGND" starfort-mode-token-table))
  (set sym "VEC_SIGND( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one DOUBLE PRECISION vectorised array to another")))

  (setq sym (intern "GQCF" starfort-mode-token-table))
  (set sym "GQCF( {wtype}, {errind}, {ncoli}, {cola}, {npci} )")
  (setplist sym '((class . token) (desc . "Inquire colour facilities")))

  (setq sym (intern "VEC_SIGNB" starfort-mode-token-table))
  (set sym "VEC_SIGNB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Transfer of sign from one BYTE vectorised array to another")))

  (setq sym (intern "PGWINDOW" starfort-mode-token-table))
  (set sym "PGWINDOW( {x1}, {x2}, {y1}, {y2} )")
  (setplist sym '((class . token) (desc . "Set window")))

  (setq sym (intern "MAG_ALOC" starfort-mode-token-table))
  (set sym "MAG_ALOC( {param}, {status} )")
  (setplist sym '((class . token) (desc . "Allocate a tape drive for continued use")))

  (setq sym (intern "NUM_INTUW" starfort-mode-token-table))
  (set sym "NUM_INTUW( {num} )")
  (setplist sym '((class . token) (desc . "Truncate UNSIGNED WORD number to an integer")))

  (setq sym (intern "INCLUDE" starfort-mode-token-table))
  (set sym "INCLUDE '{file_spec}'")
  (setplist sym '((class . token) (desc . "INCLUDE '{file_spec}'")))

  (setq sym (intern "WORD" starfort-mode-token-table))
  (set sym "INTEGER * 2")
  (setplist sym '((class . token) (desc . "INTEGER * 2 data type")))

  (setq sym (intern "GQETXI" starfort-mode-token-table))
  (set sym "GQETXI( {wkid}, {n}, {errind}, {ol}, {txind} )")
  (setplist sym '((class . token) (desc . "Inquire list element of text indices")))

  (setq sym (intern "MAG_ASSOC" starfort-mode-token-table))
  (set sym "MAG_ASSOC( {param}, {mode}, {td}, {status} )")
  (setplist sym '((class . token) (desc . "Open a tape device and return a descriptor")))

  (setq sym (intern "DAT__LOCIN" starfort-mode-token-table))
  (set sym "DAT__LOCIN")
  (setplist sym '((class . token) (desc . "Locator invalid (error code)")))

  (setq sym (intern "NUM_INTUB" starfort-mode-token-table))
  (set sym "NUM_INTUB( {num} )")
  (setplist sym '((class . token) (desc . "Truncate UNSIGNED BYTE number to an integer")))

  (setq sym (intern "CMP__FATAL" starfort-mode-token-table))
  (set sym "CMP__FATAL")
  (setplist sym '((class . token) (desc . "Fatal internal error (error code)")))

  (setq sym (intern "UNIT_PAR" starfort-mode-token-table))
  (set sym '(("UNIT_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "GPM" starfort-mode-token-table))
  (set sym "GPM( {np}, {pxa}, {pya} )")
  (setplist sym '((class . token) (desc . "Polymarker")))

  (setq sym (intern "TRN__ISOT" starfort-mode-token-table))
  (set sym "TRN__ISOT")
  (setplist sym '((class . token) (desc . "'Preserves angles and shapes' classification (symbolic constant)")))

  (setq sym (intern "GPL" starfort-mode-token-table))
  (set sym "GPL( {np}, {pxa}, {pya} )")
  (setplist sym '((class . token) (desc . "Polyline")))

  (setq sym (intern "VAL_ADDW" starfort-mode-token-table))
  (set sym "VAL_ADDW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Add two WORD values")))

  (setq sym (intern "EMS_ANNUL" starfort-mode-token-table))
  (set sym "EMS_ANNUL( {status} )")
  (setplist sym '((class . token) (desc . "Annul the contents of the current error context") (helpkey . "EMS_ANNUL")))

  (setq sym (intern "VAL_ADDR" starfort-mode-token-table))
  (set sym "VAL_ADDR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Add two REAL values")))

  (setq sym (intern "VAL_LG10W" starfort-mode-token-table))
  (set sym "VAL_LG10W( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of WORD value")))

  (setq sym (intern "VAL_ABSW" starfort-mode-token-table))
  (set sym "VAL_ABSW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of WORD value")))

  (setq sym (intern "SLA_DM2AV" starfort-mode-token-table))
  (set sym "SLA_DM2AV( {rmat}, {axvec} )")
  (setplist sym '((class . token) (desc . "From a rotation matrix, determine the corresponding axial vector.")))

  (setq sym (intern "PSX_ISATTY" starfort-mode-token-table))
  (set sym "PSX_ISATTY( {fildsc}, {istty}, {status} )")
  (setplist sym '((class . token) (desc . "Determine if a file is a terminal")))

  (setq sym (intern "OPEN" starfort-mode-token-table))
  (set sym '(("OPEN_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SLA_OAPQK" starfort-mode-token-table))
  (set sym "SLA_OAPQK( {type}, {ob1}, {ob2}, {aoprms}, {rap}, {dap} )")
  (setplist sym '((class . token) (desc . "Quick observed to apparent place")))

  (setq sym (intern "SLA_MAPQK" starfort-mode-token-table))
  (set sym "SLA_MAPQK( {rm}, {dm}, {pr}, {pd}, {px}, {rv}, {amprms}, {ra}, {da} )")
  (setplist sym '((class . token) (desc . "Quick mean to apparent place")))

  (setq sym (intern "VAL_LG10R" starfort-mode-token-table))
  (set sym "VAL_LG10R( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of REAL value")))

  (setq sym (intern "VAL_ABSR" starfort-mode-token-table))
  (set sym "VAL_ABSR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of REAL value")))

  (setq sym (intern "DAT__VERMM" starfort-mode-token-table))
  (set sym "DAT__VERMM")
  (setplist sym '((class . token) (desc . "Version mismatch (error code)")))

  (setq sym (intern "VAL_ADDI" starfort-mode-token-table))
  (set sym "VAL_ADDI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Add two INTEGER values")))

  (setq sym (intern "NDF_VALID" starfort-mode-token-table))
  (set sym "NDF_VALID( {indf}, {valid}, {status} )")
  (setplist sym '((class . token) (desc . "Determine whether an NDF identifier is valid") (helpkey . "NDF_VALID")))

  (setq sym (intern "AGI_TNEW" starfort-mode-token-table))
  (set sym "AGI_TNEW( {ncd}, {ncw}, {dtow}, {wtod}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Store a transformation in the database")))

  (setq sym (intern "NDF_CREAT" starfort-mode-token-table))
  (set sym "NDF_CREAT( {param}, {ftype}, {ndim}, {lbnd}, {ubnd}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Create a new simple NDF via the ADAM parameter system") (helpkey . "NDF_CREAT")))

  (setq sym (intern "MAG_READ" starfort-mode-token-table))
  (set sym "MAG_READ( {td}, {maxval}, {values}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Read a block from a tape")))

  (setq sym (intern "VAL_ADDD" starfort-mode-token-table))
  (set sym "VAL_ADDD( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Add two DOUBLE PRECISION values")))

  (setq sym (intern "VAL_LG10I" starfort-mode-token-table))
  (set sym "VAL_LG10I( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of INTEGER value")))

  (setq sym (intern "VAL_ADDB" starfort-mode-token-table))
  (set sym "VAL_ADDB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Add two BYTE values")))

  (setq sym (intern "VAL_ABSI" starfort-mode-token-table))
  (set sym "VAL_ABSI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of INTEGER value")))

  (setq sym (intern "DAT_ERMSG" starfort-mode-token-table))
  (set sym "DAT_ERMSG( {status}, {length}, {msg} )")
  (setplist sym '((class . token) (desc . "Translate a status value into an error message")))

  (setq sym (intern "PSX_GETEUID" starfort-mode-token-table))
  (set sym "PSX_GETEUID( {uid}, {status} )")
  (setplist sym '((class . token) (desc . "Gets the effective user ID")))

  (setq sym (intern "_BAD_BLOCK" starfort-mode-token-table))
  (set sym "IF ( STATUS .NE. SAI__OK ) THEN
{executable_statement}...
END IF	")
  (setplist sym '((class . token) (desc . "IF block which executes if STATUS is bad")))

  (setq sym (intern "VAL_LG10D" starfort-mode-token-table))
  (set sym "VAL_LG10D( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of DOUBLE PRECISION value")))

  (setq sym (intern "VAL_ABSD" starfort-mode-token-table))
  (set sym "VAL_ABSD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of DOUBLE PRECISION value")))

  (setq sym (intern "VAL_LG10B" starfort-mode-token-table))
  (set sym "VAL_LG10B( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of BYTE value")))

  (setq sym (intern "VAL_ABSB" starfort-mode-token-table))
  (set sym "VAL_ABSB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of BYTE value")))

  (setq sym (intern "SLA_MAPPA" starfort-mode-token-table))
  (set sym "SLA_MAPPA( {eq}, {date}, {amprms} )")
  (setplist sym '((class . token) (desc . "Compute star-independent parameters in preparation for conversions between mean place and geocentric apparent place.")))

  (setq sym (intern "SLA_DRANGE" starfort-mode-token-table))
  (set sym "SLA_DRANGE( {angle} )")
  (setplist sym '((class . token) (desc . "Normalise angle into range +/- pi (double precision)")))

  (setq sym (intern "OPENED_PAR" starfort-mode-token-table))
  (set sym '(("OPENED_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "PAR_PROMT" starfort-mode-token-table))
  (set sym "PAR_PROMT( {param}, {prompt}, {status} )")
  (setplist sym '((class . token) (desc . "Set a new prompt string for a parameter")))

  (setq sym (intern "CMP_SHAPE" starfort-mode-token-table))
  (set sym "CMP_SHAPE( {loc}, {name}, {ndimx}, {dim}, {ndim}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire component shape")))

  (setq sym (intern "LOCAL_DATA" starfort-mode-token-table))
  (set sym '(("LOCAL_DATA" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VAL_ADDUW" starfort-mode-token-table))
  (set sym "VAL_ADDUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Add two UNSIGNED WORD values")))

  (setq sym (intern "SLA_M2AV" starfort-mode-token-table))
  (set sym "SLA_M2AV( {rmat}, {axvec} )")
  (setplist sym '((class . token) (desc . "From a rotation matrix, determine the corresponding axial vector.")))

  (setq sym (intern "UNFORMATTED_PAR" starfort-mode-token-table))
  (set sym '(("UNFORMATTED_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NUM_MODW" starfort-mode-token-table))
  (set sym "NUM_MODW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two WORD numbers")))

  (setq sym (intern "IIMSTW" starfort-mode-token-table))
  (set sym "IIMSTW( {dispid}, {memid}, {direcn}, {xsize}, {ysize}, {depth}, {xoff}, {yoff}, {status} )")
  (setplist sym '((class . token) (desc . "Set Transfer Window")))

  (setq sym (intern "SGS_ICURZ" starfort-mode-token-table))
  (set sym "SGS_ICURZ( {izonid} )")
  (setplist sym '((class . token) (desc . "Inquire current zone")))

  (setq sym (intern "SGS_ICURW" starfort-mode-token-table))
  (set sym "SGS_ICURW( {iwkid} )")
  (setplist sym '((class . token) (desc . "Inquire current workstation")))

  (setq sym (intern "NUM_MODR" starfort-mode-token-table))
  (set sym "NUM_MODR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two REAL numbers")))

  (setq sym (intern "VAL_ADDUB" starfort-mode-token-table))
  (set sym "VAL_ADDUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Add two UNSIGNED BYTE values")))

  (setq sym (intern "FIO_START" starfort-mode-token-table))
  (set sym "FIO_START( {status} )")
  (setplist sym '((class . token) (desc . "Set up units numbers and open standard I/O streams")))

  (setq sym (intern "SLA_DCC2S" starfort-mode-token-table))
  (set sym "SLA_DCC2S( {v}, {a}, {b} )")
  (setplist sym '((class . token) (desc . "Direction cosines to spherical coordinates (double precision)")))

  (setq sym (intern "NUM_MODI" starfort-mode-token-table))
  (set sym "NUM_MODI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two INTEGER numbers")))

  (setq sym (intern "VAL_UBTOW" starfort-mode-token-table))
  (set sym "VAL_UBTOW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE value to WORD")))

  (setq sym (intern "NUM_MODD" starfort-mode-token-table))
  (set sym "NUM_MODD( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two DOUBLE PRECISION numbers")))

  (setq sym (intern "ARY_TRACE" starfort-mode-token-table))
  (set sym "ARY_TRACE( {newflg}, {oldflg} )")
  (setplist sym '((class . token) (desc . "Set the internal ARY_ system error-tracing flag")))

  (setq sym (intern "VAL_UBTOR" starfort-mode-token-table))
  (set sym "VAL_UBTOR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE value to REAL")))

  (setq sym (intern "NUM_MODB" starfort-mode-token-table))
  (set sym "NUM_MODB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two BYTE numbers")))

  (setq sym (intern "PGERRY" starfort-mode-token-table))
  (set sym "PGERRY( {n}, {x}, {y1}, {y2}, {t} )")
  (setplist sym '((class . token) (desc . "Vertical error bar")))

  (setq sym (intern "PGERRX" starfort-mode-token-table))
  (set sym "PGERRX( {n}, {x1}, {x2}, {y}, {t} )")
  (setplist sym '((class . token) (desc . "Horizontal error bar")))

  (setq sym (intern "VAL_UBTOI" starfort-mode-token-table))
  (set sym "VAL_UBTOI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE value to INTEGER")))

  (setq sym (intern "VAL_UBTOD" starfort-mode-token-table))
  (set sym "VAL_UBTOD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE value to DOUBLE PRECISION")))

  (setq sym (intern "GFA" starfort-mode-token-table))
  (set sym "GFA( {np}, {pxa}, {pya} )")
  (setplist sym '((class . token) (desc . "Fill area")))

  (setq sym (intern "VAL_UBTOB" starfort-mode-token-table))
  (set sym "VAL_UBTOB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE value to BYTE")))

  (setq sym (intern "AGI_IMORE" starfort-mode-token-table))
  (set sym "AGI_IMORE( {picid}, {lmore}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire if a MORE structure exists")))

  (setq sym (intern "GMSG" starfort-mode-token-table))
  (set sym "GMSG( {wkid}, {mess} )")
  (setplist sym '((class . token) (desc . "Message")))

  (setq sym (intern "AGI_NUPIC" starfort-mode-token-table))
  (set sym "AGI_NUPIC( {wx1}, {wx2}, {wy1}, {wy2}, {pname}, {coment}, {newx1}, {newx2}, {newy1}, {newy2}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Create a new picture in the database")))

  (setq sym (intern "INQUIRE" starfort-mode-token-table))
  (set sym '(("INQUIRE_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "ERR" starfort-mode-token-table))
  (set sym "ERR = {lbl}")
  (setplist sym '((class . token) (desc . "ERR = {lbl}")))

  (setq sym (intern "VAL_ABSUW" starfort-mode-token-table))
  (set sym "VAL_ABSUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of UNSIGNED WORD value")))

  (setq sym (intern "SLA_CC2S" starfort-mode-token-table))
  (set sym "SLA_CC2S( {v}, {a}, {b} )")
  (setplist sym '((class . token) (desc . "Direction cosines to spherical coordinates (single precision)")))

  (setq sym (intern "IIMSMV" starfort-mode-token-table))
  (set sym "IIMSMV( {dispid}, {memid}, {nmem}, {lvis}, {status} )")
  (setplist sym '((class . token) (desc . "Set Memory Visibility")))

  (setq sym (intern "GCA" starfort-mode-token-table))
  (set sym "GCA( {px}, {py}, {qx}, {qy}, {nx}, {ny}, {dimx}, {colia} )")
  (setplist sym '((class . token) (desc . "Cell array")))

  (setq sym (intern "ERR_FIOER" starfort-mode-token-table))
  (set sym "ERR_FIOER( {token}, {iostat} )")
  (setplist sym '((class . token) (desc . "Assign a Fortran I/O error message to a token") (helpkey . "ERR_FIOER")))

  (setq sym (intern "EMS_LEVEL" starfort-mode-token-table))
  (set sym "EMS_LEVEL( {level} )")
  (setplist sym '((class . token) (desc . "Inquire the current error context level") (helpkey . "EMS_LEVEL")))

  (setq sym (intern "IIMSLT" starfort-mode-token-table))
  (set sym "IIMSLT( {dispid}, {memid}, {lutnum}, {ittnum}, {status} )")
  (setplist sym '((class . token) (desc . "Select Memory Look up Tables")))

  (setq sym (intern "VAL_ABSUB" starfort-mode-token-table))
  (set sym "VAL_ABSUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Absolute value (modulus) of UNSIGNED BYTE value")))

  (setq sym (intern "DO_WHILE" starfort-mode-token-table))
  (set sym "DO WHILE ( {logical_exp} )
{executable_statement}...
END DO	")
  (setplist sym '((class . token) (desc . "Pre-tested indefinite DO (DO WHILE)")))

  (setq sym (intern "VEC_UWTOW" starfort-mode-token-table))
  (set sym "VEC_UWTOW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD vectorised array to WORD")))

  (setq sym (intern "SGS_DEFCH" starfort-mode-token-table))
  (set sym "SGS_DEFCH( {chostr} )")
  (setplist sym '((class . token) (desc . "Define valid choice keys")))

  (setq sym (intern "VEC_UWTOR" starfort-mode-token-table))
  (set sym "VEC_UWTOR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD vectorised array to REAL")))

  (setq sym (intern "ARY_BAD" starfort-mode-token-table))
  (set sym "ARY_BAD( {iary}, {check}, {bad}, {status} )")
  (setplist sym '((class . token) (desc . "Determine if an array may contain bad pixels")))

  (setq sym (intern "HDS_TUNE" starfort-mode-token-table))
  (set sym "HDS_TUNE( {param}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Set HDS parameter")))

  (setq sym (intern "SLA_DIMXV" starfort-mode-token-table))
  (set sym "SLA_DIMXV( {dm}, {va}, {vb} )")
  (setplist sym '((class . token) (desc . "Performs the 3-D backward unitary transformation")))

  (setq sym (intern "VEC_UWTOI" starfort-mode-token-table))
  (set sym "VEC_UWTOI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD vectorised array to INTEGER")))

  (setq sym (intern "ASSIGNMENT_ARITH" starfort-mode-token-table))
  (set sym "{arith_assign_elm} = {arith_exp}")
  (setplist sym '((class . token) (desc . "Arithmetic ASSIGNMENT statement")))

  (setq sym (intern "END" starfort-mode-token-table))
  (set sym "END = {lbl}")
  (setplist sym '((class . token) (desc . "END = {lbl}")))

  (setq sym (intern "VEC_UWTOD" starfort-mode-token-table))
  (set sym "VEC_UWTOD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD vectorised array to DOUBLE PRECISION")))

  (setq sym (intern "SLA_INVF" starfort-mode-token-table))
  (set sym "SLA_INVF( {fwds}, {bkwds}, {j} )")
  (setplist sym '((class . token) (desc . "Invert a linear model of the type produced by the SLA_FITXY routine.")))

  (setq sym (intern "VEC_UWTOB" starfort-mode-token-table))
  (set sym "VEC_UWTOB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD vectorised array to BYTE")))

  (setq sym (intern "NUM_NEGW" starfort-mode-token-table))
  (set sym "NUM_NEGW( {num} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of WORD number")))

  (setq sym (intern "NUM_DTOW" starfort-mode-token-table))
  (set sym "NUM_DTOW( {num} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION number to WORD")))

  (setq sym (intern "NUM_NEGR" starfort-mode-token-table))
  (set sym "NUM_NEGR( {num} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of REAL number")))

  (setq sym (intern "NUM_DTOR" starfort-mode-token-table))
  (set sym "NUM_DTOR( {num} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION number to REAL")))

  (setq sym (intern "CMP__TYPIN" starfort-mode-token-table))
  (set sym "CMP__TYPIN")
  (setplist sym '((class . token) (desc . "Type invalid (error code)")))

  (setq sym (intern "NUM_NEGI" starfort-mode-token-table))
  (set sym "NUM_NEGI( {num} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of INTEGER number")))

  (setq sym (intern "NUM_DTOI" starfort-mode-token-table))
  (set sym "NUM_DTOI( {num} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION number to INTEGER")))

  (setq sym (intern "IIMRMY" starfort-mode-token-table))
  (set sym "IIMRMY( {dispid}, {memid}, {npix}, {xstart}, {ystart}, {depth}, {pack}, {itton}, {image}, {status} )")
  (setplist sym '((class . token) (desc . "Read Memory")))

  (setq sym (intern "NUM_NEGD" starfort-mode-token-table))
  (set sym "NUM_NEGD( {num} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of DOUBLE PRECISION number")))

  (setq sym (intern "NUM_DTOD" starfort-mode-token-table))
  (set sym "NUM_DTOD( {num} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION number to DOUBLE PRECISION")))

  (setq sym (intern "CHR_TRUNC" starfort-mode-token-table))
  (set sym "CHR_TRUNC( {delim}, {string} )")
  (setplist sym '((class . token) (desc . "Truncate string rightwards from a given character")))

  (setq sym (intern "NUM_NEGB" starfort-mode-token-table))
  (set sym "NUM_NEGB( {num} )")
  (setplist sym '((class . token) (desc . "Negate (change sign) of BYTE number")))

  (setq sym (intern "NUM_DTOB" starfort-mode-token-table))
  (set sym "NUM_DTOB( {num} )")
  (setplist sym '((class . token) (desc . "Convert a DOUBLE PRECISION number to BYTE")))

  (setq sym (intern "SLA_IMXV" starfort-mode-token-table))
  (set sym "SLA_IMXV( {rm}, {va}, {vb} )")
  (setplist sym '((class . token) (desc . "Performs the 3-D backward unitary transformation: vector VB = (inverse of matrix RM) vector VA")))

  (setq sym (intern "PGLEN" starfort-mode-token-table))
  (set sym "PGLEN( {units}, {string}, {xl}, {yl} )")
  (setplist sym '((class . token) (desc . "Find length of a string in a variety of units")))

  (setq sym (intern "MAG_DEACT" starfort-mode-token-table))
  (set sym "MAG_DEACT( {status} )")
  (setplist sym '((class . token) (desc . "Deactivate MAG package at end of application")))

  (setq sym (intern "SGS_ICUAV" starfort-mode-token-table))
  (set sym "SGS_ICUAV( {avail} )")
  (setplist sym '((class . token) (desc . "Inquire cursor availability")))

  (setq sym (intern "NDF_XPT0R" starfort-mode-token-table))
  (set sym "NDF_XPT0R( {value}, {indf}, {xname}, {cmpt}, {status} )")
  (setplist sym '((class . token) (desc . "Write a scalar real value to a component within a named NDF extension") (helpkey . "NDF_XPT0R")))

  (setq sym (intern "NDF_XPT0L" starfort-mode-token-table))
  (set sym "NDF_XPT0L( {value}, {indf}, {xname}, {cmpt}, {status} )")
  (setplist sym '((class . token) (desc . "Write a scalar logical value to a component within a named NDF extension") (helpkey . "NDF_XPT0L")))

  (setq sym (intern "NDF_XPT0I" starfort-mode-token-table))
  (set sym "NDF_XPT0I( {value}, {indf}, {xname}, {cmpt}, {status} )")
  (setplist sym '((class . token) (desc . "Write a scalar integer value to a component within a named NDF extension") (helpkey . "NDF_XPT0I")))

  (setq sym (intern "NDF_XPT0D" starfort-mode-token-table))
  (set sym "NDF_XPT0D( {value}, {indf}, {xname}, {cmpt}, {status} )")
  (setplist sym '((class . token) (desc . "Write a scalar double precision value to a component within a named NDF extension") (helpkey . "NDF_XPT0D")))

  (setq sym (intern "NDF_XPT0C" starfort-mode-token-table))
  (set sym "NDF_XPT0C( {value}, {indf}, {xname}, {cmpt}, {status} )")
  (setplist sym '((class . token) (desc . "Write a scalar character value to a component within a named NDF extension") (helpkey . "NDF_XPT0C")))

  (setq sym (intern "VEC_PWRUW" starfort-mode-token-table))
  (set sym "VEC_PWRUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (UNSIGNED WORD vectorised arrays)")))

  (setq sym (intern "CHR_ISNAM" starfort-mode-token-table))
  (set sym "CHR_ISNAM( {string} )")
  (setplist sym '((class . token) (desc . "Determine whether a string is a valid name")))

  (setq sym (intern "SLA_RANGE" starfort-mode-token-table))
  (set sym "SLA_RANGE( {angle} )")
  (setplist sym '((class . token) (desc . "Normalise angle into range +/- pi (single precision)")))

  (setq sym (intern "SLA_CALYD" starfort-mode-token-table))
  (set sym "SLA_CALYD( {iy}, {im}, {id}, {ny}, {nd}, {j} )")
  (setplist sym '((class . token) (desc . "Calendar to year and day in year")))

  (setq sym (intern "VAL_LG10UW" starfort-mode-token-table))
  (set sym "VAL_LG10UW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of UNSIGNED WORD value")))

  (setq sym (intern "AGS_ACTIV" starfort-mode-token-table))
  (set sym "AGS_ACTIV( {status} )")
  (setplist sym '((class . token) (desc . "Initialise SGS")))

  (setq sym (intern "SGS_TX" starfort-mode-token-table))
  (set sym "SGS_TX( {x}, {y}, {string} )")
  (setplist sym '((class . token) (desc . "Begin a new text string with a string")))

  (setq sym (intern "NUM_UWTOUW" starfort-mode-token-table))
  (set sym "NUM_UWTOUW( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD number to UNSIGNED WORD")))

  (setq sym (intern "VEC_PWRUB" starfort-mode-token-table))
  (set sym "VEC_PWRUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Raise to a power (UNSIGNED BYTE vectorised arrays)")))

  (setq sym (intern "MSG_FMTR" starfort-mode-token-table))
  (set sym "MSG_FMTR( {token}, {format}, {rvalue} )")
  (setplist sym '((class . token) (desc . "Assign a REAL value to a message token (formatted)") (helpkey . "MSG_FMTR")))

  (setq sym (intern "SGS_ASSOC" starfort-mode-token-table))
  (set sym "SGS_ASSOC( {param}, {mode}, {izonid}, {status} )")
  (setplist sym '((class . token) (desc . "Associate graphics workstation with parameter and open it")))

  (setq sym (intern "DAT_BASIC" starfort-mode-token-table))
  (set sym "DAT_BASIC( {loc}, {mode}, {pntr}, {len}, {status} )")
  (setplist sym '((class . token) (desc . "Map primitive as basic units")))

  (setq sym (intern "SGS_SW" starfort-mode-token-table))
  (set sym "SGS_SW( {x1}, {x2}, {y1}, {y2}, {status} )")
  (setplist sym '((class . token) (desc . "Set window")))

  (setq sym (intern "SLA_MAPQKZ" starfort-mode-token-table))
  (set sym "SLA_MAPQKZ( {rm}, {dm}, {ehn}, {gr2e}, {abv}, {ab1}, {ab2}, {pnm}, {ra}, {da} )")
  (setplist sym '((class . token) (desc . "Quick mean to apparent place assuming zero parallax and proper motion.")))

  (setq sym (intern "VAL_LG10UB" starfort-mode-token-table))
  (set sym "VAL_LG10UB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of UNSIGNED BYTE value")))

  (setq sym (intern "MSG_FMTL" starfort-mode-token-table))
  (set sym "MSG_FMTL( {token}, {format}, {lvalue} )")
  (setplist sym '((class . token) (desc . "Assign a LOGICAL value to a message token (formatted)") (helpkey . "MSG_FMTL")))

  (setq sym (intern "MSG_FMTI" starfort-mode-token-table))
  (set sym "MSG_FMTI( {token}, {format}, {ivalue} )")
  (setplist sym '((class . token) (desc . "Assign an INTEGER value to a message token (formatted)") (helpkey . "MSG_FMTI")))

  (setq sym (intern "NDF_MBADN" starfort-mode-token-table))
  (set sym "NDF_MBADN( {badok}, {n}, {ndfs}, {comp}, {check}, {bad}, {status} )")
  (setplist sym '((class . token) (desc . "Merge the bad-pixel flags of the array components of a number of NDFs") (helpkey . "NDF_MBADN")))

  (setq sym (intern "MSG_FMTD" starfort-mode-token-table))
  (set sym "MSG_FMTD( {token}, {format}, {dvalue} )")
  (setplist sym '((class . token) (desc . "Assign a DOUBLE PRECISION value to a message token (formatted)") (helpkey . "MSG_FMTD")))

  (setq sym (intern "MSG_FMTC" starfort-mode-token-table))
  (set sym "MSG_FMTC( {token}, {format}, {cvalue} )")
  (setplist sym '((class . token) (desc . "Assign a CHARACTER value to a message token (formatted)") (helpkey . "MSG_FMTC")))

  (setq sym (intern "NDF_TRACE" starfort-mode-token-table))
  (set sym "NDF_TRACE( {newflg}, {oldflg} )")
  (setplist sym '((class . token) (desc . "Set the internal NDF_ system error-tracing flag") (helpkey . "NDF_TRACE")))

  (setq sym (intern "AGS_ASSOC" starfort-mode-token-table))
  (set sym "AGS_ASSOC( {param}, {acmode}, {pname}, {picid}, {newzon}, {status} )")
  (setplist sym '((class . token) (desc . "Associate a device with AGI and SGS")))

  (setq sym (intern "NUM_UWTOUB" starfort-mode-token-table))
  (set sym "NUM_UWTOUB( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD number to UNSIGNED BYTE")))

  (setq sym (intern "NUM_UBTOUW" starfort-mode-token-table))
  (set sym "NUM_UBTOUW( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE number to UNSIGNED WORD")))

  (setq sym (intern "PGMOVE" starfort-mode-token-table))
  (set sym "PGMOVE( {x}, {y} )")
  (setplist sym '((class . token) (desc . "Move pen (change current pen position)")))

  (setq sym (intern "DAT_PUTR" starfort-mode-token-table))
  (set sym "DAT_PUTR( {loc}, {ndim}, {dim}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL data to a primitive")))

  (setq sym (intern "ARY_BOUND" starfort-mode-token-table))
  (set sym "ARY_BOUND( {iary}, {ndimx}, {lbnd}, {ubnd}, {ndim}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire the pixel-index bounds of an array")))

  (setq sym (intern "NEXTREC_PAR" starfort-mode-token-table))
  (set sym '(("NEXTREC_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "DAT_PUTL" starfort-mode-token-table))
  (set sym "DAT_PUTL( {loc}, {ndim}, {dim}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL data to a primitive")))

  (setq sym (intern "GWITM" starfort-mode-token-table))
  (set sym "GWITM( {wkid}, {type}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Write item to GKSM")))

  (setq sym (intern "DAT_PUTI" starfort-mode-token-table))
  (set sym "DAT_PUTI( {loc}, {ndim}, {dim}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER data to a primitive")))

  (setq sym (intern "DAT_PUTD" starfort-mode-token-table))
  (set sym "DAT_PUTD( {loc}, {ndim}, {dim}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION data to a primitive")))

  (setq sym (intern "DAT_PUTC" starfort-mode-token-table))
  (set sym "DAT_PUTC( {loc}, {ndim}, {dim}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER data to a primitive")))

  (setq sym (intern "NUM_UBTOUB" starfort-mode-token-table))
  (set sym "NUM_UBTOUB( {num} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE number to UNSIGNED BYTE")))

  (setq sym (intern "AGI_TDTOW" starfort-mode-token-table))
  (set sym "AGI_TDTOW( {picid}, {nxy}, {dx}, {dy}, {wx}, {wy}, {status} )")
  (setplist sym '((class . token) (desc . "Transform data to world coordinates")))

  (setq sym (intern "VAL_MAXUW" starfort-mode-token-table))
  (set sym "VAL_MAXUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two UNSIGNED WORD values")))

  (setq sym (intern "CHR_CLEAN" starfort-mode-token-table))
  (set sym "CHR_CLEAN( {string} )")
  (setplist sym '((class . token) (desc . "Remove all non-printable ASCII characters from a string")))

  (setq sym (intern "NUM_LOGUW" starfort-mode-token-table))
  (set sym "NUM_LOGUW( {num} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of UNSIGNED WORD number")))

  (setq sym (intern "ARY_SECT" starfort-mode-token-table))
  (set sym "ARY_SECT( {iary1}, {ndim}, {lbnd}, {ubnd}, {iary2}, {status} )")
  (setplist sym '((class . token) (desc . "Create an array section")))

  (setq sym (intern "CMP_PUTVR" starfort-mode-token-table))
  (set sym "CMP_PUTVR( {loc}, {name}, {el}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL data to a primitive component as if vectorised")))

  (setq sym (intern "SGS_WLIST" starfort-mode-token-table))
  (set sym "SGS_WLIST( {lun} )")
  (setplist sym '((class . token) (desc . "List available workstations")))

  (setq sym (intern "NAME_PAR" starfort-mode-token-table))
  (set sym '(("NAME_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "CMP_PUTVL" starfort-mode-token-table))
  (set sym "CMP_PUTVL( {loc}, {name}, {el}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL data to a primitive component as if vectorised")))

  (setq sym (intern "CMP_PUTVI" starfort-mode-token-table))
  (set sym "CMP_PUTVI( {loc}, {name}, {el}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER data to a primitive component as if vectorised")))

  (setq sym (intern "CMP_GETVR" starfort-mode-token-table))
  (set sym "CMP_GETVR( {loc}, {name}, {elx}, {rvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive component as REAL as if vectorised")))

  (setq sym (intern "VAL_MAXUB" starfort-mode-token-table))
  (set sym "VAL_MAXUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Maximum of two UNSIGNED BYTE values")))

  (setq sym (intern "CMP_PUTVD" starfort-mode-token-table))
  (set sym "CMP_PUTVD( {loc}, {name}, {el}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION data to a primitive component as if vectorised")))

  (setq sym (intern "DAT_PAREN" starfort-mode-token-table))
  (set sym "DAT_PAREN( {loc1}, {loc2}, {status} )")
  (setplist sym '((class . token) (desc . "Locate parent structure")))

  (setq sym (intern "CMP_PUTVC" starfort-mode-token-table))
  (set sym "CMP_PUTVC( {loc}, {name}, {el}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER data to a primitive component as if vectorised")))

  (setq sym (intern "NUM_LOGUB" starfort-mode-token-table))
  (set sym "NUM_LOGUB( {num} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of UNSIGNED BYTE number")))

  (setq sym (intern "CMP_GETVL" starfort-mode-token-table))
  (set sym "CMP_GETVL( {loc}, {name}, {elx}, {lvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive componant as LOGICAL as if vectorised")))

  (setq sym (intern "CMP_GETVI" starfort-mode-token-table))
  (set sym "CMP_GETVI( {loc}, {name}, {elx}, {ivalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive componant as INTEGER as if vectorised")))

  (setq sym (intern "SLA_PREBN" starfort-mode-token-table))
  (set sym "SLA_PREBN( {bep0}, {bep1}, {rmatp} )")
  (setplist sym '((class . token) (desc . "Generate the matrix of precession between two epochs, using the old, pre-IAU1976, Bessel-Newcomb model, using Andoyer's formulation.")))

  (setq sym (intern "TRN__TRUNC" starfort-mode-token-table))
  (set sym "TRN__TRUNC")
  (setplist sym '((class . token) (desc . "Character string truncated (error code)")))

  (setq sym (intern "CMP_GETVD" starfort-mode-token-table))
  (set sym "CMP_GETVD( {loc}, {name}, {elx}, {dvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive componant as DOUBLE PRECISION as if vectorised")))

  (setq sym (intern "CMP_GETVC" starfort-mode-token-table))
  (set sym "CMP_GETVC( {loc}, {name}, {elx}, {cvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive componant as CHARACTER as if vectorised")))

  (setq sym (intern "INTEGER" starfort-mode-token-table))
  (set sym "INTEGER")
  (setplist sym '((class . token) (desc . "INTEGER data type")))

  (setq sym (intern "CMP_PRIM" starfort-mode-token-table))
  (set sym "CMP_PRIM( {loc}, {name}, {reply}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire component primitive")))

  (setq sym (intern "SLA_REFZ" starfort-mode-token-table))
  (set sym "SLA_REFZ( {zu}, {refa}, {refb}, {zr} )")
  (setplist sym '((class . token) (desc . "Adjust an unrefracted zenith distance to include the effect of atmospheric refraction, using the simple A tan Z + B tan3 Z model.")))

  (setq sym (intern "PGUPDT" starfort-mode-token-table))
  (set sym "PGUPDT")
  (setplist sym '((class . token) (desc . "Update display")))

  (setq sym (intern "SLA_REFV" starfort-mode-token-table))
  (set sym "SLA_REFV( {vu}, {refa}, {refb}, {vr} )")
  (setplist sym '((class . token) (desc . "Adjust an unrefracted Cartesian vector to include the effect of atmospheric refraction, using the simple A tan Z + B tan3 Z model.")))

  (setq sym (intern "GQEPMI" starfort-mode-token-table))
  (set sym "GQEPMI( {wkid}, {n}, {errind}, {ol}, {pmind} )")
  (setplist sym '((class . token) (desc . "Inquire list element of polymarker indices")))

  (setq sym (intern "SGS_IPEN" starfort-mode-token-table))
  (set sym "SGS_IPEN( {npen} )")
  (setplist sym '((class . token) (desc . "Inquire pen number")))

  (setq sym (intern "SLA_DBEAR" starfort-mode-token-table))
  (set sym "SLA_DBEAR( {a1}, {b1}, {a2}, {b2} )")
  (setplist sym '((class . token) (desc . "Bearing between points on a sphere (double precision)")))

  (setq sym (intern "GQEPLI" starfort-mode-token-table))
  (set sym "GQEPLI( {wkid}, {n}, {errind}, {ol}, {plind} )")
  (setplist sym '((class . token) (desc . "Inquire list element of polyline indices")))

  (setq sym (intern "AGS_DEASS" starfort-mode-token-table))
  (set sym "AGS_DEASS( {param}, {parcan}, {status} )")
  (setplist sym '((class . token) (desc . "Deassociate a device from AGI and SGS")))

  (setq sym (intern "GWM_CLOSE" starfort-mode-token-table))
  (set sym "GWM_CLOSE( {status} )")
  (setplist sym '((class . token) (desc . "Close the X client-server connection")))

  (setq sym (intern "CHR_PUTR" starfort-mode-token-table))
  (set sym "CHR_PUTR( {rvalue}, {string}, {length} )")
  (setplist sym '((class . token) (desc . "Put a real number into a string at a given position")))

  (setq sym (intern "PSX_CUSERID" starfort-mode-token-table))
  (set sym "PSX_CUSERID( {user}, {status} )")
  (setplist sym '((class . token) (desc . "Get the username")))

  (setq sym (intern "GSLWSC" starfort-mode-token-table))
  (set sym "GSLWSC( {lwsc} )")
  (setplist sym '((class . token) (desc . "Set linewidth scale factor")))

  (setq sym (intern "CHR_PUTL" starfort-mode-token-table))
  (set sym "CHR_PUTL( {lvalue}, {string}, {length} )")
  (setplist sym '((class . token) (desc . "Put a logical value into a string at a given position")))

  (setq sym (intern "GQLWSC" starfort-mode-token-table))
  (set sym "GQLWSC( {errind}, {lwidth} )")
  (setplist sym '((class . token) (desc . "Inquire linewidth scale factor")))

  (setq sym (intern "CHR_PUTI" starfort-mode-token-table))
  (set sym "CHR_PUTI( {ivalue}, {string}, {length} )")
  (setplist sym '((class . token) (desc . "Put an integer value into a string at a given position")))

  (setq sym (intern "SGS_SSPTX" starfort-mode-token-table))
  (set sym "SGS_SSPTX( {sp} )")
  (setplist sym '((class . token) (desc . "Set spacing of text")))

  (setq sym (intern "CHR_PUTD" starfort-mode-token-table))
  (set sym "CHR_PUTD( {dvalue}, {string}, {length} )")
  (setplist sym '((class . token) (desc . "Put a double precision number into a string at a given position")))

  (setq sym (intern "CHR_PUTC" starfort-mode-token-table))
  (set sym "CHR_PUTC( {str1}, {str2}, {len2} )")
  (setplist sym '((class . token) (desc . "Copy one string into another")))

  (setq sym (intern "NUM_MINW" starfort-mode-token-table))
  (set sym "NUM_MINW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Minimum of two WORD numbers")))

  (setq sym (intern "CMP_PUTNR" starfort-mode-token-table))
  (set sym "CMP_PUTNR( {loc}, {name}, {ndim}, {dimx}, {rvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL data to a primitive array component")))

  (setq sym (intern "ADAM_PARAMETER" starfort-mode-token-table))
  (set sym '(("ADAM_PARAMETERS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SLA_GALEQ" starfort-mode-token-table))
  (set sym "SLA_GALEQ( {dl}, {db}, {dr}, {dd} )")
  (setplist sym '((class . token) (desc . "Transformation from IAU 1958 galactic coordinates to J2000.0 equatorial coordinates")))

  (setq sym (intern "NUM_MINR" starfort-mode-token-table))
  (set sym "NUM_MINR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Minimum of two REAL numbers")))

  (setq sym (intern "CMP_PUTNL" starfort-mode-token-table))
  (set sym "CMP_PUTNL( {loc}, {name}, {ndim}, {dimx}, {lvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL data to a primitive array component")))

  (setq sym (intern "CMP_PUTNI" starfort-mode-token-table))
  (set sym "CMP_PUTNI( {loc}, {name}, {ndim}, {dimx}, {ivalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER data to a primitive array component")))

  (setq sym (intern "NDF_XDEL" starfort-mode-token-table))
  (set sym "NDF_XDEL( {indf}, {xname}, {status} )")
  (setplist sym '((class . token) (desc . "Delete a specified NDF extension") (helpkey . "NDF_XDEL")))

  (setq sym (intern "CMP_GETNR" starfort-mode-token-table))
  (set sym "CMP_GETNR( {loc}, {name}, {ndim}, {dimx}, {rvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive array component as REAL ")))

  (setq sym (intern "GSTXFP" starfort-mode-token-table))
  (set sym "GSTXFP( {font}, {gks$prec} )")
  (setplist sym '((class . token) (desc . "Set text font and precision")))

  (setq sym (intern "SLA_BEAR" starfort-mode-token-table))
  (set sym "SLA_BEAR( {a1}, {b1}, {a2}, {b2} )")
  (setplist sym '((class . token) (desc . "Bearing between points on a sphere")))

  (setq sym (intern "NUM_MINI" starfort-mode-token-table))
  (set sym "NUM_MINI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Minimum of two INTEGER numbers")))

  (setq sym (intern "CMP_PUTND" starfort-mode-token-table))
  (set sym "CMP_PUTND( {loc}, {name}, {ndim}, {dimx}, {dvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION data to a primitive array component")))

  (setq sym (intern "A_TASK_SECTIONS" starfort-mode-token-table))
  (set sym '(("A_TASK_SECTIONS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "CMP_PUTNC" starfort-mode-token-table))
  (set sym "CMP_PUTNC( {loc}, {name}, {ndim}, {dimx}, {cvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER data to a primitive array component")))

  (setq sym (intern "GQTXFP" starfort-mode-token-table))
  (set sym "GQTXFP( {errind}, {font}, {prec} )")
  (setplist sym '((class . token) (desc . "Inquire text font and precision")))

  (setq sym (intern "IIEPEP" starfort-mode-token-table))
  (set sym "IIEPEP( {param}, {slen}, {string}, {status} )")
  (setplist sym '((class . token) (desc . "Put Escape Parameter")))

  (setq sym (intern "CMP_GETNL" starfort-mode-token-table))
  (set sym "CMP_GETNL( {loc}, {name}, {ndim}, {dimx}, {lvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive array component as LOGICAL ")))

  (setq sym (intern "PSX_REALLOC" starfort-mode-token-table))
  (set sym "PSX_REALLOC( {size}, {pntr}, {status} )")
  (setplist sym '((class . token) (desc . "Change the size of an allocated region of virtual memory")))

  (setq sym (intern "NUM_MIND" starfort-mode-token-table))
  (set sym "NUM_MIND( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Minimum of two DOUBLE PRECISION numbers")))

  (setq sym (intern "CMP__DIMIN" starfort-mode-token-table))
  (set sym "CMP__DIMIN")
  (setplist sym '((class . token) (desc . "Dimensions invalid (error code)")))

  (setq sym (intern "IILWLT" starfort-mode-token-table))
  (set sym "IILWLT( {dispid}, {lutnum}, {start}, {nent}, {vlut}, {status} )")
  (setplist sym '((class . token) (desc . "Write Video Look Up Table")))

  (setq sym (intern "CMP_GETNI" starfort-mode-token-table))
  (set sym "CMP_GETNI( {loc}, {name}, {ndim}, {dimx}, {ivalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive array component as INTEGER ")))

  (setq sym (intern "NUM_MINB" starfort-mode-token-table))
  (set sym "NUM_MINB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Minimum of two BYTE numbers")))

  (setq sym (intern "SLA_CALDJ" starfort-mode-token-table))
  (set sym "SLA_CALDJ( {iy}, {im}, {id}, {djm}, {j} )")
  (setplist sym '((class . token) (desc . "Gregorian Calendar to Modified Julian Date")))

  (setq sym (intern "CMP_GETND" starfort-mode-token-table))
  (set sym "CMP_GETND( {loc}, {name}, {ndim}, {dimx}, {dvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive array component as DOUBLE PRECISION ")))

  (setq sym (intern "CMP_GETNC" starfort-mode-token-table))
  (set sym "CMP_GETNC( {loc}, {name}, {ndim}, {dimx}, {cvalue}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive array component as CHARACTER ")))

  (setq sym (intern "SLA_DD2TF" starfort-mode-token-table))
  (set sym "SLA_DD2TF( {ndp}, {days}, {sign}, {ihmsf} )")
  (setplist sym '((class . token) (desc . "Convert an interval in days into hours, minutes, seconds")))

  (setq sym (intern "SLA_CD2TF" starfort-mode-token-table))
  (set sym "SLA_CD2TF( {ndp}, {days}, {sign}, {ihmsf} )")
  (setplist sym '((class . token) (desc . "Convert an interval in days into hours, minutes, seconds")))

  (setq sym (intern "CHR_LDBLK" starfort-mode-token-table))
  (set sym "CHR_LDBLK( {string} )")
  (setplist sym '((class . token) (desc . "Remove leading blanks from a string")))

  (setq sym (intern "GQMNTN" starfort-mode-token-table))
  (set sym "GQMNTN( {errind}, {maxtnr} )")
  (setplist sym '((class . token) (desc . "Inquire maximum normalisation transformation number")))

  (setq sym (intern "GQENTN" starfort-mode-token-table))
  (set sym "GQENTN( {n}, {errind}, {ol}, {nprio} )")
  (setplist sym '((class . token) (desc . "Inquire list element of normalization transformation numbers")))

  (setq sym (intern "TRN__LIN" starfort-mode-token-table))
  (set sym "TRN__LIN")
  (setplist sym '((class . token) (desc . "'Preserves straight lines' classification (symbolic constant)")))

  (setq sym (intern "CMP_UNMAP" starfort-mode-token-table))
  (set sym "CMP_UNMAP( {loc}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Unmap component")))

  (setq sym (intern "IILWIT" starfort-mode-token-table))
  (set sym "IILWIT( {dispid}, {memid}, {ittnum}, {start}, {nent}, {itt}, {status} )")
  (setplist sym '((class . token) (desc . "Write Intensity Transformation Table")))

  (setq sym (intern "GSTXCI" starfort-mode-token-table))
  (set sym "GSTXCI( {txcoli} )")
  (setplist sym '((class . token) (desc . "Set text colour index")))

  (setq sym (intern "GRP_VALID" starfort-mode-token-table))
  (set sym "GRP_VALID( {igrp}, {valid}, {status} )")
  (setplist sym '((class . token) (desc . "Determine if a group identifier is valid")))

  (setq sym (intern "GQTXCI" starfort-mode-token-table))
  (set sym "GQTXCI( {errind}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire text colour index")))

  (setq sym (intern "GRENSG" starfort-mode-token-table))
  (set sym "GRENSG( {old}, {new} )")
  (setplist sym '((class . token) (desc . "Rename segment")))

  (setq sym (intern "GWM_DSWIN" starfort-mode-token-table))
  (set sym "GWM_DSWIN( {wname}, {status} )")
  (setplist sym '((class . token) (desc . "Destroy a GWM window")))

  (setq sym (intern "ARY_SBND" starfort-mode-token-table))
  (set sym "ARY_SBND( {ndim}, {lbnd}, {ubnd}, {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Set new pixel-index bounds for an array")))

  (setq sym (intern "ST" starfort-mode-token-table))
  (set sym "STATUS")
  (setplist sym '((class . token) (alias . t)))

  (setq sym (intern "GQDWKA" starfort-mode-token-table))
  (set sym "GQDWKA( {wtype}, {errind}, {plbun}, {pmbun}, {txbun}, {fabun}, {parep}, {colrep}, {wktr} )")
  (setplist sym '((class . token) (desc . "Inquire dynamic modification of workstation attributes")))

  (setq sym (intern "AGP_ACTIV" starfort-mode-token-table))
  (set sym "AGP_ACTIV( {status} )")
  (setplist sym '((class . token) (desc . "Initialise PGPLOT")))

  (setq sym (intern "GSTXAL" starfort-mode-token-table))
  (set sym "GSTXAL( {gks$txah}, {gks$txav} )")
  (setplist sym '((class . token) (desc . "Set text alignment")))

  (setq sym (intern "SGS_INIT" starfort-mode-token-table))
  (set sym "SGS_INIT( {lun}, {status} )")
  (setplist sym '((class . token) (desc . "Initialise SGS and (if necessary) open GKS")))

  (setq sym (intern "PGPTEXT" starfort-mode-token-table))
  (set sym "PGPTEXT( {x}, {y}, {angle}, {fjust}, {text} )")
  (setplist sym '((class . token) (desc . "Write text at arbitrary position and angle")))

  (setq sym (intern "GQTXAL" starfort-mode-token-table))
  (set sym "GQTXAL( {errind}, {txalh}, {txalv} )")
  (setplist sym '((class . token) (desc . "Inquire text alignment")))

  (setq sym (intern "PGMTEXT" starfort-mode-token-table))
  (set sym "PGMTEXT( {side}, {disp}, {coord}, {fjust}, {text} )")
  (setplist sym '((class . token) (desc . "Write text at position relative to viewport")))

  (setq sym (intern "VAL_DIVUW" starfort-mode-token-table))
  (set sym "VAL_DIVUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one UNSIGNED WORD value by another")))

  (setq sym (intern "GQEPAI" starfort-mode-token-table))
  (set sym "GQEPAI( {wkid}, {n}, {errind}, {ol}, {paind} )")
  (setplist sym '((class . token) (desc . "Inquire list element of pattern indices")))

  (setq sym (intern "IO_STMT" starfort-mode-token-table))
  (set sym '(("IO_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SUBROUTINE_OPTIONS" starfort-mode-token-table))
  (set sym "[examples]
[pitfalls]
[notes]
[prior_requirements]
[side_effects]
[algorithm]
[accuracy]
[timing]
[routines_used]
[deficiencies]
[machine_specifics]
[DIY_prologue_item]...
[references]
[keywords]
[copyright]")
  (setplist sym '((class . token) (desc . "Expanded list of all optional items")))

  (setq sym (intern "NDF_BOUND" starfort-mode-token-table))
  (set sym "NDF_BOUND( {indf}, {ndimx}, {lbnd}, {ubnd}, {ndim}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire the pixel-index bounds of an NDF") (helpkey . "NDF_BOUND")))

  (setq sym (intern "VAL_DIVUB" starfort-mode-token-table))
  (set sym "VAL_DIVUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one UNSIGNED BYTE value by another")))

  (setq sym (intern "SGS_DEACT" starfort-mode-token-table))
  (set sym "SGS_DEACT( {status} )")
  (setplist sym '((class . token) (desc . "Deactivate ADAM SGS after use by an application")))

  (setq sym (intern "BAD" starfort-mode-token-table))
  (set sym "STATUS .NE. SAI__OK")
  (setplist sym '((class . token) (alias . t)))

  (setq sym (intern "DAT__DELIN" starfort-mode-token-table))
  (set sym "DAT__DELIN")
  (setplist sym '((class . token) (desc . "Deletion invalid (error code)")))

  (setq sym (intern "AGP_ASSOC" starfort-mode-token-table))
  (set sym "AGP_ASSOC( {param}, {acmode}, {pname}, {border}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Associate a device with AGI and PGPLOT")))

  (setq sym (intern "SGS_RELZ" starfort-mode-token-table))
  (set sym "SGS_RELZ( {izonid} )")
  (setplist sym '((class . token) (desc . "Release zone")))

  (setq sym (intern "TRN__NDCMM" starfort-mode-token-table))
  (set sym "TRN__NDCMM")
  (setplist sym '((class . token) (desc . "Number of data coordinates mis-matched (error code)")))

  (setq sym (intern "CHR_HTOI" starfort-mode-token-table))
  (set sym "CHR_HTOI( {string}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read an integer number from a hexadecimal string")))

  (setq sym (intern "OK" starfort-mode-token-table))
  (set sym "STATUS .EQ. SAI__OK")
  (setplist sym '((class . token) (alias . t)))

  (setq sym (intern "GQLVKS" starfort-mode-token-table))
  (set sym "GQLVKS( {errind}, {level} )")
  (setplist sym '((class . token) (desc . "Inquire level of GKS")))

  (setq sym (intern "GEVTM" starfort-mode-token-table))
  (set sym "GEVTM( {x0}, {y0}, {dx}, {dy}, {phi}, {fx}, {fy}, {gks$sw}, {mout} )")
  (setplist sym '((class . token) (desc . "Evaluate transformation matrix")))

  (setq sym (intern "AGS_DEACT" starfort-mode-token-table))
  (set sym "AGS_DEACT( {status} )")
  (setplist sym '((class . token) (desc . "Close down SGS")))

  (setq sym (intern "SLA_DJCAL" starfort-mode-token-table))
  (set sym "SLA_DJCAL( {ndp}, {djm}, {iymdf}, {j} )")
  (setplist sym '((class . token) (desc . "Modified Julian Date to Gregorian Calendar")))

  (setq sym (intern "SLA_AOPPAT" starfort-mode-token-table))
  (set sym "SLA_AOPPAT( {date}, {aoprms} )")
  (setplist sym '((class . token) (desc . "Recompute the sidereal time in the apparent to observed place star-independent parameter block.")))

  (setq sym (intern "NUM_MODUW" starfort-mode-token-table))
  (set sym "NUM_MODUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two UNSIGNED WORD numbers")))

  (setq sym (intern "ARY_SAME" starfort-mode-token-table))
  (set sym "ARY_SAME( {iary1}, {iary2}, {same}, {isect}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire if two arrays are part of the same base array")))

  (setq sym (intern "FORM" starfort-mode-token-table))
  (set sym "FORM = {form_options}")
  (setplist sym '((class . token) (desc . "FORM = {form_options}")))

  (setq sym (intern "VEC_EXPW" starfort-mode-token-table))
  (set sym "VEC_EXPW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of WORD vectorised array")))

  (setq sym (intern "GRP_LIST" starfort-mode-token-table))
  (set sym "GRP_LIST( {param}, {indxlo}, {indxhi}, {comnt}, {igrp}, {status} )")
  (setplist sym '((class . token) (desc . "Write names to a text file specified by the environment")))

  (setq sym (intern "VEC_EXPR" starfort-mode-token-table))
  (set sym "VEC_EXPR( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of REAL vectorised array")))

  (setq sym (intern "IIDUPD" starfort-mode-token-table))
  (set sym "IIDUPD( {dispid}, {status} )")
  (setplist sym '((class . token) (desc . "Update Display")))

  (setq sym (intern "NO_CARRIAGE_CONTROL" starfort-mode-token-table))
  (set sym "CARRIAGECONTROL = 'LIST'")
  (setplist sym '((class . token) (desc . "CARRIAGECONTROL = 'LIST'")))

  (setq sym (intern "VEC_EXPI" starfort-mode-token-table))
  (set sym "VEC_EXPI( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of INTEGER vectorised array")))

  (setq sym (intern "NUM_MODUB" starfort-mode-token-table))
  (set sym "NUM_MODUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two UNSIGNED BYTE numbers")))

  (setq sym (intern "_ERROR_REPORT" starfort-mode-token-table))
  (set sym "STATUS = {error_code}
[define_message_token]...
CALL ERR_REP( '{routine_name}_{error_name}',
\\     :	'{message_text}',
\\     :	STATUS )")
  (setplist sym '((class . token) (desc . "Make an error report")))

  (setq sym (intern "VEC_EXPD" starfort-mode-token-table))
  (set sym "VEC_EXPD( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of DOUBLE PRECISION vectorised array")))

  (setq sym (intern "VEC_EXPB" starfort-mode-token-table))
  (set sym "VEC_EXPB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of BYTE vectorised array")))

  (setq sym (intern "IF" starfort-mode-token-table))
  (set sym "IF ( {logical_exp} ) {executable_statement}")
  (setplist sym '((class . token) (desc . "IF ( {logical_exp} ) {executable_statement}")))

  (setq sym (intern "AGI_PTREF" starfort-mode-token-table))
  (set sym "AGI_PTREF( {datref}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Store a reference object in a picture")))

  (setq sym (intern "DAT__INCHK" starfort-mode-token-table))
  (set sym "DAT__INCHK")
  (setplist sym '((class . token) (desc . "Integrity check (error code)")))

  (setq sym (intern "ARY_SBAD" starfort-mode-token-table))
  (set sym "ARY_SBAD( {bad}, {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Set the bad-pixel flag for an array")))

  (setq sym (intern "GO" starfort-mode-token-table))
  (set sym "GO TO {lbl}")
  (setplist sym '((class . token) (desc . "GO TO {lbl}")))

  (setq sym (intern "SGS_OTEXT" starfort-mode-token-table))
  (set sym "SGS_OTEXT")
  (setplist sym '((class . token) (desc . "Output buffered text")))

  (setq sym (intern "AGI_INAME" starfort-mode-token-table))
  (set sym "AGI_INAME( {pname}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire name of the current picture")))

  (setq sym (intern "AGI_GTREF" starfort-mode-token-table))
  (set sym "AGI_GTREF( {picid}, {mode}, {datref}, {status} )")
  (setplist sym '((class . token) (desc . "Get a reference object from a picture")))

  (setq sym (intern "GLOBAL_DATA" starfort-mode-token-table))
  (set sym '(("GLOBAL_DATA" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "EMS_FMTR" starfort-mode-token-table))
  (set sym "EMS_FMTR( {token}, {format}, {rvalue} )")
  (setplist sym '((class . token) (desc . "Assign a REAL value to a message token (formatted)") (helpkey . "EMS_FMTR")))

  (setq sym (intern "AGP_DEASS" starfort-mode-token-table))
  (set sym "AGP_DEASS( {param}, {parcan}, {status} )")
  (setplist sym '((class . token) (desc . "Deassociate a device from AGI and PGPLOT")))

  (setq sym (intern "COMMON" starfort-mode-token-table))
  (set sym '(("COMMON_STMT" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "SGS_BTEXT" starfort-mode-token-table))
  (set sym "SGS_BTEXT( {x}, {y} )")
  (setplist sym '((class . token) (desc . "Begin a new text string")))

  (setq sym (intern "EMS_FMTL" starfort-mode-token-table))
  (set sym "EMS_FMTL( {token}, {format}, {lvalue} )")
  (setplist sym '((class . token) (desc . "Assign a LOGICAL value to a message token (formatted)") (helpkey . "EMS_FMTL")))

  (setq sym (intern "SGS_ATEXT" starfort-mode-token-table))
  (set sym "SGS_ATEXT( {string} )")
  (setplist sym '((class . token) (desc . "Append a field to the text buffer")))

  (setq sym (intern "EMS_FMTI" starfort-mode-token-table))
  (set sym "EMS_FMTI( {token}, {format}, {ivalue} )")
  (setplist sym '((class . token) (desc . "Assign an INTEGER value to a message token (formatted)") (helpkey . "EMS_FMTI")))

  (setq sym (intern "DO" starfort-mode-token-table))
  (set sym '(lambda nil (insert "DO {lab} {do_var} = {arith_exp}, {arith_exp}, [do_increment]") (starfort-indent-line) (starfort-break-line (quote code)) (insert "{executable_statement}...") (starfort-break-line (quote code)) (insert "CONTINUE") (save-excursion (beginning-of-line) (delete-char (+ 6 starfort-do-indent)) (insert " {lab}")) t))
  (setplist sym '((class . token) (desc . "Indexed DO loop with label")))

  (setq sym (intern "EMS_FMTD" starfort-mode-token-table))
  (set sym "EMS_FMTD( {token}, {format}, {dvalue} )")
  (setplist sym '((class . token) (desc . "Assign a DOUBLE PRECISION value to a message token (formatted)") (helpkey . "EMS_FMTD")))

  (setq sym (intern "EMS_FMTC" starfort-mode-token-table))
  (set sym "EMS_FMTC( {token}, {format}, {cvalue} )")
  (setplist sym '((class . token) (desc . "Assign a CHARACTER value to a message token (formatted)") (helpkey . "EMS_FMTC")))

  (setq sym (intern "NUM_LOGW" starfort-mode-token-table))
  (set sym "NUM_LOGW( {num} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of WORD number")))

  (setq sym (intern "SLA_PREC" starfort-mode-token-table))
  (set sym "SLA_PREC( {ep0}, {ep1}, {rmatp} )")
  (setplist sym '((class . token) (desc . "Form the matrix of precession between two epochs (IAU1976/FK5)")))

  (setq sym (intern "DAT_PRIM" starfort-mode-token-table))
  (set sym "DAT_PRIM( {loc}, {reply}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire object primitive")))

  (setq sym (intern "NUM_LOGR" starfort-mode-token-table))
  (set sym "NUM_LOGR( {num} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of REAL number")))

  (setq sym (intern "ARY_SHIFT" starfort-mode-token-table))
  (set sym "ARY_SHIFT( {nshift}, {shift}, {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Apply pixel-index shifts to an array")))

  (setq sym (intern "PGENV" starfort-mode-token-table))
  (set sym "PGENV( {xmin}, {xmax}, {ymin}, {ymax}, {just}, {axis} )")
  (setplist sym '((class . token) (desc . "Set window and viewport and draw labeled frame")))

  (setq sym (intern "NUM_LOGI" starfort-mode-token-table))
  (set sym "NUM_LOGI( {num} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of INTEGER number")))

  (setq sym (intern "RIO_WRITE" starfort-mode-token-table))
  (set sym "RIO_WRITE( {fd}, {recno}, {nchar}, {buf}, {status} )")
  (setplist sym '((class . token) (desc . "Write a record to a direct access file")))

  (setq sym (intern "NUM_LOGD" starfort-mode-token-table))
  (set sym "NUM_LOGD( {num} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of DOUBLE PRECISION number")))

  (setq sym (intern "GSELNT" starfort-mode-token-table))
  (set sym "GSELNT( {tnr} )")
  (setplist sym '((class . token) (desc . "Set normalisation transformation")))

  (setq sym (intern "NUM_LOGB" starfort-mode-token-table))
  (set sym "NUM_LOGB( {num} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of BYTE number")))

  (setq sym (intern "_OK_BLOCK" starfort-mode-token-table))
  (set sym "IF ( STATUS .EQ. SAI__OK ) THEN
{executable_statement}...
END IF	")
  (setplist sym '((class . token) (desc . "IF block which executes if STATUS is OK")))

  (setq sym (intern "FIO_WRITE" starfort-mode-token-table))
  (set sym "FIO_WRITE( {fd}, {buf}, {status} )")
  (setplist sym '((class . token) (desc . "Write a sequential record")))

  (setq sym (intern "PGEND" starfort-mode-token-table))
  (set sym "PGEND")
  (setplist sym '((class . token) (desc . "Terminate PGPLOT")))

  (setq sym (intern "IIDSSS" starfort-mode-token-table))
  (set sym "IIDSSS( {dispid}, {memid}, {xoff}, {yoff}, {split}, {xsplit}, {ysplit}, {status} )")
  (setplist sym '((class . token) (desc . "Set Split Screen")))

  (setq sym (intern "NDF_FORM" starfort-mode-token-table))
  (set sym "NDF_FORM( {indf}, {comp}, {form}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the storage form of an NDF array component") (helpkey . "NDF_FORM")))

  (setq sym (intern "DAT_PREC" starfort-mode-token-table))
  (set sym "DAT_PREC( {loc}, {nbyte}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire storage precision")))

  (setq sym (intern "IIDSTC" starfort-mode-token-table))
  (set sym "IIDSTC( {dispid}, {nconf}, {status} )")
  (setplist sym '((class . token) (desc . "Stop Configuration")))

  (setq sym (intern "CMP_PUT1R" starfort-mode-token-table))
  (set sym "CMP_PUT1R( {loc}, {name}, {el}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL data to a primitive vector component")))

  (setq sym (intern "GRP_SOWN" starfort-mode-token-table))
  (set sym "GRP_SOWN( {igrp1}, {igrp2}, {status} )")
  (setplist sym '((class . token) (desc . "Establish one group as the owner of another group")))

  (setq sym (intern "DAT_CANCL" starfort-mode-token-table))
  (set sym "DAT_CANCL( {param}, {status} )")
  (setplist sym '((class . token) (desc . "Cancel an ADAM parameter/data object association")))

  (setq sym (intern "CMP_PUT1L" starfort-mode-token-table))
  (set sym "CMP_PUT1L( {loc}, {name}, {el}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL data to a primitive vector component")))

  (setq sym (intern "CMP_PUT0R" starfort-mode-token-table))
  (set sym "CMP_PUT0R( {loc}, {name}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write REAL data to a primitive scalar component")))

  (setq sym (intern "CMP_PUT1I" starfort-mode-token-table))
  (set sym "CMP_PUT1I( {loc}, {name}, {el}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER data to a primitive vector component")))

  (setq sym (intern "CMP_GET1R" starfort-mode-token-table))
  (set sym "CMP_GET1R( {loc}, {name}, {elx}, {rvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive vector component as REAL")))

  (setq sym (intern "GSMKSC" starfort-mode-token-table))
  (set sym "GSMKSC( {mszsf} )")
  (setplist sym '((class . token) (desc . "Set marker size scale factor")))

  (setq sym (intern "HDS_LOCK" starfort-mode-token-table))
  (set sym "HDS_LOCK( {loc}, {status} )")
  (setplist sym '((class . token) (desc . "Lock container file")))

  (setq sym (intern "GQMKSC" starfort-mode-token-table))
  (set sym "GQMKSC( {errind}, {mszsf} )")
  (setplist sym '((class . token) (desc . "Inquire marker size scale factor")))

  (setq sym (intern "CMP_PUT1D" starfort-mode-token-table))
  (set sym "CMP_PUT1D( {loc}, {name}, {el}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION data to a primitive vector component")))

  (setq sym (intern "CMP_PUT0L" starfort-mode-token-table))
  (set sym "CMP_PUT0L( {loc}, {name}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write LOGICAL data to a primitive scalar component")))

  (setq sym (intern "AGI_CLOSE" starfort-mode-token-table))
  (set sym "AGI_CLOSE( {status} )")
  (setplist sym '((class . token) (desc . "Close AGI in non-ADAM environments")))

  (setq sym (intern "TRN__SZPRC" starfort-mode-token-table))
  (set sym "TRN__SZPRC")
  (setplist sym '((class . token) (desc . "Size of a precision string (symbolic constant)")))

  (setq sym (intern "CMP_PUT1C" starfort-mode-token-table))
  (set sym "CMP_PUT1C( {loc}, {name}, {el}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER data to a primitive vector component")))

  (setq sym (intern "VEC_INTUW" starfort-mode-token-table))
  (set sym "VEC_INTUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate UNSIGNED WORD vectorised array to an integer")))

  (setq sym (intern "CMP_PUT0I" starfort-mode-token-table))
  (set sym "CMP_PUT0I( {loc}, {name}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write INTEGER data to a primitive scalar component")))

  (setq sym (intern "CMP_GET1L" starfort-mode-token-table))
  (set sym "CMP_GET1L( {loc}, {name}, {elx}, {lvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive vector component as LOGICAL")))

  (setq sym (intern "CMP_GET0R" starfort-mode-token-table))
  (set sym "CMP_GET0R( {loc}, {name}, {rvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive scalar component as REAL")))

  (setq sym (intern "CMP_GET1I" starfort-mode-token-table))
  (set sym "CMP_GET1I( {loc}, {name}, {elx}, {ivalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive vector component as INTEGER")))

  (setq sym (intern "CMP_PUT0D" starfort-mode-token-table))
  (set sym "CMP_PUT0D( {loc}, {name}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write DOUBLE PRECISION data to a primitive scalar component")))

  (setq sym (intern "CMP_PUT0C" starfort-mode-token-table))
  (set sym "CMP_PUT0C( {loc}, {name}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Write CHARACTER data to a primitive scalar component")))

  (setq sym (intern "GESC" starfort-mode-token-table))
  (set sym "GESC( {fctid}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Escape")))

  (setq sym (intern "CMP_GET1D" starfort-mode-token-table))
  (set sym "CMP_GET1D( {loc}, {name}, {elx}, {dvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive vector component as DOUBLE PRECISION")))

  (setq sym (intern "CMP_GET0L" starfort-mode-token-table))
  (set sym "CMP_GET0L( {loc}, {name}, {lvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive scalar component as LOGICAL")))

  (setq sym (intern "EMS_RENEW" starfort-mode-token-table))
  (set sym "EMS_RENEW")
  (setplist sym '((class . token) (desc . "Renew any annulled message tokens in the current context") (helpkey . "EMS_RENEW")))

  (setq sym (intern "CMP_GET1C" starfort-mode-token-table))
  (set sym "CMP_GET1C( {loc}, {name}, {elx}, {cvalue}, {el}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive vector component as CHARACTER")))

  (setq sym (intern "CMP_GET0I" starfort-mode-token-table))
  (set sym "CMP_GET0I( {loc}, {name}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive scalar component as INTEGER")))

  (setq sym (intern "NDF_XIARY" starfort-mode-token-table))
  (set sym "NDF_XIARY( {indf}, {xname}, {cmpt}, {mode}, {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain access to an array stored in an NDF extension") (helpkey . "NDF_XIARY")))

  (setq sym (intern "IIDSNP" starfort-mode-token-table))
  (set sym "IIDSNP( {dispid}, {cmode}, {npix}, {xstart}, {ystart}, {depth}, {pack}, {image}, {status} )")
  (setplist sym '((class . token) (desc . "Create Snapshot")))

  (setq sym (intern "CMP_GET0D" starfort-mode-token-table))
  (set sym "CMP_GET0D( {loc}, {name}, {dvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive scalar component as DOUBLE PRECISION")))

  (setq sym (intern "CMP_GET0C" starfort-mode-token-table))
  (set sym "CMP_GET0C( {loc}, {name}, {cvalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read primitive scalar component as CHARACTER")))

  (setq sym (intern "VEC_INTUB" starfort-mode-token-table))
  (set sym "VEC_INTUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Truncate UNSIGNED BYTE vectorised array to an integer")))

  (setq sym (intern "SLA_FLOTIN" starfort-mode-token-table))
  (set sym "SLA_FLOTIN( {string}, {nstrt}, {reslt}, {jflag} )")
  (setplist sym '((class . token) (desc . "Convert free-format input into single precision floating point")))

  (setq sym (intern "NDF_EXIST" starfort-mode-token-table))
  (set sym "NDF_EXIST( {param}, {mode}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "See if an existing NDF is associated with an ADAM parameter") (helpkey . "NDF_EXIST")))

  (setq sym (intern "GRP_COPY" starfort-mode-token-table))
  (set sym "GRP_COPY( {igrp}, {indxlo}, {indxhi}, {reject}, {igrp2}, {status} )")
  (setplist sym '((class . token) (desc . "Copy a section of an existing group to a new group")))

  (setq sym (intern "GCLRWK" starfort-mode-token-table))
  (set sym "GCLRWK( {wkid}, {gks$cofl} )")
  (setplist sym '((class . token) (desc . "Clear workstation")))

  (setq sym (intern "IIDRST" starfort-mode-token-table))
  (set sym "IIDRST( {dispid}, {status} )")
  (setplist sym '((class . token) (desc . "Reset Display")))

  (setq sym (intern "GSDTEC" starfort-mode-token-table))
  (set sym "GSDTEC( {sgna}, {gks$det} )")
  (setplist sym '((class . token) (desc . "Set detectability")))

  (setq sym (intern "AGP_DEACT" starfort-mode-token-table))
  (set sym "AGP_DEACT( {status} )")
  (setplist sym '((class . token) (desc . "Close down PGPLOT")))

  (setq sym (intern "DAT_VALID" starfort-mode-token-table))
  (set sym "DAT_VALID( {loc}, {reply}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire locator valid")))

  (setq sym (intern "PGVSIZE" starfort-mode-token-table))
  (set sym "PGVSIZE( {xleft}, {xright}, {ybot}, {ytop} )")
  (setplist sym '((class . token) (desc . "Set viewport (inches)")))

  (setq sym (intern "DAT_CREAT" starfort-mode-token-table))
  (set sym "DAT_CREAT( {param}, {type}, {ndim}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Create a data object via the ADAM parameter system")))

  (setq sym (intern "VAL_SQRTW" starfort-mode-token-table))
  (set sym "VAL_SQRTW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of WORD value")))

  (setq sym (intern "VAL_EXPW" starfort-mode-token-table))
  (set sym "VAL_EXPW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of WORD value")))

  (setq sym (intern "IOSTAT_PAR" starfort-mode-token-table))
  (set sym '(("IOSTAT_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "TIMING" starfort-mode-token-table))
  (set sym '(("TIMING" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "GDSG" starfort-mode-token-table))
  (set sym "GDSG( {sgna} )")
  (setplist sym '((class . token) (desc . "Delete segment")))

  (setq sym (intern "VAL_SQRTR" starfort-mode-token-table))
  (set sym "VAL_SQRTR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of REAL value")))

  (setq sym (intern "VAL_EXPR" starfort-mode-token-table))
  (set sym "VAL_EXPR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of REAL value")))

  (setq sym (intern "AGI_BEGIN" starfort-mode-token-table))
  (set sym "AGI_BEGIN")
  (setplist sym '((class . token) (desc . "Mark the beginning of a new AGI scope")))

  (setq sym (intern "VAL_SQRTI" starfort-mode-token-table))
  (set sym "VAL_SQRTI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of INTEGER value")))

  (setq sym (intern "VAL_EXPI" starfort-mode-token-table))
  (set sym "VAL_EXPI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of INTEGER value")))

  (setq sym (intern "VAL_SQRTD" starfort-mode-token-table))
  (set sym "VAL_SQRTD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of DOUBLE PRECISION value")))

  (setq sym (intern "VAL_EXPD" starfort-mode-token-table))
  (set sym "VAL_EXPD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of DOUBLE PRECISION value")))

  (setq sym (intern "VAL_SQRTB" starfort-mode-token-table))
  (set sym "VAL_SQRTB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Square root of BYTE value")))

  (setq sym (intern "VAL_EXPB" starfort-mode-token-table))
  (set sym "VAL_EXPB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Exponential function of BYTE value")))

  (setq sym (intern "HDS_STOP" starfort-mode-token-table))
  (set sym "HDS_STOP( {status} )")
  (setplist sym '((class . token) (desc . "Close down HDS")))

  (setq sym (intern "IILRLT" starfort-mode-token-table))
  (set sym "IILRLT( {dispid}, {lutnum}, {start}, {nent}, {vlut}, {status} )")
  (setplist sym '((class . token) (desc . "Read Video Look Up Table")))

  (setq sym (intern "IIDSEL" starfort-mode-token-table))
  (set sym "IIDSEL( {dispid}, {nconf}, {status} )")
  (setplist sym '((class . token) (desc . "Select Configuration")))

  (setq sym (intern "GQDSGA" starfort-mode-token-table))
  (set sym "GQDSGA( {wtype}, {errind}, {sgtr}, {vonoff}, {voffon}, {high}, {sgpr}, {add}, {sgdel} )")
  (setplist sym '((class . token) (desc . "Inquire dynamic modification of segment attributes")))

  (setq sym (intern "VAL_UWTOUW" starfort-mode-token-table))
  (set sym "VAL_UWTOUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD value to UNSIGNED WORD")))

  (setq sym (intern "IIDSDP" starfort-mode-token-table))
  (set sym "IIDSDP( {dispid}, {memid}, {nmem}, {lutlis}, {ittlis}, {status} )")
  (setplist sym '((class . token) (desc . "Select Display Path")))

  (setq sym (intern "TRN__TRNUD" starfort-mode-token-table))
  (set sym "TRN__TRNUD")
  (setplist sym '((class . token) (desc . "Transformation undefined (error code)")))

  (setq sym (intern "SLA_EQEQX" starfort-mode-token-table))
  (set sym "SLA_EQEQX( {date} )")
  (setplist sym '((class . token) (desc . "Equation of the equinoxes (double precision)")))

  (setq sym (intern "AGI_SLAB" starfort-mode-token-table))
  (set sym "AGI_SLAB( {picid}, {label}, {status} )")
  (setplist sym '((class . token) (desc . "Store label in picture")))

  (setq sym (intern "VEC_MULW" starfort-mode-token-table))
  (set sym "VEC_MULW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two WORD vectorised arrays")))

  (setq sym (intern "NUM_SUBW" starfort-mode-token-table))
  (set sym "NUM_SUBW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Subtract one WORD number from another")))

  (setq sym (intern "NDF_XGT0R" starfort-mode-token-table))
  (set sym "NDF_XGT0R( {indf}, {xname}, {cmpt}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Read a scalar real value from a component within a named NDF extension") (helpkey . "NDF_XGT0R")))

  (setq sym (intern "PAR_CANCL" starfort-mode-token-table))
  (set sym "PAR_CANCL( {param}, {status} )")
  (setplist sym '((class . token) (desc . "Cancel a parameter")))

  (setq sym (intern "IILSBV" starfort-mode-token-table))
  (set sym "IILSBV( {dispid}, {memid}, {lvis}, {status} )")
  (setplist sym '((class . token) (desc . "Set Intensity Bar Visibility")))

  (setq sym (intern "VEC_MULR" starfort-mode-token-table))
  (set sym "VEC_MULR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two REAL vectorised arrays")))

  (setq sym (intern "NUM_SUBR" starfort-mode-token-table))
  (set sym "NUM_SUBR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Subtract one REAL number from another")))

  (setq sym (intern "NDF_XGT0L" starfort-mode-token-table))
  (set sym "NDF_XGT0L( {indf}, {xname}, {cmpt}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Read a scalar logical value from a component within a named NDF extension") (helpkey . "NDF_XGT0L")))

  (setq sym (intern "IIDRLC" starfort-mode-token-table))
  (set sym "IIDRLC( {dispid}, {nconf}, {status} )")
  (setplist sym '((class . token) (desc . "Release Configuration")))

  (setq sym (intern "NDF_XGT0I" starfort-mode-token-table))
  (set sym "NDF_XGT0I( {indf}, {xname}, {cmpt}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Read a scalar integer value from a component within a named NDF extension") (helpkey . "NDF_XGT0I")))

  (setq sym (intern "AGI_ITOBS" starfort-mode-token-table))
  (set sym "AGI_ITOBS( {nxy}, {x}, {y}, {ltobs}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire if test points are obscured")))

  (setq sym (intern "SLA_EQGAL" starfort-mode-token-table))
  (set sym "SLA_EQGAL( {dr}, {dd}, {dl}, {db} )")
  (setplist sym '((class . token) (desc . "Transformation from J2000.0 equatorial coordinates to IAU 1958 galactic coordinates")))

  (setq sym (intern "SLA_DAFIN" starfort-mode-token-table))
  (set sym "SLA_DAFIN( {string}, {nstrt}, {dreslt}, {jf} )")
  (setplist sym '((class . token) (desc . "Sexagesimal character string to angle conversion")))

  (setq sym (intern "IILRIT" starfort-mode-token-table))
  (set sym "IILRIT( {dispid}, {memid}, {ittnum}, {start}, {nent}, {itt}, {status} )")
  (setplist sym '((class . token) (desc . "Read Intensity Transformation Table")))

  (setq sym (intern "VEC_MULI" starfort-mode-token-table))
  (set sym "VEC_MULI( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two INTEGER vectorised arrays")))

  (setq sym (intern "NUM_SUBI" starfort-mode-token-table))
  (set sym "NUM_SUBI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Subtract one INTEGER number from another")))

  (setq sym (intern "VAL_UWTOUB" starfort-mode-token-table))
  (set sym "VAL_UWTOUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED WORD value to UNSIGNED BYTE")))

  (setq sym (intern "VAL_UBTOUW" starfort-mode-token-table))
  (set sym "VAL_UBTOUW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE value to UNSIGNED WORD")))

  (setq sym (intern "NDF_XGT0D" starfort-mode-token-table))
  (set sym "NDF_XGT0D( {indf}, {xname}, {cmpt}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Read a scalar double precision value from a component within a named NDF extension") (helpkey . "NDF_XGT0D")))

  (setq sym (intern "GSFASI" starfort-mode-token-table))
  (set sym "GSFASI( {styli} )")
  (setplist sym '((class . token) (desc . "Set fill area style index")))

  (setq sym (intern "AGI_ICURP" starfort-mode-token-table))
  (set sym "AGI_ICURP( {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Inquire the current picture")))

  (setq sym (intern "NDF_XGT0C" starfort-mode-token-table))
  (set sym "NDF_XGT0C( {indf}, {xname}, {cmpt}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Read a scalar character value from a component within a named NDF extension") (helpkey . "NDF_XGT0C")))

  (setq sym (intern "GQFASI" starfort-mode-token-table))
  (set sym "GQFASI( {errind}, {styli} )")
  (setplist sym '((class . token) (desc . "Inquire fill area style index")))

  (setq sym (intern "VEC_MULD" starfort-mode-token-table))
  (set sym "VEC_MULD( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two DOUBLE PRECISION vectorised arrays")))

  (setq sym (intern "NUM_SUBD" starfort-mode-token-table))
  (set sym "NUM_SUBD( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Subtract one DOUBLE PRECISION number from another")))

  (setq sym (intern "VEC_LG10UW" starfort-mode-token-table))
  (set sym "VEC_LG10UW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of UNSIGNED WORD vectorised array")))

  (setq sym (intern "NDF_SHIFT" starfort-mode-token-table))
  (set sym "NDF_SHIFT( {nshift}, {shift}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Apply pixel-index shifts to an NDF") (helpkey . "NDF_SHIFT")))

  (setq sym (intern "VEC_MULB" starfort-mode-token-table))
  (set sym "VEC_MULB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two BYTE vectorised arrays")))

  (setq sym (intern "NUM_SUBB" starfort-mode-token-table))
  (set sym "NUM_SUBB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Subtract one BYTE number from another")))

  (setq sym (intern "PGDRAW" starfort-mode-token-table))
  (set sym "PGDRAW( {x}, {y} )")
  (setplist sym '((class . token) (desc . "Draw a line from the current pen position to a point")))

  (setq sym (intern "TRN__MRPAR" starfort-mode-token-table))
  (set sym "TRN__MRPAR")
  (setplist sym '((class . token) (desc . "Missing right parenthesis (error code)")))

  (setq sym (intern "PGBOX" starfort-mode-token-table))
  (set sym "PGBOX( {xopt}, {xtick}, {nxsub}, {yopt}, {ytick}, {nysub} )")
  (setplist sym '((class . token) (desc . "Draw labeled frame around viewport")))

  (setq sym (intern "SGS_ISLER" starfort-mode-token-table))
  (set sym "SGS_ISLER( {blker} )")
  (setplist sym '((class . token) (desc . "Inquire selective erase capability")))

  (setq sym (intern "VAL_UBTOUB" starfort-mode-token-table))
  (set sym "VAL_UBTOUB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Convert an UNSIGNED BYTE value to UNSIGNED BYTE")))

  (setq sym (intern "VEC_LG10UB" starfort-mode-token-table))
  (set sym "VEC_LG10UB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Common logarithm of UNSIGNED BYTE vectorised array")))

  (setq sym (intern "CHAR_RELATIONAL_EXP" starfort-mode-token-table))
  (set sym "{char_exp} {rop} {char_exp}")
  (setplist sym '((class . token) (desc . "Character relational expression")))

  (setq sym (intern "SLA_AFIN" starfort-mode-token-table))
  (set sym "SLA_AFIN( {string}, {nstrt}, {reslt}, {jf} )")
  (setplist sym '((class . token) (desc . "Sexagesimal character string to angle conversion")))

  (setq sym (intern "GDGP" starfort-mode-token-table))
  (set sym "GDGP( {n}, {px}, {py}, {primid}, {ldr}, {datrec} )")
  (setplist sym '((class . token) (desc . "Generalised drawing primitive")))

  (setq sym (intern "SGS_ZSIZE" starfort-mode-token-table))
  (set sym "SGS_ZSIZE( {xm}, {ym}, {pos}, {izonid}, {status} )")
  (setplist sym '((class . token) (desc . "Create a zone of a given size")))

  (setq sym (intern "PGASK" starfort-mode-token-table))
  (set sym "PGASK( {flag} )")
  (setplist sym '((class . token) (desc . "Control new page prompting")))

  (setq sym (intern "NUMBER_PAR" starfort-mode-token-table))
  (set sym '(("NUMBER_PAR" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NUM_MAXW" starfort-mode-token-table))
  (set sym "NUM_MAXW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Maximum of two WORD numbers")))

  (setq sym (intern "DAT_ERASE" starfort-mode-token-table))
  (set sym "DAT_ERASE( {loc}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Erase component")))

  (setq sym (intern "PGBIN" starfort-mode-token-table))
  (set sym "PGBIN( {nbin}, {x}, {data}, {center} )")
  (setplist sym '((class . token) (desc . "Histogram of binned data")))

  (setq sym (intern "GSFAIS" starfort-mode-token-table))
  (set sym "GSFAIS( {gks$ints} )")
  (setplist sym '((class . token) (desc . "Set fill area interior style")))

  (setq sym (intern "NUM_MAXR" starfort-mode-token-table))
  (set sym "NUM_MAXR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Maximum of two REAL numbers")))

  (setq sym (intern "ADAM_ACTION" starfort-mode-token-table))
  (set sym '(("ADAM_ACTION" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "GQFAIS" starfort-mode-token-table))
  (set sym "GQFAIS( {errind}, {gks$ints} )")
  (setplist sym '((class . token) (desc . "Inquire fill area interior style")))

  (setq sym (intern "CHR_EQUAL" starfort-mode-token-table))
  (set sym "CHR_EQUAL( {str1}, {str2} )")
  (setplist sym '((class . token) (desc . "Determine whether two strings are equal")))

  (setq sym (intern "CHR_ISDIG" starfort-mode-token-table))
  (set sym "CHR_ISDIG( {char} )")
  (setplist sym '((class . token) (desc . "Determine whether a character is a digit")))

  (setq sym (intern "NUM_MAXI" starfort-mode-token-table))
  (set sym "NUM_MAXI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Maximum of two INTEGER numbers")))

  (setq sym (intern "NUM_DIVW" starfort-mode-token-table))
  (set sym "NUM_DIVW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one WORD number by another")))

  (setq sym (intern "NUM_MAXD" starfort-mode-token-table))
  (set sym "NUM_MAXD( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Maximum of two DOUBLE PRECISION numbers")))

  (setq sym (intern "NUM_MAXB" starfort-mode-token-table))
  (set sym "NUM_MAXB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Maximum of two BYTE numbers")))

  (setq sym (intern "NUM_DIVR" starfort-mode-token-table))
  (set sym "NUM_DIVR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one REAL number by another")))

  (setq sym (intern "NUM_DIVI" starfort-mode-token-table))
  (set sym "NUM_DIVI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one INTEGER number by another")))

  (setq sym (intern "IF_THEN" starfort-mode-token-table))
  (set sym "IF ( {logical_exp} ) THEN
{executable_statement}...
[else_if_then]...
[else]
END IF")
  (setplist sym '((class . token) (desc . "IF ( {logical_exp} ) THEN...")))

  (setq sym (intern "IIDQDV" starfort-mode-token-table))
  (set sym "IIDQDV( {dispid}, {nconf}, {xsize}, {ysize}, {depth}, {nvlut}, {nitt}, {ncurs}, {status} )")
  (setplist sym '((class . token) (desc . "Query Device Characteristics")))

  (setq sym (intern "NUM_DIVD" starfort-mode-token-table))
  (set sym "NUM_DIVD( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one DOUBLE PRECISION number by another")))

  (setq sym (intern "TRN__WRNFA" starfort-mode-token-table))
  (set sym "TRN__WRNFA")
  (setplist sym '((class . token) (desc . "Wrong number of function arguments (error code)")))

  (setq sym (intern "NUM_DIVB" starfort-mode-token-table))
  (set sym "NUM_DIVB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Floating' division of one BYTE number by another")))

  (setq sym (intern "SLA_EQECL" starfort-mode-token-table))
  (set sym "SLA_EQECL( {dr}, {dd}, {date}, {dl}, {db} )")
  (setplist sym '((class . token) (desc . "Transformation from J2000.0 equatorial coordinates to ecliptic coordinates")))

  (setq sym (intern "FIO_UNIT" starfort-mode-token-table))
  (set sym "FIO_UNIT( {fd}, {unit}, {status} )")
  (setplist sym '((class . token) (desc . "Get a unit number given a file descriptor")))

  (setq sym (intern "IIDQCR" starfort-mode-token-table))
  (set sym "IIDQCR( {dispid}, {capid}, {narr}, {outarr}, {nout}, {status} )")
  (setplist sym '((class . token) (desc . "Query Capabilities Real")))

  (setq sym (intern "IIDQDC" starfort-mode-token-table))
  (set sym "IIDQDC( {dispid}, {nconf}, {memtyp}, {nmemax}, {modcon}, {memid}, {memsix}, {memsiy}, {memdep}, {ittdep}, {nmem}, {status} )")
  (setplist sym '((class . token) (desc . "Query Defined Configuration")))

  (setq sym (intern "IIDQCI" starfort-mode-token-table))
  (set sym "IIDQCI( {dispid}, {capid}, {narr}, {outarr}, {nout}, {status} )")
  (setplist sym '((class . token) (desc . "Query Capabilities Integer")))

  (setq sym (intern "GSFACI" starfort-mode-token-table))
  (set sym "GSFACI( {facoli} )")
  (setplist sym '((class . token) (desc . "Set fill area colour index")))

  (setq sym (intern "GQFACI" starfort-mode-token-table))
  (set sym "GQFACI( {errind}, {coli} )")
  (setplist sym '((class . token) (desc . "Inquire fill area colour index")))

  (setq sym (intern "IIDOPN" starfort-mode-token-table))
  (set sym "IIDOPN( {devnam}, {dispid}, {status} )")
  (setplist sym '((class . token) (desc . "Open Display")))

  (setq sym (intern "NUM_IDVUW" starfort-mode-token-table))
  (set sym "NUM_IDVUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one UNSIGNED WORD number by another")))

  (setq sym (intern "NUM_DIMW" starfort-mode-token-table))
  (set sym "NUM_DIMW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Positive difference of two WORD numbers")))

  (setq sym (intern "NUM_DIMR" starfort-mode-token-table))
  (set sym "NUM_DIMR( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Positive difference of two REAL numbers")))

  (setq sym (intern "SLA_DS2TP" starfort-mode-token-table))
  (set sym "SLA_DS2TP( {ra}, {dec}, {raz}, {decz}, {xi}, {eta}, {j} )")
  (setplist sym '((class . token) (desc . "Projection of spherical coordinates onto tangent plane ('gnomonic' projection - 'standard coordinates')")))

  (setq sym (intern "TRN__MISVN" starfort-mode-token-table))
  (set sym "TRN__MISVN")
  (setplist sym '((class . token) (desc . "Missing variable name (error code)")))

  (setq sym (intern "FUNCTION" starfort-mode-token-table))
  (set sym '(("FUNCTION_PROGRAM_MODULE" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "NUM_IDVUB" starfort-mode-token-table))
  (set sym "NUM_IDVUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "'Integer' division of one UNSIGNED BYTE number by another")))

  (setq sym (intern "VAL_MULW" starfort-mode-token-table))
  (set sym "VAL_MULW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two WORD values")))

  (setq sym (intern "NUM_DIMI" starfort-mode-token-table))
  (set sym "NUM_DIMI( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Positive difference of two INTEGER numbers")))

  (setq sym (intern "AGI_ASSOC" starfort-mode-token-table))
  (set sym "AGI_ASSOC( {param}, {acmode}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Associate an AGI device with an ADAM parameter")))

  (setq sym (intern "VAL_MULR" starfort-mode-token-table))
  (set sym "VAL_MULR( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two REAL values")))

  (setq sym (intern "NUM_DIMD" starfort-mode-token-table))
  (set sym "NUM_DIMD( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Positive difference of two DOUBLE PRECISION numbers")))

  (setq sym (intern "HDS_LINK" starfort-mode-token-table))
  (set sym "HDS_LINK( {loc}, {group}, {status} )")
  (setplist sym '((class . token) (desc . "Link locator to a group")))

  (setq sym (intern "SLA_DC62S" starfort-mode-token-table))
  (set sym "SLA_DC62S( {v}, {a}, {b}, {r}, {ad}, {bd}, {rd} )")
  (setplist sym '((class . token) (desc . "Conversion of position & velocity in Cartesian coordinates to spherical coordinates")))

  (setq sym (intern "NUM_DIMB" starfort-mode-token-table))
  (set sym "NUM_DIMB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Positive difference of two BYTE numbers")))

  (setq sym (intern "SLA_DAF2R" starfort-mode-token-table))
  (set sym "SLA_DAF2R( {ideg}, {iamin}, {asec}, {rad}, {j} )")
  (setplist sym '((class . token) (desc . "Convert degrees, arcminutes, arcseconds to radians")))

  (setq sym (intern "SLA_CC62S" starfort-mode-token-table))
  (set sym "SLA_CC62S( {v}, {a}, {b}, {r}, {ad}, {bd}, {rd} )")
  (setplist sym '((class . token) (desc . "Conversion of position & velocity in Cartesian coordinates to spherical coordinates")))

  (setq sym (intern "RIO_CANCL" starfort-mode-token-table))
  (set sym "RIO_CANCL( {pname}, {status} )")
  (setplist sym '((class . token) (desc . "Close a file and cancel the parameter")))

  (setq sym (intern "SLA_CAF2R" starfort-mode-token-table))
  (set sym "SLA_CAF2R( {ideg}, {iamin}, {asec}, {rad}, {j} )")
  (setplist sym '((class . token) (desc . "Convert degrees, arcminutes, arcseconds to radians")))

  (setq sym (intern "CHR_UCASE" starfort-mode-token-table))
  (set sym "CHR_UCASE( {string} )")
  (setplist sym '((class . token) (desc . "Convert a string to upper case")))

  (setq sym (intern "SLA_SUPGAL" starfort-mode-token-table))
  (set sym "SLA_SUPGAL( {dsl}, {dsb}, {dl}, {db} )")
  (setplist sym '((class . token) (desc . "Transformation from de Vaucouleurs supergalactic coordinates to IAU 1958 galactic coordinates")))

  (setq sym (intern "VAL_MULI" starfort-mode-token-table))
  (set sym "VAL_MULI( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two INTEGER values")))

  (setq sym (intern "VAL_MULD" starfort-mode-token-table))
  (set sym "VAL_MULD( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two DOUBLE PRECISION values")))

  (setq sym (intern "FIO_CANCL" starfort-mode-token-table))
  (set sym "FIO_CANCL( {pname}, {status} )")
  (setplist sym '((class . token) (desc . "Close a file and cancel the parameter")))

  (setq sym (intern "VAL_MULB" starfort-mode-token-table))
  (set sym "VAL_MULB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Multiply two BYTE values")))

  (setq sym (intern "CHR_LCASE" starfort-mode-token-table))
  (set sym "CHR_LCASE( {string} )")
  (setplist sym '((class . token) (desc . "Convert a string to lower case")))

  (setq sym (intern "PSX_GETEGID" starfort-mode-token-table))
  (set sym "PSX_GETEGID( {gid}, {status} )")
  (setplist sym '((class . token) (desc . "Gets the effective group ID")))

  (setq sym (intern "GWM_CRWIN" starfort-mode-token-table))
  (set sym "GWM_CRWIN( {wname}, {status} )")
  (setplist sym '((class . token) (desc . "Create a GWM window")))

  (setq sym (intern "FILE" starfort-mode-token-table))
  (set sym "FILE = {char_exp}")
  (setplist sym '((class . token) (desc . "FILE = {char_exp}")))

  (setq sym (intern "SLA_S2TP" starfort-mode-token-table))
  (set sym "SLA_S2TP( {ra}, {dec}, {raz}, {decz}, {xi}, {eta}, {j} )")
  (setplist sym '((class . token) (desc . "Projection of spherical coordinates onto tangent plane ('gnomonic' projection - 'standard coordinates')")))

  (setq sym (intern "VEC_LOGUW" starfort-mode-token-table))
  (set sym "VEC_LOGUW( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of UNSIGNED WORD vectorised array")))

  (setq sym (intern "VAL_MINUW" starfort-mode-token-table))
  (set sym "VAL_MINUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two UNSIGNED WORD values")))

  (setq sym (intern "PROGRAM_MODULES" starfort-mode-token-table))
  (set sym '(("PROGRAM_MODULE" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "TRN__ICDIR" starfort-mode-token-table))
  (set sym "TRN__ICDIR")
  (setplist sym '((class . token) (desc . "Incompatible transformation directions (error code)")))

  (setq sym (intern "VAL_NINTW" starfort-mode-token-table))
  (set sym "VAL_NINTW( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to WORD value")))

  (setq sym (intern "OPEN_STATUS" starfort-mode-token-table))
  (set sym "STATUS = {open_status_options}")
  (setplist sym '((class . token) (desc . "STATUS = {open_status_options}")))

  (setq sym (intern "VAL_NINTR" starfort-mode-token-table))
  (set sym "VAL_NINTR( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to REAL value")))

  (setq sym (intern "SLA_FK54Z" starfort-mode-token-table))
  (set sym "SLA_FK54Z( {r2000}, {d2000}, {bepoch}, {r1950}, {d1950}, {dr1950}, {dd1950} )")
  (setplist sym '((class . token) (desc . "Convert a J2000.0 FK5 star position to B1950.0 FK4 assuming zero proper motion and parallax.")))

  (setq sym (intern "NDF_XNUMB" starfort-mode-token-table))
  (set sym "NDF_XNUMB( {indf}, {xnumb}, {status} )")
  (setplist sym '((class . token) (desc . "Determine the number of extensions in an NDF") (helpkey . "NDF_XNUMB")))

  (setq sym (intern "SIDE_EFFECTS" starfort-mode-token-table))
  (set sym '(("SIDE_EFFECTS" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "VEC_LOGUB" starfort-mode-token-table))
  (set sym "VEC_LOGUB( {bad}, {n}, {vec}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Natural logarithm of UNSIGNED BYTE vectorised array")))

  (setq sym (intern "VAL_NINTI" starfort-mode-token-table))
  (set sym "VAL_NINTI( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to INTEGER value")))

  (setq sym (intern "VAL_MINUB" starfort-mode-token-table))
  (set sym "VAL_MINUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Minimum of two UNSIGNED BYTE values")))

  (setq sym (intern "ARY_BASE" starfort-mode-token-table))
  (set sym "ARY_BASE( {iary1}, {iary2}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain an identifier for a base array")))

  (setq sym (intern "VAL_NINTD" starfort-mode-token-table))
  (set sym "VAL_NINTD( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to DOUBLE PRECISION value")))

  (setq sym (intern "IIEGEP" starfort-mode-token-table))
  (set sym "IIEGEP( {param}, {slen}, {string}, {status} )")
  (setplist sym '((class . token) (desc . "Get Escape Parameter")))

  (setq sym (intern "VAL_NINTB" starfort-mode-token-table))
  (set sym "VAL_NINTB( {bad}, {val}, {status} )")
  (setplist sym '((class . token) (desc . "Nearest integer to BYTE value")))

  (setq sym (intern "CMP_STRUC" starfort-mode-token-table))
  (set sym "CMP_STRUC( {loc}, {name}, {reply}, {status} )")
  (setplist sym '((class . token) (desc . "Enquire component structure")))

  (setq sym (intern "NUM_SQRTUW" starfort-mode-token-table))
  (set sym "NUM_SQRTUW( {num} )")
  (setplist sym '((class . token) (desc . "Square root of UNSIGNED WORD number")))

  (setq sym (intern "CHR_ISALM" starfort-mode-token-table))
  (set sym "CHR_ISALM( {char} )")
  (setplist sym '((class . token) (desc . "Determine whether a character is alphanumeric")))

  (setq sym (intern "TRN__VARUD" starfort-mode-token-table))
  (set sym "TRN__VARUD")
  (setplist sym '((class . token) (desc . "Variable name undefined (error code)")))

  (setq sym (intern "NUM_COSR" starfort-mode-token-table))
  (set sym "NUM_COSR( {num} )")
  (setplist sym '((class . token) (desc . "Cosine function of REAL number (radians)")))

  (setq sym (intern "CHR_OTOI" starfort-mode-token-table))
  (set sym "CHR_OTOI( {string}, {ivalue}, {status} )")
  (setplist sym '((class . token) (desc . "Read an integer number from an octal string")))

  (setq sym (intern "SGS_IDUN" starfort-mode-token-table))
  (set sym "SGS_IDUN( {dxw}, {dyw} )")
  (setplist sym '((class . token) (desc . "Inquire device units")))

  (setq sym (intern "IICWCP" starfort-mode-token-table))
  (set sym "IICWCP( {dispid}, {memid}, {numcur}, {xc}, {yc}, {status} )")
  (setplist sym '((class . token) (desc . "Write Cursor Position")))

  (setq sym (intern "GQEGDP" starfort-mode-token-table))
  (set sym "GQEGDP( {wtype}, {n}, {errind}, {ngdp}, {gdpl} )")
  (setplist sym '((class . token) (desc . "Inquire list element of available generalized drawing primitives")))

  (setq sym (intern "CHR_ISALF" starfort-mode-token-table))
  (set sym "CHR_ISALF( {char} )")
  (setplist sym '((class . token) (desc . "Determine whether a character is alphabetic")))

  (setq sym (intern "RIO_READ" starfort-mode-token-table))
  (set sym "RIO_READ( {fd}, {recno}, {nchar}, {buf}, {status} )")
  (setplist sym '((class . token) (desc . "Read record from direct access file")))

  (setq sym (intern "NUM_COSD" starfort-mode-token-table))
  (set sym "NUM_COSD( {num} )")
  (setplist sym '((class . token) (desc . "Cosine function of DOUBLE PRECISION number (radians)")))

  (setq sym (intern "NUM_SQRTUB" starfort-mode-token-table))
  (set sym "NUM_SQRTUB( {num} )")
  (setplist sym '((class . token) (desc . "Square root of UNSIGNED BYTE number")))

  (setq sym (intern "NDF_TYPE" starfort-mode-token-table))
  (set sym "NDF_TYPE( {indf}, {comp}, {type}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain the numeric type of an NDF array component") (helpkey . "NDF_TYPE")))

  (setq sym (intern "SLA_FK524" starfort-mode-token-table))
  (set sym "SLA_FK524( {r2000}, {d2000}, {dr2000}, {dd2000}, {p2000}, {v2000}, {r1950}, {d1950}, {dr1950}, {dd1950}, {p1950}, {v1950} )")
  (setplist sym '((class . token) (desc . "Convert J2000.0 FK5 star data to B1950.0 FK4")))

  (setq sym (intern "NUM_MULUW" starfort-mode-token-table))
  (set sym "NUM_MULUW( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Multiply two UNSIGNED WORD numbers")))

  (setq sym (intern "SLA_FK45Z" starfort-mode-token-table))
  (set sym "SLA_FK45Z( {r1950}, {d1950}, {bepoch}, {r2000}, {d2000} )")
  (setplist sym '((class . token) (desc . "Convert B1950.0 FK4 star data to J2000.0 FK5 assuming zero proper motion in an inertial frame.")))

  (setq sym (intern "VAL_DIMUW" starfort-mode-token-table))
  (set sym "VAL_DIMUW( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two UNSIGNED WORD values")))

  (setq sym (intern "HDS_COPY" starfort-mode-token-table))
  (set sym "HDS_COPY( {loc}, {file}, {name}, {status} )")
  (setplist sym '((class . token) (desc . "Copy an object to a new container file")))

  (setq sym (intern "CHARACTER" starfort-mode-token-table))
  (set sym "CHARACTER * ( {len} )")
  (setplist sym '((class . token) (desc . "CHARACTER data type")))

  (setq sym (intern "SGS_IZONE" starfort-mode-token-table))
  (set sym "SGS_IZONE( {x1}, {x2}, {y1}, {y2}, {xm}, {ym} )")
  (setplist sym '((class . token) (desc . "Inquire zone attributes")))

  (setq sym (intern "NDF_FIND" starfort-mode-token-table))
  (set sym "NDF_FIND( {loc}, {name}, {indf}, {status} )")
  (setplist sym '((class . token) (desc . "Find an NDF in an HDS structure and import it into the NDF_ system") (helpkey . "NDF_FIND")))

  (setq sym (intern "AGS_SZONE" starfort-mode-token-table))
  (set sym "AGS_SZONE( {pname}, {coment}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Save the current SGS zone in the database")))

  (setq sym (intern "SCRATCH" starfort-mode-token-table))
  (set sym "'SCRATCH'")
  (setplist sym '((class . token) (desc . "'SCRATCH'")))

  (setq sym (intern "NUM_MULUB" starfort-mode-token-table))
  (set sym "NUM_MULUB( {num1}, {num2} )")
  (setplist sym '((class . token) (desc . "Multiply two UNSIGNED BYTE numbers")))

  (setq sym (intern "SGS_ZSHAP" starfort-mode-token-table))
  (set sym "SGS_ZSHAP( {ar}, {pos}, {izonid}, {status} )")
  (setplist sym '((class . token) (desc . "Create a zone of a given shape")))

  (setq sym (intern "AGS_NZONE" starfort-mode-token-table))
  (set sym "AGS_NZONE( {newzon}, {status} )")
  (setplist sym '((class . token) (desc . "Create a new SGS zone from the current picture")))

  (setq sym (intern "VAL_DIMUB" starfort-mode-token-table))
  (set sym "VAL_DIMUB( {bad}, {val1}, {val2}, {status} )")
  (setplist sym '((class . token) (desc . "Positive difference of two UNSIGNED BYTE values")))

  (setq sym (intern "GKS_ANNUL" starfort-mode-token-table))
  (set sym "GKS_ANNUL( {wkid}, {status} )")
  (setplist sym '((class . token) (desc . "Close graphics workstation without cancelling parameter")))

  (setq sym (intern "SLA_AOPQK" starfort-mode-token-table))
  (set sym "SLA_AOPQK( {rap}, {dap}, {aoprms}, {aob}, {zob}, {hob}, {dob}, {rob} )")
  (setplist sym '((class . token) (desc . "Quick apparent to observed place")))

  (setq sym (intern "SLA_DS2C6" starfort-mode-token-table))
  (set sym "SLA_DS2C6( {a}, {b}, {r}, {ad}, {bd}, {rd}, {v} )")
  (setplist sym '((class . token) (desc . "Conversion of position & velocity in spherical coordinates to Cartesian coordinates")))

  (setq sym (intern "SLA_CS2C6" starfort-mode-token-table))
  (set sym "SLA_CS2C6( {a}, {b}, {r}, {ad}, {bd}, {rd}, {v} )")
  (setplist sym '((class . token) (desc . "Conversion of position & velocity in spherical coordinates to Cartesian coordinates")))

  (setq sym (intern "TRN__VARIN" starfort-mode-token-table))
  (set sym "TRN__VARIN")
  (setplist sym '((class . token) (desc . "Variable name invalid (error code)")))

  (setq sym (intern "AGI_SELP" starfort-mode-token-table))
  (set sym "AGI_SELP( {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Select the given picture as the current one")))

  (setq sym (intern "SLA_FK425" starfort-mode-token-table))
  (set sym "SLA_FK425( {r1950}, {d1950}, {dr1950}, {dd1950}, {p1950}, {v1950}, {r2000}, {d2000}, {dr2000}, {dd2000}, {p2000}, {v2000} )")
  (setplist sym '((class . token) (desc . "Convert B1950.0 FK4 star data to J2000.0 FK5")))

  (setq sym (intern "EMS_BEGIN" starfort-mode-token-table))
  (set sym "EMS_BEGIN( {status} )")
  (setplist sym '((class . token) (desc . "Begin a new error reporting environment") (helpkey . "EMS_BEGIN")))

  (setq sym (intern "GQEFAI" starfort-mode-token-table))
  (set sym "GQEFAI( {wkid}, {n}, {errind}, {ol}, {faind} )")
  (setplist sym '((class . token) (desc . "Inquire list element of fill area indices")))

  (setq sym (intern "SLA_AOPPA" starfort-mode-token-table))
  (set sym "SLA_AOPPA( {date}, {dut}, {elongm}, {phim}, {hm}, {xp}, {yp}, {tdk}, {pmb}, {rh}, {wl}, {tlr}, {aoprms} )")
  (setplist sym '((class . token) (desc . "Precompute apparent to observed place parameters required by SLA_AOPQK and SLA_OAPQK.")))

  (setq sym (intern "MSG_SYNC" starfort-mode-token-table))
  (set sym "MSG_SYNC( {status} )")
  (setplist sym '((class . token) (desc . "Synchronise message output via the user interface") (helpkey . "MSG_SYNC")))

  (setq sym (intern "DAT__SUBIN" starfort-mode-token-table))
  (set sym "DAT__SUBIN")
  (setplist sym '((class . token) (desc . "Subscripts invalid (error code)")))

  (setq sym (intern "MAG_WRITE" starfort-mode-token-table))
  (set sym "MAG_WRITE( {td}, {nval}, {values}, {actval}, {status} )")
  (setplist sym '((class . token) (desc . "Write a block to tape")))

  (setq sym (intern "PROLOGUES" starfort-mode-token-table))
  (set sym '(("PROLOGUES" nil place)))
  (setplist sym '((class . token)))

  (setq sym (intern "DAT_MOULD" starfort-mode-token-table))
  (set sym "DAT_MOULD( {loc}, {ndim}, {dim}, {status} )")
  (setplist sym '((class . token) (desc . "Alter object shape")))

  (setq sym (intern "RIO_ERASE" starfort-mode-token-table))
  (set sym "RIO_ERASE( {file}, {status} )")
  (setplist sym '((class . token) (desc . "Delete a file from the file-base")))

  (setq sym (intern "PGEBUF" starfort-mode-token-table))
  (set sym "PGEBUF")
  (setplist sym '((class . token) (desc . "End batch of output (buffer)")))

  (setq sym (intern "TRN__MAPUD" starfort-mode-token-table))
  (set sym "TRN__MAPUD")
  (setplist sym '((class . token) (desc . "Mapping undefined (error code)")))

  (setq sym (intern "HDS_FLUSH" starfort-mode-token-table))
  (set sym "HDS_FLUSH( {group}, {status} )")
  (setplist sym '((class . token) (desc . "Flush locator group")))

  (setq sym (intern "AGI_TCOPY" starfort-mode-token-table))
  (set sym "AGI_TCOPY( {trnloc}, {picid}, {status} )")
  (setplist sym '((class . token) (desc . "Copy a transformation structure to the database")))

  (setq sym (intern "IIMEBM" starfort-mode-token-table))
  (set sym "IIMEBM( {dispid}, {bmdscr}, {bmtype}, {xsize}, {ysize}, {status} )")
  (setplist sym '((class . token) (desc . "Define External Bitmap")))

  (setq sym (intern "FIO_ERASE" starfort-mode-token-table))
  (set sym "FIO_ERASE( {file}, {status} )")
  (setplist sym '((class . token) (desc . "Delete a file from the file-base")))

  (setq sym (intern "VEC_MODUW" starfort-mode-token-table))
  (set sym "VEC_MODUW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two UNSIGNED WORD vectorised arrays")))

  (setq sym (intern "EMS_MLOAD" starfort-mode-token-table))
  (set sym "EMS_MLOAD( {param}, {text}, {opstr}, {oplen}, {status} )")
  (setplist sym '((class . token) (desc . "Expand and return a message") (helpkey . "EMS_MLOAD")))

  (setq sym (intern "EMS_ELOAD" starfort-mode-token-table))
  (set sym "EMS_ELOAD( {param}, {parlen}, {opstr}, {oplen}, {status} )")
  (setplist sym '((class . token) (desc . "Return error messages from the current error context") (helpkey . "EMS_ELOAD")))

  (setq sym (intern "IIMCMY" starfort-mode-token-table))
  (set sym "IIMCMY( {dispid}, {memid}, {nmem}, {back}, {status} )")
  (setplist sym '((class . token) (desc . "Clear Memory")))

  (setq sym (intern "VEC_MODUB" starfort-mode-token-table))
  (set sym "VEC_MODUB( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two UNSIGNED BYTE vectorised arrays")))

  (setq sym (intern "HDS_GTUNE" starfort-mode-token-table))
  (set sym "HDS_GTUNE( {param}, {value}, {status} )")
  (setplist sym '((class . token) (desc . "Obtain tuning parameter value")))

  (setq sym (intern "IDI_ANNUL" starfort-mode-token-table))
  (set sym "IDI_ANNUL( {dispid}, {status} )")
  (setplist sym '((class . token) (desc . "Annul the given display identifier")))

  (setq sym (intern "SGS_BZNDC" starfort-mode-token-table))
  (set sym "SGS_BZNDC( {x1}, {x2}, {y1}, {y2}, {pos}, {status} )")
  (setplist sym '((class . token) (desc . "Set a base zone extent in NDC")))

  (setq sym (intern "VEC_MODW" starfort-mode-token-table))
  (set sym "VEC_MODW( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two WORD vectorised arrays")))

  (setq sym (intern "ARY_ANNUL" starfort-mode-token-table))
  (set sym "ARY_ANNUL( {iary}, {status} )")
  (setplist sym '((class . token) (desc . "Annul an array identifier")))

  (setq sym (intern "VEC_MODR" starfort-mode-token-table))
  (set sym "VEC_MODR( {bad}, {n}, {vec1}, {vec2}, {resvec}, {ierr}, {nerr}, {status} )")
  (setplist sym '((class . token) (desc . "Fortran MOD function of two REAL vectorised arrays")))
)
(defvar starfort-mode-helpkey-table (make-vector 4095 0))
(let (sym)
  (setq sym (intern "NDF_NEWP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_NEWP"))

  (setq sym (intern "NDF_TUNE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_TUNE"))

  (setq sym (intern "ERR_LOAD" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_LOAD"))

  (setq sym (intern "NDF_ANORM" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ANORM"))

  (setq sym (intern "MSG_LOAD" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_LOAD"))

  (setq sym (intern "NDF_AFORM" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_AFORM"))

  (setq sym (intern "NDF_ANNUL" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ANNUL"))

  (setq sym (intern "ERR_STAT" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_STAT"))

  (setq sym (intern "ERR_MARK" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_MARK"))

  (setq sym (intern "NDF_UNMAP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_UNMAP"))

  (setq sym (intern "MSG_IFSET" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_IFSET"))

  (setq sym (intern "ERR_ANNUL" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_ANNUL"))

  (setq sym (intern "NDF_RESET" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_RESET"))

  (setq sym (intern "NDF_MTYPN" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_MTYPN"))

  (setq sym (intern "NDF_STYPE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_STYPE"))

  (setq sym (intern "NDF_MTYPE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_MTYPE"))

  (setq sym (intern "NDF_FTYPE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_FTYPE"))

  (setq sym (intern "NDF_ATYPE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ATYPE"))

  (setq sym (intern "ERR_LEVEL" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_LEVEL"))

  (setq sym (intern "NDF_IMPRT" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_IMPRT"))

  (setq sym (intern "EMS_STAT" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_STAT"))

  (setq sym (intern "NDF_CMPLX" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_CMPLX"))

  (setq sym (intern "NDF_NOACC" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_NOACC"))

  (setq sym (intern "EMS_MARK" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_MARK"))

  (setq sym (intern "NDF_AUNMP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_AUNMP"))

  (setq sym (intern "NDF_MBND" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_MBND"))

  (setq sym (intern "NDF_GTUNE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_GTUNE"))

  (setq sym (intern "NDF_MAPZ" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_MAPZ"))

  (setq sym (intern "MSG_OUTIF" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_OUTIF"))

  (setq sym (intern "MSG_IFLEV" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_IFLEV"))

  (setq sym (intern "NDF_CPUT" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_CPUT"))

  (setq sym (intern "NDF_CREP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_CREP"))

  (setq sym (intern "NDF_SQMF" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_SQMF"))

  (setq sym (intern "NDF_MBAD" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_MBAD"))

  (setq sym (intern "NDF_DELET" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_DELET"))

  (setq sym (intern "NDF_COPY" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_COPY"))

  (setq sym (intern "ERR_FLUSH" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_FLUSH"))

  (setq sym (intern "NDF_XNAME" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XNAME"))

  (setq sym (intern "NDF_CMSG" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_CMSG"))

  (setq sym (intern "MSG_SETR" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_SETx"))

  (setq sym (intern "NDF_TEMP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_TEMP"))

  (setq sym (intern "MSG_SETL" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_SETx"))

  (setq sym (intern "MSG_SETI" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_SETx"))

  (setq sym (intern "MSG_SETD" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_SETx"))

  (setq sym (intern "MSG_SETC" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_SETx"))

  (setq sym (intern "ERR_RLSE" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_RLSE"))

  (setq sym (intern "EMS_SYSER" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_SYSER"))

  (setq sym (intern "MSG_IFGET" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_IFGET"))

  (setq sym (intern "NDF_CLONE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_CLONE"))

  (setq sym (intern "NDF_BEGIN" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_BEGIN"))

  (setq sym (intern "MSG_RENEW" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_RENEW"))

  (setq sym (intern "NDF_CLEN" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_CLEN"))

  (setq sym (intern "NDF_SBB" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_SBB"))

  (setq sym (intern "NDF_BLOCK" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_BLOCK"))

  (setq sym (intern "NDF_SIZE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_SIZE"))

  (setq sym (intern "NDF_ASTYP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ASTYP"))

  (setq sym (intern "MSG_OUT" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_OUT"))

  (setq sym (intern "NDF_QMF" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_QMF"))

  (setq sym (intern "NDF_CINP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_CINP"))

  (setq sym (intern "NDF_ISTMP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ISTMP"))

  (setq sym (intern "EMS_SETR" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_SETx"))

  (setq sym (intern "EMS_SETL" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_SETx"))

  (setq sym (intern "EMS_SETI" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_SETx"))

  (setq sym (intern "EMS_SETD" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_SETx"))

  (setq sym (intern "EMS_SETC" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_SETx"))

  (setq sym (intern "NDF_ASSOC" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ASSOC"))

  (setq sym (intern "EMS_FIOER" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_FIOER"))

  (setq sym (intern "NDF_XSTAT" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XSTAT"))

  (setq sym (intern "NDF_ASTAT" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ASTAT"))

  (setq sym (intern "ERR_BEGIN" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_BEGIN"))

  (setq sym (intern "EMS_RLSE" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_RLSE"))

  (setq sym (intern "NDF_MSG" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_MSG"))

  (setq sym (intern "NDF_QMASK" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_QMASK"))

  (setq sym (intern "NDF_NEW" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_NEW"))

  (setq sym (intern "NDF_CGET" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_CGET"))

  (setq sym (intern "NDF_ACPUT" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ACPUT"))

  (setq sym (intern "NDF_LOC" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_LOC"))

  (setq sym (intern "NDF_MAP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_MAP"))

  (setq sym (intern "NDF_SECT" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_SECT"))

  (setq sym (intern "NDF_ASNRM" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ASNRM"))

  (setq sym (intern "ERR_REP" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_REP"))

  (setq sym (intern "EMS_REP" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_REP"))

  (setq sym (intern "NDF_ACMSG" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ACMSG"))

  (setq sym (intern "NDF_SBND" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_SBND"))

  (setq sym (intern "NDF_BB" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_BB"))

  (setq sym (intern "NDF_SAME" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_SAME"))

  (setq sym (intern "NDF_SBAD" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_SBAD"))

  (setq sym (intern "NDF_ACLEN" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ACLEN"))

  (setq sym (intern "NDF_STATE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_STATE"))

  (setq sym (intern "NDF_END" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_END"))

  (setq sym (intern "NDF_NCHNK" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_NCHNK"))

  (setq sym (intern "NDF_DIM" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_DIM"))

  (setq sym (intern "NDF_PLACE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_PLACE"))

  (setq sym (intern "NDF_AMAP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_AMAP"))

  (setq sym (intern "NDF_ACGET" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ACGET"))

  (setq sym (intern "NDF_PROP" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_PROP"))

  (setq sym (intern "NDF_MBNDN" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_MBNDN"))

  (setq sym (intern "NDF_BASE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_BASE"))

  (setq sym (intern "NDF_BAD" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_BAD"))

  (setq sym (intern "NDF_NBLOC" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_NBLOC"))

  (setq sym (intern "ERR_END" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_END"))

  (setq sym (intern "MSG_BLANK" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_BLANK"))

  (setq sym (intern "NDF_SSARY" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_SSARY"))

  (setq sym (intern "EMS_END" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_END"))

  (setq sym (intern "NDF_ISBAS" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ISBAS"))

  (setq sym (intern "NDF_MAPQL" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_MAPQL"))

  (setq sym (intern "NDF_XNEW" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XNEW"))

  (setq sym (intern "NDF_ISACC" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ISACC"))

  (setq sym (intern "NDF_XLOC" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XLOC"))

  (setq sym (intern "NDF_ACRE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_ACRE"))

  (setq sym (intern "ERR_SYSER" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_SYSER"))

  (setq sym (intern "NDF_AREST" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_AREST"))

  (setq sym (intern "NDF_CHUNK" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_CHUNK"))

  (setq sym (intern "EMS_ANNUL" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_ANNUL"))

  (setq sym (intern "NDF_VALID" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_VALID"))

  (setq sym (intern "NDF_CREAT" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_CREAT"))

  (setq sym (intern "ERR_FIOER" starfort-mode-helpkey-table))
  (set sym '("sun104" "ERR_FIOER"))

  (setq sym (intern "EMS_LEVEL" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_LEVEL"))

  (setq sym (intern "NDF_XPT0R" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XPT0x"))

  (setq sym (intern "NDF_XPT0L" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XPT0x"))

  (setq sym (intern "NDF_XPT0I" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XPT0x"))

  (setq sym (intern "NDF_XPT0D" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XPT0x"))

  (setq sym (intern "NDF_XPT0C" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XPT0x"))

  (setq sym (intern "MSG_FMTR" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_FMTx"))

  (setq sym (intern "MSG_FMTL" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_FMTx"))

  (setq sym (intern "MSG_FMTI" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_FMTx"))

  (setq sym (intern "NDF_MBADN" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_MBADN"))

  (setq sym (intern "MSG_FMTD" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_FMTx"))

  (setq sym (intern "MSG_FMTC" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_FMTx"))

  (setq sym (intern "NDF_TRACE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_TRACE"))

  (setq sym (intern "NDF_XDEL" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XDEL"))

  (setq sym (intern "NDF_BOUND" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_BOUND"))

  (setq sym (intern "EMS_FMTR" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_FMTx"))

  (setq sym (intern "EMS_FMTL" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_FMTx"))

  (setq sym (intern "EMS_FMTI" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_FMTx"))

  (setq sym (intern "EMS_FMTD" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_FMTx"))

  (setq sym (intern "EMS_FMTC" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_FMTx"))

  (setq sym (intern "NDF_FORM" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_FORM"))

  (setq sym (intern "EMS_RENEW" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_RENEW"))

  (setq sym (intern "NDF_XIARY" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XIARY"))

  (setq sym (intern "NDF_EXIST" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_EXIST"))

  (setq sym (intern "NDF_XGT0R" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XGT0x"))

  (setq sym (intern "NDF_XGT0L" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XGT0x"))

  (setq sym (intern "NDF_XGT0I" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XGT0x"))

  (setq sym (intern "NDF_XGT0D" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XGT0x"))

  (setq sym (intern "NDF_XGT0C" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XGT0x"))

  (setq sym (intern "NDF_SHIFT" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_SHIFT"))

  (setq sym (intern "NDF_XNUMB" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_XNUMB"))

  (setq sym (intern "NDF_TYPE" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_TYPE"))

  (setq sym (intern "NDF_FIND" starfort-mode-helpkey-table))
  (set sym '("sun33" "NDF_FIND"))

  (setq sym (intern "EMS_BEGIN" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_BEGIN"))

  (setq sym (intern "MSG_SYNC" starfort-mode-helpkey-table))
  (set sym '("sun104" "MSG_SYNC"))

  (setq sym (intern "EMS_MLOAD" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_MLOAD"))

  (setq sym (intern "EMS_ELOAD" starfort-mode-helpkey-table))
  (set sym '("ssn4" "EMS_ELOAD"))
)
