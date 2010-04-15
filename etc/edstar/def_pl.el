(new-place "LSE$BLOCK_COMMENT"
	   "
*  [comment]")

(new-place "LSE$LINE_COMMENT"
	   "
! [comment]")

(new-place "LSE$GENERIC"
	   "\t{executable_statement}...")

(new-place "COMMENT"
	   nil
	   '((help .
"A comment describing the code which follows.  Comments should be entered
in lower case (to distinguish them from lines of code, in upper case),
correctly punctuated (including full stops), and with appropriate
capitalisation of letters at the beginning of sentences, etc.  References
to program variables should be in upper case.  Comments should begin in
column four.")
	     (head . "\n*")))

(new-place "STATUS"
	   "STATUS")

(new-place "PROGRAM_MODULE"
	   '(("A_task" "ADAM A_task (main routine of an ADAM application)"
	      token)
	     ("subroutine" "SUBROUTINE program module" token)
	     ("function" "FUNCTION program module" token)
	     ("block_data" "BLOCK DATA program module" token)
	     ("monolith" "Top-level ADAM monolith routine" token))
	   '((vert . t)
	     (desc . "Menu of different types of program module")))

(new-token "PROGRAM_MODULES"
	   '(("PROGRAM_MODULE" nil place)))

(new-token "PROLOGUES"
	   '(("PROLOGUES" nil place)))

(new-place "PROLOGUES"
	   '(("A_Task_prologue" nil token)
	     ("Subroutine_prologue" nil token)
	     ("Function_prologue" nil token)
	     ("Block_Data_prologue" nil token)
	     ("Monolith_prologue" nil token))
	   '((desc .
	      "Menu of prologue templates for different program modules")))

(new-place "SUBROUTINE_PROGRAM_MODULE"
	   "SUBROUTINE {routine_name}( [p]... )\t
\\*+
\\*{subroutine_prologue}
\\*-
\\\t{subroutine_declarations}
\\*.
\\
[status_check]
[executable_statement]...

END"
           '((desc . "SUBROUTINE program module")))

(new-place "A_TASK_PROGRAM_MODULE"
	   "SUBROUTINE {routine_name}( STATUS )\t
\\*+
\\*{a_task_prologue}
\\*-
\\\t{a_task_declarations}
\\*.
\\
\\*\tCheck inherited global status.
\\\tIF ( STATUS .NE. SAI__OK ) RETURN

[executable_statement]...

[contextual_error_report]
END"
            '((desc . "ADAM A_task (main routine of an ADAM application)")))

(new-place "CONTEXTUAL_ERROR_REPORT"
	   "\\*\tIf an error occurred, then report a contextual message.
\\\tIF ( STATUS .NE. SAI__OK ) THEN
CALL ERR_REP( '{routine_name}_ERR',
\\     :\t'{routine_name}: {context_message}.',
\\     :\tSTATUS )
END IF\t
"
             '((desc . "Report contextual information following an error")))

(new-place "CONTEXT_MESSAGE"
	   nil
	   '((help .
"Enter a brief message explaining what the routine was trying to
accomplish when the error occurred. For instance, a suitable contextual
error message from a routine called SQRT which takes the square root of
a spectrum might be:

   'SQRT: Error taking the square root of a spectrum.'

A contextual message will be qualified by error information reported
earlier (normally from lower-level software), so it need not be too
detailed.")
	     (desc . "Text of a contextual error message" )))

(new-token "SUBROUTINE_PROLOGUE"
	   "\\*+
\\*{subroutine_prologue}
\\*-
\\\t{subroutine_declarations}
\\*."
            '((desc . "Subroutine prologue")))

(new-place "SUBROUTINE_PROLOGUE"
	   "\\*\tName:
\f{routine_name}
\\
\\*\tPurpose:
\f{routine_purpose}
\\
\\*\tLanguage:
\f{routine_language}
\\
\\*\tInvocation:
\fCALL {routine_name}( [p]... )
\\
\\*\tDescription:
\f{routine_description}
\\
\\*\t[arguments]
[optional_subroutine_items]...
Authors:
\f{original_author_entry}
\\
\\*\tHistory:
\f{original_version_entry}
\\
\\*\tBugs:
\f{note_any_bugs_here}
\\"
          '((desc . "SUBROUTINE prologue template")))

(new-token "A_TASK_PROLOGUE"
	   "\\*+
\\*{a_task_prologue}
\\*-
\\\t{a_task_declarations}
\\*."
	   '((desc . "ADAM A-task prologue")))

(new-place "A_TASK_PROLOGUE"
	   "\\*\tName:
\f{routine_name}
\\
\\*\tPurpose:
\f{routine_purpose}
\\
\\*\tLanguage:
\f{routine_language}
\\
\\*\tType of Module:
\fADAM A-task
\\
\\*\tInvocation:
\fCALL {routine_name}( STATUS )
\\
\\*\tArguments:
\fSTATUS = INTEGER (Given and Returned)
\fThe global status.
\\
\\*\tDescription:
\f{routine_description}
\\
\\*\t[usage]
[ADAM_parameters]
[examples]
[optional_A_task_items]...
Authors:
\f{original_author_entry}
\\
\\*\tHistory:
\f{original_version_entry}
\\
\\*\tBugs:
\f{note_any_bugs_here}
\\"
         '((desc . "ADAM A_task prologue template" )))

(new-place "USAGE"
	   "Usage:
\f{routine_name} {parameter_usage}
\\"
	   '((desc . "Concise description of an A-task's usage")))

(new-token "USAGE"
	   '(("USAGE" nil place)))

(new-place "PARAMETER_USAGE"
	   nil
	   '((help .
"Enter a list of parameters to show how the routine should be invoked. The
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

(new-token "A_TASK"
	   '(("A_TASK_PROGRAM_MODULE" nil place)))

(new-token "SUBROUTINE"
	   '(("SUBROUTINE_PROGRAM_MODULE" nil place)))

(new-token "FUNCTION"
	   '(("FUNCTION_PROGRAM_MODULE" nil place)))

(new-token "BLOCK_DATA"
	   '(("BLOCKDATA_PROGRAM_MODULE" nil place)))

(new-token "MONOLITH"
	   '(("MONOLITH_PROGRAM_MODULE" nil place)))

(new-place "MONOLITH_PROGRAM_MODULE"
	   "SUBROUTINE {routine_name}( STATUS )\t
\\*+
\\*{monolith_prologue}
\\*-
\\\t{monolith_declarations}
\\*.
\\
\\*\tCheck inherited global status.
\\\tIF ( STATUS .NE. SAI__OK ) RETURN

\\*\tGet the action name.
\\\tCALL TASK_GET_NAME( NAME, STATUS )

\\*\tTest the action name against each valid value in turn, calling the
appropriate routine...
\\
\\*\t[comment]
\\\tIF ( NAME .EQ. '{action_name}' ) THEN
CALL {action_name}( STATUS )

[ADAM_action]...

\\*\tIf the action name is not recognised, then report an error.
\\\tELSE\t
STATUS = SAI__ERROR
CALL MSG_SETC( 'NAME', NAME )
CALL ERR_REP( '{routine_name}_ERR',
\\     :\t'{routine_name}: The action name ''^NAME'' is ' //
\\     :\t'not recognised by the {routine_name} monolith.',
\\     :\tSTATUS )
END IF\t

END"
        '((desc . "ADAM monolith program module")))

(new-place "ADAM_ACTION"
	   "\\*\t[comment]
\\\tELSE IF ( NAME .EQ. '{action_name}' ) THEN\t
CALL {action_name}( STATUS )
"
	   '((desc . "Test for and execute an action in an ADAM monolith")
	     (vert . t)
	     (head . "\n")))

(new-token "ADAM_ACTION"
	   '(("ADAM_ACTION" nil place)))

(new-place "ACTION_NAME"
	   nil
	   '((help .
"The name of one of the actions (i.e. commands) which the monolith
recognises. Remember to use upper case.")
	     (auto . t)))

(new-token "MONOLITH_PROLOGUE"
	   "\\*+
\\*\t{monolith_prologue}
\\*-
\\\t{monolith_declarations}
\\*."
	   '((desc . "ADAM monolith routine prologue")))

(new-place "MONOLITH_PROLOGUE"
	   "\\*\tName:
\f{routine_name}
\\
\\*\tPurpose:
\fTop-level ADAM monolith routine for the {routine_name} package.
\\
\\*\tLanguage:
\fStarlink Fortran 77
\\
\\*\tInvocation:
\fCALL {routine_name}( STATUS )
\\
\\*\tDescription:
\fThis routine obtains the name of the current action and calls the
appropriate routine to perform the specified operation. An error
will be reported and STATUS will be set if the action name is not
recognised.
\\
\\*\tArguments:
\fSTATUS = INTEGER (Given and Returned)
\fThe global status.
\\
\\*\t[optional_monolith_items]...
Authors:
\f{original_author_entry}
\\
\\*\tHistory:
\f{original_version_entry}
\\
\\*\tBugs:
\f{note_any_bugs_here}
\\"
      '((desc . "MONOLITH prologue template")))

(new-place "OPTIONAL_MONOLITH_ITEMS"
	   '(("Monolith_Options"
	      "Expands to a list of placeholders for all items below" token)
	     ("Pitfalls" nil token)
	     ("Notes" nil token)
	     ("Prior_Requirements" nil token)
	     ("Side_Effects" nil token)
	     ("Routines_Used" nil token)
	     ("Deficiencies" nil token)
	     ("DIY_Prologue" nil token)
	     ("References" nil token)
	     ("Keywords" nil token)
	     ("Copyright" nil token))
	   '((vert . t)))

(new-token "MONOLITH_OPTIONS"
	   "[notes]
[prior_requirements]
[side_effects]
[routines_used]
[deficiencies]
[DIY_prologue_item]...
[references]
[keywords]
[copyright]"
           '((desc . "Expanded list of all optional items")))

(new-place "MONOLITH_DECLARATIONS"
	   "
\\*\tType Definitions:
\\\tIMPLICIT NONE              ! No implicit typing\t
\\
\\*\tGlobal Constants:
\\\tINCLUDE 'SAE_PAR'          ! Standard SAE constants\t
\\\tINCLUDE 'PAR_PAR'          ! PAR_ public constants\t
\\
\\*\tStatus:
\\\tINTEGER STATUS             ! Global status\t
\\
\\*\tLocal Variables:
\\\tCHARACTER * ( PAR__SZNAM ) NAME ! Action name\t
\\"
	   '((desc . "MONOLITH declarations")))

(new-place "ADAM_PARAMETERS"
	   "ADAM Parameters:
\f[parameter_spec]...
\\"
	   '((head . "*")
	     (desc . "Description of the routine's ADAM parameters")))

(new-token "ADAM_PARAMETER"
	   '(("ADAM_PARAMETERS" nil place)))

(new-place "PARAMETER_SPEC"
	   "{parameter_name}[pdims] = {parameter_type} ({parameter_access_mode})
\f{parameter_description}
[parameter_default]"
           '((vert . t)))

(new-place "PARAMETER_DEFAULT"
	   nil
	   '((desc . "Normal default value for parameter")
	     (help .
"If the parameter normally has a default value and is not prompted for,
then indicate the default value here. The value should be placed between
square brackets to separate it from the preceding text, e.g. [3.8].
If the value cannot be given literally (because it is calculated at
run-time, for instance), then use a brief description such as [Number
of image pixels]. Alternatively, use empty brackets [], which indicates
that the way in which the default is calculated is discussed in the
associated description of the parameter's purpose.")))

(new-place "PDIMS"
	   "( [pdim]... )"
	   '((desc . "Array dimension expression")
	     (head . "")
	     (tail . "")))

(new-place "PDIM"
	   nil
	   '((desc . "Array dimension size")
	     (help .
"The size of a parameter dimension. If the dimension does not have a fixed
size, then omit this value altogether. Note that in this latter case, the
number of commas remaining in the dimension expression should indicate the
number of parameter dimensions.")
	     (sep . ", ")))

(new-place "PARAMETER_NAME"
	   nil
	   '((help .
"The name of an ADAM parameter which the routine accesses.")))

(new-place "PARAMETER_TYPE"
	   nil
	   '((help .
"The HDS or ADAM data type of the parameter.  Normally this should be
specified in the same form in which it appears in the A-task interface
(.IFL) file, e.g:

   _INTEGER, _REAL, _CHAR, LITERAL, NDF, etc.

When the ADAM type \"UNIV\" is used to allow a variety of HDS data types
to be accepted, then a more descriptive type specification should
generally be given here.")))

(new-place "PARAMETER_ACCESS_MODE"
	   '(("Read" "")
	     ("Write" "")
	     ("Read and Write" "")))

(new-place "PARAMETER_DESCRIPTION"
	   nil
	   '((help .
"Give a description of the function that the parameter performs. Also
specify its default value in square brackets [...] if appropriate.")))

(new-place "SUBROUTINE_DECLARATIONS"
	   "
\\*\tType Definitions:
\\\tIMPLICIT NONE              ! No implicit typing\t
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
[local_data]"
      '((desc . "SUBROUTINE declarations template")))

(new-place "GLOBAL_CONSTANTS"
	   "\\*\tGlobal Constants:
\\\t[standard_SAE_constants]
[include_global_constants]...
"
      '((desc . "Define global constants")))

(new-token "GLOBAL_CONSTANTS"
	   '(("GLOBAL_CONSTANTS" nil place)))

(new-place "STANDARD_SAE_CONSTANTS"
	   "INCLUDE 'SAE_PAR'          ! Standard SAE constants\t")

(new-place "INCLUDE_GLOBAL_CONSTANTS"
	   "INCLUDE '{global_constants_file}'   ! [global_constants_description]\t"
	   '((vert . t)))

(new-place "GLOBAL_CONSTANTS_FILE"
	   nil
	   '((help .
"Enter the logical name of the global constants file you wish to include.")))

(new-place "GLOBAL_CONSTANTS_DESCRIPTION"
	   nil
	   '((help .
"Give a brief description of the contents of the global constants file.")
	     (head . "!")))

(new-place "GLOBAL_VARIABLES"
	   "\\*\tGlobal Variables:
\\\t{include_global_variables}...
"
	   '((desc . "Define global variables")))

(new-token "GLOBAL_VARIABLES"
	   '(("GLOBAL_VARIABLES" nil place)))

(new-place "INCLUDE_GLOBAL_VARIABLES"
	   "INCLUDE '{global_variables_file}' ! [global_variables_description]
\\*\t\f\f{descriptions_of_global_variables_referenced}...
\\"
	   '((vert . t)
	     (head . "\n")))

(new-place "GLOBAL_VARIABLES_FILE"
	   nil
	   '((help .
"Enter the logical name of the global variables file you wish to include.")))

(new-place "GLOBAL_VARIABLES_DESCRIPTION"
	   nil
	   '((help .
"Give a brief description of the contents of the global variables file.")
	     (head . "!")))

(new-place "DESCRIPTIONS_OF_GLOBAL_VARIABLES_REFERENCED"
	   "{global_name}[dimensions] = {data_type} ({global_access_mode})
\f[global_variable_purpose]"
	   '((vert . t)))

(new-place "GLOBAL_NAME"
	   nil
	   '((help .
"The name of a global variable referenced in this routine.")))

(new-place "GLOBAL_ACCESS_MODE"
	   '(("Read" "")
	     ("Write" "")
	     ("Read and Write" "")))

(new-place "GLOBAL_VARIABLE_PURPOSE"
	   nil
	   '((help .
"Give a description of the function that the global variable performs.")))

(new-place "ARGUMENTS_GIVEN"
	   "\\*\tArguments Given:
\\\t{declaration_statement}...
"
	   '((desc . "Define input arguments")))

(new-token "ARGUMENTS_GIVEN"
	   '(("ARGUMENTS_GIVEN" nil place)))

(new-place "ARGUMENTS_GIVEN_AND_RETURNED"
	   "\\*\tArguments Given and Returned:
\\\t{declaration_statement}...
"
	   '((desc . "Define input/output arguments")))

(new-token "ARGUMENTS_GIVEN_AND_RETURNED"
	   '(("ARGUMENTS_GIVEN_AND_RETURNED" nil place)))

(new-place "ARGUMENTS_RETURNED"
	   "\\*\tArguments Returned:
\\\t{declaration_statement}...
"
	   '((desc . "Define output arguments")))

(new-token "ARGUMENTS_RETURNED"
	   '(("ARGUMENTS_RETURNED" nil place)))

(new-place "STATUS_ARGUMENT"
	   "\\*\tStatus:
\\\tINTEGER STATUS             ! Global status\t
\\"
	   '((desc . "Define the STATUS argument")))

(new-token "STATUS_ARGUMENT"
	   '(("STATUS_ARGUMENT" nil place)))

(new-place "EXTERNAL_REFERENCES"
	   "\\*\tExternal References:
\\\t{external_function_specification}...
"
	   '((desc . "Define external function references")))

(new-token "EXTERNAL_REFERENCES"
	   '(("EXTERNAL_REFERENCES" nil place)))

(new-place "EXTERNAL_FUNCTION_SPECIFICATION"
	   "[external_declaration]
{data_type} {external_name}   ! [external_description]\t"
	   '((vert . t)))

(new-place "EXTERNAL_DECLARATION"
	   "EXTERNAL {external_name}")

(new-place "EXTERNAL_NAME"
	   nil
	   '((help .
"The name of an external function which is referenced.")
	     (auto . t)))

(new-place "EXTERNAL_DESCRIPTION"
	   nil
	   '((help .
"Give a brief description of what the external function does.")
	     (head . "!")))

(new-place "LOCAL_CONSTANTS"
	   "\\*\tLocal Constants:
\\\t{local_constant_specification}...
"
	   '((desc . "Define local constants")))

(new-token "LOCAL_CONSTANTS"
	   '(("LOCAL_CONSTANTS" nil place)))

(new-place "LOCAL_CONSTANT_SPECIFICATION"
	   "{data_type} {constant_name}   ! [constant_description]\t
\\\tPARAMETER ( {constant_name} = {cons} )"
	   '((vert . t)))

(new-place "CONSTANT_NAME"
	   nil
	   '((help .
"The name of the local constant to be defined.")
	     (auto . t )))

(new-place "CONSTANT_DESCRIPTION"
	   nil
	   '((help .
"Give a brief description of what the constant represents.")
	     (head . "!")))

(new-place "LOCAL_VARIABLES"
	   "\\*\tLocal Variables:
\\\t{local_variable_declaration}...
"
	   '((desc . "Define local variables")))

(new-token "LOCAL_VARIABLES"
	   '(("LOCAL_VARIABLES" nil place)))

(new-place "LOCAL_VARIABLE_DECLARATION"
	   "{data_type} {name}[dimensions]   ! [local_variable_description]\t"
	   '((vert . t)))

(new-place "LOCAL_VARIABLE_DESCRIPTION"
	   nil
	   '((help .
"Give a brief description of what the variable is used for.")
	     (head . "!")))

(new-place "INTERNAL_REFERENCES"
	   "\\*\tInternal References:
\\\t{internal_declarations}

{internal_definitions}
"
	   '((desc . "Define internal functions")))

(new-token "INTERNAL_REFERENCES"
	   '(("INTERNAL_REFERENCES" nil place)))

(new-place "INTERNAL_DECLARATIONS"
	   "[internal_declaration_statement]...
[include_internal_declarations]...")

(new-place "INCLUDE_INTERNAL_DECLARATIONS"
	   "INCLUDE '{internal_declaration_file}'   ! [internals_description]\t"
	   '((vert . t)))

(new-place "INTERNAL_DECLARATION_FILE"
	   nil
	   '((help .
"Enter the logical name of the file you wish to include which contains
type declarations for internal functions to be defined later.")))

(new-place "INTERNAL_DEFINITIONS"
	   "[internal_definition_statement]...
[include_internal_definitions]...")

(new-place "INTERNAL_DEFINITION_STATEMENT"
	   nil
	   '((help .
"A statement such as:

   FUNC( ARG ) = ( ARG - 6 ) ** 2

which defines an internal statement function.")))

(new-place "INCLUDE_INTERNAL_DEFINITIONS"
	   "INCLUDE '{internal_definition_file}'"
	   '((vert . t)))

(new-place "INTERNAL_DEFINITION_FILE"
	   nil
	   '((help .
"Enter the logical name of the file you wish to include which contains
definitions of internal functions.")))

(new-place "INTERNAL_DECLARATION_STATEMENT"
	   "{data_type} {internal_name}   ! [internal_description]\t"
	   '((vert . t)))

(new-place "INTERNAL_NAME"
	   nil
	   '((help .
"The name (up to 6 characters) of an internal function which is to be
defined later." )))

(new-place "INTERNAL_DESCRIPTION"
	   nil
	   '((help .
"Give a brief description of what the internal function does.")
	     (head . "!")))

(new-place "INTERNALS_DESCRIPTION"
	   nil
	   '((help .
"Give a brief description of what the internal functions do.")
	     (head . "!")))

(new-place "LOCAL_DATA"
	   "\\*\tLocal Data:
\\\t{data_stmt}...
"
	   '((desc . "Define local data")))

(new-token "LOCAL_DATA"
	   '(("LOCAL_DATA" nil place)))

(new-place "GLOBAL_DATA"
	   "\\*\tGlobal Data:
\\\t{data_stmt}...
"
	   '((desc . "Define global data")))

(new-token "GLOBAL_DATA"
	   '(("GLOBAL_DATA" nil place)))

(new-place "A_TASK_DECLARATIONS"
	   "
\\*\tType Definitions:
\\\tIMPLICIT NONE              ! No implicit typing\t
\\
\\*\tGlobal Constants:
\\\tINCLUDE 'SAE_PAR'          ! Standard SAE constants\t
\\\t[include_global_constants]...

[global_variables]
\\*\tStatus:
\\\tINTEGER STATUS             ! Global status\t
\\
[external_references]
[local_constants]
[local_variables]
[internal_references]
[local_data]"
        '((desc . "ADAM A_task declarations template")))

(new-place "FUNCTION_PROGRAM_MODULE"
	   "{function_type} FUNCTION {routine_name}( [p_]... )
\\*+
\\*{function_prologue}
\\*-
\\\t{function_declarations}
\\*.
\\
[executable_statement]...

END"
        '((desc . "FUNCTION program module")))

(new-place "FUNCTION_TYPE"
	   '(("INTEGER" nil token)
	     ("REAL" nil token)
	     ("CHARACTER_N" nil token)
	     ("CHARACTER_*" nil token)
	     ("LOGICAL" nil token)
	     ("DOUBLE_PRECISION" nil token)
	     ("WORD" nil token)
	     ("BYTE" nil token)
	     ("COMPLEX" nil token))
	   '((desc . "Function data type")))

(new-token "CHARACTER_N"
	   "CHARACTER * {len}"
	   '((desc . "CHARACTER * N data type")))

(new-token "CHARACTER_*"
	   "CHARACTER * ( * )"
	   '((desc . "CHARACTER * ( * ) data type")))

(new-token "FUNCTION_PROLOGUE"
	   "\\*+
\\*{function_prologue}
\\*-
\\\t{function_declarations}
\\*."
	   '((desc . "Function prologue")))

(new-place "FUNCTION_PROLOGUE"
	   "\\*\tName:
\f{routine_name}
\\
\\*\tPurpose:
\f{routine_purpose}
\\
\\*\tLanguage:
\f{routine_language}
\\
\\*\tInvocation:
   RESULT = {routine_name}( [p_]... )
\\
\\*\tDescription:
\f{routine_description}
\\
\\*\t[arguments]
Returned Value:
\f{routine_name} = {data_type}
\f{returned_value_description}
\\
\\*\t[optional_function_items]...
Authors:
\f{original_author_entry}
\\
\\*\tHistory:
\f{original_version_entry}
\\
\\*\tBugs:
\f{note_any_bugs_here}
\\"
        '((desc . "FUNCTION prologue template")))

(new-place "OPTIONAL_FUNCTION_ITEMS"
	   '(("Function_Options"
	      "Expands to a list of placeholders for all items below" token)
	     ("Examples" nil token)
	     ("Pitfalls" nil token)
	     ("Notes" nil token)
	     ("Prior_Requirements" nil token)
	     ("Side_Effects" nil token)
	     ("Algorithm" nil token)
	     ("Accuracy" nil token)
	     ("Timing" nil token)
	     ("Routines_Used" nil token)
	     ("Deficiencies" nil token)
	     ("Machine_Specifics" nil token)
	     ("DIY_Prologue" nil token)
	     ("References" nil token)
	     ("Keywords" nil token)
	     ("Copyright" nil token))
	   '((vert . t)))

(new-place "RETURNED_VALUE_DESCRIPTION"
	   nil
	   '((help .
"A description of the value returned via the function name.")))

(new-place "FUNCTION_DECLARATIONS"
	   "
\\*\tType Definitions:
\\\tIMPLICIT NONE              ! No implicit typing\t
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
[local_data]"
        '((desc . "FUNCTION declarations template")))

(new-place "BLOCKDATA_PROGRAM_MODULE"
	   "BLOCK DATA {routine_name}
\\*+
\\*{block_data_prologue}
\\*-
\\\t{block_data_declarations}
\\*.
\\
END"
	   '((desc . "BLOCK DATA program module")))

(new-token "BLOCK_DATA_PROLOGUE"
	   "\\*+
\\*{block_data_prologue}
\\*-
\\\t{block_data_declarations}
\\*."
	   '((desc . "Block Data prologue")))

(new-place "BLOCK_DATA_PROLOGUE"
	   "\\*\tName:
\f{routine_name}
\\
\\*\tPurpose:
\f{routine_purpose}
\\
\\*\tLanguage:
\f{routine_language}
\\
\\*\tType of Module:
\fBLOCK DATA
\\
\\*\tDescription:
\f{routine_description}
\\
\\*\t[optional_block_data_items]...
Authors:
\f{original_author_entry}
\\
\\*\tHistory:
\f{original_version_entry}
\\
\\*\tBugs:
\f{note_any_bugs_here}
\\"
        '((desc . "BLOCK DATA prologue template" )))

(new-place "OPTIONAL_BLOCK_DATA_ITEMS"
	   '(("Block_Data_Options"
	      "Expands to a list of placeholders for all items below" token)
	     ("Notes" nil token)
	     ("Side_Effects" nil token)
	     ("Deficiencies" nil token)
	     ("Machine_Specifics" nil token)
	     ("DIY_Prologue" nil token)
	     ("References" nil token)
	     ("Keywords" nil token)
	     ("Copyright" nil token))
	   '((vert . t)))

(new-place "BLOCK_DATA_DECLARATIONS"
	   "
\\*\tType Definitions:
\\\tIMPLICIT NONE              ! No implicit typing\t
\\
[global_constants]
[global_variables]
[local_constants]
[local_variables]
[global_data]"
        '((desc . "BLOCK DATA declarations template")))

(new-place "DATE"
	   '(lambda ()
	      (let (date (time (upcase (current-time-string))))
		   (setq date (concat (substring time 8 10)
				      "-"
				      (substring time 4 7)
				      "-"
				      (substring time 20 24)))
		   (if (= (aref date 0) ? ) (setq date (substring date 1)))
		   date)))

(new-place "ROUTINE_NAME"
	   '(lambda ()
	      (let (name i dot)
		(if (setq name (buffer-file-name))
		    (progn
		      (setq name (upcase (file-name-nondirectory
					  (file-name-sans-versions name))))
		      (setq i (length name))
		      (while (and (> i 0) (not dot))
			(setq dot (= ?. (elt name (setq i (- i 1))))))
		      (if dot (setq name (substring name 0 i)))
		      name)
		  nil)))
	   '((help .
"The full name of the routine.  Valid characters are letters, digits,
and the underscore character (_).  The first character must be a letter.
Normally, a routine name should consist of up to six characters, but to
reduce the chances of name clashes, the Starlink convention is to add a
three character \"facility prefix\" and to limit the length of the routine
name itself to five characters.  An underscore is used to separate the two
components, giving names such as SGS_IZONE or DAT_ANNUL.  A further
convention is that routines which are only to be called internally
within a facility are identified by appending a \"1\" to the facility
prefix, giving names such as FAC1_START.")
	     (auto . t)))

(new-place "ROUTINE_PURPOSE"
	   nil
	   '((help .
"A very brief (about one line) description of the service the routine
provides, e.g:

    \"Subtract a scalar from each pixel of an array.\"
or: \"Interpolate across a group of lines or columns in a 2-d array.\"

This description may be extracted to appear as part of the routine
heading in user documentation.")))

(new-place "ROUTINE_LANGUAGE"
	   '(("language_description" nil place)
	     ("Starlink_Fortran_77" nil place)))

(new-place "LANGUAGE_DESCRIPTION"
	   nil
	   '((help .
"Describe the type of Fortran in which the routine is written, e.g:

    Fortran 77, Fortran 66, VAX Fortran, etc.

The use of \"Starlink Fortran 77\" following the standards set out in
the Starlink Programming Standards document (SGP/16) is strongl
encouraged.  A separate menu item is provided for this language
description.")))

(new-token "STARLINK_FORTRAN_77"
	   '(("STARLINK_FORTRAN_77" nil place)))

(new-place "STARLINK_FORTRAN_77"
	   "Starlink Fortran 77")

(new-place "ROUTINE_DESCRIPTION"
	   nil
	   '((help .
"Give a full description (in English) of what the routine does and how
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

(new-place "ARGUMENTS"
	   "Arguments:
\f[argument_spec]...
[status_argument_spec]
\\"
        '((desc . "Description of the routine's argument list")
          (head . "*")))

(new-token "ARGUMENTS"
	   '(("ARGUMENTS" nil place)))

(new-place "ARGUMENT_SPEC"
	   "{argument_name}[dimensions] = {argument_data_type} ({argument_access_mode})
\f{argument_description}"
	   '((vert . t)))

(new-place "ARGUMENT_DATA_TYPE"
	   '(("INTEGER" nil token)
	     ("REAL" nil token)
	     ("CHARACTER" nil token)
	     ("LOGICAL" nil token)
	     ("DOUBLE_PRECISION" nil token)
	     ("WORD" nil token)
	     ("BYTE" nil token)
	     ("COMPLEX" nil token)
	     ("PASSED_SUBROUTINE" nil token)
	     ("PASSED_FUNCTION" nil token))
	   '((desc . "Subroutine/function data type description")))

(new-token "PASSED_SUBROUTINE"
	   "SUBROUTINE"
	   '((desc . "Subroutine passed as an argument")))

(new-token "PASSED_FUNCTION"
	   "FUNCTION"
	   '((desc . "Function passed as an argument")))

(new-place "ARGUMENT_NAME"
	   nil
	   '((help .
"A Fortran dummy argument name of up to 6 characters")))

(new-place "ARGUMENT_ACCESS_MODE"
	   '(("Given" "")
	     ("Returned" "")
	     ("Given and Returned" "")))

(new-place "ARGUMENT_DESCRIPTION"
	   nil
	   '((help .
"Give a description of the function that the argument performs.")))

(new-place "STATUS_ARGUMENT_SPEC"
	   "STATUS = INTEGER ({status_access_mode})
   The global status.")

(new-place "STATUS_ACCESS_MODE"
	   '(("Given and Returned" "")
	     ("Returned" "")))

(new-place "OPTIONAL_SUBROUTINE_ITEMS"
	   '(("Subroutine_Options"
	      "Expands to a list of placeholders for all items below" token)
	     ("Examples" nil token)
	     ("Pitfalls" nil token)
	     ("Notes" nil token)
	     ("Prior_Requirements" nil token)
	     ("Side_Effects" nil token)
	     ("Algorithm" nil token)
	     ("Accuracy" nil token)
	     ("Timing" nil token)
	     ("Routines_Used" nil token)
	     ("Deficiencies" nil token)
	     ("Machine_Specifics" nil token)
	     ("DIY_Prologue" nil token)
	     ("References" nil token)
	     ("Keywords" nil token)
	     ("Copyright" nil token))
	   '((vert . t)))

(new-place "OPTIONAL_A_TASK_ITEMS"
	   '(("A_Task_Options"
	      "Expands to a list of placeholders for all items below" token)
	     ("Pitfalls" nil token)
	     ("Notes" nil token)
	     ("Prior_Requirements" nil token)
	     ("Side_Effects" nil token)
	     ("Algorithm" nil token)
	     ("Accuracy" nil token)
	     ("Timing" nil token)
	     ("Implementation_Status" nil token)
	     ("Routines_Used" nil token)
	     ("Deficiencies" nil token)
	     ("Machine_Specifics" nil token)
	     ("DIY_Prologue" nil token)
	     ("References" nil token)
	     ("Keywords" nil token)
	     ("Copyright" nil token))
	   '((vert . t)))

(new-token "SUBROUTINE_OPTIONS"
	   "[examples]
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
[copyright]"
        '((desc . "Expanded list of all optional items")))

(new-token "A_TASK_OPTIONS"
	   "[pitfalls]
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
[copyright]"
        '((desc . "Expanded list of all optional items")))

(new-token "FUNCTION_OPTIONS"
	   "[examples]
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
[copyright]"
        '((desc . "Expanded list of all optional items")))

(new-token "BLOCK_DATA_OPTIONS"
	   "[notes]
[side_effects]
[deficiencies]
[machine_specifics]
[DIY_prologue_item]...
[references]
[keywords]
[copyright]"
        '((desc . "Expanded list of all optional items")))

(new-place "EXAMPLES"
	   "Examples:
\f{routine_example}...
\\"
	   '((desc . "Examples of how the routine can be used")))

(new-token "EXAMPLES"
	   '(("EXAMPLES" nil place)))

(new-place "ROUTINE_EXAMPLE"
	   "{routine_example_text}
\f{routine_example_description}"
	   '((vert . t)))

(new-place "ROUTINE_EXAMPLE_TEXT"
	   nil
	   '((help .
"Enter the text of an example which illustrates how the routine may be
used. It is often helpful to give several such examples, starting with
a very simple case and working gradually up to a rather complex case.
This is an opportunity to illustrate any special \"tricks\" which may make
a routine easy to use but are not obvious from the descriptions given
elsewhere.")))

(new-place "ROUTINE_EXAMPLE_DESCRIPTION"
	   nil
	   '((help .
"Describe what the example does. Be sure to explain any \"tricks\" which
may not be obvious to a beginner.")))

(new-place "NOTES"
	   "Notes:
\f{routine_notes}...
\\"
	   '((desc . "Notes which qualify earlier information")))

(new-token "NOTES"
	   '(("NOTES" nil place)))

(new-place "ROUTINE_NOTES"
	   "-  {noted_item}"
	   '((vert . t)))

(new-place "NOTED_ITEM"
	   nil
	   '((help .
"Enter the text of a note which may be used to qualify or \"fine-tune\"
the descriptive information given earlier.  For instance, a note could
be used to describe how a special case will be handled by the routine, or
to describe particular conditions which the input data must satisfy.  Such
information may be essential for a precise description of how the routine
behaves, but might not be of initial interest to the general user who will
mainly be guided by the \"Description\" topic.

The content of the note may be extracted for use in user documentation,
so it should not contain items which are only of interest to some-one
reading the code (e.g. a maintenance programmer).")))

(new-place "ALGORITHM"
	   "Algorithm:
\f{algorithm_description}...
\\"
	   '((desc . "A technical description of the algorithm used")))

(new-token "ALGORITHM"
	   '(("ALGORITHM" nil place)))

(new-place "ALGORITHM_DESCRIPTION"
	   "-  {algorithmic_step}"
	   '((vert . t)))

(new-place "ALGORITHMIC_STEP"
	   '((help .
"Describe each of the important steps in the routine's algorithm.
This information should augment that given in the \"Description\"
section, but will normally be of interest only to a programmer and
will not be extracted to appear in user documentation.  No entry need
be made here if the algorithm is trivial.")))

(new-place "PITFALLS"
	   "Pitfalls:
\f{pitfall_description}...
\\"
	   '((desc . "A description of any pitfalls for the unwary")))

(new-token "PITFALLS"
	   '(("PITFALLS" nil place)))

(new-place "PITFALL_DESCRIPTION"
	   "-  {pitfall}"
	   '((vert . t)))

(new-place "PITFALL"
	   nil
	   '((help .
"Describe any particular problem which the inexperienced or unwary
user may encounter which may not be obvious from the information
given elsewhere.")))

(new-place "PRIOR_REQUIREMENTS"
	   "Prior Requirements:
\f{routine_prior_requirements}...
\\"
	   '((desc . "Prior requirements which must be satisfied")))

(new-token "PRIOR_REQUIREMENTS"
	   '(("PRIOR_REQUIREMENTS" nil place)))

(new-place "ROUTINE_PRIOR_REQUIREMENTS"
	   "-  {prior_condition}"
	   '((vert . t)))

(new-place "PRIOR_CONDITION"
	   nil
	   '((help .
"Describe any condition which must be satisfied before the routine is
invoked, but which may not be obvious from the information given
earlier.")))

(new-place "SIDE_EFFECTS"
	   "Side Effects:
\f{routine_side_effects}...
\\"
	   '((desc . "Details of any side effects")))

(new-token "SIDE_EFFECTS"
	   '(("SIDE_EFFECTS" nil place)))

(new-place "ROUTINE_SIDE_EFFECTS"
	   "-  {side_effect}"
	   '((vert . t)))

(new-place "SIDE_EFFECT"
	   nil
	   '((help .
"Describe any side effect which the routine might have but which would
not be obvious from the information given elsewhere.")))

(new-place "DEFICIENCIES"
	   "Implementation Deficiencies:
\f{routine_deficiencies}...
\\"
	   '((desc . "Details of any implementation deficiencies")))

(new-token "DEFICIENCIES"
	   '(("DEFICIENCIES" nil place)))

(new-place "ROUTINE_DEFICIENCIES"
	   "-  {deficiency}"
	   '((vert . t)))

(new-place "DEFICIENCY"
	   nil
	   '((help .
"Describe any deficiency in the current implementation of the routine.
Examples are; limited efficiency, limited accuracy or circumstances in
which the routine does not function adequately.  In this latter case you
should be careful to describe what will actually happen if the routine
cannot cope.  It may be helpful to future developers if you indicate
how improvements might be made.")))

(new-place "ACCURACY"
	   "Accuracy:
\f{routine_accuracy}
\\"
	   '((desc . "Details of the accuracy achieved")))

(new-token "ACCURACY"
	   '(("ACCURACY" nil place)))

(new-place "ROUTINE_ACCURACY"
	   nil
	   '((help .
"If important, give details of the numerical accuracy which the routine
achieves.")))

(new-place "TIMING"
	   "Timing:
\f{routine_timing}
\\"
	   '((desc . "Details of the routine's execution time")))

(new-token "TIMING"
	   '(("TIMING" nil place)))

(new-place "ROUTINE_TIMING"
	   nil
	   '((help .
"Give details of the processor time required by the routine (and/or the
elapsed time if relevant).  Since the absolute time will depend on the
type of machine in use, you should normally only give relative
information, describing how the timing depends on the mode of use, e.g:

   \"Processor time is approximately proportional to the square root
   of the number of elements in the DATA array.\"

This information may be omitted if the time requirement is not expected
to be significant in normal use.")))

(new-place "IMPLEMENTATION_STATUS"
	   "Implementation Status:
\f{routine_implementation_status}
\\"
	   '((desc . "Extent of standard features supported by the routine")))

(new-token "IMPLEMENTATION_STATUS"
	   '(("IMPLEMENTATION_STATUS" nil place)))

(new-place "ROUTINE_IMPLEMENTATION_STATUS"
	   nil
	   '((help .
"Give details of the extent to which the current implementation of the
routine supports standard features of the environment and/or data system
in use. For instance, if the routine processes Starlink NDF data
structures, then you might say which components it can handle, whether it
supports \"bad\" pixels, what type of arithmetic is used, etc. It is best
to mention features which ARE supported, rather than listing those which
aren't, since the latter list would need to be updated whenever the
facilities offered by the environment are extended.")))

(new-place "ROUTINES_USED"
	   "External Routines Used:
\f{facility_or_package}...
\\"
	   '((desc . "Details of any external routines used")))

(new-token "ROUTINES_USED"
	   '(("ROUTINES_USED" nil place)))

(new-place "FACILITY_OR_PACKAGE"
	   "{name_of_facility_or_package}:
\f{routine_used}..."
	   '((vert . t)))

(new-place "NAME_OF_FACILITY_OR_PACKAGE"
	   nil
	   '((help .
"Enter a name identifying the facility or package from which the external
routines are drawn, e.g. HDS, SGS, etc.")))

(new-place "ROUTINE_USED"
	   nil
	   '((help .
"Enter the name of any external routine which is invoked by the routine you
are documenting.")
	     (sep . ", ")))

(new-place "MACHINE_SPECIFICS"
	   "{machine}-Specific Features Used:
\f{routine_machine_specifics}...
\\"
	   '((desc . "Any machine-specific features used")))

(new-token "MACHINE_SPECIFICS"
	   '(("MACHINE_SPECIFICS" nil place)))

(new-place "MACHINE"
    "Machine")

(new-place "ROUTINE_MACHINE_SPECIFICS"
	   "-  {machine_specific_feature}"
	   '((vert . t)))

(new-place "MACHINE_SPECIFIC_FEATURE"
	   nil
	   '((help .
"Enter details of any machine-specific feature which may affect the
routine's portability, such as dependence on a particular type of
hardware or the use of operating-system routines.  You should also
include any extensions to the language description given under the
\"Language\" heading.  Thus, if you specified \"Starlink Fortran 77\" as
the language, then you should mention any language features whose use is
not sanctioned in the Starlink Programming Standards document (SGP/16).
Machine specific features should only be used where there is no
alternative." )))

(new-place "DIY_PROLOGUE_ITEM"
	   "{DIY_prologue_heading}:
\f{DIY_prologue_text}
\\"
	   '((desc . "Design your own additional prologue item")
	     (vert . t)))

(new-token "DIY_PROLOGUE"
	   '(("DIY_PROLOGUE_ITEM" nil place)))

(new-place "DIY_PROLOGUE_HEADING"
	   nil
	   '((help .
"Enter a heading for your personal prologue item.  This may be used to
cater for special circumstances which are not covered by any of the
standard prologue item headings.  Be careful not to duplicate one of
the standard headings.")))

(new-place "DIY_PROLOGUE_TEXT"
	   nil
	   '((help .
"Enter the text of your personal prologue item.")))

(new-place "REFERENCES"
	   "References:
\f{routine_references}...
\\"
	   '((desc . "Bibliographic references")))

(new-token "REFERENCES"
	   '(("REFERENCES" nil place)))

(new-place "ROUTINE_REFERENCES"
	   "-  {reference}"
	   '((vert . t)))

(new-place "REFERENCE"
	   nil
	   '((help .
"Enter a single bibliographic reference.")))

(new-place "KEYWORDS"
	   "Keywords:
\f{routine_keywords}...
\\"
	   '((desc . "Any keywords for bibliographic classification")))

(new-token "KEYWORDS"
	   '(("KEYWORDS" nil place)))

(new-place "ROUTINE_KEYWORDS"
	   nil
	   '((help .
"Enter a keyword to be used for bibliographic classification purposes.")
	     (sep . ", ")))

(new-place "COPYRIGHT"
     "Copyright:
\f{routine_copyright}
\\"
     '((desc . "Copyright message")))

(new-token "COPYRIGHT"
	   '(("COPYRIGHT" nil place)))

(new-place "ROUTINE_COPYRIGHT"
	   "Copyright (C) {year} Central Laboratory of the Research Councils")

(new-place "YEAR"
	   '(lambda () (substring (current-time-string) 20 24)))

(new-place "NOTE_ANY_BUGS_HERE"
	   "-  {description_of_bug}
{note_new_bugs_here}")

(new-place "NOTE_NEW_BUGS_HERE"
	   "-  {description_of_bug}
{note_new_bugs_here}")

(new-place "DESCRIPTION_OF_BUG"
	   nil
	   '((help .
"A description of any bug which is known about but which has not yet been
fixed.  Known bugs should always be corrected as soon as possible, so
there should almost never be an entry under this heading.  In rare
circumstances, however, it may be impractical to correct a bug
immediately, perhaps because it has proved too difficult to trace or
because it arises from inadequate software or documentation supplied by
by another author.  If the routine can still function satisfactorily in
most circumstances, then the bug should be noted here and fixed at the
first opportunity.")))

(new-place "ORIGINAL_AUTHOR_ENTRY"
	   "{author_identifier}: {authors_name} ({affiliation})
{enter_new_authors_here}")

(new-place "AUTHOR_IDENTIFIER"
	   '(lambda ()
	      (or (getenv "EDSTAR_PERSONAL_USERID")
		  (upcase (user-login-name)))))

(new-place "AUTHORS_NAME"
	   '(lambda ()
	      (or (getenv "EDSTAR_PERSONAL_NAME") (user-full-name))))

(new-place "AFFILIATION"
	   '(lambda ()
	      (or (getenv "EDSTAR_PERSONAL_AFFILIATION") "STARLINK")))

(new-place "ENTER_NEW_AUTHORS_HERE"
	   "{author_identifier}: {authors_name} ({affiliation})
{enter_new_authors_here}")

(new-place "ORIGINAL_VERSION_ENTRY"
	   "{date} ({author_identifier}):
\fOriginal version.
\b{enter_changes_here}")

(new-place "ENTER_CHANGES_HERE"
	   "{date} ({author_identifier}):
\f{changes}
\b{enter_further_changes_here}")

(new-place "ENTER_FURTHER_CHANGES_HERE"
	   "{date} ({author_identifier}):
\f{changes}
\b{enter_further_changes_here}")

(new-place "CHANGES"
	   nil
	   '((help .
"Describe any changes made to the routine.")))

(new-place "STATUS_CHECK"
	   "\\*\tCheck inherited global status.
\\\tIF ( STATUS .NE. SAI__OK ) RETURN
")

(new-token "MENU"
	   '(("MENU" nil place)))

(new-place "MENU"
	   '(("Program_Modules" nil token)
	     ("Prologues" nil token)
	     ("Prologue_Sections" nil token)
	     ("Executable_Statement" nil token)
	     ("ADAM_Constructs" nil token))
	   '((desc . "Main menu of program modules, prologues, statements, etc.")))

(new-token "PROLOGUE_SECTIONS"
	   '(("PROLOGUE_SECTIONS" nil place)))

(new-place "PROLOGUE_SECTIONS"
	   '(("A_Task_Sections" nil token)
	     ("Subroutine_Sections" nil token)
	     ("Function_Sections" nil token)
	     ("Block_Data_Sections" nil token)
	     ("Monolith_Sections" nil token))
	   '((desc . "Menu for selecting individual prologue sections")))

(new-token "A_TASK_SECTIONS"
	   '(("A_TASK_SECTIONS" nil place)))

(new-place "A_TASK_SECTIONS"
	   '(("Usage" nil token)
	     ("ADAM_Parameters" nil token)
	     ("Examples" nil token)
	     ("Pitfalls" nil token)
	     ("Notes" nil token)
	     ("Prior_Requirements" nil token)
	     ("Side_Effects" nil token)
	     ("Algorithm" nil token)
	     ("Accuracy" nil token)
	     ("Timing" nil token)
	     ("Implementation_Status" nil token)
	     ("Routines_Used" nil token)
	     ("Deficiencies" nil token)
	     ("Machine_Specifics" nil token)
	     ("DIY_Prologue" nil token)
	     ("References" nil token)
	     ("Keywords" nil token)
	     ("Copyright" nil token)
	     ("Global_Constants" nil token)
	     ("Global_Variables" nil token)
	     ("External_References" nil token)
	     ("Local_Constants" nil token)
	     ("Local_Variables" nil token)
	     ("Internal_References" nil token)
	     ("Local_Data" nil token))
	   '((desc . "Menu of prologue sections for an ADAM A-task")))

(new-token "SUBROUTINE_SECTIONS"
	   '(("SUBROUTINE_SECTIONS" nil place)))

(new-place "SUBROUTINE_SECTIONS"
	   '(("Arguments" nil token)
	     ("Examples" nil token)
	     ("Pitfalls" nil token)
	     ("Notes" nil token)
	     ("Prior_Requirements" nil token)
	     ("Side_Effects" nil token)
	     ("Algorithm" nil token)
	     ("Accuracy" nil token)
	     ("Timing" nil token)
	     ("Routines_Used" nil token)
	     ("Deficiencies" nil token)
	     ("Machine_Specifics" nil token)
	     ("DIY_Prologue" nil token)
	     ("References" nil token)
	     ("Keywords" nil token)
	     ("Copyright" nil token)
	     ("Global_Constants" nil token)
	     ("Global_Variables" nil token)
	     ("Arguments_Given" nil token)
	     ("Arguments_Given_and_Returned" nil token)
	     ("Arguments_Returned" nil token)
	     ("Status_Argument" nil token)
	     ("External_References" nil token)
	     ("Local_Constants" nil token)
	     ("Local_Variables" nil token)
	     ("Internal_References" nil token)
	     ("Local_Data" nil token))
	   '((desc . "Menu of prologue sections for a subroutine")))

(new-token "FUNCTION_SECTIONS"
	   '(("FUNCTION_SECTIONS" nil place)))

(new-place "FUNCTION_SECTIONS"
	   '(("Arguments" nil token)
	     ("Examples" nil token)
	     ("Pitfalls" nil token)
	     ("Notes" nil token)
	     ("Prior_Requirements" nil token)
	     ("Side_Effects" nil token)
	     ("Algorithm" nil token)
	     ("Accuracy" nil token)
	     ("Timing" nil token)
	     ("Routines_Used" nil token)
	     ("Deficiencies" nil token)
	     ("Machine_Specifics" nil token)
	     ("DIY_Prologue" nil token)
	     ("References" nil token)
	     ("Keywords" nil token)
	     ("Copyright" nil token)
	     ("Global_Constants" nil token)
	     ("Global_Variables" nil token)
	     ("Arguments_Given" nil token)
	     ("Arguments_Given_and_Returned" nil token)
	     ("Arguments_Returned" nil token)
	     ("Status_Argument" nil token)
	     ("External_References" nil token)
	     ("Local_Constants" nil token)
	     ("Local_Variables" nil token)
	     ("Internal_References" nil token)
	     ("Local_Data" nil token))
	   '((desc . "Menu of prologue sections for a function")))

(new-token "BLOCK_DATA_SECTIONS"
	   '(("BLOCK_DATA_SECTIONS" nil place)))

(new-place "BLOCK_DATA_SECTIONS"
	   '(("Notes" nil token)
	     ("Side_Effects" nil token)
	     ("Deficiencies" nil token)
	     ("Machine_Specifics" nil token)
	     ("DIY_Prologue" nil token)
	     ("References" nil token)
	     ("Keywords" nil token)
	     ("Copyright" nil token)
	     ("Global_Constants" nil token)
	     ("Global_Variables" nil token)
	     ("Local_Constants" nil token)
	     ("Local_Variables" nil token)
	     ("Global_Data" nil token))
	   '((desc . "Menu of prologue sections for a block data routine")))

(new-token "MONOLITH_SECTIONS"
	   '(("MONOLITH_SECTIONS" nil place)))

(new-place "MONOLITH_SECTIONS"
	   '(("Pitfalls" nil token)
	     ("Notes" nil token)
	     ("Prior_Requirements" nil token)
	     ("Side_Effects" nil token)
	     ("Routines_Used" nil token)
	     ("Deficiencies" nil token)
	     ("DIY_Prologue" nil token)
	     ("References" nil token)
	     ("Keywords" nil token)
	     ("Copyright" nil token))
	   '((desc . "Menu of prologue sections for an ADAM monolith routine")))
