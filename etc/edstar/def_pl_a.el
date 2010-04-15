(new-place "+ OR -"
	   '(("POSIT" nil place)
	     ("MINUS" nil place))
	   '((desc . "Optional sign")))

(new-place ", {INTEGER_EXP}"
	   ", {integer_exp}")

(new-place ".NOT."
	   ".NOT.")

(new-place "/"
	   "/")

(new-place "//{CHAR_ELM}"
	   "//{char_elm}")

(new-token "ACCESS"
	   "ACCESS = {access_options}"
	   '((desc . "ACCESS = {access_options}")))

(new-place "ACCESS_OPTIONS"
	   '(("SEQUENTIAL" "Sequential file access" token)
	     ("DIRECT" "Direct (record-oriented) file access" token)
	     ("APPEND"
	      "Appended sequential access (non-standard, worth avoiding)"
	      token))
	   '((desc . "Access options")))

(new-token "DIRECT"
	   "'DIRECT'"
	   '((desc . "'DIRECT'")))

(new-token "SEQUENTIAL"
	   "'SEQUENTIAL'"
	   '((desc . "'SEQUENTIAL'")))

(new-token "APPEND"
	   "'APPEND'"
	   '((desc . "'APPEND'")))

(new-place "ACCESS_PAR"
	   "ACCESS = {char_var}"
	   '((desc . "ACCESS = {char_var}")))

(new-place "ADD"
	   "+"
	   '((desc . "+")))

(new-place "AELM"
	   '(("int_cons" nil place)
	     ("real_cons" nil place)
	     ("double_cons" nil place)
	     ("arith_var" nil place)
	     ("numeric_array_ref" nil place)
	     ("arith_exp_paren" nil place)
	     ("arith_func_ref" nil place))
	   '((desc . "Arithmetic Element")))

(new-place "AND"
	   ".AND."
	   '((desc . ".AND.")))

(new-place "AOP"
	   '(("add" nil place)
	     ("sub" nil place)
	     ("mult" nil place)
	     ("div" nil place)
	     ("expo" nil place)))

(new-place "ARG"
	   nil
	   '((help .
"Actual arguments must agree in order, number, and data type with
the dummy arguments with which they are associated.  Actual arguments
can be constants, variables, expressions, arrays, array elements,
character substrings, or subprogram names.")
	     (desc . "Subroutine argument")
	     (sep . ", ")
	     (head . "(")
	     (tail . ")")))

(new-place "ARG_"
	   nil
	   '((help .
"Actual arguments must agree in order, number, and data type with
the dummy arguments with which they are associated.  Actual arguments
can be constants, variables, expressions, arrays, array elements,
character substrings, or subprogram names.")
	     (desc . "Function argument")
	     (sep . ", ")))

(new-place "ARITH_ARRAY_NAME"
	   '(("integer_array_name" nil place)
	     ("real_array_name" nil place)
	     ("logical_array_name" nil place)))

(new-place "ARITH_ARRAY_REF"
	   "{arith_array_name}( {subscr}... )"
	   '((desc . "Arithmetic Array reference")))

(new-place "ARITH_ASSIGN_ELM"
	   '(("{arith_var}" "")
	     ("{arith_array_ref}" ""))
	   '((desc . "Arithmetic assignment element")))

(new-place "ARITH_EXP"
	   "{aelm} [{aop} {aelm}]..."
	   '((desc . "Arithmetic Expression")))

(new-place "ARITH_EXP_PAREN"
	   "( {arith_exp} )"
	   '((desc . " Arithmetic Expression in parentheses")))

(new-place "ARITH_FUNC_NAME"
	   '(("integer_func_name" nil place)
	     ("real_func_name" nil place)
	     ("logical_func_name" nil place))
	   '((desc . "Arithmetic function name")))

(new-place "ARITH_FUNC_REF"
	   "{arith_func_name}( [arg_]... )"
	   '((desc . "Arithmetic function reference")))

(new-place "ARITH_VAR"
	   '(("integer_var" nil place)
	     ("logical_var" nil place)
	     ("real_var" nil place))
	   '((desc . "Numeric variable")))

(new-place "ARRAY_DECL"
	   "{array_name}( {dims}... )"
	   '((desc . "Array declarator")
	     (sep . ", ")))

(new-place "ARRAY_NAME"
	   '(("char_array_name" nil place)
	     ("integer_array_name" nil place)
	     ("real_array_name" nil place)
	     ("complex_array_name" nil place)
	     ("logical_array_name" nil place))
	   '((desc . "Array name")))

(new-place "ARRAY_REF"
	   '(("integer_array_ref" nil place)
	     ("real_array_ref" nil place)
	     ("logical_array_ref" nil place)
	     ("complex_array_ref" nil place)
	     ("char_array_ref" nil place))
	   '((desc . "Array reference")))

(new-token "BLANK"
	   "BLANK = {blank_options}"
	   '((desc . "BLANK = {blank_options}")))

(new-place "BLANK_OPTIONS"
	   '(("NULL" "Ignore blanks in numeric fields" token)
	     ("ZERO" "Treat blanks in numeric fields as zeros" token))
	   '((desc . "Blank options")))

(new-place "BLANK_PAR"
	   "BLANK = {char_var}"
	   '((desc . "BLANK = {char_var}")))

(new-place "BN"
	   "BN"
	   '((desc . "BN")))

(new-place "BOUNDS_EXP"
	   "{bound_elm} [{aop} {bound_elm}]..."
	   '((desc . "Dimension declaration bounds expression")))

(new-place "BOUND_ELM"
	   '(("cons" nil place)
	     ("p" nil place)
	     ("common_var" nil place))
	   '((desc . "Bounds element")))

(new-token "BYTE"
    "BYTE"
    '((desc . "BYTE data type")))

(new-place "BZ"
	   "BZ"
	   '((desc . "BZ")))

(new-place "B_EDIT_FIELD"
	   '(("BN" nil place)
	     ("BZ" nil place))
	   '((desc . "BN, BZ")))

(new-place "CALL_STMT"
    "CALL {subprogram_name}( [arg]... )"
    '((desc . "CALL statement")
      (vert . t)))

(new-token "NO_CARRIAGE_CONTROL"
	   "CARRIAGECONTROL = 'LIST'"
	   '((desc . "CARRIAGECONTROL = 'LIST'")))

(new-place "CARRIAGECONTROL_PAR"
	   "CARRIAGECONTROL = {char_var}"
	   '((desc . "CARRIAGECONTROL = {char_var}")))

(new-place "CHAR"
	   nil
	   ''(help .
"Any ascii character"))

(new-place "CHARACTER_FIELD_DESCR"
	   "A[width]"
	   '((desc . "Aw")))

(new-place "CHAR_ARRAY_NAME"
	   nil
	   '((help .
"Character array name")
	     (desc . "Character array name")))

(new-place "CHAR_ARRAY_REF"
	   "{char_array_name}( {subscr}... )"
	   '((desc . "Character Array reference")))

(new-place "CHAR_ARRAY_SUBSTRING"
	   "{char_array_name}( {subscr}... )( [integer_exp]:[integer_exp] )"
	   '((desc .
	      "{char_array_name}( {subscr}... )( [integer_exp]:[integer_exp] )"
	      )))

(new-place "CHAR_ASSIGN_ELM"
	   '(("char_var" nil place)
	     ("char_substring" nil place)
	     ("char_array_ref" nil place))
	   '((desc . "Character assignment element")))

(new-place "CHAR_CONS"
	   "'{char}...'"
	   '((desc . "Character constant")))

(new-place "CHAR_ELM"
	   '(("char_cons" nil place)
	     ("char_var" nil place)
	     ("char_substring" nil place)
	     ("char_exp" nil place)
	     ("char_func_ref" nil place))
	   '((desc . "Character element")))

(new-place "CHAR_EXP"
	   "{char_elm}[//{char_elm}]..."
	   '((desc . "Character Expression")))

(new-place "CHAR_FUNC_NAME"
	   nil
	   '((help .
"Character function name")
	     (desc . "Character function name")))

(new-place "CHAR_FUNC_REF"
	   "{char_func_name}( [arg_]... )"
	   '((desc . "Character function reference")))

(new-token "CHARACTER"
	   "CHARACTER * ( {len} )"
	   '((desc . "CHARACTER data type")))

(new-place "CHAR_SUBSTRING"
	   '(("char_var_substring" nil place)
	     ("char_array_substring" nil place))
	   '((desc . "Character substring")))

(new-place "CHAR_VAR"
	   nil
	   '((help .
"Character variable")
	     (desc . "Character variable")))

(new-place "CHAR_VAR_SUBSTRING"
	   "{char_var}( [integer_exp]:[integer_exp] )"
	   '((desc . "{char_var}( [integer_exp]:[integer_exp] )")))

(new-place "CLOSE_PARAMETER"
	   '(("CLOSE_STATUS"
	      "Specify whether to keep or delete the file (default=keep)"
	      token)
	     ("ERR"
	      "Specify a statement to branch to if an error occurs"
	      token)
	     ("IOSTAT"
	      "Specify a variable to receive any I/O error code"
	      token))
	   '((desc . "CLOSE statement parameter")
	     (sep . ", ")
	     (head . ",")))

(new-place "CLOSE_STMT"
	   "CLOSE ( UNIT = {unit}, [close_parameter]... )"
	   '((desc . "CLOSE statement")
	     (vert . t)))

(new-place "CMPLX_CONS"
	   "( {cmplx_cons_elm}, {cmplx_cons_elm} )"
	   '((desc . "COMPLEX constants")))

(new-place "CMPLX_CONS_ELM"
	   '(("real_cons" nil place)
	     ("int_cons" nil place))
	   '((desc . "Complex*8 constant element")))

(new-place "COLON_FIELD"
	   ":"
	   '((desc . ":")))

(new-place "COMMON_BLK_NAME"
	   nil
	   '((help .
"Common block name")))

(new-place "COMMON_BLOCK_REF"
	   "/{common_blk_name}/")

(new-place "COMMON_ELM"
	   '(("var" nil place)
	     ("array_name" nil place)
	     ("array_decl" nil place))
	   '((desc . "Common list element")
	     (sep . ", ")))

(new-place "COMMON_STMT"
	   "COMMON /{common_blk_name}/ {common_elm}..."
	   '((desc . "COMMON statement")
	     (vert . t)))

(new-place "COMMON_VAR"
	   nil
	   '((help .
"Variable in a common block")
	     (desc . "Common block variable")))

(new-place "COMPLEX_ARRAY_NAME"
	   nil
	   '((help .
"Complex array name")
	     (desc . "Complex array name")))

(new-place "COMPLEX_ARRAY_REF"
	   "{complex_array_name}( {subscr}... )"
	   '((desc . "Complex Array reference")))

(new-token "COMPLEX"
	   "COMPLEX"
	   '((desc . "COMPLEX data type")))

(new-place "COMPLEX_VAR"
	   nil
	   '((help .
"A variable of type COMPLEX")
	     (desc . "Complex variable")))

(new-place "CONS"
	   '(("char_cons" nil place)
	     ("int_cons" nil place)
	     ("real_cons" nil place)
	     ("double_cons" nil place)
	     ("log_cons" nil place)
	     ("cmplx_cons" nil place))
	   '((desc . "constant")
	     (sep . ", ")))

(new-place "CONSTANT_EXP"
	   "{name} = {cons}"
	   '((desc . "Constant expression")
	     (sep . ", ")))

(new-token "CONTINUE"
	   "\\ [lbl] CONTINUE\t"
	   '((desc . "CONTINUE statement")))

(new-place "DATA_ELM"
	   '(("var" nil place)
	     ("array_name" nil place)
	     ("array_ref" nil place)
	     ("char_substring" nil place)
	     ("implied_do_list" nil place))
	   '((desc . "Data element for DATA statement")
	     (sep . ", ")))

(new-place "DATA_STMT"
	   "DATA {data_elm} / {data_values}... /"
	   '((desc . "DATA statement")
	     (vert . t)))

(new-place "DATA_VALUES"
	   "[{unsign_int_cons} *] {cons}"
	   '((sep . ", ")))

(new-place "DATA_TYPE"
	   '(("INTEGER" nil token)
	     ("REAL" nil token)
	     ("CHARACTER" nil token)
	     ("LOGICAL" nil token)
	     ("DOUBLE_PRECISION" nil token)
	     ("WORD" nil token)
	     ("BYTE" nil token)
	     ("COMPLEX" nil token))
	   '((desc . "Data type")))

(new-place "DECLARATION_STATEMENT"
	   "{data_type} {name}[dimensions]"
	   '((desc . "Type Declaration")
	     (vert . t)))

(new-place "DIMENSIONS"
	   "( {dims}... )"
	   '((desc . "Array dimension expression")))

(new-place "DIMS"
	   "[lbound] : {ubound}"
	   '((desc . "Dimension declarator")
	     (sep . ", ")))

(new-place "DIRECT_PAR"
	   "DIRECT = {char_var}"
	   '((desc . "DIRECT = {char_var}")))

(new-place "DIV"
	   "/"
	   '((desc . "/")))

(new-place "DLIST"
	   '(("array_ref" nil place)
	     ("char_substring" nil place)
	     ("implied_do_list" nil place)))

(new-place "DOUBLE_CONS"
	   "[+ or -][n]....{n}...[D[+ or -]{n}...]"
	   '((desc . "DOUBLE PRECISION constant")))

(new-token "DOUBLE_PRECISION"
	   "DOUBLE PRECISION"
	   '((desc . "DOUBLE PRECISION data type")))

(new-place "DO_VAR"
	   nil
	   '((help .
"An integer or real variable")))

(new-place "D[+ OR -]{N}..."
	   "D[+ or -]{n}...")

(new-place "D_CODE"
	   "[repeat]D{width}.{ndec}"
	   '((desc . "Dw.d")))

(new-place "EDIT_FIELD_DESCR"
	   '(("x_edit_field" nil place)
	     ("t_edit_field" nil place)
	     ("s_edit_field" nil place)
	     ("b_edit_field" nil place)
	     ("char_cons" nil place)
	     ("colon_field" nil place))
	   '((desc . "'...',nX,Tn,TLn,TRn,nP,:,BN,BZ,S,SP,SS")))

(new-place "ELSE"
	   "ELSE
{executable_statement}...\t"
	   '((desc . "ELSE {executable_statement}")
	     (vert . t)))

(new-place "ELSE_IF_THEN"
	   "ELSE IF ( {logical_exp} ) THEN
{executable_statement}...\t"
	   '((desc . "ELSE IF ( {logical_exp} ) THEN")
	     (vert . t)))

(new-place "EQUAL"
	   ".EQ."
	   '((desc . ".EQ.")))

(new-place "EQV"
	   ".EQV."
	   '((desc . ".EQV.")))

(new-token "ERR"
	   "ERR = {lbl}"
	   '((desc . "ERR = {lbl}")))

(new-token "END"
	   "END = {lbl}"
	   '((desc . "END = {lbl}")))

(new-place "ERR_PAR"
	   "ERR = {lbl}"
	   '((desc . "ERR = {lbl}")))

(new-place "EXECUTABLE_STATEMENT"
	   '(("DO" "DO loop" token)
	     ("IF" "IF ( {logical_exp} ) {executable_statement}" token)
	     ("IF_THEN" "IF ( {logical_exp} ) THEN ... END IF" token)
	     ("WHILE" "DO WHILE loop (standard F77 version)" token)
	     ("READ" "Internal READ statement" token)
	     ("WRITE" "Internal WRITE statement" token)
	     ("FORMAT" "FORMAT statement" token)
	     ("CALL" "CALL statement" token)
	     ("GO" "GO TO {lbl}" token)
	     ("GO_COMPUTED" "Computed GO TO statement" token)
	     ("DO_ENDDO" "DO ... ENDDO (non-standard, worth avoiding)" token)
	     ("DO_WHILE" "DO WHILE ... ENDDO (non-standard, worth avoiding)"
	      token)
	     ("CONTINUE" "CONTINUE statement" token)
	     ("IO_STMT"
	      "I/O statement (use FIO_ and RIO_ routines if possible)" token)
	     ("assignment_arith" nil token)
	     ("assignment_log" nil token)
	     ("assignment_char" nil token))
	   '((desc "FORTRAN executable statements")
	     (vert . t)))

(new-token "IO_STMT"
	   '(("IO_STMT" nil place)))

(new-place "IO_STMT"
	   '(("OPEN" "OPEN statement" token)
	     ("READ_STMT" "READ statement" token)
	     ("WRITE_STMT" "WRITE statement" token)
	     ("PRINT" "PRINT statement" token)
	     ("CLOSE" "CLOSE statement" token)
	     ("INQUIRE" "INQUIRE statement" token)
	     ("REWIND" "REWIND statement" token))
	   '((desc . "I/O statement")))

(new-token "WHILE"
	   '(lambda ()
	      (let (indent)
		(starfort-indent-line)
		(setq indent (point))
		(insert "CONTINUE             ! Start of 'DO WHILE' loop")
		(starfort-indent-line)
		(starfort-break-line 'code)
		(insert "IF ( {logical_exp} ) THEN")
		(starfort-break-line 'code)
		(insert "{executable_statement}...")
		(starfort-break-line 'code)
		(insert "GO TO {lab}")
		(starfort-break-line 'code)
		(insert "END IF")
		(starfort-indent-line)
		(save-excursion
		  (goto-char indent)
		  (setq indent (current-column))
		  (delete-region (save-excursion
				   (beginning-of-line) (point)) (point))
		  (insert " {lab}")
		  (insert-char ?  (- indent 6))))
	      t)
	   '((desc . "DO WHILE loop implemented in Standard Fortran 77")))

(new-place "EXIST_PAR"
	   "EXIST = {logical_var}"
	   '((desc . "EXIST = {logical_var}")))

(new-place "EXPO"
	   "**"
	   '((desc . "**")))

(new-place "EXTERNAL_STMT"
	   "EXTERNAL {name}"
	   '((desc . "EXTERNAL statement")
	     (vert . t)))

(new-place "E[+ OR -]{N}..."
	   "E[+ or -]{n}..."
	   '((desc . "Decimal exponent")))

(new-place "E_CODE"
	   "[repeat]E{width}.{ndec}[E{int_cons}]"
	   '((desc . "Ew.d[Ee]")))

(new-place "E{INT_CONS}"
	   "E{int_cons}"
	   '((desc . "Number of characters in exponent")))

(new-place "FALSE"
	   ".FALSE."
	   '((desc . "Logical false constant")))

(new-place "FI"
	   nil
	   '((help .
"A character expression, numeric scalar memory reference, or numeric
array name reference whose value specifies the name of the file to be
inquired about")))

(new-place "FIELD_SPEC"
	   '(("integer_field_descr" nil place)
	     ("real_and_complex_field_descr" nil place)
	     ("logical_field_descr" nil place)
	     ("character_field_descr" nil place)
	     ("edit_field_descr" nil place))
	   '((sep . ", ")))

(new-token "FILE"
	   "FILE = {char_exp}"
	   '((desc . "FILE = {char_exp}")))

(new-place "FILE_PAR"
	   "FILE = {fi}"
	   '((desc . "FILE = {fi}")))

(new-place "FILE_SPEC"
	   nil
	   '((help .
"File specification")))

(new-place "FMT"
	   nil
	   '((help .
"A statement label of a FORMAT statement, or the name of an array,
array element, or character expression containing a run-time format.
An asterisk may also be used to specify list-directed formatting.")
	     (desc . "Format Specifier")))

(new-place "IFMT"
	   nil
	   '((help .
"A statement label of a FORMAT statement, or the name of an array,
array element, or character expression containing a run-time format.")
	     (desc . "Format Specifier")))

(new-place "PRINT_FMT"
	   nil
	   '((help .
"A statement label of a FORMAT statement, or the name of an array,
array element, or character expression containing a run-time format.
An asterisk may be given to specify list-directed output.")
	     (desc . "PRINT statement format specifier")))

(new-token "FORM"
	   "FORM = {form_options}"
	   '((desc . "FORM = {form_options}")))

(new-token "FORMATTED"
	   "'FORMATTED'"
	   '((desc . "'FORMATTED'")))

(new-token "READ"
	   "READ ( {ifile}, {ifmt}, [iread_parameter]... ) [io_elm]..."
	   '((desc . "Formatted internal read statement")))

(new-place "FORMATTED_PAR"
	   "FORMATTED = {char_var}"
	   '((desc . "FORMATTED = {char_var}")))

(new-token "FORMAT"
	   '(lambda ()
	      (let (indent)
		(starfort-indent-line)
		(setq indent (current-column))
		(delete-region (save-excursion
				 (beginning-of-line) (point)) (point))
		(insert " {lbl}")
		(insert-char ?  (- indent 6))
		(insert "FORMAT ( [/]... {field_spec}... [/]... )"))
	      t)
	   '((desc . "FORMAT statement")))

(new-place "FORM_OPTIONS"
	   '(("FORMATTED" "A formatted file")
	     ("UNFORMATTED" "An unformatted file"))
	   '((desc . "Format options")))

(new-place "FORM_PAR"
	   "FORM = {char_var}"
	   '((desc . "FORM = {char_var}")))

(new-place "F_CODE"
	   "[repeat]F{width}.{ndec}"
	   '((desc . "Fw.d")))

(new-place "GREATER_THAN"
	   ".GT."
	   '((desc . ".GT.")))

(new-place "GREATER_THAN_OR_EQUAL"
	   ".GE."
	   '((desc . ".GE.")))

(new-place "G_CODE"
	   "[repeat]G{width}.{ndec}[E{int_cons}]"
	   '((desc . "Gw.d[Ee]")))

(new-place "IFILE"
	   nil
	   '((help .
"An internal file specifier")))

(new-place "IMPLICIT_NONE"
	   "IMPLICIT NONE"
	   '((desc . "IMPLICIT NONE")))

(new-place "IMPLICIT_STMT"
	   '(("implicit_none" nil place))
	   '((desc . "IMPLICIT statement")))

(new-place "IMPLIED_DO_LIST"
	   "({dlist}, {integer_var} = {integer_exp}, {integer_exp}[, {integer_exp}])"
	   '((desc . "Implied do-list")))

(new-place "INQUIRE_PARAMETER"
	   '(("file_par" nil token)
	     ("unit_par" nil token)
	     ("access_par" nil token)
	     ("blank_par" nil token)
	     ("carriagecontrol_par" nil token)
	     ("direct_par" nil token)
	     ("err_par" nil token)
	     ("exist_par" nil token)
	     ("form_par" nil token)
	     ("formatted_par" nil token)
	     ("iostat_par" nil token)
	     ("name_par" nil token)
	     ("named_par" nil token)
	     ("nextrec_par" nil token)
	     ("number_par" nil token)
	     ("opened_par" nil token)
	     ("recl_par" nil token)
	     ("sequential_par" nil token)
	     ("unformatted_par" nil token))
	   '((sep . ", ")))

(new-place "INQUIRE_STMT"
	   "INQUIRE ( {inquire_parameter}... )"
	   '((desc . "INQUIRE statement")))

(new-token "WORD"
	   "INTEGER * 2"
	   '((desc . "INTEGER * 2 data type")))

(new-place "INTEGER_ARRAY_NAME"
	   nil
	   '((help .
"Integer array name")
	     (desc . "Integer array name")))

(new-place "INTEGER_ARRAY_REF"
	   "{integer_array_name}( {subscr}... )"
	   '((desc . "Integer Array reference")))

(new-place "INTEGER_EXP"
	   "{int_elm} [{aop} {int_elm}]..."
	   '((desc . "Integer expression")))

(new-place "INTEGER_EXP_PAREN"
	   "( {integer_exp} )"
	   '((desc . "Integer Expression in parentheses")))

(new-place "INTEGER_FIELD_DESCR"
	   '(("i_code" nil place))
	   '((desc . "Iw, Iw.m")))

(new-place "INTEGER_FUNC_NAME"
	   nil
	   '((help .
"Integer function name")
	     (desc . "Integer function name")))

(new-place "INTEGER_FUNC_REF"
	   "{integer_func_name}( [arg_]... )"
	   '((desc . "Integer FUNCTION reference")))

(new-token "INTEGER"
	   "INTEGER"
	   '((desc . "INTEGER data type")))

(new-place "INTEGER_VAR"
	   nil
	   '((help .
"Integer variable")
	     (desc . "Integer variable")))

(new-place "INTRINSIC_STMT"
	   "INTRINSIC {intrinsic_func_name}"
	   '((desc . "INTRINSIC statement")
	     (vert . t )))

(new-place "INTRINSIC_FUNC_NAME"
	   nil
	   '((help .
"The name of a Fortran intrinsic function.")))

(new-place "INT_CONS"
	   "[+ or -]{n}..."
	   '((desc . "Integer constant")))

(new-place "INT_ELM"
	   '(("int_cons" nil place)
	     ("integer_var" nil place)
	     ("integer_array_ref" nil place)
	     ("integer_exp" nil place)
	     ("integer_func_ref" nil place))
	   '((desc . "Integer element")))

(new-place "IOSTAT"
	   "IOSTAT = {integer_var}"
	   '((desc . "IOSTAT = {integer_var}")))

(new-place "IOSTAT_PAR"
	   "IOSTAT = {integer_var}"
	   '((desc . "IOSTAT = {integer_var}")))

(new-place "IO_ELM"
	   nil
	   '((help .
"The I/O list in an input or output statement contains the names of
variables,  arrays, array elements, and character substrings from
which or to which data will be transferred.  The I/O list in an output
statement can also contain constants and expressions to be output.")
	     (desc . "I/O list element")
	     (sep . ", ")))

(new-place "PRINT_IO_ELM"
	   nil
	   '((help .
"The I/O list in a PRINT statement contains the names of variables,
arrays, array elements, character substrings, constants and expressions
from which data will be transferred.")
	     (desc . "PRINT statement I/O list element")
	     (sep . ", ")))

(new-place "I_CODE"
	   "[repeat]I[width][min_char]"
	   '((desc . "I{width}")))

(new-place "LBL"
	   nil
	   '((help .
"The label of an executable statement.")
	     (sep . ", ")))

(new-place "LBOUND"
	   "{bounds_exp}"
	   '((desc . "Upper bound")
	     (tail . ":")))

(new-place "LEN"
	   nil
	   '((help .
"Length in characters.")))

(new-place "LESS_THAN"
	   ".LT."
	   '((desc . ".LT.")))

(new-place "LESS_THAN_OR_EQUAL"
	   ".LE."
	   '((desc . ".LE.")))

(new-place "LOGICAL_ARRAY_NAME"
	   nil
	   '((help .
"Logical array name")
	     (desc . "Logical array name")))

(new-place "LOGICAL_ARRAY_REF"
	   "{logical_array_name}( {subscr}... )"
	   '((desc . "Logical Array reference")))

(new-place "LOGICAL_ASSIGN_ELM"
	   '(("logical_var" nil place)
	     ("logical_array_ref" nil place))
	   '((desc . "Logical assignment element")))

(new-place "LOGICAL_EXP"
	   "[.NOT.]{log_elm}[{lop}{log_elm}]..."
	   '((desc . "Logical Expression")))

(new-place "LOGICAL_EXP_PAREN"
	   "( {logical_exp} )"
	   '((desc . "Logical expression in parentheses")))

(new-place "LOGICAL_FIELD_DESCR"
	   "L{width}"
	   '((desc . "Lw")))

(new-place "LOGICAL_FUNC_NAME"
	   nil
	   '((help .
"Logical function name")
	     (desc . "Logical function name")))

(new-place "LOGICAL_FUNC_REF"
	   "{logical_func_name}( [arg_]... )"
	   '((desc . "Logical FUNCTION reference")))

(new-token "LOGICAL"
	   "LOGICAL"
	   '((desc . "LOGICAL data type")))

(new-place "LOGICAL_VAR"
	   nil
	   '((help .
"Logical variable")
	     (desc . "Logical variable")))

(new-place "LOG_CONS"
	   '(("true" nil place)
	     ("false" nil place))
	   '((desc . "Logical constant")))

(new-place "LOG_ELM"
	   '(("log_cons" nil place)
	     ("logical_var" nil place)
	     ("logical_array_ref" nil place)
	     ("relational_exp" nil place)
	     ("logical_exp_paren" nil place)
	     ("integer_exp_paren" nil place)
	     ("integer_func_ref" nil place)
	     ("logical_func_ref" nil place))
	   '((desc . "Logical element")))

(new-place "LOP"
	   '(("and" nil place)
	     ("not" nil place)
	     ("or" nil place)
	     ("neqv" nil place)
	     ("eqv" nil place))
	   '((desc . "Logical Operators")))

(new-place "MINUS"
	   "-"
	   '((desc . "-")))

(new-place "MIN_CHAR"
	   ".[min]")

(new-place "MORE_PARAMETERS"
	   "{constant_exp}"
	   '((desc . "A list of parameters")
	     (vert . t)
	     (sep . ", ")))

(new-place "MULT"
	   "*"
	   '((desc . "*")))

(new-place "N"
	   nil
	   '((help .
"A decimal digit")
	     (desc . "String of decimal digits")))

(new-place "NAME"
	   nil
	   '((help .
"A symbolic name  is  a string of  letters,  digits,  and  the  special
characters dollar sign ($) and underscore (_).   The  first  character
in a symbolic name must be a letter.   The  symbolic  name can contain
a maximum of 31 characters. FORTRAN-77 limits the length of a symbolic
name to 6 characters")
	     (desc . "Symbolic name")
	     (sep . ", ")))

(new-place "NAMED_PAR"
	   "NAMED = {logical_var}"
	   '((desc . "NAMED = {logical_var}")))

(new-place "NAME_PAR"
	   "NAME = {char_var}"
	   '((desc . "NAME = {char_var}")))

(new-place "NDEC"
	   nil
	   '((help .
"Number of characters to the right of the decimal point.")))

(new-place "NEQV"
	   ".NEQV."
	   '((desc . ".NEQV.")))

(new-token "NEW"
	   "'NEW'"
	   '((desc . "'NEW'")))

(new-place "NEXTREC_PAR"
	   "NEXTREC = {integer_var}"
	   '((desc . "NEXTREC = {integer_var}")))

(new-place "NOT"
	   ".NOT."
	   '((desc . ".NOT.")))

(new-place "NOT_EQUAL"
	   ".NE."
	   '((desc . ".NE.")))

(new-place "NULL"
	   "'NULL'"
	   '((desc . "'NULL'")))

(new-place "NUMBER_PAR"
	   "NUMBER = {integer_var}"
	   '((desc . "NUMBER = {integer_var}")))

(new-place "NUMERIC_ARRAY_REF"
	   "{arith_array_name}( {subscr}... )"
	   '((desc . "Numeric Array reference")))

(new-token "OLD"
	   "'OLD'"
	   '((desc . "'OLD'")))

(new-place "OPENED_PAR"
	   "OPENED = {logical_var}"
	   '((desc . "OPENED = {logical_var}")))

(new-place "OPEN_PARAMETER"
	   '(("ACCESS"
	      "Specify sequential or direct file access (default=sequential)"
	      token)
	     ("BLANK"
	      "Specify the interpretation of blanks in numeric fields"
	      token)
	     ("NO_CARRIAGE_CONTROL"
	      "Turn off Fortran carriage control"
	      token)
	     ("ERR"
	      "Specify a statement to branch to if an error occurs"
	      token)
	     ("FILE"
	      "Specify the name of the file to be opened"
	      token)
	     ("FORM"
	      "Specify whether the file is formatted or unformatted"
	      token)
	     ("IOSTAT"
	      "Specify a variable to receive any I/O error code"
	      token)
	     ("READONLY"
	      "Specify that file is to be read only (to allow sharing)"
	      token)
	     ("RECL"
	      "Specify the length of records for direct access files"
	      token)
	     ("OPEN_STATUS"
	      "Specify whether an old, new or scratch file is to be used"
	      token))
	   '((sep . ", ")))

(new-place "READ_PARAMETER"
	   '(("REC"
	      "Specify the required record for direct access files"
	      token)
	     ("IOSTAT"
	      "Specify a variable to receive any I/O error code"
	      token)
	     ("END"
	      "Specify a statement to branch to on end-of-file condition"
	      token)
	     ("ERR"
	      "Specify a statement to branch to if an error occurs"
	      token))
	   '((sep . ", ")
	     (head . ",")))

(new-place "REWIND_PARAMETER"
	   '(("IOSTAT"
	      "Specify a variable to receive any I/O error code"
	      token)
	     ("ERR"
	      "Specify a statement to branch to if an error occurs"
	      token))
	   '((sep . ", ")
	     (head . ",")))

(new-place "IREAD_PARAMETER"
	   '(("IOSTAT"
	      "Specify a variable to receive any I/O error code"
	      token)
	     ("END"
	      "Specify a statement to branch to on end-of-file condition"
	      token)
	     ("ERR"
	      "Specify a statement to branch to if an error occurs"
	      token))
	   '((sep . ", ")
	     (head . ",")))

(new-place "WRITE_PARAMETER"
	   '(("REC"
	      "Specify the required record for direct access files"
	      token)
	     ("IOSTAT"
	      "Specify a variable to receive any I/O error code"
	      token)
	     ("ERR"
	      "Specify a statement to branch to if an error occurs"
	      token))
	   '((sep . ", ")
	     (head . ",")))

(new-place "IWRITE_PARAMETER"
	   '(("IOSTAT"
	      "Specify a variable to receive any I/O error code"
	      token)
	     ("END"
	      "Specify a statement to branch to on end-of-file condition"
	      token)
	     ("ERR"
	      "Specify a statement to branch to if an error occurs"
	      token))
	   '((sep . ", ")
	     (head . ",")))

(new-token "REC"
	   "REC = {rec}"
	   '((desc . "REC = {rec}")))

(new-place "OPEN_STATUS_OPTIONS"
	   '(("UNKNOWN"
	      "Create a new file if one does not already exist"
	      token)
	     ("OLD"
	      "Open an existing file"
	      token)
	     ("NEW"
	      "Create a new file"
	      tokn)
	     ("SCRATCH"
	      "Use a (new) scratch file"
	      token))
	   '((desc . "STATUS options")))

(new-place "CLOSE_STATUS_OPTIONS"
	   '(("KEEP"
	      "Keep the file"
	      token)
	     ("DELETE"
	      "Delete the file"
	      token))
	   '((desc . "CLOSE statement STATUS options")))

(new-token "KEEP"
	   "'KEEP'"
	   '((desc . "'KEEP'")))

(new-token "DELETE"
	   "'DELETE'"
	   '((desc . "'DELETE'")))

(new-place "OPEN_STMT"
	   "OPEN ( UNIT = {unit}, {open_parameter}... )"
	   '((desc . "OPEN statement")
	     (vert . t)))

(new-place "OR"
	   ".OR."
	   '((desc . ".OR.")))

(new-place "P"
	   nil
	   '((help .
"Dummy argument.")
	     (desc . "Subroutine dummy argument")
	     (sep . ", ")
	     (head . "(")
	     (tail . ")")))

(new-place "P_"
	   nil
	   '((help .
"Dummy argument.")
	     (desc . "Function dummy argument")
	     (sep . ", ")))

(new-place "PARAMETER_LIST"
    "{constant_exp},
\\     :[more_parameters]...\t"
    '((desc . "  Parameter list")))

(new-place "PARAMETER_STMT"
	   "PARAMETER ( {parameter_list} )"
	   '((desc . "PARAMETER statement")
	     (vert . t)))

(new-place "POSIT"
	   "+"
	   '((desc . "+")))

(new-place "SUBPROGRAM_NAME"
	   nil
	   '((help .
"Subprogram name")
	     (desc . "Subprogram name")))

(new-token "READONLY"
	   "READONLY"
	   '((desc . "READONLY")))

(new-token "READ_STMT"
	   '(("READ_STMT" nil place)))

(new-place "READ_STMT"
	   "READ ( {unit}, [fmt], [read_parameter]... ) [io_elm]..."
	   '((desc . "READ statement")
	     (vert . t )))

(new-place "REAL_AND_COMPLEX_FIELD_DESCR"
	   '(("f_code" nil place)
	     ("e_code" nil place)
	     ("d_code" nil place)
	     ("g_code" nil place))
	   '((desc . "Fw.d, Ew.d, Dw.d, Gw.d, Ew.dEe, Gw.dEe")))

(new-place "REAL_ARRAY_NAME"
	   nil
	   '((help .
"Real array name")
	     (desc . "Real array name")))

(new-place "REAL_ARRAY_REF"
	   "{real_array_name}( {subscr}... )"
	   '((desc . "Real Array reference")))

(new-place "REAL_CONS"
	   "[+ or -][n]....{n}...[E[+ or -]{n}...]"
	   '((desc . "REAL constant")))

(new-place "REAL_FUNC_NAME"
	   nil
	   '((help .
"Real function name")
	     (desc . "Real function name")))

(new-token "REAL"
	   "REAL"
	   '((desc . "REAL data type")))

(new-place "REAL_VAR"
	   nil
	   '((help .
"Real variable")
	     (desc . "Real variable")))

(new-place "REC"
	   nil
	   '((help .
"A numeric expression with a value that represents the position in a
direct access file of the record to be accessed.  The value must be
greater than or equal to one, and less than or equal to the maximum
number of record cells allowed in the file.")))

(new-token "RECL"
	   "RECL = {arith_exp}"
	   '((desc . "RECL = {arith_exp}")))

(new-place "RECL_PAR"
	   "RECL = {integer_var}"
	   '((desc . "RECL = {integer_var}")))

(new-place "RELATIONAL_EXP"
	   '(("arith_relational_exp" nil token)
	     ("char_relational_exp" nil token))
	   '((desc . "Relational Expression")))

(new-place "REPEAT"
	   nil
	   '((help .
"Is the repeat count for the field descriptor.  If you omit this
value, the repeat count is assumed to be 1.")
	     (desc . "Repeat count")))

(new-place "REWIND_STMT"
	   "REWIND ( UNIT = {unit}, [rewind_parameter]... )"
	   '((desc . "REWIND statement")
	     (vert . t)))

(new-place "ROP"
	   '(("less_than" nil place)
	     ("less_than_or_equal" nil place)
	     ("equal" nil place)
	     ("not_equal" nil place)
	     ("greater_than" nil place)
	     ("greater_than_or_equal" nil place))
	   '((desc . "Relational Operators")))

(new-place "S"
	   "S"
	   '((desc . "S")))

(new-place "SAVE_ELM"
	   '(("var" nil place)
	     ("array_name" nil place)
	     ("common_block_ref" nil place))
	   '((sep . ", ")))

(new-place "SAVE_STMT"
	   "SAVE {save_elm}..."
	   '((desc . "SAVE statement")
	     (vert . t)))

(new-token "SCRATCH"
	   "'SCRATCH'"
	   '((desc . "'SCRATCH'")))

(new-place "SEQUENTIAL_PAR"
	   "SEQUENTIAL = {char_var}"
	   '((desc . "SEQUENTIAL = {char_var}")))

(new-place "SP"
	   "SP"
	   '((desc . "SP")))

(new-place "SS"
	   "SS"
	   '((desc . "SS")))

(new-token "CLOSE_STATUS"
	   "STATUS = {close_status_options}"
	   '((desc . "STATUS = {close_status_options}")))

(new-token "OPEN_STATUS"
	   "STATUS = {open_status_options}"
	   '((desc . "STATUS = {open_status_options}")))

(new-place "SUB"
	   "-"
	   '((desc . "-")))

(new-place "SUBSCR"
	   "{arith_exp}"
	   '((desc . "Subscript expression")
	     (sep . ", ")))

(new-place "S_EDIT_FIELD"
	   '(("S" nil place)
	     ("SS" nil place)
	     ("SP" nil place))
	   '((desc . "S, SS, SP")))

(new-place "T"
	   "T"
	   '((desc . "T")))

(new-place "TL"
	   "TL"
	   '((desc . "TL")))

(new-place "TR"
	   "TR"
	   '((desc . "TR")))

(new-place "TRUE"
	   ".TRUE."
	   '((desc . "Logical true constant")))

(new-place "T_EDIT_FIELD"
	   '(("T" nil place)
	     ("TL" nil place)
	     ("TR" nil place))
	   '((desc . "T edit descriptor")))

(new-place "UBOUND"
	   "{bounds_exp}"
	   '((desc . "Lower bound")))

(new-token "UNFORMATTED"
	   "'UNFORMATTED'"
	   '((desc . "'UNFORMATTED'")))

(new-place "UNFORMATTED_PAR"
	   "UNFORMATTED = {char_var}"
	   '((desc . "UNFORMATTED = {char_var}")))

(new-place "UNIT"
	   nil
	   '((help .
"An integer expression with a value in the range of 0 through 99
that refers to a specific file or I/O device.  To avoid possible
clashes between the unit numbers used in separate parts of a program
these numbers should not be 'hard-wired', but should be allocated
when required by calling the routine FIO_GUNIT (and subsequently
deallocated by calling FIO_PUNIT).")))

(new-place "UNIT_PAR"
	   "UNIT = {unit_value}"
	   '((desc . "UNIT = {unit_value}")))

(new-place "UNIT_VALUE"
	   nil
	   '((help .
"The number of the logical unit to be inquired about.  The unit
does not have to exist, nor does it need to be connected to a
file.  If the unit is connected to a file, the inquiry encompasses
both the connection and the file.")))

(new-token "UNKNOWN"
	   "'UNKNOWN'"
	   '((desc . "'UNKNOWN'")))

(new-place "UNSIGN_INT_CONS"
	   "{n}..."
	   '((desc . "Unsigned Integer constant")))

(new-place "VAR"
	   '(("integer_var" nil place)
	     ("char_var"nil place)
	     ("logical_var" nil place)
	     ("real_var" nil place)
	     ("complex_var" nil place))
	   '((desc . "Symbolic Name")
	     (sep . ", ")))

(new-place "WIDTH"
	   nil
	   '((help .
"External field width in characters")))

(new-token "WRITE_STMT"
	   "WRITE ( {unit}, [fmt], [write_parameter]... ) [io_elm]..."
	   '((desc . "WRITE statement")))

(new-token "WRITE"
	   "WRITE ( {ifile}, {ifmt}, [iwrite_parameter]... ) [io_elm]..."
	   '((desc . "Internal WRITE statement")))

(new-place "XNUM"
	   nil
	   '((help .
"Number of character positions to be passed over")))

(new-place "X_EDIT_FIELD"
	   "{xnum}X"
	   '((desc . "X")))

(new-token "ZERO"
	   "'ZERO'"
	   '((desc . "'ZERO'")))

(new-place "{UNSIGN_INT_CONS} *"
	   "{unsign_int_cons} *"
	   '((desc . "Constant value for Data element")
	     (sep . ", ")))

(new-place "{AOP} {AELM}"
	   "{aop} {aelm} ")

(new-place "{AOP} {BOUND_ELM}"
	   "{aop} {bound_elm}")

(new-place "{AOP} {INT_ELM}"
	   "{aop} {int_elm}")

(new-place "{LOP}{LOG_ELM}"
	   "{lop}{log_elm}")

(new-token "ACCESS_PAR"
	   '(("ACCESS_PAR" nil place)))

(new-token "ARITH_RELATIONAL_EXP"
	   "{arith_exp} {rop} {arith_exp}"
	   '((desc . "Arithmetic relational expression")))

(new-token "ASSIGNMENT_ARITH"
	   "{arith_assign_elm} = {arith_exp}"
	   '((desc . "Arithmetic ASSIGNMENT statement")))

(new-token "ASSIGNMENT_CHAR"
	   "{char_assign_elm} = {char_exp}"
	   '((desc . "Character ASSIGNMENT Statement")))

(new-token "ASSIGNMENT_LOG"
	   "{logical_assign_elm} = {logical_exp}"
	   '((desc . "Logical ASSIGNMENT statement")))

(new-token "BLANK_PAR"
	   '(("BLANK_PAR" nil place)))

(new-token "BLOCK"
	   '(("BLOCKDATA_PROGRAM_MODULE" nil place)))

(new-token "CALL"
	   '(("CALL_STMT" nil place)))

(new-token "CARRIAGECONTROL_PAR"
	   '(("CARRIAGECONTROL_PAR" nil place)))

(new-token "CHAR_RELATIONAL_EXP"
	   "{char_exp} {rop} {char_exp}"
	   '((desc . "Character relational expression")))

(new-token "CLOSE"
	   '(("CLOSE_STMT" nil place)))

(new-token "COMMON"
	   '(("COMMON_STMT" nil place)))

(new-token "DATA"
	   '(("DATA_STMT" nil place)))

(new-token "DECLARATION_STATEMENT"
	   '(("DECLARATION_STATEMENT" nil place)))

(new-token "DIRECT_PAR"
	   '(("DIRECT_PAR" nil place)))

(new-token "DO_ENDDO"
	   "DO {do_var} = {arith_exp}, {arith_exp}, [do_increment]
{executable_statement}...
END DO\t"
	   '((desc . "Indexed DO loop")))

(new-place "DO_INCREMENT"
	   "{aelm} [{aop} {aelm}]..."
	   '((head . ",")))

(new-token "DO"
	   '(lambda ()
	      (insert
	       "DO {lab} {do_var} = {arith_exp}, {arith_exp}, [do_increment]")
	      (starfort-indent-line)
	      (starfort-break-line 'code)
	      (insert "{executable_statement}...")
	      (starfort-break-line 'code)
	      (insert "CONTINUE")
	      (save-excursion
		    (beginning-of-line)
		    (delete-char (+ 6 starfort-do-indent))
		    (insert " {lab}"))
	      t)
	   '((desc . "Indexed DO loop with label")))

(new-place "LAB"
	   nil
	   '((help .
"The label of a CONTINUE statement which ends the DO loop range. The
statement must physically follow in the same program unit.")
	     (desc . "Do statement label")
	     (auto . t)))

(new-token "DO_WHILE"
	   "DO WHILE ( {logical_exp} )
{executable_statement}...\t
END DO\t"
	   '((desc . "Pre-tested indefinite DO (DO WHILE)")))

(new-token "ERR_PAR"
	   '(("ERR_PAR" nil place)))

(new-token "EXECUTABLE_STATEMENT"
	   "[executable_statement]..."
	   '((desc . "Placeholder for executable statements")))

(new-token "EXIST_PAR"
	   '(("EXIST_PAR" nil place)))

(new-token "EXTERNAL"
	   '(("EXTERNAL_STMT" nil place)))

(new-token "FILE_PAR"
	   '(("FILE_PAR" nil place)))

(new-token "FORMATTED_PAR"
	   '(("FORMATTED_PAR" nil place)))

(new-token "FORM_PAR"
	   '(("FORM_PAR" nil place)))

(new-token "GO_COMPUTED"
	   "GO TO ( {lbl}... ), {arith_exp}"
	   '((desc . "GO TO ( {lbl}... ), {arith_exp}")))

(new-token "GO"
	   "GO TO {lbl}"
	   '((desc . "GO TO {lbl}")))

(new-token "IF"
	   "IF ( {logical_exp} ) {executable_statement}"
	   '((desc . "IF ( {logical_exp} ) {executable_statement}")))

(new-token "IF_THEN"
	   "IF ( {logical_exp} ) THEN
{executable_statement}...
\b[else_if_then]...
[else]
END IF"
        '((desc . "IF ( {logical_exp} ) THEN...")))

(new-token "IMPLICIT"
	   '(("IMPLICIT_STMT" nil place)))

(new-token "INCLUDE"
	   "INCLUDE '{file_spec}'"
	   '((desc . "INCLUDE '{file_spec}'")))

(new-token "INQUIRE"
	   '(("INQUIRE_STMT" nil place)))

(new-token "INTRINSIC"
	   '(("INTRINSIC_STMT" nil place)))

(new-token "IOSTAT_PAR"
	   '(("IOSTAT_PAR" nil place)))

(new-token "NAMED_PAR"
	   '(("NAMED_PAR" nil place)))

(new-token "NAME_PAR"
	   '(("NAME_PAR" nil place)))

(new-token "NEXTREC_PAR"
	   '(("NEXTREC_PAR" nil place)))

(new-token "NUMBER_PAR"
	   '(("NUMBER_PAR" nil place)))

(new-token "OPEN"
	   '(("OPEN_STMT" nil place)))

(new-token "OPENED_PAR"
	   '(("OPENED_PAR" nil place)))

(new-token "PARAMETER"
	   '(("PARAMETER_STMT" nil place)))

(new-token "PRINT"
	   "PRINT {print_fmt}, [print_io_elm]..."
	   '((desc . "PRINT statement")))

(new-token "RECL_PAR"
	   '(("RECL_PAR" nil place)))

(new-token "REWIND"
	   '(("REWIND_STMT" nil place)))

(new-token "SAVE"
	   '(("SAVE_STMT" nil place)))

(new-token "SEQUENTIAL_PAR"
	   '(("SEQUENTIAL_PAR" nil place)))

(new-token "STATUS"
	   '(("STATUS" nil place)))

(new-token "UNFORMATTED_PAR"
	   '(("UNFORMATTED_PAR" nil place)))

(new-token "UNIT_PAR"
	   '(("UNIT_PAR" nil place)))
