(new-token "ADAM_CONSTRUCTS"
	   '(("ADAM_CONSTRUCTS" nil place)))

(new-place "ADAM_CONSTRUCTS"
	   '(("_OK_BLOCK" nil token)
	     ("_BAD_BLOCK" nil token)
	     ("_CHECK_STATUS" nil token)
	     ("_ERROR_REPORT" nil token)
	     ("_CLEANUP_CODE" nil token))
	   '((desc . "Menu of ADAM programming constructs")))

(new-token "_CHECK_STATUS"
	   "IF ( STATUS .NE. SAI__OK ) GO TO {abort_stmt}\t"
	   '((desc . "Abort if STATUS is not OK")))

(new-place "ABORT_STMT"
	   "99")

(new-token "_OK_BLOCK"
	   "IF ( STATUS .EQ. SAI__OK ) THEN
{executable_statement}...
END IF\t"
	   '((desc . "IF block which executes if STATUS is OK")))

(new-token "_BAD_BLOCK"
	   "IF ( STATUS .NE. SAI__OK ) THEN
{executable_statement}...
END IF\t"
	   '((desc . "IF block which executes if STATUS is bad")))

(new-token "_ERROR_REPORT"
	   "STATUS = {error_code}
[define_message_token]...
CALL ERR_REP( '{routine_name}_{error_name}',
\\     :\t'{message_text}',
\\     :\tSTATUS )"
        '((desc . "Make an error report")))

(new-place "ERROR_CODE"
	   nil
	   '((help .
"The name of a global symbolic constant identifying an error condition.
Such constants are conventionally named FAC__ENAME, where FAC is a three
character prefix identifying the facility from which the error originates
and ENAME is the name of the error code (up to 5 characters).  Some
existing software does not follow this convention, but all new software
should. The value SAI__ERROR is available for general use.")))

(new-place "ERROR_NAME"
	   nil
	   '((help .
"A name identifying the error being reported in such a way that it is
unique within this routine.  In combination with the routine name, the
error report will then be associated with a globally unique character
string which may be used by the error system to identify it. It is
recommended that upper case alpha-numeric characters be used to construct
a meaningful name which describes the particular type of error being
reported (e.g. FAC_OPEN_NONAME).  A maximum of 15 characters in total
(including the subroutine name and underscores) is permitted.")))

(new-place "DEFINE_MESSAGE_TOKEN"
	   "CALL MSG_{msg_token_routine}"
	   '((vert . t)))

(new-place "MSG_TOKEN_ROUTINE"
	   nil
	   '((help .
"Type the name of a MSG_ routine to be used to define a message token for
the subsequent error message.  If you are unfamiliar with the routines
available, then type SET or FMT followed by cntrl-E to obtain an
appropriate menu from which the full subroutine argument list can be
obtained.")
	     (desc . "Name of a MSG_ routine for setting a message token")))

(new-place "MESSAGE_TEXT"
	   nil
	   '((help .
"Enter the text of the error message. This should be as informative as
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
   'A value of ^NSIGMA is not valid when specifying the clipping level'")
	     (desc . "The text of an error report")))

(new-token "_CLEANUP_CODE"
	   "
\\*\tBegin a new error reporting context.
\\\tCALL ERR_BEGIN( STATUS )

\\*\t[comment]
\\\t{executable_statement}...

\\*\tEnd the error reporting context.
\\\tCALL ERR_END( STATUS )"
	   '((desc . "Cleaning up code; a new error reporting context")))

(new-place "TEMP_STATUS"
	   "TSTAT"
	   '((desc . "Temporary status variable")
	     (auto . t)))
