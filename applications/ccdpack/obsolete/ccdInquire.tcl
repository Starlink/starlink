proc ccdInquire { prompt } {

#+
#  Name:
#     ccdInquire

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Inquires a value from the user.

#  Description:
#     This routine prompts the user for a value. The string appears
#     in the form "$prompt > ". The reply is parsed and if the return is 
#     the abort sign "!!" then the routine exits the whole application.
#     If the return is a "?" then the ccdhelp application is started and
#     a reprompt is made when control is returned.

#  Arguments:
#     prompt = string (read)
#        The prompt string.

#  Return:
#     ccdInquire = string (read)
#        The return from the user.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-SEP-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#  Global variables.
    global CCDdir

#.

#  Loop until a reply is given.
    set again 1
    while { $again } { 

#  Make the prompt.
       puts -nonewline stdout "$prompt > "
       flush stdout

#  Read the value.
       set answer [gets stdin]

#  Test for !! return. 
       if { $answer == "!!" } { 
	  puts "!! Parameter abort requested"
	  exit 1
       }

#  Test for "?"
       if { $answer == "?" } { 
          ccdRunTermTask ccdhelp 
       } else { 
	  
#  No need for another attempt.
	  set again 0
       }
    }
    
#  Finally return the answer.
    return $answer
    
#  End of procedure.
}
# $Id$
