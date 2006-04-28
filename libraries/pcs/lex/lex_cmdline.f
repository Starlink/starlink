      SUBROUTINE LEX_CMDLINE(INIT,STRING,ACTION,TOKEN,TLEN,STATUS)
*+
*  Name:
*     name

*  Purpose:
*     LEX_CMDLINE

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL LEX_CMDLINE(INIT,STRING;ACTION,TOKEN,TLEN,STATUS)

*  Description:
*     Parse an ADAM command line - the routine is called repeatedly
*     and each call returns one item found in the string - normally
*     one parameter of the command

*  Arguments:
*     INIT = LOGICAL (given)
*           TRUE on first call to parse a given string
*     STRING = CHARACTER*(*) (given)
*           The string to be parsed
*     ACTION = INTEGER (returned)
*           A code describing the syntactic construct found in the string
*           1 = HDS or device name
*           2 = string
*           3 = integer
*           4 = real number
*           5 = string, name or logical constant
*           6 = start of array
*           7 = end of array
*           8 = keyword, string, name or logical constant
*           9 = keyword
*          10 = double precision number
*     TOKEN = CHARACTER*(*) (returned)
*           The string corresponding to the syntactic construct found
*     TLEN = INTEGER (returned)
*           The length of the above string
*     STATUS = INTEGER

*  Algorithm:
*     Basically just a call to LEX_PARSE, using a specific state table
*     set up previously by a call to LEX_CMDSET.

*  Authors:
*     Jeremy Bailey  (AAOEPP::JAB)  1987 Jan 8
*     {enter_new_authors_here}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
*  Arguments Given:
      LOGICAL INIT
      CHARACTER*(*) STRING

*  Arguments Returned:
      INTEGER ACTION
      CHARACTER*(*) TOKEN
      INTEGER TLEN

*  Status:
      INTEGER STATUS

*  Global Variables:
      BYTE TABLE(4,0:127,25)
      COMMON /LEX_COM/ TABLE

*  Local Variables:
      INTEGER POSN

*.


      CALL LEX_PARSE(INIT,STRING,25,TABLE,ACTION,TOKEN,
     :       TLEN,POSN,STATUS)
      
      END
