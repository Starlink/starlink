**==list.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996

************************************************************************

      SUBROUTINE LIST(FILE)

*+
*  Name :
*     LIST
*
*  Purpose
*     List data structure contents
*
*  Invocation :
*     CALL LIST( FILE )
*
*  Description :
*     This is a replacement routine for the original Daophot LIST
*     routine which listed the contents of a Figaro data structure.
*     This present version has been re-written for HDS data structures,
*     but it does nothing, except print a message advising the user to
*     use the ADAM "TRACE" utility instead.  This is because TRACE is
*     better, and also because the HDS version of Daophot only operates
*     on standard "simple" NDF structures and should therefore
*     completely ignore any other data structure components, according
*     to the Starlink standard.  Thus there is little point in listing
*     the (ignored) structure contents.
*
*  Arguments :
*     FILE = CHARACTER*(*) (Given)
*           The data structure file to be listed.
*
*  Algorithm :
*     Simply print a message saying the LIST command is not
*     implemented.
*
*  Deficiencies :
*     Does not replicate the original routine's action.
*
*  Bugs :
*     <description of any "bugs" which have not been fixed>
*
*  Authors :
*     RFWS: R.F. Warren-Smith (Durham University)
*     NE: Nick Eaton (Durham University)
*
*  History :
*     23-MAY-1988 (RFWS):
*        Original version
*      6-DEC-1991 (NE):
*        Print out file name
*     19-FEB-1992 (NE):
*        Unix version.
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'

*  Arguments Given :
      CHARACTER*(*) FILE

*  Local variables :
      INTEGER STATUS            ! HDS error status
*.

*   Initialise the status variable.
      STATUS = SAI__OK

*   Print out the file name
      CALL TBLANK
      CALL MSG_OUT(' ','File = ',FILE,STATUS)

*   Exit routine.
      END
