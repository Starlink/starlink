************************************************************************

      SUBROUTINE AGD_1IINIT ( STATUS )

*+
*  Name:
*     AGD_1IINIT

*  Purpose:
*     Initialise the IDI parameter common blocks.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AGD_1IINIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Initialise the IDI parameter common blocks which contain the
*     values of the zoom and scroll factors for the memories.
*     The definitions are held in the INCLUDE file 'AGI_IDIPS'.

*  Algorithm:
*     Set all thje values to zero. For the zoom and scroll factors
*     these are valid vaules which indicate the default display;
*     i.e. no zoom and the display origin in the bottom left hand
*     corner.

*  Authors:
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     June 1990
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_idips'


*  Local Variables:
      INTEGER I

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Clear out all the common block entries.
         CDIPID = 0
         CNMEMS = 0
         DO I = 0, MXMEMS - 1
            CZOOMF( I ) = 0
            CXSCRL( I ) = 0
            CYSCRL( I ) = 0
         ENDDO

      ENDIF

      END

