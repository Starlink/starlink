************************************************************************

      SUBROUTINE AGI_1CINIT ( STATUS )

*+
*  Name:
*     AGI_1CINIT

*  Purpose:
*     Initialise the AGI common blocks.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AGI_1CINIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Initialise the AGI common blocks which contain the references to
*     the picture identifiers. The definitions are held in the INCLUDE
*     file 'AGI_PFREE'.

*  Algorithm:
*     Check status on entry.
*     Set the variables to zero of blank strings.

*  Authors:
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     Aug 1988
*     Dec 1989  Added CIDIID
*     Jul 1990  Added CROOT
*     Oct 1990  Added picture deferral
*     Nov 1990  Added nest level and clear flags
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
      INCLUDE 'AGI_PAR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_pfree'


*  Local Variables:
      INTEGER I

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Clear out all the common block entries, but not the free list.
         DO I = 1, FRELEN
            CGRAWK( I ) = ' '
            CAGIWK( I ) = ' '
            PTNAME( I ) = ' '
            CPICNM( I ) = 0
            CIDIID( I ) = 0
            CLEVEL( I ) = 0
            CDEPS( I ) = 0
         ENDDO

*   Initialise the clear flags
         DO I = 1, CLRLEN
            CLEARF( I ) = 0
         ENDDO

*   Initialise other flags
         CURPID = 0
         CROOT = 0
         CNUMID = 0
         CISDEP = .FALSE.

      ENDIF

      END

