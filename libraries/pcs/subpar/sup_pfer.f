      SUBROUTINE SUBPAR_PFER( STATUS )
*+
*  Name:
*     SUBPAR_PFER

*  Purpose:
*     To report a system dependent message on being unable to open
*     the task parameter file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PFER( STATUS )

*  Description:
*     This outputs a message relevant to UNIX ADAM

*  Arguments:
*     STATUS = INTEGER (Given)
*        The global status.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-FEB-1992 (AJC):
*        Original version.
*     {enter_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*.

      CALL EMS_REP( 'SUP_PFER1',
     : 'Have you created directory $HOME/adam or $ADAM_USER ?',
     : STATUS )

      END
