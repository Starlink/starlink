      SUBROUTINE SPD_CZWK( FILE, FU, TNPAR, PARATY, STATUS )
*+
*  Name:
*     SPD_CZWK

*  Purpose:
*     List result parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZWK( FILE, FU, TNPAR, PARATY, STATUS )

*  Description:
*     This routine lists the contents of the result parameter indexed
*     result extension vectors in the Specdre Extension.

*  Arguments:
*     FILE = LOGICAL (Given)
*        True if an ASCII file is to be used, has been opened
*        successfully and is identified by the Fortran unit number FU.
*     FU = INTEGER (Given)
*        The Fortran unit number of the output ASCII file. This is
*        unused if FILE is false.
*     TNPAR = INTEGER (Given)
*        The length of the vector to be listed.
*     PARATY( TNPAR ) = CHARACTER * ( * ) (Given)
*        The character vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     21 Feb 1992 (hme):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL FILE
      INTEGER FU
      INTEGER TNPAR
      CHARACTER * ( * ) PARATY( TNPAR )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      CHARACTER * ( 78 ) SHORT   ! Short string to write to screen

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 201  FORMAT ( ' ', I4, ' ', A32 )
 202  FORMAT ( A )

*  If list goes to ASCII file.
      IF ( FILE ) THEN

*     Table header.
         WRITE ( FU, 202 ) '    # parameter type'

*     Table rows.
         WRITE ( FU, 201 ) ( I, PARATY(I), I = 1, TNPAR )

*  Else (list goes to screen etc.)
      ELSE

*     Table header.
         SHORT = '    # parameter type'
         CALL MSG_OUT( 'SPD_CZWK_LIST', SHORT, STATUS )

*     Table rows.
         DO 1 I = 1, TNPAR
            WRITE ( SHORT, 201 ) I, PARATY(I)
            CALL MSG_OUT( 'SPD_CZWK_LIST', SHORT, STATUS )
 1       CONTINUE
      END IF

*  Return.
      END
