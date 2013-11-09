*+ AXCONV - expands spaced axis arrays
      SUBROUTINE AXCONV( STATUS )
*+
*  Name:
*     AXCONV

*  Purpose:
*     Expands spaced axes in a file so that the file may subsequently
*     be used in applications built with the NDF routines (e.g. KAPPA)

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AXCONV( STATUS )

*  Description:
*     The routine expands 'spaced array' axes into 'simple' form.
*     The idea being that applications using the NDF routines aren't
*     currently capable of supporting spaced arrays.

*  ADAM Parameters:
*     INP = NDF (Update)
*        NDF to be modified.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     Richard Saxton   (LTVAD::RDS)

*  History:
*     22 Jun 1992 V1.6-1 (RDS):
*        Original version.
*      3 May 1994 V1.7-0 (DJA):
*        Removed _FILL routine in favour of ASTLIB routine
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     18 Jan 1996
*        Much simpler - map axes for update and unmap and let BDI perform
*        conversion.

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE                 ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'             ! Standard SAE constants
      INCLUDE 'ADI_PAR'             ! Standard SAE constants

*  Status:
      INTEGER STATUS                ! Global status

*  Local Variables:
      INTEGER			APTR			! Axis data pointer
      INTEGER			DIMS(ADI__MXDIM)	! Input dimensions
      INTEGER			FID			! Input file id
      INTEGER			IAX			! Input axis loop
      INTEGER			NDIM         		! Input dimensionality

      LOGICAL			OK			! Axis data ok?

*  Version
      CHARACTER*30 VERSION
          PARAMETER (VERSION='AXCONV Version 2.2-0')
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give version message
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get name of NDF
      CALL USI_ASSOC( 'INP', 'BinDS', 'UPDATE', FID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get dimensionality
      CALL BDI_GETSHP( FID, ADI__MXDIM, DIMS, NDIM, STATUS )

*  Map each axis
      DO IAX =1 ,NDIM
        CALL BDI_AXCHK( FID, IAX, 'Data', OK, STATUS )
        IF ( OK ) THEN
          CALL BDI_AXMAPD( FID, IAX, 'Data', 'UPDATE', APTR, STATUS )
          CALL BDI_AXUNMAP( FID, IAX, 'Data', APTR, STATUS )
        END IF
      END DO

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
