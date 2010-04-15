      SUBROUTINE IRM_QCOL( COLOUR, STATUS )
*+
*  Name:
*     IRM_QCOL

*  Purpose:
*     Tests whether the current graphics device supports colour.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_QCOL( COLOUR, STATUS )

*  Description:
*     This routine determines whether the current GKS graphics device
*     supports colour.

*  Arguments:
*     COLOUR = LOGICAL (Returned)
*        If true the device supports colour.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  An SGS workstation must be open.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 12 (MJC):
*        Original version.
*     18-JAN-1993 (DSB):
*        Name changed from KPG1_QCOL to IRM_QCOL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Returned:
      LOGICAL COLOUR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CONID              ! Connection identifier
      INTEGER GSTAT              ! Graphics status
      INTEGER NCOLS              ! Number of colours
      INTEGER NPCI               ! Number of predefined colour indices
      INTEGER SWCOL              ! Colour (1) or monochrome (0)
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WSTYPE             ! Workstation type

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Inquire the workstation identifier for GKS inquiries.
      CALL SGS_ICURW( WKID )

*  Get the workstation type.
      CALL GQWKC( WKID, GSTAT, CONID, WSTYPE )

*  Inquire whether GKS/SGS has reported an error.
      CALL GKS_GSTAT( STATUS )

*  Initialise the returned flag.
      COLOUR = .FALSE.

*  Inquire whether colour is supported on the device.
      CALL GQCF( WSTYPE, GSTAT, NCOLS, SWCOL, NPCI )

*  Inquire whether GKS has reported an error.
      CALL GKS_GSTAT( STATUS )

*  Convert from a numerical flag.
      COLOUR = SWCOL .EQ. 1

      END
