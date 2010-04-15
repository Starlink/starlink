      SUBROUTINE TRACA5( BSMP, ESMP,  BDET, EDET, INSCN, NDISP, DTINDX,
     :                   PARAM, XLMT, STATUS )
*+
*  Name:
*     TRACA5

*  Purpose:
*     Get in-scan limit of the CRDD trace display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACA5( BSMP, ESMP,  BDET, EDET,  INSCN, NDISP, DTINDX,
*                  PARAM, XLMT, STATUS )

*  Description:
*     This routine get the lower and upper in-scan distance limits for
*     a CRDD trace display.  The min. and max. in-scan distances is
*     offered as default values of lower and upper limits,
*     respectively.
*
*  Arguments:
*     BSMP = INTEGER (Given)
*        The begin of the sample index of the input CRDD data array.
*     ESMP = INTEGER (Given)
*        The end of the sample index of the input CRDD data array.
*     BDET = INTEGER (Given)
*        The begin of the detector index of the input CRDD data array.
*     EDET = INTEGER (Given)
*        The end of the detector index of the input CRDD data array.
*     INSCN( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The in-scan distance of each sample of each detector.
*     NDISP = INTEGER (Given)
*        The number of detectors to be displayed.
*     DTINDX( NDISP ) = INTEGER (Given)
*        The indice of the detectors to be displayed.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the user specified
*        in-scan limits from the environment.
*     XLMT( 2 ) = REAL (Returned)
*        The limits of the displayed in-scan distance. XLMT( 2 ) will
*        always be greater than or equal to XLMT( 1 ), no matter the
*        order of the values associated with the parameter PARAM.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      INTEGER NDISP
      INTEGER DTINDX( NDISP )
      CHARACTER PARAM*(*)

*  Arguments Returned:
      REAL XLMT( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      INTEGER J                  ! Do loop index

      REAL DEFVAL( 2 )           ! Default limits
      REAL INSC                  ! In scan distance.
      REAL MXMI( 2 )             ! Min. and max. of in-scan distance of
                                 ! the traces to be displayed
      REAL TEMP                  ! A temporary buffer used in swapping.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the min. and max. in-scan distance of the
*  data traces to be displayed.
      MXMI( 2 ) = VAL__MINR
      MXMI( 1 ) = VAL__MAXR

      DO I = 1, NDISP
         DO J = BSMP, ESMP

           INSC = INSCN( J, DTINDX( I ) )

           IF( MXMI( 2 ) .LT. INSC ) MXMI( 2 ) = INSC
           IF( MXMI( 1 ) .GT. INSC ) MXMI( 1 ) = INSC

         END DO
      END DO

*  Set the default values for the x limit, considering the round-off
*  error to make sure the default values are in the max and min range.
      DEFVAL( 2 ) = 0.999*MXMI( 2 ) + 0.001*MXMI( 1 )
      DEFVAL( 1 ) = 0.001*MXMI( 2 ) + 0.999*MXMI( 1 )

*  Get the user specified limits from the environment.
      CALL PAR_GDR1R( PARAM, 2, DEFVAL, MXMI( 1 ), MXMI( 2 ),
     :               .FALSE., XLMT, STATUS )

*  If the values get from the enirovnment are in wrong order, swap them.
      IF ( XLMT( 2 ) .LT. XLMT( 1 ) ) THEN
         TEMP = XLMT( 2 )
         XLMT( 2 ) = XLMT( 1 )
         XLMT( 1 ) = TEMP
      END IF

      END
