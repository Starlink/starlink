      SUBROUTINE TRACB2( BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
     :                   NDISP, DTINDX, SCALE, OFFSET, XLMT,
     :                   YDAT, XDAT, STATUS )
*+
*  Name:
*     TRACB2

*  Purpose:
*     Put the scaled and offset  data into an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACB2( BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
*                  NDISP, DTINDX, SCALE, OFFSET, XLMT,
*                  YDAT, XDAT, STATUS )

*  Description:
*     This routine put the specified rows of input arrays DETDAT and
*     INSCN into the output arrays YDAT and XDAT respectively, The
*     DETDAT data are scaled and offset while put into YDAT.

*  Arguments:
*     BSMP = INTEGER (Given)
*        The begin index of the samples in the input data.
*     ESMP = INTEGER (Given)
*        The end index of the samples in the input data.
*     BDET = INTEGER (Given)
*        The begin index of the detectors in the input data.
*     EDET = INTEGER (Given)
*        The end of index of the detectors in the input data.
*     INSCN( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The input x data.
*     DETDAT( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The input y data.
*     NDISP = INTEGER (Given)
*        The number of detectors to be displayed.
*     DTINDX( NDISP ) = INTEGER (Given)
*        Detector index in input arrays whose data to be put into output
*        arrays.
*     SCALE( NDISP ) = REAL (Given)
*        The scale factor to produce scaled data.
*     OFFSET( NDISP ) = REAL (Given)
*        The offset for scaled y data of each row when put into
*        output array.
*     XLMT( 2 ) = REAL (Given)
*        The in-scan display limits.
*     YDAT( BSMP : ESMP, NDISP ) = REAL (Returned)
*        The output data holding specified rows of the arry DETDAT.
*     XDAT( BSMP : ESMP, NDISP ) = REAL (Returned)
*        The output data holding specified rows of the arry INSCAN.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei GONG (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-JAN-1991 (WG)
*        Original version
*     8-DEC-1993 (DSB):
*        Guard against bad SCALE values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PRM_PAR'         ! VAL_ constants.

*  Arguments Given:
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      REAL DETDAT( BSMP : ESMP, BDET : EDET )
      INTEGER NDISP
      INTEGER DTINDX( NDISP )
      REAL SCALE( NDISP )
      REAL OFFSET( NDISP )
      REAL XLMT( 2 )

*  Arguments Returned
      REAL YDAT( BSMP : ESMP, NDISP )
      REAL XDAT( BSMP : ESMP, NDISP )

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER I                 ! Do loop index
      INTEGER J                 ! Do loop index
      INTEGER NDATA             ! Index of the samples within display
                                ! limits
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Process the data row by row
      DO I = 1, NDISP

*  Initial the data account.
         NDATA = 0

         DO J = BSMP, ESMP

*  Process the sample only if the in-scan distance of the sample
*  drops inside the display limits.
            IF ( INSCN( J, DTINDX( I ) ) .GE. XLMT( 1 ) .AND.
     :           INSCN( J, DTINDX( I ) ) .LE. XLMT( 2 ) ) THEN
               NDATA = NDATA + 1

*  If input sample value is invalid, set output sample as invalid.
               IF ( ( DETDAT( J, DTINDX( I ) ) .EQ. VAL__BADR ) .OR.
     :              ( INSCN( J, DTINDX( I ) ) .EQ. VAL__BADR ) .OR.
     :              SCALE( I ) .EQ. VAL__BADR ) THEN
                  YDAT( NDATA, I ) = VAL__BADR
                  XDAT( NDATA, I ) = VAL__BADR

*  Otherwise, scale and offset the data.
               ELSE
                  YDAT( NDATA, I ) = OFFSET( I ) + SCALE( I ) *
     :                               DETDAT( J, DTINDX( I ) )
                  XDAT( NDATA, I ) = INSCN( J, DTINDX( I ) )
               END IF
            END IF
         END DO

*  Fill the left samples with bad values.
         DO J = NDATA, ESMP
            YDAT( J, I ) = VAL__BADR
            XDAT( J, I ) = VAL__BADR
         END DO

      END DO

      END
