      SUBROUTINE TRACB1( OFMTHD, BDET, EDET, NDISP, DTINDX, YLMT,
     :                   AVERAG, SCALE, OFFSET, STATUS )
*+
*  Name:
*     TRACB1

*  Purpose:
*     Calculate an offset for each trace to vertically seperate them.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACB1( OFMTHD, BDET, EDET, NDISP, DTINDX, YLMT, AVERAG,
*                  SCALE, OFFSET, STATUS )

*  Description:
*     This routine use one of the two possible methods to calculate
*     offsets required to vertically seperate the displayed traces.
*     These two offset method are:
*
*     1) 'CONSTANT' :This method returns offsets which are evenly
*     spaced. If the minimum data value in the displayed data is datmin,
*     the offsets ensure that the positions of the datmin level for each
*     trace are evenly spaced between the uper and lower limits of the
*     plotting box. This results in any detector to detector striping
*     being apparent in the traces, which could result in the traces
*     becoming confused under high magnification.
*
*     2) 'AVERAGE' :This method uses an 'average' data value for each
*     trace and produces offsets which ensure that these 'average' data
*     values are equally spaced over the plotting area.  Any detector to
*     detector striping is thus hidden and the amount of overlap of
*     adjacent traces is minimised.

*  Arguments:
*     OFMTHD = INTEGER (Given)
*        Defines the method to offset the traces. It can take
*
*         1 :'CONSTANT' offset
*
*         2 :'AVERAGE' offset
*
*     BDET = INTEGER (Given)
*        The begin index of the detectors in the CRDD data array.
*     EDET = INTEGER (Given)
*        The end index of the detectors in the CRDD data array.
*     NDISP = INTEGER (Given)
*        The number of detector whose data traces will be displayed.
*     DTINDX( NDISP ) = INTEGER (Given)
*        The detector indice whose data traces will be displayed.
*     YLMT( 2 ) = REAL (Given)
*        The lower and upper limits of the display in flux density after
*        scaling.
*     AVERAG( BDET : EDET ) = REAL (Given)
*        The average unscale data values for each detector.
*     SCALE( NDISP ) = REAL (Given)
*        The scale factor to produce the offset values in the required
*        units.
*     OFFSET( NDISP ) = REAL (Returned)
*        The offset for each scaled data trace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-FEB-1991 (WG):
*        Original version (Based on INTERIM version CALOFF by DSB)
*     8-DEC-1993 (DSB):
*        Guard against bad SCALE factors.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants.

*  Arguments Given:
      INTEGER OFMTHD
      INTEGER BDET
      INTEGER EDET
      INTEGER NDISP
      INTEGER DTINDX( NDISP )
      REAL YLMT( 2 )
      REAL AVERAG( BDET : EDET )
      REAL SCALE( NDISP )

*  Arguments Returned:
      REAL OFFSET( NDISP )

*  Status:
      INTEGER STATUS             ! Global status
                                 !
*  Local Variables:
      INTEGER I                  ! Do loop index
      REAL AVEBTM                ! scaled average value of bottom trace

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If method is 'CONSTANT', calculate evenly spaced offsets for each
*  trace.
      IF ( OFMTHD .EQ. 1 ) THEN
         DO I = 1, NDISP
            OFFSET( I ) = REAL( I - 1 ) * ( YLMT( 2 ) -YLMT( 1 ) )
     :                                     / REAL( NDISP )
         END DO

*  If method is 'AVERAGE', calculate offsets which cause the 'average'
*  trace data values to be evenly spaced.
      ELSE IF ( OFMTHD .EQ. 2 ) THEN

*  Get the scaled 'average' value of the bottom trace if it is valid.
         IF ( AVERAG( DTINDX( 1 ) ) .NE. VAL__BADR .AND.
     :        SCALE( 1 ) .NE. VAL__BADR ) THEN
            AVEBTM = SCALE( 1 ) * AVERAG( DTINDX( 1 ) )

*  Otherwise set the bottom average as 0.
         ELSE
            AVEBTM = 0.0
         END IF

*  Enter a do loop to calculate offset for each trace.
         DO I = 1, NDISP

*  If the detector is valid, calculate its offset.
            IF ( AVERAG( DTINDX( I ) ) .NE. VAL__BADR .AND.
     :           SCALE( I ) .NE. VAL__BADR ) THEN
               OFFSET( I ) = AVEBTM - SCALE( I ) * AVERAG( DTINDX( I ) )
     :                       + REAL( I - 1 ) * ( YLMT( 2 ) - AVEBTM )
     :                                        / REAL( NDISP )

*  Otherwise set its offset as 0
            ELSE
               OFFSET( I ) = 0.0
            END IF
         END DO
      END IF

      END
