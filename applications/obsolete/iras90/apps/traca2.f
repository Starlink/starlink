      SUBROUTINE TRACA2( IDC, NAVAIL, BDET, EDET, ADET, XSCN, XSCAN,
     :                   DETS, NDETS, DETLIS, IAT, STATUS )
*+
*  Name:
*     TRACA2

*  Purpose:
*     Construct the a list of detectors limited by cross scan distance.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACA2( IDC, NAVAIL, BDET, EDET, ADET, XSCN, XSCAN, DETS,
*                  NDETS, DETLIS, IAT, STATUS )

*  Description:
*     This routine takes detectors with cross-scan distances in the
*     range given by XSCAN as the required detectors. If no detectors
*     satisfy this reqirement, then all detectors are used. The
*     detector numbers are returned in a character string separated by
*     commas.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for CRDD NDF file.
*     NAVAIL = INTEGER (Given)
*        The number of available detectors for selection.
*     BDET = INTEGER (Given)
*        The begin index of the detectors in the CRDD NDF file.
*     EDET = INTEGER (Given)
*        The end index of the detectors in the CRDD NDF file.
*     ADET( NAVAIL ) = INTEGER (Given)
*        The detectors contained in the CRDD file.
*     XSCN( BDET : EDET ) = REAL (Given)
*        Cross-scan distance of each detector scan track.
*     XSCAN( 2 ) = REAL (Given)
*        The cross scan limits of the default detectors.
*     DETS( NAVAIL ) = INTEGER (Returned)
*        The integer detector numbers.
*     NDETS = INTEGER (Returned)
*        The number of detector numbers returned in DETS and DETLIS.
*     DETLIS = CHARACTER * ( * ) (Returned)
*        The list of default displayed detector numbers. If no detectors
*        fall within the given cross scan range, then DETLIS is returned
*        holding the string 'ALL'.
*     IAT = INTEGER (Returned)
*        Index of last non-blank character in DETLIS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-APR-1991 (WG):
*        Original version.
*     18-NOV-1992 (DSB):
*        Re-written to use XSCAN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  External References:
      INTEGER IRC_DETIN          ! Detector index of a given
                                 ! detector number

*  Arguments Given:
      INTEGER IDC
      INTEGER NAVAIL
      INTEGER BDET
      INTEGER EDET
      INTEGER ADET( NAVAIL )
      REAL XSCN( BDET : EDET )
      REAL XSCAN( 2 )

*  Arguments Returned:
      INTEGER DETS( NAVAIL )
      INTEGER NDETS
      CHARACTER DETLIS*( * )
      INTEGER IAT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DETNUM*2         ! Encoded detector number


      INTEGER DETLN              ! Used length of string DETNUM
      INTEGER I                  ! Do loop index
      INTEGER INDX               ! Detector index of a detector

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the index of the last non-blank character in DEFLIS.
      IAT = 0

*  Loop round each detector.
      NDETS = 0
      DO I = 1, NAVAIL

*  Get the detector index of this detector.
         INDX = IRC_DETIN( IDC, ADET( I ), STATUS )

*  If this detector has a cross-scan distance within the range given
*  by XSCAN, add it to the list of default detectors.
         IF (  XSCN( INDX ) .GE. XSCAN( 1 ) .AND.
     :         XSCN( INDX ) .LE. XSCAN( 2 ) ) THEN

*  Store the detector number.
            NDETS = NDETS + 1
            DETS( NDETS ) = ADET( I )

*  Convert the detector number to a text string.
            CALL CHR_ITOC( ADET( I ), DETNUM, DETLN )

*  Append this string to the end of the default detector list.
            CALL CHR_APPND( DETNUM, DETLIS, IAT )

*  Now append a comma.
            CALL CHR_APPND( ',', DETLIS, IAT )

         END IF

      END DO

*  If no detectors have been included in the list, include all detectors
*  in the list.
      IF( NDETS .EQ. 0 ) THEN

         DO I = 1, NAVAIL
            DETS( I ) = ADET( I )
         END DO

         DETLIS = 'ALL'
         IAT = 4
         NDETS = NAVAIL

      END IF

*  Remove the comma from the end of the list.
      DETLIS( IAT : IAT ) = ' '
      IAT = IAT - 1

      END
