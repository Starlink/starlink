      SUBROUTINE USR_SHOW( STATUS )
*+
*  Name:
*     SUBROUTINE USR_SHOW

*  Purpose:
*     Selectable values in the DATASET are printed out.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_SHOW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Parameters:
*     DATASET:  dataset name
*     V:  list of items to be printed.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     07-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      LOGICAL STR_SIMLR  ! Caseless string equality.

      INTEGER STR_INDEX  ! Character index in string.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMFILE'

*  Local Variables:
      BYTE ITEMS( 16 )   ! List of ITEM character indicators.
      BYTE NEEDS( 16 )
      BYTE VITAL( 16 )

      INTEGER ACTVAL     ! Parameter value count.
      INTEGER D_VM       ! Data VM.
      INTEGER NAXIS1     ! Axis1 size.
      INTEGER NAXIS2     ! Axis2 size.
      INTEGER NPRINT     ! Print level.
      INTEGER Q_VM       ! Data quality VM.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   V parameter.
      CALL STR_MOVE( 'H\\', 16, ITEMS )
      CALL RDPARC( 'V\\', .TRUE., 16, ITEMS, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'V\\', STATUS )
         GO TO 999
      END IF

      CALL STR_UCASE( ITEMS )
      CALL CNPAR( 'V\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PCANER( 'V\\', STATUS )
         GO TO 999
      END IF

*   Select appropriate file needs based on list of items.
      CALL STR_TERM( 0, 16, NEEDS )
      CALL STR_TERM( 0, 16, VITAL )

      IF ( STR_INDEX( ITEMS, 'Q' ) .GT. 0 ) THEN
         CALL STR_APPND( 'I\\', 16, NEEDS )
         CALL STR_APPND( 'T\\', 16, VITAL )
      END IF

      IF ( STR_INDEX( ITEMS, 'S' ) .GT. 0 ) THEN
         CALL STR_APPND( 'S\\', 16, NEEDS )
         CALL STR_APPND( 'F\\', 16, VITAL )
      END IF

      IF ( STR_INDEX( ITEMS, 'M' ) .GT. 0 ) THEN
         CALL STR_APPND( 'M\\', 16, NEEDS )
         CALL STR_APPND( 'F\\', 16, VITAL )
      END IF

      IF ( STR_INDEX( ITEMS, '*' ) .GT. 0 ) THEN
         CALL STR_APPND( 'SM\\', 16, NEEDS )
         CALL STR_APPND( 'FF\\', 16, VITAL )
      END IF

*   Get Needs.
      CALL DASSOC( NEEDS, VITAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*   A sensible dynamic default for PRINT (really fixed, see below).
      IF ( STR_SIMLR( 'H\\', ITEMS ) ) THEN
         NPRINT = 1

      ELSE IF ( STR_INDEX( ITEMS, '*' ) .LE. 0 ) THEN
         NPRINT = 2

      ELSE
         NPRINT = 1
      END IF

*   Check against nothing there.
      IF ( NODSN ) THEN
         CALL LINE_WCONT( '%p There is no current dataset.\\' )
         CALL PRTBUF( STATUS )
         GO TO 999
      END IF

*   Banner Clause.
      CALL PRTEOL( STATUS )
      CALL LINE_WRITS( '%p Information for ''%S'':\\', DSNAME )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

*   Basic identification.
      IF ( STR_INDEX( ITEMS, 'H' ) + STR_INDEX( ITEMS, '*' ) .GT. 0 )
     :     CALL PRPAR( STATUS )

*   Image shape.
      IF ( STR_INDEX( ITEMS, 'I' ) + STR_INDEX( ITEMS, '*' ) .GT. 0 )
     :     CALL PRIMG( STATUS )

*   Pixel data quality.
      IF ( STR_INDEX( ITEMS, 'Q' ) .GT. 0 ) THEN
         CALL MRDATA( NAXIS1, NAXIS2, D_VM, Q_VM, STATUS )
         IF ( STATUS .EQ. SAI__OK) THEN
            CALL PLIST( NAXIS1, NAXIS2, %VAL( D_VM ), %VAL( Q_VM ),
     :                  NPRINT, STATUS )
         END IF
      END IF

*   Fiducials.
      IF ( STR_INDEX( ITEMS, 'F' ) + STR_INDEX( ITEMS, '*' ) .GT. 0 )
     :     CALL PRFID( STATUS )

*   Geometry.
      IF ( STR_INDEX( ITEMS, 'G' ) + STR_INDEX( ITEMS, '*' ) .GT. 0 )
     :     CALL PRGEOM( STATUS )

*   Dispersion.
      IF ( STR_INDEX( ITEMS, 'D' ) + STR_INDEX( ITEMS, '*' ) .GT. 0 )
     :     CALL PRDISP( STATUS )

*   Halation, ripple, and clipping.
      IF ( ( STR_INDEX( ITEMS, 'R' ) +
     :       STR_INDEX( ITEMS, '*' ) ) .GT. 0 ) THEN
         CALL PRHAL( STATUS )
         CALL PRRIP( STATUS )
      END IF

*   Absolute calibration.
      IF ( STR_INDEX( ITEMS, 'A' ) + STR_INDEX( ITEMS, '*' ) .GT. 0 )
     :     CALL PRABS( STATUS )

*   Spectrum.
      IF ( STR_INDEX( ITEMS, 'S' ) + STR_INDEX( ITEMS, '*' ) .GT. 0 )
     :     CALL PRSPEC( STATUS )

*   Mean spectrum.
      IF ( STR_INDEX( ITEMS, 'M' ) + STR_INDEX( ITEMS, '*' ) .GT. 0 )
     :     CALL PRMAP( STATUS )

*   Closing clause.
      CALL LINE_WCONT( '%p end.\\' )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

 999  CONTINUE

      END
