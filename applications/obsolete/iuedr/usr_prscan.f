      SUBROUTINE USR_PRSCAN( STATUS )
*+
*   Name:
*      SUBROUTINE USR_PRSCAN

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_PRSCAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   Description:
*      The current scan is printed as a table suitable for "working"
*      by hand.
*      The output should be diverted to a file!

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*   History:
*     08-MAY-83 (JRG):
*       IUEDR Vn. 1.3
*     06-NOV-88 (PCTR):
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

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSCAN'

*  External References:
      LOGICAL STR_SIMLR  ! Caseless string equality.

*  Local Variables:
      REAL*8 U           ! U-value

      INTEGER I          ! Loop index.
      INTEGER IIQ
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999

      ELSE IF ( NOSCAN ) THEN
         CALL ERROUT( 'Error: no scan\\', STATUS )
         GO TO 999
      END IF

*   1st label (CAMERA,IMAGE,RESOLUTION).
      CALL LINE_WRITS( '%p %s\\', CAMERA )
      CALL LINE_WRITI( '%i\\', IMAGE )
      CALL LINE_WRITS( ' %s\\', RESOL )
      IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
         CALL LINE_WRITF( ',SCAN,SCANDIST=%.3f\\', V1SCAN )

      ELSE IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
         CALL LINE_WRITF( ',SCAN,WAV=%.3f\\', V1SCAN )
      END IF
      CALL LINE_WRITF( ',FWHM=%.1f\\', DVSCAN )
      CALL PRTBUF( STATUS )

*   2nd label (OBJECT).
      CALL LINE_WRITS( '%p OBJECT=''%S''\\', TITLE )
      CALL PRTBUF( STATUS )

*   Print data quality key.
      CALL PRTEOL( STATUS )
      CALL PRQKEY( STATUS )

*   Print column labels.
      CALL PRTEOL( STATUS )
      CALL LINE_WCONT( '%p %5w\\' )
      CALL LINE_WRITS( '%2w%12s\\', ULABEL )
      CALL LINE_WRITS( '%2w%12s\\', SLABEL )
      CALL PRTBUF( STATUS )

*   Print column units.
      CALL LINE_WCONT( '%p Point\\' )
      CALL LINE_WRITS( '%2w%12s\\', UUNITS )
      CALL LINE_WRITS( '%2w%12s\\', SUNITS )
      CALL PRTBUF( STATUS )

*   Print ordinate and absisca.
      CALL PRTEOL( STATUS )

      DO I = 1, NSCAN
         U = DBLE( I - 1 ) * DUSCAN + U1SCAN
         IIQ = QSCAN( I )
         CALL PRXYQ( I, U, FSCAN( I ), IIQ, STATUS )
      END DO

 999  CONTINUE

      END
