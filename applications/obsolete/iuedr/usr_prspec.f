      SUBROUTINE USR_PRSPEC( STATUS )
*+
*  Name:
*     SUBROUTINE USR_PRSPEC

*  Description:
*     The current spectrum/order is printed as a table suitable for
*     "working" by hand.
*     The output should be diverted to a file!

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_PRSPEC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
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

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMFLX'
      INCLUDE 'CMNET'
      INCLUDE 'CMWAV'
      INCLUDE 'CMSPEC'

*  Status:
      INTEGER STATUS        ! Global status.

*  External References:
      LOGICAL STR_SIMLR     ! Caseless string equality.

*  Local Variables:
      INTEGER I             ! Loop index.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  DATASET.
      CALL RDSPEC( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: accessing spectrum\\', STATUS )
         GO TO 999

      ELSE IF ( NOFLX ) THEN
         CALL ERROUT('Error: no flux spectrum\\', STATUS)
         GO TO 999
      END IF

*  1st label (CAMERA,IMAGE,RESOLUTION,APERTURE/ORDER).
      CALL LINE_WRITS( '%p %s\\', CAMERA )
      CALL LINE_WRITI( '%i\\', IMAGE )
      CALL LINE_WRITS( ' %s\\', RESOL )
      IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
         CALL LINE_WRITS( ' %s\\', APERS(1, ORDER) )

      ELSE
         CALL LINE_WRITI( ',ORDER=%i\\', ORDER )
      END IF
      CALL PRTBUF( STATUS )

*  2nd label (OBJECT).
      CALL LINE_WRITS( '%p OBJECT=''%S''\\', TITLE )
      CALL PRTBUF( STATUS )

*  Print data quality key.
      CALL PRTEOL( STATUS )
      CALL PRQKEY( STATUS )

*  Print column labels.
      CALL PRTEOL( STATUS )
      CALL LINE_WCONT( '%p %5w\\' )
      CALL LINE_WRITS( '%2w%12s\\', WLABEL )
      CALL LINE_WRITS( '%2w%12s\\', NLABEL )
      CALL LINE_WRITS( '%2w%12s\\', FLABEL )
      CALL PRTBUF( STATUS )

*  Print column units.
      CALL LINE_WCONT( '%p Point\\' )
      CALL LINE_WRITS( '%2w%12s\\', WUNITS )
      CALL LINE_WRITS( '%2w%12s\\', NUNITS )
      CALL LINE_WRITS( '%2w%12s\\', FUNITS )
      CALL PRTBUF( STATUS )

*  Print wavelength and flux.
      CALL PRTEOL( STATUS )
      DO I = 1, NWAV
         CALL PRSPC( I, STATUS )
      END DO

 999  CONTINUE

      END
