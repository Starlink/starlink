      SUBROUTINE USR_PRMEAN( STATUS )
*+
*  Name:
*     SUBROUTINE USR_PRMEAN

*  Description:
*     The current mean spectrum is printed as a table suitable for
*     "working" by hand.
*     The output should be diverted to a file!

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_PRMEAN( STATUS )

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

*  Type Deifnitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMCOMB'

*  Status:
      INTEGER STATUS  ! Global status.

*  Local Variables:
      INTEGER I       ! Loop index.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  DATASET.
      CALL RDCOMB( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: accessing mean spectrum\\', STATUS )
         GO TO 999

      ELSE IF ( NOCOMB ) THEN
         CALL ERROUT( 'Error: no mean spectrum\\', STATUS )
         GO TO 999
      END IF

*  1st label (CAMERA,IMAGE,RESOLUTION).
      CALL LINE_WRITS( '%p %s\\', CAMERA )
      CALL LINE_WRITI( '%i\\', IMAGE )
      CALL LINE_WRITS( ' %s\\', RESOL )
      CALL LINE_WCONT( ',MEAN\\' )
      CALL PRTBUF( STATUS )

*  2nd label (OBJECT).
      CALL LINE_WRITS( '%p OBJECT=''%S''\\', MTITLE )
      CALL PRTBUF( STATUS )

*  Print data quality key.
      CALL PRTEOL( STATUS )
      CALL PRQKEY( STATUS )

*  Print column labels.
      CALL PRTEOL( STATUS )
      CALL LINE_WCONT( '%p %5w\\' )
      CALL LINE_WRITS( '%2w%12s\\', XMLAB )
      CALL LINE_WRITS( '%2w%12s\\', YMLAB )
      CALL PRTBUF( STATUS )

*  Print column units.
      CALL LINE_WCONT( '%p Point\\' )
      CALL LINE_WRITS( '%2w%12s\\', XMUN )
      CALL LINE_WRITS( '%2w%12s\\', YMUN )
      CALL PRTBUF( STATUS )

*  Print wavelength and flux.
      CALL PRTEOL( STATUS )

      DO I = 1, NCOMB
         CALL PRXYQ( I, XCOMB( I ), YCOMB( I ), QCOMB( I ), STATUS )
      END DO

 999  CONTINUE

      END
