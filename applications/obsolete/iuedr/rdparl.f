      SUBROUTINE RDPARL( NAME, GIVEN, MAXVAL, VALUES, ACTVAL, STATUS )
*+
*  Name:
*     SUBROUTINE RDPARL

*  Purpose:
*     Read LOGICAL parameter values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDPARL( NAME, GIVEN, MAXVAL, VALUES, ACTVAL, STATUS )

*  Arguments:
*     NAME = BYTE( MAXNAME ) (Given)
*        Parameter name.
*     GIVEN = LOGICAL (Given)
*        Whether a default value has been given.
*     MAXVAL = INTEGER (Given)
*        Size of parameter array.
*     VALUES = LOGICAL( MAXVAL )  (Given and Returned)
*        Array of parameter values.
*     ACTVAL = INTEGER (Returned)
*        Number of values returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     15-SEP-82 (JRG):
*       AT4 version.
*     29-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     01-JUN-89 (PCTR):
*       IUEDR Vn. 2.1
*       Conversion to SGP/16 style.
*     01-OCT-92 (DMILLS):
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms
*     26-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2
*       Tidyed and SAEised
*     28-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*       Echo parameters to the log file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Global Variables:
      INCLUDE 'CMPRT'
      INCLUDE 'CMPSX'

*  Local Constants:
      INTEGER ERR                  ! Error status.
      INTEGER MAXNAME              ! Maximum length of name string.
      INTEGER STDOUT
      PARAMETER ( ERR = -3, MAXNAME = 16, STDOUT = 6 )

*  Arguments Given:
      BYTE NAME( MAXNAME )         ! Parameter name.

      LOGICAL GIVEN                ! Whether default given.

      INTEGER MAXVAL               ! Size of parameter array.

*  Arguments Given and Returned:
      LOGICAL VALUES( MAXVAL )     ! Array of parameter values.

*  Arguments Returned:
      INTEGER ACTVAL               ! Number of values returned.

*  Status:
      INTEGER STATUS               ! Global status.

*  External References:
      INTEGER CHR_LEN              ! String length.

*  Local Variables:
      CHARACTER*( MAXNAME ) LNAME  ! CHARACTER version of parameter name.

      INTEGER I                    ! Loop index.
      INTEGER IOU                  ! I/O unit for output log.
      INTEGER NCHAR                ! String length.
      INTEGER PCHAR                ! Parameter name length.
*.

*   Check inherited global status.
*      IF (STATUS .NE. SAI__OK) RETURN
      STATUS = SAI__OK

      CALL GEN_STOC( NAME, MAXNAME, LNAME, NCHAR )

      IF ( GIVEN ) THEN
         IF ( MAXVAL .GT. 1 ) THEN
            CALL PAR_DEF1L( LNAME, MAXVAL, VALUES, STATUS )

         ELSE
            CALL PAR_DEF0L( LNAME, VALUES, STATUS )
         END IF
      END IF

      IF ( MAXVAL .GT. 1 ) THEN
         CALL PAR_GET1L( LNAME, MAXVAL, VALUES, ACTVAL, STATUS )

      ELSE
         CALL PAR_GET0L( LNAME, VALUES, STATUS )
         ACTVAL = 1
      END IF

      IF ( ( PRTRED .OR. ISLOG ) .AND. PARRED .AND.
     :     STATUS.EQ.SAI__OK ) THEN
         IF ( ISLOG ) THEN
            IOU = LOGFILE

         ELSE
            IOU = STDOUT
         END IF
         PCHAR = CHR_LEN( LNAME )
         DO I = 1, ACTVAL
            IF ( VALUES( I ) ) THEN
               WRITE( IOU,  '( 1X, A, A, A, A )' )
     :                LNAME( : PCHAR ), '=', 'TRUE', '.'

            ELSE
               WRITE( IOU,  '( 1X, A, A, A, A )' )
     :                LNAME( : PCHAR ), '=', 'FALSE', '.'
            END IF
         END DO
      END IF

*  Flush Parameter-system errors, except PAR__NULL which can have
*  a special meaning to some commands.
*  Other commands pass parameters to PARFER which treats the PAR__NULL
*  status as an effective PAR__ABORT.
      IF ( STATUS.NE.PAR__NULL .AND. STATUS.NE.SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = ERR
      END IF

      END
