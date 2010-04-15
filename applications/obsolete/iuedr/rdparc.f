      SUBROUTINE RDPARC( NAME, GIVEN, MAXVAL, VALUES, ACTVAL, STATUS )
*+
*  Name:
*     SUBROUTINE RDPARC

*  Purpose:
*     Read CHARACTER parameter values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDPARC( NAME, GIVEN, MAXVAL, VALUES, ACTVAL, STATUS )

*  Arguments:
*     NAME = BYTE( MAXNAME ) (Given)
*        Parameter name.
*     GIVEN = LOGICAL (Given)
*        Whether a default value has been given.
*     MAXVAL = INTEGER (Given)
*        Size of BYTE array.
*     VALUES = BYTE( MAXVAL )  (Given and Returned)
*        Array for parameter value.
*     ACTVAL = INTEGER (Returned)
*        Length of string returned.
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
      INTEGER MAXLEN               ! Maximum length of character string.
      INTEGER STDOUT
      PARAMETER ( ERR = -3, MAXNAME = 16, MAXLEN = 255, STDOUT = 6 )

*  Arguments Given:
      BYTE NAME( MAXNAME )         ! Parameter name.
      LOGICAL GIVEN                ! Whether default given.
      INTEGER MAXVAL               ! Size of array.

*  Arguments Given and Returned:
      BYTE VALUES( MAXVAL )        ! Array of parameter values.

*  Arguments Returned:
      INTEGER ACTVAL               ! Length of string returned.

*  Status:
      INTEGER STATUS               ! Status return.

*  External References:
      INTEGER CHR_LEN              ! String length.

*  Local Variables:
      CHARACTER*( MAXNAME ) LNAME  ! CHARACTER version of parameter name.
      CHARACTER*( MAXLEN ) ISTRING ! CHARACTER version of parameter value.

      INTEGER I                    ! Loop index.
      INTEGER IOU                  ! I/O unit for output log.
      INTEGER NCHAR                ! String Length.
      INTEGER PCHAR                ! Parameter name length.
*.

*  Check inherited global status.
*      IF ( STATUS .NE. SAI__OK ) RETURN
      STATUS = SAI__OK

      CALL GEN_STOC( NAME, MAXNAME, LNAME, NCHAR )

      IF ( GIVEN ) THEN
         CALL GEN_STOC( VALUES, MAXLEN, ISTRING, NCHAR )
         CALL PAR_DEF0C( LNAME, ISTRING( : NCHAR ), STATUS )
      END IF

      ISTRING = ' '
      DO I = 1, MAXVAL
         VALUES( I ) = ICHAR(' ')
      END DO

      CALL PAR_GET0C( LNAME, ISTRING, STATUS )

      CALL GEN_CTOS( ISTRING, MAXVAL, VALUES, NCHAR )
      NCHAR = CHR_LEN( ISTRING )
      CALL STR_TERM( NCHAR, MAXVAL, VALUES )
      ACTVAL = NCHAR + 1

      IF ( ( PRTRED .OR. ISLOG ) .AND. PARRED .AND.
     :     STATUS.EQ.SAI__OK ) THEN
         IF ( ISLOG ) THEN
            IOU = LOGFILE

         ELSE
            IOU = STDOUT
         END IF
         PCHAR = CHR_LEN( LNAME )
         WRITE( IOU, '( 1X, A, A, A, A )' )
     :          LNAME( : PCHAR ), '=''', ISTRING( : NCHAR ), '''.'
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
