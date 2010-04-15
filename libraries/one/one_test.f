      PROGRAM ONE_TEST
*+
*  Name:
*     one_test.f

*  Purpose:
*     Tests program for ONE library

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     one_test

*  Description:
*     {routine_description}

*  Arguments:
*     None.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     RTP: Roy Platon (Starlink, RAL)
*     BLY: Martin Bly (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     29-SEP-2000 RTP:
*        Original version.
*     30_NOV-2001 BLY:
*        Ruggedisation for production test.
*        Added standard prologue.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'ONE_ERR'         ! Local constants

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      LOGICAL ONE_FIND_FILE

*  Local Variables:
      INTEGER CONTEXT
      INTEGER COLUMNS, ROWS
      LOGICAL FOUND
      INTEGER NFOUND
      CHARACTER*30 COMMAND, FILES, FILENAME

      COMMAND = 'ls'            ! Command for one_exec test.
      FILES = '*.*'             ! Mask for one_find_files test.

*  Local Constants

*.

*  Set STATUS OK and report start of test.
      STATUS = SAI__OK
      CALL EMS_BEGIN( STATUS )

      WRITE(*,*) 'Test of libone routines ...'
      WRITE(*,*) ' '

*  First test: one_exec routine.

      WRITE(*,*) '... one_exec: ', COMMAND
      WRITE(*,*) ' '

      CALL ONE_EXEC( COMMAND, STATUS )

      WRITE(*,*) ' '
      IF ( STATUS .NE. SAI__OK ) THEN
         WRITE(*,*) '... test of one_exec failed ... aborting'
         GOTO 9999
      ELSE
         WRITE(*,*) '... test of one_exec successful.'
      ENDIF

*  Second test: one_find_file routine.

      WRITE(*,*) ' '
      WRITE(*,*) '... one_find_file: ', FILES
      WRITE(*,*) ' '

      CONTEXT = 0

      NFOUND = 0
      DO WHILE ( STATUS .EQ. SAI__OK)
         FOUND = ONE_FIND_FILE( FILES, .FALSE., FILENAME, CONTEXT,
     :        STATUS)
         IF ( FOUND ) THEN
            NFOUND = NFOUND + 1
            WRITE(*,*) '   ... found file: ', FILENAME
         ELSE
            WRITE(*,*) '   ... no more files match search mask: ', FILES
         ENDIF
      END DO

*  Reset status and close find file system.

      CALL EMS_ANNUL( STATUS )
      CALL ONE_FIND_FILE_END( CONTEXT, STATUS )

      WRITE(*,*) ' '
      IF (STATUS .NE. SAI__OK .OR. NFOUND .EQ. 0) THEN
         WRITE(*,*) '... test of one_find_file failed ... aborting'
         IF (NFOUND .EQ. 0) WRITE(*,*) '   ... No files found ...'
         GOTO 9999
      ELSE
         WRITE(*,*) '... test of one_find_file successful.'
      ENDIF

*  Third test: one_scrsz routine.

      WRITE(*,*) ' '
      WRITE(*,*) '... one_scrsz: '
      WRITE(*,*) ' '

      CALL ONE_SCRSZ( COLUMNS, ROWS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         WRITE(*,*) '... test of one_scrsz failed ... aborting'
         GOTO 9999
      ELSE
         WRITE(*,*) '   ... Screen size: Columns: ', COLUMNS
         WRITE(*,*) '                    Rows   : ', ROWS
         WRITE(*,*) ' '
         WRITE(*,*) '... test of one_scrsz successful.'
      ENDIF

 9999 CONTINUE

      WRITE(*,*) ' '
      IF ( STATUS .EQ. SAI__OK ) THEN
         WRITE(*,*) 'Test of libone successful.'
      ELSE
         WRITE(*,*) 'Test of libone failed.'
      ENDIF

      CALL EMS_END( STATUS )

      END
