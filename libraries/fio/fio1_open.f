      SUBROUTINE FIO1_OPEN( UNIT, FILE, FSTAT, FORMAT, ACCESS, RDONLY,
     :   CCNTL, CC, RECLEN, RECSZ, STATUS )
*+
*  Name:
*     FIO1_OPEN

*  Purpose:
*     Do the actual Fortran OPEN statement

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO1_OPEN( UNIT, FILE, FSTAT, FORMAT, ACCESS, RDONLY,
*    :   CCNTL, CC, RECLEN, RECSZ, STATUS )

*  Description:
*     Execute the actual Fortran OPEN statement. This is isolated in a
*     subroutine as it contains machine dependent code.

*  Arguments:
*     UNIT = INTEGER (Given)
*        The FORTRAN unit number to be opened
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the file to be opened.
*     FSTAT = CHARACTER * ( * ) (Given)
*        The open status of the file, i.e. the "STATUS=" option.
*     FORMAT = CHARACTER * ( * ) (Given)
*        The format of the file (FORMATTED or UNFORMATTED)
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access mode (SEQUENTIAL, DIRECT or APPEND)
*     RDONLY = LOGICAL (Given)
*        Is the file to be opened read-only?
*     CCNTL = LOGICAL (Given)
*        Is the CARRIAGECONTROL option to be used?
*     CC = character * ( * ) (Given)
*        The value of the CARRIAGECONTROL option.
*     RECLEN = LOGICAL (Given)
*        Is the RECLEN option to be used?
*     RECSZ = INTEGER (Given)
*        The record length used when opening the file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Open the file
*     -  Handle any error.
      
*  Machine-specific features used:
*     Linux - The extension ACCESS='APPEND' is available. The CARRIAGECONTROL
*             and READONLY parameters are not available and are ignored.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     BKM: Brian McIlwrath (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1993 (PMA):
*        Original version.
*     15-MAR-1996 (BKM)
*        Linux version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) ACCESS
      CHARACTER * ( * ) CC
      CHARACTER * ( * ) FILE
      CHARACTER * ( * ) FORMAT
      CHARACTER * ( * ) FSTAT
      INTEGER RECSZ
      INTEGER UNIT
      LOGICAL CCNTL
      LOGICAL RDONLY
      LOGICAL RECLEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SYSERR             ! The Fortran I/O status value
      CHARACTER*1 CH             ! A dummy character to read

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the file.
      OPEN( UNIT=UNIT, FILE=FILE, STATUS=FSTAT, FORM=FORMAT,
     :   ACCESS=ACCESS, ERR=90, IOSTAT=SYSERR )
      GOTO 999

*  Handle any error condition.
   90 CALL FIO_SERR( SYSERR, STATUS )
      CALL FIO_PUNIT( UNIT, STATUS )

  999 CONTINUE
      END
