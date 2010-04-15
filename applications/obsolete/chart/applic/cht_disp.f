      SUBROUTINE CHT_DISP( STATUS )
*+
*  Name:
*     CHT_DISP

*  Purpose:
*     To output the contents of the parameters file to the terminal.

*  Language:
*     Starlink Fortran 77
*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_DISP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Opens file, outputs it and then closes it again.

*  Usage:
*     disp


*  ADAM Parameters:
*     [parameter_spec]...

*  Prior Requirements:
*     -  This routine requires the softlink PAR, when run on Unix
*     machines, or a logical name PAR on a Vax, to point to the Chart
*     parameters file (chartpar.dat unless user has specified other) in
*     current usage.
*     [routine_prior_requirements]...

*  Authors:
*     AJJB: A.J.J.Broderick (Starlink - RAL)
*     {enter_new_authors_here}

*  History:
*     29-APR-1993 (AJJB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER PARSIZ             ! No. of lines in parameter file
      PARAMETER ( PARSIZ = 25 )

*  Local Variables:
      INTEGER COUNT              ! Loop counter for reading file
      CHARACTER*70 LINE          ! Line read from file/output to screen
*.

      CALL FILEOPEN( 1, 'PAR', 'OLD', 'DIRECT', 'FORMATTED',
     :              .TRUE., 70, .FALSE., STATUS )

* The above call is equivalent to the following OPEN statement:
*
*     OPEN (UNIT=1,FILE='PAR',ACCESS='DIRECT',STATUS='OLD',
*    :      CARRIAGECONTROL='LIST',RECL=70, FORM='FORMATTED')
*

      IF (STATUS .NE. SAI__OK) RETURN

      DO 100 COUNT = 1, PARSIZ
         READ (1, REC = COUNT, FMT='(A70)') LINE
         CALL MSG_OUT( ' ', LINE, STATUS )
 100  CONTINUE

      CLOSE( UNIT = 1 )

      END

