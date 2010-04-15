      PROGRAM CREPAR
*+
*  Name:
*     CREPAR

*  Purpose:
*     To create a direct access file containing the standard CHART parameters

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Main program

*  Description:
*     Read the names and values of the CHART parameters from an ASCII file and
*     create a direct access file containing them. This program is necessary
*     since the resulting file is not portable across machine types. There
*     appears to be a one byte difference between the record lengths on Ultrix
*     and other systems.

*  Algorithm:
*     -  Open the ASCII input file.
*     -  Create the direct access output file.
*     -  For each record in the input file:
*        -  Read a record.
*        -  Write a record.
*     -  Close the files.

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     8-FEB-1994 (PMA):
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
      INTEGER NUMPAR             ! No. of CHART parameters
      PARAMETER ( NUMPAR = 25 )

*  Local Variables:
      INTEGER I                  ! Loop counter
      CHARACTER * ( 80 ) LINE    ! A line of data
*.

      STATUS = SAI__OK

      OPEN( UNIT=1, STATUS='OLD' )

      CALL FILEOPEN( 2, 'fort.2', 'NEW', 'DIRECT', 'FORMATTED',
     :   .TRUE., 70, .FALSE., STATUS )

      IF( STATUS .EQ. SAI__OK ) THEN
         DO I = 1, NUMPAR
            READ( 1, '(A)' ) LINE
            WRITE( 2, REC=I, FMT='(A70)' ) LINE
         END DO

         CLOSE( 2 )
      END IF

      CLOSE( 1 )

      END
