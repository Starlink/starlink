      SUBROUTINE SPD_WZZA( FILENO, LINES, COLAXE, COLWID, COLDAT,
     :   NELM, STATUS )
*+
*  Name:
*     SPD_WZZA

*  Purpose:
*     ASCIN's first pass through table (1-D case).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZZA( FILENO, LINES, COLAXE, COLWID, COLDAT, NELM, STATUS )

*  Description:
*     This routine is designed to be called by ASCIN. It reads a certain
*     range of lines from a file with an ASCII table. From these lines
*     it extracts numbers from certain columns. The only result returned
*     is the number of lines where these numbers could be extracted.

*  Arguments:
*     FILENO = INTEGER (Given)
*        Fortran unit number of the table file.
*     LINES( 2 ) = INTEGER (Given)
*        The start and end number of lines to be used.
*     COLAXE = INTEGER (Given)
*        The column number where in the table to find the axis data.
*     COLWID = INTEGER (Given)
*        The column number where in the table to find the width data.
*        COLWID.EQ.0 is taken to indicate that no widths are to be
*        looked for.
*     COLDAT( 2 ) = INTEGER (Given)
*        The column numbers where in the table to find the data and the
*        data errors. COLDAT(2).EQ.0 is taken to indicate that no errors
*        are to be looked for.
*     NELM = INTEGER (Returned)
*        The number of lines found with valid information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22 Nov 1991 (hme):
*        Original version.
*     24 Nov 1994 (hme):
*        Renamed from ASCIN3.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values

*  Arguments Given:
      INTEGER FILENO
      INTEGER LINES( 2 )
      INTEGER COLAXE
      INTEGER COLWID
      INTEGER COLDAT( 2 )

*  Arguments Returned:
      INTEGER NELM

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER MAXVAL             ! Maximum number of values read
      PARAMETER ( MAXVAL = 4 )   ! axis + width + data + error

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER RDSTAT             ! Status returned from SPD_UAAK
      INTEGER GETVAL             ! Number of columns to be read
      INTEGER MAXCOL             ! Rightmost column used
      INTEGER COLUMN( MAXVAL )   ! Column numbers to be used
      REAL VALI(      MAXVAL )   ! Values read
      CHARACTER * ( 132 ) LINE   ! A line of text from table file

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the column numbers and find the highest column number.
*  First comes the axis column and the width column, then the data
*  column, then the error column.
      GETVAL = 1
      COLUMN(1) = COLAXE
      MAXCOL = COLUMN(1)
      IF ( COLWID .NE. 0 ) THEN
         GETVAL = GETVAL + 1
         COLUMN(GETVAL) = COLWID
         MAXCOL = MAX( MAXCOL, COLUMN(GETVAL) )
      END IF
      GETVAL = GETVAL + 1
      COLUMN(GETVAL) = COLDAT(1)
      MAXCOL = MAX( MAXCOL, COLUMN(GETVAL) )
      IF ( COLDAT(2) .GT. 0 ) THEN
         GETVAL = GETVAL + 1
         COLUMN(GETVAL) = COLDAT(2)
         MAXCOL = MAX( MAXCOL, COLUMN(GETVAL) )
      END IF

*  Check number of requested values against maximum supported.
      IF ( GETVAL .GT. MAXVAL ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASCIN_MAXVAL',
     :      'ASCIN: Pass 1: Cannot handle that many columns.', STATUS )
      END IF

*  Forward to first selected line.
      IF ( LINES(1) .GT. 1 ) THEN
         DO 1 I = 2, LINES(1)
            READ ( FILENO, '(A)', END = 501 ) LINE
 1       CONTINUE
      END IF

*  Loop through selected part of file.
      NELM = 0
      DO 6 I = LINES(1), LINES(2)

*     Read the values in this line.
         CALL SPD_UAAK( FILENO, MAXCOL, GETVAL, VAL__BADR, COLUMN, VALI,
     :      LINE, RDSTAT )

*     If EOF found, the previous line was the last.
         IF ( RDSTAT .EQ. 1 ) THEN
            GO TO 7

*     Else if this was a comment line, goto next line.
         ELSE IF ( RDSTAT .EQ. 2 ) THEN
            GO TO 6

*     Else if this line had less columns than we need, abort.
         ELSE IF ( RDSTAT .EQ. 3 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ASCIN_TOOFEW',
     :         'ASCIN: Pass 1: Line with too few columns found.',
     :         STATUS )
            GO TO 500

*     Else if SPD_UAAK cannot handle that high a column number, abort.
         ELSE IF ( RDSTAT .EQ. 4 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ASCIN_MAXCOL',
     :         'ASCIN: Pass 1: ' //
     :         'Cannot access such high a column number.',
     :         STATUS )
            GO TO 500
         END IF

*     Count this line as valid.
         NELM = NELM + 1
 6    CONTINUE
 7    CONTINUE

*  Return without error.
      GO TO 500

*  EOF encountered while forwarding to first selected line.
 501  CONTINUE
      STATUS = SAI__ERROR
      CALL ERR_REP( 'ASCIN_BNDEOF',
     :   'ASCIN: Pass 1: First selected line is beyond EOF.',
     :   STATUS )

*  Return.
 500  CONTINUE

      END
