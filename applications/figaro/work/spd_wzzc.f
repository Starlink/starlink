      SUBROUTINE SPD_WZZC( FILENO, NDIM, LINES, COLUMN,
     :   START, ENDV, STEP, STATUS )
*+
*  Name:
*     SPD_WZZC

*  Purpose:
*     ASCIN's first pass through table (N-D case).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZZC( FILENO, NDIM, LINES, COLUMN, START, ENDV, STEP,
*        STATUS )

*  Description:
*     This routine is designed to be called by ASCIN. It reads a certain
*     range of lines from a file with an ASCII table. From these lines
*     it extracts numbers from certain columns. These are taken as
*     coordinates along NDIM axes. From the extrema and first non-zero
*     differences of coordinates a guess for a linear grid is evaluated.

*  Arguments:
*     FILENO = INTEGER (Given)
*        Fortran unit number of the table file.
*     NDIM = INTEGER (Given)
*        The number of values to be extracted from any table line.
*     LINES( 2 ) = INTEGER (Given)
*        The start and end number of lines to be used.
*     COLUMN( NDIM ) = INTEGER (Given)
*        The numbers of the columns from where to get the values.
*     START( NDIM ) = REAL (Returned)
*        The minimum values found in each specified column.
*     ENDV( NDIM ) = REAL (Returned)
*        The maximum values found in each specified column.
*     STEP( NDIM ) = REAL (Returned)
*        The guessed steps for linear grids from each specified column.
*        This is calculated as follows. In the first approximation STEP
*        is the first non-zero difference of values found in each
*        specified column. In a second approximation this and the
*        extreme values are used to guess the number of pixels. That
*        being an integer the final guess for step is
*        (ENDV-START)/(size-1) where size is the guessed number of
*        pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     18-MAY-1991 (HME):
*        Original version.
*     05-JUL-1991 (HME):
*        STATUS added to argument list.
*     22-NOV-1991 (HME):
*        Convert to N-D version.
*     05-MAY-1992 (HME):
*        Improve the guess of STEP.
*     09-JUL-1992 (HME):
*        Replace NINT with INT(..+0.5).
*     24 Nov 1994 (hme):
*        Renamed from ASCIN1.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PRM_PAR'          ! Bad values

*  Arguments Given:
      INTEGER FILENO
      INTEGER NDIM
      INTEGER LINES( 2 )
      INTEGER COLUMN( NDIM )

*  Arguments Returned:
      REAL START( NDIM )
      REAL ENDV(  NDIM )
      REAL STEP(  NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J               ! Loop indices
      INTEGER RDSTAT             ! Status returned by SPD_UAAK
      INTEGER MAXCOL             ! Rightmost column used
      LOGICAL DATFND             ! True if valid line found
      LOGICAL STPFND( NDF__MXDIM ) ! True if step found
      REAL VALI( NDF__MXDIM )    ! Values read
      CHARACTER * ( 132 ) LINE   ! A line of text from table file

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check number of requested values against maximum supported.
      IF ( NDIM .GT. NDF__MXDIM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASCIN_MAXVAL',
     :      'ASCIN: Pass 1: Cannot handle that many columns.', STATUS )
      END IF

*  Find out the highest column number to be read.
      MAXCOL = COLUMN(1)
      DO 1 I = 1, NDIM
         STPFND(I) = .FALSE.
         MAXCOL = MAX( MAXCOL, COLUMN(I) )
 1    CONTINUE

*  Forward to first selected line
      IF ( LINES(1) .GT. 1 ) THEN
         DO 2 I = 1, LINES(1)-1
            READ ( FILENO, '(A)', END = 501 ) LINE
 2       CONTINUE
      END IF

*  Loop through selected part of file.
      DATFND = .FALSE.
      DO 5 I = LINES(1), LINES(2)

*     Read the values in this line.
         CALL SPD_UAAK( FILENO, MAXCOL, NDIM, VAL__BADR, COLUMN, VALI,
     :      LINE, RDSTAT )

*     If EOF found, the previous line was the last.
         IF ( RDSTAT .EQ. 1 ) THEN
            GO TO 6

*     Else if this was a comment line, goto next line.
         ELSE IF ( RDSTAT .EQ. 2 ) THEN
            GO TO 5

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

*     If this is first valid set of data, set the extrema and prevent
*     further valid sets to be taken as the first ones.
         IF ( .NOT. DATFND ) THEN
            DATFND = .TRUE.
            DO 3 J = 1, NDIM
               START(J) = VALI(J)
               ENDV(J)  = VALI(J)
 3          CONTINUE

*     Else (if this valid, but not the first valid), ...
         ELSE

*        For each axis, ...
            DO 4 J = 1, NDIM

*           Update the extrema.
               START(J) = MIN( START(J), VALI(J) )
               ENDV(J)  = MAX(  ENDV(J), VALI(J) )

*           If no step found yet, ...
               IF ( .NOT. STPFND(J) ) THEN

*              Make a new guess at step. If it is valid, prevent further
*              guesses.
                  STEP(J) = ENDV(J) - START(J)
                  IF ( STEP(J) .NE. 0. ) STPFND(J) = .TRUE.
               END IF
 4          CONTINUE
         END IF
 5    CONTINUE

*  Regular escape from the loop. Try a better guess for STEP
 6    CONTINUE
      DO 7 I = 1, NDIM
         J = INT( (ENDV(I) - START(I)) / STEP(I) + 0.5 )
         STEP(I) = (ENDV(I) - START(I)) / FLOAT(J)
 7    CONTINUE

*  Circumvent the error report on label 501.
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
