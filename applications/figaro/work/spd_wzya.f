      SUBROUTINE SPD_WZYA( FILENO, VXIST, WIDTH, BADVAL, IN, AXIS, IDIM,
     :   OELM, LABEL, UNITS, ZLABEL, ZUNITS, DCOL, FORM, STATUS )
*+
*  Name:
*     SPD_WZYA

*  Purpose:
*     Write the header for ASCOUT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZYA( FILENO, VXIST, WIDTH, BADVAL, IN, AXIS, IDIM, OELM,
*        LABEL, UNITS, ZLABEL, ZUNITS, DCOL, FORM, STATUS )

*  Description:
*     This routine is designed to be called by ASCOUT and to write the
*     header information.

*  Arguments:
*     FILENO = INTEGER (Given)
*        Fortran unit number for output file.
*     VXIST = LOGICAL (Given)
*        True if column for errors necessary.
*     WIDTH = LOGICAL (Given)
*        True if columns for pixel widths necessary.
*     BADVAL = REAL (Given)
*        The bad value to be used in the ASCII table.
*     IN = CHARACTER * ( * ) (Given)
*        The input NDF name.
*     AXIS = INTEGER (Given)
*        Number of the axis selected for higher precision.
*     IDIM = INTEGER (Given)
*        Number of axes in input file.
*     OELM = INTEGER (Given)
*        Number of pixels in input.
*     LABEL( IDIM ) = CHARACTER * ( * ) (Given)
*        Axis labels.
*     UNITS( IDIM ) = CHARACTER * ( * ) (Given)
*        Axis units.
*     ZLABEL = CHARACTER * ( * ) (Given)
*        Data label.
*     ZUNITS = CHARACTER * ( * ) (Given)
*        Data unit.
*     DCOL = INTEGER (Given)
*        Number of the column which is to be of higher precision.
*     FORM = CHARACTER * ( * ) (Returned)
*        Output format for ASCWRD. A length of at least 40 is
*        recommended.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     HME: Horst Meyerdierks (UoE)
*     {enter_new_authors_here}

*  History:
*     11-APR-1991 (HME):
*        Original version.
*     05-JUL-1991 (HME):
*        Argument list changed.
*     20-NOV-1991 (HME):
*        Rewrite for N-D version of ASCOUT.
*     19-MAY-1992 (HME):
*        Port ASCOUT to NDF (and Unix). This makes some arguments
*        obsolete, since an NDF section retains degenerate axes, while
*        ASCOUT before used SSETDO with RETAIN false.
*        Replace empty label and unit strings with "unknown". This is to
*        work around the Sun not printing blank strings and thus
*        shifting output to the left.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FILENO
      LOGICAL VXIST
      LOGICAL WIDTH
      REAL BADVAL
      CHARACTER * ( * ) IN
      INTEGER AXIS
      INTEGER IDIM
      INTEGER OELM
      CHARACTER * ( * ) LABEL( IDIM )
      CHARACTER * ( * ) UNITS( IDIM )
      CHARACTER * ( * ) ZLABEL
      CHARACTER * ( * ) ZUNITS
      INTEGER DCOL

*  Arguments returned.
      CHARACTER * ( * ) FORM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER HEADLN             ! Number of lines in header
      PARAMETER ( HEADLN = 9 )

*  Local Variables:
      INTEGER I                  ! Loop index
      CHARACTER * ( 40 ) FORMA   ! Text format for section 8
      CHARACTER * ( 80 ) FORMD   ! Line of -'s in section 9

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Formats.
*  Second digit of label indicates section number.
*  Other formats used are the strings FORMA and FORMD.
*  FORM is a format string returned to the calling routine.
 111  FORMAT ( ' !1' /
     :   ' ! SPECDRE: ASCII table from data set (18-MAY-1992).' /
     :   ' ! Input NDF:         ', A / ' ! Table is in lines: ', 2I8 /
     :   ' ! Bad value:         ', G15.7E2 )
 181  FORMAT ( ' !8' )

*  First section.
*  Title, input file, line numbers occupied by table, bad value.
      WRITE( FILENO, 111 ) IN, HEADLN+1, HEADLN+OELM, BADVAL

*  If the 12-digit column is neither the first nor the last possible,
*  but is requested: Format strings for eighth and ninth section.
      IF ( DCOL .GT. 1 .AND. DCOL .LT. 8 ) THEN

*     Number format to be used by ASCWRD to write the table itself.
         FORM  = '(''  '',1G15.7E2,G20.12E2,1G15.7E2)'
         WRITE( FORM(7:7), '(I1)' ) DCOL-1
         WRITE( FORM(25:25), '(I1)' ) 8-DCOL

*     String format for the label and units lines of the column
*     headings.
         FORMA = '('' !'',1('' '',A14),'' '',A19,1('' '',A14))'
         WRITE( FORMA(7:7), '(I1)' ) DCOL-1
         WRITE( FORMA(26:26), '(I1)' ) 8-DCOL

*     String format for the line of dashes that separates the column
*     headings from the table itself.
         FORMD = '('' !9'',1(''-------------- '')' //
     :      ',''------------------- '',' //
     :      '1(''-------------- ''))'
         WRITE( FORMD(8:8), '(I1)' ) DCOL-1
         WRITE( FORMD(52:52), '(I1)' ) 8-DCOL

*  Else if the 12-digit column is the first: Format strings for eighth
*  and ninth section.
      ELSE IF ( DCOL .EQ. 1 ) THEN
         FORM  = '(''  '',G20.12E2,7G15.7E2)'
         FORMA = '('' ! '',A19,7('' '',A14))'
         FORMD = '('' !9'',''------------------- '',' //
     :      '7(''-------------- ''))'

*  Else if the 12-digit column is the last possible: Format strings for
*  eighth and ninth section.
      ELSE IF ( DCOL .EQ. 8 ) THEN
         FORM  = '(''  '',7G15.7E2,G20.12E2)'
         FORMA = '('' !'',7('' '',A14),'' '',A19)'
         FORMD = '('' !9'',7(''-------------- '')' //
     :      ',''------------------- '')'

*  Else (if there is no 12-digit column): Format strings for eighth and
*  ninth section.
      ELSE
         FORM  = '(''  '', 8G15.7E2 )'
         FORMA = '('' !'',8('' '',A14))'
         FORMD = '('' !9'',8(''-------------- ''))'
      END IF

*  Eighth section.
      WRITE( FILENO, 181 )

*  If we need columns for axes, widths, data and error.
      IF ( VXIST .AND. WIDTH ) THEN
         WRITE( FILENO, FORMA )
     :      ( LABEL(I), 'width              ', I = 1, IDIM ),
     :      ZLABEL, 'error              '
         WRITE( FILENO, FORMA )
     :      ( UNITS(I), UNITS(I), I = 1, IDIM ),
     :      ZUNITS, ZUNITS

*  Else if we need error but not widths.
      ELSE IF ( VXIST ) THEN
         WRITE( FILENO, FORMA )
     :      ( LABEL(I), I = 1, IDIM ),
     :      ZLABEL, 'error              '
         WRITE( FILENO, FORMA )
     :      ( UNITS(I), I = 1, IDIM ),
     :      ZUNITS, ZUNITS

*  Else if we need widths but not error.
      ELSE IF ( WIDTH ) THEN
         WRITE( FILENO, FORMA )
     :      ( LABEL(I), 'width              ', I = 1, IDIM ),
     :      ZLABEL
         WRITE( FILENO, FORMA )
     :      ( UNITS(I), UNITS(I), I = 1, IDIM ),
     :      ZUNITS

*  Else (if we need only axes and data).
      ELSE
         WRITE( FILENO, FORMA )
     :      ( LABEL(I), I = 1, IDIM ), ZLABEL
         WRITE( FILENO, FORMA )
     :      ( UNITS(I), I = 1, IDIM ), ZUNITS
      END IF

*  Ninth section.
*  The ninth section of the file is the table itself. The data are
*  written by ASCWRD. Here we write only the section marker !9 followed
*  by dashes separating the column headers from the data.
      WRITE( FILENO, FORMD )

*  Return.
 500  CONTINUE
      END
