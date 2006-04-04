      SUBROUTINE KPS1_LOOK( FRM, XLO, XHI, YLO, YHI, ARRAY, QUIET, LOG,
     :                      FD, LINE, FORMAT, MAXLEN, VALUE, STATUS )
*+
*  Name:
*     KPS1_LOOK

*  Purpose:
*     Write the contents of an array to a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LOOK( FRM, XLO, XHI, YLO, YHI, ARRAY, QUIET, LOG, FD, LINE,
*                     FORMAT, MAXLEN, LINE, VALUE, STATUS )

*  Description:
*     This routine writes out the contents of an array to a text file 
*     and to the screen.

*  Arguments:
*     FRM = INTEGER (Given)
*        An AST Frame. The AST_FORMAT method for axis 1 of this Frame is used 
*        to format the data values.
*     XLO = INTEGER (Given)
*        The lower pixel bound of the array on the first axis.
*     XHI = INTEGER (Given)
*        The upper pixel bound of the array on the first axis.
*     YLO = INTEGER (Given)
*        The lower pixel bound of the array on the second axis.
*     YHI = INTEGER (Given)
*        The upper pixel bound of the array on the second axis.
*     ARRAY( XLO:XHI, YLO:YHI ) = DOUBLE PRECISION (Given)
*        The array to list.
*     QUIET = LOGICAL (Given)
*        Suppress screen output?
*     LOG = INTEGER (Given)
*        Write output to the text file?
*     FD = INTEGER (Given)
*        An FIO descriptor for the text file. Only accessed if LOG is .TRUE.
*     LINE = CHARACTER * ( * ) (Given)
*        Work space to use as a buffer for textual output. Should be at least 
*        MAXLEN characters long.
*     FORMAT = CHARACTER * ( * ) (Given)
*        The format in which the array contents are to be listed:
*
*        - "STRIPS" -- The area being displayed is divided up into
*        vertical strips of limited width. Each strip is displayed in
*        turn, with Y pixel index at the left of each row, and
*        X pixel index at the top of each column. The highest row is
*        listed first in each strip.
*
*        - "CLIST" -- Each row of textual output consists of an X pixel 
*        index, followed by a Y pixel index, followed by the pixel data
*        value. No headers or blank lines are included. The pixels are
*        listed in "fortran order" - the lower left pixel first, and the
*        upper right pixel last. 
*
*        - "CGLIST" -- Like CLIST except bad pixels are omitted.
*
*        - "VLIST" -- Each row of textual output consists of just the
*        pixel data value. No headers or blank lines are included. The 
*        pixels are listed in "fortran order" - the lower left pixel first, 
*        and the upper right pixel last.
*
*        - "REGION" -- The pixel data values are listed as a 2 dimensional
*        region. Each row of textual output contains a whole row of data 
*        values. The textual output may be truncated if it is too wide. The
*        highest row is listed first.
*
*        In all cases, adjacent values are sepaerated by spaces, and bad
*        pixel values are represented by the string "BAD". Values equal
*        to (VAL__MAXD - 1) are represented by the string "OUT".
*     MAXLEN = INTEGER (Given)
*        The maximum length allowed for a line of textual output.
*     VALUE = DOUBLE PRECISION (Returned)
*        The value stored in ARRAY(XHI,YHI).
*     STATUS = INTEGER (Given)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-OCT-2001 (DSB):
*        Original version.
*     4-APR-2006 (DSB):
*        Added CGList format.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER FRM
      INTEGER XLO
      INTEGER XHI
      INTEGER YLO
      INTEGER YHI
      DOUBLE PRECISION ARRAY( XLO:XHI, YLO:YHI )
      LOGICAL QUIET
      LOGICAL LOG
      INTEGER FD
      CHARACTER LINE*(*)
      CHARACTER FORMAT*(*)
      INTEGER MAXLEN

*  Arguments Returned:
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BADLEN           ! Length of text for bad values
      PARAMETER( BADLEN = 3 )

      CHARACTER BADTXT*(BADLEN)! Text for bad values
      PARAMETER( BADTXT = 'BAD' )

      DOUBLE PRECISION OUTVAL  ! The out value
      PARAMETER( OUTVAL = VAL__MAXD - 1.0D0 )

      INTEGER OUTLEN           ! Length of text for out values
      PARAMETER( OUTLEN = 3 )

      CHARACTER OUTTXT*(OUTLEN)! Text for out values
      PARAMETER( OUTTXT = 'OUT' )

      INTEGER XGAP             ! No. of characters between row index and data
      PARAMETER( XGAP = 3    ) ! values in Strips format (one will be a ":").

*  Local Variables:
      INTEGER COLWID           ! Max field width for a column in a "strip"
      INTEGER IAT              ! Length of string
      INTEGER IX               ! X pixel index
      INTEGER IX0              ! X pixel index of left most column in strip
      INTEGER IY               ! Y pixel index
      INTEGER JAT              ! Length of string
      INTEGER NV               ! Max number of columns in a strip
      INTEGER VWID             ! Max field width for a data value
      INTEGER XWID             ! Max field width for an X pixel index
      INTEGER YWID             ! Max field width for a Y pixel index

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Format every value, using CHR to get the most compact format. Find
*  the maximum field width needed to format any value.
      VWID = 0
      DO IY = YLO, YHI
         DO IX = XLO, XHI
            IF( ARRAY( IX, IY ) .EQ. VAL__BADD ) THEN
               JAT = BADLEN
            ELSE IF( ARRAY( IX, IY ) .EQ. OUTVAL ) THEN
               JAT = OUTLEN
            ELSE
               CALL CHR_CTOC( AST_FORMAT( FRM, 1, ARRAY( IX, IY ), 
     :                                    STATUS ), LINE, JAT )
            END IF
            VWID = MAX( VWID, JAT )
         END DO
      END DO

*  Find the maximum field width for a pixel index.
      CALL CHR_ITOC( XLO, LINE, IAT )
      XWID = IAT

      CALL CHR_ITOC( XHI, LINE, IAT )
      XWID = MAX( XWID, IAT )

      CALL CHR_ITOC( YLO, LINE, IAT )
      YWID = IAT

      CALL CHR_ITOC( YHI, LINE, IAT )
      YWID = MAX( YWID, IAT )

*  Add an extra space onto all field widths.
      VWID = VWID + 1
      XWID = XWID + 1
      YWID = YWID + 1

*  Strips: Output consists of a set of rectangular blocks, displayed one after 
*  the other. Each block represents a vertical strip covering the entire
*  height of the array. The width is chosen so that the maximum line
*  length, MAXLEN, is not exceeded. Each column has a header giving its X
*  pixel index, and each row starts with the Y pixel index for the row.
      IF( FORMAT .EQ. 'STRIPS' ) THEN

*  A column must be wide enough to hold both a value and an X pixel index.
         COLWID = MAX( VWID, XWID )

*  The maximum number of values across one strip.
         NV = MIN( XHI - XLO + 1, ( MAXLEN - YWID - XGAP )/COLWID )

*  If there is room left over, make the columns wider (but do not allow the
*  extra space to be more than half the original column width).
         COLWID = COLWID + 
     :            MIN( COLWID/2, 
     :                 MAX( 0, ( MAXLEN - YWID - XGAP - NV*COLWID )/NV ) 
     :               )

*  Initialize the X pixel index of the first column in the next strip.
         IX0 = XLO

*  Loop round producing strips until all columns have been produced.
         DO WHILE( IX0 .LE. XHI .AND. STATUS .EQ. SAI__OK )

*  A blank line.
            CALL KPG1_REPRT( ' ', QUIET, LOG, FD, STATUS )

*  Produce the line containing the X pixel index for each column.
            LINE = ' '
            IAT = YWID + XGAP + 1
            DO IX = IX0, MIN( IX0 + NV - 1, XHI )
               JAT = IAT
               CALL CHR_PUTI( IX, LINE, JAT )
               IAT = IAT + COLWID
            END DO
            CALL KPG1_REPRT( LINE( : JAT ), QUIET, LOG, FD, STATUS )

*  Another blank line.
            CALL KPG1_REPRT( ' ', QUIET, LOG, FD, STATUS )

*  Loop round all rows, in reverse order (so that the top row is at the
*  top of the listing).
            DO IY = YHI, YLO, -1
               LINE = ' '
               IAT = 0

*  The Y pixel index.
               JAT = IAT
               CALL CHR_PUTI( IY, LINE, JAT )
               CALL CHR_PUTC( ':', LINE, JAT )
               IAT = IAT + YWID + XGAP 

*  Append the data values for each column.
               DO IX = IX0, MIN( IX0 + NV - 1, XHI )
                  JAT = IAT
                  IF( ARRAY( IX, IY ) .EQ. VAL__BADD ) THEN
                     CALL CHR_PUTC( BADTXT, LINE, JAT )

                  ELSE IF( ARRAY( IX, IY ) .EQ. OUTVAL ) THEN
                     CALL CHR_PUTC( OUTTXT, LINE, JAT )

                  ELSE
                     CALL CHR_PUTC( AST_FORMAT( FRM, 1, ARRAY( IX, IY ), 
     :                                          STATUS ), LINE, JAT )
                  END IF
                  IAT = IAT + COLWID
               END DO

*  Write out this line.
               CALL KPG1_REPRT( LINE( : JAT ), QUIET, LOG, FD, STATUS )

            END DO

*  Set the first column index for the next strip.
            IX0 = IX0 + NV

         END DO

*  A blank line.
         CALL KPG1_REPRT( ' ', QUIET, LOG, FD, STATUS )

*  "CLIST": Each row of textual output consists of an X pixel index, followed 
*  by a Y pixel index, followed by the pixel data value. No headers or blank 
*  lines are included. The pixels are listed in "fortran order" - the lower 
*  left pixel first, and the upper right pixel last. All columns left
*  justified.
      ELSE IF( FORMAT .EQ. 'CLIST' ) THEN
         DO IY = YLO, YHI
            DO IX = XLO, XHI
               LINE = ' '

               IAT = 0
               CALL CHR_PUTI( IX, LINE, IAT )
               IAT = XWID

               JAT = IAT
               CALL CHR_PUTI( IY, LINE, JAT )
               IAT = IAT + YWID

               IF( ARRAY( IX, IY ) .EQ. VAL__BADD ) THEN
                  CALL CHR_PUTC( BADTXT, LINE, IAT )

               ELSE IF( ARRAY( IX, IY ) .EQ. OUTVAL ) THEN
                  CALL CHR_PUTC( OUTTXT, LINE, IAT )

               ELSE
                  CALL CHR_PUTC( AST_FORMAT( FRM, 1, ARRAY( IX, IY ), 
     :                                       STATUS ), LINE, IAT )
               END IF

               CALL KPG1_REPRT( LINE( : IAT ), QUIET, LOG, FD, STATUS )

            END DO
         END DO

*  "CGLIST": Like CLIST except bad pixels are omitted.
      ELSE IF( FORMAT .EQ. 'CGLIST' ) THEN
         DO IY = YLO, YHI
            DO IX = XLO, XHI
               IF( ARRAY( IX, IY ) .NE.VAL__BADD ) THEN
                  LINE = ' '
         
                  IAT = 0
                  CALL CHR_PUTI( IX, LINE, IAT )
                  IAT = XWID
         
                  JAT = IAT
                  CALL CHR_PUTI( IY, LINE, JAT )
                  IAT = IAT + YWID
         
                  IF( ARRAY( IX, IY ) .EQ. VAL__BADD ) THEN
                     CALL CHR_PUTC( BADTXT, LINE, IAT )
         
                  ELSE IF( ARRAY( IX, IY ) .EQ. OUTVAL ) THEN
                     CALL CHR_PUTC( OUTTXT, LINE, IAT )
         
                  ELSE
                     CALL CHR_PUTC( AST_FORMAT( FRM, 1, ARRAY( IX, IY ), 
     :                                          STATUS ), LINE, IAT )
                  END IF
         
                  CALL KPG1_REPRT( LINE( : IAT ), QUIET, LOG, FD, 
     :                             STATUS )

               END IF
            END DO
         END DO

*  "VLIST":  Each row of textual output consists of just the pixel data value. 
*  No headers or blank lines are included. The pixels are listed in "fortran 
*  order" - the lower left pixel first, and the upper right pixel last.
      ELSE IF( FORMAT .EQ. 'VLIST' ) THEN
         DO IY = YLO, YHI
            DO IX = XLO, XHI
               IF( ARRAY( IX, IY ) .EQ. VAL__BADD ) THEN
                  CALL CHR_PUTC( BADTXT, LINE, JAT )

               ELSE IF( ARRAY( IX, IY ) .EQ. OUTVAL ) THEN
                  CALL CHR_PUTC( OUTTXT, LINE, JAT )

               ELSE
                  CALL CHR_CTOC( AST_FORMAT( FRM, 1, ARRAY( IX, IY ), 
     :                                       STATUS ), LINE, JAT )
               END IF
               CALL KPG1_REPRT( LINE( : JAT ), QUIET, LOG, FD, STATUS )
            END DO
         END DO

*  "REGION": The pixel data values are listed as a 2 dimensional region. 
*  Each row of textual output contains a whole row of data values. The 
*  textual output may be truncated if it is too wide. The highest row is 
*  listed first.
      ELSE IF( FORMAT .EQ. 'REGION' ) THEN
         DO IY = YLO, YHI
            LINE = ' '
            IAT = 0

            DO IX = XLO, XHI
               JAT = IAT
               IF( ARRAY( IX, IY ) .EQ. VAL__BADD ) THEN
                  CALL CHR_PUTC( BADTXT, LINE, JAT )

               ELSE IF( ARRAY( IX, IY ) .EQ. OUTVAL ) THEN
                  CALL CHR_PUTC( OUTTXT, LINE, JAT )

               ELSE
                  CALL CHR_PUTC( AST_FORMAT( FRM, 1, ARRAY( IX, IY ), 
     :                                       STATUS ), LINE, JAT )
               END IF
               IAT = IAT + VWID
            END DO

            CALL KPG1_REPRT( LINE( : JAT ), QUIET, LOG, FD, STATUS )

         END DO

*  Report an error for any unknown file format.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'F', FORMAT )
         CALL ERR_REP( 'KPS1_LOOK_ERR1', 'KPS1_LOOK: Unknown file '//
     :                 'format ''^F'' (programming error).', STATUS )
      END IF

*  Return the top right value.
      VALUE = ARRAY( XHI, YHI )

      END
