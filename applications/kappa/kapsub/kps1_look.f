      SUBROUTINE KPS1_LOOK( FRM, XLO, XHI, YLO, YHI, ARRAY, QUIET, LOG,
     :                      FD, LINE, IWCS, FORMAT, MAXLEN, VALUE,
     :                      STATUS )
*+
*  Name:
*     KPS1_LOOK

*  Purpose:
*     Write the contents of an array to a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LOOK( FRM, XLO, XHI, YLO, YHI, ARRAY, QUIET, LOG, FD, LINE,
*                     IWCS, FORMAT, MAXLEN, LINE, VALUE, STATUS )

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
*     IWCS = INTEGER (Given)
*        The WCS FrameSet from the NDF.
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
*        - "WLIST" -- Each row of textual output consists of the WCS
*        co-ords of the pixel, followed by the pixel data value. No
*        headers or blank lines are included. The pixels are listed in
*        "fortran order" - the lower left pixel first, and the upper
*        right pixel last.
*
*        - "CGLIST" -- Like CLIST except bad pixels are omitted.
*
*        - "WGLIST" -- Like WLIST except bad pixels are omitted.
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
*        In all cases, adjacent values are separated by spaces, and bad
*        pixel values are represented by the string "BAD". Values equal
*        to (VAL__MAXD - 1) are represented by the string "OUT".
*     MAXLEN = INTEGER (Given)
*        The maximum length allowed for a line of textual output.
*     VALUE = DOUBLE PRECISION (Returned)
*        The value stored in ARRAY(XHI,YHI).
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-OCT-2001 (DSB):
*        Original version.
*     4-APR-2006 (DSB):
*        Added CGList format.
*     5-MAY-2009 (DSB):
*        Added WList format.
*     11-NOV-2009 (DSB):
*        Fix formatting of axis and data values in WLIST mode.
*     5-MAY-2011 (DSB):
*        Added WGList format.
*     2-SEP-2011 (DSB):
*        Fix VLIST formating of BAD and OUT values.
*     14-OCT-2016 (DSB):
*        X and Y field widths in WLIST and WGLIST format were totally
*        wrong. They assumes the current Frame was a SkyFrame, which it
*        may not be. It was really wrong if the current Frame was in fact
*        the PIXEL Frame - in that it gave a max field width of 3. They
*        now just use twice the value of the axis Digits attribute.
*     {enter_further_changes_here}

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
      INTEGER IWCS
      CHARACTER FORMAT*(*)
      INTEGER MAXLEN

*  Arguments Returned:
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS           ! Global status

*  External References:
      INTEGER CHR_LEN          ! Used length of a string

*  Local Constants:
      INTEGER MXSTOR           ! No. of WCS positions to transform
      PARAMETER( MXSTOR = 100 )

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
      CHARACTER CC*40          ! Buffer for formatted value
      DOUBLE PRECISION ASTORE( MXSTOR ) ! Stored WCS axis 1 values
      DOUBLE PRECISION B       ! Value for second WCS axis
      DOUBLE PRECISION BSTORE( MXSTOR ) ! Stored WCS axis 2 values
      DOUBLE PRECISION POS( 2 )! Normalised WCS position
      DOUBLE PRECISION XSTORE( MXSTOR ) ! Stored pixel axis 1 values
      DOUBLE PRECISION YSTORE( MXSTOR ) ! Stored pixel axis 2 values
      DOUBLE PRECISION VSTORE( MXSTOR ) ! Stored data values
      INTEGER COLWID           ! Max field width for a column in a "strip"
      INTEGER I                ! Loop index for stored positions
      INTEGER IAT              ! Length of string
      INTEGER IPIX             ! Index of PIXEL Frame in WCS FrameSet
      INTEGER IX               ! X pixel index
      INTEGER IX0              ! X pixel index of left most column in strip
      INTEGER IY               ! Y pixel index
      INTEGER JAT              ! Length of string
      INTEGER MAP              ! PIXEL-> WCS Mapping
      INTEGER NV               ! Max number of columns in a strip
      INTEGER STORED           ! No. of positions in store arrays
      INTEGER VWID             ! Max field width for a data value
      INTEGER XWID             ! Max field width for an X pixel index
      INTEGER YWID             ! Max field width for a Y pixel index
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  For WLIST and WGLIST, get the Mapping from PIXEL to WCS Frame.
      IF( FORMAT .EQ. 'WLIST' .OR. FORMAT .EQ. 'WGLIST' ) THEN
         CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )
         MAP = AST_GETMAPPING( IWCS, IPIX, AST__CURRENT, STATUS )
      END IF

*  For WLIST, WGLIST, CLIST and CGLIST format, assume the maximum possible
*  field width.
      IF( FORMAT .EQ. 'WLIST' .OR. FORMAT .EQ. 'WGLIST' .OR.
     :    FORMAT .EQ. 'CLIST' .OR. FORMAT .EQ. 'CGLIST' ) THEN
         VWID = VAL__SZD

*  For other formats, format every value, using CHR to get the most compact
*  format. Find the maximum field width needed to format any value.
      ELSE
         VWID = 0
         DO IY = YLO, YHI
            DO IX = XLO, XHI
               IF( ARRAY( IX, IY ) .EQ. VAL__BADD ) THEN
                  JAT = BADLEN
               ELSE IF( ARRAY( IX, IY ) .EQ. OUTVAL ) THEN
                  JAT = OUTLEN
               ELSE
                  CALL CHR_CTOC( AST_FORMAT( FRM, 1, ARRAY( IX, IY ),
     :                                       STATUS ), LINE, JAT )
               END IF
               VWID = MAX( VWID, JAT )
            END DO
         END DO
      END IF

*  Find the maximum field width for a pixel index or WCS coord value.
      IF( FORMAT .EQ. 'WLIST' .OR. FORMAT .EQ. 'WGLIST' ) THEN
         XWID = 2*AST_GETI( IWCS, 'Digits(1)', STATUS )
         YWID = 2*AST_GETI( IWCS, 'Digits(2)', STATUS )

      ELSE
         CALL CHR_ITOC( XLO, LINE, IAT )
         XWID = IAT

         CALL CHR_ITOC( XHI, LINE, IAT )
         XWID = MAX( XWID, IAT )

         CALL CHR_ITOC( YLO, LINE, IAT )
         YWID = IAT

         CALL CHR_ITOC( YHI, LINE, IAT )
         YWID = MAX( YWID, IAT )
      END IF

*  Add two extra spaces onto all field widths.
      VWID = VWID + 2
      XWID = XWID + 2
      YWID = YWID + 2

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

*  "WLIST" & "WGLIST": Each row of textual output consists of the WCS coords,
*  followed by the pixel data value. No headers or blank lines are included.
*  The pixels are listed in "fortran order" - the lower left pixel first,
*  and the upper right pixel last. All columns left justified. WGLIST
*  skips bad data values.
      ELSE IF( FORMAT .EQ. 'WLIST' .OR. FORMAT .EQ. 'WGLIST' ) THEN

*  Transforming every required pixel position into WCS using a separate
*  call to AST_TRAN2 would be very inefficient. So we collect a group of
*  positions together, and transform them all using a single call to
*  AST_TRAN2. Initialise the number of positions ready to transform.
         STORED = 0

*  Loop round every pixel.
         DO IY = YLO, YHI
            DO IX = XLO, XHI

*  If mode is WGLIST, check the data value is good.
               IF( FORMAT .EQ. 'WLIST' .OR.
     :             ARRAY( IX, IY ) .NE. VAL__BADD ) THEN

*  Add these pixel centre positions to the store of positions to be
*  transformed.
                  STORED = STORED + 1
                  XSTORE( STORED ) = DBLE( IX ) - 0.5D0
                  YSTORE( STORED ) = DBLE( IY ) - 0.5D0
                  VSTORE( STORED ) = ARRAY( IX, IY )

*  If the store is now full, or if this is the last pixel, transform the
*  stored pixel positions into WCS positions.
                  IF( STORED .EQ. MXSTOR .OR.
     :                ( IX .EQ. XHI .AND. IY .EQ. YHI ) ) THEN

                     CALL AST_TRAN2( MAP, STORED, XSTORE, YSTORE,
     :                               .TRUE., ASTORE, BSTORE, STATUS )

*  Loop round displaying each stored WCS position and the corresponding
*  pixel value.
                     DO I = 1, STORED
                        LINE = ' '
                        IAT = 0

                        POS( 1 ) = ASTORE( I )
                        POS( 2 ) = BSTORE( I )
                        CALL AST_NORM( IWCS, POS, STATUS )

                        CC = AST_FORMAT( IWCS, 1, POS( 1 ), STATUS )
                        CC( XWID: ) = ' '
                        CALL CHR_APPND( CC, LINE, IAT )

                        IAT = XWID
                        JAT = IAT

                        CC = AST_FORMAT( IWCS, 2, POS( 2 ), STATUS )
                        CC( YWID: ) = ' '
                        CALL CHR_APPND( CC, LINE, IAT )

                        IAT = JAT + YWID

                        IF( VSTORE( I ) .EQ. VAL__BADD ) THEN
                           CALL CHR_PUTC( BADTXT, LINE, IAT )

                        ELSE IF( VSTORE( I ) .EQ. OUTVAL ) THEN
                           CALL CHR_PUTC( OUTTXT, LINE, IAT )

                        ELSE
                           CALL CHR_PUTC( AST_FORMAT( FRM, 1,
     :                                                VSTORE( I ),
     :                                                STATUS ),
     :                                    LINE, IAT )
                        END IF

                        CALL KPG1_REPRT( LINE( : IAT ), QUIET, LOG, FD,
     :                                   STATUS )

                     END DO

*  Indicate no positions are ready to be transformed.
                     STORED = 0

                  END IF
               END IF
            END DO
         END DO

*  Do any remaining positions.
         IF( STORED .GT. 0 ) THEN

            CALL AST_TRAN2( MAP, STORED, XSTORE, YSTORE, .TRUE., ASTORE,
     :                      BSTORE, STATUS )

            DO I = 1, STORED
               LINE = ' '
               IAT = 0

               POS( 1 ) = ASTORE( I )
               POS( 2 ) = BSTORE( I )
               CALL AST_NORM( IWCS, POS, STATUS )

               CC = AST_FORMAT( IWCS, 1, POS( 1 ), STATUS )
               CC( XWID: ) = ' '
               CALL CHR_APPND( CC, LINE, IAT )

               IAT = XWID
               JAT = IAT

               CC = AST_FORMAT( IWCS, 2, POS( 2 ), STATUS )
               CC( YWID: ) = ' '
               CALL CHR_APPND( CC, LINE, IAT )

               IAT = JAT + YWID

               IF( VSTORE( I ) .EQ. VAL__BADD ) THEN
                  CALL CHR_PUTC( BADTXT, LINE, IAT )

               ELSE IF( VSTORE( I ) .EQ. OUTVAL ) THEN
                  CALL CHR_PUTC( OUTTXT, LINE, IAT )

               ELSE
                  CALL CHR_PUTC( AST_FORMAT( FRM, 1,
     :                                       VSTORE( I ),
     :                                       STATUS ),
     :                           LINE, IAT )
               END IF

               CALL KPG1_REPRT( LINE( : IAT ), QUIET, LOG, FD,
     :                          STATUS )
            END DO
         END IF

*  "VLIST":  Each row of textual output consists of just the pixel data value.
*  No headers or blank lines are included. The pixels are listed in "fortran
*  order" - the lower left pixel first, and the upper right pixel last.
      ELSE IF( FORMAT .EQ. 'VLIST' ) THEN
         DO IY = YLO, YHI
            DO IX = XLO, XHI
               IF( ARRAY( IX, IY ) .EQ. VAL__BADD ) THEN
                  CALL CHR_CTOC( BADTXT, LINE, JAT )

               ELSE IF( ARRAY( IX, IY ) .EQ. OUTVAL ) THEN
                  CALL CHR_CTOC( OUTTXT, LINE, JAT )

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
