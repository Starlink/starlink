      SUBROUTINE NCRAXS( XLOW, YLOW, XHIGH, YHIGH, TITLE, ABSLAB,
     :                   ORDLAB, MINTIC, MAJTIC, OUTTIC, THICK, NICE,
     :                   STATUS )
*+
*  Name:
*     NCRAXS

*  Purpose:
*     Draws NCAR annotated axes in the current SGS zone.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NCRAXS( XLOW, YLOW, XHIGH, YHIGH, TITLE, ABSLAB, ORDLAB,

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This subroutine uses NCAR and SNX to plot labelled and
*     annotated axes with a title within the current SGS zone.

*     NCAR has unfortunate defaults for many of its plotting parameters.
*     These have been changed to obtain a clearer plot on a variety of
*     devices. The changes are:
*       a) increase the width of the enumerations, labels and title;
*       b) disable the rotation and squashing of the axis ticks;
*       c) increase the size of axis tick marks; and
*       d) increase the maximum label length to 50 characters.

*     There are options controlled by input arguments:
*       e) plot the tick marks outside the grid so they do not cross
*          the graph (useful for contour plots);
*       f) start and end the axes at the start and end pixels rather
*          than at major tick marks (also useful for contour plots);
*       g) control the maximum number of major tick marks to prevent
*          shrinkage,
*       h) control the number of minor ticks per major tick;
*       i) plot the tick marks inside the grid (default is outside); and
*       j) plot with thick lines

*     Also the terminator of character strings is a dollar.  This is
*     changed to a <NUL>.

*     Note before using the routine an SGS device must be opened, a call
*     to SNX_AGWV made and an SGS zone selected or created. The zone
*     should have the aspect ratio of the sub-array (of data values that
*     are to be contoured) to get square pixels.

*  Algorithm:
*     If no error on entry then return
*     Store input values of all AUTOGRAPH parameters to be changed.
*     Reset AUTOGRAPH parameters as required
*     Draw labelled ( abslab, ordlab, title ) and enumerated axes
*     Flush AUTOGRAPH buffers
*     Restore altered AUTOGRAPH parameters to their values on input to
*       this routine
*     Return

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     Malcolm J. Currie  STARLINK (RAL::CUR)
*     {enter_new_authors_here}

*  History:
*     1990 Oct 4  : Original based on CNTAXS (RAL::CUR).
*     1991 Apr 5  : Allow for reversed axes. (RAL::CUR).
*     1991 Aug 22 : Change terminator to null to permit dollars in the
*        plot title or axis labels. (RAL::CUR).
*     1995 Nov 9  : Set the dummy point to be large and negative thus
*        will not appear as a spot in plots (RAL::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed


*  Global Constants:
      INCLUDE 'SAE_PAR'        ! global SSE definitions


*  Arguments Given:
      REAL
     :  XLOW,
     :  XHIGH,
     :  YLOW,
     :  YHIGH

      REAL
     :  THICK,
     :  MINTIC( 2 ),
     :  MAJTIC( 2 )

      LOGICAL
     :  OUTTIC,
     :  NICE

      CHARACTER*(*)
     :  ABSLAB,
     :  ORDLAB,
     :  TITLE


*  Status:
      INTEGER STATUS

*  External References:

      INTEGER CHR_LEN          ! String length ignoring trailing blanks


*  Local Variables:
      REAL
     :  XD( 2 ),               ! Dummy x co-ordinates
     :  YD( 2 ),               ! Dummy y co-ordinates
                               ! Storage for input AUTOGRAPH parameter
                               ! in order of appearance:
     :  XORDER,                ! x-axis polarity
     :  YORDER,                ! y-axis polarity
     :  CNTROL( 4 ),           ! Input AUTOGRAPH grid parameters
     :  MAJCOU( 4 ),           ! "number" of major tick marks
     :  MINCOU( 4 ),           ! "number" of minor tick marks
     :  NICEA( 2 ),            ! Nice axes
     :  OMITIC( 4 ),           ! Outward-minor-tick-mark sizes
     :  OMATIC( 4 ),           ! Outward-major-tick-mark sizes
     :  IMITIC( 4 ),           ! Inward-minor-tick-mark sizes
     :  IMATIC( 4 ),           ! Inward-major-tick-mark sizes
     :  LIMITS( 4 ),           ! x,y axis maxima and minima
     :  NLOFF( 2 ),            ! Bottom and left numeric-label offsets
     :  NLW( 2 ),              ! Bottom and left numeric-label widths
     :  LINMAX,                ! Maximum number of characters plotted in
                               ! a label
     :  LABW( 3 )              ! Bottom, left and top label widths

      CHARACTER * 1
     :  TERMST                 ! Default string terminator.

      LOGICAL                  ! True if:
     :  XNEG,                  ! X axis is inverted
     :  YNEG                   ! Y axis is inverted

      INTEGER
     :  LINWDT                 ! NCAR default line width


*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Store dummy data at location very unlikely to be needed for actual
*    data.  If the location of the axes is known, use the lower bounds.
      IF ( NICE ) THEN
         XD( 1 ) = XHIGH + 0.5 * ( XHIGH - XLOW )
         YD( 1 ) = YHIGH + 0.5 * ( YHIGH - YLOW )
      ELSE
         XD( 1 ) = XHIGH
         YD( 1 ) = YHIGH
      END IF

*    Plot graph with AUTOGRAPH

      CALL SNX_AGCS

*    Store current nice values

      IF ( .NOT. NICE ) THEN
         CALL AGGETF( 'X/NICE.', NICEA(1) )
         CALL AGGETF( 'Y/NICE.', NICEA(2) )

*       Do not necessarily want major tick marks at vertices.

         CALL AGSETF( 'X/NICE.', 0. )
         CALL AGSETF( 'Y/NICE.', 0. )
      END IF

*    Determine wether or not either axis is inverted.

      XNEG = XHIGH .LT. XLOW
      YNEG = YHIGH .LT. YLOW

*    Record and modify the x axis polarity.  1.0 means it increases from
*    right to left.

      IF ( XNEG ) THEN
         CALL AGGETF( 'X/ORDER.', XORDER )
         CALL AGSETF( 'X/ORDER.', 1.0 )
      END IF

*    Record and modify the y axis polarity.  1.0 means it increases from
*    top to bottom.

      IF ( YNEG ) THEN
         CALL AGGETF( 'Y/ORDER.', YORDER )
         CALL AGSETF( 'Y/ORDER.', 1.0 )
      END IF

*    Store current axis control values.

      CALL AGGETF( 'AXIS/LEFT/CONTROL.', CNTROL(2) )
      CALL AGGETF( 'AXIS/BOTTOM/CONTROL.', CNTROL(1) )
      CALL AGGETF( 'AXIS/RIGHT/CONTROL.', CNTROL(4) )
      CALL AGGETF( 'AXIS/TOP/CONTROL.', CNTROL(3) )

*    Disable squashing and rotating of the ticks.

      CALL AGSETF( 'AXIS/BOTTOM/CONTROL.', 1. )
      CALL AGSETF( 'AXIS/TOP/CONTROL.', 1. )

*    Disable rotating of the ticks.

      CALL AGSETF( 'AXIS/LEFT/CONTROL.', 2. )
      CALL AGSETF( 'AXIS/RIGHT/CONTROL.', 2. )

*    Store current extrema

      CALL AGGETF( 'X/MINIMUM.', LIMITS(1) )
      CALL AGGETF( 'X/MAXIMUM.', LIMITS(2) )
      CALL AGGETF( 'Y/MINIMUM.', LIMITS(3) )
      CALL AGGETF( 'Y/MAXIMUM.', LIMITS(4) )

*    Set axis limits because a dummy point is to be plotted.  Allow for
*    inverted axes.

      CALL AGSETF( 'X/MINIMUM.', MIN( XLOW, XHIGH ) )
      CALL AGSETF( 'X/MAXIMUM.', MAX( XLOW, XHIGH ) )
      CALL AGSETF( 'Y/MINIMUM.', MIN( YLOW, YHIGH ) )
      CALL AGSETF( 'Y/MAXIMUM.', MAX( YLOW, YHIGH ) )

*    Store current major-ticks parameters

      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(1) )
      CALL AGGETF( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(2) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(3) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(4) )

*    Get requested number of major tick marks

      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(1) )
      CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(2) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(1) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(2) )

*    Store current minor-ticks parameters and

      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', MINCOU(1) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', MINCOU(3) )
      CALL AGGETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', MINCOU(2) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', MINCOU(4) )

*    Set the requested number of minor tick marks per major tick

      IF ( MINTIC( 1 ) .GT. 0.999999 ) THEN
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', MINTIC(1) )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', MINTIC(1) )
      END IF
      IF ( MINTIC( 2 ) .GT. 0.999999 ) THEN
         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', MINTIC(2) )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', MINTIC(2) )
      END IF

      IF ( OUTTIC ) THEN

*       Store current outward tick mark sizes

         CALL AGGETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(2) )
         CALL AGGETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(4) )
         CALL AGGETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(3) )
         CALL AGGETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(1) )

         CALL AGGETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(2) )
         CALL AGGETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(4) )
         CALL AGGETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(3) )
         CALL AGGETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(1) )

*       Set outward-pointing axis tick marks so they do no intersect the
*       contours

         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
      END IF

*    Store current inward tick mark sizes

      CALL AGGETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(2) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(4) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', IMITIC(3) )
      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', IMITIC(1) )

      CALL AGGETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(2) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(4) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(3) )
      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(1) )

      IF ( OUTTIC ) THEN

*       Remove the inward-pointing tick marks because only outward are
*       required

         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', 0.0 )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
      ELSE

*       Lengthen the interior tick marks

         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', 0.012 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', 0.012 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', 0.012 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', 0.012 )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', 0.02 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', 0.02 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', 0.02 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', 0.02 )
      END IF

      IF ( OUTTIC ) THEN

*       Store numeric-label offsets

         CALL AGGETF( 'AXIS/LEFT/OFFSET.', NLOFF(2) )
         CALL AGGETF( 'AXIS/BOTTOM/OFFSET.', NLOFF(1) )

*       As a by-product, need to shift numeric labels outward

         CALL AGSETF( 'AXIS/LEFT/OFFSET.', 0.02 )
         CALL AGSETF( 'AXIS/BOTTOM/OFFSET.', 0.02 )
      END IF

*    Store the axis annotation sizes

      CALL AGGETF( 'AXIS/LEFT/NUMERIC/WIDTH.', NLW(2) )
      CALL AGGETF( 'AXIS/BOTTOM/NUMERIC/WIDTH.', NLW(1) )

*    Store the maximum label length in characters.

      CALL AGGETF( 'LINE/MAXIMUM.', LINMAX )

*    Set maximum label length to 50 characters.

      CALL AGSETF( 'LINE/MAXIMUM.', 50.0 )

*    Store and reset the character terminator string.

      CALL AGGETC( 'LINE/END.', TERMST )
      CALL AGSETC( 'LINE/END.', CHAR( 0 ) )

*    Now the store the labels

      CALL AGSETC( 'LABEL/NAME.', 'L' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGGETF( 'LINE/CHARACTER.', LABW(2) )

      CALL AGSETC( 'LABEL/NAME.', 'B' )
      CALL AGSETI( 'LINE/NUMBER.', -100 )
      CALL AGGETF( 'LINE/CHARACTER.', LABW(1) )

      CALL AGSETC( 'LABEL/NAME.', 'T' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGGETF( 'LINE/CHARACTER.', LABW(3) )


*    Make the axis annotations larger
*    First the numbers

      CALL AGSETF( 'AXIS/LEFT/NUMERIC/WIDTH.', 0.025 )
      CALL AGSETF( 'AXIS/BOTTOM/NUMERIC/WIDTH.', 0.025 )

*    Now the labels

      CALL AGSETC( 'LABEL/NAME.', 'L' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', 0.03 )

      CALL AGSETC( 'LABEL/NAME.', 'B' )
      CALL AGSETI( 'LINE/NUMBER.', -100 )
      CALL AGSETF( 'LINE/CHARACTER.', 0.03 )

      CALL AGSETC( 'LABEL/NAME.', 'T' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', 0.036 )

*    Store and reset the line width

      IF ( THICK .GT. 0.000999 ) THEN
         CALL GETUSV( 'LW', LINWDT )
         CALL SETUSV( 'LW', NINT( THICK*1000 ) )
      END IF

*    Draw labelled axes

      CALL SNX_EZRXY( XD, YD, 1, ABSLAB( :CHR_LEN( ABSLAB ) ),
     :                ORDLAB( :CHR_LEN( ORDLAB ) ),
     :                TITLE( :CHR_LEN( TITLE ) ) )

*    Flush the NCAR buffers.

      CALL PLOTIT( 0, 0, 2 )

*    Restore input parameters which have been altered.

*    axis nice values

      IF ( .NOT. NICE ) THEN
         CALL AGSETF( 'X/NICE.', NICEA(1) )
         CALL AGSETF( 'Y/NICE.', NICEA(2) )
      END IF

*    x-axis polarity

      IF ( XNEG ) CALL AGSETF( 'X/ORDER.', XORDER )

*    y-axis polarity

      IF ( YNEG ) CALL AGSETF( 'Y/ORDER.', YORDER )

*    axis control values.

      CALL AGSETF( 'AXIS/LEFT/CONTROL.', CNTROL(2) )
      CALL AGSETF( 'AXIS/BOTTOM/CONTROL.', CNTROL(1) )
      CALL AGSETF( 'AXIS/RIGHT/CONTROL.', CNTROL(4) )
      CALL AGSETF( 'AXIS/TOP/CONTROL.', CNTROL(3) )

*    extrema

      CALL AGSETF( 'X/MINIMUM.', LIMITS(1) )
      CALL AGSETF( 'X/MAXIMUM.', LIMITS(2) )
      CALL AGSETF( 'Y/MINIMUM.', LIMITS(3) )
      CALL AGSETF( 'Y/MAXIMUM.', LIMITS(4) )

*    major-ticks parameters

      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(1) )
      CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(2) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(3) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(4) )

*    minor-ticks parameters

      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', MINCOU(1) )
      CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', MINCOU(2) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', MINCOU(3) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', MINCOU(4) )

      IF ( OUTTIC ) THEN

*       outward tick mark sizes

         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(2) )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(4) )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(3) )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(1) )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(2) )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(4) )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(3) )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(1) )
      END IF

*    inward tick mark sizes

      CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(2) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(4) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', IMITIC(3) )
      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', IMITIC(1) )

      CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(2) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(4) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(3) )
      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(1) )

      IF ( OUTTIC ) THEN

*       numeric-label offsets

         CALL AGSETF( 'AXIS/LEFT/OFFSET.', NLOFF(2) )
         CALL AGSETF( 'AXIS/BOTTOM/OFFSET.', NLOFF(1) )
      END IF

*    axis annotation sizes

      CALL AGSETF( 'AXIS/LEFT/NUMERIC/WIDTH.', NLW(2) )
      CALL AGSETF( 'AXIS/BOTTOM/NUMERIC/WIDTH.', NLW(1) )

*    Maximum label length

      CALL AGSETF( 'LINE/MAXIMUM.', LINMAX )

*    The character terminator string.

      CALL AGSETC( 'LINE/END.', TERMST )

*    Now the store the labels

      CALL AGSETC( 'LABEL/NAME.', 'L' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', LABW(2) )

      CALL AGSETC( 'LABEL/NAME.', 'B' )
      CALL AGSETI( 'LINE/NUMBER.', -100 )
      CALL AGSETF( 'LINE/CHARACTER.', LABW(1) )

      CALL AGSETC( 'LABEL/NAME.', 'T' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', LABW(3) )

*    Store and reset the line width

      IF ( THICK .GT. 0.000999 ) THEN
         CALL SETUSV( 'LW', LINWDT )
      END IF

      END
