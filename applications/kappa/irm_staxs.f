      SUBROUTINE IRM_STAXS( XBMT, YLFT, XLAB, YLAB, LABSZE, MAJTIC, 
     :                      MINTIC, TSZE, OUTIC, STATUS )
*+
*  Name:
*     IRM_STAXS

*  Purpose:
*     Set the axes attributes of a NCAR graph.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_STAXS( XBMT, YLFT, XLAB, YLAB, LABSZE, MAJTIC, MINTIC, 
*                     TCKSZE, OUTIC, STATUS )

*  Description:
*     This routine decides the attributes of four axes, the labels
*     of the axes, the number of major and minor ticks of axes, and
*     the dirction of ticks. Calling routine can select either left
*     or right edge of grid window as y axis, and either bottom or
*     top edge of grid window as x axis. The edges selected as axes
*     will have specified labels and tick marks associating with them.
*     The edges which are not selected will only have a line to be
*     drawn.  

*  Arguments:
*     XBMT = LOGICAL (Given)
*        It is true, if bottom edge of grid window is to be selected
*        as x axis. Otherwise, top edge as x axis.
*     YLFT = LOGICAL (Given)
*        It is true, if left edge of grid window is to be selected as
*        y axis. Otherwise, right edge as y axis.
*     XLAB = CHARACTER*(*) (Given)
*        Label associated with X axis.
*     YLAB = CHARACTER*(*) (Given)
*        Label associated with Y axis.
*     LABSZE = REAL (Given)
*        The character width of the axes labels, stated as a fraction 
*        of the smaller dimension of the grid window. The value less
*        than or equal to zero will set the character as NCAR default. 
*        The character width of the title of the plot will be 
*        1.2 * LABSZE. The numeric labels of the axes will text width
*        0.8 * LABSZE for its mantissa, and 0.55 * LABSZE for its
*        exponent. 
*     MAJTIC( 2 ) = REAL (Given)
*        The number of major ticks on X and Y axes, respectively.
*        Number used is between MAJTIC*2 and 5*MAJTIC/2+4. A negative
*        value forces the NCAR to compute appropriate values.
*     MINTIC( 2 ) = REAL (Given)
*        The number of minor tick marks between each major tick mark for
*        the x and y axes. A negative value forces the NCAR to compute 
*        appropriate values. A value less than 1.0 but greater than 0.0
*        will suppresses minor tick completely.
*     TSZE = REAL (Given)
*        The length of the major tick marks, stated as a fraction of the
*        smaller dimension of the grid window. Its value should be within
*        the range ( 0, 0.05 ). The value outsid this range means using 
*        NCAR default. The minor tick marks will always have length of
*        2/3 length of major tick marks.
*     OUTIC = LOGICAL (Given)
*        If true the axis tick marks are drawn outside the box.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     11-JAN-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL XBMT
      LOGICAL YLFT
      LOGICAL OUTIC

      CHARACTER*(*) XLAB
      CHARACTER*(*) YLAB
      REAL LABSZE
      
      REAL MAJTIC( 2 )
      REAL MINTIC( 2 )
      REAL TSZE

*  Status:
      INTEGER STATUS             ! Global status

*  External Reference:
      INTEGER CHR_LEN

*  Local Variables:
      REAL NULL1                 ! 'Null 1' of NCAR special value

      INTEGER XLBLN              ! Used length of x label
      INTEGER YLBLN              ! Used length of y label

      REAL MXLBLN                ! Max. label length setting
      REAL MXLNXY                ! Max. length of x label and y label

      CHARACTER*1 ENDMRK         ! End mark of label string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the used length of x label and y label.
      XLBLN = CHR_LEN( XLAB )
      YLBLN = CHR_LEN( YLAB )

*  If either of x label or y label is not a blank string. set label 
*  control parameter to enable labels.
      CALL AGSETF( 'LABEL/CONTROL.', 2. )

*  Get the max. label length of present setting
      CALL AGGETF( 'LINE/MAXIMUM.', MXLBLN )

*  If the length of x label or the length of y label exceeds the max.
*  label length of present setting, reset the max. label length.
      MXLNXY = REAL( MAX( XLBLN, YLBLN ) )
      IF (MXLNXY .GT. MXLBLN ) THEN
        CALL AGSETF( 'LINE/MAXIMUM.', MXLNXY )
        MXLBLN = MXLNXY
      END IF

*  Get the line end mark.
      CALL AGGETC( 'LINE/END.', ENDMRK )

*  Append the label whose length less than the max. label length with
*  line end mark.
      IF ( REAL( XLBLN ) .LT. MXLBLN ) THEN
         CALL CHR_APPND( ENDMRK, XLAB, XLBLN )
      END IF
      IF ( REAL( YLBLN ) .LT. MXLBLN ) THEN
         CALL CHR_APPND( ENDMRK, YLAB, YLBLN )
      END IF

*  Get the NULL/1 value of present NCAR setting.
      CALL AGGETF( 'NULL/1.', NULL1 )

*  Check if default tick mark length will be used.
      IF ( TSZE .LT. 0.0 .OR. TSZE .GT. 0.05 ) TSZE = 0.015

*  If outward tick marks is required, set the length of outward tick
*  marks as non-zero, and inward tick marks as zero.
      IF ( OUTIC ) THEN
         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/OUTWARD.', TSZE )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/OUTWARD.', TSZE )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/OUTWARD.', TSZE )      
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/OUTWARD.', TSZE )
      
         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
      
         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/OUTWARD.', 
     :                 0.667 * TSZE )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/OUTWARD.', 
     :                 0.667 * TSZE )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/OUTWARD.', 
     :                 0.667 * TSZE )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/OUTWARD.', 
     :                 0.667 * TSZE )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
       
*  Otherwise, set inward tick mark length as specified.
       ELSE
         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', TSZE )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', TSZE )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', TSZE )      
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', TSZE )
      
         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/OUTWARD.', 0.0 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/OUTWARD.', 0.0 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/OUTWARD.', 0.0 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/OUTWARD.', 0.0 )
      
         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', 
     :                 0.667 * TSZE )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', 
     :                 0.667 * TSZE )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', 
     :                 0.667 * TSZE )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', 
     :                 0.667 * TSZE )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/OUTWARD.', 0.0 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/OUTWARD.', 0.0 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/OUTWARD.', 0.0 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/OUTWARD.', 0.0 )
       END IF

*  Check if the default NCAR label size will be used.
       IF ( LABSZE .LE. 0.0 ) LABSZE = 0.015

*  If MINTIC is negative, set it as 'null 1' of NCAR to let the
*  AUTOGRAPH chooses appropriate values.
       IF ( MINTIC( 1 ) .LT. 0.0 ) MINTIC( 1 ) = NULL1
       IF ( MINTIC( 2 ) .LT. 0.0 ) MINTIC( 2 ) = NULL1
       
*  If bottom edge is selected as x axis. Set the parameters associated
*  with the bottom edge and suppress parameters with the top edge.
      IF ( XBMT ) THEN

*  Set x label for bottom edge.
         CALL AGSETC( 'LABEL/NAME.', 'B' )
         CALL AGSETI( 'LINE/NUMBER.', -100 )
         CALL AGSETF( 'LINE/CHARACTER.', LABSZE )
         CALL AGSETC( 'LINE/TEXT.', XLAB )

*  Set major tick number for bottom edge.
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.',
     :                 MAJTIC( 1 ) )

*  Set minor tick number for bottom edge.
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', MINTIC( 1 ) )

*  Set the character width for the numeric label along this edge.
         CALL AGSETF( 'AXIS/BOTTOM/NUMERIC/WIDTH/MANTISSA.',
     :                 0.8 * LABSZE )
         CALL AGSETF( 'AXIS/BOTTOM/NUMERIC/WIDTH/EXPONENT.',
     :                 0.55 * LABSZE )
  
*  Set control parameter for top edge so that only a line will be drawn
*  for this edge.
         CALL AGSETF( 'AXIS/TOP/CONTROL.', -1.0 )

*  Otherwise top edge is selected as x axis.
      ELSE

*  Set x label for top edge.
         CALL AGSETC( 'LABEL/NAME.', 'T' )
         CALL AGSETI( 'LINE/NUMBER.', 90 )
         CALL AGSETF( 'LINE/CHARACTER.', LABSZE )
         CALL AGSETC( 'LINE/TEXT.', XLAB )

*  Set the default bottom edge label as blank.
         CALL AGSETC( 'LABEL/NAME.', 'B' )
         CALL AGSETI( 'LINE/NUMBER.', -100 )
         CALL AGSETC( 'LINE/TEXT.', ' ' )

*  Turn on the numeric label for top edge.
         CALL AGSETF( 'TOP/TYPE.', NULL1 )

*  Set major tick number for top edge.
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.',
     :                 MAJTIC( 1 ) )

*  Set minor tick number for top edge.
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', MINTIC( 1 ) )

*  Set the character width for the numeric label along this edge.
         CALL AGSETF( 'AXIS/TOP/NUMERIC/WIDTH/MANTISSA.',
     :                 0.8 * LABSZE )
         CALL AGSETF( 'AXIS/TOP/NUMERIC/WIDTH/EXPONENT.',
     :                 0.55 * LABSZE )
  
*  Set control parameter for bottom edge so that only a line will be
*  drawn for this edge.
         CALL AGSETF( 'AXIS/BOTTOM/CONTROL.', -1.0 )

      END IF

*  If left edge is selected as y axis. Set the parameters associated
*  with the bottom edge and suppress parameters with the top edge.
      IF ( YLFT ) THEN

*  Set y label for left edge.
         CALL AGSETC( 'LABEL/NAME.', 'L' )
         CALL AGSETI( 'LINE/NUMBER.', 100 )
         CALL AGSETF( 'LINE/CHARACTER.', LABSZE )
         CALL AGSETC( 'LINE/TEXT.', YLAB )

*  Set major tick number for left edge.
         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.',
     :                 MAJTIC( 2 ) )

*  Set minor tick number for left edge.
         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', MINTIC( 2 ) )

*  Set the character width for the numeric label along this edge.
         CALL AGSETF( 'AXIS/LEFT/NUMERIC/WIDTH/MANTISSA.',
     :                 0.8 * LABSZE )
         CALL AGSETF( 'AXIS/LEFT/NUMERIC/WIDTH/EXPONENT.',
     :                 0.55 * LABSZE )
  
*  Set control parameter for right edge so that only a line will be
*  drawn for this edge.
         CALL AGSETF( 'AXIS/RIGHT/CONTROL.', -1.0 )

*  Otherwise right edge is selected as x axis.
      ELSE

*  Set y label for right edge.
         CALL AGSETC( 'LABEL/NAME.', 'R' )
         CALL AGSETI( 'LINE/NUMBER.', -100 )
         CALL AGSETF( 'LINE/CHARACTER.', LABSZE )
         CALL AGSETC( 'LINE/TEXT.', YLAB )

*  Set label of left edge as blank. 
         CALL AGSETC( 'LABEL/NAME.', 'L' )
         CALL AGSETI( 'LINE/NUMBER.', 100 )
         CALL AGSETC( 'LINE/TEXT.', ' ' )


*  Turn on the numeric label of right edge.
         CALL AGSETF( 'RIGHT/TYPE.', NULL1 )

*  Set major tick number for right edge.
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.',
     :                 MAJTIC( 2 ) )

*  Set minor tick number for right edge.
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', MINTIC( 2 ) )

*  Set the character width for the numeric label along this edge.
         CALL AGSETF( 'AXIS/RIGHT/NUMERIC/WIDTH/MANTISSA.',
     :                 0.8 * LABSZE )
         CALL AGSETF( 'AXIS/RIGHT/NUMERIC/WIDTH/EXPONENT.',
     :                 0.55 * LABSZE )
  
*  Set control parameter for left edge so that only a line will be
*  drawn for this edge.
         CALL AGSETF( 'AXIS/LEFT/CONTROL.', -1.0 )

      END IF

*  Set the character size for graph title of the display drawn by
*  the NCAR (AUTOGRAPH) routine EZY, EZMY, etc.
      CALL AGSETC( 'LABEL/NAME.', 'T' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', 1.2 * LABSZE )

 999  CONTINUE
      END
