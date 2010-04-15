      SUBROUTINE SPD_CYZD( REASON, PICID, OVER, NDF, LIN,
     :                     BIN, MARK, ERROR, WIDTH, FRAME, LABGVN,
     :                     CWGIVN, SWGIVN, LEGIVN, TEXT, BOTTOM, LEFT,
     :                     TOP, RIGHT, XLEGND, YLEGND, FILL, CWORLD,
     :                     SWORLD, LEGEND, CELLSZ, LABSPC, ROMAN,
     :                     CHIGHT, COLOUR, THICK, AXES, TICK, NUML,
     :                     MAJOR, MINOR, DASH, STATUS )
*+
*  Name:
*     SPD_CYZD

*  Purpose:
*     Parameter-free SPECGRID call back.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CYZD( REASON, PICID, OVER, NDF,
*        LIN, BIN, MARK, ERROR, WIDTH, FRAME,
*        LABGVN, CWGIVN, SWGIVN, LEGIVN,
*        TEXT, BOTTOM, LEFT, TOP, RIGHT, XLEGND, YLEGND,
*        FILL, CWORLD, SWORLD, LEGEND, CELLSZ,
*        LABSPC, ROMAN, CHIGHT, COLOUR, THICK,
*        AXES, TICK, NUML, MAJOR, MINOR, DASH, STATUS )

*  Description:
*     This is the call back for the SPECGRID application common to the
*     Motif- and ADAM-based interfaces. All parameter handling takes
*     place outwith this routine, all data access and work takes place
*     under the control of this routine. Since NDF_BEGIN/END must be
*     outside NDF_ASSOC, they too are handled outwith this routine.

*  Arguments:
*     REASON = INTEGER (Given)
*        The call back reason:
*         0 Clean up,
*         1 start up,
*         2 do the work.
*     PICID = INTEGER (Given)
*        The AGI picture identifier. Depending on OVER this is a DATA
*        picture to be used as view port or a picture inside which a new
*        view port and DATA picture is to be created.
*     OVER = INTEGER (Given)
*        Non-zero if the plot is to be an overlay on a previous plot. In
*        this case the AGI data base is checked for a recent DATA
*        picture to define the view port.
*     NDF = INTEGER (Given)
*        The cube NDF identifier.
*     LIN = INTEGER (Given)
*        Non-zero if the spectra are to be plotted as lines connecting
*        points.
*     BIN = INTEGER (Given)
*        Non-zero if the spectra are to be plotted in histogram style.
*     MARK = INTEGER (Given)
*        The marker code to be used for the data points of the spectra.
*     ERROR = INTEGER (Given)
*        Non-zero if the spectra are to be plotted with error bars.
*     WIDTH = INTEGER (Given)
*        Non-zero if the spectra are to be plotted with width bars.
*     FRAME = INTEGER (Given)
*        Non-zero if each spectral cell is to get a plain box.
*     LABGVN( 6 ) = INTEGER (Given)
*        Each is non-zero if a plot label string is supplied by the
*        calling routine for the bottom, left, top, right of the plot
*        and for the bottom and left of the legend cell. Label strings
*        that are not supplied by the caller are derived from the data
*        set.
*     CWGIVN = INTEGER (Given)
*        Non-zero if plot ranges for coordinate space are given. If
*        zero, these ranges are derived from the data to be plotted.
*        Used only if OVER is zero (false).
*     SWGIVN = INTEGER (Given)
*        Non-zero if plot ranges for spectroscopic space are given. If
*        zero, these ranges are derived from the data to be plotted.
*     LEGIVN = INTEGER (Given)
*        Non-zero if a position in coordinate space is given for a
*        legend cell. If zero, no legend cell will be drawn.
*     TEXT( 4 ) = INTEGER (Given)
*        For each axis in coordinate space (bottom, left, top, right),
*        telling whether a text label is to be drawn. Used only if OVER
*        is zero (false).
*     BOTTOM = CHARACTER * ( * ) (Given)
*        The given plot label string for the bottom edge. Used only if
*        LABGVN(1) is non-zero (true) and OVER is zero.
*     LEFT = CHARACTER * ( * ) (Given)
*        The given plot label string for the left edge. Used only if
*        LABGVN(2) is non-zero (true) and OVER is zero.
*     TOP = CHARACTER * ( * ) (Given)
*        The given plot label string for the top edge. Used only if
*        LABGVN(3) is non-zero (true) and OVER is zero.
*     RIGHT = CHARACTER * ( * ) (Given)
*        The given plot label string for the right edge. Used only if
*        LABGVN(4) is non-zero (true) and OVER is zero.
*     XLEGND = CHARACTER * ( * ) (Given)
*        The given plot label string for the bottom of the legend cell.
*        Used only if LABGVN(5) is non-zero (true) and LEGIVN is
*        non-zero (true).
*     YLEGND = CHARACTER * ( * ) (Given)
*        The given plot label string for the left of the legend cell.
*        Used only if LABGVN(6) is non-zero (true) and LEGIVN is
*        non-zero (true).
*     FILL = INTEGER (Given)
*        Zero if the PGPLOT window is to be adjusted so as to give
*        equal plot scales horizontally and vertically. Used only if
*        OVER is zero (false).
*     CWORLD( 4 ) = REAL (Given)
*        If CWGIVN is non-zero (true) then this is the PGPLOT window to
*        be used for coordinate space. Used only if OVER is zero (false).
*     SWORLD( 4 ) = REAL (Given)
*        If SWGIVN is non-zero (true) then this is the PGPLOT window to
*        be used for spectroscopic space. Used only if OVER is zero
*        (false).
*     LEGEND( 2 ) = REAL (Given)
*        The position in coordinate space where the legend cell is to be
*        drawn. Not used if LEGIVN is zero (false).
*     CELLSZ( 2 ) = REAL (Given)
*        The size of the spectral cells measured in coordinate space.
*     LABSPC( 4 ) = REAL (Given)
*        The fraction of the view surface to be reserved for labels. The
*        elements apply to the bottom, left, top, right in that order.
*        Each element must be between 0. and 0.5, or this routine will
*        abort with an error message. Used only if OVER is zero (false).
*     ROMAN = INTEGER (Given)
*        Non-zero if the double-stroke roman font is to be used.
*     CHIGHT = REAL (Given)
*        Character height in PGPLOT units. This height applies to the
*        box labels. Contour labels are plotted half this size.
*     COLOUR = INTEGER (Given)
*        PGPLOT pen number to be used.
*     THICK = INTEGER (Given)
*        PGPLOT line thickness parameter.
*     AXES( 4 ) = INTEGER (Given)
*        0 or 1, telling whether the axis is to be drawn. 1 for yes.
*        Used only if OVER is zero (false).
*     TICK( 4 ) = INTEGER (Given)
*        -1, 0, or +1, telling whether ticks are to be drawn at all and
*        whether they are inside or outside the box. +1 for outside.
*        Used only if OVER is zero (false).
*     NUML( 4 ) = INTEGER (Given)
*        0 or 1, telling whether numeric labels are to be drawn. 1 for
*        yes. Used only if OVER is zero (false).
*     MAJOR( 2 ) = REAL (Given)
*        Distance between major tick marks for horizontal and vertical
*        directions. Used only if OVER is zero (false).
*     MINOR( 2 ) = INTEGER (Given)
*        Number of minor tick intervals per major tick interval for
*        horizontal and vertical directions. Used only if OVER is zero
*        (false).
*     DASH = INTEGER (Given)
*        PGPLOT line style (dash pattern) parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 1.1.
*
*     This routine changes the PGPLOT attributes. It is expected that
*     PGEND will be called after the call to this routine, and between
*     invokations of this routine.

*  Implementation Status:
*     Error and width bars cannot be plotted.
*
*     Apparently the only thing left out is that an auto-scaled plot
*     would not allow to plot half of the extreme bars. So you might
*     well just try to plot these bars, but expect them not to fit in
*     an automatic box. (hme, 25 Nov 1995, more than a year since I
*     wrote this routine.)

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22 Sep 1994 (hme):
*        Original version.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER REASON
      INTEGER PICID
      INTEGER OVER
      INTEGER NDF
      INTEGER LIN
      INTEGER BIN
      INTEGER MARK
      INTEGER ERROR
      INTEGER WIDTH
      INTEGER FRAME
      INTEGER LABGVN( 6 )
      INTEGER CWGIVN
      INTEGER SWGIVN
      INTEGER LEGIVN
      INTEGER TEXT(   4 )
      CHARACTER * ( * ) BOTTOM, LEFT, TOP, RIGHT, XLEGND, YLEGND
      INTEGER FILL
      REAL    CWORLD( 4 )
      REAL    SWORLD( 4 )
      REAL    LEGEND( 2 )
      REAL    CELLSZ( 2 )
      REAL    LABSPC( 4 )
      INTEGER ROMAN
      REAL    CHIGHT
      INTEGER COLOUR, THICK
      INTEGER AXES( 4 ), TICK( 4 ), NUML( 4 )
      REAL    MAJOR( 2 )
      INTEGER MINOR( 2 )
      INTEGER DASH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Volatile Variables:
      LOGICAL XTHERE             ! Existence of HDS component
      INTEGER I                  ! Temporary integer
      INTEGER NDIM               ! NDF dimensionality
      INTEGER NELM               ! NDF size
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER SPAXIS             ! Which axis is spectroscopic
      INTEGER SVAL               ! Spec. val. array pointer
      INTEGER DATA               ! Data array pointer
      INTEGER COORDS( 2 )        ! COORD array pointers
      INTEGER SVNDF              ! Spectroscopic value NDF identifier
      INTEGER SVISND             ! Non-zero if SVAL is N-dimensional
      INTEGER CNDFS( 2 )         ! COORD NDF identifiers
      INTEGER NROWS( 2 )         ! COORD array sizes
      REAL LCELSZ( 2 )           ! Local copy of CELLSZ
      REAL VIEW( 4 )             ! Coordinate space view port
      REAL WORLD( 8 )            ! PGPLOT windows
      CHARACTER * ( 64 ) LABEL( 6 ) ! Actual plot labels
      CHARACTER * ( 64 ) SLABEL  ! Spectroscopic axis label
      CHARACTER * ( 64 ) SUNITS  ! Spectroscopic axis unit
      CHARACTER * ( 64 ) TLABEL  ! NDF label
      CHARACTER * ( 64 ) TUNITS  ! NDF unit
      CHARACTER * ( 64 ) CLABEL( 2 ) ! COORD labels
      CHARACTER * ( 64 ) CUNITS( 2 ) ! COORD units
      CHARACTER * ( NDF__SZTYP ) TYPE( 2 ) ! COORD mapped types
      CHARACTER * ( DAT__SZLOC ) XLOC ! Extension locator

*  Internal References:
      INTEGER CHR_LEN            ! Used length of a string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Clean up.
*  =========

      IF ( REASON .EQ. 0 ) THEN
         CONTINUE


*  Start up.
*  =========

      ELSE IF ( REASON .EQ. 1 ) THEN
         CONTINUE


*  Work.
*  =====

      ELSE IF ( REASON .EQ. 2 ) THEN

*     Check and access data.
*     ======================

*     Check that first non-degenerate axis is spectroscopic.
         CALL NDF_DIM( NDF, NDF__MXDIM, DIM, NDIM, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         CALL SPD_EAAA( NDF, 'READ', XTHERE, XLOC, STATUS )
         CALL SPD_EABA( NDF, XTHERE, SPAXIS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         DO 1001 I = SPAXIS-1, 1, -1
            IF ( DIM(I) .GT. 1 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_CYZD_E01',
     :            'SPECGRID: Error: Spectroscopic axis is not first.',
     :             STATUS )
               GO TO 500
            END IF
 1001    CONTINUE
         IF ( DIM(SPAXIS) .LE. 1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CYZD_E02',
     :         'SPECGRID: Error: Spectroscopic axis is degenerate.',
     :         STATUS )
            GO TO 500
         END IF

*     Map the coordinates (of the cells measured in the outer frame).
*     These are vectors with one element per data row. One vector is the
*     first coordinate while the other is the second coordinate of each
*     row.
         CLABEL(1) = ' '
         CLABEL(2) = ' '
         CUNITS(1) = ' '
         CUNITS(2) = ' '
         TYPE(1) = '_REAL'
         TYPE(2) = '_REAL'
         CALL SPD_EAJD( NDF, XLOC, 'READ', TYPE, CLABEL, CUNITS,
     :                  COORDS, CNDFS, NROWS, STATUS )

*     Map the data (intensity or whatever).
*     This is a 2-D array, each row a spectrum.
         CALL NDF_MAP( NDF, 'DATA', '_REAL', 'READ',
     :      DATA, NELM, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         IF ( NELM .NE. NROWS(1) * DIM(SPAXIS) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CYZD_E03',
     :         'SPECGRID: Error: Data array and coordinate vectors ' //
     :         'have inconsistent shapes.', STATUS )
            GO TO 500
         END IF

*     Map the spectroscopic values, either as single vector
*     corresponding to each data row, or as 2-D array with row-by-row
*     correspondence with the data array. Which of the two is the case
*     is signalled by SVNDF.EQ.NDF__NOID.
         SLABEL = ' '
         SUNITS = ' '
         CALL SPD_EAEA( NDF, XLOC, SPAXIS, 'READ', '_REAL', SLABEL,
     :                  SUNITS, SVAL, SVNDF, I, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         SVISND = 0
         IF ( SVNDF .NE. NDF__NOID ) SVISND = 1


*     Derive label strings.
*     =====================

*     Bottom, left, top, right labels.
*     All are needed only if the plot is not an overlay.
*     Each is needed only if TEXT() says so.
*     For each, if LABGVN then the given string is used, else the label
*     is constructed from the NDF.
         IF ( OVER .EQ. 0 ) THEN
            IF ( TEXT(1) .NE. 0 ) THEN
               IF ( LABGVN(1) .NE. 0 ) THEN
                  LABEL(1) = BOTTOM
               ELSE
                  LABEL(1) = CLABEL(1)
                  IF ( CUNITS(1) .NE. ' ' )
     :               LABEL(1) = CLABEL(1)(:CHR_LEN(CLABEL(1))) //
     :                  ' [' // CUNITS(1)(:CHR_LEN(CUNITS(1))) // ']'
               END IF
            END IF
            IF ( TEXT(2) .NE. 0 ) THEN
               IF ( LABGVN(2) .NE. 0 ) THEN
                  LABEL(2) = LEFT
               ELSE
                  LABEL(2) = CLABEL(2)
                  IF ( CUNITS(2) .NE. ' ' )
     :               LABEL(2) = CLABEL(2)(:CHR_LEN(CLABEL(2))) //
     :                  ' [' // CUNITS(2)(:CHR_LEN(CUNITS(2))) // ']'
               END IF
            END IF
            IF ( TEXT(3) .NE. 0 ) THEN
               IF ( LABGVN(3) .NE. 0 ) THEN
                  LABEL(3) = TOP
               ELSE
                  LABEL(3) = ' '
                  CALL NDF_CGET( NDF, 'TITLE', LABEL(3), STATUS )
               END IF
            END IF
            IF ( TEXT(4) .NE. 0 ) THEN
               IF ( LABGVN(4) .NE. 0 ) THEN
                  LABEL(4) = RIGHT
               ELSE
                  LABEL(4) = ' '
               END IF
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Legend bottom and left labels.
*     Both are needed only if a legend position is given.
*     For each, if LABGVN then the given string is used, else the label
*     is constructed from the NDF.
         IF ( LEGIVN .NE. 0 ) THEN
            IF ( LABGVN(5) .NE. 0 ) THEN
               LABEL(5) = XLEGND
            ELSE
               LABEL(5) = SLABEL
               IF ( SUNITS .NE. ' ' )
     :            LABEL(5) = SLABEL(:CHR_LEN(SLABEL)) //
     :               ' [' // SUNITS(:CHR_LEN(SUNITS)) // ']'
            END IF
            IF ( LABGVN(6) .NE. 0 ) THEN
               LABEL(6) = YLEGND
            ELSE
               TLABEL = ' '
               TUNITS = ' '
               CALL NDF_CGET( NDF, 'LABEL', TLABEL, STATUS )
               CALL NDF_CGET( NDF, 'UNITS', TUNITS, STATUS )
               LABEL(6) = TLABEL
               IF ( TUNITS .NE. ' ' )
     :            LABEL(6) = TLABEL(:CHR_LEN(TLABEL)) //
     :               ' [' // TUNITS(:CHR_LEN(TUNITS)) // ']'
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 500


*     Derive plot window in coordinate space.
*     =======================================

*     If plot is overlay.
         IF ( OVER .NE. 0 ) THEN

*        We have a work routine to check that the AGI picture's
*        transform is what we need, linear and diagonal, and to return
*        the plot window.
            CALL SPD_WAAB( WORLD(1), WORLD(2), WORLD(3),
     :         WORLD(4), STATUS )

*     Else if the relevant plot window is given.
         ELSE IF ( CWGIVN .NE. 0 ) THEN
            WORLD(1) = CWORLD(1)
            WORLD(2) = CWORLD(2)
            WORLD(3) = CWORLD(3)
            WORLD(4) = CWORLD(4)

*     Else (the plot window is to be derived from the data).
         ELSE
            CALL SPD_UAAAR( .FALSE., NROWS(1),
     :                      %VAL( CNF_PVAL( COORDS(1) ) ),
     :                      WORLD(1), WORLD(2), STATUS )
            CALL SPD_UAAAR( .FALSE., NROWS(1),
     :                      %VAL( CNF_PVAL( COORDS(2) ) ),
     :                      WORLD(3), WORLD(4), STATUS )
            WORLD(1) = WORLD(1) - CELLSZ(1) / 2.
            WORLD(2) = WORLD(2) + CELLSZ(1) / 2.
            WORLD(3) = WORLD(3) - CELLSZ(2) / 2.
            WORLD(4) = WORLD(4) + CELLSZ(2) / 2.
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Adjust sign of cell size.
         IF ( WORLD(1) .LT. WORLD(2) ) THEN
            LCELSZ(1) = +CELLSZ(1)
         ELSE
            LCELSZ(1) = -CELLSZ(1)
         END IF
         IF ( WORLD(3) .LT. WORLD(4) ) THEN
            LCELSZ(2) = +CELLSZ(2)
         ELSE
            LCELSZ(2) = -CELLSZ(2)
         END IF


*     Derive plot window in spectroscopic space.
*     ==========================================

*     If SWORLD given, just use it.
*     Else use ranges of spectroscopic values and data values,
*     disregarding error or width bars.
         IF ( SWGIVN .NE. 0 ) THEN
            WORLD(5) = SWORLD(1)
            WORLD(6) = SWORLD(2)
            WORLD(7) = SWORLD(3)
            WORLD(8) = SWORLD(4)
         ELSE
            IF ( SVISND .NE. 0 ) THEN
               CALL SPD_UAAAR( .FALSE., NELM, %VAL( CNF_PVAL( SVAL ) ),
     :                         WORLD(5), WORLD(6), STATUS )
            ELSE
               CALL SPD_UAAAR( .FALSE., DIM(SPAXIS),
     :                         %VAL( CNF_PVAL( SVAL ) ),
     :                         WORLD(5), WORLD(6), STATUS )
            END IF
            CALL SPD_UAAAR( .TRUE., NELM, %VAL( CNF_PVAL( DATA ) ),
     :                      WORLD(7), WORLD(8), STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 500


*     Check the windows.
*     ==================

         IF ( WORLD(1) .EQ. WORLD(2) .OR.
     :        WORLD(3) .EQ. WORLD(4) .OR.
     :        WORLD(5) .EQ. WORLD(6) .OR.
     :        WORLD(7) .EQ. WORLD(8) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CYZD_E05',
     :         'SPECGRID: Error: Zero-extent plot window.', STATUS )
            GO TO 500
         END IF


*     Derive coordinate view port.
*     ============================

*     For an overlay the given AGI picture is already the view port.
         IF ( OVER .NE. 0 ) THEN
            VIEW(1) = 0.
            VIEW(2) = 1.
            VIEW(3) = 0.
            VIEW(4) = 1.

*     Otherwise we have to check LABSPC and derive the view port from
*     those.
         ELSE
            DO 1003 I = 1, 4
               IF ( LABSPC(I) .LT. 0. .OR. LABSPC(I) .GT. 0.5 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPD_CYZD_E04',
     :               'SPECGRID: Error: Invalid label space ' //
     :               'requested.', STATUS )
                  GO TO 500
               END IF
 1003       CONTINUE

*        Provisional view port (assuming FILL is true).
            VIEW(1) =      LABSPC(2)
            VIEW(2) = 1. - LABSPC(4)
            VIEW(3) =      LABSPC(1)
            VIEW(4) = 1. - LABSPC(3)

*        If FILL is false, provisionally set the view port, but have
*        PGPLOT adjust it to equal scales. Then ask what the view port
*        is and work out the label spaces.
*        We leave LABSPC unchanged, because it tells the plotting
*        subroutine about the distance of the text labels from the view
*        port.
            IF ( FILL .EQ. 0 ) THEN
               CALL PGSVP( VIEW(1), VIEW(2), VIEW(3), VIEW(4) )
               CALL PGWNAD( WORLD(1), WORLD(2), WORLD(3), WORLD(4) )
               CALL PGQVP( 0, VIEW(1), VIEW(2), VIEW(3), VIEW(4) )
            END IF

         END IF


*     Plot.
*     =====

*     This is left to a subroutine. One advantage is that the pointers
*     to arrays then can become arrays.
         CALL SPD_WYZA( PICID, OVER, LIN, BIN, MARK, ERROR, WIDTH,
     :                  FRAME, TEXT, LEGIVN, LABEL, VIEW, WORLD,
     :                  LEGEND, LCELSZ, LABSPC, ROMAN, CHIGHT, COLOUR,
     :                  THICK, AXES, TICK, NUML, MAJOR, MINOR, DASH,
     :                  SVISND, DIM(SPAXIS), NROWS(1),
     :                  %VAL( CNF_PVAL( COORDS(1) ) ),
     :                  %VAL( CNF_PVAL( COORDS(2) ) ),
     :                  %VAL( CNF_PVAL( SVAL ) ),
     :                  %VAL( CNF_PVAL( DATA ) ), STATUS )


*     Tidy up.
*     ========

 500     CONTINUE
         CALL NDF_ANNUL( CNDFS(1), STATUS )
         CALL NDF_ANNUL( CNDFS(2), STATUS )
         IF ( SVISND .NE. 0 ) CALL NDF_ANNUL( SVNDF, STATUS )
         IF ( XTHERE ) CALL DAT_ANNUL( XLOC, STATUS )

      END IF


*  Return.
*  =======

      END
