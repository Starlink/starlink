      SUBROUTINE TRACA7( BSMP, ESMP, BDET, EDET, INSCN, XSCN, DETDAT,
     :                   UNITS, AREF, BREF, COORDS, NDISP, DET, DTINDX,
     :                   SCALE, XLMT, YLMT, TITLE, SCNDIR, COLOUR,
     :                   AVERAG, OFMTHD, FLAGS, OFFSET, STATUS )
*+
*  Name:
*     TRACA7

*  Purpose:
*     Draw the CRDD data traces display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACA7( BSMP, ESMP, BDET, EDET, INSCN, XSCN, DETDAT,
*                  UNITS, AREF, BREF, COORDS, NDISP, DET, DTINDX,
*                  SCALE, XLMT, YMAX, TITLE, SCNDIR, COLOUR,
*                  AVERAG, OFMTHD, FLAGS, OFFSET, STATUS )

*  Description:
*     This routine draw the CRDD data traces in the specified units
*     in the current SGS zone. Each trace has a specified offset to
*     seperate it from other traces.
*
*     The input data are the unscaled original data read in from the
*     CRDD file, that is, they are not in the specified units given by
*     UNITS. The argument SCALE gives the values needed to scale the
*     data to the specified units.  The OFFSET gives the already scaled
*     offset for each trace. Therefore the displayed values are
*     SCALE*DETDAT+OFFSET.
*
*     The traces are in-line labelled with its detector number. The
*     horizontal axis gives the in-scan distance from the reference
*     position in arcmins. The right hand side vertical axis gives the
*     strength (flux, or flux density ) of the data trace in the
*     specified units. The left hand side vertical axis are marked with
*     detector numbers at the corresponding offset positions of the
*     traces.  The current specified sky coordinate system and the sky
*     coordinates value of the reference position are displayed as
*     well. The value of the offsets of the data traces and the
*     cross-scan distances from the reference position of each trace
*     are displayed in a table at the top-left corner of the zone.
*
*     Before calling this routine, a graphic device must be opened.
*
*     After drawing, the graphic system is switched from NCAR to
*     SGS/GKS, the world coordinates of the SGS zone is the display box
*     coordinates, that is, the display box will have bounds ( 0.0,
*     1.0, 0.0, 1.0 ).
*
*  Arguments:
*     BSMP = INTEGER (Given)
*        Begin index of the samples in input CRDD data.
*     ESMP = INTEGER (Given)
*        End index of the samples in input CRDD data.
*     BDET = INTEGER (Given)
*        Begin index of the detectors in input CRDD data.
*     EDET = INTEGER (Given)
*        End index of the detectors in input CRDD data.
*     INSCN( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The in-scan distance from the reference position in arcmins
*        of each detector data.
*     XSCN( BDET : EDET ) = REAL (Given)
*        Cross-scan distance from the reference position in arcmins
*        of each detector track.
*     DETDAT( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The input CRDD data.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units in which the data are displayed.
*     AREF = DOUBLE PRECISION (Given)
*        The sky longitude of the reference position.
*     BREF = DOUBLE PRECISION (Given)
*        The sky latitude of the reference position.
*     COORDS = CHARACTER * ( * ) (Given)
*        The sky coordinate system used to specify the reference
*        positon.
*     NDISP = INTEGER (Given)
*        The number of detectors to be displayed.
*     DET( NDISP ) = INTEGER (Given)
*        Detector numbers whose data is to be displayed.
*     DTINDX( NDISP ) = INTEGER (Given)
*        Detector indices whose data is to be displayed.
*     SCALE( NDISP ) = REAL (Given)
*        The scaling factor for converting the data of each trace from
*        original units to displayed units.
*     XLMT( 2 ) = REAL (Given)
*        The lower and upper limits of the display in X axis (in-scan)
*        direction in arcmins north of the reference position.
*     YLMT( 2 ) = REAL (Given)
*        The lower and upper limits of the y axis in the display.
*     TITLE = CHARACTER * ( * ) (Given)
*        Title of the plot.
*     SCNDIR = LOGICAL (Given)
*        If it is true, the scan is from north to south, otherwise,
*        from south to north.
*     COLOUR = LOGICAL (Given)
*        It is true if current graphic device supports colour graphics,
*        otherwise, not.
*     AVERAG( BDET : EDET ) = REAL (Given)
*        The average unscale data values of each detector data.
*     OFMTHD = INTEGER (Given)
*        It specifies the one of three ways to offset the traces
*        when display. It can take following values:
*
*           0 :'FREE' offset.
*
*           1 :'CONSTANT' offset.
*
*           2 :'AVERAGE' offset.
*
*     FLAGS( 8 ) = LOGICAL (Given)
*        Flags indicating if each part of the display is required.
*     OFFSET( NDISP ) = REAL (Given and Returned)
*        If the traces are to be offset in the way of 'FREE', it is a
*        given argument containing the offsets for each data trace. For
*        other offset methods, it is a returned argument containing the
*        offset of each data trace in the display.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPFAM)
*     DSB: David Berry (Starlink)
*     {enter_new_authors_here}

*  History:
*     24-JAN-1991 (WG):
*        Original version (Based on the INTERIM version TRDISP by
*        MAVAD::DSB )
*     8-DEC-1993 (DSB):
*        Modified to display the string '(dead)' in the offset table
*        for any dead detectors.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'I90_DAT'          ! IRAS90 data
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PRM_PAR'          ! PRM_ constants

*  Arguments Given:
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      REAL XSCN( BDET : EDET )
      REAL DETDAT( BSMP : ESMP, BDET : EDET )
      CHARACTER UNITS*(*)
      DOUBLE PRECISION AREF
      DOUBLE PRECISION BREF
      CHARACTER COORDS*(*)
      INTEGER NDISP
      INTEGER DET( NDISP )
      INTEGER DTINDX( NDISP )
      REAL SCALE( NDISP )
      REAL XLMT( 2 )
      REAL YLMT( 2 )
      CHARACTER TITLE*(*)
      LOGICAL SCNDIR
      LOGICAL COLOUR
      REAL AVERAG( BDET : EDET )
      INTEGER OFMTHD
      LOGICAL FLAGS( 8 )

*  Arguments given and returned:
      REAL OFFSET( NDISP )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      REAL SNX_AGUGY             ! Conver user y coord. to grid y coord.

*  Local Constants:
      REAL TEXTHT                ! The hieght of text label
      PARAMETER ( TEXTHT = 0.018 )

*  Local Variables:
      CHARACTER INLAB( I90__MAXDT )*5! In-line label of each trace.
      CHARACTER OFFTAB( 3 * I90__MAXDT + 3 )*12 ! Contents of the
                                 ! offset table
      CHARACTER STR*2            ! String expression of detector number
      CHARACTER XLAB*35          ! Label string of X axis
      CHARACTER YLAB*35          ! Label string of Y axis


      INTEGER ALBPEN             ! SGS pen number used to write axis
                                 ! labels
      INTEGER AXSPEN             ! SGS pen number used to draw axes
      INTEGER CRDSLN             ! Used length of COORDS string
      INTEGER EL                 ! Number of elements of a temporary
                                 ! array
      INTEGER I                  ! Do loop index
      INTEGER INLPEN( I90__MAXDT ) ! The SGS pen number of in-line
                                 ! labels.
      INTEGER NCHAR              ! Number of character of offset codes
      INTEGER NLBPEN             ! SGS pen number used to draw numeric
                                 ! labels
      INTEGER NSTR               ! Number of characters in the STR
      INTEGER PEN( I90__MAXDT )  ! Pen number for each trace
      INTEGER PEN1, PEN2         ! SGS pen number for 1st and 2nd pen.
      INTEGER TCKPEN             ! SGS pen number used to draw tick
                                 ! marks
      INTEGER TITLN              ! Used length of the title string
      INTEGER TITPEN             ! SGS pen number used to write the
                                 ! title
      INTEGER XLABLN             ! Used length of the x label string
      INTEGER XORDER             ! the mapping order for horizontal axis
      INTEGER XPNTR              ! The pointer to the temporary X array
      INTEGER YLABLN             ! Used length of the y label string
      INTEGER YPNTR              ! The pointer to the temporary Y array


      REAL INLBPS( I90__MAXDT )  ! The position of each in-line label.
      REAL MAJTIC( 2 )           ! Number of major tick marks
      REAL MINTIC( 2 )           ! Number of minor tick marks
      REAL OFFGY( I90__MAXDT )   ! Grid y coordinate of an offset.
      REAL OFFLEN                ! The length of trace offset marks
      REAL X1                    ! Lower X limit of grid window
      REAL X2                    ! Upper X limit of grid window
      REAL Y1                    ! Lower Y limit of grid window
      REAL Y2                    ! Upper Y limit of grid window
      REAL YTEMP                 ! A temporary y position.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Define the size of grid window in which data traces will be plotted.
      X1 = 0.3
      X2 = 0.85
      Y1 = 0.24
      Y2 = 0.94

*  Set the number of major tick marks on axis.
      MAJTIC( 1 ) = 4.0
      MAJTIC( 2 ) = 4.0

*  Set the number of minor tick marks on axis.
      MINTIC( 1 ) = -1.0
      MINTIC( 2 ) = -1.0

*  Loop round each trace label.
      DO I = 1, NDISP

*  If in line lables are not required, set the in line label positions
*  to a negative value.
         IF( FLAGS( 1 ) ) THEN
            INLBPS( I ) = 0.1
         ELSE
            INLBPS( I ) = -1.0
         END IF

*  Construct the character strings.
         CALL CHR_ITOC( DET( I ), STR, NSTR )
         INLAB( I ) = '#'//STR( : NSTR )

      END DO

*  If the way to offset the traces is not 'FREE', calculate the offset
*  for each trace.
      IF( OFMTHD .NE. 0 ) THEN
         CALL TRACB1( OFMTHD, BDET, EDET, NDISP, DTINDX, YLMT,
     :                AVERAG, SCALE, OFFSET, STATUS )
      END IF

*  Get the temporary work space to store the X and Y data of the
*  display.
      EL = NDISP*(ESMP - BSMP + 1 )
      CALL PSX_CALLOC( EL, '_REAL', XPNTR, STATUS )
      CALL PSX_CALLOC( EL, '_REAL', YPNTR, STATUS )

*  Put the trace to be drawn into temproray work space.
      CALL TRACB2( BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
     :             NDISP, DTINDX, SCALE, OFFSET, XLMT,
     :             %VAL( YPNTR ), %VAL( XPNTR ), STATUS )

*  If the scan is from north to south, set the mapping order for
*  in-scan coordinates to horizontal axis as increasing from right
*  to left so that the south is always at the left.
      IF ( SCNDIR ) THEN
         XORDER = 1

*  Otherwise, scan is from south to north, set the mapping order as
*  increasing from left to right.
      ELSE
         XORDER = 0
      END IF

*  Set all pens of the data traces as pen 1.
      DO I = 1, NDISP
         PEN( I ) = 1
      END DO

*  If colour is available, set the in-line labels and axes as pen 3.
      IF ( COLOUR ) THEN
         AXSPEN = 1
         DO I = 1, NDISP
            INLPEN( I ) = 3
         END DO

*  Otherwise set them as pen 1.
      ELSE
         AXSPEN = 1
         DO I = 1, NDISP
            INLPEN( I ) = 1
         END DO
      END IF

*  Set the colour of title, axis labels, numeric labels and tick marks
*  as pen 1.
      TITPEN = 1
      ALBPEN = 1
      NLBPEN = 1
      TCKPEN = 1

*  Construct x label and y label of the display.
      IF( FLAGS( 2 ) ) THEN
         XLAB = 'Arcmins from reference position'
         XLABLN = CHR_LEN( XLAB )
      ELSE
         XLAB = ' '
         XLABLN = 1
      END IF

      IF( FLAGS( 3 ) ) THEN
         YLAB = UNITS
         YLABLN = CHR_LEN( YLAB )
      ELSE
         YLAB = ' '
         YLABLN = 1
      END IF

*  Get the used length of the title.
      TITLN = CHR_LEN( TITLE )

*  Draw the data traces.
      CALL IRM_MLINE( ESMP - BSMP + 1, NDISP, .FALSE., %VAL( XPNTR ),
     :                %VAL( YPNTR ), XORDER, 0, PEN, .TRUE., X1, X2,
     :                Y1, Y2, AXSPEN, TITLE( :TITLN ), TITPEN,
     :                XLAB( :XLABLN ), YLAB( : YLABLN ), 1.2*TEXTHT,
     :                ALBPEN, INLAB, INLBPS, INLPEN, -1, -1, XLMT, YLMT,
     :               .FALSE., .FALSE., .TRUE., .FALSE., MAJTIC, MINTIC,
     :                0.015, .TRUE., TCKPEN, NLBPEN, .TRUE., STATUS )

*  Get the grid y coordinate for each trace offset.
      DO I = 1, NDISP
         OFFGY( I ) = SNX_AGUGY( OFFSET( I ) )
      END DO

*  If colour is available, set PEN1 as pen 1 and PEN2 as pen 3.
      IF ( COLOUR ) THEN
         PEN1 = 1
         PEN2 = 3

*  Otherwise, set both pens as pen 1.
      ELSE
         PEN1 = 1
         PEN2 = 1
      END IF

*  Set up text height and justification, and initial Y position for the
*  extra information.
      CALL SGS_SHTX( TEXTHT )
      CALL SGS_STXJ( 'CC' )
      YTEMP = -0.035 - 4.2 * TEXTHT

*  Write additional labels for X axis
      IF( FLAGS( 4 ) ) THEN

         CALL SGS_TX( 0.0, YTEMP, 'South' )
         CALL SGS_TX( 1.0, YTEMP, 'North' )
         CALL SGS_TX( 0.5, YTEMP, 'Scan direction' )

*  Draw scan direction mark
         IF ( .NOT.SCNDIR ) THEN
            CALL SGS_LINE( 0.5 + 6.0 * TEXTHT, YTEMP,
     :                     0.5 + 9.0 * TEXTHT, YTEMP )
            CALL SGS_LINE( 0.5 + 8.5 * TEXTHT, YTEMP + 0.5 * TEXTHT,
     :                     0.5 + 9.0 * TEXTHT, YTEMP )
            CALL SGS_LINE( 0.5 + 8.5 * TEXTHT, YTEMP - 0.5 * TEXTHT,
     :                     0.5 + 9.0 * TEXTHT, YTEMP )
         ELSE
            CALL SGS_LINE( 0.5 - 6.0 * TEXTHT, YTEMP,
     :                     0.5 - 9.0 * TEXTHT, YTEMP )
            CALL SGS_LINE( 0.5 - 8.5 * TEXTHT, YTEMP + 0.5 * TEXTHT,
     :                     0.5 - 9.0 * TEXTHT, YTEMP )
            CALL SGS_LINE( 0.5 - 8.5 * TEXTHT, YTEMP - 0.5 * TEXTHT,
     :                     0.5 - 9.0 * TEXTHT, YTEMP )
         END IF

      END IF

*  Draw trace  offset marks
      OFFLEN = 0.05 * ( X2 - X1 )
      CALL SGS_SPEN( PEN2 )
      DO I = 1, NDISP

*  Draw the left and right mark for the Ith trace, if the mark is in the
*  plotting box
         IF ( OFFGY( I ) .GE. 0.0 .AND. OFFGY( I ) .LE. 1.0 ) THEN

*  Right hand edge...
            IF( FLAGS( 5 ) ) CALL SGS_LINE( 1.0 - OFFLEN, OFFGY( I ),
     :                                      1.0, OFFGY( I ) )

*  Left hand edge (with trace number)...
            IF( FLAGS( 6 ) ) THEN
               CALL SGS_LINE( 0.0, OFFGY( I ), OFFLEN, OFFGY( I ) )
               CALL SGS_STXJ( 'BL' )
               CALL SGS_TX( 0.0, OFFGY( I ) + 0.01, INLAB( I ) )
            END IF

         END IF
      END DO

*  Write the contents of the first column of the offset table.
      IF( FLAGS( 7 ) ) THEN

         OFFTAB( 1 ) = 'Det'
         OFFTAB( NDISP + 2 ) = 'Offset'
         OFFTAB( 2 * NDISP + 3 ) ='X-dist.'

         DO I = 1, NDISP
            OFFTAB( I + 1 )( 1 : 1 ) = '#'
            CALL CHR_ITOC( DET( NDISP - I + 1 ),
     :                     OFFTAB( I + 1 )( 2 : ), NCHAR )

            IF( SCALE( I ) .NE. VAL__BADR ) THEN
               CALL CHR_RTOC( OFFSET( NDISP - I + 1 ),
     :                        OFFTAB( NDISP + 2 + I ), NCHAR )
            ELSE
               OFFTAB( NDISP + 2 + I ) = ' (dead) '
            END IF

            CALL CHR_RTOC( XSCN( DTINDX( NDISP - I + 1 ) ),
     :                     OFFTAB( 2 * NDISP + 3 + I ), NCHAR )

         END DO

*  Display the offset table.
         CALL IRM_TABLE( ' ', NDISP + 1, 3, OFFTAB( 1 ), -0.5,
     :                   -0.05, 0.4, 1.0, .TRUE., 1, YTEMP,  STATUS )

      ELSE
         YTEMP = 1.0
      END IF

*  Write description of reference position.
      IF( FLAGS( 8 ) ) THEN
         CALL SGS_SPEN( PEN1 )
         CALL SGS_STXJ( 'CL' )

         YTEMP = YTEMP - 3.0 * TEXTHT
         CALL SGS_TX( -0.5, YTEMP, 'Current sky coordinate system:' )

         YTEMP = YTEMP - 2.5 * TEXTHT
         CRDSLN = CHR_LEN( COORDS )
         CALL SGS_TX( -0.5 + 2.0*TEXTHT, YTEMP, COORDS( : CRDSLN ) )

         YTEMP = YTEMP - 3.5 * TEXTHT
         CALL SGS_TX( -0.5, YTEMP, 'Reference Position:' )

*  Write the sky coordinates of the reference position.
         CALL TRACC1( AREF, BREF, -0.5 + 2.0*TEXTHT,
     :                YTEMP - 2.5*TEXTHT, COORDS, STATUS )

      END IF

*  Flush out the drawing
      CALL SGS_FLUSH

*  Release the temporary work space
      CALL PSX_FREE( XPNTR, STATUS )
      CALL PSX_FREE( YPNTR, STATUS )

      END
