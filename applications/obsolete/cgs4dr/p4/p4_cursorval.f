*+  P4_CURSORVAL - Get data value at cursor position
       SUBROUTINE P4_CURSORVAL( STATUS )
*    Description :
*     Routine to position cursor, return co-ords and keystroke,
*     the value of the data array at that point
*    Invocation :
*     CALL P4_CURSORVAL( STATUS )
*    Deficiencies :
*     If the FITS item DCOLUMNS does not exist, this routine arbitrarily
*     assumes it is 62. This will only cause a problem if DCOLUMNS does
*     not exist, but DETINCR and DETNINCR do, and the detector is not
*     62 columns across.
*    Bugs :
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S. M. Beard (REVAD::SMB)
*     P. N. Daly (JACH::PND)
*    History :
*     1989:        Original version.                                 (JFL)
*     24-Oct-1989: History added. Extra status checks added.         (SMB)
*     25-Oct-1989: Setting of status to ACT_END removed, so that
*                  P4_ENDACTION may be used.                         (SMB)
*      2-Nov-1989: Arguments of P4_SLICE and P4_BYTESLICE rearranged.(SMB)
*      2-Nov-1989: Modified so that a cursor position outside the
*                  plot window is regarded as an ERROR.              (SMB)
*     29-Jan-1990: Modified to take modification of
*                  P4_FINDVAL into account.                          (SMB)
*     16-Feb-1990: CURSOR_STATUS parameter added, so that ICL
*                  procedures can check when the cursoring has
*                  failed.                                           (SMB)
*      7-May-1990: DYN dynamic memory functions replaced by %val,
*                  so the code can be compiled with array bounds
*                  checking.                                         (SMB)
*      6-Aug-1990: Bug, in which the routine crashed with an access
*                  violation if an attempt was made to cursor on
*                  something plotted with a 3-D data array, fixed.
*                  MAXDIM parameter added. More status checks added.
*                  Code spaced out more.                             (SMB)
*      7-Aug-1990: Brought up to date with earlier changes made to
*                  P4_PLOTGRAPH (which allowed a whole region to be
*                  extracted and plotted).                           (SMB)
*     10-Feb-1991: Modified to return MASK_X and MASK_Y parameters,
*                  to be used for bad pixel mask editing. Bug which
*                  caused this routine to crash when called without
*                  a device open fixed. Bug which caused IPOS and
*                  JPOS parameters to be occasionally left undefined
*                  fixed.                                            (SMB)
*     29-May-1991: Modified so the cursor is left in the same location
*                  rather than being returned to the centre of the
*                  window (which was found to be annoying). Symbols
*                  representing good and bad points changed to + and X.
*                  Bug fixes: ACT_X and ACT_Y should be REAL and not
*                  INTEGER. The routine was not taking SUPERSAMPLING
*                  into account when calculating MASK_X and MASK_Y.  (SMB)
*     18-Feb-1993: Tidy up code, use ERR_ANNUL, remove STR$          (PND)
*      4-Aug-1994: Port to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER DSA_TYPESIZE                    ! Finds size of datatypes
*    Global variables :
      INCLUDE 'P4COM.INC'                     ! P4 common block
*    Local Constants :
      INTEGER MAXDIM                          ! Maximum number of dimensions
      PARAMETER ( MAXDIM = 3 )
      INTEGER PLUS, CROSS                     ! Code numbers for PGPLOT markers
      PARAMETER ( PLUS  = 2,                  ! "+" shape
     :            CROSS = 5 )                 ! "X" shape
*    Local variables :
      LOGICAL ERRORS                          ! T if error array present
      LOGICAL QUALITY                         ! T if quality array present
      LOGICAL OUTSIDE                         ! TRUE if cursor posn outside window
      LOGICAL EXIST                           ! Indicates if FITS item exists
      INTEGER ELEMENTS                        ! Number of elements of FITS item
      INTEGER PORT                            ! Port number
      INTEGER STRLEN                          ! Number of characters in FITS item
      INTEGER NDIM                            ! DSA stuff
      INTEGER DIMS ( MAXDIM )
      INTEGER NELM
      INTEGER DSLT                            ! Data array
      INTEGER DPTR
      INTEGER A1SLT                           ! Axis1
      INTEGER A1PTR
      INTEGER A2SLT                           ! Axis2
      INTEGER A2PTR
      INTEGER QSLT                            ! Quality array
      INTEGER QPTR
      INTEGER ESLT                            ! Error array
      INTEGER EPTR
      INTEGER TD_SLOT                         ! Temporary data
      INTEGER TD_PTR
      INTEGER TE_SLOT                         ! Temporary errors
      INTEGER TE_PTR
      INTEGER TQ_SLOT                         ! Temporary quality
      INTEGER TQ_PTR
      INTEGER A1_PTR                          ! Temporary axis1
      INTEGER A2_PTR                          ! Temporary axis2
      INTEGER C_SLOT                          ! Temporary counter workspace
      INTEGER C_PTR
      INTEGER BYTES                           ! Size of temporary data array
      INTEGER DATAQUAL                        ! 0 if data value returned is
*                                             !  valid
      INTEGER IPOS, JPOS                      ! Array coords of specified data
*                                             !  point
      INTEGER MASK_X, MASK_Y                  ! Array coordinates of the
*                                             !  equivalent point in a
*                                             !  bad pixel mask.
      INTEGER SUPERSAMPLING                   ! "Supersampling" factor
      INTEGER OVERSAMPLING                    ! "Oversampling" factor
      INTEGER DCOLUMNS                        ! Number of columns on the detector
      INTEGER DET_NINCR                       ! Number of detector increments
      REAL DET_INCR                           ! The detector increment in pixels.
      REAL X, Y                               ! cursor position
      REAL ACT_X, ACT_Y                       ! actual position of datum
      REAL DATAVAL                            ! data value at cursor position
      REAL DATAERR                            ! error value at cursor position
      CHARACTER*4
     :  ACCESS,                               ! Access type for FITS item
     :  COMMENT                               ! Dummy comment
*   Set the initial values of X and Y
      SAVE X, Y
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the port and read the noticebaord
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      CALL P4_READ_NB( PORT, STATUS )

*    Check that the port is open
      CALL P4_CHECK_PORT( PORT, STATUS )

*    Get the keystroke
      CALL P4_GET_CURSOR( PORT, STATUS )
      CALL PAR_GET0R( 'X', X, STATUS )
      CALL PAR_GET0R( 'Y', Y, STATUS )

*    Now map in the data, error and quality arrays (if present)
      CALL DSA_OPEN( STATUS )
      CALL P4_CHECK_INPUT( DISPLAY_DATA( PORT ), STATUS )
      CALL DSA_NAMED_INPUT ('DATA', DISPLAY_DATA( PORT ), STATUS )

*    Determine the value of DCOLUMNS
      DCOLUMNS = 62
      CALL DSA_SEEK_FITS( 'DATA', 'DCOLUMNS', EXIST,
     :  ACCESS, ELEMENTS, STRLEN, STATUS )
      IF ( EXIST ) CALL DSA_GET_FITS_I( 'DATA', 'DCOLUMNS', 0,
     :  DCOLUMNS, COMMENT, STATUS )

*    Determine the value of DETNINCR
      DET_NINCR = 1
      CALL DSA_SEEK_FITS( 'DATA', 'DETNINCR', EXIST,
     :  ACCESS, ELEMENTS, STRLEN, STATUS )
      IF ( EXIST ) CALL DSA_GET_FITS_I( 'DATA', 'DETNINCR', 0,
     :    DET_NINCR, COMMENT, STATUS )

*    Determine the value of DETINCR
      DET_INCR = 1.0
      CALL DSA_SEEK_FITS( 'DATA', 'DETINCR', EXIST,
     :  ACCESS, ELEMENTS, STRLEN, STATUS )
      IF ( EXIST ) CALL DSA_GET_FITS_F( 'DATA', 'DETINCR', 0,
     :    DET_INCR, COMMENT, STATUS )

*    Determine the oversampling and supersampling
      IF ( DET_NINCR .GT. 1 ) THEN
        SUPERSAMPLING = NINT( DET_NINCR * DET_INCR )
      ELSE
        SUPERSAMPLING = 1
      ENDIF
      OVERSAMPLING = DET_NINCR / SUPERSAMPLING

*    Map the data, quality and errors
      CALL DSA_DATA_SIZE( 'DATA', MAXDIM, NDIM, DIMS, NELM, STATUS )
      CALL DSA_USE_QUALITY( 'DATA', STATUS )
      CALL DSA_MAP_DATA( 'DATA', 'READ', 'FLOAT', DPTR, DSLT, STATUS )

      CALL DSA_SEEK_QUALITY( 'DATA', QUALITY, STATUS )
      IF ( QUALITY ) THEN
        CALL DSA_MAP_QUALITY ('DATA', 'READ', 'BYTE', QPTR, QSLT, STATUS )
      ENDIF

      CALL DSA_SEEK_ERRORS( 'DATA', ERRORS, STATUS )
      IF ( ERRORS ) THEN
        CALL DSA_MAP_ERRORS( 'DATA', 'READ', 'FLOAT', EPTR, ESLT, STATUS )
      ENDIF

*    Map axis data
      CALL DSA_MAP_AXIS_DATA( 'DATA', 1, 'READ',
     :  'FLOAT', A1PTR, A1SLT, STATUS )
      IF ( NDIM .GE. 1 ) THEN
        CALL DSA_MAP_AXIS_DATA( 'DATA', 2, 'READ',
     :                 'FLOAT', A2PTR, A2SLT, STATUS )
      ELSE
        DIMS(2) = 1
      ENDIF

*    Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'P4_CURSORVAL: '/
     :    /'Error accessing data structure', STATUS )
        CALL DSA_CLOSE( STATUS )
        RETURN
      ENDIF

*    Further treatment depends on type of display involved
      IF ( DISPLAY_TYPE( PORT ) .EQ. 'IMAGE' ) THEN

*      Simply copy pointers, since data is already in desired format
        TD_PTR = DPTR
        TE_PTR = EPTR
        TQ_PTR = QPTR
        A1_PTR = A1PTR
        A2_PTR = A2PTR
      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'GRAPH' ) THEN

*      Get a work array, and copy into it the slice of data
        IF ( NDIM .EQ. 1 ) THEN

*        Already have data in suitable form, just set the pointers
          TD_PTR = DPTR
          TE_PTR = EPTR
          TQ_PTR = QPTR
          A1_PTR = A1PTR
          A2_PTR = 0
        ELSE IF ( ( NDIM .EQ. 2 ) .AND.
     :            ( CUT_DIRECTION( PORT ) .EQ. 'X' ) ) THEN

*        A cut in X through a 2d array.
          BYTES = DSA_TYPESIZE( 'FLOAT', STATUS ) * DIMS( 1 )
          CALL DSA_GET_WORKSPACE( BYTES, TD_PTR, TD_SLOT, STATUS )

*        If necessary, get some workspace for the slice quality and errors.
          TQ_PTR = 0
          IF ( QUALITY )
     :      CALL DSA_GET_WORKSPACE (DIMS(1), TQ_PTR, TQ_SLOT, STATUS )

          TE_PTR = 0
          IF ( ERRORS )
     :      CALL DSA_GET_WORKSPACE (BYTES, TE_PTR, TE_SLOT, STATUS )

*        If the start and end positions are the same, we have a single slice
          IF (  NINT( SLICE_START( PORT ) ) .EQ.
     :          NINT( SLICE_END( PORT ) ) ) THEN
            CALL P4_SLICE( DIMS( 1 ), DIMS( 2 ), %val( DPTR ), 'X',
     :        NINT( SLICE_START( PORT ) ), %val( TD_PTR ), STATUS )

            IF ( ERRORS ) THEN
              CALL P4_SLICE( DIMS( 1 ), DIMS( 2 ), %val( EPTR ), 'X',
     :          NINT( SLICE_START( PORT ) ), %val( TE_PTR ), STATUS )
            ENDIF

            IF ( QUALITY ) THEN
              CALL P4_BYTESLICE( DIMS( 1 ), DIMS( 2 ), %val( QPTR ), 'X',
     :          NINT( SLICE_START( PORT ) ), %val( TQ_PTR ), STATUS )
            ENDIF
          ELSE

*          Else we have several slices
            CALL DSA_GET_WORKSPACE( BYTES, C_PTR, C_SLOT, STATUS )

*          Average together the required slices from the data array.
            CALL P4_EXTRACT( DIMS( 1 ), DIMS( 2 ), %val( DPTR ),
     :        %val( QPTR ), QUALITY, 'X', NINT( SLICE_START( PORT ) ),
     :        NINT( SLICE_END( PORT ) ), %val( C_PTR ), %val( TD_PTR ),
     :        %val( TQ_PTR ), STATUS )

            IF ( ERRORS ) THEN
              CALL P4_EXTRACT( DIMS( 1 ), DIMS( 2 ), %val( EPTR ),
     :          %val( QPTR ), QUALITY, 'X', NINT( SLICE_START( PORT ) ),
     :          NINT( SLICE_END( PORT ) ), %val( C_PTR ), %val( TE_PTR ),
     :          %val( TQ_PTR ), STATUS )
            ENDIF
          ENDIF

*        Axes
          A1_PTR = A1PTR
          A2_PTR = 0

*        Change dimensions to show P4_FINDVAL that we're looking at a 1-d array
          NDIM = 1
          DIMS( 2 ) = 1
        ELSE IF ( ( NDIM .EQ. 2 ) .AND.
     :            ( CUT_DIRECTION( PORT ) .EQ. 'Y' ) ) THEN

*        A cut in Y through a 2d array.
          BYTES = DSA_TYPESIZE( 'FLOAT', STATUS ) * DIMS( 2 )
          CALL DSA_GET_WORKSPACE( BYTES, TD_PTR, TD_SLOT, STATUS )

          TQ_PTR = 0
          IF ( QUALITY )
     :      CALL DSA_GET_WORKSPACE( DIMS( 2 ), TQ_PTR, TQ_SLOT, STATUS )

          TE_PTR = 0
          IF ( ERRORS )
     :      CALL DSA_GET_WORKSPACE( BYTES, TE_PTR, TE_SLOT, STATUS )

*        If the start and end positions are the same, we have a single slice
          IF ( NINT( SLICE_START( PORT ) ) .EQ.
     :         NINT( SLICE_END( PORT ) ) ) THEN
            CALL P4_SLICE( DIMS( 1 ), DIMS( 2 ), %val( DPTR ), 'Y',
     :        NINT( SLICE_START( PORT ) ), %val( TD_PTR ), STATUS )

            IF ( ERRORS ) THEN
              CALL P4_SLICE( DIMS( 1 ), DIMS( 2 ), %val( EPTR ), 'Y',
     :          NINT( SLICE_START( PORT ) ), %val( TE_PTR ), STATUS )
            ENDIF

            IF ( QUALITY ) THEN
              CALL P4_BYTESLICE( DIMS( 1 ), DIMS( 2 ), %val( QPTR ), 'Y',
     :          NINT( SLICE_START( PORT ) ), %val( TQ_PTR ), STATUS )
            ENDIF
          ELSE

*          Several slices need to be averaged together.
            CALL DSA_GET_WORKSPACE( BYTES, C_PTR, C_SLOT, STATUS )

*          Average together the required slices from the data array.
            CALL P4_EXTRACT( DIMS( 1 ), DIMS( 2 ), %val( DPTR ),
     :        %val( QPTR ), QUALITY, 'Y', NINT( SLICE_START( PORT ) ),
     :        NINT( SLICE_END( PORT ) ), %val( C_PTR ), %val( TD_PTR ),
     :        %val( TQ_PTR ), STATUS )

            IF ( ERRORS ) THEN
              CALL P4_EXTRACT( DIMS( 1 ), DIMS( 2 ), %val( EPTR ),
     :          %val( QPTR ), QUALITY, 'Y',  NINT( SLICE_START( PORT ) ),
     :          NINT( SLICE_END( PORT ) ), %val( C_PTR ), %val( TE_PTR ),
     :          %val( TQ_PTR ), STATUS )
            ENDIF
          ENDIF

*        Axes
          A1_PTR = A2PTR
          A2_PTR = 0

*        Set dimensions to show P4_FINDVAL the shape of the temporary array
          NDIM = 1
          DIMS( 1 ) = DIMS( 2 )
          DIMS( 2 ) = 1
        ENDIF
      ENDIF

*    Check that X,Y is inside the plot window
      IF ( ( X .LT. XSTART( PORT ) ) .OR.
     :     ( X .GT. XEND( PORT ) )   .OR.
     :     ( Y .LT. YSTART( PORT ) ) .OR.
     :     ( Y .GT. YEND( PORT ) ) ) THEN
        OUTSIDE = .TRUE.
        DATAVAL = 0.0
        DATAERR = 0.0
        DATAQUAL = 1
        IPOS = 0
        JPOS = 0
        MASK_X = 0
        MASK_Y = 0
      ELSE
        OUTSIDE = .FALSE.

*      Find data value at position of cursor X,Y
        CALL P4_FINDVAL( X, Y, %val( A1_PTR ), %val( A2_PTR ),
     :    %val( TD_PTR ), %val( TE_PTR ), %val( TQ_PTR ),
     :    NDIM, DIMS( 1 ), DIMS( 2 ), ERRORS, QUALITY, DATAVAL,
     :    DATAERR, DATAQUAL, ACT_X, ACT_Y, IPOS, JPOS, STATUS )

*      Determine the mask co-ords from the array co-ords and the oversampling
        IF ( OVERSAMPLING .GT. 1 ) THEN
          MASK_X = ( IPOS + OVERSAMPLING - 1 ) / OVERSAMPLING
          MASK_Y = JPOS
        ELSE
          MASK_X = IPOS
          MASK_Y = JPOS
        ENDIF

*      Adjust the mask co-ordinates to take account of supersampling
        IF ( SUPERSAMPLING .GT. 1 ) THEN
           IF ( MASK_X .GT. DCOLUMNS ) THEN
             MASK_X = MASK_X - SUPERSAMPLING + 1
           ENDIF
        ENDIF
      ENDIF

*    Set error, quality, etc. parameters
      CALL PAR_PUT0R( 'DATAVAL', DATAVAL, STATUS )
      CALL PAR_PUT0R( 'DATAERR', DATAERR, STATUS )
      CALL PAR_PUT0I( 'DATAQUAL', DATAQUAL, STATUS )
      CALL PAR_PUT0R( 'ACT_X', ACT_X, STATUS )
      CALL PAR_PUT0R( 'ACT_Y', ACT_Y, STATUS )
      CALL PAR_PUT0I( 'IPOS', IPOS, STATUS )
      CALL PAR_PUT0I( 'JPOS', JPOS, STATUS )
      CALL PAR_PUT0I( 'MASK_X', MASK_X, STATUS )
      CALL PAR_PUT0I( 'MASK_Y', MASK_Y, STATUS )

*    Plot a marker
      IF ( STATUS .EQ. SAI__OK ) THEN
        IF ( OUTSIDE ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'P4_CURSORVAL: '/
     :      /'Cursor is outside plot window', STATUS )
        ELSE
          IF ( DATAQUAL .EQ. 0 ) THEN
            CALL PGPOINT( 1, ACT_X, ACT_Y, PLUS )
          ELSE
            CALL PGPOINT( 1, ACT_X, ACT_Y, CROSS )
          ENDIF
        ENDIF
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'P4_CURSORVAL: '/
     :    /'Failed to write output parameters', STATUS )
      ENDIF

*    Close DSA
      CALL DSA_CLOSE( STATUS )

      END
