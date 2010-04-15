*+  P4_GET_AXLIM - Get the axis limits for plot
      SUBROUTINE P4_GET_AXLIM( PORT, DIM1, DIM2, STATUS )
*    Description :
*     This routine gets the axis limits
*    Invocation :
*     CALL P4_GET_AXLIM( PORT, DIM1, DIM2, STATUS )
*    Deficiencies :
*     Assumes DSA structure is open under label 'DATA'
*    Authors :
*     P. N. Daly ( JACH::PND )
*    History :
*     19-Aug-1994: Original Unix version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER PORT                            ! Port number
      INTEGER DIM1, DIM2                      ! Dimensions of array
*    External references :
      INTEGER GEN_BSEARCH                     ! Finds pixel value for given real
      INTEGER CHR_LEN                         ! Finds used length of string
*    Global variables :
      INCLUDE 'P4COM.INC'                     ! P4 common block
*    Local Constants :
      REAL TOLER                              ! Tolerance for testing
      PARAMETER ( TOLER = 1.0E-10 )
*    Local variables :
      REAL XMAX, XMIN, YMAX, YMIN             ! Axis limits
      INTEGER IMIN, IMAX, JMIN, JMAX          ! Pixel limits
*-

*   Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set the X-axis pixel limits
      IF ( DIM1 .GT. 1 ) THEN
        IMIN = 1
        IMAX = DIM1
      ELSE
        IMIN = 1
        IMAX = 1
      ENDIF
      IF ( VERBOSE ) THEN
        CALL MSG_SETI( 'IMIN', IMIN )
        CALL MSG_SETI( 'IMAX', IMAX )
        CALL MSG_OUT( ' ', 'P4_GET_AXLIM: X-axis pixel range is ^IMIN to ^IMAX', STATUS )
      ENDIF

*   Set the Y-axis pixel limits
      IF ( DIM2 .GT. 1 ) THEN
        JMIN = 1
        JMAX = DIM2
      ELSE
        JMIN = 1
        JMAX = 1
      ENDIF
      IF ( VERBOSE ) THEN
        CALL MSG_SETI( 'JMIN', JMIN )
        CALL MSG_SETI( 'JMAX', JMAX )
        CALL MSG_OUT( ' ', 'P4_GET_AXLIM: Y-axis pixel range is ^JMIN to ^JMAX', STATUS )
      ENDIF

*   Get the X-axis data limits
      IF ( IMAX .GT. 1 ) THEN
        CALL DSA_MAP_AXIS_DATA( 'DATA', 1, 'READ', 'FLOAT', AXIS1_PTR, AXIS1_SLT, STATUS )
        CALL GEN_RANGEF( %val(AXIS1_PTR), IMIN, IMAX, XMAX, XMIN )
        IF ( VERBOSE ) THEN
          CALL MSG_SETR( 'XMIN', XMIN )
          CALL MSG_SETR( 'XMAX', XMAX )
          CALL MSG_OUT( ' ', 'P4_GET_AXLIM: X-axis data range is ^XMIN to ^XMAX', STATUS )
        ENDIF
      ENDIF

*   Get the Y-axis data limits
      IF ( JMAX .GT. 1 ) THEN
        CALL DSA_MAP_AXIS_DATA( 'DATA', 2, 'READ', 'FLOAT', AXIS2_PTR, AXIS2_SLT, STATUS )
        CALL GEN_RANGEF( %val(AXIS2_PTR), JMIN, JMAX, YMAX, YMIN )
        IF ( VERBOSE ) THEN
          CALL MSG_SETR( 'YMIN', YMIN )
          CALL MSG_SETR( 'YMAX', YMAX )
          CALL MSG_OUT( ' ', 'P4_GET_AXLIM: Y-axis data range is ^YMIN to ^YMAX', STATUS )
        ENDIF
      ENDIF

      IF ( PLOT_WHOLE(PORT) ) THEN

*     Use data limits (whole=true)
        IF ( (ISTART(PORT).LT.0) .AND. (IEND(PORT).LT.0) .AND. (JSTART(PORT).LT.0) .AND. (JEND(PORT).LT.0) ) THEN
          IF ( IMAX .GT. 1 ) THEN
            XSTART(PORT) = XMIN
            XEND(PORT)   = XMAX
            ISTART(PORT) = GEN_BSEARCH( %val(AXIS1_PTR), IMAX, XMIN )
            IEND(PORT)   = GEN_BSEARCH( %val(AXIS1_PTR), IMAX, XMAX )
          ENDIF
          IF ( JMAX .GT. 1 ) THEN
            YSTART(PORT) = YMIN
            YEND(PORT)   = YMAX
            JSTART(PORT) = GEN_BSEARCH( %val(AXIS2_PTR), JMAX, YMIN )
            JEND(PORT)   = GEN_BSEARCH( %val(AXIS2_PTR), JMAX, YMAX )
          ENDIF

*     Use pixel limits (whole=true)
        ELSE
          CALL CHR_FILL( ' ', TITLE(PORT) )
          TITLE(PORT) = DISPLAY_DATA(PORT)(1:CHR_LEN(DISPLAY_DATA(PORT))) // ' (pixels)'
          IF ( IMAX .GT. 1 ) THEN
            ISTART(PORT) = IMIN
            IEND(PORT)   = IMAX
            XSTART(PORT) = REAL( IMIN )
            XEND(PORT)   = REAL( IMAX )
          ENDIF
          IF ( JMAX .GT. 1 ) THEN
            JSTART(PORT) = JMIN
            JEND(PORT)   = JMAX
            YSTART(PORT) = REAL( JMIN )
            YEND(PORT)   = REAL( JMAX )
          ENDIF
        ENDIF
      ELSE

*     Use data limits (whole=false)
        IF ( (ISTART(PORT).LT.0) .AND. (IEND(PORT).LT.0) .AND. (JSTART(PORT).LT.0) .AND. (JEND(PORT).LT.0) ) THEN
          IF ( IMAX .GT. 1 ) THEN
            IF ( (XSTART(PORT).LT.XMIN) .OR. (XSTART(PORT).GT.XMAX) ) XSTART(PORT) = XMIN
            IF ( (XEND(PORT).LT.XMIN) .OR. (XEND(PORT).GT.XMAX) ) XEND(PORT) = XMAX
            ISTART(PORT) = GEN_BSEARCH( %val(AXIS1_PTR), IMAX, XSTART(PORT) )
            IEND(PORT)   = GEN_BSEARCH( %val(AXIS1_PTR), IMAX, XEND(PORT) )
          ENDIF
          IF ( JMAX .GT. 1 ) THEN
            IF ( (YSTART(PORT).LT.YMIN) .OR. (YSTART(PORT).GT.YMAX) ) YSTART(PORT) = YMIN
            IF ( (YEND(PORT).LT.YMIN) .OR. (YEND(PORT).GT.YMAX) ) YEND(PORT) = YMAX
            JSTART(PORT) = GEN_BSEARCH( %val(AXIS2_PTR), JMAX, YSTART(PORT) )
            JEND(PORT)   = GEN_BSEARCH( %val(AXIS2_PTR), JMAX, YEND(PORT) )
          ENDIF

*     Use pixel limits (whole=false but reset to sensible defaults as necessary)
        ELSE
          CALL CHR_FILL( ' ', TITLE(PORT) )
          TITLE(PORT) = DISPLAY_DATA(PORT)(1:CHR_LEN(DISPLAY_DATA(PORT))) // ' (pixels)'
          IF ( IMAX .GT. 1 ) THEN
            IF ( (ISTART(PORT).LT.IMIN) .OR. (ISTART(PORT).GT.IMAX) ) ISTART(PORT) = IMIN
            IF ( (IEND(PORT).LT.IMIN) .OR. (IEND(PORT).GT.IMAX) ) IEND(PORT) = IMAX
            XSTART(PORT) = REAL( ISTART(PORT) )
            XEND(PORT)   = REAL( IEND(PORT) )
          ENDIF
          IF ( JMAX .GT. 1 ) THEN
            IF ( (JSTART(PORT).LT.JMIN) .OR. (JSTART(PORT).GT.JMAX) ) JSTART(PORT) = JMIN
            IF ( (JEND(PORT).LT.JMIN) .OR. (JEND(PORT).GT.JMAX) ) JEND(PORT) = JMAX
            YSTART(PORT) = REAL( JSTART(PORT) )
            YEND(PORT)   = REAL( JEND(PORT) )
          ENDIF
        ENDIF
      ENDIF

*   At this point we should have a cosher X,Y and I,J
      IF ( VERBOSE ) THEN
         CALL MSG_SETI( 'IMIN', ISTART(PORT) )
         CALL MSG_SETI( 'IMAX', IEND(PORT) )
         CALL MSG_SETR( 'XMIN', XSTART(PORT) )
         CALL MSG_SETR( 'XMAX', XEND(PORT) )
         CALL MSG_OUT( ' ', 'X-axis : Data range = ^XMIN to ^XMAX, Pixel range = ^IMIN to ^IMAX', STATUS )
         CALL MSG_SETI( 'JMIN', JSTART(PORT) )
         CALL MSG_SETI( 'JMAX', JEND(PORT) )
         CALL MSG_SETR( 'YMIN', YSTART(PORT) )
         CALL MSG_SETR( 'YMAX', YEND(PORT) )
         CALL MSG_OUT( ' ', 'Y-axis : Data range = ^YMIN to ^YMAX, Pixel range = ^JMIN to ^JMAX', STATUS )
      ENDIF

*   Reset the limits if necessary
      IF ( IMAX .GT. 1 ) THEN
        IF ( ABS( XSTART(PORT) - XEND(PORT) ) .LT. TOLER ) THEN
          XSTART(PORT) = XSTART(PORT) - 0.5
          XEND(PORT) = XEND(PORT) + 0.5
        ENDIF
      ELSE
        XSTART(PORT) = 0.5
        XEND(PORT) = 1.5
        ISTART(PORT) = 1
        IEND(PORT) = 1
      ENDIF
      IF ( JMAX .GT. 1 ) THEN
        IF ( ABS( YSTART(PORT) - YEND(PORT) ) .LT. TOLER ) THEN
          YSTART(PORT) = YSTART(PORT) - 0.5
          YEND(PORT) = YEND(PORT) + 0.5
        ENDIF
      ELSE
        YSTART(PORT) = 0.5
        YEND(PORT) = 1.5
        JSTART(PORT) = 1
        JEND(PORT) = 1
      ENDIF

*    If we have a contour plot, calculate the transformation matrix
      IF ( DISPLAY_TYPE(PORT) .EQ. 'CONTOUR' )  THEN
         CALL P4_GENTRAN2( ISTART(PORT), IEND(PORT), JSTART(PORT),
     :      JEND(PORT), %val( AXIS1_PTR ), %val( AXIS2_PTR ),
     :      TMATRIX, STATUS )

*    If we have a graph or overgraph, calculate the start and end of slice
      ELSE IF ( INDEX( DISPLAY_TYPE(PORT), 'GRAPH' ) .GT. 0 .AND. DIM2 .GT. 1 ) THEN
         IF ( ( CUT_DIRECTION(PORT)(1:1) .EQ. 'H' ) .OR.
     :        ( CUT_DIRECTION(PORT)(1:1) .EQ. 'h' ) .OR.
     :        ( CUT_DIRECTION(PORT)(1:1) .EQ. 'X' ) .OR.
     :        ( CUT_DIRECTION(PORT)(1:1) .EQ. 'x' ) )  THEN
           CUT_DIRECTION(PORT) = 'X'
           ISLICE_START = GEN_BSEARCH( %val(AXIS2_PTR), JMAX, SLICE_START(PORT) )
           IF ( ISLICE_START .EQ. 0 ) ISLICE_START = JSTART(PORT)
           ISLICE_END = GEN_BSEARCH( %val(AXIS2_PTR), JMAX, SLICE_END(PORT) )
           IF ( ISLICE_END .EQ. 0 ) ISLICE_END = JEND(PORT)
         ELSE IF ( ( CUT_DIRECTION(PORT)(1:1) .EQ. 'V' ) .OR.
     :        ( CUT_DIRECTION(PORT)(1:1) .EQ. 'v' ) .OR.
     :        ( CUT_DIRECTION(PORT)(1:1) .EQ. 'Y' ) .OR.
     :        ( CUT_DIRECTION(PORT)(1:1) .EQ. 'y' ) ) THEN
           CUT_DIRECTION(PORT) = 'Y'
           ISLICE_START = GEN_BSEARCH( %val(AXIS1_PTR), IMAX, SLICE_START(PORT) )
           IF ( ISLICE_START .EQ. 0 ) ISLICE_START = ISTART(PORT)
           ISLICE_END = GEN_BSEARCH( %val(AXIS1_PTR), IMAX, SLICE_END(PORT) )
           IF ( ISLICE_END .EQ. 0 ) ISLICE_END = IEND(PORT)
           XSTART(PORT) = YSTART(PORT)
           XEND(PORT) = YEND(PORT)
           ISTART(PORT) = JSTART(PORT)
           IEND(PORT) = JEND(PORT)
         ENDIF
      ENDIF

*   At this point we should have the final X,Y and I,J
      IF ( VERBOSE ) THEN
         CALL MSG_SETI( 'IMIN', ISTART(PORT) )
         CALL MSG_SETI( 'IMAX', IEND(PORT) )
         CALL MSG_SETR( 'XMIN', XSTART(PORT) )
         CALL MSG_SETR( 'XMAX', XEND(PORT) )
         CALL MSG_OUT( ' ', 'X-axis on exit : Data range = ^XMIN to ^XMAX, Pixel range = ^IMIN to ^IMAX', STATUS )
         CALL MSG_SETI( 'JMIN', JSTART(PORT) )
         CALL MSG_SETI( 'JMAX', JEND(PORT) )
         CALL MSG_SETR( 'YMIN', YSTART(PORT) )
         CALL MSG_SETR( 'YMAX', YEND(PORT) )
         CALL MSG_OUT( ' ', 'Y-axis on exit : Data range = ^YMIN to ^YMAX, Pixel range = ^JMIN to ^JMAX', STATUS )
      ENDIF
      END
