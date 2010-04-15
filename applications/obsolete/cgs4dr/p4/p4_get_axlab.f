*+  P4_GET_AXLAB - Get the axis labels for plot
      SUBROUTINE P4_GET_AXLAB( PORT, SAME, STATUS )
*    Description :
*     This routine gets the axis labels
*    Invocation :
*     CALL P4_GET_AXLAB( PORT, SAME, STATUS )
*    Deficiencies :
*     Datafile must already be open under DSA name 'DATA' and uses values
*     of low, high, mean, sigma and mode from the common block.
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
      LOGICAL SAME                            ! T if axis units are the same
*    External references :
      INTEGER CHR_LEN                         ! String length function
*    Global variables :
      INCLUDE 'P4COM.INC'                     ! P4 common block
*    Local Constants :
      INTEGER NINFO                           ! Maximum number of info items
      PARAMETER ( NINFO = 2 )
*    Local variables :
      CHARACTER*4  ACCESS, COMMENT            ! FITS access
      CHARACTER*32 AXIS1_INFO( NINFO )        ! Labels
      CHARACTER*32 AXIS2_INFO( NINFO )        ! Labels
      CHARACTER*32 DATA_INFO( NINFO )         ! Labels
      DOUBLE PRECISION DIGNORE                ! Return from DSA call
      INTEGER NOBJ, NSKY                      ! FITS items
      INTEGER STRLEN, ELEMENTS                ! FITS access
      INTEGER CPOS, CLEN1, CLEN2              ! String lengths
      LOGICAL EXIST                           ! T if FITS item exists
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise the labels as stored in the common block / noticeboard
      CALL CHR_FILL( ' ', DEFTITLE(PORT) )
      CALL CHR_FILL( ' ', SUBTITLE(PORT) )
      CALL CHR_FILL( ' ', XLABEL(PORT) )
      CALL CHR_FILL( ' ', YLABEL(PORT) )
      CALL CHR_FILL( ' ', RH1LABEL(PORT) )
      CALL CHR_FILL( ' ', RH2LABEL(PORT) )
      CALL CHR_FILL( ' ', RH3LABEL(PORT) )

*    Get some information on the object
      CALL DSA_GET_DATA_INFO( 'DATA', NINFO, DATA_INFO, 0, DIGNORE, STATUS )
      CALL DSA_OBJECT_NAME( 'DATA', DEFTITLE( PORT ), STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

      CLEN1 = CHR_LEN( DEFTITLE( PORT ) )
      CLEN2 = CHR_LEN( DISPLAY_DATA( PORT ) )
      DEFTITLE( PORT ) = DEFTITLE(PORT)(1:CLEN1) // ' ' /
     :   / DISPLAY_DATA(PORT)(1:CLEN2)

      CLEN1 = CHR_LEN( DATA_INFO(1) )
      CLEN2 = CHR_LEN( DATA_INFO(2) )
      SUBTITLE( PORT ) = DATA_INFO(2)(1:CLEN2) // ' (' /
     :   / DATA_INFO(1)(1:CLEN1) // ')'

      CLEN1 = CHR_LEN( SUBTITLE( PORT ) )
      IF ( DISPLAY_PLANE( PORT ) .EQ. 'DATA' ) THEN
        SUBTITLE( PORT ) = SUBTITLE(PORT)(1:CLEN1) // ' [Data,'
      ELSE IF ( DISPLAY_PLANE( PORT ) .EQ. 'ERRORS' ) THEN
        SUBTITLE( PORT ) = SUBTITLE(PORT)(1:CLEN1) // ' [Errors,'
      ELSE IF ( DISPLAY_PLANE( PORT ) .EQ. 'QUALITY' ) THEN
        SUBTITLE( PORT ) = SUBTITLE(PORT)(1:CLEN1) // ' [Quality,'
      ENDIF

*    Now add the number of coadds
      CALL DSA_SEEK_FITS( 'DATA', 'NOBJ', EXIST, ACCESS,
     :   ELEMENTS, STRLEN, STATUS )
      IF ( EXIST ) THEN
        CALL DSA_GET_FITS_I( 'DATA', 'NOBJ', 0, NOBJ, COMMENT, STATUS )
      ELSE
        NOBJ = 0
      ENDIF

      CALL DSA_SEEK_FITS( 'DATA', 'NSKY', EXIST, ACCESS,
     :   ELEMENTS, STRLEN, STATUS )
      IF ( EXIST ) THEN
        CALL DSA_GET_FITS_I( 'DATA', 'NSKY', 0, NSKY, COMMENT, STATUS )
      ELSE
        NSKY = 0
      ENDIF

      CLEN1 = MAX( 1, CHR_LEN( SUBTITLE( PORT ) ) )
      CALL CHR_PUTC( ' Nobj-Nsky=', SUBTITLE(PORT), CLEN1 )
      CALL CHR_PUTI( NOBJ, SUBTITLE(PORT), CLEN1 )
      CALL CHR_PUTC( '-', SUBTITLE(PORT), CLEN1 )
      CALL CHR_PUTI( NSKY, SUBTITLE(PORT), CLEN1 )
      CALL CHR_PUTC( ']', SUBTITLE(PORT), CLEN1 )

*    Get axis info
      CALL DSA_GET_AXIS_INFO( 'DATA', 1, NINFO, AXIS1_INFO,
     :  0, DIGNORE, STATUS )
      CALL DSA_GET_AXIS_INFO( 'DATA', 2, NINFO, AXIS2_INFO,
     :  0, DIGNORE, STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
      IF ( AXIS1_INFO(1) .EQ. AXIS2_INFO(1) ) SAME = .TRUE.

*    Create some labels by string concatenation
      CLEN1 = CHR_LEN( AXIS1_INFO(1) )
      CLEN2 = CHR_LEN( AXIS1_INFO(2) )
      XLABEL( PORT ) = AXIS1_INFO(2)(1:CLEN2) // ' (' /
     :   / AXIS1_INFO(1)(1:CLEN1) // ')'
      CLEN1 = CHR_LEN( AXIS2_INFO(1) )
      CLEN2 = CHR_LEN( AXIS2_INFO(2) )
      YLABEL( PORT ) = AXIS2_INFO(2)(1:CLEN2) // ' (' /
     :   / AXIS2_INFO(1)(1:CLEN1) // ')'

*    Change labels for IMAGE plots
      IF ( DISPLAY_TYPE( PORT ) .EQ. 'IMAGE' ) THEN
         IF ( DISPLAY_PLANE( PORT ) .NE. 'QUALITY' ) THEN
           CPOS = CHR_LEN( XLABEL(PORT) )
           CALL CHR_PUTC( ' [Mean=', XLABEL(PORT), CPOS )
           CALL CHR_PUTR( MEAN( PORT ), XLABEL(PORT), CPOS )
           CALL CHR_PUTC( '; Sigma=', XLABEL(PORT), CPOS )
           CALL CHR_PUTR( SIGMA( PORT ), XLABEL(PORT), CPOS )
           CALL CHR_PUTC( ']', XLABEL(PORT), CPOS )
         ENDIF

      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'HISTOGRAM' ) THEN
         CALL CHR_FILL( ' ', XLABEL(PORT) )
         CPOS = 0
         CALL CHR_PUTC( 'Smooth=', XLABEL(PORT), CPOS )
         CALL CHR_PUTI( HIST_SMOOTH( PORT ), XLABEL(PORT), CPOS )
         CALL CHR_PUTC( ' Mode=', XLABEL(PORT), CPOS )
         CALL CHR_PUTR( MODE( PORT ), XLABEL(PORT), CPOS )
         IF ( DISPLAY_PLANE( PORT ) .NE. 'QUALITY' ) THEN
           CALL CHR_PUTC( ' Mean=', XLABEL(PORT), CPOS )
           CALL CHR_PUTR( MEAN( PORT ), XLABEL(PORT), CPOS )
           CALL CHR_PUTC( ' Sigma=', XLABEL(PORT), CPOS )
           CALL CHR_PUTR( SIGMA( PORT ), XLABEL(PORT), CPOS )
         ENDIF

         CALL CHR_FILL( ' ', YLABEL(PORT) )
         CPOS = 0
         CALL CHR_PUTC( 'Frequency [', YLABEL(PORT), CPOS )
         CALL CHR_PUTI( TOOSMALL(PORT), YLABEL(PORT), CPOS )
         CALL CHR_PUTC( '<Low; ', YLABEL(PORT), CPOS )
         CALL CHR_PUTI( TOOLARGE(PORT), YLABEL(PORT), CPOS )
         CALL CHR_PUTC( '>High]', YLABEL(PORT), CPOS )

         CPOS = 0
         CALL CHR_PUTC( 'X Range: ', RH1LABEL(PORT), CPOS )
         CALL CHR_PUTI( ISTART( PORT ), RH1LABEL(PORT), CPOS )
         CALL CHR_PUTC( '-', RH1LABEL(PORT), CPOS )
         CALL CHR_PUTI( IEND( PORT ), RH1LABEL(PORT), CPOS )
         CALL CHR_PUTC( '  X Step: ', RH1LABEL(PORT), CPOS )
         CALL CHR_PUTI( HISTOGRAM_XSTEP( PORT ), RH1LABEL(PORT), CPOS )

         CPOS = 0
         CALL CHR_PUTC( 'Y Range: ', RH2LABEL(PORT), CPOS )
         CALL CHR_PUTI( JSTART( PORT ), RH2LABEL(PORT), CPOS )
         CALL CHR_PUTC( '-', RH2LABEL(PORT), CPOS )
         CALL CHR_PUTI( JEND( PORT ), RH2LABEL(PORT), CPOS )
         CALL CHR_PUTC( '  Y Step: ', RH2LABEL(PORT), CPOS )
         CALL CHR_PUTI( HISTOGRAM_YSTEP( PORT ), RH2LABEL(PORT), CPOS )

      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'CONTOUR' ) THEN
         CALL CHR_FILL( ' ', XLABEL(PORT) )
         CPOS = 0
         CALL CHR_PUTC( ' Levels=', XLABEL(PORT), CPOS )
         CALL CHR_PUTI( CONTOUR_LEVELS( PORT ), XLABEL(PORT), CPOS )
         CALL CHR_PUTC( '  Mean=', XLABEL(PORT), CPOS )
         CALL CHR_PUTR( MEAN( PORT ), XLABEL(PORT), CPOS )
         CALL CHR_PUTC( '  Sigma=', XLABEL(PORT), CPOS )
         CALL CHR_PUTR( SIGMA( PORT ), XLABEL(PORT), CPOS )

         CALL CHR_FILL( ' ', YLABEL(PORT) )
         CPOS = 0
         CALL CHR_PUTC( 'Range: ', YLABEL(PORT), CPOS )
         CALL CHR_PUTR( ARRAY_CONTOURS(1), YLABEL(PORT), CPOS )
         CALL CHR_PUTC( ' - ', YLABEL(PORT), CPOS )
         CALL CHR_PUTR( ARRAY_CONTOURS(CONTOUR_LEVELS( PORT )), YLABEL(PORT), CPOS )

      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'GRAPH' ) THEN
         CALL CHR_FILL( ' ', XLABEL(PORT) )
         CPOS = 0
         CALL CHR_PUTC( 'Cut along ', XLABEL(PORT), CPOS )
         CALL CHR_PUTC( CUT_DIRECTION( PORT )(1:1), XLABEL(PORT), CPOS )
         CALL CHR_PUTC( '-axis with ', XLABEL(PORT), CPOS )
         IF ( CUT_DIRECTION(PORT)(1:1) .EQ. 'X' ) THEN
           CALL CHR_PUTC( 'Y rows in range ', XLABEL(PORT), CPOS )
         ELSE IF ( CUT_DIRECTION(PORT)(1:1) .EQ. 'Y' ) THEN
           CALL CHR_PUTC( 'X columns in range ', XLABEL(PORT), CPOS )
         ENDIF
         CALL CHR_PUTR( SLICE_START( PORT ), XLABEL(PORT), CPOS )
         CALL CHR_PUTC( ' to ', XLABEL(PORT), CPOS )
         CALL CHR_PUTR( SLICE_END( PORT ), XLABEL(PORT), CPOS )

         CALL CHR_FILL( ' ', YLABEL(PORT) )
         CPOS = 0
         CALL CHR_PUTC( DATA_INFO(1), YLABEL(PORT), CPOS )

      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'OVERGRAPH' ) THEN
         CALL CHR_FILL( ' ', RH1LABEL(PORT) )
         RH1LABEL( PORT ) = DEFTITLE( PORT )
         CPOS = CHR_LEN( RH1LABEL(PORT) ) + 1
         CALL CHR_PUTC( CUT_DIRECTION( PORT )(1:1), RH1LABEL(PORT), CPOS )
         CALL CHR_PUTC( '-cut with ', RH1LABEL(PORT), CPOS )
         IF ( CUT_DIRECTION(PORT)(1:1) .EQ. 'X' ) THEN
           CALL CHR_PUTC( 'Y=', RH1LABEL(PORT), CPOS )
         ELSE IF ( CUT_DIRECTION(PORT)(1:1) .EQ. 'Y' ) THEN
           CALL CHR_PUTC( 'X=', RH1LABEL(PORT), CPOS )
         ENDIF
         CALL CHR_PUTR( SLICE_START( PORT ), RH1LABEL(PORT), CPOS )
         CALL CHR_PUTC( '-', RH1LABEL(PORT), CPOS )
         CALL CHR_PUTR( SLICE_END( PORT ), RH1LABEL(PORT), CPOS )

      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'SURFACE' ) THEN
         IF ( DISPLAY_PLANE( PORT ) .NE. 'QUALITY' ) THEN
           CPOS = CHR_LEN( XLABEL(PORT) )
           CALL CHR_PUTC( ' [Mean=', XLABEL(PORT), CPOS )
           CALL CHR_PUTR( MEAN( PORT ), XLABEL(PORT), CPOS )
           CALL CHR_PUTC( '; Sigma=', XLABEL(PORT), CPOS )
           CALL CHR_PUTR( SIGMA( PORT ), XLABEL(PORT), CPOS )
           CALL CHR_PUTC( ']', XLABEL(PORT), CPOS )
         ENDIF

         CALL CHR_FILL( ' ', YLABEL(PORT) )
         CPOS = 0
         CALL CHR_PUTC( DATA_INFO(1), YLABEL(PORT), CPOS )

         CPOS = 0
         CALL CHR_PUTC( ' J Range: ', RH1LABEL(PORT), CPOS )
         CALL CHR_PUTI( JSTART( PORT ), RH1LABEL(PORT), CPOS )
         CALL CHR_PUTC( ' - ', RH1LABEL(PORT), CPOS )
         CALL CHR_PUTI( JEND( PORT ), RH1LABEL(PORT), CPOS )
      ENDIF
      END
