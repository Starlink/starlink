*+
      SUBROUTINE RED4_IRFLAT( STATUS )
*
*     R E D 4 _ I R F L A T
*
*     Figaro function that produces a "flatfield" ripple
*     spectrum from a infrared  spectrum, by averaging the data from
*     regions of the spectrum uncontaminated with spectral features
*     (i.e. assumed flat) to determine the relative response of each
*     detector or scan. The output spectrum can be divided into the original
*     spectrum using IDIV to flat field the data.
*
*     The program is used to remove two kinds of ripple from spectra.
*     In instruments which interleave a number of scan positions to give
*     a fully sampled spectrum (such as CGS3 and CGS4), the program removes
*     ripple which results from seeing or transparency fluctuations between
*     scan positions. In an instrument such as CGS2 it can remove the ripple
*     which results from the fact that the flatfield (i.e. relative detector
*     responses) is different for extended and point sources. In the case of
*     CGS2 data it makes use of a .MORE.PIXELS extension in the data which
*     specifies the detector and scan position corresponding to each pixel.
*     If this structure is not present it prompts for a period and assumes
*     a periodic ripple. The period will normally be the oversampling factor,
*     typically 2 or 3 for CGS4 or CGS3 data.
*
*     If the program is run in batch only one region can be specified.
*     Multiple regions can only be specified in interactive mode.
*
*     Command parameters -
*
*     SPECTRUM    (Character) The name of the file containing the
*                 spectrum to be used.
*     OUTPUT      (Character) The name of the resulting ripple spectrum.
*     XSTART      (Real) First X value for region to be used.
*     XEND        (Real) Second X value for region to be used.
*
*     Command keywords -
*
*     MORE        If TRUE the prompts for XSTART and XEND are repeated for
*                 another region.
*
*     10th Dec 1990 - JAB / JAC
*
*     Modified:
*         May 15th 1991  -  Add handling for seeing ripple in CGS4 data.
*         May 11th 1992  -  Converted to RED4 task under ADAM (PND / JAC)
*         Feb 22nd 1993  -  Conform to error strategy         (PND / JAC)
*         May 10th 1993  -  Add check for dims > 1            (PND / JAC)
*
*-
      IMPLICIT NONE

*   ADAM definitions etc
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'RED4_COMMON.INC'

*   Functions used
      INTEGER DSA_TYPESIZE

*   Local variables
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      IXST         ! start pixel for flatfielding
      INTEGER      IXEN         ! end pixel for flatfielding
      LOGICAL      MORE         ! TRUE if more ranges
      INTEGER      ND           ! Number of pixels
      INTEGER      NS           ! Number of scan positions
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      STATUS       ! Running status for ADAM routines
      INTEGER      DSA_STATUS   ! Running status for DSA_ routines
      INTEGER      DSA__OK      ! Good status for DSA_ routines
      INTEGER      DPTR         ! Dynamic-memory pointer to detector numbers
      INTEGER      DSLT         ! Dynamic-memory slot to detector numbers
      INTEGER      EPTR         ! Dynamic-memory pointer to error array
      INTEGER      ESLT         ! Dynamic-memory slot to error array
      INTEGER      NPTR         ! Dynamic-memory pointer to NUM array
      INTEGER      NSLT         ! Dynamic-memory slot to NUM array
      INTEGER      OPTR 	! Dynamic-memory pointer to output data array
      INTEGER      OSLT 	! Dynamic-memory slot to output data array
      INTEGER      QPTR         ! Dynamic-memory pointer to quality array
      INTEGER      QSLT         ! Dynamic-memory slot to quality array
      INTEGER      SPTR         ! Dynamic-memory pointer to scan positions
      INTEGER      SSLT         ! Dynamic-memory slot to scan positions
      INTEGER      SUMPTR       ! Dynamic-memory pointer to SUM array
      INTEGER      SUMSLT       ! Dynamic-memory slot to SUM array
      INTEGER      UPTR         ! Dynamic-memory pointer to USES array
      INTEGER      USLT         ! Dynamic-memory slot to USES array
      CHARACTER*80 OUTPUT       ! Name of output
      CHARACTER*80 SPECTRUM     ! Name of spectrum
      CHARACTER*80 STRING       ! Output string
      REAL         DETINCR      ! FITS item - detector oversampling translation
      REAL         XS           ! Starting X value
      REAL         XE           ! Ending X value
*   Parameters
      PARAMETER ( DSA__OK = 0 )

*   Check STATUS on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Initialisation of DSA_ routines
      DSA_STATUS = DSA__OK
      CALL DSA_OPEN( DSA_STATUS )
      IF ( DSA_STATUS .NE. DSA__OK ) GOTO 500

*   Get input and output
      CALL PAR_GET0C( 'SPECTRUM', SPECTRUM, STATUS )
      CALL PAR_GET0C( 'OUTPUT', OUTPUT, STATUS )
      IF ( STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_IRFLAT: '/
     :        /'Failed to get input or output', STATUS )
         RETURN
      ENDIF

*   Open named input stream
      CALL RED4_CHECK_INPUT( SPECTRUM, STATUS )
      CALL DSA_NAMED_INPUT( 'SPECTRUM', SPECTRUM, DSA_STATUS )
      CALL DSA_DATA_SIZE( 'SPECTRUM', 1, NDIM, DIMS, NX, DSA_STATUS )
      IF ( DSA_STATUS .NE. DSA__OK ) GOTO 500
      IF ( NDIM .GT. 1 ) GOTO 500

*   Open named output stream
      CALL DSA_NAMED_OUTPUT( 'OUTPUT', OUTPUT, 'SPECTRUM',
     :     0, 1, DSA_STATUS )
      IF ( STATUS .NE. DSA__OK ) GOTO 500
      CALL DSA_USE_QUALITY( 'OUTPUT', DSA_STATUS )

*   Find value of period
      CALL DSA_GET_FITS_F( 'SPECTRUM', 'DETINCR', 0, DETINCR,
     :     STRING, DSA_STATUS )

      ND = NINT( 1.0 / DETINCR )

      IF ( VERBOSE ) THEN
         CALL MSG_SETI( 'ND', ND )
         CALL MSG_OUT( ' ', 'RED4_IRFLAT: '/
     :     /'The period of the ripple is ^ND', STATUS )
      ENDIF

      NS = NX/ND

      CALL DSA_GET_WORK_ARRAY( NX, 'INT', SPTR, SSLT, DSA_STATUS )
      CALL DSA_GET_WORK_ARRAY( NX, 'INT', DPTR, DSLT, DSA_STATUS )
      CALL FIG_IRFLAT_FILL( NX, ND, NS, %val(SPTR), %val(DPTR) )

*   Force creation of error array by mapping it, then zero it
      CALL DSA_MAP_ERRORS( 'OUTPUT', 'UPDATE', 'FLOAT',
     :     EPTR, ESLT, DSA_STATUS)
      CALL GEN_FILL( NX*DSA_TYPESIZE('FLOAT',STATUS), 0, %val(EPTR) )

*   Map data  and quality array
      CALL DSA_MAP_DATA( 'OUTPUT', 'UPDATE', 'FLOAT',
     :     OPTR, OSLT, DSA_STATUS )
      CALL DSA_MAP_QUALITY( 'OUTPUT', 'UPDATE', 'BYTE',
     :     QPTR, QSLT, DSA_STATUS )
      IF ( DSA_STATUS .NE. DSA__OK ) GOTO 500

*   Select initial region
      CALL MSG_OUT(' ', 'RED4_IRFLAT: Select regions to be used to '/
     :    /'generate flat field spectrum ', STATUS )
      CALL DSA_AXIS_RANGE( 'SPECTRUM', 1, ' ', .FALSE., XS, XE,
     :     IXST, IXEN, DSA_STATUS )

*   Get workspace arrays
      CALL DSA_GET_WORK_ARRAY( NX, 'INT', UPTR, USLT, DSA_STATUS )
      CALL DSA_GET_WORK_ARRAY( ND, 'FLOAT', SUMPTR, SUMSLT, DSA_STATUS )
      CALL DSA_GET_WORK_ARRAY( ND, 'INT', NPTR, NSLT, DSA_STATUS )
      CALL GEN_FILL( ND*DSA_TYPESIZE('FLOAT',STATUS), 0, %val(SUMPTR) )
      CALL GEN_FILL( ND*DSA_TYPESIZE('INT',STATUS), 0, %val(NPTR) )

*   Operate on data
      CALL FIG_IRFLAT_FOLD( %val(OPTR), %val(QPTR), NX, IXST, IXEN,
     :     %val(DPTR), %val(SPTR), %val(UPTR), NS, ND,
     :     %val(SUMPTR), %val(NPTR) )

*   Repeat for more ranges if necessary
      CALL PAR_RDKEY( 'MORE', .FALSE., MORE )
      DO WHILE (MORE)
          CALL PAR_RDKEY( 'MORE', .FALSE., MORE )
          CALL PAR_CNPAR( 'MORE' )
          IF ( MORE ) THEN
              CALL PAR_CNPAR( 'XSTART' )
              CALL PAR_CNPAR( 'XEND' )
              CALL DSA_AXIS_RANGE( 'SPECTRUM', 1, ' ', .FALSE., XS, XE,
     :             IXST, IXEN, DSA_STATUS )
              CALL FIG_IRFLAT_FOLD( %val(OPTR), %val(QPTR), NX, IXST,
     :             IXEN, %val(DPTR), %val(SPTR), %val(UPTR), NS, ND,
     :             %val(SUMPTR), %val(NPTR) )
          ENDIF
      ENDDO

*   Make the flat field spectrum
      CALL FIG_IRFLAT_WORK( ND, %val(SUMPTR), %val(NPTR),
     :     NX, %val(DPTR), %val(OPTR) )

*   Tidy up
  500 CONTINUE

*   Closedown everything
      CALL DSA_CLOSE( DSA_STATUS )
      END
