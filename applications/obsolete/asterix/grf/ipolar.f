*+  IPOLAR - Produce a polar surface brightness profile of an image
      SUBROUTINE IPOLAR( STATUS )
*
*    Description :
*
*     For a 2D image dataset, produce a 1D binned dataset containing
*     surface brightness vs radius or a 2D binned dataset containing
*     surface brightness vs radius and azimuth.
*
*    Parameters :
*     INP     CHAR    Name of input data structure
*     OUT     CHAR    Name of output data structure
*     XCENT   REAL    X value of centre of polar
*     YCENT   REAL    Y value of centre of polar
*     RBIN    REAL    Size of radial bins
*     ABIN    REAL    Size of azimuthal bins
*
*    Method :
*
*     Check class of object
*     Obtain user requirements
*     Map input data
*     Calculate number of polar bins
*     Set up output object and map
*     Rebin data into polar bins
*     Convert to surface brightness
*     Unmap data
*     Update history
*     Tidy up
*
*    Deficiencies :
*
*     Algorithm to put data into polar bins is not exact
*
*    Bugs :
*    Authors :
*
*     Chris Eyles (BHVAD::CJE)
*     Richard Saxton (LTVAD::RDS)
*
*    History :
*
*     29 Jul 86 : Original (BHVAD::CJE)
*      4 Mar 87 : V0.6-1 Check for zero errors in REB_POLE
*                        Handle correctly situation where input dataset
*                        has axis1_ or axis2_units undefined
*                        Zero output arrays in REB_POL & REB_POLE (CJE)
*     25 Mar 89 : Modified to run under ISIS (RDS)
*     25 May 89 : Asterix88 version (RDS)
*      8 Jun 90 : Bug fix (bad arguments to ATAN2) (RJV)
*      8 Jun 90 : Output axes handled properly (RJV)
*      6 Aug 91 : V1.2-3 Updated to allow an azimuthal start angle to be input,
*                        output units have been sorted out and history info.
*                        increased. (RDS)
*      7 May 93 : V1.7-0 Extra STATUS removed from MSG_SETx calls (DJA)
*     26 Jul 93 : V1.7-1 Handles cubes (DJA)
*      1 Oct 93 : V1.7-2 Half-pixel error corrected (DJA)
*     21 Feb 94 : V1.7-3 IPOLAR_DOIT became IMG_POLAR (DJA)
*      3 Mar 94 : V1.7-4 Corrected axis values for AZSTART .NE. 0 (RJV)
*      6 Jul 94 : V1.7-5 Optional limit on radial extent (RJV)
*                        Also switch off even scaling of 2D axes (RJV)
*     16 Sep 94 : V1.7-6 NORM parameter (RJV)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER  CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ILOC	! input object locator
      CHARACTER*(DAT__SZLOC) OLOC	! output object locator
      CHARACTER*(80) PATH(8) 		! History input axis2-units
      CHARACTER*(80) TITLE 		! Title of files
      CHARACTER*40 AUNITS(2)            ! Units of axes
      CHARACTER*60 DUNITS               ! Units of the data

      DOUBLE PRECISION UNITFACT         ! Conversion factor between pixels
                                        ! and square axis units

      REAL          ABIN		! Azimuth bin size
      REAL          AMIN(2),AMAX(2)     ! First and last elements of axes
      REAL          ASCALE(2)           ! Width of each spatial axis bin
      REAL          AZANG               ! Initial azimuth angle (degrees)
      REAL          DIST		! Distance from centre to edge
      REAL          PXCENT,PYCENT	! Coords of centre of polar region in
                                        ! pixels
      REAL          PRBIN		! Radial bin size in pixels
      REAL          PSIZE               ! Pixel size (square axis units)
      REAL 	    RBIN		! Radial bin size in axis units
      REAL          XCENT,YCENT		! Coords of centre of polar region in
                                        ! axis units
      REAL          XLOW,XHIGH,         ! Max and min values of axes
     :              YLOW,YHIGH

      INTEGER       AXLP                ! Loop over input axes
      INTEGER       AZAX, RAX           ! Output axis id's
      INTEGER       DIMS(DAT__MXDIM)    ! Dimensions of input data_array
      INTEGER       IDPTR               ! Input data
      INTEGER       IVPTR               ! input variance
      INTEGER       IQPTR               ! Input quality
      INTEGER       INELM               ! Input number of elements
      INTEGER       INDIM		! # of dimensions of input data_array
      INTEGER       K                   ! Length of strings
      INTEGER       NABIN,NRBIN		! # of azimuthal & radial bins
      INTEGER       NRAD		! limit on # of radial bins
      INTEGER       NLINES              ! # lines of history text
      INTEGER       NVAL                ! # values in an axis
      INTEGER       ODIM(3)             ! Output dimensions (1=radial,2=az)
      INTEGER       ONDIM               ! Output dimensionality
      INTEGER       ODPTR               ! Output data
      INTEGER       OVPTR               ! Output variance
      INTEGER       OQPTR               ! Output quality
      INTEGER       VNDIM,              ! Dimensionality of input variance
     :              VDIMS(DAT__MXDIM)   ! Dimensions of input variance
      INTEGER       WPTR                ! Workspace for polaring process

      BYTE          BADBITS             ! Quality mask

      LOGICAL       CUBE                ! Cube input?
      LOGICAL       OK			! Object is in D_A subclass
      LOGICAL       INPRIM 		! Primitive array ?
      LOGICAL       ERRORS, REG
      LOGICAL       NORM                ! Normalise to axis units
*
*    Version id :
*
      CHARACTER*30 VERSION
        PARAMETER  ( VERSION = 'IPOLAR Version 1.7-6' )
*-

*    Version id :
      CALL MSG_PRNT( VERSION )

*    Initialise the Asterix88 common block
      CALL AST_INIT()

*    Get input and output data paths
      CALL USI_ASSOC2( 'INP', 'OUT', 'READ', ILOC, OLOC,
     :                                  INPRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map the input data array if present
      CALL BDA_CHKDATA( ILOC, OK, INDIM, DIMS, STATUS )
      CALL ARR_SUMDIM( INDIM, DIMS, INELM )

*    Check dimensionality.
      IF ( (INDIM .NE. 2) .AND. (INDIM.NE.3) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input must be 2 or 3 dimensional', STATUS )
        GOTO 99
      END IF
      CUBE = (INDIM.EQ.3)
      IF ( CUBE ) THEN
        CALL MSG_PRNT( 'Assuming spatial dimension are numbers'/
     :                                             /' 1 and 2' )
        CALL DAT_RETYP( OLOC, 'PROFILE_SET', STATUS )
      END IF

      IF ( OK ) THEN
        CALL BDA_MAPDATA( ILOC, 'READ', IDPTR, STATUS )
      ELSE
        CALL ERR_REP(' ','Data array not found in input file',STATUS)
        GOTO 99
      END IF
*
        CALL BDA_CHKVAR( ILOC, ERRORS, VNDIM, VDIMS, STATUS )
        CALL BDA_MAPVAR( ILOC, 'READ', IVPTR, STATUS )
*
        IF (STATUS.NE.SAI__OK) THEN
           CALL MSG_PRNT('Error mapping input variance array')
           GOTO 99
        ENDIF

*    Map quality array - if not present all qualities are set good
      CALL BDA_MAPQUAL( ILOC, 'READ', IQPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT( 'Error mapping input quality array' )
        GOTO 99
      END IF

*    Attempt to get badbits value
      CALL BDA_GETMASK( ILOC, BADBITS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         BADBITS = QUAL__MASK
      END IF

*    Find axis values if present
      DO AXLP = 1, 2
        CALL BDA_CHKAXVAL( ILOC, AXLP, OK, REG, NVAL, STATUS )

*      Is it regular ?
        IF ( REG ) THEN
          CALL BDA_GETAXVAL( ILOC, AXLP, AMIN(AXLP), ASCALE(AXLP),
     :                                              NVAL, STATUS )
          AMAX(AXLP) = AMIN(AXLP)+(NVAL-1)*ASCALE(AXLP)
        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETI( 'N', AXLP )
          CALL ERR_REP( ' ', 'Axis number ^N is not regular', STATUS )
        END IF
      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Tell user the axes ranges
      CALL MSG_SETR( 'XMIN',AMIN(1) )
      CALL MSG_SETR( 'XMAX',AMAX(1) )
      CALL MSG_PRNT( 'X Axis range: ^XMIN to ^XMAX' )
      CALL MSG_SETR( 'YMIN',AMIN(2) )
      CALL MSG_SETR( 'YMAX',AMAX(2) )
      CALL MSG_PRNT( 'Y Axis range: ^YMIN to ^YMAX' )

*    Because an axis may increase in any direction find max values.
      XLOW = MIN( AMIN(1), AMAX(1) )
      XHIGH = MAX( AMIN(1), AMAX(1) )
      YLOW = MIN( AMIN(2), AMAX(2) )
      YHIGH = MAX( AMIN(2), AMAX(2) )

*    Get central position
*
      CALL USI_GET0R('XCENT',XCENT,STATUS)
*
*
*  Test if this is within the image.
      IF (XCENT .LE. XLOW .OR. XCENT .GT. XHIGH.AND.STATUS.EQ.SAI__OK)
     :                                                            THEN
         CALL MSG_PRNT('XCENT is outside range of image')
         STATUS=SAI__ERROR
      ENDIF

      IF (STATUS .NE. SAI__OK) GOTO 99
*
*  Get Y centre
      CALL USI_GET0R('YCENT',YCENT,STATUS)
*
*
*  Test if this is within the image (pixel co-ords only)
      IF (YCENT .LE. YLOW .OR. YCENT .GT. YHIGH.AND.STATUS.EQ.SAI__OK)
     :                                                            THEN
         CALL MSG_PRNT('YCENT is outside range of image')
         STATUS=SAI__ERROR
      ENDIF

      IF (STATUS .NE. SAI__OK) GOTO 99

*    Find pixel position of the polar centre. This is measured in fractional
*    pixels from the extreme "left" and "bottom" edges of the 2d area, ie.
*    PXCENT runs from 0.0 to REAL(DIMS(1)).
      PXCENT = (XCENT-AMIN(1)) / ASCALE(1) + 0.5
      PYCENT = (YCENT-AMIN(2)) / ASCALE(2) + 0.5

*    Get radial and azimuthal binsizes
      CALL USI_GET0R('RBIN',RBIN,STATUS)
      CALL USI_GET0R('ABIN',ABIN, STATUS)

*    Normalise to axis units or /pixel?
      CALL USI_GET0L('NORM',NORM,STATUS)

      IF (STATUS .NE. SAI__OK) GOTO 99

*   Calculate number of output bins in each dimension
*    Find closest border distance in axis units
      DIST = MIN( (XHIGH-XCENT), (XCENT-XLOW), (YHIGH-YCENT),
     :                                              (YCENT-YLOW) )
*   Calc max number of radial bins.
      NRBIN=INT(DIST/RBIN)
*
*   Only 1 radial bin
      IF (NRBIN .LT. 1) THEN
        NRBIN=1
        RBIN=DIST/NRBIN
      ELSE
*   See if limit on radial extent
        CALL USI_DEF0I('NRAD',NRBIN,STATUS)
        CALL USI_GET0I('NRAD',NRAD,STATUS)
        IF (NRAD.LT.NRBIN) THEN
          NRBIN=NRAD
        ELSE
          RBIN=DIST/NRBIN
        ENDIF
      ENDIF
*
*
*   Calc radial binsize in pixels
      PRBIN=ABS(RBIN/ASCALE(1))
*
*   Calc number of azimuthal bins
      NABIN=NINT(360./ABIN)
*
      IF (NABIN .LT. 1) NABIN=1
*
      ABIN=360./REAL(NABIN)

*    Tell user the binning being used.
      CALL MSG_SETI('NRBIN', NRBIN)
      IF ( NABIN .EQ. 1 ) THEN
        CALL MSG_PRNT( 'Rebinning into ^NRBIN radial bins' )
      ELSE
        CALL MSG_SETI( 'NABIN', NABIN )
	CALL MSG_PRNT( 'Rebinning into ^NRBIN radial and ^NABIN'//
     :                                         ' azimuthal bins' )
      END IF

*    Create and map output data, variance and quality arrays.
      ODIM(1) = NRBIN
      ODIM(2) = NABIN
      RAX = 1
      AZAX = 2
      IF ( NRBIN .EQ. 1 ) THEN
        ONDIM = 1
        AZAX = 1
        ODIM(AZAX) = NABIN
      ELSE IF ( NABIN .EQ. 1 ) THEN
        ONDIM = 1
      ELSE
        ONDIM = 2
      END IF
      IF ( CUBE ) THEN
        ONDIM = ONDIM + 1
        ODIM(ONDIM) = DIMS(INDIM)
      END IF

*    Create and map output arrays
      CALL BDA_CREDATA( OLOC, ONDIM, ODIM, STATUS )
      CALL BDA_CREVAR( OLOC, ONDIM, ODIM, STATUS )
      CALL BDA_CREQUAL( OLOC, ONDIM, ODIM, STATUS )
      CALL BDA_MAPDATA( OLOC, 'WRITE', ODPTR, STATUS )
      CALL BDA_MAPVAR( OLOC, 'WRITE', OVPTR, STATUS )
      CALL BDA_MAPQUAL( OLOC, 'WRITE', OQPTR, STATUS )

*    Write in badbits mask
      CALL BDA_PUTMASK(OLOC, BADBITS, STATUS)

      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error creating output data, variance'/
     :                   /' and quality')
        GOTO 99
      END IF

*   Write ancilliary objects to new file.
*    Copy More structure
      CALL BDA_COPMORE( ILOC, OLOC, STATUS )

*    Make sure even axis scaling switched off
      CALL GCB_ATTACH('LOCAL',STATUS)
      CALL GCB_SETL('AXES_SCALING',.FALSE.,STATUS)
      CALL GCB_SAVE(OLOC,STATUS)
      CALL GCB_DETACH(STATUS)

*    Create axis structure. Either one axis or two.
      CALL BDA_CREAXES( OLOC, ONDIM, STATUS )

*    Get the data units of the input file
      CALL BDA_GETUNITS( ILOC, DUNITS, STATUS )

*    Get axis units
      DO AXLP = 1, 2
        CALL BDA_GETAXUNITS( ILOC, AXLP, AUNITS(AXLP), STATUS )
      END DO

*    Check that the units of each axis are the same
      K = CHR_LEN(AUNITS(1))
*

        IF (AUNITS(1)(1:K) .NE. AUNITS(2)(1:K) .OR. STATUS .NE.
     :               SAI__OK.OR..NOT.NORM) THEN
*
*     Set units to be per pixel
           DUNITS = DUNITS(1:CHR_LEN(DUNITS)) // '/pixel'
*
           CALL MSG_SETC('UNIT', DUNITS)
           IF (NORM) THEN
             CALL MSG_PRNT('Axis units are not the same - output data'/
     :                  /' will be in ^UNIT')
           ELSE
             CALL MSG_PRNT('Output data will be in ^UNIT')
           ENDIF
           UNITFACT = 1.0
*
*     Annul status in case that was the problem
           CALL ERR_ANNUL(STATUS)
*
        ELSE
*
*     Set units to be per whatever the axis units were squared
           DUNITS = DUNITS(1:CHR_LEN(DUNITS)) // ' /' //
     :        AUNITS(1)(1:K) //'**2'
*
           CALL MSG_SETC('UNIT', DUNITS)
           CALL MSG_PRNT('Output data will be in ^UNIT')
           UNITFACT = ABS( ASCALE(1) * ASCALE(2) )
*
        ENDIF
*

*    If an azimuthal axis is wanted get the start position from
*    the user
      IF ( NABIN .NE. 1 ) THEN
        CALL USI_GET0R('AZSTART', AZANG, STATUS)
        IF (STATUS .NE. SAI__OK) GOTO 99
      ELSE
        AZANG = 0.0
      END IF

*    Want a radial axis ?
      IF ( NRBIN .NE. 1 ) THEN

*      Write axis label and units
        CALL BDA_PUTAXLABEL( OLOC, RAX, 'Radius', STATUS )

*      Put output input axis units
        CALL BDA_PUTAXUNITS( OLOC, RAX, AUNITS(1), STATUS )

*      Create and map axis array
        CALL BDA_CREAXVAL( OLOC, RAX, .TRUE., NRBIN, STATUS )
        CALL BDA_PUTAXVAL( OLOC, RAX, RBIN/2.0, RBIN, NRBIN, STATUS )

      END IF

*    Create an azimuth axis?
      IF ( NABIN .NE. 1 ) THEN

*      Write axis label and units
        CALL BDA_PUTAXLABEL( OLOC, AZAX, 'Azimuth', STATUS )
        CALL BDA_PUTAXUNITS( OLOC, AZAX, 'degrees', STATUS )

*      Create and map axis array
        CALL BDA_CREAXVAL( OLOC, AZAX, .TRUE., NABIN, STATUS )
        CALL BDA_PUTAXVAL( OLOC, AZAX, AZANG+ABIN/2.0, ABIN, NABIN,
     :                                                      STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Error creating axes', STATUS )
        GOTO 99
      END IF

*    Copy third input axis if a cube
      IF ( CUBE ) THEN
        CALL BDA_COPAXIS( ILOC, OLOC, 3, ONDIM, STATUS )
      END IF

*    Write data units to output file
      CALL BDA_PUTUNITS( OLOC, DUNITS, STATUS )

*    Write label
      CALL BDA_PUTLABEL( OLOC, 'Surface brightness', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Error writing data units and label',
     :                                                  STATUS )
        GOTO 99
      END IF

*    Map a temporary workspace array for IMG_POLAR
      CALL DYN_MAPI( 1, NRBIN*NABIN, WPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Produce the polar, looping over all 2D slices. Note that we modify the
*    pointers here which is ok as long as we don't use them after the loop
      DO K = 1, INELM/(DIMS(1)*DIMS(2))

*      If pixels are square we can use the fast method.
*      Check if the deviation from squareness has a less than one pixel
*      effect on the image
        IF ( ABS(1 - ABS(ASCALE(1)/ASCALE(2))) .GE.
     :              ( 1.0 / MAX ( DIMS(1), DIMS(2) )) ) THEN
          IF ( K .EQ. 1 ) THEN
            CALL MSG_PRNT('** Image pixels are not square - the '/
     :                    /'algorithm used may be inaccurate **')
          END IF
        END IF

*      Polar this slice
        CALL IMG_POLAR( DIMS(1), DIMS(2), %VAL(IDPTR), ERRORS,
     :                  %VAL(IVPTR), .TRUE., %VAL(IQPTR), BADBITS,
     :                  UNITFACT, PXCENT, PYCENT, PRBIN, AZANG,
     :                  NRBIN, NABIN, %VAL(WPTR), %VAL(ODPTR),
     :                  %VAL(OVPTR), %VAL(OQPTR), STATUS )

*      Advance to next input slice
        IDPTR = IDPTR + DIMS(1)*DIMS(2)*VAL__NBR
        IVPTR = IVPTR + DIMS(1)*DIMS(2)*VAL__NBR
        IQPTR = IQPTR + DIMS(1)*DIMS(2)*VAL__NBUB

*      Advance to next output slice
        ODPTR = ODPTR + NRBIN*NABIN*VAL__NBR
        OVPTR = OVPTR + NRBIN*NABIN*VAL__NBR
        OQPTR = OQPTR + NRBIN*NABIN*VAL__NBUB

      END DO

*    Get title of the input file
      CALL BDA_GETTITLE(ILOC, TITLE, STATUS)
      TITLE = TITLE(1:CHR_LEN(TITLE)) // ' - polar distribution'

*    Put title to the output file
      CALL BDA_PUTTITLE( OLOC, TITLE, STATUS )

*    Copy and update history file
      CALL HIST_COPY( ILOC, OLOC, STATUS )
      CALL HIST_ADD( OLOC, VERSION, STATUS )

*    Create text strings for history
      CALL USI_NAMEI( NLINES, PATH, STATUS )

        WRITE(PATH(NLINES+1),1000) XCENT,YCENT
1000    FORMAT(' Polar centred on the point X=',F8.2,' Y=',F8.2)
*
*   Write azimuthal start position if required.
        IF ( NABIN .NE. 1) THEN
           WRITE(PATH(NLINES+2),1010)AZANG
1010       FORMAT(' Initial azimuthal angle ',F7.3,' degrees')
        ENDIF
*
        PSIZE = ABS( ASCALE(1) * ASCALE(2) )
        WRITE(PATH(NLINES+3),1020) PSIZE, AUNITS(1)(1:K)
1020    FORMAT(' Original pixel size ',E10.4, 'square', A)
*

*    Write strings to history structure
      CALL HIST_PTXT( OLOC, NLINES+3, PATH, STATUS )

*    Annul and unmap data accessed by asterix routines.
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
