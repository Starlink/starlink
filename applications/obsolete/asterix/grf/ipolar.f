      SUBROUTINE IPOLAR( STATUS )
*+
*  Name:
*     IPOLAR

*  Purpose:
*     Produce a polar surface brightness profile of an image, or image stack

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL IPOLAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     For each spatial plane in the input dataset produces either a 1D
*     binned dataset containing surface brightness versus radius or a
*     2D binned dataset containing surface brightness versus radius and
*     azimuth.

*  Usage:
*     ipolar {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input dataset to be polared
*     OUT = CHAR (read)
*        Name of output dataset
*     XCENT = REAL (read)
*        X value of centre of polar
*     YCENT = REAL (read)
*        Y value of centre of polar
*     RBIN = REAL (read)
*        Size of radial bins
*     NRAD = INTEGER (read)
*        Number of radial bins
*     ABIN = REAL (read)
*        Size of azimuthal bins
*     AZSTART = REAL (read)
*        Start azimuth for azimuthal bins
*     NORM = LOGICAL (read)
*        Normalise output dataset wrt input pixel area

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
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

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     ipolar, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     CJE: Chris Eyles (University of Birmingham)
*     RDS: Richard Saxton (Starlink, University of Leicester)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     29 Jul 1986 V0.6-0 (CJE):
*        Original Version
*      4 Mar 1987 V0.6-1 (CJE):
*        Check for zero errors in REB_POLE. Handle correctly situation where
*        input dataset has axis1_ or axis2_units undefined. Zero output
*        arrays in REB_POL & REB_POLE
*     25 Mar 1989 (RDS):
*        Modified to run under ISIS
*     25 May 1989 V1.2-0 (RDS):
*        Asterix88 version
*      8 Jun 1990 V1.2-1 (RJV):
*        Bug fix (bad arguments to ATAN2)
*      8 Jun 1990 V1.2-2 (RJV):
*        Output axes handled properly
*      6 Aug 1991 V1.2-3 (RDS):
*        Updated to allow an azimuthal start angle to be input,
*        output units have been sorted out and history info increased
*      7 May 1993 V1.7-0 (DJA):
*        Extra STATUS removed from MSG_SETx calls
*     26 Jul 1993 V1.7-1 (DJA):
*        Handles cubes
*      1 Oct 1993 V1.7-2 (DJA):
*        Half-pixel error corrected
*     21 Feb 1994 V1.7-3 (DJA):
*        IPOLAR_DOIT became IMG_POLAR
*      3 Mar 1994 V1.7-4 (RJV):
*        Corrected axis values for AZSTART .NE. 0
*      6 Jul 1994 V1.7-5 (RJV):
*        Optional limit on radial extent.
*        Also switch off even scaling of 2D axes.
*     16 Sep 1994 V1.7-6 (RJV):
*        NORM parameter
*     24 Apr 1995 V1.8-0 (DJA):
*        Updated data interfaces
*      9 Nov 1995 V2.0-0 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER  		CHR_LEN

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'IPOLAR Version V2.0-0' )

*  Local Variables:
      CHARACTER*40 		AUNITS(2)            	! Units of axes
      CHARACTER*60 		DUNITS               	! Units of the data
      CHARACTER*80		PATH(8) 		! History input axis2-units
      CHARACTER*80 		TITLE 			! Title of files

      DOUBLE PRECISION 		UFACT         		! Conversion factor between pixels
							! and square axis units

      REAL          		ABIN			! Azimuth bin size
      REAL          		AMIN(2),AMAX(2)     	! First and last elements of axes
      REAL          		ASCALE(2)           	! Width of each spatial axis bin
      REAL          		AZANG               	! Initial azimuth angle (degrees)
      REAL          		DIST			! Distance from centre to edge
      REAL          		PXCENT,PYCENT		! Coords of centre of
							! polar region in pixels
      REAL          		PRBIN			! Radial bin size in pixels
      REAL          		PSIZE               	! Pixel size (square axis units)
      REAL 	   		RBIN			! Radial bin size in axis units
      REAL			SPARR(2)		! Spaced array data
      REAL          		XCENT,YCENT		! Coords of centre of polar region in
							! axis units
      REAL          		XLOW,XHIGH,         	! Max and min values of axes
     :              		YLOW,YHIGH

      INTEGER       		AXLP                	! Loop over input axes
      INTEGER			AXPTR(2)		! Mapped axis data
      INTEGER       		AZAX, RAX           	! Output axis id's
      INTEGER			BINID			! O/p BinDS object
      INTEGER       		DIMS(3)    		! Input dimensions
      INTEGER       		IDPTR               	! Input data
      INTEGER			IDUM			! BDI_GET dummy arg
      INTEGER			IFID			! Input dataset id
      INTEGER       		IVPTR               	! input variance
      INTEGER       		IQPTR               	! Input quality
      INTEGER       		INELM               	! Input # elements
      INTEGER       		INDIM			! I/p dimensionality
      INTEGER       		K                   	! Length of strings
      INTEGER       		NABIN,NRBIN		! # of azimuthal & radial bins
      INTEGER       		NRAD			! limit on # of radial bins
      INTEGER       		NLINES              	! # lines of history
      INTEGER       		ODIM(3)             	! Output dimensions (1=radial,2=az)
      INTEGER			OFID			! Output dataset id
      INTEGER       		ONDIM               	! Output dimensionality
      INTEGER       		ODPTR               	! Output data
      INTEGER       		OVPTR               	! Output variance
      INTEGER       		OQPTR               	! Output quality
      INTEGER			TLEN			! Length of text string
      INTEGER       		WPTR                	! Polaring workspace

      BYTE          		BADBITS             	! Quality mask

      LOGICAL       		CUBE                	! Cube input?
      LOGICAL       		OK			! Object is ok?
      LOGICAL       		ERRORS,QOK		! Variance/quality ok?
      LOGICAL       		NORM                	! Normalise to axis units
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input and output data paths
      CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Map the input data array if present
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, 3, DIMS, INDIM, STATUS )
      CALL ARR_SUMDIM( INDIM, DIMS, INELM )

*  Check dimensionality.
      IF ( (INDIM .NE. 2) .AND. (INDIM.NE.3) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input must be 2 or 3 dimensional', STATUS )
        GOTO 99
      END IF
      CUBE = (INDIM.EQ.3)
      IF ( CUBE ) THEN
        CALL MSG_PRNT( 'Assuming spatial dimension are numbers'/
     :                                             /' 1 and 2' )
      END IF

      IF ( OK ) THEN
        CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
      ELSE
        CALL ERR_REP(' ','Data array not found in input file',STATUS)
        GOTO 99
      END IF

*  Map input variance if present
      CALL BDI_CHK( IFID, 'Variance', ERRORS, STATUS )
      IF ( ERRORS ) THEN
        CALL BDI_MAPR( IFID, 'Variance', 'READ', IVPTR, STATUS )
        IF (STATUS.NE.SAI__OK) THEN
           CALL MSG_PRNT('Error mapping input variance array')
           GOTO 99
        ENDIF
      END IF

*  Map quality array - if not present all qualities are set good
      CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
      IF ( QOK ) THEN
        CALL BDI_MAP( IFID, 'Quality', 'UBYTE', 'READ', IQPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_PRNT( 'Error mapping input quality array' )
          GOTO 99
        END IF

*    Attempt to get badbits value
        CALL BDI_GET( IFID, 'QualityMask', 'UBYTE', 0, 0, BADBITS,
     :                IDUM, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          BADBITS = QUAL__MASK
        END IF

      END IF

*  Find axis values if present
      DO AXLP = 1, 2
        CALL BDI_AXMAPR( IFID, AXLP, 'Data', 'READ', AXPTR(AXLP),
     :                   STATUS )
        CALL ARR_ELEM1R( AXPTR(AXLP), DIMS(AXLP), 1, AMIN(AXLP),
     :                   STATUS )
        CALL ARR_ELEM1R( AXPTR(AXLP), DIMS(AXLP), DIMS(AXLP),
     :                   AMAX(AXLP), STATUS )
        ASCALE(AXLP) = ABS( AMAX(AXLP) - AMIN(AXLP))/REAL(DIMS(AXLP)-1)
      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Tell user the axes ranges
      CALL MSG_SETR( 'XMIN',AMIN(1) )
      CALL MSG_SETR( 'XMAX',AMAX(1) )
      CALL MSG_PRNT( 'X Axis range: ^XMIN to ^XMAX' )
      CALL MSG_SETR( 'YMIN',AMIN(2) )
      CALL MSG_SETR( 'YMAX',AMAX(2) )
      CALL MSG_PRNT( 'Y Axis range: ^YMIN to ^YMAX' )

*  Because an axis may increase in any direction find max values.
      XLOW = MIN( AMIN(1), AMAX(1) )
      XHIGH = MAX( AMIN(1), AMAX(1) )
      YLOW = MIN( AMIN(2), AMAX(2) )
      YHIGH = MAX( AMIN(2), AMAX(2) )

*  Get central position
      CALL USI_GET0R('XCENT',XCENT,STATUS)

*  Test if this is within the image.
      IF (XCENT .LE. XLOW .OR. XCENT .GT. XHIGH.AND.STATUS.EQ.SAI__OK)
     :                                                            THEN
        CALL MSG_PRNT('XCENT is outside range of image')
        STATUS = SAI__ERROR
      END IF

      IF (STATUS .NE. SAI__OK) GOTO 99

*  Get Y centre
      CALL USI_GET0R('YCENT',YCENT,STATUS)

*  Test if this is within the image (pixel co-ords only)
      IF (YCENT .LE. YLOW .OR. YCENT .GT. YHIGH.AND.STATUS.EQ.SAI__OK)
     :                                                            THEN
         CALL MSG_PRNT('YCENT is outside range of image')
         STATUS=SAI__ERROR
      ENDIF

      IF (STATUS .NE. SAI__OK) GOTO 99

*  Find pixel position of the polar centre. This is measured in fractional
*  pixels from the extreme "left" and "bottom" edges of the 2d area, ie.
*  PXCENT runs from 0.0 to REAL(DIMS(1)).
      PXCENT = (XCENT-AMIN(1)) / ASCALE(1) + 0.5
      PYCENT = (YCENT-AMIN(2)) / ASCALE(2) + 0.5

*  Get radial and azimuthal binsizes
      CALL USI_GET0R('RBIN',RBIN,STATUS)
      CALL USI_GET0R('ABIN',ABIN, STATUS)

*  Normalise to axis units or /pixel?
      CALL USI_GET0L('NORM',NORM,STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Calculate number of output bins in each dimension
*  Find closest border distance in axis units
      DIST = MIN( (XHIGH-XCENT), (XCENT-XLOW), (YHIGH-YCENT),
     :                                              (YCENT-YLOW) )

*  Calc max number of radial bins.
      NRBIN=INT(DIST/RBIN)

*  Only 1 radial bin
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

*  Create o/p BinDS object and link to output file
      CALL BDI_NEW( 'BinDS', ONDIM, ODIM, 'REAL', BINID, STATUS )
      IF ( CUBE ) THEN
        CALL BDI_SETDST( BINID, 'PROFILE_SET', STATUS )
      ELSE
        CALL BDI_SETDST( BINID, 'RADIAL_PROFILE', STATUS )
      END IF
      CALL ADI_SETLNK( BINID, OFID, STATUS )
      OFID = BINID

*  Create and map output arrays
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', ODPTR, STATUS )
      CALL BDI_MAPR( OFID, 'Variance', 'WRITE', OVPTR, STATUS )
      CALL BDI_MAP( OFID, 'Quality', 'UBYTE', 'WRITE', OQPTR, STATUS )

*  Write in badbits mask
      CALL BDI_PUT( OFID, 'QualityMask', 'UBYTE', 0, 0, BADBITS,
     :              STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error creating output data, variance'/
     :                   /' and quality')
        GOTO 99
      END IF

*  Write ancilliary objects to new file.
      CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )

*  Make sure even axis scaling switched off
      CALL GCB_ATTACH( 'LOCAL', STATUS )
      CALL GCB_SETL( 'AXES_SCALING', .FALSE., STATUS )
      CALL GCB_FSAVE( OFID, STATUS )
      CALL GCB_DETACH( STATUS )

*  Get the data units of the input file
      CALL BDI_GET0C( IFID, 'Units', DUNITS, STATUS )

*  Get axis units
      DO AXLP = 1, 2
        CALL BDI_AXGET0C( IFID, AXLP, 'Units', AUNITS(AXLP), STATUS )
      END DO

*  Check that the units of each axis are the same
      K = CHR_LEN(AUNITS(1))
      IF (AUNITS(1)(1:K) .NE. AUNITS(2)(1:K) .OR. STATUS .NE.
     :               SAI__OK.OR..NOT.NORM) THEN

*    Set units to be per pixel
          DUNITS = DUNITS(1:CHR_LEN(DUNITS)) // '/pixel'
*
           CALL MSG_SETC('UNIT', DUNITS)
           IF (NORM) THEN
             CALL MSG_PRNT('Axis units are not the same - output data'/
     :                  /' will be in ^UNIT')
           ELSE
             CALL MSG_PRNT('Output data will be in ^UNIT')
           ENDIF
           UFACT = 1.0
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
           UFACT = ABS( ASCALE(1) * ASCALE(2) )
*
        ENDIF
*

*  If an azimuthal axis is wanted get the start position from the user
      IF ( NABIN .NE. 1 ) THEN
        CALL USI_GET0R('AZSTART', AZANG, STATUS)
        IF (STATUS .NE. SAI__OK) GOTO 99
      ELSE
        AZANG = 0.0
      END IF

*  Want a radial axis ?
      IF ( NRBIN .NE. 1 ) THEN

*    Write axis label and units
        CALL BDI_AXPUT0C( OFID, RAX, 'Label', 'Radius', STATUS )
        CALL BDI_AXPUT0C( OFID, RAX, 'Units', AUNITS(1), STATUS )

*    Create and map axis array
        SPARR(1) = RBIN/2.0
        SPARR(2) = RBIN
        CALL BDI_AXPUT1R( OFID, RAX, 'SpacedData', 2, SPARR, STATUS )

      END IF

*  Create an azimuth axis?
      IF ( NABIN .NE. 1 ) THEN

*    Write axis label and units
        CALL BDI_AXPUT0C( OFID, AZAX, 'Label', 'Azimuth', STATUS )
        CALL BDI_AXPUT0C( OFID, AZAX, 'Units', 'degrees', STATUS )


*    Create and map axis array
        SPARR(1) = AZANG + ABIN/2.0
        SPARR(2) = ABIN
        CALL BDI_AXPUT1R( OFID, AZAX, 'SpacedData', 2, SPARR, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Error creating axes', STATUS )
        GOTO 99
      END IF

*  Copy third input axis if a cube
      IF ( CUBE ) THEN
        CALL BDI_AXCOPY( IFID, 3, ' ', OFID, ONDIM, STATUS )
      END IF

*  Write data units to output file
      CALL BDI_PUT0C( OFID, 'Units', DUNITS, STATUS )

*  Write label
      CALL BDI_PUT0C( OFID, 'Label', 'Surface brightness', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Error writing data units and label',
     :                                                  STATUS )
        GOTO 99
      END IF

*  Map a temporary workspace array for IMG_POLAR
      CALL DYN_MAPI( 1, NRBIN*NABIN, WPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Produce the polar, looping over all 2D slices. Note that we modify the
*  pointers here which is ok as long as we don't use them after the loop
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
     :                  %VAL(IVPTR), QOK, %VAL(IQPTR), BADBITS,
     :                  UFACT, PXCENT, PYCENT, PRBIN, AZANG,
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

*  Get title of the input file
      CALL BDI_GET0C( IFID, 'Title', TITLE, STATUS )
      TITLE = TITLE(1:CHR_LEN(TITLE)) // ' - polar distribution'

*  Put title to the output file
      CALL BDI_PUT0C( OFID, 'Title', TITLE, STATUS )

*  Copy and update history file
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Create text strings for history
      CALL USI_NAMEI( NLINES, PATH, STATUS )

      CALL MSG_SETR( 'X', XCENT )
      CALL MSG_SETR( 'Y', YCENT )
      CALL MSG_MAKE( 'Polar centred on the point X=^X  Y=^Y',
     :               PATH(NLINES+1), TLEN )

*  Write azimuthal start position if required.
      IF ( NABIN .NE. 1) THEN
        CALL MSG_SETR( 'AZANG', AZANG )
        CALL MSG_MAKE( 'Initial azimuthal angle ^AZANG degrees',
     :                 PATH(NLINES+2), TLEN )
      END IF
      PSIZE = ABS( ASCALE(1) * ASCALE(2) )
      CALL MSG_SETR( 'PS', PSIZE )
      CALL MSG_SETC( 'UN', AUNITS(1) )
      CALL MSG_MAKE( 'Original pixel size ^PS square ^UN',
     :               PATH(NLINES+3), TLEN )

*  Write strings to history structure
      CALL HSI_PTXT( OFID, NLINES+3, PATH, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
