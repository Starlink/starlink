*+  APERPOL - aperture polarimetry in user defined aperture with sky
*              annulus around object aperture.

      SUBROUTINE APERPOL ( STATUS )

*    Description :
*
*     This routine takes 4 input arrays and bins up all the pixels
*     that lie within a user specified aperture to simulate measurement
*     of polarization with single element detector instruments.
*     A sky annulus is used around the object aperture to calculate
*     the sky contribution in the object aperture.
*
*    Invocation :
*
*     CALL APERPOL ( STATUS )
*
*    Parameters :
*
*     INPIC1 = IMAGE( READ )
*            Input image to be processed : 0 degrees
*     INPIC2 = IMAGE( READ )
*            Input image to be processed : 45 degrees
*     INPIC3 = IMAGE( READ )
*            Input image to be processed : 22 degrees
*     INPIC4 = IMAGE( READ )
*            Input image to be processed : 67 degrees
*     DATASOURCE = CHAR( READ )
*            Source of subsequent data input, terminal (T) or file (F)
*     FNAME = CHAR( READ )
*            Name of file containing input data
*     TERMOUT = CHAR( READ )
*            Does use want terminal out of file input data
*     XCEN = REAL( READ )
*            X coordinate of aperture centre
*     YCEN = REAL( READ )
*            Y coordinate of aperture centre
*     ECC = REAL( READ )
*            Eccentricity of apertures/annuli
*     POSANG = REAL( READ )
*            Position angle of major axis of apertures/annuli
*     SKYANNUL = LOGICAL( READ )
*            Option to use sky anulus around object annulus or not
*     MAJAX1 = REAL( READ )
*            Major axis of 1ST aperture in arbitrary units (e.g. ")
*     MAJAX2 = REAL( READ )
*            Major axis of 2ND aperture in arbitrary units (e.g. ")
*     SCALE = REAL( READ )
*            Number of units per pixel (e.g. "/pixel)
*     USEWHAT = CHAR( READ )
*            Use MEDIAN or mean in sky aperture
*     USEBAD = LOGICAL( READ )
*            Use bad pixel value in annuli calculations
*     BADVAL = REAL( READ )
*            Value of bad value
*     THETACOR = REAL( READ )
*            Position angle correction factor
*     ELDN = REAL( READ )
*            Electron/DN in image (could be scaled for DN/S)
*     AGAIN = LOGICAL( READ )
*            Another input option
*     RUNSORT = CHAR( READ )
*            Sort of additional run
*
*    Method :
*
*     Get input images from environment
*     If no error so far then
*        Map in DATA_ARRAYs
*        Check dimesions and output to user
*        Get the source of the subsequent input
*        If user has selected file input then
*          Get filename
*          Does user want terminal output of data or just file
*        Else if user has selected terminal input then
*          Get aperture centre x, y coordinates somewhere on array
*          Get aperture eccentricity and position angle
*          Get option to subtract sky from sky annulus or not
*          Get inner aperture major-axis in arbitrary units
*          Get outer aperture major axis in arbitrary units
*          Get pixel scale in these arbitrary units
*          Get option to use MEDIAN or MEAN in sky aperture
*          Get option to use bad value and get it if selected
*          Get polarization position angle correction value
*          Get electrons/DN in image for shot-noise calculation
*        If no error so far then
*           Call subroutine APERPHOTSUB to do work for inner apertures
*           Call subroutine APERPHOTSUB to do work for outer annuli
*           Write out results on return
*           Calculate the polarization for the inner aperture
*        Endif
*        Clear up input data
*        Ask user whether want another aperture
*        If user wants another aperture get what to change D or P
*     Endif
*     End
*
*    Deficiencies :

*     Works on integer pixels only not partial pixels
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Colin Aspin (JACH::CAA)
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     22-10-1985 : First implementation (REVA::MJM)
*     17-01-1986 : More error checking and tidying (REVA::MJM)
*     03-12-1987 : ask if want another input (UKTH::CAA)
*     16-01-1988 : created aperphot from aperadd (UKTH::CAA)
*     13-05-1990 : added option to change aperture size/position (JACH::CAA)
*     14-05-1990 : changed to use ANNSTATS subroutines (JACH::CAA)
*     15-05-1990 : added eccentric rotated apertures (JACH::CAA)
*     18-May-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     24_JUN-1994  Changed TYPE statements to ERR_REP,
*                  STR$ to CHR_, LIB$ TO FIO_ (SKL@JACH)
*     11-AUG-2004  Use FIO for open and close (TIMJ@JACH)

*
*      Type definitions :
	IMPLICIT  NONE              ! no implicit typing allowed

*      Global constants :
	INCLUDE  'SAE_PAR'          ! SSE global definitions
        INCLUDE  'NDF_PAR'
        INCLUDE  'NDF_ERR'
        INCLUDE  'CHR_ERR'
        INCLUDE  'FIO_PAR'

*      Status :
	INTEGER  STATUS             ! global status parameter

*      Local Constants :
	INTEGER  NDIMS              ! dimensionality of input data
	PARAMETER( NDIMS = 2 )      ! 2d images only

*      Local variables :
	INTEGER
     :     LOCI1,              ! locator to input image structure
     :     LOCI2,              ! locator to input image structure
     :     LOCI3,              ! locator to input image structure
     :     LOCI4,              ! locator to input image structure
     :     IDIMS1( NDIMS ),    ! input array dimensions
     :     IDIMS2( NDIMS ),    ! input array dimensions
     :     IDIMS3( NDIMS ),    ! input array dimensions
     :     IDIMS4( NDIMS ),    ! input array dimensions
     :     SUMDIMS( NDIMS ),   ! sum of input array dimensions
     :     ACTDIM,             ! actual dimensions from NDF_DIM
     :     NELEMENTS,          ! number of elements mapped by NDF_MAP
     :     PNTRI1,             ! pointer to input image
     :     PNTRI2,             ! pointer to input image
     :     PNTRI3,             ! pointer to input image
     :     PNTRI4              ! pointer to input image
      INTEGER
     :	   IXCEN,              ! integer x-centre
     :	   IYCEN,              ! integer y-centre
     :     NUMPIX11,           ! number of pixels added in aperture
     :     NUMPIX12,           ! number of pixels added in aperture
     :     NUMPIX13,           ! number of pixels added in aperture
     :     NUMPIX14,           ! number of pixels added in aperture
     :     NUMPIX21,           ! number of pixels added in aperture
     :     NUMPIX22,           ! number of pixels added in aperture
     :     NUMPIX23,           ! number of pixels added in aperture
     :     NUMPIX24,           ! number of pixels added in aperture
     :     BNUMPIX11,          ! number of bad pixels in aperture
     :     BNUMPIX12,          ! number of bad pixels in aperture
     :     BNUMPIX13,          ! number of bad pixels in aperture
     :     BNUMPIX14,          ! number of bad pixels in aperture
     :     BNUMPIX21,          ! number of bad pixels in aperture
     :     BNUMPIX22,          ! number of bad pixels in aperture
     :     BNUMPIX23,          ! number of bad pixels in aperture
     :     BNUMPIX24           ! number of bad pixels in aperture
      INTEGER
     :	   LUN1,               ! lun for input file
     :	   LUN2,               ! lun for output file
     :	   L1,                 ! length variable
     :	   L2,                 ! length variable
     :	   ISKY,               ! integer SKYANNUL, 0=NO SKY, 1=SKY
     :	   IUSEWHAT,           ! integer USEWHAT, 0=MEAN, 1=MEDIAN
     :	   IUSEBAD             ! integer USEBAD, 0=NO, 1=YES

	REAL
     :	   ECC,                ! ecentricity of aperture
     :	   POSANG,             ! position angle of major axis of aperture
     :     XCEN,               ! x coord of aperture centre
     :     YCEN,               ! y   "    "    "      "
     :     MAJAX1,             ! aperture major axis in arbitrary units
     :     MAJAX2,             ! aperture major axis in arbitrary units
     :     SCALE               ! number of units per pixel

	REAL
     :     TOTAL11,            ! total intensity in added aperture
     :     TOTAL12,            ! total intensity in added aperture
     :     TOTAL13,            ! total intensity in added aperture
     :     TOTAL14,            ! total intensity in added aperture
     :     TOTAL21,            ! total intensity in added aperture
     :     TOTAL22,            ! total intensity in added aperture
     :     TOTAL23,            ! total intensity in added aperture
     :     TOTAL24,            ! total intensity in added aperture
     :     TOTAL31,            ! total intensity in added aperture minus sky
     :     TOTAL32,            ! total intensity in added aperture minus sky
     :     TOTAL33,            ! total intensity in added aperture minus sky
     :     TOTAL34             ! total intensity in added aperture minus sky

	REAL
     :     MEAN11,             ! mean      "      "   "      "
     :     MEAN12,             ! mean      "      "   "      "
     :     MEAN13,             ! mean      "      "   "      "
     :     MEAN14,             ! mean      "      "   "      "
     :     MEAN21,             ! mean      "      "   "      "
     :     MEAN22,             ! mean      "      "   "      "
     :     MEAN23,             ! mean      "      "   "      "
     :     MEAN24,             ! mean      "      "   "      "
     :	   MEDIAN11,           ! median    "      "   "      "
     :	   MEDIAN12,           ! median    "      "   "      "
     :	   MEDIAN13,           ! median    "      "   "      "
     :	   MEDIAN14,           ! median    "      "   "      "
     :	   MEDIAN21,           ! median    "      "   "      "
     :	   MEDIAN22,           ! median    "      "   "      "
     :	   MEDIAN23,           ! median    "      "   "      "
     :	   MEDIAN24            ! median    "      "   "      "

	REAL
     :	   VALMAX11,           ! max       "      "   "      "
     :	   VALMAX12,           ! max       "      "   "      "
     :	   VALMAX13,           ! max       "      "   "      "
     :	   VALMAX14,           ! max       "      "   "      "
     :	   VALMAX21,           ! max       "      "   "      "
     :	   VALMAX22,           ! max       "      "   "      "
     :	   VALMAX23,           ! max       "      "   "      "
     :	   VALMAX24,           ! max       "      "   "      "
     :	   VALMIN11,           ! min       "      "   "      "
     :	   VALMIN12,           ! min       "      "   "      "
     :	   VALMIN13,           ! min       "      "   "      "
     :	   VALMIN14,           ! min       "      "   "      "
     :	   VALMIN21,           ! min       "      "   "      "
     :	   VALMIN22,           ! min       "      "   "      "
     :	   VALMIN23,           ! min       "      "   "      "
     :	   VALMIN24            ! min       "      "   "      "

	REAL
     :	   MODE11,             ! mode      "      "   "      "
     :	   MODE12,             ! mode      "      "   "      "
     :	   MODE13,             ! mode      "      "   "      "
     :	   MODE14,             ! mode      "      "   "      "
     :	   MODE21,             ! mode      "      "   "      "
     :	   MODE22,             ! mode      "      "   "      "
     :	   MODE23,             ! mode      "      "   "      "
     :	   MODE24,             ! mode      "      "   "      "
     :     NOISE11,            ! std       "      "   "      "
     :     NOISE12,            ! std       "      "   "      "
     :     NOISE13,            ! std       "      "   "      "
     :     NOISE14,            ! std       "      "   "      "
     :     NOISE21,            ! std       "      "   "      "
     :     NOISE22,            ! std       "      "   "      "
     :     NOISE23,            ! std       "      "   "      "
     :     NOISE24             ! std       "      "   "      "

	REAL
     :	   BADVAL,             ! bad pixel value
     :	   RAD1,               ! radius of inner edge of annulus
     :	   RAD2,               ! radius of outer edge of annulus
     :	   MAG3,               ! magnitude from absolute total
     :	   I,                  ! polarization total intensity
     :	   Q,                  ! Stokes parameter Q
     :	   U,                  ! Stokes parameter U
     :	   P,                  ! percentage polarization
     :	   T,                  ! polarization position angle
     :	   PE,                 ! polarization shot-noise error
     :	   TE,                 ! polarization position angle error
     :	   ELDN,               ! electrons/dn in image
     :	   THETACOR            ! position angle correction to eq. system

	CHARACTER*80
     :	   RUNSORT,            ! sort of addtional run
     :	   USEWHAT,            ! what to use in sky aperture
     :	   DATASOURCE,         ! source of input file (F) or terminal (T)
     :	   FNAME1,             ! input filename
     :	   FNAME2,             ! output filename
     :	   FEXT,               ! filename extension of input file
     :	   TERMOUT             ! terminal, file or both output

	CHARACTER*132
     :	   LINE1               ! input line

	LOGICAL
     :	   AGAIN,              ! another input option
     :	   MORE,               ! another read option
     :	   USEBAD,             ! option to use bad pixel value
     :	   IS_COMMENT,         ! determines whether line is comment or not
     :	   SKYANNUL            ! yes= want sky annulus, no=don't want it

*-
*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	   RETURN
	END IF

*      start by obtaining the input image structure locator
	CALL GETINP( 'INPIC1', LOCI1, STATUS )
	CALL GETINP( 'INPIC2', LOCI2, STATUS )
	CALL GETINP( 'INPIC3', LOCI3, STATUS )
	CALL GETINP( 'INPIC4', LOCI4, STATUS )

*      proceed if no error
	IF ( STATUS .EQ. SAI__OK ) THEN

*        map its DATA_ARRAY component onto a pointer and get dimensions

          CALL NDF_MAP( LOCI1, 'DATA', '_REAL', 'READ',
     :                  PNTRI1, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI1, NDIMS, IDIMS1, ACTDIM, STATUS )

          CALL NDF_MAP( LOCI2, 'DATA', '_REAL', 'READ',
     :                  PNTRI2, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI2, NDIMS, IDIMS2, ACTDIM, STATUS )

          CALL NDF_MAP( LOCI3, 'DATA', '_REAL', 'READ',
     :                  PNTRI3, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI3, NDIMS, IDIMS3, ACTDIM, STATUS )

          CALL NDF_MAP( LOCI4, 'DATA', '_REAL', 'READ',
     :                  PNTRI4, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI4, NDIMS, IDIMS4, ACTDIM, STATUS )

*        check dimensions are all same
	  SUMDIMS( 1)
     :          = IDIMS1( 1) + IDIMS2( 1) + IDIMS3( 1) + IDIMS4( 1)
	  SUMDIMS( 2)
     :          = IDIMS1( 2) + IDIMS2( 2) + IDIMS3( 2) + IDIMS4( 2)

	  IF( SUMDIMS( 1) .NE. IDIMS1( 1)*4 .OR.
     :	      SUMDIMS( 2) .NE. IDIMS1( 2)*4) THEN

*          tell user that images are not the same size and he is a bozo
	    CALL MSG_OUT( 'BLANK', ' ', STATUS)
	    CALL MSG_OUT( 'BOZO',
     :                    'VAXVMS-E-BOZO, Images are not same size',
     :	                   STATUS)
	    CALL MSG_OUT( 'BLANK', ' ', STATUS)

*          release images and quit cause the user is a bozo
	    CALL NDF_ANNUL( LOCI1, STATUS )
	    CALL NDF_ANNUL( LOCI2, STATUS )
	    CALL NDF_ANNUL( LOCI3, STATUS )
	    CALL NDF_ANNUL( LOCI4, STATUS )

	    RETURN

	  ELSE

*          write out the array dimensions to the user
            CALL MSG_SETI( 'XDIM', IDIMS1( 1 ) )
            CALL MSG_SETI( 'YDIM', IDIMS2( 2 ) )
            CALL MSG_OUT( 'INPUT_DIMS',
     :          'Images are all ^XDIM by ^YDIM pixels in size',
     :           STATUS )

	  END IF

*        get source of data input F=file, T=terminal
	  CALL PAR_GET0C( 'DATASOURCE', DATASOURCE, STATUS)
	  CALL CHR_UCASE( DATASOURCE )
	  IF( DATASOURCE( 1:1) .NE. 'F' .AND.
     :	      DATASOURCE( 1:1) .NE. 'T') THEN
	    DATASOURCE( 1:1) = 'T'
	  END IF

*        test if user wants to take input from Terminal or File
	  IF( DATASOURCE( 1:1) .NE. 'F') THEN

*          get option to have values written to Terminal, File or Both
	    IF( DATASOURCE( 1:1) .EQ. 'F') THEN
	      CALL PAR_GET0C( 'TERMOUT', TERMOUT, STATUS)
	      CALL CHR_UCASE( TERMOUT )
	      IF( TERMOUT( 1:1) .NE. 'T' .AND.
     :	          TERMOUT( 1:1) .NE. 'F' .AND.
     :	          TERMOUT( 1:1) .NE. 'B') THEN
	        TERMOUT( 1:1) = 'B'
	      END IF
	    END IF

*          initialize run variable
	    RUNSORT = 'BOTH'

	    AGAIN = .TRUE.
	    DO WHILE ( AGAIN)

*            get the aperture centre coordinates - the aperture centre
*            cannot be off edge of the image
	      IF( RUNSORT( 1:1) .EQ. 'P' .OR. RUNSORT( 1:1) .EQ. 'B')
     :         THEN
                CALL AIF_GET0R( 'XCEN', REAL( IDIMS1( 1 )/2.0 ), 0.0,
     :                          REAL( IDIMS1( 1 ) ), XCEN, STATUS )
                CALL AIF_GET0R( 'YCEN', REAL( IDIMS1( 2 )/2.0 ), 0.0,
     :                          REAL( IDIMS1( 2 ) ), YCEN, STATUS )
	        CALL PAR_CANCL( 'XCEN', STATUS)
	        CALL PAR_CANCL( 'YCEN', STATUS)

*              calculate integer x and y centre
	        IXCEN = IFIX( XCEN + 0.5)
	        IYCEN = IFIX( YCEN + 0.5)

	      END IF

*            get the aperture eccentricity and position angle
	      IF( RUNSORT( 1:1) .EQ. 'B') THEN
                CALL AIF_GET0R( 'ECC', 0.0, 0.0, 0.9999, ECC,
     :                           STATUS )
                CALL AIF_GET0R( 'POSANG', 0.0, 0.0, 180.0, POSANG,
     :	                        STATUS )
	        CALL PAR_CANCL( 'ECC', STATUS)
	        CALL PAR_CANCL( 'POSANG', STATUS)
	      END IF

*            get option to use sky annulus or not
	      IF( RUNSORT( 1:1) .EQ. 'B') THEN
                CALL PAR_GET0L( 'SKYANNUL', SKYANNUL, STATUS )
	        CALL PAR_CANCL( 'SKYANNUL', STATUS)
	      END IF

*            get the major axis of the aperture in arbitrary units
	      IF( RUNSORT( 1:1) .EQ. 'M' .OR. RUNSORT( 1:1) .EQ. 'B')
     :         THEN
	        CALL AIF_GET0R( 'MAJAX1', 10.0, 0.0001, 10000.0,
     :                          MAJAX1, STATUS )
	        CALL PAR_CANCL( 'MAJAX1', STATUS)
	        IF( SKYANNUL) THEN
                  CALL AIF_GET0R( 'MAJAX2', MAJAX1*2, MAJAX1, 10000.0,
     :                            MAJAX2, STATUS )
	          CALL PAR_CANCL( 'MAJAX2', STATUS)
	        END IF
	      END IF

*            get the size of a pixel in these arbitrary units
	      IF( RUNSORT( 1:1) .EQ. 'B') THEN
                CALL AIF_GET0R( 'SCALE', 0.286, 0.0001, 10000.0,
     :                          SCALE, STATUS )
	        CALL PAR_CANCL( 'SCALE', STATUS)
	      END IF

*            check for error here - do not divide unless parameters have
*            been read in safely - not the case if STATUS = PAR__NULL
              IF ( STATUS .NE. SAI__OK ) THEN

*              just announce the error, clear up and abort
                CALL ERR_REP( 'FUNNY_PARS',
     :           'Something wrong with input parameters - aborting',
     :            STATUS )
                CALL NDF_ANNUL( LOCI1, STATUS )
                CALL NDF_ANNUL( LOCI2, STATUS )
                CALL NDF_ANNUL( LOCI3, STATUS )
                CALL NDF_ANNUL( LOCI4, STATUS )
                RETURN
              END IF

*            get option to use median or mean in sky aperture
	      IF( RUNSORT( 1:1) .EQ. 'B') THEN
	        IF( SKYANNUL) THEN
	          CALL PAR_GET0C( 'USEWHAT', USEWHAT, STATUS)
	          CALL PAR_CANCL( 'USEWHAT', STATUS)
	          CALL CHR_UCASE( USEWHAT )
	          IF( USEWHAT( 1:3) .NE. 'MED' .AND.
     :                USEWHAT( 1:3) .NE. 'MEA') THEN
	            CALL MSG_OUT( 'MESS', 'Using MEDIAN...',
     :                STATUS)
	            USEWHAT( 1:3) = 'MED'
	          END IF
	        END IF
	      END IF

*            ask if want to use bad value
	      IF( RUNSORT( 1:1) .EQ. 'B') THEN
	        CALL PAR_GET0L( 'USEBAD', USEBAD, STATUS)
	        CALL PAR_CANCL( 'USEBAD', STATUS)

*              if user wants to use bad value then get it
	        CALL PAR_GET0R( 'BADVAL', BADVAL, STATUS)
	        CALL PAR_CANCL( 'BADVAL', STATUS)
	      END IF

*            ask user for position angle correction value
	      IF( RUNSORT( 1:1) .EQ. 'B') THEN
	        CALL AIF_GET0R( 'THETACOR', 0.0, 0.0, 180.0, THETACOR,
     :	                        STATUS)
	        CALL PAR_CANCL( 'THETACOR', STATUS)
	      END IF

*            ask user for electrons/dn in image
	      IF( RUNSORT( 1:1) .EQ. 'B') THEN
	        CALL AIF_GET0R( 'ELDN', 6.0, 0.0, 1.0E37, ELDN,
     :                           STATUS )
	        CALL PAR_CANCL( 'ELDN', STATUS)
	      END IF

*            check for error before calling working subroutine
              IF ( STATUS .EQ. SAI__OK ) THEN

*              calculate the inner and outer radius of the star aperture
	        RAD1 = 0.0
	        RAD2 = MAJAX1/2.0

*              call subroutine that does work for object aperture
	        CALL APERPHOTSUB( IDIMS1( 1), IDIMS1( 2), %VAL( PNTRI1),
     :	                          IXCEN, IYCEN, ECC, POSANG, RAD1, RAD2,
     :	                          SCALE, USEBAD, BADVAL, NUMPIX11,
     :	                          BNUMPIX11, TOTAL11, MEAN11, MEDIAN11,
     :	                          MODE11, VALMAX11, VALMIN11, NOISE11)

*	type *, idims1( 1), idims1( 2)
*	type *, ixcen, iycen
*	type *, ecc, posang, rad1, rad2
*	type *, scale, usebad, badval
*	type *, numpix11, bnumpix11
*	type *, total11, mean11, median11, mode11
*	type *, valmax11, valmin11, noise11

	        CALL APERPHOTSUB( IDIMS2( 1), IDIMS2( 2), %VAL( PNTRI2),
     :	                          IXCEN, IYCEN, ECC, POSANG, RAD1, RAD2,
     :	                          SCALE, USEBAD, BADVAL, NUMPIX12,
     :	                          BNUMPIX12, TOTAL12, MEAN12, MEDIAN12,
     :	                          MODE12, VALMAX12, VALMIN12, NOISE12)

*	type *, idims2( 1), idims2( 2)
*	type *, ixcen, iycen
*	type *, ecc, posang, rad1, rad2
*	type *, scale, usebad, badval
*	type *, numpix12, bnumpix12
*	type *, total12, mean12, median12, mode12
*	type *, valmax12, valmin12, noise12

	        CALL APERPHOTSUB( IDIMS3( 1), IDIMS3( 2), %VAL( PNTRI3),
     :	                          IXCEN, IYCEN, ECC, POSANG, RAD1, RAD2,
     :	                          SCALE, USEBAD, BADVAL, NUMPIX13,
     :	                          BNUMPIX13, TOTAL13, MEAN13, MEDIAN13,
     :	                          MODE13, VALMAX13, VALMIN13, NOISE13)

	        CALL APERPHOTSUB( IDIMS4( 1), IDIMS4( 2), %VAL( PNTRI4),
     :	                          IXCEN, IYCEN, ECC, POSANG, RAD1, RAD2,
     :	                          SCALE, USEBAD, BADVAL, NUMPIX14,
     :	                          BNUMPIX14, TOTAL14, MEAN14, MEDIAN14,
     :	                          MODE14, VALMAX14, VALMIN14, NOISE14)

*              put all interesting parameters to interface
	        CALL PAR_PUT0I( 'NUMPIX11', NUMPIX11, STATUS)
	        CALL PAR_PUT0R( 'TOTAL11', TOTAL11, STATUS)
	        CALL PAR_PUT0R( 'MEAN11', MEAN11, STATUS)
	        CALL PAR_PUT0R( 'MEDIAN11', MEDIAN11, STATUS)
	        CALL PAR_PUT0R( 'NOISE11', NOISE11, STATUS)
	        CALL PAR_PUT0I( 'NUMPIX12', NUMPIX12, STATUS)
	        CALL PAR_PUT0R( 'TOTAL12', TOTAL12, STATUS)
	        CALL PAR_PUT0R( 'MEAN12', MEAN12, STATUS)
	        CALL PAR_PUT0R( 'MEDIAN12', MEDIAN12, STATUS)
	        CALL PAR_PUT0R( 'NOISE12', NOISE12, STATUS)
	        CALL PAR_PUT0I( 'NUMPIX13', NUMPIX13, STATUS)
	        CALL PAR_PUT0R( 'TOTAL13', TOTAL13, STATUS)
	        CALL PAR_PUT0R( 'MEAN13', MEAN13, STATUS)
	        CALL PAR_PUT0R( 'MEDIAN13', MEDIAN13, STATUS)
	        CALL PAR_PUT0R( 'NOISE13', NOISE13, STATUS)
	        CALL PAR_PUT0I( 'NUMPIX14', NUMPIX14, STATUS)
	        CALL PAR_PUT0R( 'TOTAL14', TOTAL14, STATUS)
	        CALL PAR_PUT0R( 'MEAN14', MEAN14, STATUS)
	        CALL PAR_PUT0R( 'MEDIAN14', MEDIAN14, STATUS)
	        CALL PAR_PUT0R( 'NOISE14', NOISE14, STATUS)

*              calculate the inner and outer radius of the sky annulus
	        RAD1 = MAJAX1/2.0+0.1
	        RAD2 = MAJAX2/2.0

*              call subroutines that do the actual work
	        IF( SKYANNUL) THEN
	          CALL APERPHOTSUB( IDIMS1( 1), IDIMS1( 2),
     :	                            %VAL( PNTRI1), IXCEN, IYCEN, ECC,
     :	                            POSANG, RAD1, RAD2, SCALE, USEBAD,
     :                              BADVAL, NUMPIX21, BNUMPIX21,
     :	                            TOTAL21, MEAN21, MEDIAN21, MODE21,
     :	                            VALMAX21, VALMIN21, NOISE21)

	          CALL APERPHOTSUB( IDIMS2( 1), IDIMS2( 2),
     :	                            %VAL( PNTRI2), IXCEN, IYCEN, ECC,
     :	                            POSANG, RAD1, RAD2, SCALE, USEBAD,
     :                              BADVAL, NUMPIX22, BNUMPIX22,
     :	                            TOTAL22, MEAN22, MEDIAN22, MODE22,
     :	                            VALMAX22, VALMIN22, NOISE22)

	          CALL APERPHOTSUB( IDIMS3( 1), IDIMS3( 2),
     :	                            %VAL( PNTRI3), IXCEN, IYCEN, ECC,
     :	                            POSANG, RAD1, RAD2, SCALE, USEBAD,
     :	                            BADVAL, NUMPIX23, BNUMPIX23, TOTAL23,
     :	                            MEAN23, MEDIAN23, MODE23, VALMAX23,
     :                              VALMIN23, NOISE23)

	          CALL APERPHOTSUB( IDIMS4( 1), IDIMS4( 2),
     :	                            %VAL( PNTRI4), IXCEN, IYCEN, ECC,
     :	                            POSANG, RAD1, RAD2, SCALE, USEBAD,
     :                              BADVAL, NUMPIX24, BNUMPIX24,
     :	                            TOTAL24, MEAN24, MEDIAN24, MODE24,
     :	                            VALMAX24, VALMIN24, NOISE24)

*                put all interesting parameters to interface
	          CALL PAR_PUT0I( 'NUMPIX21', NUMPIX21, STATUS)
	          CALL PAR_PUT0R( 'TOTAL21', TOTAL21, STATUS)
	          CALL PAR_PUT0R( 'MEAN21', MEAN21, STATUS)
	          CALL PAR_PUT0R( 'MEDIAN21', MEDIAN21, STATUS)
	          CALL PAR_PUT0R( 'NOISE21', NOISE21, STATUS)
	          CALL PAR_PUT0I( 'NUMPIX22', NUMPIX22, STATUS)
	          CALL PAR_PUT0R( 'TOTAL22', TOTAL22, STATUS)
	          CALL PAR_PUT0R( 'MEAN22', MEAN22, STATUS)
	          CALL PAR_PUT0R( 'MEDIAN22', MEDIAN22, STATUS)
	          CALL PAR_PUT0R( 'NOISE22', NOISE22, STATUS)
	          CALL PAR_PUT0I( 'NUMPIX23', NUMPIX23, STATUS)
	          CALL PAR_PUT0R( 'TOTAL23', TOTAL23, STATUS)
	          CALL PAR_PUT0R( 'MEAN23', MEAN23, STATUS)
	          CALL PAR_PUT0R( 'MEDIAN23', MEDIAN23, STATUS)
	          CALL PAR_PUT0R( 'NOISE23', NOISE23, STATUS)
	          CALL PAR_PUT0I( 'NUMPIX24', NUMPIX24, STATUS)
	          CALL PAR_PUT0R( 'TOTAL24', TOTAL24, STATUS)
	          CALL PAR_PUT0R( 'MEAN24', MEAN24, STATUS)
	          CALL PAR_PUT0R( 'MEDIAN24', MEDIAN24, STATUS)
	          CALL PAR_PUT0R( 'NOISE24', NOISE24, STATUS)

*                calculate total from median if median selected
	          IF( USEWHAT( 1:3) .EQ. 'MED') THEN
	            TOTAL21 = MEDIAN21*NUMPIX11
	            TOTAL22 = MEDIAN22*NUMPIX12
	            TOTAL23 = MEDIAN23*NUMPIX13
	            TOTAL24 = MEDIAN24*NUMPIX14
	          ELSE
	            TOTAL21 = MEAN21*NUMPIX11
	            TOTAL22 = MEAN22*NUMPIX12
	            TOTAL23 = MEAN23*NUMPIX13
	            TOTAL24 = MEAN24*NUMPIX14
	          END IF

	        ELSE

	          NUMPIX21 = 0
	          NUMPIX22 = 0
	          NUMPIX23 = 0
	          NUMPIX24 = 0
	          TOTAL21 = 0.0
	          TOTAL22 = 0.0
	          TOTAL23 = 0.0
	          TOTAL24 = 0.0

	        END IF

*              calculate the source minus sky in inner aperture
	        TOTAL31 = TOTAL11-TOTAL21
	        TOTAL32 = TOTAL12-TOTAL22
	        TOTAL33 = TOTAL13-TOTAL23
	        TOTAL34 = TOTAL14-TOTAL24

*              check if total is positive, if not then set to 0
	        IF( TOTAL31 .LT. 0.0 .OR.
     :	            TOTAL32 .LT. 0.0 .OR.
     :	            TOTAL33 .LT. 0.0 .OR.
     :	            TOTAL34 .LT. 0.0) THEN
	          TOTAL31 = 0.0
	          TOTAL32 = 0.0
	          TOTAL33 = 0.0
	          TOTAL34 = 0.0
	        END IF

*	       on return, output the relevant figures
	        CALL MSG_OUT( 'BLANK', ' ', STATUS )

	        CALL MSG_SETI( 'NP1', NUMPIX11)
	        CALL MSG_SETI( 'NP2', NUMPIX21)
	        CALL MSG_SETI( 'IX', IXCEN)
	        CALL MSG_SETI( 'IY', IYCEN)
	        CALL MSG_OUT( 'APER_NUMPIX',
     :'Number of pixels in object/sky aperture (^IX,^IY) = ^NP1/^NP2',
     :	          STATUS )

	        CALL MSG_OUT( 'BLANK', ' ', STATUS )

	        IF( SKYANNUL) THEN
	          IF( USEWHAT( 1:3) .EQ. 'MED') THEN
	            CALL MSG_OUT( 'MESS',
     :	          'Sky contribution calculated from MEDIAN in annulus',
     :	              STATUS )
	          ELSE
	            CALL MSG_OUT( 'MESS',
     :	             'Sky contribution calculated from MEAN in annulus',
     :	              STATUS )
	          END IF
	        END IF

	        CALL MSG_SETR( 'TL1', TOTAL11)
	        CALL MSG_SETR( 'TL2', TOTAL21)
	        CALL MSG_OUT( 'APER_TOTAL',
     :	          'Object/Sky intensities ( 0 degs)  = ^TL1/^TL2',
     :	          STATUS )
	        CALL MSG_SETR( 'TL1', TOTAL12)
	        CALL MSG_SETR( 'TL2', TOTAL22)
	        CALL MSG_OUT( 'APER_TOTAL',
     :	          'Object/Sky intensities (45 degs)  = ^TL1/^TL2',
     :	          STATUS )
	        CALL MSG_SETR( 'TL1', TOTAL13)
	        CALL MSG_SETR( 'TL2', TOTAL23)
	        CALL MSG_OUT( 'APER_TOTAL',
     :	          'Object/Sky intensities (22 degs)  = ^TL1/^TL2',
     :	          STATUS )
	        CALL MSG_SETR( 'TL1', TOTAL14)
	        CALL MSG_SETR( 'TL2', TOTAL24)
	        CALL MSG_OUT( 'APER_TOTAL',
     :	          'Object/Sky intensities (67 degs)  = ^TL1/^TL2',
     :	          STATUS )

	        CALL MSG_OUT( 'BLANK', ' ', STATUS )

*              calculate the polarization

	        CALL POL_STOKESCAL( TOTAL31, TOTAL32, Q)
	        CALL POL_STOKESCAL( TOTAL33, TOTAL34, U)
	        CALL POL_POLCAL( Q, U, P)
	        CALL POL_THETACAL( Q, U, T)
	        CALL POL_ERRCAL( TOTAL31, TOTAL32, TOTAL33, TOTAL34,
     :	                         P, ELDN, PE, TE)
	        CALL POL_INTCAL( TOTAL31, TOTAL32, TOTAL33,
     :                           TOTAL34, I)

*              correct the position angle by users amount
	        IF( I .GT. 1.0E-10) THEN
	          T = T + THETACOR
	          IF( T .LT. 0.0) T = T + 180.0
	          IF( T .GT. 180.0) T = T - 180.0
	        ELSE
	          T = 0.0
	        END IF

*              tell user what's going down brother ...
	        CALL MSG_SETR( 'I', I)
	        CALL MSG_OUT( 'MESS',
     :                        'Total Intensity                   = ^I',
     :	                      STATUS)
	        CALL MSG_SETR( 'Q', Q)
	        CALL MSG_OUT( 'MESS',
     :                        'Q-Stokes parameter (%)            = ^Q',
     :	                      STATUS)
	        CALL MSG_SETR( 'U', U)
	        CALL MSG_OUT( 'MESS',
     :                        'U-Stokes parameter (%)            = ^U',
     :	                      STATUS)
	        CALL MSG_SETR( 'P', P)
	        CALL MSG_OUT( 'MESS',
     :                        'Percentage polarization (%)       = ^P',
     :	                      STATUS)
	        CALL MSG_SETR( 'T', T)
	        CALL MSG_OUT( 'MESS',
     :                        'Position angle (degrees)          = ^T',
     :	                      STATUS)
	        CALL MSG_SETR( 'PE', PE)
	        CALL MSG_OUT( 'MESS',
     :                        'Polarization shot-noise error (%) = ^PE',
     :	                       STATUS)
	        CALL MSG_SETR( 'TE', TE)
	        CALL MSG_OUT( 'MESS',
     :                        'Position angle error (degrees)    = ^TE',
     :	                      STATUS)

	        CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*              	calculate magnitude from total
	        IF( I .NE. -999 .AND. I .NE. 0.0) THEN
	          MAG3 = -2.5*LOG10( ABS( I))
	        ELSE
	          MAG3 = -999
	        END IF

*              tell user the even more marvellous news
	        CALL PAR_PUT0R( 'MAG', MAG3, STATUS)
	        CALL MSG_SETR( 'MAG3', MAG3)
	        CALL MSG_OUT( 'MESSAGE',
     :	          'Magnitude from absolute value of I      = ^MAG3',
     :	          STATUS)

	        CALL MSG_OUT( 'BLANK', ' ', STATUS)

              END IF

*            ask if want to input another area
	      CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS)
	      CALL PAR_CANCL( 'AGAIN', STATUS)

              CALL MSG_OUT( 'BLANK', ' ', STATUS )

*            if user wants more runs then get sort of run
	      IF( AGAIN) THEN
	        CALL PAR_GET0C( 'RUNSORT', RUNSORT, STATUS)
	        CALL PAR_CANCL( 'RUNSORT', STATUS)
	        CALL CHR_UCASE( RUNSORT )
	        IF( RUNSORT( 1:1) .NE. 'M' .AND.
     :	            RUNSORT( 1:1) .NE. 'P') THEN
	          CALL MSG_OUT( 'MESS',
     :                          'Changing aperture MAJOR AXIS ...',
     :	                        STATUS)
	          RUNSORT( 1:1) = 'M'
	        END IF

	        CALL MSG_OUT( 'BLANK', ' ', STATUS )

	      END IF

	    END DO

	  ELSE ! ZZZ

*          get name of file containing source position data etc
	    CALL PAR_GET0C( 'FNAME', FNAME1, STATUS)

*          get option to have values written to Terminal, File or Both
	    IF( DATASOURCE( 1:1) .EQ. 'F') THEN
	      CALL PAR_GET0C( 'TERMOUT', TERMOUT, STATUS)
	      CALL CHR_UCASE( TERMOUT )
	      IF( TERMOUT( 1:1) .NE. 'T' .AND.
     :	          TERMOUT( 1:1) .NE. 'F' .AND.
     :	          TERMOUT( 1:1) .NE. 'B') THEN
	        TERMOUT( 1:1) = 'B'
	      END IF
	    END IF

*          open the output data list file
            CALL FIO_OPEN( FNAME1, 'READ','LIST',0, LUN1,
     :           STATUS)

            IF (STATUS .NE. SAI__OK) GO TO 999

*          check if user wants terminal, file or both output
	    IF( TERMOUT( 1:1) .EQ. 'F' .OR.
     :	        TERMOUT( 1:1) .EQ. 'B') THEN

*            create output filename
	      L1 = INDEX( FNAME1, '.')
              CALL CHR_CLEAN( FNAME1 )
              L2 = 0
	      CALL CHR_APPND( FNAME1, FNAME1, L2 )
	      FEXT = FNAME1( L1:L2)
	      FNAME2 = FNAME1( 1:L1-1)
              CALL CHR_CLEAN( FNAME2 )
              L1 = 0
	      CALL CHR_APPND( FNAME2, FNAME2, L1 )
              CALL CHR_CLEAN( FEXT )
              L2 = 0
	      CALL CHR_APPND( FEXT, FEXT, L2 )
	      FNAME2 = FNAME2( 1:L1) // 'A' // FEXT( 1:L2)
	      CALL CHR_UCASE( FNAME2 )

*            open output data file
              CALL FIO_OPEN( FNAME2, 'WRITE','LIST',0, LUN2,
     :             STATUS)

              IF (STATUS .NE. SAI__OK) GO TO 998

*            write header line to output file
	      LINE1 =
     :	      '     X     Y     E    POS    D1    D2    N1   BN1    '//
     :	      'N2   BN2             I         P        TH        ' //
     :	      'PE        TE        MAG'

              CALL CHR_CLEAN( LINE1 )
              L1 = 0
              CALL CHR_APPND( LINE1, LINE1, L1 )
              CALL FIO_WRITE( LUN2, LINE1(1:L1), STATUS)

	    END IF

*          loop to read all info from input file, proces it and write
*          line to output file
	    MORE = .TRUE.
	    DO WHILE ( MORE)

*            read line from input file
	      READ( LUN1, '(A)', END=100) LINE1

*            see if this is a comment line
	      CALL CHECK_COMMENT( LINE1, IS_COMMENT)

*            test if current line is a comment or not
	      IF( .NOT. IS_COMMENT ) THEN

*              read input variables from input line
	        READ( LINE1, *, ERR=997, END=996) XCEN, YCEN, ECC,
     :	                                          POSANG, ISKY,
     :	                                          MAJAX1, MAJAX2,
     :	                                          SCALE, IUSEWHAT,
     :	                                          IUSEBAD, BADVAL,
     :	                                          THETACOR, ELDN

*              setup sky annuli option from input
	        IF( ISKY .EQ. 0) THEN
	          SKYANNUL = .FALSE.
	        ELSE
	          SKYANNUL = .TRUE.
	        END IF

*              setup usewhat and usebad character variables from input
	        IF( IUSEWHAT .EQ. 0) THEN
	          USEWHAT = 'MEAN'
	        ELSE
	          USEWHAT = 'MEDIAN'
	        END IF
	        IF( IUSEBAD .EQ. 0) THEN
	          USEBAD = .FALSE.
	        ELSE
	          USEBAD = .TRUE.
	        END IF

*              calculate integer x and y centre
	        IXCEN = IFIX( XCEN + 0.5)
	        IYCEN = IFIX( YCEN + 0.5)

*              calculate the inner and outer radius of the star aperture
	        RAD1 = 0.0
	        RAD2 = MAJAX1/2.0

*              call subroutine that does the actual work
	        CALL APERPHOTSUB( IDIMS1( 1), IDIMS1( 2), %VAL( PNTRI1),
     :	                          IXCEN, IYCEN, ECC, POSANG, RAD1, RAD2,
     :	                          SCALE, USEBAD, BADVAL, NUMPIX11,
     :	                          BNUMPIX11, TOTAL11, MEAN11, MEDIAN11,
     :	                          MODE11, VALMAX11, VALMIN11, NOISE11)

	        CALL APERPHOTSUB( IDIMS2( 1), IDIMS2( 2), %VAL( PNTRI2),
     :	                          IXCEN, IYCEN, ECC, POSANG, RAD1, RAD2,
     :	                          SCALE, USEBAD, BADVAL, NUMPIX12,
     :	                          BNUMPIX12, TOTAL12, MEAN12, MEDIAN12,
     :	                          MODE12, VALMAX12, VALMIN12, NOISE12)

	        CALL APERPHOTSUB( IDIMS3( 1), IDIMS3( 2), %VAL( PNTRI3),
     :	                          IXCEN, IYCEN, ECC, POSANG, RAD1, RAD2,
     :	                          SCALE, USEBAD, BADVAL, NUMPIX13,
     :	                          BNUMPIX13, TOTAL13, MEAN13, MEDIAN13,
     :	                          MODE13, VALMAX13, VALMIN13, NOISE13)

	        CALL APERPHOTSUB( IDIMS4( 1), IDIMS4( 2), %VAL( PNTRI4),
     :	                          IXCEN, IYCEN, ECC, POSANG, RAD1, RAD2,
     :	                          SCALE, USEBAD, BADVAL, NUMPIX14,
     :	                          BNUMPIX14, TOTAL14, MEAN14, MEDIAN14,
     :	                          MODE14, VALMAX14, VALMIN14, NOISE14)

*              put all interesting parameters to interface
!	        CALL PAR_PUT0I( 'NUMPIX11', NUMPIX11, STATUS)
!	        CALL PAR_PUT0R( 'TOTAL11', TOTAL11, STATUS)
!	        CALL PAR_PUT0R( 'MEAN11', MEAN11, STATUS)
!	        CALL PAR_PUT0R( 'MEDIAN11', MEDIAN11, STATUS)
!	        CALL PAR_PUT0R( 'NOISE11', NOISE11, STATUS)
!	        CALL PAR_PUT0I( 'NUMPIX12', NUMPIX12, STATUS)
!	        CALL PAR_PUT0R( 'TOTAL12', TOTAL12, STATUS)
!	        CALL PAR_PUT0R( 'MEAN12', MEAN12, STATUS)
!	        CALL PAR_PUT0R( 'MEDIAN12', MEDIAN12, STATUS)
!	        CALL PAR_PUT0R( 'NOISE12', NOISE12, STATUS)
!	        CALL PAR_PUT0I( 'NUMPIX13', NUMPIX13, STATUS)
!	        CALL PAR_PUT0R( 'TOTAL13', TOTAL13, STATUS)
!	        CALL PAR_PUT0R( 'MEAN13', MEAN13, STATUS)
!	        CALL PAR_PUT0R( 'MEDIAN13', MEDIAN13, STATUS)
!	        CALL PAR_PUT0R( 'NOISE13', NOISE13, STATUS)
!	        CALL PAR_PUT0I( 'NUMPIX14', NUMPIX14, STATUS)
!	        CALL PAR_PUT0R( 'TOTAL14', TOTAL14, STATUS)
!	        CALL PAR_PUT0R( 'MEAN14', MEAN14, STATUS)
!	        CALL PAR_PUT0R( 'MEDIAN14', MEDIAN14, STATUS)
!	        CALL PAR_PUT0R( 'NOISE14', NOISE14, STATUS)

*              calculate the inner and outer radius of the sky annulus
	        RAD1 = MAJAX1/2.0+0.1
	        RAD2 = MAJAX2/2.0

*              call subroutines that do the actual work
	        IF( SKYANNUL) THEN
	          CALL APERPHOTSUB( IDIMS1( 1), IDIMS1( 2),
     :	                            %VAL( PNTRI1), IXCEN, IYCEN, ECC,
     :                              POSANG, RAD1, RAD2, SCALE, USEBAD,
     :	                            BADVAL, NUMPIX21, BNUMPIX21,
     :	                            TOTAL21, MEAN21, MEDIAN21, MODE21,
     :	                            VALMAX21, VALMIN21, NOISE21)

	          CALL APERPHOTSUB( IDIMS2( 1), IDIMS2( 2),
     :	                            %VAL( PNTRI2), IXCEN, IYCEN, ECC,
     :                              POSANG, RAD1, RAD2, SCALE, USEBAD,
     :	                            BADVAL, NUMPIX22, BNUMPIX22,
     :	                            TOTAL22, MEAN22, MEDIAN22, MODE22,
     :	                            VALMAX22, VALMIN22, NOISE22)

	          CALL APERPHOTSUB( IDIMS3( 1), IDIMS3( 2),
     :	                            %VAL( PNTRI3), IXCEN, IYCEN, ECC,
     :                              POSANG, RAD1, RAD2, SCALE, USEBAD,
     :	                            BADVAL, NUMPIX23, BNUMPIX23,
     :	                            TOTAL23, MEAN23, MEDIAN23,
     :	                            MODE23, VALMAX23, VALMIN23,
     :                              NOISE23)

	          CALL APERPHOTSUB( IDIMS4( 1), IDIMS4( 2),
     :	                            %VAL( PNTRI4), IXCEN, IYCEN, ECC,
     :                              POSANG, RAD1, RAD2, SCALE, USEBAD,
     :                              BADVAL, NUMPIX24, BNUMPIX24,
     :	                            TOTAL24, MEAN24, MEDIAN24, MODE24,
     :	                            VALMAX24, VALMIN24, NOISE24)

*                put all interesting parameters to interface
!	          CALL PAR_PUT0I( 'NUMPIX21', NUMPIX21, STATUS)
!	          CALL PAR_PUT0R( 'TOTAL21', TOTAL21, STATUS)
!	          CALL PAR_PUT0R( 'MEAN21', MEAN21, STATUS)
!	          CALL PAR_PUT0R( 'MEDIAN21', MEDIAN21, STATUS)
!	          CALL PAR_PUT0R( 'NOISE21', NOISE21, STATUS)
!	          CALL PAR_PUT0I( 'NUMPIX22', NUMPIX22, STATUS)
!	          CALL PAR_PUT0R( 'TOTAL22', TOTAL22, STATUS)
!	          CALL PAR_PUT0R( 'MEAN22', MEAN22, STATUS)
!	          CALL PAR_PUT0R( 'MEDIAN22', MEDIAN22, STATUS)
!	          CALL PAR_PUT0R( 'NOISE22', NOISE22, STATUS)
!	          CALL PAR_PUT0I( 'NUMPIX23', NUMPIX23, STATUS)
!	          CALL PAR_PUT0R( 'TOTAL23', TOTAL23, STATUS)
!	          CALL PAR_PUT0R( 'MEAN23', MEAN23, STATUS)
!	          CALL PAR_PUT0R( 'MEDIAN23', MEDIAN23, STATUS)
!	          CALL PAR_PUT0R( 'NOISE23', NOISE23, STATUS)
!	          CALL PAR_PUT0I( 'NUMPIX24', NUMPIX24, STATUS)
!	          CALL PAR_PUT0R( 'TOTAL24', TOTAL24, STATUS)
!	          CALL PAR_PUT0R( 'MEAN24', MEAN24, STATUS)
!	          CALL PAR_PUT0R( 'MEDIAN24', MEDIAN24, STATUS)
!	          CALL PAR_PUT0R( 'NOISE24', NOISE24, STATUS)

*                calculate total from median if median selected
	          IF( USEWHAT( 1:3) .EQ. 'MED') THEN
	            TOTAL21 = MEDIAN21*NUMPIX11
	            TOTAL22 = MEDIAN22*NUMPIX12
	            TOTAL23 = MEDIAN23*NUMPIX13
	            TOTAL24 = MEDIAN24*NUMPIX14
	          ELSE
	            TOTAL21 = MEAN21*NUMPIX11
	            TOTAL22 = MEAN22*NUMPIX12
	            TOTAL23 = MEAN23*NUMPIX13
	            TOTAL24 = MEAN24*NUMPIX14
	          END IF

	        ELSE

	          NUMPIX21 = 0
	          NUMPIX22 = 0
	          NUMPIX23 = 0
	          NUMPIX24 = 0
	          TOTAL21 = 0.0
	          TOTAL22 = 0.0
	          TOTAL23 = 0.0
	          TOTAL24 = 0.0

	        END IF

*              calculate the source minus sky in inner aperture
	        TOTAL31 = TOTAL11-TOTAL21
	        TOTAL32 = TOTAL12-TOTAL22
	        TOTAL33 = TOTAL13-TOTAL23
	        TOTAL34 = TOTAL14-TOTAL24

*              check if total is positive, if not then set to 0
	        IF( TOTAL31 .LT. 0.0 .OR.
     :	            TOTAL32 .LT. 0.0 .OR.
     :	            TOTAL33 .LT. 0.0 .OR.
     :	            TOTAL34 .LT. 0.0) THEN
	          TOTAL31 = 0.0
	          TOTAL32 = 0.0
	          TOTAL33 = 0.0
	          TOTAL34 = 0.0
	        END IF

*              calculate the polarization
	        CALL POL_STOKESCAL( TOTAL31, TOTAL32, Q)
	        CALL POL_STOKESCAL( TOTAL33, TOTAL34, U)
	        CALL POL_POLCAL( Q, U, P)
	        CALL POL_THETACAL( Q, U, T)
	        CALL POL_ERRCAL( TOTAL31, TOTAL32, TOTAL33, TOTAL34,
     :	                         P, ELDN, PE, TE)
	        CALL POL_INTCAL( TOTAL31, TOTAL32, TOTAL33,
     :                           TOTAL34, I)

*              correct the position angle by users amount
	        IF( I .GT. 1.0E-10) THEN
	          T = T + THETACOR
	          IF( T .LT. 0.0) T = T + 180.0
	          IF( T .GT. 180.0) T = T - 180.0
	        ELSE
	          T = 0.0
	        END IF

*              calculate magnitude from total
	        IF( I .NE. -999 .AND. I .NE. 0.0) THEN
	          MAG3 = -2.5*LOG10( ABS( I))
	        ELSE
	          MAG3 = -999
	        END IF

*              check if user wants terminal, file or both output
	        IF( TERMOUT( 1:1) .EQ. 'T' .OR.
     :	            TERMOUT( 1:1) .EQ. 'B') THEN

*	         on return, output the relevant figures
	          CALL MSG_OUT( 'BLANK', ' ', STATUS )

	          CALL MSG_SETI( 'NP1', NUMPIX11)
	          CALL MSG_SETI( 'NP2', NUMPIX21)
	          CALL MSG_SETI( 'IX', IXCEN)
	          CALL MSG_SETI( 'IY', IYCEN)
	          CALL MSG_OUT( 'APER_NUMPIX',
     :'Number of pixels in object/sky aperture (^IX,^IY) = ^NP1/^NP2',
     :	          STATUS )

	          CALL MSG_OUT( 'BLANK', ' ', STATUS )

	          IF( SKYANNUL) THEN
	            IF( USEWHAT( 1:3) .EQ. 'MED') THEN
	              CALL MSG_OUT( 'MESS',
     :            'Sky contribution calculated from MEDIAN in annulus',
     :	                STATUS )
	            ELSE
	              CALL MSG_OUT( 'MESS',
     :	             'Sky contribution calculated from MEAN in annulus',
     :	                STATUS )
	            END IF
	          END IF

	          CALL MSG_SETR( 'TL1', TOTAL11)
	          CALL MSG_SETR( 'TL2', TOTAL21)
	          CALL MSG_OUT( 'APER_TOTAL',
     :	            'Object/Sky intensities ( 0 degs) = ^TL1/^TL2',
     :	            STATUS )
	          CALL MSG_SETR( 'TL1', TOTAL12)
	          CALL MSG_SETR( 'TL2', TOTAL22)
	          CALL MSG_OUT( 'APER_TOTAL',
     :	            'Object/Sky intensities (45 degs) = ^TL1/^TL2',
     :	            STATUS )
	          CALL MSG_SETR( 'TL1', TOTAL13)
	          CALL MSG_SETR( 'TL2', TOTAL23)
	          CALL MSG_OUT( 'APER_TOTAL',
     :	            'Object/Sky intensities (22 degs)  = ^TL1/^TL2',
     :	            STATUS )
	          CALL MSG_SETR( 'TL1', TOTAL14)
	          CALL MSG_SETR( 'TL2', TOTAL24)
	          CALL MSG_OUT( 'APER_TOTAL',
     :	            'Object/Sky intensities (67 degs)  = ^TL1/^TL2',
     :	            STATUS )

	          CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                tell user what's going down brother ...
	          CALL MSG_SETR( 'I', I)
	          CALL MSG_OUT( 'MESS',
     :                        'Total Intensity                   = ^I',
     :	                        STATUS)
	          CALL MSG_SETR( 'Q', Q)
	          CALL MSG_OUT( 'MESS',
     :                        'Q-Stokes parameter (%)            = ^Q',
     :	                        STATUS)
	          CALL MSG_SETR( 'U', U)
	          CALL MSG_OUT( 'MESS',
     :                        'U-Stokes parameter (%)            = ^U',
     :	                        STATUS)
	          CALL MSG_SETR( 'P', P)
	          CALL MSG_OUT( 'MESS',
     :                        'Percentage polarization (%)       = ^P',
     :	                        STATUS)
	          CALL MSG_SETR( 'T', T)
	          CALL MSG_OUT( 'MESS',
     :                        'Position angle (degrees)          = ^T',
     :	                        STATUS)
	          CALL MSG_SETR( 'PE', PE)
	          CALL MSG_OUT( 'MESS',
     :                        'Polarization shot-noise error (%) = ^PE',
     :	                        STATUS)
	          CALL MSG_SETR( 'TE', TE)
	          CALL MSG_OUT( 'MESS',
     :                       'Position angle error (degrees)    = ^TE',
     :	                        STATUS)

	          CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*                tell user the even more marvellous news
	          CALL PAR_PUT0R( 'MAG', MAG3, STATUS)
	          CALL MSG_SETR( 'MAG3', MAG3)
	          CALL MSG_OUT( 'MESSAGE',
     :	            'Magnitude from absolute value of I      = ^MAG3',
     :	            STATUS)

	          CALL MSG_OUT( 'BLANK', ' ', STATUS)

	        END IF

*              check if user wants terminal, file or both output
	        IF( TERMOUT( 1:1) .EQ. 'F' .OR.
     :	            TERMOUT( 1:1) .EQ. 'B') THEN

*                write line to users output file
	          WRITE( LUN2,
     :	    '(2F6.1,2X,F4.2,2X,F5.1,2F6.1,4I6,F14.2,4(2x,F8.3),F11.3)')
     :	            XCEN, YCEN,
     :	            ECC, POSANG,
     :	            MAJAX1, MAJAX2,
     :	            NUMPIX11, BNUMPIX11,
     :	            NUMPIX21, BNUMPIX21,
     :	            I, P,
     :	            T, PE,
     :	            TE, MAG3

	        END IF

	      ELSE

*              check if user wants terminal, file or both output
	        IF( TERMOUT( 1:1) .EQ. 'F' .OR.
     :	          TERMOUT( 1:1) .EQ. 'B') THEN

*                write comment line to users output file
                  CALL CHR_CLEAN( LINE1 )
                  L1 = 0
	          CALL CHR_APPND( LINE1, LINE1, L1 )
                  CALL FIO_WRITE(LUN2, LINE1(1:L1), STATUS)

	        END IF

              END IF

	    END DO

 100        CONTINUE

*          here when input file empty, close files and free lun
            CALL FIO_CLOSE( LUN1, STATUS )

*          check if user wants terminal, file or both output
	    IF( TERMOUT( 1:1) .EQ. 'F' .OR.
     :	        TERMOUT( 1:1) .EQ. 'B') THEN

*            close output files and free lun
              CALL FIO_CLOSE( LUN2, STATUS )

*            tell user the name of the output file
	      CALL MSG_OUT( 'BLANK', ' ', STATUS)
	      CALL MSG_SETC( 'FO', FNAME2)
	      CALL MSG_OUT( 'MESS', 'Output file is called ^FO',
     :                       STATUS)
	      CALL MSG_OUT( 'BLANK', ' ', STATUS)

	    END IF

	  END IF

	END IF

*      jump to end since next bit is for OPEN/READ errors
	GOTO 200

*      error messages

  999   CALL MSG_SETC( 'FI', FNAME1)
        CALL ERR_REP( 'MESSAGE', 'Error, opening input file ^FI',
     :                 STATUS )
	GOTO 200
  998	CALL MSG_SETC( 'FO', FNAME2)
        CALL ERR_REP( 'MESSAGE', 'Error, opening output file ^FO',
     :                 STATUS )
	GOTO 200
  997	CALL MSG_SETC( 'LINE', LINE1)
        CALL ERR_REP( 'MESSAGE',
     :                'Error, reading from input line ^LINE', STATUS )
	GOTO 200
  996	CALL MSG_SETC( 'LINE', LINE1)
        CALL ERR_REP( 'MESSAGE',
     :                'Error, end of input line ^LINE', STATUS )
*      here at end of program
  200	CONTINUE

*      now tidy up input data
	CALL NDF_ANNUL( LOCI1, STATUS )
	CALL NDF_ANNUL( LOCI2, STATUS )
	CALL NDF_ANNUL( LOCI3, STATUS )
	CALL NDF_ANNUL( LOCI4, STATUS )

	END
