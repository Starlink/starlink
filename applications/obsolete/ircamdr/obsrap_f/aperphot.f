*+  APERPHOT - aperture photometry in user defined aperture with sky
*              annulus around object aperture.

      SUBROUTINE APERPHOT ( STATUS )

*    Description :
*
*     This routine takes an input array and bins up all the pixels
*     that lie within a user specified aperture to either increase
*     the signal-to-noise over that region, or to simulate a circular
*     aperture measurement of the image as with single element
*     detector instruments.  A sky annulus is used around the
*     object aperture to calculate the sky contribution in the
*     object aperture.
*
*    Invocation :
*
*     CALL APERPHOT ( STATUS )
*
*    Parameters :
*
*     INPIC = IMAGE( READ )
*            Input image to be processed
*     DATASOURCE = CHAR( READ )
*            Source of subsequent data input, terminal (T) or file (F)
*     FNAME = CHAR( READ )
*            Name of file containing input data
*     TERMOUT = CHAR( READ )
*            Does use want terminal out of file input data
*     SKYANNUL = LOGICAL( READ )
*            Whether user want to subtract sky from concentric annulus
*     XCEN = REAL( READ )
*            X coordinate of aperture centre
*     YCEN = REAL( READ )
*            Y coordinate of aperture centre
*     ECC = REAL( READ )
*            Eccentricity of apertures/annuli
*     POSANG = REAL( READ )
*            Position angle of major axis of apertures/annuli
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
*     AGAIN = LOGICAL( READ )
*            Another input option
*     RUNSORT = CHAR( READ )
*            Sort of additional run
*
*    Method :
*
*     Get input image from environment
*     If no error so far then
*        Map in DATA_ARRAY
*        Output image dimensions to user
*        Get the source of the subsequent input
*        If user has selected file input then
*          Get filename
*          Does user want terminal output of data or just file
*        Else if user has selected terminal input then
*          Get aperture centre x, y coordinates somewhere on array
*          Get aperture eccentricity and position angle
*          Get option to subtract sky from concentric annulus
*          Get inner aperture major-axis in arbitrary units
*          Get outer aperture major axis in arbitrary units
*          Get pixel scale in these arbitrary units
*          Get option to use MEDIAN or MEAN in sky aperture
*          Get option to use bad value and get it if selected
*        If no error so far then
*           Call subroutine APERPHOTSUB to do work for inner aperture
*           Write out results on return
*           Call subroutine APERPHOTSUB to do work for outer annulus
*           Write out results on return
*           Calculate the sky, object-sky for the inner aperture
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
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     24-JUN-1994  Changed LIB$ to FIO_, STR$ to CHR_, TYPE to ERR_ (SKL@JACH)
*
*      Type definitions :
	IMPLICIT  NONE              ! no implicit typing allowed

*      Global constants :
	INCLUDE  'SAE_PAR'          ! SSE global definitions
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'
        INCLUDE 'FIO_PAR'
        INCLUDE 'CHR_ERR'

*      Status :
	INTEGER  STATUS             ! global status parameter

*      Local Constants :
	INTEGER  NDIMS              ! dimensionality of input data
	PARAMETER( NDIMS = 2 )      ! 2d images only

*      Local variables :
	INTEGER
     :     LOCI,               ! locator to input image structure
     :     IDIMS( NDIMS ),     ! input array dimensions
     :     PNTRI,              ! pointer to input image
     :     ACTDIM,             ! actual dimensions from NDF_DIM
     :     NELEMENTS,          ! number of elements mapped by NDF_MAP
     :	   IXCEN,              ! integer x-centre
     :	   IYCEN,              ! integer y-centre
     :     NUMPIX1,            ! number of pixels added in aperture
     :     NUMPIX2,            ! number of pixels added in aperture
     :     BNUMPIX1,           ! number of bad pixels in aperture
     :     BNUMPIX2,           ! number of bad pixels in aperture
     :	   LUN1,               ! lun for input file
     :	   LUN2,               ! lun for output file
     :	   L1,                 ! length variable
     :	   L2,                 ! length variable
     :	   ISKY,               ! integer SKYANNUL, 0=NO, 1=YES
     :	   IUSEWHAT,           ! integer USEWHAT, 0=MEAN, 1=MEDIAN
     :	   IUSEBAD             ! integer USEBAD, 0=NO, 1=YES

	REAL
     :     junk1,
     :	   ECC,                ! ecentricity of aperture
     :	   POSANG,             ! position angle of major axis of aperture
     :     XCEN,               ! x coord of aperture centre
     :     YCEN,               ! y   "    "    "      "
     :     MAJAX1,             ! aperture major axis in arbitrary units
     :     MAJAX2,             ! aperture major axis in arbitrary units
     :     MAJAX3,             ! aperture major axis in arbitrary units
     :     SCALE,              ! number of units per pixel
     :     TOTAL1,             ! total intensity in added aperture
     :     TOTAL2,             ! total intensity in added aperture
     :     TOTAL3,             ! total intensity in added aperture minus sky
     :     MEAN1,              ! mean      "      "   "      "
     :     MEAN2,              ! mean      "      "   "      "
     :	   MEDIAN1,            ! median    "      "   "      "
     :	   MEDIAN2,            ! median    "      "   "      "
     :	   VALMAX1,            ! max       "      "   "      "
     :	   VALMAX2,            ! max       "      "   "      "
     :	   VALMIN1,            ! min       "      "   "      "
     :	   VALMIN2,            ! min       "      "   "      "
     :	   MODE1,              ! mode      "      "   "      "
     :	   MODE2,              ! mode      "      "   "      "
     :     NOISE1,             ! std       "      "   "      "
     :     NOISE2,             ! std       "      "   "      "
     :	   BADVAL,             ! bad pixel value
     :	   RAD1,               ! radius of inner edge of annulus
     :	   RAD2,               ! radius of outer edge of annulus
     :	   MAG3                ! magnitude from absolute total

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
     :	   SKYANNUL            ! subtract sky in sky annulus?

*-
*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	   RETURN
	END IF

*      start by obtaining the input image structure locator
	CALL GETINP( 'INPIC', LOCI, STATUS )

*      proceed if no error
	IF ( STATUS .EQ. SAI__OK ) THEN

*        map its DATA_ARRAY component and get dimensions
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, ACTDIM, STATUS )

*        write out the array dimensions to the user
          CALL MSG_SETI( 'XDIM', IDIMS( 1 ) )
          CALL MSG_SETI( 'YDIM', IDIMS( 2 ) )
          CALL MSG_OUT( 'INPUT_DIMS',
     :        'Image is ^XDIM by ^YDIM pixels', STATUS )

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
     :        THEN
                CALL AIF_GET0R( 'XCEN', REAL( IDIMS( 1 )/2.0 ), 0.0,
     :                          REAL( IDIMS( 1 ) ), XCEN, STATUS )
                CALL AIF_GET0R( 'YCEN', REAL( IDIMS( 2 )/2.0 ), 0.0,
     :                          REAL( IDIMS( 2 ) ), YCEN, STATUS )
	        CALL PAR_CANCL( 'XCEN', STATUS)
	        CALL PAR_CANCL( 'YCEN', STATUS)

*              calculate integer x and y centre
	        IXCEN = IFIX( XCEN + 0.5)
	        IYCEN = IFIX( YCEN + 0.5)

	      END IF

*            get the aperture eccentricity and position angle
	      IF( RUNSORT( 1:1) .EQ. 'B') THEN
                CALL AIF_GET0R( 'ECC', 0.0, 0.0, 0.9999, ECC, STATUS )
                CALL AIF_GET0R( 'POSANG', 0.0, 0.0, 180.0, POSANG,
     :	                        STATUS )
	        CALL PAR_CANCL( 'ECC', STATUS)
	        CALL PAR_CANCL( 'POSANG', STATUS)
	      END IF

*            get the option to subtract sky in concentric annulus
	      IF( RUNSORT( 1:1) .EQ. 'B') THEN
                CALL PAR_GET0L( 'SKYANNUL', SKYANNUL, STATUS )
	        CALL PAR_CANCL( 'SKYANNUL', STATUS)
	      END IF

*            get the major axis of the aperture in arbitrary units
	      IF( RUNSORT( 1:1) .EQ. 'M' .OR. RUNSORT( 1:1) .EQ. 'B')
     :        THEN
	        CALL AIF_GET0R( 'MAJAX1', 10.0, 0.0001, 10000.0,
     :                          MAJAX1, STATUS )
!	print *, 'after aif'
	        CALL PAR_CANCL( 'MAJAX1', STATUS)
!	print *, 'after cancl', skyannul
	        IF( SKYANNUL) THEN
	          junk1 = majax1*2
!	print *, 'in test', junk1, majax1
                  CALL AIF_GET0R( 'MAJAX2', 10.0, MAJAX1, 10000.0,
     :                            MAJAX2, STATUS )
!	print *, 'after aif 2'
	          CALL PAR_CANCL( 'MAJAX2', STATUS)
	          junk1 = majax1*3
                  CALL AIF_GET0R( 'MAJAX3', 20.0, MAJAX2, 10000.0,
     :                            MAJAX3, STATUS )
	          CALL PAR_CANCL( 'MAJAX3', STATUS)
	        END IF
	      END IF

*            get the size of a pixel in these arbitrary units
	      IF( RUNSORT( 1:1) .EQ. 'B') THEN
                CALL AIF_GET0R( 'SCALE', 1.0, 0.0001, 10000.0,
     :                          SCALE, STATUS )
	        CALL PAR_CANCL( 'SCALE', STATUS)
	      END IF

*            check for error here - do not divide unless parameters have
*            been read in safely - not the case if STATUS = PAR__NULL
              IF ( STATUS .NE. SAI__OK ) THEN

*              just announce the error, clear up and abort
                CALL ERR_REP( 'FUNNY_PARS',
     :           'Something wrong with input parameters - aborting',
     :           STATUS )
                CALL NDF_ANNUL( LOCI, STATUS )
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
	            CALL MSG_OUT( 'MESS', 'Using MEDIAN...', STATUS)
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

*            check for error before calling working subroutine
              IF ( STATUS .EQ. SAI__OK ) THEN

*              calculate the inner and outer radius of the star aperture
	        RAD1 = 0.0
	        RAD2 = MAJAX1/2.0

*              call subroutine that does the actual work
	        CALL APERPHOTSUB(
     :                 IDIMS( 1), IDIMS( 2), %VAL( PNTRI), IXCEN,
     :	               IYCEN, ECC, POSANG, RAD1, RAD2, SCALE,
     :	               USEBAD, BADVAL, NUMPIX1, BNUMPIX1, TOTAL1,
     :	               MEAN1, MEDIAN1, MODE1, VALMAX1, VALMIN1,
     :	               NOISE1)

*              put all interesting parameters to interface
	        CALL PAR_PUT0I( 'NUM1', NUMPIX1, STATUS)
	        CALL PAR_PUT0R( 'TOTAL1', TOTAL1, STATUS)
	        CALL PAR_PUT0R( 'MEAN1', MEAN1, STATUS)
	        CALL PAR_PUT0R( 'MED1', MEDIAN1, STATUS)
	        CALL PAR_PUT0R( 'NOISE1', NOISE1, STATUS)

*              on return, output the relevant figures
                CALL MSG_OUT( 'BLANK', ' ', STATUS )
                CALL MSG_OUT( 'BLANK', ' ', STATUS )
                CALL MSG_OUT( 'MESS', 'STAR APERTURE', STATUS )
                CALL MSG_OUT( 'BLANK', '*************', STATUS )

                CALL MSG_SETI( 'NUMPIX', NUMPIX1 )
                CALL MSG_SETI( 'BNUMPIX', BNUMPIX1 )
                CALL MSG_OUT( 'APER_NUMPIX',
     :    'Number of pixels in aperture         = ^NUMPIX/^BNUMPIX',
     :           STATUS )

                CALL MSG_SETR( 'TOTAL', TOTAL1 )
                CALL MSG_OUT( 'APER_TOTAL',
     :           'Total intensity in aperture          = ^TOTAL',
     :           STATUS )

                CALL MSG_SETR( 'MEAN', MEAN1 )
                CALL MSG_OUT( 'APER_MEAN',
     :           'Mean intensity over aperture         = ^MEAN',
     :           STATUS )

                CALL MSG_SETR( 'MEDIAN', MEDIAN1 )
                CALL MSG_OUT( 'APER_MEDIAN',
     :           'Median intensity over aperture       = ^MEDIAN',
     :           STATUS )

                CALL MSG_SETR( 'NOISE', NOISE1)
                CALL MSG_OUT( 'APER_NOISE',
     :           'Standard deviation in aperture       = ^NOISE',
     :           STATUS )

*              calculate the inner and outer radius of the sky annulus
	        RAD1 = MAJAX2/2.0
	        RAD2 = MAJAX3/2.0

*              call subroutine that does the actual work
	        IF( SKYANNUL) THEN
	          CALL APERPHOTSUB(
     :                   IDIMS( 1), IDIMS( 2), %VAL( PNTRI), IXCEN,
     :	                 IYCEN, ECC, POSANG, RAD1, RAD2, SCALE,
     :	                 USEBAD, BADVAL, NUMPIX2, BNUMPIX2, TOTAL2,
     :	                 MEAN2, MEDIAN2, MODE2, VALMAX2, VALMIN2,
     :	                 NOISE2)

*                put all interesting parameters to interface
	          CALL PAR_PUT0I( 'NUM2', NUMPIX2, STATUS)
	          CALL PAR_PUT0R( 'TOTAL2', TOTAL2, STATUS)
	          CALL PAR_PUT0R( 'MEAN2', MEAN2, STATUS)
	          CALL PAR_PUT0R( 'MED2', MEDIAN2, STATUS)
	          CALL PAR_PUT0R( 'NOISE2', NOISE2, STATUS)

*                on return, output the relevant figures
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL MSG_OUT( 'MESS', 'SKY ANNULUS', STATUS )
                  CALL MSG_OUT( 'BLANK', '***********', STATUS )

                  CALL MSG_SETI( 'NUMPIX', NUMPIX2 )
                  CALL MSG_SETI( 'BNUMPIX', BNUMPIX2 )
                  CALL MSG_OUT( 'APER_NUMPIX',
     :     'Number of GOOD/BAD pixels in annulus = ^NUMPIX/^BNUMPIX',
     :             STATUS )

                  CALL MSG_SETR( 'TOTAL', TOTAL2 )
                  CALL MSG_OUT( 'APER_TOTAL',
     :             'Total intensity in annulus           = ^TOTAL',
     :             STATUS )

                  CALL MSG_SETR( 'MEAN', MEAN2 )
                  CALL MSG_OUT( 'APER_MEAN',
     :             'Mean intensity over annulus          = ^MEAN',
     :             STATUS )

                  CALL MSG_SETR( 'MEDIAN', MEDIAN2 )
                  CALL MSG_OUT( 'APER_MEDIAN',
     :             'Median intensity over annulus        = ^MEDIAN',
     :             STATUS )

                  CALL MSG_SETR( 'NOISE', NOISE2)
                  CALL MSG_OUT( 'APER_NOISE',
     :             'Standard deviation over annulus      = ^NOISE',
     :             STATUS )

	          CALL MSG_OUT( 'BLANK', ' ', STATUS)

*                calculate total from median if median selected
	          IF( USEWHAT( 1:3) .EQ. 'MED') THEN
	            TOTAL2 = MEDIAN2*NUMPIX1
	          ELSE
	            TOTAL2 = MEAN2*NUMPIX1
	          END IF

*                tell user about the sky level in the object aperture
	          IF( USEWHAT( 1:3) .EQ. 'MED') THEN
	            CALL MSG_SETR( 'TOTAL2', TOTAL2)
	            CALL MSG_OUT( 'MESSAGE',
     :               'Sky in object aperture (MEDIAN)      = ^TOTAL2',
     :                STATUS)
	          ELSE
	            CALL MSG_SETR( 'TOTAL2', TOTAL2)
	            CALL MSG_OUT( 'MESSAGE',
     :	             'Sky in object aperture (MEAN)        = ^TOTAL2',
     :	              STATUS)
	          END IF

	          CALL MSG_OUT( 'BLANK', ' ', STATUS)

	        ELSE

	          NUMPIX2 = 0
	          TOTAL2 = 0

	        END IF

*              calculate the source minus sky in inner aperture
	        TOTAL3 = TOTAL1-TOTAL2

*              tell user the marvellous news
	        CALL PAR_PUT0R( 'OBJSKY', TOTAL3, STATUS)
	        CALL MSG_SETR( 'TOTAL3', TOTAL3)
	        CALL MSG_OUT( 'MESSAGE',
     :	          'Object-Sky value                     = ^TOTAL3',
     :	          STATUS)

	        CALL MSG_OUT( 'BLANK', ' ', STATUS)

*              	calculate magnitude from total
	        IF( TOTAL3 .NE. -999) THEN
	          MAG3 = -2.5*LOG10( ABS( TOTAL3))
	        ELSE
	          MAG3 = -999
	        END IF

*              tell user the even more marvellous news
	        CALL PAR_PUT0R( 'MAG', MAG3, STATUS)
	        CALL MSG_SETR( 'MAG3', MAG3)
	        CALL MSG_OUT( 'MESSAGE',
     :	          'Magnitude from absolute value        = ^MAG3',
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

*          get lun for open of ascii file
	    CALL FIO_GUNIT( LUN1, STATUS )

*          open input data file
	    OPEN( UNIT=LUN1, FILE=FNAME1, STATUS='OLD', ERR=999)

*          check if user wants terminal, file or both output
	    IF( TERMOUT( 1:1) .EQ. 'F' .OR.
     :	        TERMOUT( 1:1) .EQ. 'B') THEN

*            get lun for open of OUTPUT ascii file
	      CALL FIO_GUNIT( LUN2, STATUS )

*            create output filename
	      L1 = INDEX( FNAME1, '.')
              CALL CHR_CLEAN( FNAME1 )
              L2 = 0
	      CALL CHR_APPND( FNAME1, FNAME1, L2)
	      FNAME2 = FNAME1( 1:L1-1)
	      FEXT = FNAME1( L1:L2)
              CALL CHR_CLEAN( FNAME2 )
              L1 = 0
	      CALL CHR_APPND( FNAME2, FNAME2, L1)
              CALL CHR_CLEAN( FEXT )
              L2 = 0
	      CALL CHR_APPND( FEXT, FEXT, L2)
	      FNAME2 = FNAME2( 1:L1) // 'a' // FEXT( 1:L2)
!	      CALL CHR_UCASE( FNAME2 )

*            open output data file
	      OPEN( UNIT=LUN2, FILE=FNAME2, STATUS='UNKNOWN', ERR=998)

*            write header line to output file
	      LINE1 =
     :	      '     X     Y     E    POS    D1    D2    D3    N1   '//
     :        'BN1    N2   BN2          TOT1         '//
     :	      ' TOT2          TOT3        MAG'
              CALL CHR_CLEAN( LINE1 )
              L1 = 0
	      CALL CHR_APPND( LINE1, LINE1, L1)
	      WRITE( LUN2, '(A)') LINE1( 1:L1)

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
     :	                                          MAJAX1, MAJAX2, MAJAX3,
     :	                                          SCALE, IUSEWHAT,
     :	                                          IUSEBAD, BADVAL

*              setup skyannul, usewhat and usebad character variables
	        IF( ISKY .EQ. 0) THEN
	          SKYANNUL = .FALSE.
	        ELSE
	          SKYANNUL = .TRUE.
	        END IF
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
	        CALL APERPHOTSUB(
     :                    IDIMS( 1), IDIMS( 2), %VAL( PNTRI), IXCEN,
     :	                  IYCEN, ECC, POSANG, RAD1, RAD2, SCALE,
     :	                  USEBAD, BADVAL, NUMPIX1, BNUMPIX1, TOTAL1,
     :	                  MEAN1, MEDIAN1, MODE1, VALMAX1, VALMIN1,
     :	                  NOISE1)

*              check if user wants terminal, file or both output
	        IF( TERMOUT( 1:1) .EQ. 'T' .OR.
     :	            TERMOUT( 1:1) .EQ. 'B') THEN

*                on return, output the relevant figures
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL MSG_OUT( 'MESS', 'STAR APERTURE', STATUS )
                  CALL MSG_OUT( 'BLANK', '*************', STATUS )

	          CALL PAR_PUT0I( 'NUM1', NUMPIX1, STATUS)
                  CALL MSG_SETI( 'NUMPIX', NUMPIX1 )
                  CALL MSG_SETI( 'BNUMPIX', BNUMPIX1 )
                  CALL MSG_OUT( 'APER_NUMPIX',
     :    'Number of GOOD/BAD pixels in aperture= ^NUMPIX/^BNUMPIX',
     :             STATUS )

	          CALL PAR_PUT0R( 'TOTAL1', TOTAL1, STATUS)
                  CALL MSG_SETR( 'TOTAL', TOTAL1 )
                  CALL MSG_OUT( 'APER_TOTAL',
     :             'Total intensity in aperture          = ^TOTAL',
     :             STATUS )

	          CALL PAR_PUT0R( 'MEAN1', MEAN1, STATUS)
                  CALL MSG_SETR( 'MEAN', MEAN1 )
                  CALL MSG_OUT( 'APER_MEAN',
     :             'Mean intensity over aperture         = ^MEAN',
     :             STATUS )

	          CALL PAR_PUT0R( 'MED1', MEDIAN1, STATUS)
                  CALL MSG_SETR( 'MEDIAN', MEDIAN1 )
                  CALL MSG_OUT( 'APER_MEDIAN',
     :             'Median intensity over aperture       = ^MEDIAN',
     :             STATUS )

	          CALL PAR_PUT0R( 'NOISE1', NOISE1, STATUS)
                  CALL MSG_SETR( 'NOISE', NOISE1)
                  CALL MSG_OUT( 'APER_NOISE',
     :             'Standard deviation in aperture       = ^NOISE',
     :             STATUS )

	        END IF

*              calculate the inner and outer radius of the sky annulus
	        RAD1 = MAJAX2/2.0
	        RAD2 = MAJAX3/2.0

*              call subroutine that does the actual work
	        IF( SKYANNUL) THEN
!	print *, 'just before aperphotsub call 2'
	          CALL APERPHOTSUB(
     :                IDIMS( 1), IDIMS( 2), %VAL( PNTRI), IXCEN,
     :	              IYCEN, ECC, POSANG, RAD1, RAD2, SCALE,
     :	              USEBAD, BADVAL, NUMPIX2, BNUMPIX2, TOTAL2,
     :	              MEAN2, MEDIAN2, MODE2, VALMAX2, VALMIN2,
     :	              NOISE2)

*                check if user wants terminal, file or both output
	          IF( TERMOUT( 1:1) .EQ. 'T' .OR.
     :	              TERMOUT( 1:1) .EQ. 'B') THEN

*                  on return, output the relevant figures
                    CALL MSG_OUT( 'BLANK', ' ', STATUS )
                    CALL MSG_OUT( 'MESS', 'SKY ANNULUS', STATUS )
                    CALL MSG_OUT( 'BLANK', '***********', STATUS )

	            CALL PAR_PUT0I( 'NUM2', NUMPIX2, STATUS)
                    CALL MSG_SETI( 'NUMPIX', NUMPIX2 )
                    CALL MSG_SETI( 'BNUMPIX', BNUMPIX2 )
                    CALL MSG_OUT( 'APER_NUMPIX',
     :      'Number of GOOD/BAD pixels in annulus = ^NUMPIX/^BNUMPIX',
     :               STATUS )

	            CALL PAR_PUT0R( 'TOTAL2', TOTAL2, STATUS)
                    CALL MSG_SETR( 'TOTAL', TOTAL2 )
                    CALL MSG_OUT( 'APER_TOTAL',
     :               'Total intensity in annulus           = ^TOTAL',
     :               STATUS )

	            CALL PAR_PUT0R( 'MEAN2', MEAN2, STATUS)
                    CALL MSG_SETR( 'MEAN', MEAN2 )
                    CALL MSG_OUT( 'APER_MEAN',
     :               'Mean intensity over annulus          = ^MEAN',
     :               STATUS )

	            CALL PAR_PUT0R( 'MED2', MEDIAN2, STATUS)
                    CALL MSG_SETR( 'MEDIAN', MEDIAN2 )
                    CALL MSG_OUT( 'APER_MEDIAN',
     :               'Median intensity over annulus        = ^MEDIAN',
     :               STATUS )

	            CALL PAR_PUT0R( 'NOISE2', NOISE2, STATUS)
                    CALL MSG_SETR( 'NOISE', NOISE2)
                    CALL MSG_OUT( 'APER_NOISE',
     :               'Standard deviation over annulus      = ^NOISE',
     :               STATUS )

	            CALL MSG_OUT( 'BLANK', ' ', STATUS)

	          END IF

*                calculate total from median if median selected
	          IF( USEWHAT( 1:3) .EQ. 'MED') THEN
	            TOTAL2 = MEDIAN2*NUMPIX1
	          ELSE
	            TOTAL2 = MEAN2*NUMPIX1
	          END IF

*                check if user wants terminal, file or both output
	          IF( TERMOUT( 1:1) .EQ. 'T' .OR.
     :	              TERMOUT( 1:1) .EQ. 'B') THEN

*                  tell user about the sky level in the object aperture
	            IF( USEWHAT( 1:3) .EQ. 'MED') THEN
	              CALL MSG_SETR( 'TOTAL2', TOTAL2)
	              CALL MSG_OUT( 'MESSAGE',
     :               'Sky in object aperture (MEDIAN)      = ^TOTAL2',
     :                  STATUS)
	            ELSE
	              CALL MSG_SETR( 'TOTAL2', TOTAL2)
	              CALL MSG_OUT( 'MESSAGE',
     :	             'Sky in object aperture (MEAN)        = ^TOTAL2',
     :	                STATUS)
	            END IF

	            CALL MSG_OUT( 'BLANK', ' ', STATUS)

	          END IF

	         ELSE

	           NUMPIX2 = 0
	           TOTAL2 = 0.0

	         END IF

*              calculate the source minus sky in inner aperture
	        TOTAL3 = TOTAL1-TOTAL2

*              check if user wants terminal, file or both output
	        IF( TERMOUT( 1:1) .EQ. 'T' .OR.
     :	            TERMOUT( 1:1) .EQ. 'B') THEN

*                tell user the marvellous news
	          CALL PAR_PUT0R( 'OBJSKY', TOTAL3, STATUS)
	          CALL MSG_SETR( 'TOTAL3', TOTAL3)
	          CALL MSG_OUT( 'MESSAGE',
     :	            'Object-Sky value                     = ^TOTAL3',
     :	            STATUS)

	          CALL MSG_OUT( 'BLANK', ' ', STATUS)

	        END IF

*              	calculate magnitude from total
	        IF( TOTAL3 .NE. -999) THEN
	          MAG3 = -2.5*LOG10( ABS( TOTAL3))
	        ELSE
	          MAG3 = -999
	        END IF

*              check if user wants terminal, file or both output
	        IF( TERMOUT( 1:1) .EQ. 'T' .OR.
     :	            TERMOUT( 1:1) .EQ. 'B') THEN

*                tell user the even more marvellous news
	          CALL PAR_PUT0R( 'MAG', MAG3, STATUS)
	          CALL MSG_SETR( 'MAG3', MAG3)
	          CALL MSG_OUT( 'MESSAGE',
     :	            'Magnitude from absolute value        = ^MAG3',
     :	            STATUS)

	          CALL MSG_OUT( 'BLANK', ' ', STATUS)

	        END IF

*              check if user wants terminal, file or both output
	        IF( TERMOUT( 1:1) .EQ. 'F' .OR.
     :	            TERMOUT( 1:1) .EQ. 'B') THEN

*                write line to users output file
	          WRITE( LUN2,
     :            '(2F6.1,2X,F4.2,2X,F5.1,3F6.1,4I6,3F14.2,F11.3)')
     :	            XCEN, YCEN,
     :	            ECC, POSANG,
     :	            MAJAX1, MAJAX2, MAJAX3,
     :	            NUMPIX1, BNUMPIX1,
     :	            NUMPIX2, BNUMPIX2,
     :	            TOTAL1, TOTAL2,
     :	            TOTAL3, MAG3

	        END IF

	      ELSE

*              check if user wants terminal, file or both output
	        IF( TERMOUT( 1:1) .EQ. 'F' .OR.
     :	          TERMOUT( 1:1) .EQ. 'B') THEN

*                write comment line to users output file
                  CALL CHR_CLEAN( LINE1 )
                  L1 = 0
	          CALL CHR_APPND( LINE1, LINE1, L1)
	          WRITE( LUN2, '(A)') LINE1( 1:L1)

	        END IF

              END IF

	    END DO

*          here when input file empty, close files and free lun
  100	    CLOSE( LUN1)
	    CALL FIO_PUNIT( LUN1, STATUS )

*          check if user wants terminal, file or both output
	    IF( TERMOUT( 1:1) .EQ. 'F' .OR.
     :	        TERMOUT( 1:1) .EQ. 'B') THEN

*            close output files and free lun
	      CLOSE( LUN2)
	      CALL FIO_PUNIT( LUN2, STATUS )

*            tell user the name of the output file
	      CALL MSG_OUT( 'BLANK', ' ', STATUS)
	      CALL MSG_SETC( 'FO', FNAME2)
	      CALL MSG_OUT( 'MESS', 'Output file is called ^FO',
     :                      STATUS)
	      CALL MSG_OUT( 'BLANK', ' ', STATUS)

	    END IF

	  END IF

	END IF

*      jump to end since next bit is for OPEN/READ errors
	GOTO 200

*      error messages
  999	CALL MSG_SETC( 'FI', FNAME1)
        CALL ERR_REP( 'MESSAGE', 'Error, opening input file ^FI',
     :  STATUS )
	GOTO 200
  998	CALL MSG_SETC( 'FO', FNAME2)
        CALL ERR_REP( 'MESSAGE', 'Error, opening output file ^FO',
     :  STATUS )
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
	CALL NDF_ANNUL( LOCI, STATUS )

	END
