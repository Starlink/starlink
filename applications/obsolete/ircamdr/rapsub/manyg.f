
*+  MANYG - generates a specified number of Gaussian stars in an image

      SUBROUTINE MANYG ( IDIMS1, IDIMS2, IMAGE, MAX, MIN, SKY, NSTARS,
     :                   SCALE, SEEING, DISTRIB, BADPIX, FRACTION,
     :                   BADCOL, DISPLAY, SCREEN, FILENAME, STATUS )

*    Description :
*
*     This routine generates a specified number of Gaussian star images
*     with dimensions, intensities and so on as input. A pseudo-Poisson
*     noise distribution is added, and the option to simulate bad pixels
*     and columns is included. The relevant stellar parameters may be
*     output via the screen or into a file.
*
*    Invocation :
*
*       CALL MANYG ( IDIMS, IMAGE, MAX, MIN, SKY, NSTARS,
*     :              SCALE, SEEING, DISTRIB, BADPIX, FRACTION,
*     :              BADCOL, DISPLAY, SCREEN, FILENAME, STATUS )
*
*
*    Method :
*
*    Deficiencies :
*
*     Uses Vax specific random number generator RAN
*     Uses Fortran OPEN and CLOSE statements
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     14-10-1985 : First Adam implementation (from Aspic MANYG) (REVA::MJM)
*     09-12-1985 : Changed to call POISSON noise subroutine (UKTH::MJM)
*     14-01-1986 : Added display options (REVA::MJM)
*     24-11-1986 : Bug fix in FRACTION to NUMBAD conversion (HILO::MJM)
*     04-03-1989 : Fixed gaussian to use SIGMA not FWHM (JACH::CAA)
*     20-JUL-1994  Changed VAX-specific RAN function to NAG G05 rouitines
*                  removed SEED as no longer required;
*                  channged arguments to input DIMS separately so that
*                  routine will still compile  (SKL@JACH)
*     12-AUG-2004  Now use PDA for random numbers (TIMJ@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    IDIMS1,             ! dimensions of image
     :    IDIMS2,             ! dimensions of image
     :    NSTARS                  ! number of stars to be generated

      REAL
     :    MAX,                    ! maximum stellar intensity
     :    MIN,                    ! minimum    "        "
     :    SKY,                    ! sky background value to be used
     :    SCALE,                  ! pixel scale in arcseconds
     :    SEEING,                 ! seeing in arcseconds
     :    FRACTION                ! fraction of bad pixels to be made

      CHARACTER*3
     :    DISTRIB                 ! stellar radial distribution

      CHARACTER*(*)
     :    FILENAME                ! filename to be used for output of
                                  ! stellar parameters

      LOGICAL
     :    BADPIX,                 ! true if bad pixels to be included
     :    BADCOL,                 ! true if bad column to be included
     :    DISPLAY,                ! true if parameters to be output
     :    SCREEN                  ! true if output to be to screen

*    Export :

      REAL
     :    IMAGE( IDIMS1, IDIMS2 )     ! image to be created

*    Status :

      INTEGER  STATUS             ! global status parameter

* External function

      REAL PDA_RAND
      EXTERNAL PDA_RAND

*    Local variables :

      INTEGER
*     :    IFAIL,                  ! NAG error
     :    XSTART,                 ! x start coord of box round star
     :    YSTART,                 ! y   "     "    "  "    "     "
     :    XFINISH,                ! x finish coord of box round star
     :    YFINISH,                ! y    "     "    "  "    "     "
     :    XBAD,                   ! x position of random bad pixel
     :    YBAD,                   ! y     "     "    "    "    "
     :    CURRBAD,                ! number of pixels currently set bad
     :    NUMBAD,                 ! number required to be set bad
     :    COLPOS,                 ! random bad column position
     :    I, J, K, L, N,          ! general array counters
     :    SEED, TICKS             ! For random number seed

      INTEGER
     :    BOX,                    ! half size of box round each star
                                  ! centre in which each pixel is set
                                  ! to the calculated Gaussian value
     :    UNITNUM                 ! unit number used for display

      REAL
     :    X,                      ! dummy variable for NAG routine
     :    VALUE,                  ! general random variable from RAN
     :    DUMMY,                  ! dummy variable used when adding noise
     :    FWHM,                   ! FWHM of Gaussian profile in pixels
     :    INTERVAL,               ! interval between max and min star peaks
     :    XPOS,                   ! randomly generated x centre of star
     :    YPOS,                   !     "        "     y    "    "   "
     :    PEAK,                   ! peak brightness of current star
     :    CURRX,                  ! x distance of pixel from star centre
     :    CURRY,                  ! y     "     "   "     "    "     "
     :    INTENSITY,              ! calculated intensity for current pixel
                                  ! from Gaussian profile formula
     :	  SIGMA                   ! sigma of gaussian

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

      UNITNUM = 0

      PRINT *, IDIMS1, IDIMS2, MAX, MIN, SKY, NSTARS,
     :                   SCALE, SEEING, DISTRIB, BADPIX, FRACTION,
     :                   BADCOL, DISPLAY, SCREEN, FILENAME

*    start by checking to see if output of the stellar parameters
*    is required, and act accordingly
      IF ( DISPLAY .AND. .NOT. SCREEN ) THEN

*       output to a file is required - set the unit number
*       and open the file
         UNITNUM  =  10
         OPEN( UNITNUM, FILE=FILENAME, STATUS='NEW' )

      ELSE IF ( DISPLAY .AND. SCREEN ) THEN

*       display is required to the screen - set unit number to screen
         UNITNUM  =  6

      END IF

*    work out the interval between max and min
      INTERVAL  =  MAX - MIN

*    work out the Gaussian full width at half maximum (FWHM) from
*    the input seeing and scalesize
      FWHM  =  SEEING / SCALE

*    work out sigma of gaussian profile assuming sigma = fwhm/2.354
      SIGMA = FWHM/2.354

*    work out a box size to be used from the Gaussian FWHM
      BOX   =  NINT( FWHM * 5 )

*    fill array with the sky value
	PRINT *, 'before do 1'
      DO  J  =  1, IDIMS2
         DO  I  =  1, IDIMS1
            IMAGE( I, J )  =  SKY
         ENDDO
      ENDDO

*    initialise the random number generator seed using system clock

      CALL PSX_TIME( TICKS, STATUS )
      SEED = ( TICKS / 4 ) * 4 + 1
      CALL PDA_RNSED( SEED )
	PRINT *, 'after pda_rnsed'

*    loop round the requested number of stars generating a random
*    x,y position and a peak intensity. Then create a Gaussian star
*    of that intensity at that point
      DO  N  =  1, NSTARS
	PRINT *, n

*       get a random, non-integral x,y position
         VALUE  =  PDA_RAND(X)
         XPOS   =  VALUE * IDIMS1
         VALUE  =  PDA_RAND(X)
         YPOS   =  VALUE * IDIMS2

*       now get a random peak intensity within the specified range,
*       and according to the specified radial distribution
         VALUE  =  PDA_RAND(X)

*       force the distribution string to upper case before checking it
         CALL UPCASE( DISTRIB, DISTRIB, STATUS )

         IF( DISTRIB .EQ. 'FIX' ) THEN
*          we are generating all our stars at the same radial distance
*          as for a protostellar cluster. Thus the stars should be more
*          or less uniformly distributed in brightness.
            PEAK  =  ( VALUE * INTERVAL ) + MIN

         ELSE
*          the requested distribution is one over r-squared - thus we
*          have one factor of r-squared from the fact that we see more
*          stars the further away we look, and another factor of r-squared
*          as the brightness of a star dims as the square of its distance.
            VALUE  =  VALUE ** 4
            PEAK   =  ( VALUE * INTERVAL ) + MIN

         END IF

*       now we have the x,y postion, peak intensity, and FWHM, we can
*       create the Gaussian star - first define a box in which the star
*       sits. BOX is the half side dimension of the box in pixels.
         XSTART   =  NINT( XPOS - ( BOX * FWHM ) )
         XFINISH  =  NINT( XPOS + ( BOX * FWHM ) )
         YSTART   =  NINT( YPOS - ( BOX * FWHM ) )
         YFINISH  =  NINT( YPOS + ( BOX * FWHM ) )

*       loop round the y dimension of the calculated box
         DO  L  =  YSTART, YFINISH

*          check whether or not this pixel is in array - continue if so
            IF( L .GE. 1 .AND. L .LE. IDIMS2 ) THEN

*             work out y distance of pixel from star centre
               CURRY  =  ABS( REAL( L ) - YPOS )

*             loop round x dimension of box
               DO  K  =  XSTART, XFINISH

*                check whether pixel in array - continue if so
                  IF( K .GE. 1 .AND. K .LE. IDIMS1 ) THEN

*                   work out x distance of pixel from star centre
                     CURRX  =  ABS( REAL( K ) - XPOS )

*                   work out intensity at this point from Gaussian
*                   profile formula
                     INTENSITY  =  PEAK *
     :  EXP( -( CURRX*CURRX + CURRY*CURRY ) / ( 2.0 * SIGMA * SIGMA ) )

*                   add this intensity to any existing flux
                     IMAGE( K, L )  =  IMAGE( K, L ) + INTENSITY

                  ENDIF
               ENDDO
            ENDIF
         ENDDO

*    write out the stellar parameters if so requested
      IF ( DISPLAY ) THEN
         WRITE( UNITNUM, 100 )N, XPOS, YPOS, PEAK
100      FORMAT( 3X,'Star : ',I3,'  x : ',F8.3,'  y : ',F8.3,
     :           '  peak : ',F9.3 )
      END IF

*    bottom of DO N = 1, NSTARS loop
      END DO

*    close any open units at this pont
      IF ( DISPLAY .AND. .NOT. SCREEN ) THEN
         CLOSE( UNITNUM )
      END IF

*    now we have created the background and the stars, we can add the
*    pseudo-Poisson noise

*    loop around all image pixels
      DO  J  =  1, IDIMS2
         DO  I  =  1, IDIMS1

*          call the routine POISSON which takes a number and returns
*          a semi-random number near the input according to a Poisson
*          statistical distribution

            CALL POISSON( IMAGE( I, J ), DUMMY, STATUS )
            IMAGE( I, J )  =  DUMMY

         END DO
      END DO


*    next, create some bad pixels if so requested - bad is defined as
*    dead in this context

      IF( BADPIX ) THEN

*       work out number of bad pixels from input fraction
         NUMBAD  =  INT( FRACTION * IDIMS1  * IDIMS2 )

*       initialise the current number of pixels set bad
         CURRBAD  =  0

*       loop until the requested number of pixels are set bad
         DO WHILE( CURRBAD .LT. NUMBAD )

*          using the RAN function again, get a random bad pixel position,
*          making sure it is in the image
            VALUE  =  PDA_RAND(X)
            XBAD   =  NINT( VALUE * IDIMS1 )
            IF( XBAD .EQ. 0 ) XBAD = 1
            VALUE  =  PDA_RAND(X)
            YBAD   =  NINT( VALUE * IDIMS2 )
            IF( YBAD .EQ. 0 ) YBAD = 1

*          if this pixel is non-zero, set it to zero, and increment
*          the counter by one - else continue
            IF( IMAGE( XBAD, YBAD ) .NE. 0.0 ) THEN
               IMAGE( XBAD, YBAD )  =  0.0
               CURRBAD  =  CURRBAD + 1
            END IF

         END DO

*    end of IF( BADPIX ) condition
      END IF


*    finally, include a bad column if requested
      IF( BADCOL ) THEN

*       get a random column that is in the image
         VALUE   =  PDA_RAND(X)
         COLPOS  =  NINT( VALUE * IDIMS1 )
         IF( COLPOS .EQ. 0 ) COLPOS = 1

*       set all pixels in that column to zero
         DO  J  =  1, IDIMS2
            IMAGE( COLPOS, J ) = 0.0
         END DO

*    end of IF( BADCOL ) condition
      END IF


*    that's it - return
      END
