*+  MANYG - generates a specified number of Gaussians in a 2-d array

      SUBROUTINE MANYG ( IDIM1, IDIM2, MAX, MIN, BCKGRD, NGAUSS, FWHM,
     :                   DISTRB, BADPIX, FRACTN, BADCOL, SCREEN,
     :                   FILNAM, IMAGE, STATUS )
*
*    Description :
*
*     This routine generates a specified number of 2-D Gaussian images
*     with dimensions, intensities and so on as input. A pseudo-
*     Poissonian noise distribution is added, and the option to
*     simulate bad pixels and columns is included. The relevant Gaussian
*     parameters may be output via the screen or into a file.
*
*    Invocation :
*
*       CALL MANYG ( IDIM1, IDIM2, MAX, MIN, BCKGRD, NGAUSS, FWHM,
*     :              DISTRB, BADPIX, FRACTN, BADCOL, SCREEN, FILNAM,
*     :              IMAGE, STATUS )
*
*    Arguments :
*
*     IDIM1 = INTEGER( READ )
*         First dimension of array to be created
*     IDIM2 = INTEGER( READ )
*         Second dimension of array to be created
*     MAX = REAL( READ )
*         Maximum stellar intensity
*     MIN = REAL( READ )
*         Minimum stellar intensity
*     BCKGRD = REAL( READ )
*         Background value to be used
*     NGAUSS = INTEGER ( READ )
*         Number of stars to be generated
*     FWHM = REAL( READ )
*         Full Width Half Maximum of Gaussian (equivalent to seeing)
*           in pixels
*     DISTRB = CHARACTER*3( READ )
*         Radial distribution of Gaussians - default (RSQ) gives
*           1-over-r squared; FIX gives fixed distance
*     BADPIX = LOGICAL( READ )
*         True if bad pixels are to be included
*     FRACTN = REAL( READ )
*         Fraction of pixels that are to be made bad
*     BADCOL = LOGICAL( READ )
*         True if a bad column is to be included
*     SCREEN = LOGICAL( READ )
*         True if Gaussian parameters are to be reported
*     FILNAM = CHARACTER*(*)( READ )
*         Parameter name for the filename to be used for the output of
*           Gaussian parameters
*     IMAGE( IDIM1, IDIM2 ) = REAL( WRITE )
*         Created image
*     STATUS = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Open logging file if required
*     If error then abort
*     Find intensity interval, define boxsize in terms of FWHM
*     Fill output array with the constant background value
*     Initialise the random-number seed
*     For each Gaussian
*        Get a random x,y position
*        Get an intensity within the specified range subject to the
*          distribution type
*        Find x,y limits of box
*        For each pixel within box
*           If pixel lies within main array then
*              Find x,y distances from Gaussian's centre
*              Compute intensity and add to the pixel in the array
*           Endif
*        Endfor
*        Write out Gaussian parameters if requested
*     Endfor
*     For each pixel
*        Add pseudo-Poissonian noise
*     Endfor
*     If bad pixels required then
*        Find number of bad pixels
*        For the number of bad pixels
*           Obtain a random x,y position
*           If pixel not already bad
*              Set the pixel bad
*              Increment counter of bad pixels added
*           Endif
*        Endfor
*     End if
*     If a bad column is required then
*        get a random column with the array
*        make all pixels in the column bad
*     Endif
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     14-10-1985 : First Adam implementation (from Aspic MANYG)
*                : (REVA::MJM)
*     09-12-1985 : Changed to call POISSON noise subroutine
*                : (UKTH::MARK)
*     14-01-1986 : Added display options (REVA::MJM)
*     1986 Aug 14: Changed argument order, renamed POISSON and reordered
*                  its argument, completed the prologue, nearly 
*                  conformed to Starlink programming standards, open
*                  IOSTAT added (RL.STAR::CUR).
*     1986 Sep 4 : Renamed parameters section to arguments, applied
*                  bad-pixel handling (RL.STAR::CUR).
*     1987 Nov 30: Bug fix - now uses SIGMA for Gaussian, and E format
*                  used for output of peak values (RL.STAR::CUR)
*     1988 Jun 27: Converted to FIO, added error reporting and
*                  restructured (RL.STAR::CUR).
*     1988 Aug 5 : Removed lingering astronomical references and
*                  SCALE argument (RL.STAR::CUR).
*     1989 Jul 25: Removed DSPLAY argument --- output can now be to
*                  both, either, or neither the screen and/or the file;
*                  passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*     1989 Jul 27: Used packaged access for obtaining the log file
*                  (RL.STAR::CUR).
*     1990 Feb 20: AIF_OPFIO renamed AIF_ASFIO (RAL::CUR).
*     1990 Aug 3 : Converted to use Starlink standard co-ordinates
*                  rather than pixel indices; output file will now
*                  work with PHOTOM (RAL::CUR).
*     1992 Mar 17: Used portable random-number generation (RAL::CUR).
*     1993 Feb 9 : Used the improved FIO_ASSOC and the new FIO_ANNUL.
*                  (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE           ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitionsffssdaw
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'PRM_PAR'        ! Magic-value constants

*    Import :

      INTEGER
     :     IDIM1, IDIM2,
     :     NGAUSS

      REAL
     :     MAX,
     :     MIN,
     :     BCKGRD,
     :     FWHM,
     :     FRACTN

      CHARACTER*3
     :     DISTRB

      CHARACTER*(*)
     :     FILNAM

      LOGICAL
     :     BADPIX,
     :     BADCOL,
     :     SCREEN

*    Export :

      REAL
     :     IMAGE( IDIM1, IDIM2 )

*    Status :

      INTEGER  STATUS

*    External References:
      REAL SLA_RANDOM           ! Random-number generator
      REAL KPG1_SEED            ! Random-number seed initialisation

*    Local Constants :

      INTEGER
     :    NCHLIN               ! maximum number of characters in an
                               ! output record
      PARAMETER ( NCHLIN = 44 )

*    Local variables :

      INTEGER
     :    BOX,                 ! half size of box round each star 
                               ! centre in which each pixel is set 
                               ! to the calculated Gaussian value
     :    FD,                  ! file description
     :    XSTART,              ! x start co-ord of box round Gaussian
     :    YSTART,              ! y   "     "    "  "    "       "
     :    XFINSH,              ! x finish co-ord of box round Gaussian
     :    YFINSH,              ! y    "     "    "  "    "       "
     :    XBAD,                ! x position of random bad pixel
     :    YBAD,                ! y     "     "    "    "    "
     :    CURBAD,              ! number of pixels currently set bad
     :    NUMBAD,              ! number required to be set bad
     :    COLPOS,              ! random bad column position
     :    I, J, K, L, N        ! general counters

      REAL
     :    SEED,                ! seed variable for random numbers
     :    SIGMA,               ! Standard deviation of Gaussian in
                               ! pixels
     :    INTRVL,              ! interval between max. and min. Gaussian
                               ! peaks
     :    VALUE,               ! general random variable from SLA_RANDOM
     :    XPOS,                ! randomly generated x centre of Gaussian
     :    YPOS,                !     "        "     y    "    "    "
     :    PEAK,                ! peak brightness of current Gaussian
     :    CURRX,               ! x distance of pixel from Gaussian
                               ! centre
     :    CURRY,               ! y distance of pixel from Gaussian
                               ! centre
     :    INTENS               ! calculated intensity for current pixel
                               ! from Gaussian profile formula

      LOGICAL                  ! true if:
     :    LOGGP                ! Gaussian parameters are to be output to
                               ! a log file

      CHARACTER*(NCHLIN)
     :    BUFFER               ! buffer to store output string

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    start by checking to see if output of the Gaussian parameters
*    is required, and act accordingly

      LOGGP = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( FILNAM, 'WRITE', 'FORTRAN', NCHLIN, FD, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGGP = .TRUE.
      END IF
      CALL ERR_RLSE
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( LOGGP ) THEN
         CALL MSG_SETC( 'FILNAM', FILNAM )
         CALL MSG_OUT( 'LOG', 'Logging to $^FILNAM', STATUS )
      END IF

*    Write caption if the Gaussians used are to be reported to the user.
*    Ideally, for the log file there should be a description file for
*    use with SCAR.

      IF ( SCREEN ) THEN
         BUFFER = '  Number   x centre  y centre      Peak'

*       Write to the user directly.

         CALL MSG_SETC( 'BUFFER', BUFFER )
         CALL MSG_OUT( 'RESULT', BUFFER, STATUS )

      END IF

*    work out the interval between max and min

      INTRVL  =  MAX - MIN

*    work out a box size to be used from the Gaussian FWHM

      BOX   =  NINT( FWHM * 5 )

*    convert FWHM to standard error

      SIGMA = FWHM / 2.354

*    fill array with the background value

      DO  J  =  1, IDIM2
         DO  I  =  1, IDIM1
            IMAGE( I, J )  =  BCKGRD
         END DO
      END DO

*    Initialise the random-number generator seed.  It is taken as input
*    by SLA_RANDOM, which returns a pseudo-random number between 0 and
*    1, and is updated on return.

      SEED = KPG1_SEED( STATUS )

*    loop round the requested number of Gaussians generating a random
*    x,y position and a peak intensity. Then create a Gaussian of that
*    intensity at that point

      DO  N  =  1, NGAUSS

*       get a random, non-integral x,y position

         VALUE  =  SLA_RANDOM( SEED )
         XPOS   =  VALUE * REAL( IDIM1 )
         VALUE  =  SLA_RANDOM( SEED )
         YPOS   =  VALUE * REAL( IDIM2 )

*       now get a random peak intensity within the specified range,
*       and according to the specified radial distribution

         VALUE  =  SLA_RANDOM( SEED )

*       force the distribution string to upper case before checking it

         CALL CHR_UCASE( DISTRB )

         IF ( DISTRB .EQ. 'FIX' ) THEN

*          we are generating all our Gaussian at the same radial
*          distance. The Gaussians should be more or less uniformly
*          distributed in brightness.

            PEAK  =  ( VALUE * INTRVL ) + MIN
         ELSE

*          ! Warning - ASTRONOMICAL content !
*          the requested distribution is one over r-squared - thus we
*          have one factor of r-squared from the fact that we see more
*          stars the further away we look, and another factor of 
*          r-squared as the brightness of a star dims as the square of
*          its distance.

            VALUE  =  VALUE ** 4
            PEAK   =  ( VALUE * INTRVL ) + MIN
         END IF

*       now we have the x,y position, peak intensity, and FWHM, we can
*       create the Gaussian star - first define a box in which the star
*       sits. BOX is the half-side dimension of the box in pixels.

         XSTART  =  NINT( XPOS - ( BOX * FWHM ) + 0.5 )
         XFINSH  =  NINT( XPOS + ( BOX * FWHM ) + 0.5 )
         YSTART  =  NINT( YPOS - ( BOX * FWHM ) + 0.5 )
         YFINSH  =  NINT( YPOS + ( BOX * FWHM ) + 0.5 )
       
*       loop round the y dimension of the calculated box

         DO  L  =  YSTART, YFINSH

*          check whether or not this pixel is in array - continue if so

            IF ( L .GE. 1 .AND. L .LE. IDIM2 ) THEN

*             work out y distance of pixel from Gaussian's centre

               CURRY  =  ABS( REAL( L ) - 0.5 - YPOS )

*             loop round x dimension of box

               DO  K  =  XSTART, XFINSH

*                check whether pixel in array - continue if so

                  IF ( K .GE. 1 .AND. K .LE. IDIM1 ) THEN

*                   work out x distance of pixel from Gaussian's centre

                     CURRX  =  ABS( REAL( K ) - 0.5 - XPOS )

*                   work out intensity at this point from Gaussian
*                   profile formula

                     INTENS  =  PEAK * 
     :                          EXP( -( CURRX*CURRX + CURRY*CURRY ) /
     :                          ( 2.0 * SIGMA * SIGMA ) )

*                   add this intensity to any existing flux

                     IMAGE( K, L )  =  IMAGE( K, L ) + INTENS

                  END IF
               END DO
            END IF
         END DO
      
*       write out the Gaussian parameters if so requested

         IF ( SCREEN .OR. LOGGP ) THEN

            WRITE( BUFFER, '( I6,2X,2F10.3,E15.6 )' ) N, XPOS, YPOS,
     :             PEAK
         END IF

         IF ( SCREEN ) THEN

*          Write to the user directly.

            CALL MSG_SETC( 'BUFFER', BUFFER )
            CALL MSG_OUT( 'RESULT', BUFFER, STATUS )
         END IF

         IF ( LOGGP ) THEN

*          write to file

            CALL FIO_WRITE( FD, BUFFER, STATUS )

*          report error context

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'FILNAM', FILNAM )
               CALL ERR_REP( 'ERR_MANYG_WDATA',
     :           'MANYG: Error writing results to the file $^FILNAM',
     :           STATUS )
            END IF
         END IF

*    bottom of loop for Gaussians

      END DO

*    close any open units at this point

      IF ( LOGGP ) CALL FIO_ANNUL( FD, STATUS )

*    We have created the background and the Gaussians, we can add
*    the pseudo-Poissonian noise.  The routine KPG1_POISR takes the
*    array and adds or subtracts a semi-random number to the input
*    according to a Poisson statistical distribution.

      CALL KPG1_POISR( IDIM1 * IDIM2, IMAGE, SEED, STATUS )

*    next, create some bad pixels if so requested - bad is defined as
*    dead in this context

      IF ( BADPIX ) THEN

*       work out number of bad pixels from input fraction

         NUMBAD  =  INT( FRACTN * REAL( IDIM1 * IDIM2 )  )

*       initialise the current number of pixels set bad

         CURBAD  =  0

*       loop until the requested number of pixels are set bad

         DO WHILE( CURBAD .LT. NUMBAD )

*          using the SLA_RANDOM function again, get a random bad-pixel
*          position, making sure it is in the image

            VALUE  =  SLA_RANDOM( SEED )
            XBAD   =  NINT( VALUE * REAL( IDIM1 ) )
            IF ( XBAD .EQ. 0 ) XBAD = 1
            VALUE  =  SLA_RANDOM( SEED )
            YBAD   =  NINT( VALUE * REAL( IDIM2 ) )
            IF ( YBAD .EQ. 0 ) YBAD = 1

*          if this pixel is non-zero, set it to zero, and increment
*          the counter by one - else continue

            IF ( IMAGE( XBAD, YBAD ) .NE. VAL__BADR ) THEN
               IMAGE( XBAD, YBAD )  =  VAL__BADR
               CURBAD  =  CURBAD + 1
            END IF

         END DO

*    end of IF ( BADPIX ) condition

      END IF

*    finally, include a bad column if requested

      IF ( BADCOL ) THEN

*       get a random column that is in the image

         VALUE   =  SLA_RANDOM( SEED )
         COLPOS  =  NINT( VALUE * REAL( IDIM1 ) )
         IF ( COLPOS .EQ. 0 ) COLPOS = ( IDIM1 + 1 )/ 2

*       set all pixels in that column to zero

         DO  J  =  1, IDIM2
            IMAGE( COLPOS, J ) = VAL__BADR
         END DO

*    end of IF ( BADCOL ) condition

      END IF

 999  CONTINUE

*    that's it - return

      END
