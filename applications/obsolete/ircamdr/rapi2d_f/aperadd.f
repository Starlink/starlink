
*+  APERADD - bins up pixels within a specified circle

      SUBROUTINE APERADD ( STATUS )

*    Description :
*
*     This routine takes an input array and bins up all the pixels
*     that lie within a user specified circle to either increase
*     the signal-to-noise over that region, or to simulate a circular
*     aperture measurement of the image as with single element
*     detector instruments.
*     Output are the noise on the intensity of the pixels before
*     binning, the integrated value over the aperture, and the
*     calculated mean signal level and reduced noise after binning.
*
*    Invocation :
*
*     CALL APERADD( STATUS )
*
*    Parameters :
*
*     INPIC = IMAGE( READ )
*            Input image to be processed
*     XCEN = REAL( READ )
*            x coordinate of circle centre
*     YCEN = REAL( READ )
*            y coordinate of circle centre
*     DIAM = REAL( READ )
*            Diameter of circle in arbitrary units (e.g. ")
*     SCALE = REAL( READ )
*            Number of units per pixel (e.g. "/pixel)
*     AGAIN = LOGICAL( READ )
*            Another input option
*     QUIET = LOGICAL( READ )
*            Whether output is restricted to result
*
*    Method :
*
*     Get input image from environment
*     If no error so far then
*        Map in DATA_ARRAY
*        Output image dimensions to user
*        Get circle centre x, y coordinates somewhere on array
*        Initialise circle diameter to invalid value 0.0
*        Do while no valid circle diameter obtained
*           Get circle diameter in arbitrary units
*           Get pixel scale in these arbitrary units
*           Work out circle diameter in pixel units
*           If circle diameter less than one pixel then
*              Write out error message
*              Cancel parameter values
*           Endif
*        Enddo
*        If no error so far then
*           Call subroutine APERADDSUB to do work
*           Write out results on return
*        Endif
*        Clear up input data
*     Endif
*     End
*
*    Deficiencies :
*
*     The circle centre must lie somewhere on the array, which is
*     by far the most likely option, but somewhat ungeneral.
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
*     22-10-1985 : First implementation (REVA::MJM)
*     17-01-1986 : More error checking and tidying (REVA::MJM)
*     03-12-1987 : ask if want another input (UKTH::CAA)
*     24-10-1990 : added output restriction (JACH::CAA)
*     10-MAR-94    DAT_,CMP_ calls changed to NDF_ (SKL@JACH)
*     12-Aug-1994  Changed input DIM arguments for APERADDSUB (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'          ! SSE global definitions

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      INTEGER  NDIMS              ! dimensionality of input data
      PARAMETER( NDIMS = 2 )      ! 2d images only

*    Local variables :

      INTEGER
     :     IDIMS( NDIMS ),     ! input array dimensions
     :     PNTRI,              ! pointer to input image
     :     NUMPIX,             ! number of pixels added in circle
     :     NELEMENTS,          ! number of elements mapped
     :     NDIM,               ! Number dimensions from NDF_DIM
     :     LOCI                ! locator to input image structure

      REAL
     :     XCEN,               ! x coord of circle centre
     :     YCEN,               ! y   "    "    "      "
     :     DIAM,               ! circle diameter in arbitrary units
     :     SCALE,              ! number of units per pixel
     :     PIXDIAM,            ! circle diameter in pixels
     :     TOTAL,              ! total intensity in added circle
     :     MEAN,               ! mean      "      "   "      "
     :     OLDNOISE,           ! standard deviation of pixels before add
     :     NEWNOISE            !     "        "      "    "   after   "

	LOGICAL
     :	   AGAIN,              ! another input option
     :	   QUIET               ! output option

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    set up default position, aperture size and plate scale
      XCEN = 31
      YCEN = 29
      DIAM = 10.0
      SCALE = 1.0

*    start by obtaining the input image structure locator
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    proceed if no error
      IF ( STATUS .EQ. SAI__OK ) THEN

*       get the output option
	 CALL PAR_GET0L( 'QUIET', QUIET, STATUS)

*       map its DATA_ARRAY component onto a pointer
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

*       get array dimensions
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS )

*       write out the array dimensions to the user
	 IF( .NOT. QUIET) THEN
           CALL MSG_SETI( 'XDIM', IDIMS( 1 ) )
           CALL MSG_SETI( 'YDIM', IDIMS( 2 ) )
           CALL MSG_OUT( 'INPUT_DIMS',
     :        'Image is ^XDIM by ^YDIM pixels', STATUS )
	 END IF

*       get the circle centre coordinates - the circle centre
*       cannot be off edge of the image
  100	 CONTINUE

         CALL AIF_GET0R( 'XCEN', XCEN, 0.0,
     :                    REAL( IDIMS( 1 ) ), XCEN, STATUS )

         CALL AIF_GET0R( 'YCEN', YCEN, 0.0,
     :                    REAL( IDIMS( 2 ) ), YCEN, STATUS )

*       initialise the circle diameter before entering loop
*       to get a valid value
         PIXDIAM  =  0.0

*       loop to get valid circle diameter
         DO WHILE( PIXDIAM .LE. 1.0 )

*          get the diameter of the circle in arbitrary units
            CALL AIF_GET0R( 'DIAM', DIAM, 0.0001, 10000.0,
     :                       DIAM, STATUS )

*          get the size of a pixel in these arbitrary units
            CALL AIF_GET0R( 'SCALE', SCALE, 0.0001, 10000.0,
     :                       SCALE, STATUS )

*          check for error here - do not divide unless parameters have
*          been read in safely - not the case if STATUS = PAR__NULL
            IF ( STATUS .NE. SAI__OK ) THEN

*              just announce the error, clear up and abort
                CALL ERR_REP( 'FUNNY_PARS',
     :     'Something wrong with input parameters - aborting', STATUS )
                CALL NDF_ANNUL( LOCI, STATUS )
                RETURN

            END IF

*          work out the circle diameter in pixels
            PIXDIAM  =  DIAM / SCALE

*          check this diameter is big enough to continue with
            IF( PIXDIAM .LE. 1.0 ) THEN

*             circle diameter of less than one pixel - exit
               CALL MSG_SETR( 'PIXDIAM', PIXDIAM )
               CALL MSG_OUT( 'TOO_SMALL',
     :     'Circle diameter requested is ^PIXDIAM - must be > 1'/
     :    /' - try again', STATUS )

*             cancel parameters before looping
               CALL PAR_CANCL( 'DIAM', STATUS )
               CALL PAR_CANCL( 'SCALE', STATUS )

*          end of if-circle-too-small check
            END IF

*       bottom of circle diameter loop
         END DO

*       check for error before calling working subroutine
         IF ( STATUS .EQ. SAI__OK ) THEN

*          given valid x,y centre and circle diameter, call the
*          subroutine that does the actual work
            CALL APERADDSUB( %VAL( PNTRI ), IDIMS(1), IDIMS(2),
     :                       XCEN, YCEN, PIXDIAM, NUMPIX, OLDNOISE,
     :                       TOTAL, MEAN, NEWNOISE, STATUS )


*          on return, output the relevant figures
	    CALL PAR_PUT0I( 'NUMPIX', NUMPIX, STATUS)

	    IF( .NOT. QUIET) THEN
              CALL MSG_OUT( 'BLANK', ' ', STATUS )
              CALL MSG_SETI( 'NUMPIX', NUMPIX )
              CALL MSG_OUT( 'APER_NUMPIX',
     :          'Number of pixels binned together     = ^NUMPIX',
     :          STATUS )
	    END IF

	    CALL PAR_PUT0R( 'TOTAL', TOTAL, STATUS)

	    IF( .NOT. QUIET) THEN
              CALL MSG_SETR( 'TOTAL', TOTAL )
              CALL MSG_OUT( 'APER_TOTAL',
     :          'Total intensity in binned pixels     = ^TOTAL',
     :          STATUS )
	    END IF

	    CALL PAR_PUT0R( 'MEAN', MEAN, STATUS)

	    IF( .NOT. QUIET) THEN
              CALL MSG_SETR( 'MEAN', MEAN )
              CALL MSG_OUT( 'APER_MEAN',
     :          'Mean intensity over circle           = ^MEAN',
     :          STATUS )
	    END IF

	    CALL PAR_PUT0R( 'OLDNOISE', OLDNOISE, STATUS)

	    IF( .NOT. QUIET) THEN
              CALL MSG_SETR( 'OLDNOISE', OLDNOISE )
              CALL MSG_OUT( 'APER_OLDNOISE',
     :          'Noise for pixels before binning      = ^OLDNOISE',
     :          STATUS )
	    END IF

	    CALL PAR_PUT0R( 'NEWNOISE', NEWNOISE, STATUS)

	    IF( .NOT. QUIET) THEN
              CALL MSG_SETR( 'NEWNOISE', NEWNOISE )
              CALL MSG_OUT( 'APER_NEWNOISE',
     :          'Noise for pixels after binning       = ^NEWNOISE',
     :          STATUS )
              CALL MSG_OUT( 'BLANK', ' ', STATUS )
	    END IF

*       end of if-no-error-before-calling-subroutine check
         END IF

*       cancel association with input parameters
	 CALL PAR_CANCL( 'XCEN', STATUS)
	 CALL PAR_CANCL( 'YCEN', STATUS)
	 CALL PAR_CANCL( 'DIAM', STATUS)
	 CALL PAR_CANCL( 'SCALE', STATUS)

*       ask if want to input another area
	 CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS)
	 CALL PAR_CANCL( 'AGAIN', STATUS)

         CALL MSG_OUT( 'BLANK', ' ', STATUS )

	 IF( AGAIN) GOTO 100

*    end of if-no-error-after-getting-input check
      END IF

*    now tidy up input data
      CALL NDF_ANNUL( LOCI, STATUS )

      END
