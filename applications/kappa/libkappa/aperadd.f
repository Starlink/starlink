*+  APERADD - Derives statistics of pixels within a specified circle of 
*             a 2-d data array

      SUBROUTINE APERADD ( STATUS )
*
*    Description :
*
*     This routine takes an input 2-dimensional data array in an IMAGE
*     structure and bins up all the pixels that lie within specified
*     circles to either increase the signal-to-noise over that region,
*     or to simulate a circular-aperture measurement of the image.
*
*     WARNING: This simple task does not divide the light of pixels
*     spanning the circle.  If the pixel's centre lies within the circle,
*     its full value is included in the summation; if it lies outside
*     the circle, the pixel value is excluded from the summation.
*     Therefore this task is not suitable for accurate aperture
*     photometry, especially where the aperture diameter is less than
*     about ten times the pixel size.  Use PHOTOM where accuracy, rather
*     than speed, is paramount.

*     The following are displayed: the standard deviation of the
*     intensity of the pixels within the aperture before binning, the
*     integrated value over the aperture, and the calculated mean level
*     and reduced noise after binning.
*
*     The magic-value method is used for processing bad data.

*    Invocation :
*
*     CALL APERADD( STATUS )
*
*    Parameters :
*
*     LOGFILE = FILENAME( READ )
*         Name of the text file to record the statistics. If null,
*           there will be no logging.
*     INPIC = IMAGE( READ )
*         Input IMAGE structure containing data array to be processed
*     XCEN = REAL( READ )
*         x co-ordinate of the circle centre
*     YCEN = REAL( READ )
*         y co-ordinate of the circle centre
*     DIAM = REAL( READ )
*         Diameter of the circle in pixels
*     AGAIN = LOGICAL( READ )
*         If true then another aperture can be chosen
*     NUMPIX = INTEGER( WRITE )
*         The number of pixels within the aperture.
*     TOTAL = REAL( WRITE )
*         The total of the pixel values within the aperture.
*     MEAN = REAL( WRITE )
*         The mean of the pixel values within the aperture.
*     SIGMA = REAL( WRITE )
*         The standard deviation of the pixel values within the
*         aperture.
*     NOISE = REAL( WRITE )
*         The standard deviation of the pixel values within the
*         aperture after binning.
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Open the logfile if required
*     Get input IMAGE structure from environment
*     If no error so far then
*        Map in DATA_ARRAY
*        If no error so far then
*           Output array dimensions to user
*           Record the image name in the log file
*           For all centres required
*              Get circle centre x, y co-ordinates somewhere on array
*              Compute maximum diameter of circle in pixels
*              Get circle diameter in pixels
*              Record the input parameters  in the log file
*              If no error so far then
*                 Call subroutine APADSB to do work
*                 If no error occurred within subroutine then
*                    Write out results on return
*                    Record the results in the logfile if required
*                    See whether or not the loop is to continue
*                 Endif
*              Else
*                 Report error and abort loop
*              Endif
*              Cancel parameters for next try
*           Endfor
*           Record the statistics in output parameters
*           Unmap input data array
*        Else
*           Report error
*        Endif
*        Annul input IMAGE structure
*     Else
*        Report error
*     Endif
*     End
*
*    Deficiencies :
*
*     The circle centre must lie somewhere on the array, which is
*     by far the most likely option, but it not totally general.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL ( UK.AC.RL.STAR::CUR )
*
*    History :
*
*     22-10-1985 : First implementation (REVA::MJM)
*     17-01-1986 : More error checking and tidying (REVA::MJM) 
*     1986 Aug 3 : Renamed algorithm subroutine (APADSB) (RL.STAR::CUR)
*     1986 Aug 27: Argument section added and nearly conformed to
*                  Starlink standards (RL.STAR::CUR)
*     1987 Oct 13: Extra status check for mapping image (RL.STAR::CUR)
*     1988 Feb 14: Removed SCALE parameter and references to intensity
*                  and referred to `array' rather than `image'
*                  (RL.STAR::CUR)
*     1988 Apr 30: Correct labelling of output and a loop introduced
*                  to measure in more than one aperture (RL.STAR::CUR).
*     1988 May 29: More reporting of error context (RL.STAR::CUR)
*     1989 Jul 27: Passed array dimensions as separate variables
*                  to APADSB (RL.STAR::CUR).
*     1990 Mar 10: Logfile option added (RAL::CUR).
*     1990 Sep 18: Added results parameters (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*     1995 Sep 19: Clarified the limitations of APERADD.  Made it use
*                  the floating-point centre co-ordinates rather than
*                  pixel indices. (MJC)
*
*    Type definitions :

      IMPLICIT  NONE           ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! PARameter-system errors
      INCLUDE 'PRM_PAR'        ! Magic-value bad pixels

*    Status :

      INTEGER  STATUS

*    External references :

      INTEGER CHR_LEN          ! Length of string ignoring trailing
                               ! blanks

*    Local Constants :

      INTEGER  NDIMS           ! Dimensionality of input data
      PARAMETER( NDIMS = 2 )   ! 2d arrays only

*    Local variables :

      LOGICAL                  ! True if:
     :  AGAIN                  ! Another aperture is to selected

      INTEGER
     :  DIMS( NDIMS ),         ! Input array dimensions
     :  FDL,                   ! File description of logfile
     :  NC,                    ! Character column counter
     :  NCI,                   ! Character column counter of image name
     :  NUMPIX,                ! Number of pixels added in circle
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI                  ! Pointer to input array

      REAL
     :  MAXDIA,                ! Maximum diameter of the circle
     :  PIXDIA,                ! Circle diameter in pixels
     :  TOTAL,                 ! Total intensity in added circle
     :  MEAN,                  ! Mean      "      "   "      "
     :  OLNOIS,                ! Standard deviation of pixels before add
     :  NWNOIS,                !     "         "     "    "   after   "
     :  XCEN,                  ! x co-ord of circle centre
     :  YCEN                   ! y   "    "    "      "

      CHARACTER
     :  DATNAM*100,            ! Name of input IMAGE
     :  BUFFER*132             ! Buffer for writing to the logfile

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEI                 ! Name of the input data-array component

      CHARACTER*( DAT__SZLOC )
     :  LOCDI,                 ! structure containing the input data
                               ! array
     :  LOCI                   ! Locator to input IMAGE structure

      LOGICAL                  ! True if :
     :  LOGFIL                 ! A log file is being written

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Attempt to obtain and open a log file to list the statistics.  A
*    null value, meaning no logfile is required, is handled invisibly.

      CALL ERR_MARK
      LOGFIL = .FALSE.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 132, FDL, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGFIL = .TRUE.
      END IF
      CALL ERR_RLSE
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( LOGFIL ) CALL MSG_OUT( 'LOG', 'Logging to $LOGFILE.',
     :                             STATUS )

*    start by obtaining the input IMAGE structure locator

      CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*    proceed if no error

      IF ( STATUS .EQ. SAI__OK ) THEN

*       map its DATA_ARRAY component onto a pointer

         CALL CMP_MAPN( LOCDI, DNAMEI, '_REAL', 'READ', NDIMS,
     :                  PNTRI, DIMS, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*          write out the array dimensions to the user

            CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
            CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
            CALL MSG_OUT( 'INPUT_DIMS',
     :           'Array is ^XDIM by ^YDIM pixels', STATUS )

            IF ( LOGFIL ) THEN

*             Get the names of the data array to store in the log file.

               NC = 0
               BUFFER = ' '
               CALL AIF_FLNAM( 'INPIC', DATNAM, STATUS )
               NCI = CHR_LEN( DATNAM )
               CALL CHR_PUTC( 'Input IMAGE is ', BUFFER, NC )
               CALL CHR_PUTC( DATNAM( :NCI ), BUFFER, NC )
               CALL CHR_PUTC( '.', BUFFER, NC )
               CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
            END IF

            AGAIN = .TRUE.
            DO WHILE ( AGAIN .AND. STATUS .EQ. SAI__OK )

*             get the circle centre co-ordinates - the circle centre
*             cannot be off edge of the array and the maximum diameter
*             of the circle

               CALL PAR_GDR0R( 'XCEN', REAL( DIMS( 1 )/2.0 ), 0.0,
     :                         REAL( DIMS( 1 ) ), .FALSE., XCEN,
     :                         STATUS )
               CALL PAR_GDR0R( 'YCEN', REAL( DIMS( 2 )/2.0 ), 0.0,
     :                         REAL( DIMS( 2 ) ), .FALSE., YCEN,
     :                         STATUS )

               IF ( STATUS .EQ. SAI__OK )
     :            MAXDIA = SQRT( REAL( DIMS( 1 ) * DIMS( 1 ) ) +
     :                           REAL( DIMS( 2 ) * DIMS( 2 ) ) )

*             get the diameter of the circle in arbitrary units

               CALL PAR_GDR0R( 'DIAM', 10.0, 1.00001, MAXDIA, .FALSE.,
     :                         PIXDIA, STATUS )

*             check for error before calling working subroutine

               IF ( STATUS .EQ. SAI__OK ) THEN

                  IF ( LOGFIL ) THEN

                     CALL FIO_WRITE( FDL, ' ', STATUS )

*                   Build up the output for the logfile.

                     NC = 0
                     CALL CHR_PUTC( 'The centre of the circle is at '/
     :                              /'( ', BUFFER, NC )
                     CALL CHR_PUTR( XCEN, BUFFER, NC )
                     CALL CHR_PUTC( ', ', BUFFER, NC )
                     CALL CHR_PUTR( YCEN, BUFFER, NC )
                     CALL CHR_PUTC( ' ), and its diameter is ', BUFFER,
     :                              NC )
                     CALL CHR_PUTR( PIXDIA, BUFFER, NC )
                     CALL CHR_PUTC( ' pixels.', BUFFER, NC )
                     CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
                  END IF

*                given valid x,y centre and circle diameter, call the
*                subroutine that does the actual work

                  CALL APADSB( %VAL( PNTRI ), DIMS( 1 ), DIMS( 2 ),
     :                         XCEN, YCEN, PIXDIA, NUMPIX, OLNOIS,
     :                         TOTAL, MEAN, NWNOIS, STATUS )

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   on return, output the relevant figures by building
*                   output strings

                     CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                   Number of pixels...

                     NC = 0
                     CALL CHR_PUTC( 'Number of pixels binned together '/
     :                              /'    = ', BUFFER, NC )
                     CALL CHR_PUTI( NUMPIX, BUFFER, NC )
                     IF ( LOGFIL )
     :                 CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
                     CALL MSG_OUT( 'APER_NUMPIX', BUFFER( :NC ),
     :                             STATUS )

*                   Total...

                     NC = 0
                     CALL CHR_PUTC( 'Total value in binned pixels     '/
     :                              /'    = ', BUFFER, NC )

*                   Assign value to token depending on whether or not
*                   value is valid

                     IF ( TOTAL .NE. VAL__BADR ) THEN
                        CALL CHR_PUTR( TOTAL, BUFFER, NC )
                     ELSE
                        CALL CHR_PUTC( 'INVALID', BUFFER, NC )
                     END IF
                     IF ( LOGFIL )
     :                  CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
                     CALL MSG_OUT( 'APER_TOTAL', BUFFER( :NC ),
     :                             STATUS )

*                   Mean...

                     NC = 0
                     CALL CHR_PUTC( 'Mean value over circle           '/
     :                              /'    = ', BUFFER, NC )

*                   Assign value to token depending on whether or not
*                   value is valid

                     IF ( MEAN .NE. VAL__BADR ) THEN
                        CALL CHR_PUTR( MEAN, BUFFER, NC )
                     ELSE
                        CALL CHR_PUTC( 'INVALID', BUFFER, NC )
                     END IF
                     IF ( LOGFIL )
     :                  CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
                     CALL MSG_OUT( 'APER_MEAN', BUFFER( :NC ),
     :                             STATUS )

*                   Noise before binning...

                     NC = 0
                     CALL CHR_PUTC( 'Noise for pixels before binning  '/
     :                              /'    = ', BUFFER, NC )

*                   Assign value to token depending on whether or not
*                   value is valid

                     IF ( OLNOIS .NE. VAL__BADR ) THEN
                        CALL CHR_PUTR( OLNOIS, BUFFER, NC )
                     ELSE
                        CALL CHR_PUTC( 'INVALID', BUFFER, NC )
                     END IF
                     IF ( LOGFIL )
     :                  CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
                     CALL MSG_OUT( 'APER_OLDNOISE', BUFFER( :NC ),
     :                             STATUS )

*                   Noise after binning...

                     NC = 0
                     CALL CHR_PUTC( 'Error in the mean value          '/
     :                              /'    = ', BUFFER, NC )

*                   Assign value to token depending on whether or not
*                   value is valid

                     IF ( NWNOIS .NE. VAL__BADR ) THEN
                        CALL CHR_PUTR( NWNOIS, BUFFER, NC )
                     ELSE
                        CALL CHR_PUTC( 'INVALID', BUFFER, NC )
                     END IF
                     IF ( LOGFIL )
     :                  CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
                     CALL MSG_OUT( 'APER_NEWNOISE', BUFFER( :NC ),
     :                             STATUS )
                  END IF

*                See whether or not another aperture is required

                  CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS )
                  CALL PAR_CANCL( 'AGAIN', STATUS )
               ELSE

                  IF ( STATUS .NE. PAR__ABORT ) THEN

*                   just announce the error

                     CALL ERR_REP( 'ERR_APERADD_PAR',
     :                 'APERADD : Error obtaining input parameters - '/
     :                 /'try again', STATUS )
                     CALL ERR_FLUSH( STATUS )
                  ELSE

*                   abort loop

                     AGAIN = .FALSE.
                  END IF

*             end of if-no-error-before-calling-subroutine check

               END IF

*             Store the results of the calculations in the parameter
*             system for use by other applications.

               CALL PAR_PUT0I( 'NUMPIX', NUMPIX, STATUS )
               CALL PAR_PUT0R( 'TOTAL', TOTAL, STATUS )
               CALL PAR_PUT0R( 'MEAN', MEAN, STATUS )
               CALL PAR_PUT0R( 'SIGMA', OLNOIS, STATUS )
               CALL PAR_PUT0R( 'NOISE', NWNOIS, STATUS )

*             Cancel the association with input parameters for a further
*             loop, otherwise retain them so they can be stored in the
*             parameter file.

               IF ( AGAIN ) THEN
                  CALL PAR_CANCL( 'XCEN', STATUS )
                  CALL PAR_CANCL( 'YCEN', STATUS )
                  CALL PAR_CANCL( 'DIAM', STATUS )
               END IF

*          end of another-aperture loop

            END DO

*          unmap input data array

            CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

         ELSE

            CALL ERR_REP( 'ERR_APERADD_NOMPI',
     :        'APERADD : Error occurred whilst trying to map input '/
     :        /'frame', STATUS )

*       end of if-no-error-mapping-input-data check

         END IF

*       now tidy up input data

         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_APERADD_NOFRI',
     :        'APERADD : Error occurred whilst trying to access '/
     :        /'input frame', STATUS )
         END IF

*    end of if-no-error-after-getting-input check

      END IF

*    Close the logfile and release any identifiers used to access it.
      IF ( LOGFIL ) CALL FIO_ANNUL( FDL, STATUS )

 999  CONTINUE

*    end

      END

