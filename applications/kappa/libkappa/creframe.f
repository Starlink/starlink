*+  CREFRAME - Generates a test 2-d data array from a selection of
*              several types

      SUBROUTINE CREFRAME( STATUS )
*
*    Description :
*
*     This routine allows you to generate several different types of
*     2-d data array for test purposes.  The data array is written to an
*     output IMAGE structure.  The types of array are summarised as
*     follows:
*
*     [Random]   - between 0 and 1, or specified limits
*     [Constant] - 0 or at a specified value
*     [Noisy]    - Poissonian or Gaussian noise about a specified mean
*     [Ramped]   - between specified minimum and maximum values and a
*                  choice of four directions
*     [Gaussian] - a random distribution of 2-d Gaussians of defined
*                  FWHM and range of maximum peak values on a specified
*                  background, with optional invalid pixels and bad
*                  column. There is a choice of distributions for the
*                  Gaussians: fixed, or inverse square radially from the
*                  array centre. (In essence it is equivalent to a
*                  simulated star field.) The x-y position and peak
*                  value of each Gaussian may be stored in a Fortran
*                  formatted file, or reported to you.  Magic-value
*                  bad data may be included randomly, and/or in a column
*                  or line of the array.
*
*     The maximum size of generated array is 4096-by-4096 pixels,
*     though generally test data should be much smaller.
*
*    Invocation :
*
*     CALL CREFRAME( STATUS )
*
*    Parameters :
*
*     OUTPIC   =  IMAGE( WRITE )
*         Output IMAGE structure for the generated data array
*     OTITLE   =  CHAR( READ )
*         Title for the output IMAGE structure
*     IDIMS    =  INTEGER( READ )
*         x and y dimensions of the output data array
*     TYPED    =  CHAR( READ )
*         Type of data to be generated. The options are GS - Gaussian;
*           RR - random 0 -- 1; RP - random Poisson noise about mean;
*           RL - random with set limits; FL - flat; BL - zeroes;
*           RA - ramps; and GN - Gaussian noise about mean.
*     HIGH     =  REAL( READ )
*         High value used in the generated data array (RA and RL types)
*     LOW      =  REAL( READ )
*         Low value used in the generated data array (RA and RL types)
*     DIRN     =  INTEGER( READ )
*         Direction of the ramp. 1 means left to right, 2 is right to
*           left, 3 is bottom to top, and 4 is top to bottom. (RA type)
*     MEAN     =  REAL( READ )
*         Mean value used in the generated data array (FL and RP types)
*     SIGMA    =  REAL( READ )
*         Standard deviation of noise to be used in the generated data
*           array (GN type)
*     MAX      =  REAL( READ )
*         Peak Gaussian intensity to be used in the generated data array
*           (GS type)
*     MIN      =  REAL( READ )
*         Lowest Gaussian intensity to be used in the generated data
*           array  (GS type)
*     BACKGROUND  =  REAL( READ )
*         Background intensity to be used in the generated data array
*           (GS type)
*     NGAUSS   =  INTEGER( READ )
*         Number of Gaussian star-like images to be generated (GS type)
*     SEEING   =  REAL( READ )
*         Seeing (FWHM) in pixels (not the same as the standard
*           deviation) (GS type)
*     DISTRIB  =  CHAR( READ )
*         Radial distribution of the Gaussians to be used; alternatives
*           weightings are FIX = fixed distance; and RSQ = one over
*           radius squared. (GS type)
*     BADPIX   =  LOGICAL( READ )
*         Whether or not bad pixels are to be included (GS type)
*     FRACTION =  REAL( READ )
*         Fraction of bad pixels to be included (GS type)
*     BADCOL   =  LOGICAL( READ )
*         Whether or not a bad column is to be included (GS type)
*     SCREEN   =  LOGICAL( READ )
*         True if the Gaussian parameters are reported to you (GS type)
*     FILENAME =  CHAR( READ )
*         Filename for the output of the Gaussian parameters (GS type)
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get size and type of array to be generated
*     If there is no error then
*        Get or set parameters required to generate the specified array
*        If there is no error then
*           Create output IMAGE structure
*           If there is no error then
*              Map output data array onto a pointer
*              If there is no error then
*                 If Gaussian field required then
*                    Call MANYG routine
*                 Else
*                    Call multi-purpose data generating routine
*                 Endif
*                 Unmap output data array
*              Else
*                 Report error context
*              Endif
*              Tidy output data structure
*           Else
*              Report error context
*           Endif
*        Else
*           Report error context
*        Endif
*     Else
*        Report error context
*     Endif
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     29-07-1985 : First implementation, explicitly for testing
*                : of A-tasks. (REVA::MJM)
*     14-10-1985 : Added simulated starfield option (REVA::MJM)
*     09-01-1986 : Added blank option (again) (REVA::MJM)
*     14-01-1986 : Added option to display or store the star
*                : parameters if GS option chosen (REVA::MJM)
*     1986 Aug 5 : Renamed algorithm subroutine (CRFRSB), correctly
*                  ordered arguments in MANYG (2nd to 7th) and in
*                  CRFRSB (2nd to penultimate). Added list of options
*                  to the prologue (RL.STAR::CUR).
*     1986 Aug 28: Completed the prologue and nearly conformed to
*                  Starlink programming standards (RL.STAR::CUR).
*     1987 Oct 13: Modified status check of output array mapping
*                  (RL.STAR::CUR)
*     1988 Mar 17: Referred to `array' rather than `image'
*                  (RL.STAR::CUR)
*     1988 May 23: Added Gaussian noise option (RL.STAR::CUR).
*     1988 Jun 7 : More error reporting (RL.STAR::CUR).
*     1988 Jun 30: File name obtained in MANYG (RL.STAR::CUR).
*     1988 Aug 5 : Removed lingering astronomical references and
*                  SCALE parameter (RL.STAR::CUR).
*     1989 Jul 25: Altered MANYG argument list; removed DISPLAY
*                  parameter and reordered alternatives of DISTRIB
*                  parameter (RL.STAR::CUR)
*     1989 Aug  7: Passed array dimensions as separate variables
*                  to CRFRSB (RL.STAR::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter-system errors

*    Status :

      INTEGER STATUS

*    Local Constants :

      INTEGER NDIMS            ! output array dimensionality
      PARAMETER ( NDIMS = 2 )  ! defaults to 2-d

*    Local variables :

      INTEGER 
     :  IDIMS( NDIMS ),        ! dimensions of input DATA_ARRAY
     :  PNTRO,                 ! pointer to output DATA_ARRAY component
     :  NGAUSS,                ! number of Gaussians (simulated stars)
                               ! generated
     :  DIRN                   ! direction of ramping in data
                               ! directions : 0 = flat, 1 = L-R
                               ! 2 = R-L, 3 = B-T, 4 = T-B

      REAL
     :  LOW,                   ! low value in data to be generated
     :  HIGH,                  ! high value in data to be generated
     :  MEAN,                  ! mean value in data to be generated
     :  SIGMA,                 ! standard deviation in data to be
                               ! generated
     :  MAX,                   ! peak Gaussian intensity to be used
     :  MIN,                   ! low     "        "      "  "   "
     :  BCKGRD,                ! background intensity
     :  SEEING,                ! seeing in pixels
     :  FRACTN                 ! fraction of bad pixels to be included


      CHARACTER*(DAT__SZLOC)   ! locator for :
     :  LOCO                   ! output data structure

      CHARACTER*2
     :  TYPED                  ! type of data to be generated

      CHARACTER*3
     :  DISTRB                 ! radial distribution of Gaussians
                               ! FIX = Fixed distance
                               ! RSQ = one over radius squared 

      LOGICAL                  ! true if:
     :  BADPIX,                ! bad pixels to be included
     :  BADCOL,                ! bad column "  "     "
     :  SCREEN                 ! parameters are reported to the user

*-
*    check status on entry (do not check status after this point -
*    have adopted method where status is checked on entry to each
*    subroutine, and if status is bad, there is an immediate
*    return, and the routine just ripples down to the bottom not
*    doing anything)

      IF ( STATUS .NE. SAI__OK ) RETURN

*    get size of array to be generated 

      CALL PAR_GDR0I( 'XDIM', 64, 1, 4096, .FALSE., IDIMS( 1 ), STATUS )
      CALL PAR_GDR0I( 'YDIM', 64, 1, 4096, .FALSE., IDIMS( 2 ), STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*       describe options available (insufficient room in interface help)

         CALL MSG_OUT( 'CREFRAME_OPTIONS',  'GS = Gaussians, RR ='/
     :                 /' Random 0 to 1, RL = Random from Min to Max, ',
     :                 STATUS )
         CALL MSG_OUT( 'CREFRAME_OPTIONS', 'RA = Ramp across image, '/
     :                 /'FL = Flat, BL = Blank,', STATUS )
         CALL MSG_OUT( 'CREFRAME_OPTIONS', 'GN = Gaussian noise with '/
     :                 /'standard deviation about the mean,', STATUS )
         CALL MSG_OUT( 'CREFRAME_OPTIONS', 'RP = '/
     :                 /'Poissonian noise about the mean.', STATUS )

*       get type of data to be generated

         CALL PAR_CHOIC( 'TYPED', 'GS', 'GS,RR,RL,RP,RA,FL,BL,GN',
     :                   .TRUE., TYPED, STATUS )

*       force input string to upper case

         CALL CHR_UCASE( TYPED )

*       now assign variables for subroutine call according to the
*       choice of data requested.
*
*       Gaussian `stars'

         IF ( TYPED .EQ. 'GS' .AND. STATUS .EQ. SAI__OK ) THEN

*          get number of Gaussians to be generated

            CALL PAR_GDR0I( 'NGAUSS', 10, 1, 1000, .TRUE., NGAUSS,
     :                      STATUS )

*          get maximum allowable intensity for Gaussians

            CALL PAR_GDR0R( 'MAX', 100.0, 0.1, 1.0E20, .TRUE., MAX,
     :                      STATUS )

*          get minimum allowable intensity for Gaussians

            CALL PAR_GDR0R( 'MIN', 0.0, 0.0, 1.0E20, .TRUE., MIN,
     :                      STATUS )

*          get background intensity

            CALL PAR_GDR0R( 'BACKGROUND', 10.0, 0.0, 1.0E20, .TRUE.,
     :                      BCKGRD, STATUS )

*          get seeing in pixels

            CALL PAR_GDR0R( 'SEEING', 1.0, 0.01, 100.0, .TRUE., SEEING,
     :                      STATUS )

*          get type of distribution of Gaussians - fixed distance or
*          inverse square

            CALL PAR_CHOIC( 'DISTRIB', 'FIX', 'FIX,RSQ', .TRUE., DISTRB,
     :                      STATUS )

*          find out whether bad pixels are to be included

            CALL PAR_GTD0L( 'BADPIX', .FALSE., .TRUE., BADPIX, STATUS )

*          if so, then get the fraction to be set bad

            IF ( BADPIX ) THEN
               CALL PAR_GDR0R( 'FRACTION', 0.01, 0.0, 1.0, .TRUE.,
     :                         FRACTN, STATUS )
            END IF

*          find whether a bad column is to be included

            CALL PAR_GTD0L( 'BADCOL', .FALSE., .TRUE., BADCOL, STATUS )

*          find out whether the display is wanted on the screen

            CALL PAR_GTD0L( 'SCREEN', .FALSE., .TRUE., SCREEN, STATUS )

*       random between 0 and 1

         ELSE IF ( TYPED .EQ. 'RR' .AND. STATUS .EQ. SAI__OK ) THEN

            HIGH  =  1.0
            LOW   =  0.0
            MEAN  =  0.5
            SIGMA =  0.0
            DIRN  =  0

*       random between set limits

         ELSE IF ( TYPED .EQ. 'RL' .AND. STATUS .EQ. SAI__OK ) THEN
 
            CALL PAR_GET0R( 'LOW', LOW, STATUS )
            CALL PAR_GET0R( 'HIGH', HIGH, STATUS )

            MEAN  =  ( HIGH + LOW ) / 2
            SIGMA =  0.0
            DIRN  =  0

*       Poissonian noise about mean

         ELSE IF ( TYPED .EQ. 'RP' .AND. STATUS .EQ. SAI__OK ) THEN

            CALL PAR_GET0R( 'MEAN', MEAN, STATUS )

            HIGH  =  0.0
            LOW   =  0.0
            DIRN  =  0
            SIGMA =  0.0

*       Gaussian noise about mean

         ELSE IF ( TYPED .EQ. 'GN' .AND. STATUS .EQ. SAI__OK ) THEN

            CALL PAR_GET0R( 'MEAN', MEAN, STATUS )
            CALL PAR_GET0R( 'SIGMA', SIGMA, STATUS )

            HIGH  =  0.0
            LOW   =  0.0
            DIRN  =  0

*       ramp across array

         ELSE IF ( TYPED .EQ. 'RA' .AND. STATUS .EQ. SAI__OK ) THEN

            CALL PAR_GET0R( 'LOW', LOW, STATUS )
            CALL PAR_GET0R( 'HIGH', HIGH, STATUS )
            CALL PAR_GDR0I( 'DIRN', 1, 1, 4, .FALSE., DIRN, STATUS )

            MEAN  =  ( HIGH + LOW ) / 2

*       flat all over array

         ELSE IF ( TYPED .EQ. 'FL' .AND. STATUS .EQ. SAI__OK ) THEN

            CALL PAR_GET0R( 'MEAN', MEAN, STATUS )

            HIGH  =  MEAN
            LOW   =  MEAN
            SIGMA =  0.0
            DIRN  =  0

*       zero all over array

         ELSE IF ( TYPED .EQ. 'BL' .AND. STATUS .EQ. SAI__OK ) THEN

            HIGH  =  0.0
            LOW   =  0.0
            SIGMA =  0.0
            DIRN  =  0

         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN

*          now create output IMAGE type data structure with DATA_ARRAY 
*          component; also create and get value for DATA_LABEL component

            CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS, LOCO,
     :                   STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*             map output DATA_ARRAY component

               CALL CMP_MAPN( LOCO, 'DATA_ARRAY', '_REAL', 'WRITE',
     :                        NDIMS, PNTRO, IDIMS, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*                call actual subroutines to do the work - use CRFRSB for
*                the simple data types and MANYG for Gaussians

                  IF ( TYPED .EQ. 'GS' ) THEN

                     CALL MANYG( IDIMS( 1 ), IDIMS( 2 ), MAX, MIN,
     :                           BCKGRD, NGAUSS, SEEING, DISTRB, BADPIX,
     :                           FRACTN, BADCOL, SCREEN, 'FILENAME',
     :                           %VAL( PNTRO ), STATUS )

                  ELSE

                     CALL CRFRSB( IDIMS( 1 ), IDIMS( 2 ), TYPED, MEAN,
     :                            HIGH, LOW, DIRN, SIGMA,
     :                            %VAL( PNTRO ), STATUS )

                  END IF

*                unmap output data array

                  CALL CMP_UNMAP( LOCO, 'DATA_ARRAY', STATUS )

               ELSE
                  CALL ERR_REP( 'ERR_CREFRAME_NOMPO',
     :              'CREFRAME: Error occurred whilst trying to map '/
     :              /'output frame', STATUS )

*             end of if-no-error-after-mapping-output-data-array check

               END IF

*             tidy up the output data structure

               CALL DAT_ANNUL( LOCO, STATUS )

            ELSE

               IF ( STATUS .NE. PAR__ABORT ) THEN
                  CALL ERR_REP( 'ERR_CREFRAME_NOFRO',
     :              'CREFRAME: Error occurred whilst trying to '/
     :              /'access output frame', STATUS )
               END IF

*          end of if-no-error-after-creating-output-structure check

            END IF

         ELSE

            IF ( STATUS .NE. PAR__ABORT .AND.
     :           STATUS .NE. PAR__NULL ) THEN

*             announce the error

               CALL ERR_REP( 'ERR_CREFRAME_PAR',
     :           'CREFRAME: Error obtaining parameters - aborting',
     :           STATUS )
            END IF

*       end of no-error-getting-parameters check

        END IF

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_CREFRAME_ARSE',
     :        'CREFRAME: Error getting size of output array',
     :        STATUS )
         END IF

*    end of no-error-getting-output-array-dimensions check

      END IF

*    end

      END
