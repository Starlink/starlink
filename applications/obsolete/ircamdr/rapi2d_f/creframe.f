
*+  CREFRAME - allows the user to generate several types of 2-d image

      SUBROUTINE CREFRAME( STATUS )

*    Description :
*
*     This routine allows the user to generate several different types
*     of image for test purposes. Random, fixed, Poisson, ramping type
*     images of any size are allowed. Simulated star fields are also
*     catered for, using the STARGEN software from the interim
*     Starlink environment.
*
*    Parameters :
*
*     OUTPIC   =  IMAGE( WRITE )
*           Output file for generated image
*     OTITLE   =  CHARACTER( READ )
*           Label for the output image
*     IDIMS    =  INTEGERARRAY( READ )
*           x and y dimensions of output array
*     TYPED    =  CHARACTER( READ )
*           Type of data to be generated
*     HIGH     =  REAL( READ )
*           High value used in generated data
*     LOW      =  REAL( READ )
*           Low value used in generated data
*     SIGMA    =  REAL( READ )
*           Sigma of noise used in generated data
*     MEAN     =  REAL( READ )
*           Mean value used in generated data
*     MAX      =  REAL( READ )
*           Peak stellar intensity to be used
*     MIN      =  REAL( READ )
*           Lowest stellar intensity to be used
*     SKY      =  REAL( READ )
*           Sky background intensity to be used
*     NSTARS   =  INTEGER( READ )
*           Number of stars to be generated
*     SCALE    =  REAL( READ )
*           Pixel scalesize on sky in arcseconds
*     SEEING   =  REAL( READ )
*           Seeing in arcseconds
*     DISTRIB  =  CHARACTER( READ )
*           Stellar radial distribution to be used
*     BADPIX   =  LOGICAL( READ )
*           Whether or not bad pixels are to be included
*     FRACTION =  REAL( READ )
*           Fraction of bad pixels to be included
*     BADCOL   =  LOGICAL( READ )
*           Whether or not a bad column is to be included
*     DISPLAY  =  LOGICAL( READ )
*           True if star parameters from MANYG are to be displayed
*     SCREEN   =  LOGICAL( READ )
*           True if star parameters displayed on screen - else in file
*     FILENAME =  CHARACTER( READ )
*           Filename for output of star parameters
*
*    Method :
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     29-07-1985 : First implementation, explicitly for testing
*                : of A-tasks. (REVA::MJM)
*     14-10-1985 : Added simulated starfield option (REVA::MJM)
*     09-01-1986 : Added blank option (again) (REVA::MJM)
*     14-01-1986 : Added option to display or store the star
*                : parameters if GS option chosen (REVA::MJM)
*     04-04-1987 : Added option of noisy data with a defined sigma
*                : about a defined mean (NM) (UKTH::MJM)
*     10-Mar-94    Changed DAT-, CMP_ calls to NDF_ (SKL@JACH)
*     15-AUG-1994 Changed input DIM arguments for MANYG, CREFRAMESUB (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE                ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'            ! global SSE constants
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS               ! global status parameter

*    Local Constants :

      INTEGER NDIMS                ! output image dimensionality
      PARAMETER ( NDIMS = 2 )      ! defaults to 2-d

*    Local variables :

      INTEGER
     :  LOCO,           ! output data structure
     :  IDIMS( NDIMS ), ! dimensions of input DATA_ARRAY
     :  NELEMENTS,      ! number of elements mapped
     :  PNTRO,          ! pointer to output DATA_ARRAY component
     :  NSTARS,         ! number of simulated stars generated
     :  DIRN            ! direction of ramping in data
                        ! directions : 0 = flat, 1 = L-R
                        ! 2 = R-L, 3 = B-T, 4 = T-B

      REAL
     :  LOW,            ! low value in data to be generated
     :  HIGH,           ! high value in data to be generated
     :  SIGMA,          ! sigma of noise to be generated
     :  MEAN,           ! mean value in data to be generated
     :  MAX,            ! peak stellar intensity to be used
     :  MIN,            ! low     "        "      "  "   "
     :  SKY,            ! sky background intensity
     :  SCALE,          ! pixel scalesize on sky in arcseconds
     :  SEEING,         ! seeing in arcseconds
     :  FRACTION        ! fraction of bad pixels to be included


      CHARACTER*2
     :  TYPED           ! type of data to be generated

      CHARACTER*3
     :  DISTRIB         ! stellar radial distribution -
                        ! FIX = Fixed distance
                        ! RSQ = one over radius squared

      CHARACTER*80
     :  FILENAME        ! filename to be used for output of
                        ! star parameters

      LOGICAL
     :  BADPIX,         ! true if bad pixels to be included
     :  BADCOL,         ! true if bad column  "  "     "
     :  DISPLAY,        ! true if star parameters to be displayed
     :  SCREEN          ! true if parameters displayed on screen
                        ! false if output wanted to file

*-
*    check status on entry (do not check status after this point -
*    have adopted method where status is checked on entry to each
*    subroutine, and if status is bad, there is an immediate
*    return, and the routine just ripples down to the bottom not
*    doing anything)

      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get size of array to be generated

      CALL AIF_GET0I( 'XDIM', 256, 1, 512, IDIMS( 1 ), STATUS )
      CALL AIF_GET0I( 'YDIM', 256, 1, 512, IDIMS( 2 ), STATUS )

      if( status .ne. sai__ok) then
        return
      end if

*    tell user of the types of input possible

      CALL MSG_OUT( 'MESSAGE', 'GS = Gaussian stars', STATUS)
      CALL MSG_OUT( 'MESSAGE', 'RR = Random 0-1', STATUS)
      CALL MSG_OUT( 'MESSAGE', 'RL = Random Min-Max,', STATUS)
      CALL MSG_OUT( 'MESSAGE', 'NM = Noise with Sigma about mean',
     :  STATUS)
      CALL MSG_OUT( 'MESSAGE', 'RP = Poisson noise about mean',
     :  STATUS)
      CALL MSG_OUT( 'MESSAGE', 'RA = Ramp across image', STATUS)
      CALL MSG_OUT( 'MESSAGE', 'FL = Flat', STATUS)
      CALL MSG_OUT( 'MESSAGE', 'BL = Blank', STATUS)

*    get type of data to be generated

      CALL AIF_CHOIC( 'TYPED', 'GS,RR,RL,NM,RP,RA,FL,BL', TYPED,
     :                 STATUS )

*    force input string to upper case

      CALL UPCASE( TYPED, TYPED, STATUS )

*    now assign variables for subroutine call according to the
*    choice of data requested.

      IF( TYPED .EQ. 'GS' ) THEN
                                            ! Gaussian stars
*       get number of stars to be generated
         CALL AIF_GET0I( 'NSTARS', 10, 1, 1000, NSTARS, STATUS )

*       get maximum allowable intensity for stars
         CALL AIF_GET0R( 'MAX', 100.0, 0.1, 1.0E20, MAX, STATUS )

*       get minimum allowable intensity for stars
         CALL AIF_GET0R( 'MIN', 0.0, 0.0, 1.0E20, MIN, STATUS )

*       get sky background intensity
         CALL AIF_GET0R( 'SKY', 10.0, 0.0, 1.0E20, SKY, STATUS )

*       get pixel scalesize on sky in arcseconds
         CALL AIF_GET0R( 'SCALE', 0.286, 0.01, 10.0, SCALE, STATUS )

*       get seeing in arcseconds
         CALL AIF_GET0R( 'SEEING', 0.5, 0.01, 10.0, SEEING, STATUS )

*       get type of stellar distribution - fixed distance or 1/r2
         CALL AIF_CHOIC( 'DISTRIB', 'RSQ,FIX', DISTRIB, STATUS )

*       find out whether bad pixels are to be included
         CALL PAR_GET0L( 'BADPIX', BADPIX, STATUS )

*       if so, then get the fraction to be set bad
         IF( BADPIX ) THEN
            CALL AIF_GET0R( 'FRACTION', 0.01, 0.0, 1.0,
     :                       FRACTION, STATUS )
         END IF

*       find whether a bad column is to be included
         CALL PAR_GET0L( 'BADCOL', BADCOL, STATUS )

*       find out whether or not the star parameters generated are
*       to be output
         CALL PAR_GET0L( 'DISPLAY', DISPLAY, STATUS )

*       if so, then find out whether the display is wanted on the
*       screen, or to a file
         IF ( DISPLAY ) THEN
            CALL PAR_GET0L( 'SCREEN', SCREEN, STATUS )

*          if not to the screen, then get a file name to be used
            IF ( .NOT. SCREEN ) THEN
               CALL PAR_GET0C( 'FILENAME', FILENAME, STATUS )
            END IF

         END IF


      ELSE IF( TYPED .EQ. 'RR' ) THEN
                                            ! random between 0 and 1
         HIGH  =  1.0
         LOW   =  0.0
         MEAN  =  0.5
         SIGMA =  0.0
         DIRN  =  0


      ELSE IF( TYPED .EQ. 'RL' ) THEN
                                        ! random between set limits
         CALL PAR_GET0R( 'LOW', LOW, STATUS )
         CALL PAR_GET0R( 'HIGH', HIGH, STATUS )

         MEAN  =  ( HIGH + LOW ) / 2
         SIGMA =  0.0
         DIRN  =  0


      ELSE IF( TYPED .EQ. 'NM' ) THEN
                                        ! noise with defined sigma about a
                                        ! defined mean
         CALL PAR_GET0R( 'MEAN', MEAN, STATUS )
         CALL PAR_GET0R( 'SIGMA', SIGMA, STATUS )

         HIGH  =  0.0
         LOW   =  0.0
         DIRN  =  0

      ELSE IF( TYPED .EQ. 'RP' ) THEN
                                        ! Poisson noise about mean
         CALL PAR_GET0R( 'MEAN', MEAN, STATUS )

         SIGMA =  0.0
         HIGH  =  0.0
         LOW   =  0.0
         DIRN  =  0


      ELSE IF( TYPED .EQ. 'RA' ) THEN
                                        ! ramp across image
         CALL PAR_GET0R( 'LOW', LOW, STATUS )
         CALL PAR_GET0R( 'HIGH', HIGH, STATUS )
         CALL AIF_GET0I( 'DIRN', 1, 1, 4, DIRN, STATUS )

         MEAN  =  ( HIGH + LOW ) / 2
         SIGMA =  0.0


      ELSE IF( TYPED .EQ. 'FL' ) THEN
                                        ! flat all over image
         CALL PAR_GET0R( 'MEAN', MEAN, STATUS )

         SIGMA =  0.0
         HIGH  =  MEAN
         LOW   =  MEAN
         DIRN  =  0

      ELSE IF( TYPED .EQ. 'BL' ) THEN
                                        ! zero all over image
         SIGMA =  0.0
         HIGH  =  0.0
         LOW   =  0.0
         DIRN  =  0

      END IF

      IF( STATUS .NE. SAI__OK) THEN
        RETURN
      END IF

*    now create output IMAGE type data structure with DATA_ARRAY
*    component; also create and get value for DATA_LABEL component

      CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS, LOCO, STATUS )

      IF( STATUS .NE. SAI__OK) THEN
        RETURN
      END IF

*    map output DATA_ARRAY component

      CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :               PNTRO, NELEMENTS, STATUS )


*    call actual subroutines to do the work - use CREFRAMESUB for
*    the simple data types and MANYG for stars

      IF( TYPED .EQ. 'GS' ) THEN

         CALL MANYG( IDIMS(1), IDIMS(2), %VAL( PNTRO ), MAX, MIN,
     :               SKY, NSTARS, SCALE, SEEING, DISTRIB, BADPIX,
     :               FRACTION, BADCOL, DISPLAY, SCREEN, FILENAME,
     :               STATUS )

      ELSE

         CALL CREFRAMESUB( IDIMS(1), IDIMS(2), %VAL( PNTRO ), TYPED,
     :                     MEAN, SIGMA, HIGH, LOW, DIRN, STATUS )

      ENDIF


*    tidy up the output data structure

      CALL NDF_ANNUL( LOCO, STATUS )

*    end

      END
