      SUBROUTINE POL_CALE( NEL, NSET, NPOS, NPAIR, IPDIN, IPVIN,
     :                     NSTATE, VAR, TOLS, TOLZ, MAXIT, SKYSUP,
     :                     ID, IMGID, F, ETOL, WEIGHT,  IPDOU,
     :                     IPVOU, EEST, ZEST, VE, VZ, DE, TI1, TI2,
     :                     STATUS )
*+
* Name:
*     POL_CALE

*  Purpose:
*     To calculate time dependent (exposure) scale factors for dual beam
*     linear and circular polarimetry images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL_CALE( NEL, NSET, NPOS, NPAIR, IPDIN, IPVIN, NSTATE, VAR,
*                    TOLS, TOLZ,  MAXIT, SKYSUP, ID, IMGID, F,
*                    ETOL, WEIGHT, IPDOU, IPVOU, EEST, ZEST, VE, VZ, DE,
*                    TI1, TI2, STATUS )

*  Description:
*     This routine calculates the scale factors (E factors) that relate
*     different exposures taken with a dual beam imaging polarimeter to
*     a common intensity scale. These scale factors will incorporate
*     changes due to effects such a exposure time variations between
*     images, but should be independent of polarisation state.
*
*     The routine first forms a series of total intensity images by
*     adding together input images with orthogonal polarisation states,
*     after correcting for the instrumental polarisation efficiency. A
*     median total intensity image is then formed against which the
*     component total instensity images are compared, using an iterative
*     least-squares technique. The scale factors resulting from these
*     intercomparisons are used to correct the component total intensity
*     images and a new median image is formed and the process repeats.
*     The median image is therefore iteratively refined with the
*     iteration continuing until the scale factors (E factors) converge
*     to within a required tolerance, or the maximum number of
*     iterations is exceeded.

*  Arguments:
*     NEL = INTEGER (Given)
*        Number of image elements.
*     NSET = INTEGER (Given)
*        Number of sorted polarisation data sets. There will be a
*        maximum of 8 images in each set.
*     NPOS = INTEGER (Given)
*        Number of waveplate positions. 4 for linear and 2 for circular.
*     NPAIR = INTEGER (Given)
*        The number of polarimetric pairs.
*     IPDIN( 8, NSET ) =  (Given)
*        An array of memory pointers to reference the mapped
*        polarisation data, sorted according to waveplate position and
*        polarisation set.
*     IPVIN( 8, NSET ) = INTEGER (Given)
*        {An array of memory pointers to reference the mapped
*        polarisation data VARIANCES, sorted according to waveplate
*        position and polarisation set.
*     NSTATE( NPOS ) = INTEGER (Given)
*        The number of images recorded at each waveplate position.
*     VAR = LOGICAL (Given)
*        If true then variance calculations will be performed.
*     TOLS = REAL (Given)
*        Tolerance required on the scale factor when performing image
*        intercomparisons.
*     TOLZ = REAL (Given)
*        Tolerance required on the zero shift when performing image
*        intercomparisons.
*     MAXIT = INTEGER (Given)
*        Maximum number of image intercomparison iterations.
*     SKYSUP = REAL (Given)
*        Sky level suppression factor to use when performing image
*        intercomparisons. Also used here for the E factor refinement.
*     ID( NPAIR ) = CHARACTER * ( * ) (Given)
*        Image Identifiers.
*     IMGID( 4, NSET ) = CHARACTER * ( * ) (Given)
*        Image identifiers.
*     F = REAL (Given)
*        F factor
*     ETOL = REAL (Given)
*        Convergence criterion for E factors.
*     WEIGHT( NPAIR ) = REAL (Given)
*        Weights used when combining the total intensity images.
*     IPDOU( 8, NSET ) = INTEGER (Returned)
*        Pointers to the output data, corrected for E and F factors.
*     IPVOU( 8, NSET ) = INTEGER (Returned)
*        Pointers to variances on the output data.
*     EEST( NPAIR ) = REAL (Returned)
*        E factor estimates for each pair.
*     ZEST( NPAIR ) = REAL (Returned)
*        Zero level estimates for each pair.
*     VE( NPAIR ) = REAL (Returned)
*        Variances on the E factor estimates.
*     VZ( NPAIR ) = REAL (Returned)
*        Variances on the zero shifts.
*     DE( NPAIR ) = REAL (Given)
*        Changes in E factor estimates for each pair.
*     TI1( NEL, NPAIR ) = REAL (Returned)
*        Total intensity images.
*     TI2( NEL, NPAIR ) = REAL (Returned)
*        Total intensity images (corrected).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     TMG: Tim Gledhill (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-SEP-1997 (TMG):
*        Original version.
*     16-JAN-1998 (DSB):
*        Declaration of RE, SENSL, SENSR, SENS2L and SENS2R changed from REAL
*        to DOUBLE PRECISION.
*     11-MAY-1998 (DSB):
*        Introduced lagging into the iterative estimation of the E factors
*        to suppress instability in the process. Also, corrected the
*        order of the arguments to CCD1_CMPRR (previously the data arrays were
*        passed the other way round, resulting in the calculated E factors
*        being the reciprocal of the correct values).
*     02-JUN-1998 (TMG):
*        Correctly dimension IPDIN, IPVIN, IMGID, IPVOUT, IPDOUT. Add extra
*        passed array ID.
*     4-JUN-1998 (DSB):
*        Removed 10 character restriction on image identifiers. Swapped
*        order of arguments ID and IMGID to use of mapped dyanmic memory
*        for ID.
*     24-JUN-1998 (DSB):
*        Continue processing if the image inter-comparisons fail to converge.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     13-JUL-2009 (DSB):
*        Make WEIGHT argument REAL, not DOUBLE PRECISION. Use kaplibs
*        CCG_MD3R instead of CCDPACK CCG1_MDR3R.
*     31-JUL-2009 (TIMJ):
*        Remove ILEVEL. Use MSG filtering.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Global Variables:
*      {include_global_variables}...

*  Arguments Given:
      INTEGER NEL
      INTEGER NSET
      INTEGER NPOS
      INTEGER NPAIR
      INTEGER NSTATE( NPOS )
      LOGICAL VAR
      REAL TOLS
      REAL TOLZ
      REAL SKYSUP
      INTEGER MAXIT
      REAL F
      REAL ETOL
      INTEGER IPDIN( 8, NSET )
      INTEGER IPVIN( 8, NSET )
      CHARACTER * ( * ) ID( NPAIR )
      CHARACTER * ( * ) IMGID( 4, NSET )
      REAL WEIGHT( NPAIR )

*  Arguments Given and Returned:
      REAL EEST( NPAIR )
      REAL ZEST( NPAIR )
      REAL VE( NPAIR )
      REAL VZ( NPAIR )
      REAL DE( NPAIR )
      REAL TI1( NEL, NPAIR )
      REAL TI2( NEL, NPAIR )

*  Arguments Returned:
      INTEGER IPDOU( 8, NSET )
      INTEGER IPVOU( 8, NSET )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL LAG                   ! The E-factor lagging coefficient
      PARAMETER ( LAG = 0.5 )

*  Local Variables:
      INTEGER NWRK1, NWRK2, NWRK3, NWRK4
                                 ! workspace dimensions
      INTEGER IPWRK1, IPWRK2, IPWRK3, IPWRK4, IPWRK5, IPWRK6, IPWRK7,
     :        IPWRK8, IPWRK9     ! memory pointers to workspace
      INTEGER IPMED              ! pointer to median image
      INTEGER IPAIR, IPOS, ISET  ! loop counters
      INTEGER NBAD               ! Number of bad pixels created
      INTEGER NTOT               ! total pixel count
      INTEGER IERR, NERR         ! PRM error information
      INTEGER ITER               ! iteration count in convergence loop
      INTEGER NITER, NPTS        ! number of iterations and points used
                                 ! in image intercomparisons
      REAL E                     ! local estimate of E
      REAL MAXDE                 ! max change in E
      REAL EMED, ZMED            ! median scale factor and zero shift

      LOGICAL GETS, GETZ, BAD    ! logical flags
      LOGICAL CONVERGED          ! convergence flag
      LOGICAL FLUSHING           ! If we are in verbose mode and flushing

      DOUBLE PRECISION SENSL, SENSR, SENS2L, SENS2R ! sensitivity corrections
      DOUBLE PRECISION RE        ! local estimate of 1/E
      DOUBLE PRECISION SCALE, DSCALE ! scale factor and standard error
      DOUBLE PRECISION ZERO, DZERO   ! zero shift and standard error
      DOUBLE PRECISION ORIGIN, DS, DZ ! false origin and changes in
                                 ! scale and zero on final iteration.

      CHARACTER * ( 80 ) STRING  ! output information buffer
      REAL EOLD
*.

* Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) GO TO 99

* Determine whether errors are flushed
      FLUSHING = MSG_FLEVOK( MSG__VERB, STATUS )

* Assume BAD pixel values are present in all images.

      BAD = .TRUE.

* Calculate both scale factors and zero offsets during image
* intercomaprison.

      GETS = .TRUE.
      GETZ = .TRUE.

* Size the workspace arrays for image intercomparison as necessary. See
* CCD1_CMPRx for details.

      NWRK1 = 1
      NWRK2 = 1
      NWRK3 = 1
      NWRK4 = 1
      IF ( GETS .AND. GETZ ) THEN
         NWRK1 = 3 * NEL
         NWRK2 = NEL
      ELSE IF ( GETS .OR. GETZ ) THEN
         NWRK1 = NEL
         NWRK2 = NEL
      ENDIF
      IF ( VAR .AND. GETS .AND. ( SKYSUP .GT. 0.0 ) ) THEN
         NWRK3 = 2 * NEL
         NWRK4 = NEL
      ELSE IF ( VAR .OR. ( GETS .AND. ( SKYSUP .GT. 0.0 ) ) ) THEN
         NWRK3 = NEL
      ENDIF

* Allocate workspace for intercomparisons.

      CALL PSX_CALLOC( NWRK1, '_INTEGER', IPWRK1, STATUS )
      CALL PSX_CALLOC( NWRK2, '_REAL', IPWRK2, STATUS )
      CALL PSX_CALLOC( NWRK3, '_REAL', IPWRK3, STATUS )
      CALL PSX_CALLOC( NWRK4, '_REAL', IPWRK4, STATUS )

* Allocate workspace for forming the median image.

      CALL PSX_CALLOC( NEL, '_REAL', IPMED, STATUS )
      CALL PSX_CALLOC( NPAIR, '_REAL', IPWRK5, STATUS )
      CALL PSX_CALLOC( NPAIR, '_REAL', IPWRK6, STATUS )
      CALL PSX_CALLOC( NPAIR, '_DOUBLE', IPWRK7, STATUS )
      CALL PSX_CALLOC( NPAIR, '_INTEGER', IPWRK8, STATUS )
      CALL PSX_CALLOC( NPAIR, '_LOGICAL', IPWRK9, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

* Loop through the image pairs that can be used to form total intensity
* images.

      IPAIR = 0
      DO ISET = 1, NSET
         DO IPOS = 1, NPOS
            IF ( NSTATE( IPOS ) .GE. ISET ) THEN
               IPAIR = IPAIR + 1

* Correct the right hand image with the polarisation sensitivity factor.
* Add the left and corrected right hand images to form a total intensity
* image. Index the result into an array of total intensity images.

               CALL POL_CALTI( NEL,
     :   %VAL( CNF_PVAL( IPDIN( 2 * IPOS - 1, ISET ) ) ),
     :
     :   %VAL( CNF_PVAL( IPDIN( 2 * IPOS, ISET ) ) ), F,
     :                        TI1( 1, IPAIR ), STATUS )
               ID( IPAIR ) = IMGID( IPOS, ISET )
            ENDIF
         ENDDO
      ENDDO

* Set up unit weightings and initialise the scale factor and zero level
* estimates.

      DO IPAIR = 1, NPAIR
         WEIGHT( IPAIR ) = 1.0
         EEST( IPAIR ) = 1.0
         ZEST( IPAIR ) = 0.0
      ENDDO

* Copy the total intensity images into another workspace array. This
* allows us to maintain both corrected and uncorrected images.

      NTOT = NEL * NPAIR
      CALL VEC_RTOR( BAD, NTOT, TI1, TI2, IERR, NERR, STATUS )

* Enter a loop to calculate E factor estimates for each image pair by
* comparing then against an iteratively refined median total intensity
* image. Loop until a convergence criterion is met.

      CONVERGED = .FALSE.
      ITER = 0
      DO WHILE ( .NOT. CONVERGED )

* Form a median image of the total intensity images. Use unit weighting.
* Insist on at least two pixels being valid so that different images are
* intercompared.

         CALL CCG_MD3R( NEL, NPAIR, TI2, WEIGHT, 2,
     :        %VAL( CNF_PVAL( IPMED ) ),
     :        %VAL( CNF_PVAL( IPWRK5 ) ), %VAL( CNF_PVAL( IPWRK6 ) ),
     :        %VAL( CNF_PVAL( IPWRK8 ) ), %VAL( CNF_PVAL( IPWRK9 ) ),
     :        %VAL( CNF_PVAL( IPWRK7 ) ), NBAD, STATUS )

* Loop through the intensity images to compare them with the median and
* form estimates of the scale factors and zero shifts. Variance
* information is not used here.

         DO IPAIR = 1, NPAIR
            NITER = 0
            CALL CCD1_CMPRR( BAD, .FALSE., NEL,
     :                       %VAL( CNF_PVAL( IPMED ) ),
     :                       %VAL( CNF_PVAL( IPMED ) ), TI1( 1, IPAIR ),
     :                       TI1( 1, IPAIR ),
     :                       GETS, GETZ, TOLS, TOLZ, MAXIT, SKYSUP,
     :                       SCALE, DSCALE, ZERO, DZERO, ORIGIN, NPTS,
     :                       NITER, DS, DZ, %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       %VAL( CNF_PVAL( IPWRK3 ) ),
     :                       %VAL( CNF_PVAL( IPWRK4 ) ), STATUS )

*  SAI__ERROR report will be made by CCD1_CMPRR. In this case, the
*  resulting approximate solution will probably be OK, so just annul (or
*  flush if verbose) the error and carry on.
            IF( STATUS .EQ. SAI__ERROR .AND. NITER .EQ. MAXIT ) THEN
               IF( FLUSHING ) THEN
                  CALL ERR_REP( ' ', 'The above error probably '//
     :                       'does not matter and so is being '//
     :                       'ignored... The E factors may only '//
     :                       'be approximate!', STATUS )
                  CALL ERR_FLUSH( STATUS )
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
            END IF

*  Store the values returned by CCD1_CMPRR.
            E = SNGL( SCALE )
            VE( IPAIR ) = SNGL( DSCALE ) ** 2
            ZEST( IPAIR ) = SNGL( ZERO )
            VZ( IPAIR ) = SNGL( DZERO ) ** 2

* Instabilty can occur in this iterative process, for instance causing E
* values to oscillate between two values. This prevents convergence. To
* overcome this, do not allow the E factor to change quickly. This is
* achieved by lagging the estimate of E found above with some faction of
* the previous estimate. Also not the resulting change in E factor, and
* save the estimate.
            IF( ITER .GT. 0 ) THEN

               EOLD = EEST( IPAIR ) * EMED
               E = EOLD + LAG*( E - EOLD )

               DE( IPAIR ) = ABS( E - EOLD )

            ENDIF
            EEST( IPAIR ) = E

* Apply the E factor estimate.

            RE = 1.0D0 / DBLE( E )
            CALL CCG1_CMLTR( BAD, NEL, TI1( 1, IPAIR ), RE,
     :                       TI2( 1, IPAIR ), NERR, STATUS )

         ENDDO

* If an error has occurred then abort.

         IF ( STATUS .NE. SAI__OK ) GO TO 99

* If we have more than one estimate of the E factor, calculate the
* maximum E factor change on this iteration.

         MAXDE = 0.0
         IF ( ITER .GT. 0 ) THEN
            DO IPAIR = 1, NPAIR
               MAXDE = MAX( MAXDE, DE( IPAIR ) )
            ENDDO
         ENDIF

* Calculate the median of the E factor estimates and normalise the
* estimates to it (the called routine calculates the 50% quantile for
* unweighted data and no interpolation). Do the same for the zero shift
* estimates.

         CALL CCD1_QNTLR( .FALSE., .FALSE., 0.5, NPAIR, EEST, EEST,
     :        %VAL( CNF_PVAL( IPWRK8 ) ), EMED, STATUS )
         CALL CCD1_QNTLR( .FALSE., .FALSE., 0.5, NPAIR, ZEST, ZEST,
     :        %VAL( CNF_PVAL( IPWRK8 ) ), ZMED, STATUS )
         DO IPAIR = 1, NPAIR
            EEST( IPAIR ) = EEST( IPAIR ) / EMED
            ZEST( IPAIR ) = ZEST( IPAIR ) / ZMED
         ENDDO

* If at least two iterations have been performed and the maximum change
* in the E factor estimate on the last iteration was within the
* specified tolerance then the iteration has converged.
* If the iterations have not converged but we have run out of iterations
* then exit anyway.

         ITER = ITER + 1
         CONVERGED = ( ITER .GT. 1 ) .AND. ( MAXDE .LE. ETOL ) .OR.
     :               ( ITER .EQ. MAXIT )

* If diagnostic information is required then print some headings for the
* estimates on each iteration.

         IF ( MSG_FLEVOK( MSG__VERB, STATUS ) ) THEN
            CALL MSG_BLANKIF( MSG__VERB, STATUS )
            CALL MSG_SETI( 'ITER', ITER )
            CALL MSG_OUTIF( MSG__VERB, ' ', ' Iteration: ^ITER', STATUS)
            CALL MSG_BLANKIF( MSG__VERB, STATUS )
            WRITE( STRING,
     :         '( 5X, ''Image Pair     Scale    Zero  '' )' )
            CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
            WRITE( STRING,
     :         '( 5X, ''------------------------------'' )' )
            CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
            CALL MSG_BLANKIF( MSG__VERB, STATUS )

* Write out the estimates on this iteration for each image pair.

            DO IPAIR = 1, NPAIR
              WRITE( STRING,
     :           '( 5X, A, 5X, F6.4, 4X, F6.4 )' )
     :           ID( IPAIR ), EEST( IPAIR ), ZEST( IPAIR )
              CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
            ENDDO

* Write out the maximum change on this iteration.

            IF ( ITER .GT. 1 ) THEN
               CALL MSG_BLANK( STATUS )
               WRITE( STRING,
     :         '( '' Maximum change this iteration: '', F6.4 )' ) MAXDE
               CALL MSG_OUT( ' ', STRING, STATUS )
               WRITE( STRING,
     :         '( '' Maximum change permitted: '', F6.4 )' ) ETOL
               CALL MSG_OUT( ' ', STRING, STATUS )
            ENDIF
         ENDIF
      ENDDO

* Print out messages...

      CALL MSG_BLANK( STATUS )

      IF ( CONVERGED ) THEN
         WRITE( STRING,
     :        '( '' Convergence in '', I3, '' iterations.'' )' ) ITER
         CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
      ELSE
         WRITE( STRING,
     :       '( '' *** FAILED to converge in '', I3, ''iterations '' )')
     :        ITER
         CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
      ENDIF

      IF ( CONVERGED ) THEN
         WRITE( STRING, '( 3X, ''Image       E-factor'' )' )
         CALL MSG_OUT( ' ', STRING, STATUS )
         WRITE( STRING, '( 3X, ''-----       --------'' )' )
         CALL MSG_OUT( ' ', STRING, STATUS )

         DO IPAIR = 1, NPAIR
            WRITE( STRING, '( 3X, A, 2X, F6.4 )' )
     :           ID( IPAIR ), EEST( IPAIR )
            CALL MSG_OUT( ' ', STRING, STATUS )
         ENDDO
      ELSE
         WRITE( STRING,
     :      '( '' *** FAILED to converge in '', I3, ''iterations '' )' )
     :        ITER
         CALL MSG_OUT( ' ', STRING, STATUS )
      END IF

      CALL MSG_BLANK( STATUS )

* Loop through the input images again to apply the left and right
* channel sensitivity factors to the input data and put the result in
* the output data workspace.

      IPAIR = 0
      DO ISET = 1, NSET
         DO IPOS = 1, NPOS
            IF ( NSTATE( IPOS ) .GE. ISET ) THEN
               IPAIR = IPAIR + 1
               SENSL = 1.0D0 / DBLE( EEST( IPAIR ) )
               SENSR = SENSL / DBLE( F )
               CALL CCG1_CMLTR( BAD, NEL,
     :              %VAL( CNF_PVAL( IPDIN( 2 * IPOS - 1, ISET ) ) ),
     :              SENSL,
     :              %VAL( CNF_PVAL( IPDOU( 2 * IPOS - 1, ISET ) ) ),
     :              NERR, STATUS )
               CALL CCG1_CMLTR( BAD, NEL,
     :              %VAL( CNF_PVAL( IPDIN( 2 * IPOS, ISET ) ) ), SENSR,
     :              %VAL( CNF_PVAL( IPDOU( 2 * IPOS, ISET ) ) ),
     :              NERR, STATUS )

* If variance information is present then scale it with the
* sensitivites. Note that the errors on the sensitivities are not
* incorporated into the variance calculation at the moment.

               IF ( VAR ) THEN
                  SENS2L = SENSL * SENSL
                  SENS2R = SENSR * SENSR
                  CALL CCG1_CMLTR( BAD, NEL,
     :                 %VAL( CNF_PVAL( IPVIN( 2 * IPOS - 1, ISET ) ) ),
     :                 SENS2L,
     :                 %VAL( CNF_PVAL( IPVOU( 2 * IPOS - 1, ISET ) ) ),
     :                 NERR,
     :                 STATUS )
                  CALL CCG1_CMLTR( BAD, NEL,
     :                 %VAL( CNF_PVAL( IPVIN( 2 * IPOS, ISET ) ) ),
     :                 SENS2R,
     :                 %VAL( CNF_PVAL( IPVOU( 2 * IPOS, ISET ) ) ),
     :                 NERR, STATUS )
               ENDIF
            ENDIF
         ENDDO
      ENDDO

* Free workspace.

      CALL PSX_FREE( IPMED, STATUS )
      CALL PSX_FREE( IPWRK1, STATUS )
      CALL PSX_FREE( IPWRK2, STATUS )
      CALL PSX_FREE( IPWRK3, STATUS )
      CALL PSX_FREE( IPWRK4, STATUS )
      CALL PSX_FREE( IPWRK5, STATUS )
      CALL PSX_FREE( IPWRK6, STATUS )
      CALL PSX_FREE( IPWRK7, STATUS )
      CALL PSX_FREE( IPWRK8, STATUS )
      CALL PSX_FREE( IPWRK9, STATUS )

* Exit routine.

 99   CONTINUE
      END
