      SUBROUTINE POL_CALF( NEL, NSET, NPOS, IPDIN, IPVIN, NSTATE, VAR,
     :                     TOLS, TOLZ, MAXIT, SKYSUP, IMGID,
     :                     FEST, VFEST, F,  VF, STATUS )
*+
* Name:
*    POL_CALF

*  Purpose:
*     Calculate the instrumental polarisation efficiency factor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL_CALF(  NEL, NSET, NPOS, IPDIN, IPVIN, NSTATE, VAR,
*                     TOLS, TOLZ, MAXIT, SKYSUP, IMGID,
*                     FEST, VFEST, F,  VF, STATUS )

*  Description:
*     This routine calculates the polarisation-dependent instrumental
*     efficiency factor (F factor) for dual beam polarimetry. This
*     factor relates the sensitivity of the two channels of a dual beam
*     polarimeter and should be time independent, depending only on the
*     instrument characteristics.
*
*     A number of estimates of the F factor are made by intercomparing
*     appropriate polarisation states from the input data sets. These
*     data sets are assumed to have been already validated, paired and
*     sorted into polarisation states. The image intercomparisons are
*     preformed using an iterative least-squares technique. The final F
*     factor estimate is made by forming the mean of the individual
*     estimates weighted by their variances.
*
*     Various degrees of informational output can be requested via
*     MSG message filtering, ranging from none (QUIET) to diagnostic
*     (DEBUG).

*  Arguments:
*     NEL = INTEGER (Given)
*        Number of image elements
*     NSET = INTEGER (Given)
*        Number of sorted polarisation sets. Each set contains a maximum
*        of eight images sorted into polarisation states.
*     NPOS = INTEGER (Given) 
*        The number of waveplate positions recorded. This will be 4 for
*        linear polarimetry and 2 for circular polarimetry.
*     IPDIN( 8, NSET ) =  (Given)
*        An array of memory pointers to reference the mapped
*        polarisation data, sorted according to waveplate position and
*        polarisation set.
*     IPVIN( 8, NSET ) = INTEGER (Given)
*        {An array of memory pointers to reference the mapped
*        polarisation data VARIANCES, sorted according to waveplate
*        position and polarisation set.
*     NSTATE( NPOS ) = INTEGER (Given)
*        The number of images at each waveplate position.
*     VAR = LOGICAL (Given)
*        If TRUE then variance calculations are required.
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
*        intercomparisons.
*     IMGID( 4, NSET ) = CHARACTER * ( * ) (Given)
*        Image identifiers.
*     FEST( 2 * NSET ) = REAL (Given and Returned)
*        F factor estimates.
*     VFEST( 2 * NSET ) = REAL (Given and Returned)
*        Variances on F factor estimates.
*     F = REAL (Returned)
*        Final F factor estimate.
*     VF = REAL (Returned)
*        Variance on final F factor estimate.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     All Rights Reserved.

*  Authors:
*     TMG: Tim Gledhill (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-SEP-1997 (TMG):
*        Original version.
*     12-MAY-1998 (DSB):
*        Reversed order of data arrays passed to CCD1_CMPRR. Previously
*        the returned scale factor was the reciprocal of the required
*        value.
*     02-JUN-1998 (TMG):
*        Dimension IPDIN, IPVIN and IMGID correctly.
*     4-JUN-1998 (DSB):
*        Removed 10 character restrictions on image identifiers.
*     24-JUN-1998 (DSB):
*        Continue processing if the image inter-comparisons fail to converge.
*     19-APR-2000 (DSB):
*        Annull ignorable errors instead of flushing them if ILEVEL is
*        less than 2.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
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
      INCLUDE 'MSG_PAR'          ! For MSG__ constants
      
*  Global Variables:
*      {include_global_variables}...
      
*  Arguments Given:
      INTEGER NEL
      INTEGER NSET
      INTEGER NPOS
      INTEGER IPDIN( 8, NSET )
      INTEGER IPVIN( 8, NSET )
      INTEGER NSTATE( NPOS )
      LOGICAL VAR
      REAL TOLS
      REAL TOLZ
      INTEGER MAXIT
      REAL SKYSUP
      CHARACTER * ( * ) IMGID( 4, NSET )
      
*  Arguments Given and Returned:

      REAL FEST( 2 * NSET )
      REAL VFEST( 2 * NSET )
      
*  Arguments Returned:
      REAL F
      REAL VF
      
*  Status:
      INTEGER STATUS             ! Global status

      
*  Local Variables:
      INTEGER NWRK1, NWRK2, NWRK3, NWRK4
                                 ! workspace dimensions
      INTEGER IPWRK1, IPWRK2, IPWRK3, IPWRK4
                                 ! memory pointers to workspace
      INTEGER NITER, NPTS        ! number of iterations and points use
                                 ! in image intercomparisons
      INTEGER NEST               ! number of valid F factor estimates
      INTEGER ISET, I            ! loop counters
      
      REAL F1, F2, DF1, DF2      ! image scales and errors
      REAL SUM1, SUM2            ! sums for calculating mean F and
                                 ! variance
      
      LOGICAL GETS, GETZ, BAD    ! logical flags
      LOGICAL FLUSHING           ! If we are in verbose mode and flushing

      DOUBLE PRECISION SCALE, DSCALE ! scale factor and standard error
      DOUBLE PRECISION ZERO, DZERO   ! zero shift and standard error
      DOUBLE PRECISION ORIGIN, DS, DZ ! false origin and changes in
                                 ! scale and zero on final iteration.

      CHARACTER * ( 80 ) STRING  ! Output information buffer.
      
*.

* Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) GO TO 99

* Determine whether errors are flushed
      FLUSHING = MSG_FLEVOK( MSG__VERB, STATUS )

* Both scale and zero offsets should be calculated when performing the
* image intercomparisons so that the scale factor relating the two
* images is not biased by any residual from the sky subtraction.

      GETS = .TRUE.
      GETZ = .TRUE.

* Assume BAD pixel values are present in all images. 

      BAD = .TRUE.

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

* Allocate workspace for image intercomparisons.

      CALL PSX_CALLOC( NWRK1, '_INTEGER', IPWRK1, STATUS )
      CALL PSX_CALLOC( NWRK2, '_REAL', IPWRK2, STATUS )
      CALL PSX_CALLOC( NWRK3, '_REAL', IPWRK3, STATUS )
      CALL PSX_CALLOC( NWRK4, '_REAL', IPWRK4, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      
* Loop through the mapped input image sets to perform F factor
* calculations where possible. Initialise the count of F factor
* estimates.

      NEST = 0
      DO ISET = 1, NSET

* To calculate the F factor associated with the Q images in the data set
* images 1->4 must be defined.

         IF ( NSTATE( 1 ) .GE. ISET .AND. NSTATE( 2 ) .GE. ISET ) THEN
         
* Calculate the ratio of the first pair of like `Q' polarisation states,
* I4/I1.
            NITER = 0
            CALL CCD1_CMPRR( BAD, VAR, NEL, 
     :                       %VAL( CNF_PVAL( IPDIN( 1, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPVIN( 1, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPDIN( 4, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPVIN( 4, ISET ) ) ),
     :                       GETS, GETZ, TOLS, TOLZ, MAXIT, SKYSUP,
     :                       SCALE, DSCALE, ZERO, DZERO, ORIGIN, NPTS,
     :                       NITER, DS, DZ, %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ), 
     :                       %VAL( CNF_PVAL( IPWRK3 ) ),
     :                       %VAL( CNF_PVAL( IPWRK4 ) ), STATUS )

*  If the iteration limit was reached before convergence was achieved, an
*  SAI__ERROR report will be made by CCD1_CMPRR. In this case, the
*  resulting approximate solution will probably be OK, so just annul (or
*  flush if verbose mode) the error and carry on.
            IF( STATUS .EQ. SAI__ERROR .AND. NITER .EQ. MAXIT ) THEN 
               IF( FLUSHING ) THEN
                  CALL ERR_REP( ' ', 'The above error probably '//
     :                       'does not matter and so is being '//
     :                       'ignored... The F factors may only '//
     :                       'be approximate!', STATUS )
                  CALL ERR_FLUSH( STATUS )
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
            END IF

*  Store the values returned by CCD1_CMPRR.
            F1 = SNGL( SCALE )
            DF1 = SNGL( DSCALE )

* Calculate the ratio of the second pair of like `Q' polarisation
* states, I2/I3.
            NITER = 0
            CALL CCD1_CMPRR( BAD, VAR, NEL, 
     :                       %VAL( CNF_PVAL( IPDIN( 3, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPVIN( 3, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPDIN( 2, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPVIN( 2, ISET ) ) ),
     :                       GETS, GETZ, TOLS, TOLZ, MAXIT, SKYSUP,
     :                       SCALE, DSCALE, ZERO, DZERO, ORIGIN, NPTS,
     :                       NITER, DS, DZ, %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ), 
     :                       %VAL( CNF_PVAL( IPWRK3 ) ),
     :                       %VAL( CNF_PVAL( IPWRK4 ) ), STATUS )

            IF( STATUS .EQ. SAI__ERROR .AND. NITER .EQ. MAXIT ) THEN 
               IF( FLUSHING ) THEN
                  CALL ERR_REP( ' ', 'The above error probably '//
     :                       'does not matter and so is being '//
     :                       'ignored... The F factors may only '//
     :                       'be approximate!', STATUS )
                  CALL ERR_FLUSH( STATUS )
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
            END IF

            F2 = SNGL( SCALE )
            DF2 = SNGL( DSCALE )

* If the intercomparisons were successful then calculate an estimate of
* the F factor. Otherwise flush the error. Errors on the scale factor
* are produced regardless of the setting of VAR so we can still
* calculate an estimate of the error on F.

            IF ( STATUS .EQ. SAI__OK ) THEN
               NEST = NEST + 1
               FEST( NEST ) = SQRT( F1 * F2 )
               VFEST( NEST ) = ( DF1 * DF1 ) * ( F2 / ( 4.0 * F1 ) )
     :              +          ( DF2 * DF2 ) * ( F1 / ( 4.0 * F2 ) )

* If diagnostic information is required then print out the F factor
* estimates.

               IF ( MSG_FLEVOK( MSG__VERB, STATUS ) ) THEN
                  CALL MSG_BLANKIF( MSG__VERB, STATUS )
                  WRITE( STRING,
     :         '( 5X, ''Image             Ratio      F       DF '' )' )
                  CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
                  WRITE( STRING,
     :         '( 5X, ''---------------------------------------'' )' )
                  CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
                  CALL MSG_BLANKIF( MSG__VERB, STATUS )
                  WRITE( STRING, '( 5X, A, ''(E)'' )' )
     :                 IMGID( 2, ISET )
                  CALL MSG_OUTIF( MSG__VERB, ' ', STRING , STATUS )
                  WRITE( STRING,
     :               '( 5X, A, ''(O)'', 4X, F6.3 )' )
     :                 IMGID( 1, ISET ), F1
                  CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
                  WRITE( STRING, '( 5X, A, ''(E)'' )' )
     :                 IMGID( 1, ISET )
                  CALL MSG_OUTIF( MSG__VERB, ' ', STRING , STATUS )
                  WRITE( STRING,
     :            '( 5X, A, ''(O)'', 4X, F6.3, 4X, F6.3, 2X, '//
     :              'E8.3 )' ), IMGID( 2, ISET ), F2, FEST( NEST ),
     :                          VFEST( NEST )
                  CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
               ENDIF
            ELSE
               IF( FLUSHING ) THEN
                  CALL ERR_REP( ' ', 'The above error probably '//
     :                       'does not matter and so is being '//
     :                       'ignored...', STATUS )
                  CALL ERR_FLUSH( STATUS )
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
            ENDIF
         ENDIF
            
* To calculate the F factor associated with the U images in the data set
* images 5->8 must be defined.

         IF ( NSTATE( 3 ) .GE. ISET .AND. NSTATE( 4 ) .GE. ISET ) THEN

* Calculate the ratio of the first pair of like `U' polarisation states,
* I8/I5.
            NITER = 0
            CALL CCD1_CMPRR( BAD, VAR, NEL, 
     :                       %VAL( CNF_PVAL( IPDIN( 5, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPVIN( 5, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPDIN( 8, ISET ) ) ) ,
     :                       %VAL( CNF_PVAL( IPVIN( 8, ISET ) ) ),
     :                       GETS, GETZ, TOLS, TOLZ, MAXIT, SKYSUP,
     :                       SCALE, DSCALE, ZERO, DZERO, ORIGIN, NPTS,
     :                       NITER, DS, DZ, %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ), 
     :                       %VAL( CNF_PVAL( IPWRK3 ) ),
     :                       %VAL( CNF_PVAL( IPWRK4 ) ), STATUS )

            IF( STATUS .EQ. SAI__ERROR .AND. NITER .EQ. MAXIT ) THEN 
               IF( FLUSHING ) THEN
                  CALL ERR_REP( ' ', 'The above error probably '//
     :                       'does not matter and so is being '//
     :                       'ignored... The F factors may only '//
     :                       'be approximate!', STATUS )
                  CALL ERR_FLUSH( STATUS )
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
            END IF

            F1 = SNGL( SCALE )
            DF1 = SNGL( DSCALE )
      
* Calculate the ratio of the second pair of like `U' polarisation
* states, I6/I7.

            NITER = 0
            CALL CCD1_CMPRR( BAD, VAR, NEL, 
     :                       %VAL( CNF_PVAL( IPDIN( 7, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPVIN( 7, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPDIN( 6, ISET ) ) ),
     :                       %VAL( CNF_PVAL( IPVIN( 6, ISET ) ) ),
     :                       GETS, GETZ, TOLS, TOLZ, MAXIT, SKYSUP,
     :                       SCALE, DSCALE, ZERO, DZERO, ORIGIN, NPTS,
     :                       NITER, DS, DZ, %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ), 
     :                       %VAL( CNF_PVAL( IPWRK3 ) ),
     :                       %VAL( CNF_PVAL( IPWRK4 ) ), STATUS )

            IF( STATUS .EQ. SAI__ERROR .AND. NITER .EQ. MAXIT ) THEN 
               IF( FLUSHING ) THEN
                  CALL ERR_REP( ' ', 'The above error probably '//
     :                       'does not matter and so is being '//
     :                       'ignored... The F factors may only '//
     :                       'be approximate!', STATUS )
                  CALL ERR_FLUSH( STATUS )
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
            END IF

            F2 = SNGL( SCALE )
            DF2 = SNGL( DSCALE )

* If the intercomparisons were successful then calculate an estimate of
* the F factor. Otherwise flush the error. Errors on the scale factor
* are produced regardless of the setting of VAR so we can still
* calculate an estimate of the error on F.

            IF ( STATUS .EQ. SAI__OK ) THEN
               NEST = NEST + 1
               FEST( NEST ) = SQRT( F1 * F2 )
               VFEST( NEST ) = ( DF1 * DF1 ) * ( F2 / ( 4.0 * F1 ) )
     :              +          ( DF2 * DF2 ) * ( F1 / ( 4.0 * F2 ) )

* If diagnostic information is required then print out the F factor
* estimates.

               IF ( MSG_FLEVOK( MSG__VERB, STATUS ) ) THEN
                  WRITE( STRING, '( 5X, A, ''(E)'' )' )
     :                 IMGID( 4, ISET )
                  CALL MSG_OUTIF( MSG__VERB, ' ', STRING , STATUS )
                  WRITE( STRING,
     :               '( 5X, A, ''(O)'', 4X, F6.3 )' )
     :                 IMGID( 3, ISET ), F1
                  CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
                  WRITE( STRING, '( 5X, A, ''(E)'' )' )
     :                 IMGID( 3, ISET )
                  CALL MSG_OUTIF( MSG__VERB, ' ', STRING , STATUS )
                  WRITE( STRING,
     :            '( 5X, A, ''(O)'', 4X, F6.3, 4X, F6.3, 2X, '//
     :              'E8.3 )' ) IMGID( 4, ISET ), F2, FEST( NEST ),
     :                         VFEST( NEST )
                  CALL MSG_OUTIF( MSG__VERB, ' ', STRING, STATUS )
               ENDIF
            ELSE
               IF( FLUSHING ) THEN
                  CALL ERR_REP( ' ', 'The above error probably '//
     :                       'does not matter and so is being '//
     :                       'ignored...', STATUS )
                  CALL ERR_FLUSH( STATUS )
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
            ENDIF
         ENDIF
      ENDDO

* If there are some F factor estimates, calculate the weighted  mean
* and variance.

      IF ( NEST .GE. 1 ) THEN
         SUM1 = 0.0
         SUM2 = 0.0
         DO I = 1, NEST
            SUM1 = SUM1 + ( FEST( I ) / MAX( 1.0E-10, VFEST( I ) ) ) 
            SUM2 = SUM2 + ( 1.0 / MAX( 1.0E-10, VFEST( I ) ) )
         ENDDO
         F = SUM1 / SUM2
         VF = 1.0 / SUM2

* If user information is required then print out the mean F factor.

         CALL MSG_BLANK( STATUS )
         WRITE( STRING,
     :        '( ''   Mean F factor,'', I3, '' estimates: '', F6.3 )' )
     :        NEST, F
         CALL MSG_OUT( ' ', STRING, STATUS )
         
* If no estimates were possible then exit with an error and add context.

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POL_CALF_NOF', 'POL_CALF: No F factor '//
     :                 'estimates could be made', STATUS )
      ENDIF
      
* Free workspace.

      CALL PSX_FREE( IPWRK1, STATUS )
      CALL PSX_FREE( IPWRK2, STATUS )
      CALL PSX_FREE( IPWRK3, STATUS )
      CALL PSX_FREE( IPWRK4, STATUS )

* Exit routine.

 99   CONTINUE
      END
