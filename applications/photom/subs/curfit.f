************************************************************************

        SUBROUTINE CURFIT( X, Y, W, POINTS, MODE_W, A, DA, N_PAR,
     1                     N_TERM, COVAR, DIM_C, D_CHI, B_CHI,
     2                     SIG_CLIP, M_REP, FLAG, FUNCTN, FDERIV )

*+
*  Name :
*     CURFIT
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*
*
*  Description :
*     A new version of CURFIT; see write-up
*
*  Arguments :
*     {arguement_description}...
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     KM: Koji Mukai (Oxford University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     08-FEB-1999 (AA):
*        Cut and hack for Starlink
*     17-FEB-1999 (AA):
*        Converted to use MSG system
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings. Initialise
*        STATUS so that messages are output. Wrap lines longer than
*        72 characters. Purge all tab characters.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
        IMPLICIT NONE

*  Include files:
        INCLUDE 'SAE_PAR'
        INCLUDE 'MSG_PAR'

*
        INTEGER M_PAR
        PARAMETER( M_PAR = 32 )

*

        INTEGER POINTS             ! Number of data points
        REAL X( POINTS )           ! X-data
        REAL Y( POINTS )           ! Y-data
        REAL W( POINTS )           ! Weight
        INTEGER MODE_W             ! 1 Poisson, 0 equal weight, -1 explicit
        INTEGER N_PAR              ! Number of parameters
        REAL A( N_PAR )            ! The parameters
        REAL DA( N_PAR )           ! Step for numerical differenciation
        INTEGER N_TERM             ! Number of free parameters
        INTEGER DIM_C              ! Dimension of covariance matrix
        REAL COVAR( DIM_C, DIM_C ) ! The covariance matrix
        REAL D_CHI                 ! Fractional Chi change for convergence
        REAL B_CHI                 ! The final (best) reduced Chi squared
        REAL SIG_CLIP              ! If >0, do iterative clipping
        INTEGER M_REP              ! Maximum number of trials
        INTEGER FLAG               ! Controls output; error handling

*               0: ok; -1: not converged; -2: lambda too big;
*               -91: initial parameter out of range; -92: no D.O.F.

        REAL FUNCTN                ! User-supplied model function and its
        EXTERNAL FUNCTN, FDERIV    ! derivatives

        INTEGER N_FREE, J, K, REJECT
        REAL LAMBDA, TEMP
        REAL O_CHI, CHI, THRES, IMP_CHI
        LOGICAL INTERACTIVE

        EXTERNAL FIXED_PAR              ! Compulsory initialization of
        EXTERNAL LINKED_POS             ! free_list, link_status, boundary_par,
        EXTERNAL BOUNDARY               ! cyclic_list, data_cyclic and
        EXTERNAL CYCLIC_PAR             ! scale_down common blocks through
        EXTERNAL CYCLIC_FUNCTION        ! BLOCK DATA units; all can be over-
        EXTERNAL SCALE_CHANGE           ! ridden by the user

        REAL LO_A( M_PAR ), HI_A( M_PAR )
        INTEGER LIST_CYCLIC( M_PAR )
        REAL TAB_PERIOD( M_PAR )
        REAL TAB_BOTTOM( M_PAR )
        LOGICAL DEBUG
        COMMON / BOUNDARY_PAR / LO_A, HI_A
        COMMON / CYCLIC_LIST / LIST_CYCLIC, TAB_PERIOD, TAB_BOTTOM
        COMMON / BUG / DEBUG
        CHARACTER TEXT * ( MSG__SZMSG )

        INTEGER STATUS

*.
*        WRITE(*,*) ' DEBUG --- --- --- --- Entering CURFIT()'

*       STATUS needs initialising if we are to see any reports.
        STATUS = SAI__OK

        IF( FLAG .GT. 1000 ) THEN
          INTERACTIVE = .FALSE.
        ELSE
          INTERACTIVE = .TRUE.
        END IF
        FLAG = 0
*
        DO K = 1, N_PAR
          IF( A( K ) .LE. LO_A( K ) .OR. A( K ) .GE. HI_A( K ) ) THEN
            FLAG = -91
            RETURN
          END IF
        END DO
*
        DO K = 1, N_PAR
          IF( LIST_CYCLIC( K ) .EQ. 1 ) THEN    ! actively cyclic
            IF( A( K ) .LT. TAB_BOTTOM( K ) ) THEN

              TEXT = ' WARNING > Parameter out of cyclic '//
     :               'range. It will be modified before fitting...'
              CALL MSG_OUT( ' ', TEXT, STATUS )


              A( K ) = TAB_BOTTOM( K ) + TAB_PERIOD( K ) + MOD( A( K )
     :                 - TAB_BOTTOM( K ), TAB_PERIOD( K ) )
            ELSE IF( A( K ) .GE. TAB_BOTTOM( K ) + TAB_PERIOD( K )) THEN

              TEXT = ' WARNING > Parameter out of cyclic ' //
     1               'range. It will be modified before fitting...'
              CALL MSG_OUT( ' ', TEXT, STATUS )


              A( K ) = TAB_BOTTOM( K ) +
     1                 MOD( A( K ) - TAB_BOTTOM( K ), TAB_PERIOD( K ) )
            END IF
          END IF
        END DO
*
        REJECT = 1
        IF( MODE_W .GT. 0 ) THEN        ! Poisson weighting
          DO J = 1, POINTS
            IF( Y( J ) .GT. 0 ) THEN
              W( J ) = 1.0 / Y( J )
            ELSE                        ! Ignore negative points
              W( J ) = 0.0
            END IF
          END DO
        ELSE IF( MODE_W .EQ. 0 ) THEN   ! Equal weighting
          DO J = 1, POINTS
            W( J ) = 1.0
          END DO
*       ELSE                            ! Predetermined (W, not sigma)
        END IF
*
        DO WHILE( REJECT .GT. 0 )       ! Iterative Clipping Loop
          IMP_CHI = 1000.0
*                                       ! First, degree of freedom
          N_FREE = 0
          DO J = 1, POINTS
            IF( W( J ) .NE. 0.0 ) THEN
              N_FREE = N_FREE + 1
            END IF
          END DO
          N_FREE = N_FREE - N_TERM
          IF( N_FREE .LE. 0 ) THEN

            TEXT = 'ERROR > No degrees of freedom in CURFIT'
            CALL MSG_OUT(' ', TEXT, STATUS)
            WRITE(TEXT, '(''        # of points ='',I5)') POINTS
            CALL MSG_OUT(' ', TEXT, STATUS)
            WRITE(TEXT, '(''        # of free parameters ='',I2)')
     :            N_TERM
            CALL MSG_OUT(' ', TEXT, STATUS)
            WRITE(TEXT, '(''        # DOF ='',I3)') N_FREE
            CALL MSG_OUT(' ', TEXT, STATUS)

            FLAG = -92
            RETURN
          END IF

*       first call to marquardt

          LAMBDA = -0.001
*          WRITE(*,*) ' DEBUG --- --- --- --- 1st call to MARQUARDT()'
          CALL MARQUARDT( X, Y, W, POINTS, LAMBDA, A, DA, COVAR,
     1                    DIM_C, O_CHI, N_PAR, N_TERM, FUNCTN, FDERIV )
          O_CHI = O_CHI / N_FREE
          IF( INTERACTIVE ) THEN

            WRITE(TEXT, '(''Iteration # '', I2, '' Chi-squared = '',
     1          E10.3,  '' Lambda = '', E8.1 / (5('' '',E10.3)) )' )
     2          0, O_CHI, LAMBDA, ( A( J ), J = 1, N_PAR )
            CALL MSG_OUT(' ', TEXT, STATUS)

          END IF
*                               ! Iteration
          DO K = 1, M_REP
*            WRITE(*,*) ' DEBUG --- --- --- --- 2nd call to MARQUARDT()'
            CALL MARQUARDT( X, Y, W, POINTS, LAMBDA, A, DA,
     1                          COVAR, DIM_C, CHI, N_PAR, N_TERM,
     2                                                  FUNCTN, FDERIV )
            CHI = CHI / N_FREE
            IF( INTERACTIVE ) THEN
             WRITE(TEXT, '(''Iteration # '', I2, '' Chi-squared = '',
     1       E10.3, '' Lambda = '', E8.1 / ( 5( '' '', E10.3 ) ))' )
     2       K, CHI, LAMBDA, ( A( J ), J = 1, N_PAR )
             CALL MSG_OUT(' ', TEXT, STATUS)

            END IF

*    Small bug (which has a huge effect) spotted by Timn and Tariq.
*    This line used to read .gt., which meant that if the fit had
*    converged so well that o_chi was equal to chi you never tested
*    against the convergence condition.

            IF( O_CHI .GE. CHI ) THEN
              IMP_CHI = ( O_CHI - CHI ) / CHI
              IF( IMP_CHI .LT. D_CHI ) GOTO 300
              O_CHI = CHI
            END IF
            IF( LAMBDA .GE. 1.0E+30 ) THEN

              WRITE(TEXT, '(''WARNING > After '', I3, ''th iteration, '
     1              // ' lambda is becoming too big'')') K
              CALL MSG_OUT(' ', TEXT, STATUS)
              WRITE(TEXT, '(''          The last fractional improvement'
     1              // ' in reduced chi-squared is '', E8.2 )')  IMP_CHI
              CALL MSG_OUT(' ', TEXT, STATUS)
              WRITE(*,*) TEXT, STATUS

              IF( IMP_CHI .LT. 1.0E-03 ) THEN

              TEXT = '          It has converged!'
              CALL MSG_OUT(' ', TEXT, STATUS)


              ELSE IF( IMP_CHI .LT. 0.1 ) THEN

              TEXT = '          It may have converged!'
              CALL MSG_OUT(' ', TEXT, STATUS)

              ELSE

              TEXT = 'ERROR > CURFIT has problems.'
              CALL MSG_OUT(' ', TEXT, STATUS)

              END IF
              FLAG = -2
              GOTO 300
            END IF
          END DO

*    .NOT. converged

          WRITE(TEXT, '(''WARNING > not converged ' //
     1          'after '', I2, '' iterations!'')') M_REP
          CALL MSG_OUT(' ', TEXT, STATUS)

          FLAG = -1

*     converged

300       CONTINUE
          B_CHI = CHI

*     check for clipping

          REJECT = 0
          IF( SIG_CLIP .GT. 0.0 ) THEN
            THRES = CHI * SIG_CLIP * SIG_CLIP
            DO J = 1, POINTS
              TEMP = Y( J ) - FUNCTN( X( J ), A, N_PAR )
              TEMP = TEMP * TEMP * W( J )
              IF( TEMP .GT. THRES ) THEN
                REJECT = REJECT + 1
                W( J ) = 0.0
              END IF
            END DO
          END IF
          IF( REJECT .GT. 0 ) THEN

            WRITE(TEXT, '(I3, '' points are rejected as outliers. '
     1      // ' Restarting fit again...'')') REJECT
            CALL MSG_OUT(' ', TEXT, STATUS)

          END IF
        END DO

*    Final call...calculate covariances

        LAMBDA = 0.0
*        WRITE(*,*) ' DEBUG --- --- --- --- 3rd call to MARQUARDT()'
        CALL MARQUARDT( X, Y, W, POINTS, LAMBDA, A, DA, COVAR,
     1                    DIM_C, CHI, N_PAR, N_TERM, FUNCTN, FDERIV )
        DO K = 1, N_PAR
          IF( LIST_CYCLIC( K ) .NE. 0 ) THEN    ! CYCLIC
            IF( A( K ) .LT. TAB_BOTTOM( K ) ) THEN
              A( K ) = TAB_BOTTOM( K ) + TAB_PERIOD( K ) +
     1                 MOD( A( K ) - TAB_BOTTOM( K ), TAB_PERIOD( K ) )
            ELSE
              A( K ) = TAB_BOTTOM( K ) + MOD( A( K ) -
     1                 TAB_BOTTOM( K ), TAB_PERIOD( K ) )
            END IF
          END IF
        END DO
*
*        WRITE(*,*) ' DEBUG --- --- --- --- Leaving CURFIT()'
        END
