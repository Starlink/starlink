      SUBROUTINE POL_CALP( NEL, NSET, NPOS, NPAIR, IPDCOR, IPVCOR,
     :                     NSTATE, VAR, IEST, VIEST, QEST, VQEST, UEST,
     :                     VUEST, WEIGHT, OUT, VOUT, STATUS )
*+ 
*  Name:
*     POL_CALP

*  Purpose:
*     Calculate the stokes images from the input corrected polarisation
*     images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL_CALP(  NEL, NSET, NPOS, NPAIR, IPDCOR, IPVCOR,
*    :                NSTATE, VAR, IEST, VIEST, QEST, VQEST, UEST,
*    :                VUEST, WEIGHT, OUT, VOUT, STATUS )

*  Description:
*     This routine will calculate estimates of the I, Q, and U Stokes
*     images from a series of input polarisation images. The input
*     images should have been fully validated and corrected for both
*     polarisation and time dependent instrumental efficiency factors.
*
*     Estimates of the stokes images are formed by simple addition and
*     subtraction of the input polarisation images. The estimates are
*     then combined to form weighted median stokes values which are
*     returned in a single output array, along with the variances if
*     required.

*  Arguments:
*     NEL = INTEGER (Given)
*        Number of image elements
*     NSET = INTEGER (Given)
*        Number of polarisation data sets. Each set can contain up to a
*        maximum of 8 polarisation images
*     NPOS = INTEGER (Given)
*        Number of waveplate positions. 8 for linear polarimetry and 4
*        for circular
*     NPAIR = INTEGER (Given)
*        Number of polarimetric pairs
*     IPDCOR( 2 * NPOS, NSET ) = INTEGER (Given)
*        Pointers to the corrected input polarisation images
*     IPVCOR( 2 * NPOS, NSET ) = INTEGER (Given)
*        Pointers to variances on the input images
*     NSTATE( NPOS ) = INTEGER (Given)
*        the number of images at each waveplate position
*     VAR = LOGICAL (Given)
*        If TRUE then variance calculations are required
*     IEST( NEL, NPAIR ) = REAL (Given and Returned)
*        Array to hold estimates of I
*     VIEST( NEL, NPAIR ) = REAL (Given and Returned)
*        Array to hold estimates of the variance on I. Only used when
*        VAR is TRUE
*     QEST( NEL, NPAIR ) = REAL (Given and Returned)
*        Array to hold estimates of Q
*     VQEST( NEL, NPAIR ) = REAL (Given and Returned)
*        Array to hold estimates of the variance on Q. Only used when
*        VAR is TRUE
*     UEST( NEL, NPAIR ) = REAL (Given and Returned)
*        Array to hold estimates of U
*     VUEST( NEL, NPAIR ) = REAL (Given and Returned)
*        Array to hold estimates of the variance on U. Only used when
*        VAR is TRUE
*     WEIGHT( NPAIR ) = DOUBLE PRECISION (Given and Returned)
*        Array to hold weights for median stacking. This is only used
*        when VAR is FALSE.
*     OUT( NEL, * ) = REAL (Returned)
*        Output array containing the stokes images
*     VOUT( NEL, * ) = REAL (Returned)
*        Output array containing variances on the stokes images. This
*        is only valid when VAR is TRUE
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*
*  Authors:
*     TMG: Tim Gledhill (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-SEP-1997 (TMG):
*        Original version.
*     16-JAN-1998 (DSB):
*        Added missing IERR argument to VEC_RTOR calls. Added call to
*        CCD1_ORVAR to initialise the co-variance matrix.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      
*  Arguments Given:
      INTEGER NEL
      INTEGER NSET
      INTEGER NPOS
      INTEGER NPAIR
      INTEGER IPDCOR( 2 * NPOS, NSET )
      INTEGER IPVCOR( 2 * NPOS, NSET )
      INTEGER NSTATE( NPOS )
      LOGICAL VAR
      
*  Arguments Given and Returned:
      REAL IEST( NEL, NPAIR )
      REAL VIEST( NEL, NPAIR )
      REAL QEST( NEL, NPAIR )
      REAL VQEST( NEL, NPAIR )
      REAL UEST( NEL, NPAIR )
      REAL VUEST( NEL, NPAIR )
      DOUBLE PRECISION WEIGHT( NPAIR )

*  Arguments Returned:
      REAL OUT( NEL, * )
      REAL VOUT( NEL, * )
      
* Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISET, I            ! loop counter
      INTEGER IERR, NERR         ! primdat error information
      INTEGER NI, NQ, NU         ! the number of estimates of each
                                 ! stokes parameter
      INTEGER NMAT               ! covariance matrix dimension
      INTEGER IPCOV, IPWRK1, IPWRK2, IPWRK3, IPWRK4, IPWRK5
                                 ! workspace pointers
      LOGICAL BAD                ! bad pixel flag
      
      
*.

* Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

* Assume that bad pixels are present.
      BAD = .TRUE.
      
* Loop through the polarisation datasets. Note that these images should
* have been corrected with the polarisation and time dependent
* efficiency factors by this stage. 
      NI = 0
      NQ = 0
      NU = 0
      DO ISET = 1, NSET

* Estimates of I and Q can be obtained from the first waveplate
* position. If variance information is required then add the variance
* arrays. If we are operating in circular polarimetry mode (only two
* waveplate positions present) then Q will actually by V. 
         IF ( NSTATE( 1 ) .GE. ISET ) THEN
            NI = NI + 1
            NQ = NQ + 1
            CALL VEC_SUBR( BAD, NEL, %VAL( IPDCOR( 1, ISET ) ),
     :           %VAL( IPDCOR( 2, ISET ) ), QEST( 1, NQ ), IERR, NERR,
     :           STATUS )
            CALL VEC_ADDR( BAD, NEL, %VAL( IPDCOR( 1, ISET ) ),
     :           %VAL( IPDCOR( 2, ISET ) ), IEST( 1, NI ), IERR, NERR,
     :           STATUS )
            IF ( VAR ) THEN
               CALL VEC_ADDR( BAD, NEL, %VAL( IPVCOR( 1, ISET ) ),
     :              %VAL( IPVCOR( 2, ISET ) ), VIEST( 1, NI ), IERR,
     :              NERR, STATUS )
               CALL VEC_RTOR( BAD, NEL, VIEST( 1, NI ), VQEST( 1, NQ ),
     :              IERR, NERR, STATUS )
            ENDIF
         ENDIF

* Estimates of I and Q can be obtained from the second waveplate
* position. If variance information is required then add the variance
* arrays. If we are operating in circular polarimetry mode (only two
* waveplate positions present) then Q will actually be V.
         IF ( NSTATE( 2 ) .GE. ISET ) THEN
            NI = NI + 1
            NQ = NQ + 1
            CALL VEC_SUBR( BAD, NEL, %VAL( IPDCOR( 4, ISET ) ),
     :           %VAL( IPDCOR( 3, ISET ) ), QEST( 1, NQ ), IERR, NERR,
     :           STATUS )
            CALL VEC_ADDR( BAD, NEL, %VAL( IPDCOR( 4, ISET ) ),
     :           %VAL( IPDCOR( 3, ISET ) ), IEST( 1, NI ), IERR, NERR,
     :           STATUS )
            IF ( VAR ) THEN
               CALL VEC_ADDR( BAD, NEL, %VAL( IPVCOR( 4, ISET ) ),
     :              %VAL( IPVCOR( 3, ISET ) ), VIEST( 1, NI ), IERR,
     :              NERR, STATUS )
               CALL VEC_RTOR( BAD, NEL, VIEST( 1, NI ), VQEST( 1, NQ ),
     :              IERR, NERR, STATUS )
            ENDIF
         ENDIF

* Estimates of I and U can be obtained from the third waveplate
* position. If variance information is required then add the variance
* arrays. Check that we have sufficient waveplate positions to estimate
* U (i.e. that we are operating in linear polarimetry mode with four
* waveplate positions).
         IF ( NSTATE( 3 ) .GE. ISET ) THEN
            NI = NI + 1
            NU = NU + 1
            CALL VEC_SUBR( BAD, NEL, %VAL( IPDCOR( 5, ISET ) ),
     :           %VAL( IPDCOR( 6, ISET ) ), UEST( 1, NU ), IERR,
     :           NERR, STATUS )
            CALL VEC_ADDR( BAD, NEL, %VAL( IPDCOR( 5, ISET ) ),
     :           %VAL( IPDCOR( 6, ISET ) ), IEST( 1, NI ), IERR,
     :           NERR, STATUS )
            IF ( VAR ) THEN
               CALL VEC_ADDR( BAD, NEL, %VAL( IPVCOR( 5, ISET ) ),
     :              %VAL( IPVCOR( 6, ISET ) ), VIEST( 1, NI ), IERR,
     :              NERR, STATUS )
               CALL VEC_RTOR( BAD, NEL, VIEST( 1, NI ),
     :              VUEST( 1, NU ), IERR, NERR, STATUS )
            ENDIF
         ENDIF

* Estimates of I and U can be obtained from the fourth waveplate
* position. If variance information is required then add the variance
* arrays.  Check that we have sufficient waveplate positions to estimate
* U (i.e. that we are operating in linear polarimetry mode with four
* waveplate positions).
         IF ( NSTATE( 4 ) .GE. ISET ) THEN
            NI = NI + 1
            NU = NU + 1
            CALL VEC_SUBR( BAD, NEL, %VAL( IPDCOR( 8, ISET ) ),
     :           %VAL( IPDCOR( 7, ISET ) ), UEST( 1, NU ), IERR,
     :           NERR, STATUS )
            CALL VEC_ADDR( BAD, NEL, %VAL( IPDCOR( 8, ISET ) ),
     :           %VAL( IPDCOR( 7, ISET ) ), IEST( 1, NI ), IERR,
     :           NERR, STATUS )
            IF ( VAR ) THEN
               CALL VEC_ADDR( BAD, NEL, %VAL( IPVCOR( 7, ISET ) ),
     :              %VAL( IPVCOR( 8, ISET ) ), VIEST( 1, NI ), IERR,
     :              NERR, STATUS )
               CALL VEC_RTOR( BAD, NEL, VIEST( 1, NI ),
     :              VUEST( 1, NQ ), IERR, NERR, STATUS )
            ENDIF
         ENDIF

* Quit the loop and abort if an error occurs.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      ENDDO

* Allocate workspace for calculating the median images. Since NI should
* always be >= NQ and NU, we use NI to dimension the workspace.
      IF ( VAR ) THEN
         NMAT = NI * ( NI + 1 ) / 2
         CALL PSX_CALLOC( NMAT * NI, '_DOUBLE', IPCOV, STATUS )
      ENDIF
      CALL PSX_CALLOC( NI, '_REAL', IPWRK1, STATUS )
      CALL PSX_CALLOC( NI, '_REAL', IPWRK2, STATUS )
      CALL PSX_CALLOC( NI, '_DOUBLE', IPWRK3, STATUS )
      CALL PSX_CALLOC( NI, '_INTEGER', IPWRK4, STATUS )
      CALL PSX_CALLOC( NI, '_LOGICAL', IPWRK5, STATUS )

* Initialise the variances and covariances of the order statistics
* from n to 1, assuming an initially normal distribution.
      CALL CCD1_ORVAR( NI, NMAT, %VAL( IPWRK3 ), %VAL( IPCOV ), STATUS )

* The method for forming the median stokes parameter images depends on
* whether we require variance information. If variances are required
* then use the variances on the estimates of the stokes parameters as
* weights when forming the median. Also calculate the variances on the
* output median images.
      IF ( VAR ) THEN
         CALL CCG1_MDR1R( IEST, NEL, NI, VIEST, 1, %VAL( IPCOV ), NMAT,
     :        OUT( 1, 1 ), VOUT( 1, 1 ), %VAL( IPWRK1 ), %VAL( IPWRK2 ),
     :        %VAL( IPWRK3 ), %VAL( IPWRK4 ), %VAL( IPWRK5 ), STATUS )
         IF ( NQ .GT. 0 ) THEN
            CALL CCG1_MDR1R( QEST, NEL, NQ, VQEST, 1, %VAL( IPCOV ),
     :           NMAT, OUT( 1, 2 ), VOUT( 1, 2 ), %VAL( IPWRK1 ),
     :           %VAL( IPWRK2 ), %VAL( IPWRK3 ), %VAL( IPWRK4 ),
     :           %VAL( IPWRK5 ), STATUS )
         ENDIF
         IF ( NU .GT. 0 ) THEN
            CALL CCG1_MDR1R( UEST, NEL, NU, VUEST, 1, %VAL( IPCOV ),
     :           NMAT, OUT( 1, 3 ), VOUT( 1, 3 ), %VAL( IPWRK1 ),
     :           %VAL( IPWRK2 ), %VAL( IPWRK3 ), %VAL( IPWRK4 ),
     :           %VAL( IPWRK5 ), STATUS )
         ENDIF

* If variance information is not required then asign uniform weights to
* the input images and calculate the median stokes images.
      ELSE
         DO I = 1, NI
            WEIGHT( I ) = 1.0D0
         ENDDO
         CALL CCG1_MDR3R( IEST, NEL, NI, WEIGHT, 1, OUT( 1, 1 ),
     :           %VAL( IPWRK1 ), %VAL( IPWRK2 ), %VAL( IPWRK3 ),
     :           %VAL( IPWRK4 ), %VAL( IPWRK5 ), STATUS )
         IF ( NQ .GT. 0 ) THEN
            CALL CCG1_MDR3R( QEST, NEL, NQ, WEIGHT, 1, OUT( 1, 2 ),
     :           %VAL( IPWRK1 ), %VAL( IPWRK2 ), %VAL( IPWRK3 ),
     :           %VAL( IPWRK4 ), %VAL( IPWRK5 ), STATUS )
         ENDIF
         IF ( NU .GT. 0 ) THEN
            CALL CCG1_MDR3R( UEST, NEL, NU, WEIGHT, 1, OUT( 1, 3 ),
     :           %VAL( IPWRK1 ), %VAL( IPWRK2 ), %VAL( IPWRK3 ),
     :           %VAL( IPWRK4 ), %VAL( IPWRK5 ), STATUS )
         ENDIF
      ENDIF

* Free workspace.
      IF ( VAR ) CALL PSX_FREE( IPCOV, STATUS )
      CALL PSX_FREE( IPWRK1, STATUS )
      CALL PSX_FREE( IPWRK2, STATUS )
      CALL PSX_FREE( IPWRK3, STATUS )
      CALL PSX_FREE( IPWRK4, STATUS )
      CALL PSX_FREE( IPWRK5, STATUS )
      
*  Exit routine.
 99   CONTINUE
      END
