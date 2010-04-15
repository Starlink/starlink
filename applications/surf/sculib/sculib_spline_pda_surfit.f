      SUBROUTINE SCULIB_SPLINE_PDA_SURFIT (NDP, S, X_IN, Y_IN, DATA_IN,
     :  VARIANCE_IN, NX_OUT, NY_OUT, X_OUT, Y_OUT, DATA_OUT, STATUS)

*+
*  Name:
*     SCULIB_SPLINE_PDA_SURFIT

*  Purpose:
*     Fit a surface to an irregular grid using the PDA function PDA_SURFIT

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_SPLINE_PDA_SURFIT (NDP, X_IN, Y_IN, DATA_IN,
*    :   VARIANCE_IN, NX_OUT, NY_OUT, X_OUT, Y_OUT, DATA_OUT, STATUS)

*  Description:
*     This routine provides a wrapper for the PDA_SURFIT spline interpolation
*     routine. The quirks of the algorithm are dealt with here so that
*     SCULIB_SPLINE_REGRID does not have to know anything about the interpolation
*     routine.

*  Arguments:
*     NDP = INTEGER (Given)
*       Number of data points on irregular grid
*     S = REAL (Given)
*       Smoothness factor
*     X_IN ( NDP ) = REAL (Given)
*       X coordinates of input data
*     Y_IN ( NDP ) = REAL (Given)
*       Y coordinates of input data
*     DATA_IN ( NDP ) = REAL (Given)
*       Data values for each X,Y
*     VARIANCE_IN ( NDP ) = REAL (Given)
*       Variance for each data value
*     NX_OUT = INTEGER (Given)
*       Number of pixels in X direction
*     NY_OUT = INTEGER (Given)
*       Number of pixels in Y direction
*     X_OUT ( NX_OUT ) = REAL (Given)
*       X coordinates of output points
*     Y_OUT ( NY_OUT ) = REAL (Given)
*       Y coordinates of output points
*     DATA_OUT ( NX_OUT, NY_OUT ) = REAL (Returned)
*       Output data values
*     STATUS = INTEGER (Given & Returned)
*       Global status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     17 April 1997 (TimJ)
*        Original version

*  Bugs:

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'MSG_PAR'                          ! MSG__ constants
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! VAL__ constants
      INCLUDE 'CNF_PAR'                          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDP
      REAL    DATA_IN ( NDP )
      INTEGER NX_OUT
      INTEGER NY_OUT
      REAL    S
      REAL    VARIANCE_IN ( NDP )
      REAL    X_IN ( NDP )
      REAL    X_OUT( NX_OUT )
      REAL    Y_IN ( NDP )
      REAL    Y_OUT( NY_OUT )

*  Arguments Returned:
      REAL    DATA_OUT ( NX_OUT, NY_OUT )

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:

*  Local Constants:
      REAL    SMALL                              ! Minimum variance
      PARAMETER (SMALL = 1.0E-5)
      REAL    EPS                                ! Fit tolerance
      PARAMETER (EPS = 1.0E-10)

*  Local Variables:
      REAL    B1                                 ! Used in LWRK1 calculation
      REAL    B2                                 ! Used in LWRK1 calculation
      REAL    BX                                 ! Used in LWRK1 calculation
      REAL    BY                                 ! Used in LWRK1 calculation
      INTEGER C_END                              ! End of C_PTR
      INTEGER C_PTR                              ! Spline coefficients
      INTEGER DATA_END                           ! End of DATA_PTR
      INTEGER DATA_OFFSET                        ! Pointer offset
      INTEGER DATA_PTR                           ! Scratch output grid
      REAL    FP                                 ! Sum of squared residuals
      INTEGER I                                  ! Loop counter
      INTEGER IER                                ! Return value from fit
      INTEGER IERR                               ! Used for VEC_
      INTEGER IOPT                               ! Fit mode
      INTEGER IWRK_END                           ! End of IWRK
      INTEGER IWRK_PTR                           ! Integer scratch array
      INTEGER J                                  ! Loop counter
      INTEGER KWRK                               ! Size of IWRK
      REAL    KM                                 ! Used in LWRK1 calculation
      INTEGER KX                                 ! Degree of spline in X
      INTEGER KY                                 ! Degree of spline in Y
      INTEGER LWRK1                              ! Size of WRK1
      INTEGER LWRK2                              ! Size of WRK2
      REAL    NE                                 ! Used in LWRK1 calculation
      INTEGER NERR                               ! Used for VEC_
      INTEGER NMAX                               ! Size of TX_PTR and TY_PTR
      INTEGER NX                                 ! Number of X knots
      INTEGER NXEST                              ! Max number of knots in X
      INTEGER NY                                 ! Number of Y knots
      INTEGER NYEST                              ! Max number of knots in Y
      REAL    RTEMP                              ! Temp real
      INTEGER SIZE_C                             ! Size of C_PTR
      INTEGER TX_END                             ! End of TX
      INTEGER TX_PTR                             ! Knot positions
      INTEGER TY_END                             ! End of TY
      INTEGER TY_PTR                             ! Knot positions
      REAL    U                                  ! Used in LWRK1 calculation
      REAL    V                                  ! Used in LWRK1 calculation
      REAL    WEIGHT                             ! Temporary weight
      INTEGER WEIGHT_END                         ! End of weight
      INTEGER WEIGHT_PTR                         ! 1/sqrt(variance)
      INTEGER WRK1_END                           ! End of WRK1
      INTEGER WRK1_PTR                           ! Scratch array
      INTEGER WRK2_END                           ! End of WRK2
      INTEGER WRK2_PTR                           ! Scratch array
      REAL    XB                                 ! X Lower bound of approx
      REAL    XE                                 ! X upper bound of approx
      REAL    YB                                 ! Y lower bound of approx
      REAL    YE                                 ! Y upper bound of approx

*   local data
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Make sure variables are initialised
      WRK1_PTR = 0
      WRK1_END = 0
      WRK2_PTR = 0
      WRK2_END = 0
      IWRK_END = 0
      IWRK_PTR = 0
      DATA_PTR = 0
      DATA_END = 0
      TX_PTR   = 0
      TX_END   = 0
      TY_PTR   = 0
      TY_END   = 0
      C_PTR    = 0
      C_END    = 0
      WEIGHT_PTR = 0
      WEIGHT_END = 0


*     Calculate the weights as 1/standard deviation

      CALL SCULIB_MALLOC(VAL__NBR * NDP, WEIGHT_PTR, WEIGHT_END, STATUS)

      DO I = 1, NDP

         IF (VARIANCE_IN(I) .LT. SMALL) THEN
            WEIGHT = 1.0 / SQRT(SMALL)
         ELSE
            WEIGHT = 1.0 / SQRT(VARIANCE_IN(I))
         END IF

         CALL VEC_RTOR(.FALSE., 1, WEIGHT, %VAL(CNF_PVAL(WEIGHT_PTR) +
     :        (I-1) * VAL__NBR), IERR, NERR, STATUS)

      END DO


*     Setup the input values

      IER = 10

      KX = 3
      KY = 3
      IOPT = 0

*     Need to find bound of input data (which is specified in X_OUT and Y_OUT)

      XB = MIN(X_OUT(1), X_OUT(NX_OUT))
      XE = MAX(X_OUT(1), X_OUT(NX_OUT))
      YB = MIN(Y_OUT(1), Y_OUT(NY_OUT))
      YE = MAX(Y_OUT(1), Y_OUT(NY_OUT))

      NXEST = INT(KX + 1 + SQRT(REAL(NDP) / 2.0) + 0.5)
      NYEST = INT(KY + 1 + SQRT(REAL(NDP) / 2.0) + 0.5)

      NX = 0  ! This is returned
      NY = 0  ! This is returned

*     Calculate size of array
      NMAX = 1 + MAX(NXEST, NYEST)
      SIZE_C = (NXEST - KX - 1) * (NYEST - KY - 1)

      U = NXEST - KX - 1
      V = NYEST - KY - 1
      KM = MAX(KX, KY) + 1
      NE = MAX(NXEST, NYEST)
      BX = KX * V + KY + 1
      BY = KY * U + KY + 1
      IF (BX .LE. BY) THEN
         B1 = BX
         B2 = B1 + U - KX
      ELSE IF (BX .GT. BY) THEN
         B1 = BY
         B2 = B1 + U - KX
      END IF

      LWRK1 = U*V*(2+B1+B2) + 2*(U+V+KM*(NDP+NE)+NE-KX-KY) + B2 + 1

      LWRK2 = U*V*(B2+1) + B2

      KWRK = NDP + (NXEST - 2 * KX - 1) * (NYEST - 2 * KY - 1)

*     Allocate some work memory
      CALL SCULIB_MALLOC(VAL__NBR * NMAX, TX_PTR, TX_END, STATUS)
      CALL SCULIB_MALLOC(VAL__NBR * NMAX, TY_PTR, TY_END, STATUS)
      CALL SCULIB_MALLOC(VAL__NBR * SIZE_C, C_PTR, C_END, STATUS)
      CALL SCULIB_MALLOC(VAL__NBR * LWRK1, WRK1_PTR, WRK1_END, STATUS)
      CALL SCULIB_MALLOC(VAL__NBR * LWRK2, WRK2_PTR, WRK2_END, STATUS)
      CALL SCULIB_MALLOC(VAL__NBI * KWRK, IWRK_PTR, IWRK_END, STATUS)


*     Call the fitting routine

      CALL PDA_SURFIT(IOPT, NDP, X_IN, Y_IN, DATA_IN,
     :                %VAL(CNF_PVAL(WEIGHT_PTR)),
     :     XB, XE, YB, YE, KX, KY, S, NXEST, NYEST, NMAX, EPS, NX,
     :     %VAL(CNF_PVAL(TX_PTR)), NY, %VAL(CNF_PVAL(TY_PTR)),
     :     %VAL(CNF_PVAL(C_PTR)), FP,
     :     %VAL(CNF_PVAL(WRK1_PTR)), LWRK1, %VAL(CNF_PVAL(WRK2_PTR)),
     :     LWRK2, %VAL(CNF_PVAL(IWRK_PTR)),
     :     KWRK, IER)


      IF (IER .GT. 0) THEN
         IF (IER .EQ. 2 .OR. IER .EQ. 3) THEN
            CALL MSG_SETI('IER', IER)
            CALL MSG_OUTIF(MSG__QUIET,' ', 'SPLINE_PDA_SURFIT: '//
     :           'Warning: IER = ^IER. This suggests that SFACTOR is '//
     :           'too small.', STATUS)
         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETI('IER', IER)
            CALL ERR_REP(' ', 'SPLINE_PDA_SURFIT: Spline fit failed '//
     :           'with IER = ^IER', STATUS)
         END IF
      END IF

*     Run it again with a new S
      IF (IER .EQ. -2 .AND. STATUS .EQ. SAI__OK) THEN
         RTEMP = FP / 2.0
         IOPT = 0
         CALL PDA_SURFIT(IOPT, NDP, X_IN, Y_IN,DATA_IN,
     :                   %VAL(CNF_PVAL(WEIGHT_PTR)),
     :        XB, XE, YB, YE, KX, KY, RTEMP, NXEST, NYEST, NMAX, EPS,NX,
     :        %VAL(CNF_PVAL(TX_PTR)), NY, %VAL(CNF_PVAL(TY_PTR)),
     :        %VAL(CNF_PVAL(C_PTR)), FP,
     :        %VAL(CNF_PVAL(WRK1_PTR)), LWRK1, %VAL(CNF_PVAL(WRK2_PTR)),
     :        LWRK2,
     :        %VAL(CNF_PVAL(IWRK_PTR)), KWRK, IER)

         IF (IER .GT. 0) THEN
            IF (IER .EQ. 2 .OR. IER .EQ. 3) THEN
               CALL MSG_SETI('IER', IER)
               CALL MSG_OUTIF(MSG__QUIET, ' ','SPLINE_PDA_SURFIT: '//
     :              ' Warning IER = ^IER. This suggests that SFACTOR '//
     :              'is too small.',
     :              STATUS)
            ELSE

               STATUS = SAI__ERROR
               CALL MSG_SETI('IER', IER)
               CALL ERR_REP(' ', 'SPLINE_PDA_SURFIT: Spline fit '//
     :              'failed with IER = ^IER', STATUS)
            END IF
         END IF

      END IF

*     Some diagnostics
      CALL MSG_SETI('NX', NX)
      CALL MSG_SETI('NY', NY)
      CALL MSG_SETR('FP', FP)
      CALL MSG_SETI('IER',IER)
      CALL MSG_OUTIF(MSG__NORM,' ','SPLINE_PDA_SURFIT: There '//
     :     'are ^NX knots in X and ^NY in Y. Sum of squared '//
     :     'residuals is ^FP (IER=^IER)', STATUS)


*     Free memory that is no longer needed
      CALL SCULIB_FREE('SURFIT_WRK1', WRK1_PTR, WRK1_END, STATUS)
      CALL SCULIB_FREE('SURFIT_WRK2', WRK2_PTR, WRK2_END, STATUS)
      CALL SCULIB_FREE('SURFIT_IWRK', IWRK_PTR, IWRK_END, STATUS)
      CALL SCULIB_FREE('SURFIT_WEIGHT', WEIGHT_PTR, WEIGHT_END, STATUS)

*     Get some memory for the grid calculation
      IF (STATUS .EQ. SAI__OK) THEN

         LWRK1 = NX_OUT * (KX + 1) + NY_OUT * (KY + 1)
         KWRK = NX_OUT + NY_OUT

         WRK1_PTR = 0
         WRK1_END = 0
         IWRK_END = 0
         IWRK_PTR = 0

         CALL SCULIB_MALLOC(VAL__NBR * LWRK1, WRK1_PTR, WRK1_END,
     :        STATUS)
         CALL SCULIB_MALLOC(VAL__NBR * KWRK, IWRK_PTR, IWRK_END, STATUS)

         CALL SCULIB_MALLOC(VAL__NBR * NX_OUT * NY_OUT, DATA_PTR,
     :        DATA_END, STATUS)

*     Calculate grid

*     PDA_BISPEV has the following restrictions:
*       1) Cant handle reversed axis
*       2) Fills the ouput array in Y first
*     Therefore must flip the output data

*     Flip the  X axis (by sorting it)
         IF (X_OUT(1) .GT. X_OUT(NX_OUT)) THEN
            CALL PDA_QSAR(NX_OUT, X_OUT)
         END IF

*     Calculate the output data
         CALL PDA_BISPEV(%VAL(CNF_PVAL(TX_PTR)), NX,
     :                   %VAL(CNF_PVAL(TY_PTR)), NY,
     :        %VAL(CNF_PVAL(C_PTR)),
     :        KX, KY, X_OUT, NX_OUT, Y_OUT, NY_OUT,
     :        %VAL(CNF_PVAL(DATA_PTR)), %VAL(CNF_PVAL(WRK1_PTR)), LWRK1,
     :        %VAL(CNF_PVAL(IWRK_PTR)),
     :        KWRK, IER)

         IF (IER .NE. 0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','SPLINE_PDA_SURFIT: Error calculating '//
     :           'output grid via PDA_BISPEV', STATUS)
         END IF

*     Invert the data stream (Column major to row major)
*     Take the flipped X axis into account as well
         IF (STATUS .EQ. SAI__OK) THEN
            DO I = 1, NX_OUT
               DO J = 1, NY_OUT

                  DATA_OFFSET = (J-1) + (I-1) * NY_OUT

                  CALL VEC_RTOR(.FALSE., 1, %VAL(CNF_PVAL(DATA_PTR) +
     :                 DATA_OFFSET * VAL__NBR),
     :                 DATA_OUT(NX_OUT + 1 - I, J), IERR, NERR, STATUS)
               END DO
            END DO
         END IF

*     Free some memory
         CALL SCULIB_FREE('SURFIT_WRK1', WRK1_PTR, WRK1_END, STATUS)
         CALL SCULIB_FREE('SURFIT_IWRK', IWRK_PTR, IWRK_END, STATUS)
         CALL SCULIB_FREE('SURFIT_DATA', DATA_PTR, DATA_END, STATUS)

      END IF

*     Free some more memory
      CALL SCULIB_FREE('SURFIT_TX', TX_PTR, TX_END, STATUS)
      CALL SCULIB_FREE('SURFIT_TY', TY_PTR, TY_END, STATUS)
      CALL SCULIB_FREE('SURFIT_C', C_PTR, C_END, STATUS)

      END
