*+  SCULIB_CONVOLVE - convolve array A with array B to give result in R
      SUBROUTINE SCULIB_CONVOLVE (A_DATA, A_VARIANCE, A_QUALITY,
     :  B, N_A, N_B, N_MIDDLE, NORM, R_DATA, R_VARIANCE, R_QUALITY,
     :  STATUS)
*    Description :
*     This routine convolves array A with array B using multiplication
*     only (i.e. no FFTs are used). The routine assumes that the A arrays
*     are N_A long and that the convolution array B is N_B long, centred
*     at N_MIDDLE. The final result is normalised by dividing by NORM.
*     Elements of the A array that have bad quality are ignored. The input
*     variances are propagated as:-
*
*     R_VARIANCE = sum (B**2 * A_VARIANCE) / NORM **2
*
*     After this process the errors can no longer be considered independent.
*
*    Invocation :
*     CALL SCULIB_CONVOLVE (A_DATA, A_VARIANCE, A_QUALITY, B, N_A, N_B,
*    :  N_MIDDLE, NORM, R_DATA, R_VARIANCE, R_QUALITY, STATUS)
*    Parameters :
*     A_DATA (N_A)                  = REAL (Given)
*           input data array
*     A_VARIANCE (N_A)              = REAL (Given)
*           variance on A_DATA
*     A_QUALITY (N_A)               = INTEGER (Given)
*           quality on A_DATA
*     B (N_B)                       = REAL (Given)
*           convolution function
*     N_A                           = INTEGER (Given)
*           the size of A_DATA
*     N_B                           = INTEGER (Given)
*           length of convolution function
*     N_MIDDLE                      = INTEGER (Given)
*           index of zero point of convolution function
*     NORM                          = REAL (Given)
*           normalisation factor for convolution function
*     R_DATA (N_A)                  = REAL (Returned)
*           result of the convolution
*     R_VARIANCE (N_A)              = REAL (Returned)
*           variance on R_DATA
*     R_QUALITY (N_A)               = INTEGER (Returned)
*           quality on R_DATA
*     STATUS                        = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (jfl@roe.ac.uk)
*    History :
*     $Id$
*     11-OCT-1995: original version
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      REAL    A_DATA (N_A)
      REAL    A_VARIANCE (N_A)
      INTEGER A_QUALITY (N_A)
      REAL    B (N_B)
      INTEGER N_A
      INTEGER N_B
      INTEGER N_MIDDLE
      REAL    NORM
*    Import-Export :
*    Export :
      REAL    R_DATA (N_A)
      REAL    R_VARIANCE (N_A)
      INTEGER R_QUALITY (N_A)
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER JK                           ! DO loop index
      INTEGER JK1                          ! index in convolution array
      INTEGER KL                           ! DO loop index
      REAL    SUM                          ! convolution data sum
      REAL    SUM_VAR                      ! convolution variance sum
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*  a little whiff of NOD2 code

      DO JK = 1, N_A
         SUM = 0.0
         SUM_VAR = 0.0
         R_QUALITY (JK) = 1

         DO KL = MIN(1,1+N_MIDDLE-JK), N_B
            JK1 = JK - N_MIDDLE + KL

            IF (JK1.LE.N_A .AND. JK1.GE.1) THEN
               IF (A_QUALITY(JK1) .EQ. 0) THEN
                  SUM = SUM + A_DATA (JK1) * B (KL)
                  SUM_VAR = SUM_VAR + A_VARIANCE (JK1) * B (KL) **2
                  R_QUALITY (JK) = 0
               END IF
            END IF
         END DO

         R_DATA (JK) = SUM / NORM
         R_VARIANCE (JK) = SUM_VAR / NORM**2
      END DO

      END
