      SUBROUTINE SCULIB_CONVOLVE (A_DATA, A_VARIANCE, A_QUALITY,
     :  B, N_A, N_B, N_MIDDLE, NORM, R_DATA, R_VARIANCE, R_QUALITY,
     :  BADBIT, STATUS)
*+
*  Name:
*     SCULIB_CONVOLVE

*  Purpose:
*     convolve array A with array B to give result in R

*  Description:
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

*  Invocation:
*     CALL SCULIB_CONVOLVE (A_DATA, A_VARIANCE, A_QUALITY, B, N_A, N_B,
*    :  N_MIDDLE, NORM, R_DATA, R_VARIANCE, R_QUALITY, STATUS)

*  Arguments:
*     A_DATA (N_A)                  = REAL (Given)
*           input data array
*     A_VARIANCE (N_A)              = REAL (Given)
*           variance on A_DATA
*     A_QUALITY (N_A)               = BYTE (Given)
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
*     R_QUALITY (N_A)               = QUALITY (Returned)
*           quality on R_DATA
*     BADBIT                        = BYTE (Given)
*           badbit mask
*     STATUS                        = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:

*  History:
*     $Id$
*     11-OCT-1995: original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER N_A
      INTEGER N_B
      REAL    A_DATA (N_A)
      REAL    A_VARIANCE (N_A)
      BYTE    A_QUALITY (N_A)
      REAL    B (N_B)
      INTEGER N_MIDDLE
      REAL    NORM
      BYTE    BADBIT

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL    R_DATA (N_A)
      REAL    R_VARIANCE (N_A)
      BYTE    R_QUALITY (N_A)

*  Status:
      INTEGER STATUS

*  External references:
      BYTE SCULIB_BITAND

*  Global variables:

*  Local Constants:

*  Local variables:
      LOGICAL DONE
      INTEGER JK                           ! DO loop index
      INTEGER JK1                          ! index in convolution array
      INTEGER KL                           ! DO loop index
      BYTE    QUAL
      REAL    SUM                          ! convolution data sum
      REAL    SUM_VAR                      ! convolution variance sum

*  Internal References:

*  Local data:
      INCLUDE 'NDF_FUNC'

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  a little whiff of NOD2 code

      DO JK = 1, N_A
         SUM = 0.0
         SUM_VAR = 0.0
         R_QUALITY (JK) = 1
         QUAL = 0
         DONE = .FALSE.

         DO KL = MIN(1,1+N_MIDDLE-JK), N_B
            JK1 = JK - N_MIDDLE + KL

            IF (JK1.LE.N_A .AND. JK1.GE.1) THEN
               IF (NDF_QMASK(A_QUALITY(JK1), BADBIT) .AND.
     :              A_DATA(JK1) .NE. VAL__BADR) THEN

                  SUM = SUM + A_DATA (JK1) * B (KL)
                  SUM_VAR = SUM_VAR + A_VARIANCE (JK1) * B (KL) **2
                  QUAL = SCULIB_BITAND(QUAL, A_QUALITY(JK1))
                  DONE = .TRUE.

                  R_QUALITY (JK) = A_QUALITY(JK1)

               END IF
            END IF
         END DO

         IF (DONE) R_QUALITY(JK) = QUAL
         R_DATA (JK) = SUM / NORM
         R_VARIANCE (JK) = SUM_VAR / NORM**2
      END DO

      END
