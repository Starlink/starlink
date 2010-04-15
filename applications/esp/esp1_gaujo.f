

      SUBROUTINE GAU1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
*+
*  Name:
*     GAU1_GAUJO

*  Purpose:
*     Inverts a matrix containing preprocessed histogram values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_GAUJO(B,A,DETERM,STATUS)

*  Description:
*     Employs the very stable Gauss-Jordan with optimised
*     array pivot elements method to invert a matrix. The matrix to be
*     inverted (INPMAT) is received in a form preprocessed.
*     On completion, the array INPMAT contains the inverted matrix and
*     the vector array VECTOR contains the coefficients of the parabolic
*     equation.
*
*     If the routine suceeds, the determinant (DETERM) of the array
*     is significantly non-zero.

*  Arguments:
*     VECTOR(3) = REAL ARRAY (Given and Returned)
*        Preprocessed count values are given. Values for the parabola
*        coefficients are returned.
*     INPMAT(3,3) = REAL (Given and Returned)
*        The matrix to be inverted. The inverted matrix is returned.
*     DETERM = REAL (Returned)
*        The determinant of the inverted array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-May-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                   ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given and Returned:
      REAL INPMAT(3,3)                ! Matrix to be inverted

*  Arguments Returned:
      REAL VECTOR(3)                  ! Results vector
      REAL DETERM                     ! The inverted matrix determinant

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER I                       ! Loop variable
      INTEGER COL                     ! Matrix column index
      INTEGER INDEX(2,3)              ! Row and column look-up table
      INTEGER ROW                     ! Matrix row index
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Number of coefficients required
      INTEGER N                       ! Size of matrix to be inverted
      LOGICAL LPIVOT(3)               ! Has column been pivoted flag
      REAL PIVOT                      ! The pivot element
      REAL TEMP                       ! Temporary variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the number of coefficients and size of the matrix to be
*   inverted. 3 given that a parabola is being considered.
      L=3
      N=3

*   Set up the initial determinant value and set the pivot flags to
*   their initial values.
      DETERM=1.0
      DO I=1,N
         LPIVOT(I)=.FALSE.
      END DO

      DO I=1,N
         PIVOT=0.0

*   Search for the pivot element.

         DO J=1,N
            IF (.NOT.LPIVOT(J)) THEN
               DO K=1,N
                  IF (.NOT.LPIVOT(K)) THEN
                     IF (ABS(PIVOT).LT.ABS(INPMAT(K,J))) THEN
                        PIVOT=INPMAT(K,J)
                        ROW=J
                        COL=K
                     END IF
                  END IF
               END DO
            END IF
         END DO

*      Calculate the determinant and exit if the value is zero ie
*      a singular matrix.
         DETERM=DETERM*PIVOT
         IF (DETERM.LT.1e-10) THEN
            DETERM=0.0
            GOTO 9999
         END IF

         LPIVOT(COL)=.TRUE.

         INDEX(1,I)=ROW
         INDEX(2,I)=COL

*   Interchange rows so that the pivot element is now on the diagonal.

         IF (ROW.NE.COL) THEN
            DETERM=-DETERM
            DO J=1,N
               TEMP=INPMAT(J,ROW)
               INPMAT(J,ROW)=INPMAT(J,COL)
               INPMAT(J,COL)=TEMP
            END DO
            TEMP=VECTOR(ROW)
            VECTOR(ROW)=VECTOR(COL)
            VECTOR(COL)=TEMP
         END IF

*   Divide the pivot row by the pivot element.

         INPMAT(COL,COL)=1.0
         DO J=1,N
            INPMAT(J,COL)=INPMAT(J,COL)/PIVOT
         END DO
         VECTOR(COL)=VECTOR(COL)/PIVOT

*   Subtract the pivot row values from the other rows.

         DO J=1,N
            IF (J.NE.COL) THEN
               TEMP=INPMAT(COL,J)
               INPMAT(COL,J)=0.0
               DO K=1,N
                  INPMAT(K,J)=INPMAT(K,J)-INPMAT(K,COL)*TEMP
               END DO
               VECTOR(J)=VECTOR(J)-VECTOR(COL)*TEMP
            END IF
         END DO
      END DO

*   Interchange the columns to recover the solution coefficients.

      DO I=N,1,-1
         IF (INDEX(1,I).NE.INDEX(2,I)) THEN
            ROW=INDEX(1,I)
            COL=INDEX(2,I)
            DO J=1,N
               TEMP=INPMAT(ROW,J)
               INPMAT(ROW,J)=INPMAT(COL,J)
               INPMAT(COL,J)=TEMP
            END DO
         END IF
      END DO

*   Exit if the parabola is up the wrong way.
      IF (VECTOR(3).GE.0.0) THEN
         DETERM=0.0
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE HIS1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
*+
*  Name:
*     HIS1_GAUJO

*  Purpose:
*     Inverts a matrix containing preprocessed histogram values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_GAUJO(B,A,DETERM,STATUS)

*  Description:
*     Employs the very stable Gauss-Jordan with optimised
*     array pivot elements method to invert a matrix. The matrix to be
*     inverted (INPMAT) is received in a form preprocessed by
*     subroutine ******. On completion, the array INPMAT contains
*     the inverted matrix and the vector array VECTOR contains the
*     coefficients of the parabolic equation.
*
*     If the routine suceeds, the determinant (DETERM) of the array
*     is significantly non-zero.

*  Arguments:
*     VECTOR(3) = REAL ARRAY (Given and Returned)
*        Preprocessed count values are given. Values for the parabola
*        coefficients are returned.
*     INPMAT(3,3) = REAL (Given and Returned)
*        The matrix to be inverted. The inverted matrix is returned.
*     DETERM = REAL (Returned)
*        The determinant of the inverted array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-June-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                   ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'HIS_PAR'               ! HISTPEAK system variables

*  Arguments Given and Returned:
      REAL INPMAT(3,3)                ! Matrix to be inverted

*  Arguments Returned:
      REAL VECTOR(3)                  ! Results vector
      REAL DETERM                     ! The inverted matrix determinant

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER I                       ! Loop variable
      INTEGER COL                     ! Matrix column index
      INTEGER INDEX(2,3)              ! Row and column look-up table
      INTEGER ROW                     ! Matrix row index
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Number of coefficients required
      INTEGER N                       ! Size of matrix to be inverted
      LOGICAL LPIVOT(3)               ! Has column been pivoted flag
      REAL PIVOT                      ! The pivot element
      REAL TEMP                       ! Temporary variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the number of coefficients and size of the matrix to be
*   inverted. 3 given that a parabola is being considered.
      L=3
      N=3

*   Set up the initial determinant value and set the pivot flags to
*   their initial values.
      DETERM=1.0
      DO I=1,N
         LPIVOT(I)=.FALSE.
      END DO

      DO I=1,N
         PIVOT=0.0

*   Search for the pivot element.

         DO J=1,N
            IF (.NOT.LPIVOT(J)) THEN
               DO K=1,N
                  IF (.NOT.LPIVOT(K)) THEN
                     IF (ABS(PIVOT).LT.ABS(INPMAT(K,J))) THEN
                        PIVOT=INPMAT(K,J)
                        ROW=J
                        COL=K
                     END IF
                  END IF
               END DO
            END IF
         END DO

*      Calculate the determinant and exit if the value is zero ie
*      a singular matrix.
         DETERM=DETERM*PIVOT
         IF (DETERM.LT.HIS__VSMAL) THEN
            DETERM=0.0
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','Unable to complete the parabolic '//
     :                       'interpolation.',STATUS)
            GOTO 9999
         END IF

         LPIVOT(COL)=.TRUE.

         INDEX(1,I)=ROW
         INDEX(2,I)=COL

*   Interchange rows so that the pivot element is now on the diagonal.

         IF (ROW.NE.COL) THEN
            DETERM=-DETERM
            DO J=1,N
               TEMP=INPMAT(J,ROW)
               INPMAT(J,ROW)=INPMAT(J,COL)
               INPMAT(J,COL)=TEMP
            END DO
            TEMP=VECTOR(ROW)
            VECTOR(ROW)=VECTOR(COL)
            VECTOR(COL)=TEMP
         END IF

*   Divide the pivot row by the pivot element.

         INPMAT(COL,COL)=1.0
         DO J=1,N
            INPMAT(J,COL)=INPMAT(J,COL)/PIVOT
         END DO
         VECTOR(COL)=VECTOR(COL)/PIVOT

*   Subtract the pivot row values from the other rows.

         DO J=1,N
            IF (J.NE.COL) THEN
               TEMP=INPMAT(COL,J)
               INPMAT(COL,J)=0.0
               DO K=1,N
                  INPMAT(K,J)=INPMAT(K,J)-INPMAT(K,COL)*TEMP
               END DO
               VECTOR(J)=VECTOR(J)-VECTOR(COL)*TEMP
            END IF
         END DO
      END DO

*   Interchange the columns to recover the solution coefficients.

      DO I=N,1,-1
         IF (INDEX(1,I).NE.INDEX(2,I)) THEN
            ROW=INDEX(1,I)
            COL=INDEX(2,I)
            DO J=1,N
               TEMP=INPMAT(ROW,J)
               INPMAT(ROW,J)=INPMAT(COL,J)
               INPMAT(COL,J)=TEMP
            END DO
         END IF
      END DO

*   Exit if the parabola is up the wrong way.
      IF (VECTOR(3).GE.0.0) THEN
         DETERM=0.0
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Unable to complete the parabolic '//
     :                    'interpolation.',STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE HSB1_GAUJO(INPMAT,STATUS,VECTOR,DETERM)
*+
*  Name:
*     HSB1_GAUJO

*  Purpose:
*     Inverts a matrix containing preprocessed histogram values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_GAUJO(A,STATUS,B,DETERM)

*  Description:
*     Employs the very stable Gauss-Jordan with optimised
*     array pivot elements method to invert a matrix. The matrix to be
*     inverted (INPMAT) is received in a form preprocessed.
*
*     On completion, the array INPMAT contains
*     the inverted matrix and the vector array VECTOR contains the
*     coefficients of the parabolic equation.
*
*     If the routine suceeds, the determinant (DETERM) of the array
*     is significantly non-zero.

*  Arguments:
*     INPMAT(3,3) = REAL (Given and Returned)
*        The matrix to be inverted. The inverted matrix is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     VECTOR(3) = REAL ARRAY (Given and Returned)
*        Preprocessed count values are given. Values for the parabola
*        coefficients are returned.
*     DETERM = REAL (Returned)
*        The determinant of the inverted array.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-June-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                   ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'HSB_PAR'               ! HSUB system variables

*  Arguments Given and Returned:
      REAL INPMAT(3,3)                ! Matrix to be inverted

*  Arguments Returned:
      REAL VECTOR(3)                  ! Results vector
      REAL DETERM                     ! The inverted matrix determinant

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER I                       ! Loop variable
      INTEGER COL                     ! Matrix column index
      INTEGER INDEX(2,3)              ! Row and column look-up table
      INTEGER ROW                     ! Matrix row index
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Number of coefficients required
      INTEGER N                       ! Size of matrix to be inverted
      LOGICAL LPIVOT(3)               ! Has column been pivoted flag
      REAL PIVOT                      ! The pivot element
      REAL TEMP                       ! Temporary variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the number of coefficients and size of the matrix to be
*   inverted. 3 given that a parabola is being considered.
      L=3
      N=3

*   Set up the initial determinant value and set the pivot flags to
*   their initial values.
      DETERM=1.0
      DO I=1,N
         LPIVOT(I)=.FALSE.
      END DO

      DO I=1,N
         PIVOT=0.0

*   Search for the pivot element.

         DO J=1,N
            IF (.NOT.LPIVOT(J)) THEN
               DO K=1,N
                  IF (.NOT.LPIVOT(K)) THEN
                     IF (ABS(PIVOT).LT.ABS(INPMAT(K,J))) THEN
                        PIVOT=INPMAT(K,J)
                        ROW=J
                        COL=K
                     END IF
                  END IF
               END DO
            END IF
         END DO

*      Calculate the determinant and exit if the value is zero ie
*      a singular matrix.
         DETERM=DETERM*PIVOT
         IF (DETERM.LT.HSB__VSMAL) THEN
            DETERM=0.0
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','Unable to complete the parabolic '//
     :                       'interpolation.',STATUS)
            GOTO 9999
         END IF

         LPIVOT(COL)=.TRUE.

         INDEX(1,I)=ROW
         INDEX(2,I)=COL

*   Interchange rows so that the pivot element is now on the diagonal.

         IF (ROW.NE.COL) THEN
            DETERM=-DETERM
            DO J=1,N
               TEMP=INPMAT(J,ROW)
               INPMAT(J,ROW)=INPMAT(J,COL)
               INPMAT(J,COL)=TEMP
            END DO
            TEMP=VECTOR(ROW)
            VECTOR(ROW)=VECTOR(COL)
            VECTOR(COL)=TEMP
         END IF

*   Divide the pivot row by the pivot element.

         INPMAT(COL,COL)=1.0
         DO J=1,N
            INPMAT(J,COL)=INPMAT(J,COL)/PIVOT
         END DO
         VECTOR(COL)=VECTOR(COL)/PIVOT

*   Subtract the pivot row values from the other rows.

         DO J=1,N
            IF (J.NE.COL) THEN
               TEMP=INPMAT(COL,J)
               INPMAT(COL,J)=0.0
               DO K=1,N
                  INPMAT(K,J)=INPMAT(K,J)-INPMAT(K,COL)*TEMP
               END DO
               VECTOR(J)=VECTOR(J)-VECTOR(COL)*TEMP
            END IF
         END DO
      END DO

*   Interchange the columns to recover the solution coefficients.

      DO I=N,1,-1
         IF (INDEX(1,I).NE.INDEX(2,I)) THEN
            ROW=INDEX(1,I)
            COL=INDEX(2,I)
            DO J=1,N
               TEMP=INPMAT(ROW,J)
               INPMAT(ROW,J)=INPMAT(COL,J)
               INPMAT(COL,J)=TEMP
            END DO
         END IF
      END DO

*   Exit if the parabola is up the wrong way.
      IF (VECTOR(3).GE.0.0) THEN
         DETERM=0.0
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Unable to complete the parabolic '//
     :                    'interpolation.',STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE LOB1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
*+
*  Name:
*     LOB1_GAUJO

*  Purpose:
*     Inverts a matrix containing preprocessed histogram values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_GAUJO(B,A,DETERM,STATUS)

*  Description:
*     Employs the very stable Gauss-Jordan with optimised
*     array pivot elements method to invert a matrix. The matrix to be
*     inverted (INPMAT) is received in a form partially preprocessed.
*
*     On completion, the array INPMAT contains the inverted matrix and
*     the vector array VECTOR contains the coefficients of the
*     parabolic equation.
*
*     If the routine suceeds, the determinant (DETERM) of the array
*     is significantly non-zero.

*  Arguments:
*     DETERM = REAL (Returned)
*        The determinant of the inverted array.
*     INPMAT(3,3) = REAL (Given and Returned)
*        The matrix to be inverted. The inverted matrix is returned.
*     VECTOR(3) = REAL ARRAY (Given and Returned)
*        Preprocessed count values are given. Values for the parabola
*        coefficients are returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-June-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                   ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'LOB_PAR'               ! LOBACK system variables

*  Arguments Given and Returned:
      REAL INPMAT(3,3)                ! Matrix to be inverted

*  Arguments Returned:
      REAL VECTOR(3)                  ! Results vector
      REAL DETERM                     ! The inverted matrix determinant

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER I                       ! Loop variable
      INTEGER COL                     ! Matrix column index
      INTEGER INDEX(2,3)              ! Row and column look-up table
      INTEGER ROW                     ! Matrix row index
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Number of coefficients required
      INTEGER N                       ! Size of matrix to be inverted
      LOGICAL LPIVOT(3)               ! Has column been pivoted flag
      REAL PIVOT                      ! The pivot element
      REAL TEMP                       ! Temporary variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the number of coefficients and size of the matrix to be
*   inverted. 3 given that a parabola is being considered.
      L=3
      N=3

*   Set up the initial determinant value and set the pivot flags to
*   their initial values.
      DETERM=1.0
      DO I=1,N
         LPIVOT(I)=.FALSE.
      END DO

      DO I=1,N
         PIVOT=0.0

*   Search for the pivot element.

         DO J=1,N
            IF (.NOT.LPIVOT(J)) THEN
               DO K=1,N
                  IF (.NOT.LPIVOT(K)) THEN
                     IF (ABS(PIVOT).LT.ABS(INPMAT(K,J))) THEN
                        PIVOT=INPMAT(K,J)
                        ROW=J
                        COL=K
                     END IF
                  END IF
               END DO
            END IF
         END DO

*      Calculate the determinant and exit if the value is zero ie
*      a singular matrix.
         DETERM=DETERM*PIVOT
         IF (DETERM.LT.LOB__VSMAL) THEN
            DETERM=0.0
            CALL MSG_OUT(' ','WARNINGdir!!!',STATUS)
            CALL MSG_OUT(' ','Unable to complete the parabolic '//
     :                       'interpolation.',STATUS)
            GOTO 9999
         END IF

         LPIVOT(COL)=.TRUE.

         INDEX(1,I)=ROW
         INDEX(2,I)=COL

*   Interchange rows so that the pivot element is now on the diagonal.

         IF (ROW.NE.COL) THEN
            DETERM=-DETERM
            DO J=1,N
               TEMP=INPMAT(J,ROW)
               INPMAT(J,ROW)=INPMAT(J,COL)
               INPMAT(J,COL)=TEMP
            END DO
            TEMP=VECTOR(ROW)
            VECTOR(ROW)=VECTOR(COL)
            VECTOR(COL)=TEMP
         END IF

*   Divide the pivot row by the pivot element.

         INPMAT(COL,COL)=1.0
         DO J=1,N
            INPMAT(J,COL)=INPMAT(J,COL)/PIVOT
         END DO
         VECTOR(COL)=VECTOR(COL)/PIVOT

*   Subtract the pivot row values from the other rows.

         DO J=1,N
            IF (J.NE.COL) THEN
               TEMP=INPMAT(COL,J)
               INPMAT(COL,J)=0.0
               DO K=1,N
                  INPMAT(K,J)=INPMAT(K,J)-INPMAT(K,COL)*TEMP
               END DO
               VECTOR(J)=VECTOR(J)-VECTOR(COL)*TEMP
            END IF
         END DO
      END DO

*   Interchange the columns to recover the solution coefficients.

      DO I=N,1,-1
         IF (INDEX(1,I).NE.INDEX(2,I)) THEN
            ROW=INDEX(1,I)
            COL=INDEX(2,I)
            DO J=1,N
               TEMP=INPMAT(ROW,J)
               INPMAT(ROW,J)=INPMAT(COL,J)
               INPMAT(COL,J)=TEMP
            END DO
         END IF
      END DO

*   Exit if the parabola is up the wrong way.
      IF (VECTOR(3).GE.0.0) THEN
         DETERM=0.0
         GOTO 9999
      END IF

 9999 CONTINUE

      END
