      SUBROUTINE grf_DRQUAL( N, X, Y, Q )

*+
*
*   Name:
*      SUBROUTINE grf_DRQUAL
*
*   Description:
*      Draw data quality as symbols.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings     01-MAY-82
*         AT4 version.
*      Paul Rees         14-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         09-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*      Martin Clayton    08-JUL-94     IUEDR Vn. 3.1-1
*
*   Method:
*      Display the Data Quality in the form of selected graphic symbols.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Local constants:
      INTEGER ICEN        ! centring of symbol
      INTEGER IORI        ! orientation of symbol
      INTEGER ISIZ        ! size of symbol
      INTEGER LENGTH      ! number of characters to be drawn

*   MJC changed ISIZ to make marks smaller
      PARAMETER (ICEN = 0, IORI = 0, ISIZ = 18, LENGTH = 1)

*   Import:
      INTEGER N           ! number of points

      REAL X(N)           ! X-axis data
      REAL Y(N)           ! Y-axis data

      INTEGER Q(N)        ! data quality

*   External references:
      INTEGER dq_AND      ! data quality AND

*   Local variables:
      INTEGER I           ! loop index
      INTEGER QSEV        ! data quality severity (bits 5-8)

      CHARACTER MARK      ! marker character
      CHARACTER MSEV(7)   ! marker characters

*   Local data:
      DATA MSEV / '1', '2', '3', '4', '5', '6', '7' /

*   Loop to sample data quality flags, writing to display when necessary
      DO I = 1, N
         IF ( Q(I) .NE. 0 ) THEN

*         Write data quality flag to display
            IF ( dq_AND(Q(I), 1) .EQ. 0 ) THEN
               CALL dq_RDPK( Q(I), 5, 4, QSEV )
               IF ( QSEV .GT. 0 ) THEN
                  IF ( QSEV .LE. 7 ) THEN
                     MARK = MSEV(QSEV)
                  ELSE
                     MARK = '?'
                  END IF
               ELSE IF ( dq_AND(Q(I), 2) .NE. 0 ) THEN
                  MARK = 'U'
               ELSE
                  MARK = '?'
               END IF
               CALL AGPWRT( X(I), Y(I), MARK, LENGTH, ISIZ, IORI,
     :                      ICEN )
            END IF
         END IF
      END DO
      END
