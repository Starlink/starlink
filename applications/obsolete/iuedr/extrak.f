      SUBROUTINE EXTRAK( NSUB, DSUB, QSUB, RSUB, WSUB )
*+
*  Name:
*     SUBROUTINE EXTRAK

*  Description:
*     Extract spectrum from image subset list. The object spectrum is
*     extracted from the image subset list provided.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL EXTRAK( NSUB, DSUB, QSUB, RSUB, WSUB )

*  Method:
*     The background level is smoothed using a triangle function, with
*     deviant pixels being avoided.
*     The spectrum position template is adjusted by following the
*     centroid of object signal above background.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*   History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-SEP-94 (MJC):
*       IUEDR Vn. 3.1-4
*     06-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMEXTP'

*  Arguments Given:
      INTEGER NSUB             ! number of subset pixels

*  Arguments Given and Returned:
      INTEGER*2 DSUB(NSUB)     ! DATA values

      BYTE QSUB(NSUB)          ! QUAL values

      REAL*8 RSUB(NSUB)          ! R-coordinates
      REAL*8 WSUB(NSUB)          ! W-coordinates

*  Local Variables:
      INTEGER ITER             ! iteration counter

*.

*   Zero out centroid array or use template
      CALL SECEN

*   Iterate on centroids
      ITER = 0

      DO WHILE ( .TRUE. )

*      Evaluate mean background and errors
         CALL EXBKG( NSUB, DSUB, QSUB, RSUB, WSUB )

*      Evaluate object spectrum, centroid shifts and errors.
         CALL EXOBJ( NSUB, DSUB, QSUB, RSUB, WSUB )

*      Finish after CENIT number of iterations.
         IF ( ITER .GE. CENIT ) THEN
            GO TO 100
         END IF

*      Repeat
         ITER = ITER + 1
      END DO
 100  CONTINUE

*   Save centroids
      CALL SACEN

      END
