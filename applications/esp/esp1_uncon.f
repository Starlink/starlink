


      SUBROUTINE GRA1_UNCON(RADISP,LOWR,HIGHR,STATUS)
*+
*  Name:
*     GRA1_UNCON

*  Purpose:
*     Takes the value returned by the cursor for the radius required and
*     converts it to linear radius.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRA1_UNCON(RADISP,LOWR,HIGHR,STATUS)

*  Description:
*     Take the values chosen by the user (using a cursor) from the graph
*     plot of raw data and converts it from the transform value i.e. log,
*     squared or quarter power to linear values.
*
*     The value is modified to ignore values for radius beyond the left hand
*     edge of the plot.

*  Arguments:
*     RADISP = CHAR (Given)
*        Character variable denoting the format used for transforming
*        the radius value on the plotted graph.
*        R=Linear Q=Quarter power, L=Logarithmic and S=Squared.
*     LOWR = REAL (Given and Returned)
*        The low value for transformed radius derived from the graph using
*        a cursor.
*     HIGHR = REAL (Given and Returned)
*        The high value for transformed radius derived from the graph using
*        a cursor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     23-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      CHARACTER RADISP *(256)         ! Defines the type of transformation
                                      ! to be applied to the radius value

*  Arguments Given and Returned:
      REAL LOWR                       ! Low transfromed radius value to
                                      ! be untransformed
      REAL HIGHR                      ! High transformed radius value to
                                      ! be untransformed

*  Status:
      INTEGER STATUS                  ! Global status

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Transformation not required.
      IF (RADISP.EQ.'R') THEN
         IF (LOWR.LT.0.0) LOWR=0.0
         IF (HIGHR.LT.0.0) HIGHR=0.0
      END IF

*   Untransform data from a squared plot.
      IF (RADISP.EQ.'S') THEN
         IF (LOWR.LT.0.0) LOWR=0.0
         IF (HIGHR.LT.0.0) HIGHR=0.0
         LOWR=LOWR**(0.5)
         HIGHR=HIGHR**(0.5)
      END IF

*   Untransform data from a log base 10 plot.
      IF (RADISP.EQ.'L') THEN
         LOWR=10.**(LOWR)
         HIGHR=10.**(HIGHR)
      END IF

*   Untransform data from a quarter power plot.
      IF (RADISP.EQ.'Q') THEN
         IF (LOWR.LT.0.0) LOWR=0.0
         IF (HIGHR.LT.0.0) HIGHR=0.0
         LOWR=LOWR**(4.0)
         HIGHR=HIGHR**(4.0)
      END IF

 9999 CONTINUE

      END


      SUBROUTINE SEC1_UNCON(RADISP,LOWR,HIGHR,STATUS)
*+
*  Name:
*     SEC1_UNCON

*  Purpose:
*     Takes the value returned by the cursor for the radius required and
*     converts it to linear radius.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_UNCON(RADISP,LOWR,HIGHR,STATUS)

*  Description:
*     Take the values chosen by the user (using a cursor) from the graph
*     plot of raw data and converts it from the transform value i.e. log,
*     squared or quarter power to linear values.
*
*     The value is modified to ignore values for radius beyond the left hand
*     edge of the plot.

*  Arguments:
*     RADISP = CHAR (Given)
*        Character variable denoting the format used for transforming
*        the radius value on the plotted graph.
*        R=Linear Q=Quarter power, L=Logarithmic and S=Squared.
*     LOWR = REAL (Given and Returned)
*        The low value for transformed radius derived from the graph using
*        a cursor.
*     HIGHR = REAL (Given and Returned)
*        The high value for transformed radius derived from the graph using
*        a cursor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     23-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      CHARACTER RADISP *(256)         ! Defines the type of transformation
                                      ! to be applied to the radius value

*  Arguments Given and Returned:
      REAL LOWR                       ! Low transfromed radius value to
                                      ! be untransformed
      REAL HIGHR                      ! High transformed radius value to
                                      ! be untransformed

*  Status:
      INTEGER STATUS                  ! Global status

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Transformation not required.
      IF (RADISP.EQ.'R') THEN
         IF (LOWR.LT.0.0) LOWR=0.0
         IF (HIGHR.LT.0.0) HIGHR=0.0
      END IF

*   Untransform data from a squared plot.
      IF (RADISP.EQ.'S') THEN
         IF (LOWR.LT.0.0) LOWR=0.0
         IF (HIGHR.LT.0.0) HIGHR=0.0
         LOWR=LOWR**(0.5)
         HIGHR=HIGHR**(0.5)
      END IF

*   Untransform data from a log base 10 plot.
      IF (RADISP.EQ.'L') THEN
         LOWR=10.**(LOWR)
         HIGHR=10.**(HIGHR)
      END IF

*   Untransform data from a quarter power plot.
      IF (RADISP.EQ.'Q') THEN
         IF (LOWR.LT.0.0) LOWR=0.0
         IF (HIGHR.LT.0.0) HIGHR=0.0
         LOWR=LOWR**(4.0)
         HIGHR=HIGHR**(4.0)
      END IF

 9999 CONTINUE

      END
