      SUBROUTINE SURFLIB_REM_TIMESERIES ( N_BOLS, N_POS,
     :     TIME_SER, TIME_VAR, IN_DATA, IN_VAR, STATUS)
*+
*  Name:
*     SURFLIB_REM_TIMESERIES

*  Purpose:
*     Remove a time series  from a 2D array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_REM_TIMESERIES ( N_BOLS, N_POS,
*    :     TIME_SER, TIME_VAR, IN_DATA, IN_VAR, STATUS)

*  Description:
*     Remove a time series (Y data) from a 2D array where the
*     second dimension corresponds to time. It is assumed that
*     both the time series and the 2D array have the same
*     number of points in time (N_POS)

*  Arguments:
*     N_BOLS = INTEGER (Given)
*       Number of bolometers in data array (x dimension)
*     N_POS = INTEGER (Given)
*       Number of time data values (y dim)
*     TIME_SER ( N_POS ) = REAL (Given)
*       Time series to be removed
*     TIME_VAR ( N_POS ) = REAL (Given)
*       Variance associated with each time value
*     IN_DATA (N_BOLS, N_POS ) = REAL (Given & Returned)
*       Input data to be modified
*     IN_VAR (N_BOLS, N_POS ) = REAL (Given & Returned)
*       Input variance
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  Notes:


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.2  1999/08/03 19:32:52  timj
*     Add copyright message to header.
*
*     Revision 1.1  1998/05/29 23:28:34  timj
*     Initial revision
*

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! For VAL__BAD

*  Arguments Given:
      INTEGER N_BOLS
      INTEGER N_POS
      REAL    TIME_SER ( N_POS )
      REAL    TIME_VAR ( N_POS )

*  Arguments Given & Returned:
      REAL    IN_DATA ( N_BOLS, N_POS )
      REAL    IN_VAR  ( N_BOLS, N_POS )

*  Global Status:
      INTEGER STATUS

*  Local variables:
      INTEGER I             ! Counter
      INTEGER J             ! Counter

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, N_POS

         DO J = 1, N_BOLS


*     If input data is bad then do nothing
            IF (IN_DATA(J,I) .NE. VAL__BADR) THEN

*     If TIME series is bad set data (and variance) to bad
               IF (TIME_SER(I) .EQ. VAL__BADR) THEN
                  IN_DATA(J,I) = VAL__BADR
                  IN_VAR(J,I) = VAL__BADR

               ELSE

                  IN_DATA(J,I) = IN_DATA(J,I) - TIME_SER(I)

               END IF

            END IF

*     If variance is bad do nothing to the variance
            IF (IN_VAR(J,I) .NE. VAL__BADR) THEN

*     If variance of time series is bad set variance to bad
               IF (TIME_VAR(I) .EQ. VAL__BADR) THEN
                  IN_VAR(J,I) = VAL__BADR
               ELSE
                  IN_VAR(J,I)  = IN_VAR(J,I) + TIME_VAR(I)
               END IF
            END IF

         END DO

      END DO

      END
