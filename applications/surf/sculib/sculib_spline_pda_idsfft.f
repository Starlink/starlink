      SUBROUTINE SCULIB_SPLINE_PDA_IDSFFT (NDP, X_IN, Y_IN, DATA_IN,
     :  NX_OUT, NY_OUT, X_OUT, Y_OUT, DATA_OUT, STATUS)

*+
*  Name:
*     SCULIB_SPLINE_PDA_IDSFFT

*  Purpose:
*     Fit a surface to an irregular grid using the PDA function PDA_IDSFFT

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_SPLINE_PDA_IDSFFT (NDP, X_IN, Y_IN, DATA_IN,
*    :  NX_OUT, NY_OUT, X_OUT, Y_OUT, DATA_OUT, STATUS)

*  Description:
*     This routine provides a wrapper for the PDA_IDSFFT spline interpolation
*     routine. The quirks of the algorithm are dealt with here so that
*     SCULIB_SPLINE_REGRID does not have to know anything about the
*     interpolation routine.

*  Arguments:
*     NDP = INTEGER (Given)
*       Number of data points on irregular grid
*     X_IN ( NDP ) = REAL (Given)
*       X coordinates of input data
*     Y_IN ( NDP ) = REAL (Given)
*       Y coordinates of input data
*     DATA_IN ( NDP ) = REAL (Given)
*       Data values for each X,Y
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
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! VAL__ constants
      INCLUDE 'CNF_PAR'                          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDP
      REAL    DATA_IN ( NDP )
      INTEGER NX_OUT
      INTEGER NY_OUT
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
      INTEGER NCP                                ! Number of points used
      PARAMETER ( NCP = 5 )                      ! by spline fit for gradients

*  Local Variables:
      INTEGER ISTAT                              ! Spline fit status
      INTEGER ITEMP                              ! Scratch int
      INTEGER IWK_END                            ! Pointer to end of IWK
      INTEGER IWK_PTR                            ! Integer scratch array
      INTEGER MODE                               ! Mode of fit
      REAL    RTEMP                              ! Dummy real
      INTEGER WK_END                             ! Pointer to End of WK
      INTEGER WK_PTR                             ! Pointer to REAL scratch array


*   local data
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     initialise pointers
      WK_END = 0
      WK_PTR = 0
      IWK_PTR = 0
      IWK_END = 0

*     Allocate the scratch memory

      ITEMP = MAX(31, 27 + NCP)
      CALL SCULIB_MALLOC(VAL__NBI * (ITEMP * NDP + NX_OUT * NY_OUT),
     :     IWK_PTR, IWK_END, STATUS)
      CALL SCULIB_MALLOC(VAL__NBR * 5 * NDP, WK_PTR, WK_END, STATUS)

*     Setup the status

      ISTAT = 0

*     First do the interpolation to setup the spline by calculating
*     for only one input point

      MODE = 1

      IF (STATUS .EQ. SAI__OK) THEN

         CALL PDA_IDSFFT(MODE, NCP, NDP, X_IN, Y_IN, DATA_IN,
     :        1,1, X_IN(1), Y_IN(1), RTEMP, %VAL(CNF_PVAL(IWK_PTR)),
     :        %VAL(CNF_PVAL(WK_PTR)),
     :        ISTAT, STATUS)

         IF (STATUS .NE. SAI__OK .OR. ISTAT .NE. 0) THEN
            CALL MSG_SETI('ISTAT', ISTAT)
            CALL ERR_REP(' ', 'SPLINE_PDA_IDSFFT: Spline setup '//
     :           'failed with ISTAT = ^ISTAT', STATUS)
         ELSE

*     Now that the spline has been calculated retrieve all the data
*     for the output grid using MODE 2.

            MODE = 2

            CALL PDA_IDSFFT(MODE, NCP, NDP, X_IN, Y_IN, DATA_IN, NX_OUT,
     :           NY_OUT, X_OUT, Y_OUT, DATA_OUT,
     :           %VAL(CNF_PVAL(IWK_PTR)),
     :           %VAL(CNF_PVAL(WK_PTR)), ISTAT, STATUS)

            IF (ISTAT .NE. 0 .OR. STATUS .NE. SAI__OK) THEN
               CALL MSG_SETI('ISTAT', ISTAT)
               CALL ERR_REP(' ','SPLINE_PDA_IDSFFT: Spline '//
     :              'interpolation failed with ISTAT = ^ISTAT', STATUS)
            END IF
         END IF
      END IF

*     Tidy up memory

      CALL SCULIB_FREE ('SPLINE_IWK', IWK_PTR, IWK_END, STATUS)
      CALL SCULIB_FREE ('SPLINE_WK', WK_PTR, WK_END, STATUS)


      END
