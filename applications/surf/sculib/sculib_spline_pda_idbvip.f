      SUBROUTINE SCULIB_SPLINE_PDA_IDBVIP (NDP, X_IN, Y_IN, DATA_IN,
     :  N_PTS, X_OUT, Y_OUT, DATA_OUT, STATUS)

*+
*  Name:
*     SCULIB_SPLINE_PDA_IDBVIP

*  Purpose:
*     Fit a surface to an irregular grid using the PDA function PDA_IDBVIP

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE SCULIB_SPLINE_PDA_IDBVIP (NDP, X_IN, Y_IN, DATA_IN,
*    :  N_PTS, X_OUT, Y_OUT, DATA_OUT, STATUS)

*  Description:
*     This routine provides a wrapper for the PDA_IDBVIP spline interpolation
*     routine. The quirks of the algorithm are dealt with here so that
*     SCULIB_SPLINE_REGRID does not have to know anything about the interpolation
*     routine.

*  Arguments:
*     NDP = INTEGER (Given)
*       Number of data points on irregular grid
*     X_IN ( NDP ) = REAL (Given)
*       X coordinates of input data
*     Y_IN ( NDP ) = REAL (Given)
*       Y coordinates of input data
*     DATA_IN ( NDP ) = REAL (Given)
*       Data values for each X,Y
*     N_PTS = INTEGER (Given)
*       Number of points in output grid
*     X_OUT ( N_PTS ) = REAL (Given)
*       X coordinates of output points
*     Y_OUT ( N_PTS ) = REAL (Given)
*       Y coordinates of output points
*     DATA_OUT ( N_PTS ) = REAL (Returned)
*       Output data values
*     STATUS = INTEGER (Given & Returned)
*       Global status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  History:
*     17 April 1997 (TimJ)
*        Original version

*  Bugs:
*     PDA_IDBVIP can only deal with INCR output points at any one time and
*     a loop is required in this subroutine to process sets containing 
*     more points.

*-
      
*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! VAL__ constants

*  Arguments Given:
      REAL    DATA_IN ( NDP )
      INTEGER NDP
      INTEGER N_PTS
      REAL    X_IN ( NDP )
      REAL    X_OUT( N_PTS )
      REAL    Y_IN ( NDP )
      REAL    Y_OUT( N_PTS )

*  Arguments Returned:
      REAL    DATA_OUT ( N_PTS )

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:

*  Local Constants:
      INTEGER INCR                               ! Number of points that can
      PARAMETER ( INCR = 700 )                   ! be processed by PDA_IDBVIP
      INTEGER NCP                                ! Number of points used
      PARAMETER ( NCP = 5 )                      ! by spline fit for gradients

*  Local Variables:
      INTEGER DATA_OFFSET                        ! Loop counter
      INTEGER ISTAT                              ! Spline fit status
      INTEGER ITEMP                              ! Scratch int
      INTEGER IWK_END                            ! Pointer to end of IWK
      INTEGER IWK_PTR                            ! Integer scratch array
      INTEGER MODE                               ! Mode of fit
      INTEGER NOP                                ! Number of output points
      REAL    RTEMP                              ! Dummy real
      INTEGER WK_END                             ! Pointer to End of WK
      INTEGER WK_PTR                             ! Pointer to REAL scratch array

      
*   local data
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Allocate the scratch memory

      ITEMP = MAX(31, 27 + NCP)
      CALL SCULIB_MALLOC(VAL__NBI * ITEMP * NDP + N_PTS, IWK_PTR, 
     :     IWK_END, STATUS)
      CALL SCULIB_MALLOC(VAL__NBR * 8 * NDP, WK_PTR, WK_END, STATUS)

*     Setup the status

      ISTAT = 0

*     First do the interpolation to setup the spline by calculating
*     for only one input point

      MODE = 1
      NOP = 1

      CALL PDA_IDBVIP(MODE, NCP, NDP, X_IN, Y_IN, DATA_IN,
     :     NOP, X_IN(1), Y_IN(1), RTEMP, %VAL(IWK_PTR), %VAL(WK_PTR), 
     :     ISTAT, STATUS)

      IF (STATUS .NE. SAI__OK .OR. ISTAT .NE. 0) THEN
         CALL MSG_SETI('ISTAT', ISTAT)
         CALL ERR_REP(' ', 'SPLINE_PDA_IDBVIP: Spline setup failed '//
     :        'with ISTAT = ^ISTAT', STATUS)
      END IF

*     Now that the spline has been calculated retrieve all the data
*     for the output grid using MODE 2. Note that for some reason I can 
*     only retrieve approximately 700 data points from PDA_IDBVIP with 
*     each call.

      MODE = 2
      DATA_OFFSET = 1

      DO WHILE ((N_PTS - DATA_OFFSET .GT. 0) .AND. STATUS .EQ. SAI__OK) 

         IF (N_PTS - DATA_OFFSET .LT. INCR) THEN
            NOP = N_PTS - DATA_OFFSET
         ELSE
            NOP = INCR
         END IF

         CALL PDA_IDBVIP(MODE, NCP, NDP, X_IN, Y_IN, DATA_IN, NOP,
     :        X_OUT(DATA_OFFSET), Y_OUT(DATA_OFFSET),
     :        DATA_OUT(DATA_OFFSET), %VAL(IWK_PTR), %VAL(WK_PTR), 
     :        ISTAT, STATUS)

         IF (ISTAT .NE. 0 .OR. STATUS .NE. SAI__OK) THEN
            CALL MSG_SETI('ISTAT', ISTAT)
            CALL MSG_SETI('OFFSET', DATA_OFFSET)
            CALL MSG_SETI('END',DATA_OFFSET + NOP)
            CALL ERR_REP(' ','SPLINE_PDA_IDBVIP: Spline interpolation'//
     :           ' with ISTAT = ^ISTAT (whilst processing points '//
     :           ' ^OFFSET to ^END', STATUS)
         END IF

         DATA_OFFSET = DATA_OFFSET + INCR
      END DO

*     Tidy up memory

      CALL SCULIB_FREE ('SPLINE_IWK', IWK_PTR, IWK_END, STATUS)
      CALL SCULIB_FREE ('SPLINE_WK', WK_PTR, WK_END, STATUS)
      

      END
