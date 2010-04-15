      PROGRAM SF2DTEST2

*+
*  Name:
*     SF2DTEST2

*  Purpose:
*   This program is a test  routine for a modified form of
*   the TOM526 subprogram packages PDA_IDBVIP and PDA_IDSFFT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SF2DTEST2

*  Description:
*   This program is a test  routine for a modified form of
*   the PAD_IDBVIP and PDA_IDSFFT subroutines.

*   The TOMS routines have been modified to ensure error
*   messages are not generated during execution. The value ISTAT
*   returns a value that must be interpreted.


*   PDA_IDBVIP - Summary

*   This subroutine performs bivariate interpolation when the
*   projections of the data points in the x-y plane are irregularly
*   distributed in the plane.

*   PDA_IDSFFT - Summary

*   This subroutine performs smooth surface fitting when the
*   projections of the data points in the x-y plane are irregularly
*   distributed in the plane.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     18-Jan-1996 (GJP):
*     (Original version)
*     24-FEB-1997 (DSB):
*        Changed from an ATASK to a stand-alone program.
*     22-MAR-2005 (TIMJ):
*        Initialise the TOTAL and write it out.
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*   Status:
      INTEGER  STATUS

*  Local Variables:
      INTEGER  ISTAT
      INTEGER  IWK(2500)
      INTEGER  NCP
      INTEGER  NDP
      INTEGER  NXI,NYI
      INTEGER  NX,NY
      INTEGER  MD
      INTEGER  N
      INTEGER  NOP
      INTEGER  X,Y

      REAL     TOTAL
      REAL     WK(640)
      REAL     XD(80),YD(80),ZD(80)
      REAL     XI(1),YI(1)
      REAL     ZI(1,1),ZI2(1,1)
      REAL     XINC,YINC
*.

*   Set the error flag default value.
      ISTAT=0
      STATUS = 0

*   Call the PDA_IDBVIP subroutine. First call should be in mode 1 but
*   subsequent call may be in mode 2. See subroutine header for details.

*   The data used to define the surface are in XD,YD,ZD
*   and the points for which interpolated values will be returned are
*   contained in XI,YI. Results are returned in ZI1.

*   Program assumes that the failure of the routine constitutes
*   a fatal problem for the package and exits a loop structure untidily.

*   Size of input grid along each axis.
      NX=8
      NY=8

*   Distance between rows and columns.
      XINC=1.0
      YINC=1.0

*   Create X and Y values for the surface. 1D array.
      N=1
      DO 100 X=1,NX
         DO 200 Y=1,NY

*         Create x and y coords for each point on surface.
            XD(N)=REAL(X*XINC)
            YD(N)=REAL(Y*YINC)
            ZD(N)= 5.0 +
     :             1.*XD(N) +               0.5*YD(N) +
     :             0.25*XD(N)*XD(N) +       0.33*YD(N)*YD(N) +
     :             0.1*XD(N)*XD(N)*XD(N) +  0.05*YD(N)*YD(N)*YD(N)

*         Increment the counter.
            N=N+1

 200     CONTINUE
 100  CONTINUE
      NDP=N-1

*   Set mode
      MD = 1
      NCP= 2
      NOP= 1

*   Call interpolation subroutine.
      WRITE(*,*)
      WRITE(*,*) 'Testing PDA_IDBVIP.'
      WRITE(*,*)

*   Check a number of co-ordinates.
      TOTAL = 0.0
      DO 1001 X=1,NX-1
         DO 2001 Y=1,NY-1

*         Set location for which interpolation is to be returned.
            XI(1)=XD(X)+.5
            YI(1)=YD(Y)+.5

*         Call routine for each co-ordinate.
            CALL PDA_IDBVIP(MD,NCP,NDP,XD,YD,ZD,NOP,XI,YI,
     :                      ZI,IWK,WK,ISTAT,STATUS)

*         Check to see if the call was successful.
            IF (ISTAT.NE.0) THEN
               WRITE(*,*) 'Problems interpolating'
            ELSE
               TOTAL=TOTAL+ABS((5.0+XI(1)+.5*YI(1)+
     :             .25*XI(1)*XI(1)+
     :             .33*YI(1)*YI(1)+.1*XI(1)*XI(1)*XI(1)+
     :             .05*YI(1)*YI(1)*YI(1)-ZI(1,1))/ZI(1,1))
            END IF

*         Change mode after first call. To test alternate code path.
            MD = 2

 2001    CONTINUE
 1001 CONTINUE

*   Write out the values obtained.
      TOTAL=100.*total/(NX-1)/(NY-1)
      IF (TOTAL.LT.1.0) THEN
         WRITE(*,*) 'Ungridded 2-D polynomial fit PASSED'//
     :                ' self test. Total = ',TOTAL
      ELSE
         WRITE(*,*) 'Ungridded 2-D polynomial fit FAILED'//
     :                ' self test.'
      END IF

*   Call the PDA_IDSFFT subroutine.

*   First call should be in mode 1 but subsequent call might be in
*   mode 2. See subroutine header for details.

*   The data used to define the surface are in XD,YD,ZD and the grid
*   points defining the grid for which values are contained in XI,YI.
*   Results are returned in ZI12

*   Set up the grid positions.
      NXI=1
      NYI=1

*   Reset residuals total.
      TOTAL=0.0

*   Call interpolation subroutine.
      WRITE(*,*)
      WRITE(*,*) 'Testing PDA_IDSFFT.'
      WRITE(*,*)

*   Choose mode 1 for initial call
      MD=1

*   Check a number of co-ordinates.
      DO 3001 X=1,NX-1
         DO 4001 Y=1,NY-1

*         Set location for which interpolation is to be returned.
            XI(1)=XD(X)+.5
            YI(1)=YD(Y)+.5

*         Call the surface fitting subroutine.
            CALL PDA_IDSFFT(MD,NCP,NDP,XD,YD,ZD,
     :                      NXI,NYI,XI,YI,ZI2,IWK,
     :                      WK,ISTAT,STATUS)

            IF (ISTAT.NE.0) THEN
               WRITE(*,*) 'Problems interpolating.'
            ELSE
               TOTAL=TOTAL+ABS((5.0+XI(1)+.5*YI(1)+
     :             .25*XI(1)*XI(1)+
     :             .33*YI(1)*YI(1)+.1*XI(1)*XI(1)*XI(1)+
     :             .05*YI(1)*YI(1)*YI(1)-ZI2(1,1))/ZI2(1,1))
            END IF

*         Change mode after first call.
            MD=2

 4001    CONTINUE
 3001 CONTINUE

*   Write out the values obtained.
      TOTAL=100.*total/(NX-1)/(NY-1)
      IF (TOTAL.LT.1.0) THEN
         WRITE(*,*) 'Ungridded 2-D polynomial fit PASSED'//
     :                ' self test. Total = ', TOTAL
      ELSE
         WRITE(*,*) 'Ungridded 2-D polynomial fit FAILED'//
     :                ' self test.'
      END IF

      END
