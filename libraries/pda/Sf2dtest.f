      PROGRAM SF2DTEST

*+
*  Name:
*     SF2DTEST

*  Purpose:
*   This program is a test  routine for a modified form of
*   the CMLIB DB2INK/DB2VAL subprogram package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SF2DTEST

*  Description:
*   The routines used are modified versions of the CMLIB DB2INK and
*   DB2VAL spline interpolation routines.

*   The DB2 routines have been modified to ensure error
*   messages are not generated during execution. The value IFLAG
*   returns a value that must be interpreted.

*   PDA_DB2INK - Summary

*   DB2INK determines the parameters of a  function  that  interpolates
*   the two-dimensional gridded data (X(i),Y(j),FCN(i,j)) for i=1,..,NX
*   and j=1,..,NY. The interpolating function and its  derivatives  may
*   subsequently be evaluated by the function DB2VAL.

*   PDA_DB2VAL - Summary

*   DB2VAL  evaluates   the   tensor   product   piecewise   polynomial
*   interpolant constructed  by  the  routine  DB2INK  or  one  of  its
*   derivatives at the point (XVAL,YVAL). To evaluate  the  interpolant
*   itself, set IDX=IDY=0, to evaluate the first partial  with  respect
*   to x, set IDX=1,IDY=0, and so on.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-Jan-1996 (GJP)
*        (Original version)
*     24-Feb-1997 (DSB)
*        Changed from an ATASK to a stand-alone program.
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*   Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I                       ! Loop variable
      INTEGER ID                      ! Specifies use value not differential
      INTEGER IFAIL                   ! Was the surface successfully created?
      INTEGER J                       ! Loop variable
      INTEGER MXY                     ! Size of the grid
      INTEGER ORD                     ! Order of polynomial used

      DOUBLE PRECISION DVALUE         ! Interpolated value returned
      DOUBLE PRECISION FV1(8,8)       ! Grid Z values
      DOUBLE PRECISION X1(8)          ! X grid location
      DOUBLE PRECISION Y1(8)          ! Y grid location
      DOUBLE PRECISION XD             ! X coord
      DOUBLE PRECISION YD             ! Y coord

      DOUBLE PRECISION BCOEF(8,8)     ! Array used by PDA_DB2INK
      DOUBLE PRECISION TX(11),TY(11)  ! Array used by PDA_DB2INK
      DOUBLE PRECISION WORK(168)      ! Array used by PDA_DB2INK

      DOUBLE PRECISION TOTAL          ! Fit residue total
*.
      STATUS = 0

*   Tell user what is happening.
      WRITE(*,*)
      WRITE(*,*) 'Testing PDA_DB2INK/PDA_DB2VAL.'
      WRITE(*,*)

*   Subroutine initial error value.
      IFAIL=0

*   Order of polynomial used.
      ORD=3

*   Use value rather than differential.
      ID=0
*   Size of data grid.
      MXY=8

*   Set up grid co-ordinates.
      DO 10 I=1,MXY
         X1(I)=DBLE(5.+I)
         Y1(I)=DBLE(2.+I)
 10   CONTINUE

*   Assign surface values.
      DO 30 I=1,MXY
         DO 40 J=1,MXY

*         Individual point values.
            FV1(I,J)=10.+X1(I)*X1(I)/4.+
     :            Y1(J)*Y1(J)*Y1(J)/10.

 40      CONTINUE
 30   CONTINUE

*   Build the surface fit using the grid contents.
      CALL PDA_DB2INK(X1,MXY,Y1,MXY,FV1,MXY,
     :                ORD,ORD,TX,TY,BCOEF,
     :                WORK,IFAIL,STATUS)

*   If IFAIL=1 then okay to interpolate values.
      IF (IFAIL.EQ.1) THEN

*      Interpolate values for whole of a NEW grid.
          TOTAL=0.0
          DO 50 I=1,MXY-1
             DO 60 J=1,MXY-1

*             Set values of each grid point.
                XD=X1(I)+.5
                YD=Y1(J)+.5

*             Setup evaluation routine.
                IFAIL=0
                CALL PDA_DB2VAL(XD,YD,ID,ID,TX,TY,
     :                          MXY,MXY,ORD,ORD,BCOEF,WORK,
     :                          DVALUE,IFAIL,STATUS)

*             Calculate residuals.

*             Alert user to problems or calc residuals.
                IF (IFAIL.NE.0) THEN
                   WRITE(*,*) 'Problems interpolating.'
                ELSE
                   TOTAL=TOTAL+ABS((DVALUE-
     :             (10.+XD*XD/4.+YD*YD*YD/10.))/DVALUE)
                END IF

 60          CONTINUE
 50       CONTINUE

*       Check to see if the residues were sensible. ie 1% accuracy.
          TOTAL=100.*total/(MXY-1)/(MXY-1)
          IF (TOTAL.LT.1.0) THEN
             WRITE(*,*) 'Gridded 2-D polynomial fit PASSED self test.'
          ELSE
             WRITE(*,*) 'Gridded 2-D polynomial fit FAILED self test.'
          END IF

      ELSE

*       Tell user there is a problem/
        WRITE(*,*) 'Problems creating the surface.'

      END IF

      WRITE(*,*)

      END
