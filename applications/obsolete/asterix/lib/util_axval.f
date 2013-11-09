*+UTIL_AXVAL   Converts bin numbers into real axis values
      SUBROUTINE UTIL_AXVAL(NPTS,BIN,WIDTH,START,BINSTART,X)
* Description :
*     Takes an array of bin numbers and produces an array of axis positions.
* History :
*     23-May 1988       original     Richard Saxton
* Type Definitions :
      IMPLICIT NONE
* Import :
      INTEGER NPTS                    !No of points to be converted
      INTEGER BIN(NPTS)               !Bin numbers
      REAL WIDTH                      !Width of each bin
      REAL START                      !Axis value of first bin
      INTEGER BINSTART                !The bin number of the initial bin.
* Import-Export :
* Export :
      REAL X(NPTS)                    !Axis values for the bin numbers.
* Local constants :
* Local variables :
      INTEGER LP
*-
      DO LP=1,NPTS
*
         X(LP)=START + (BIN(LP)+BINSTART-1) * WIDTH
*
      ENDDO
*
      END
