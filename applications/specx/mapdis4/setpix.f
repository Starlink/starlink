*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE SETPIX (PBEG, PEND, CELL_SIZE, NSTEPS,
     &                   PL, PR, IL, IR, NI)

*     Routine to determine distribution of map points within a
*     nominated image area.

      IMPLICIT NONE

*     Formal parameters:

      REAL     PBEG         ! Start point of axis
      REAL     PEND         ! End point of axis
      REAL     CELL_SIZE    ! Size of pixel
      INTEGER  NSTEPS       ! Number of pixels in map (centered on (0.0))
      REAL     PL           ! map position at left edge of image
      REAL     PR           ! map position at right edge of image
      INTEGER  IL           ! map pixel at left edge of image
      INTEGER  IR           ! map pixel at right edge of image
      INTEGER  NI           ! total number of pixels displayed in image

*     Local variables

      REAL     PMID
      REAL     X1, X2

*  Ok, go...

CD    Print *, ' -- setpix --'

*     Map axes are implicit arrays with zero at (N+1)/2 points and
*     CELL_SIZE arcseconds per point from here. Normalize input
*     values to cells

      PMID = 0.5 * (FLOAT(NSTEPS)+1.)

      X1   = PBEG/CELL_SIZE + PMID
      X2   = PEND/CELL_SIZE + PMID

CD    Print *, '    begin and end at map-pts ', X1, X2

*     Now find included pixels. First included pixel is integer
*     greater than the smaller of X1 & X2, last pixel is integer
*     smaller than max of X1 & X2.

      IL = NINT (MIN (X1,X2) + 0.49999)
      IR = NINT (MAX (X1,X2) - 0.49999)

CD    Print *, '    limiting included pixels ', IL, IR

      NI = IR - IL + 1

CD    Print *, '    total number of pixels remaining = ', NI

*     Flip IL & IR if input variables in "wrong" order

      IF ((PEND-PBEG)/CELL_SIZE .lt. 0.0) CALL SWAP2 (IL,IR)

*     Assign "real" values to these pixels

      PL = CELL_SIZE * (FLOAT(IL) - PMID)
      PR = CELL_SIZE * (FLOAT(IR) - PMID)

CD    Print *, '    limiting values in array are ', PL, PR

      RETURN
      END

*-----------------------------------------------------------------------

