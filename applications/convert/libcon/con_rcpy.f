      SUBROUTINE CON_RCPY (NX, NY, NZ, INCUBE, QLCUBE, BADBIT, VARFLG,
     :  INVAR, OUCUBE, OUTVAR, BADPIX, STATUS)
*+
*  Name:
*     CON_RCPY
*  Purpose:
*     Copy an Asterix data array to an NDF.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CON_RCPY (NX, NY, NZ, INCUBE, QLCUBE, BADBIT, VARFLG,
*       INVAR, OUCUBE, OUTVAR, BADPIX, STATUS)
*  Description:
*     Copy an Asterix data array to an NDF.  For each pixel the quality
*     array is checked and where appropriate bad values are
*     substituted for the input pixels.
*
*     If a variance array is present it is copied.
*
*     Note that the dimensionality is re-arranged.
*  Arguments:
*     NX  =  INTEGER (Given)
*        X size of the output cube.
*     NY  =  INTEGER (Given)
*        Y size of the output cube.
*     NZ  =  INTEGER (Given)
*        Z size of the output cube.
*     INCUBE(NY, NZ, NX)  =  REAL (Given)
*        Input cube.
*     QLCUBE(NY, NZ, NX)  =  BYTE (Given)
*        Quality cube corresponding to the input cube.
*     BADBIT  =  BYTE (Given)
*        Bad bits mask.
*     VARFLG  =  LOGICAL (Given)
*        Flag indicating whether a variance array is to be copied,
*        coded as follows:
*        .TRUE.  -  copy variance array,
*        .FALSE. -  do not copy variance array.
*     INVAR(NY, NZ, NX)  =  REAL (Given)
*        Input variance array, if present; otherwise unused.
*     OUCUBE(NX, NY, NZ)  =  REAL (Returned)
*        Output cube.
*     OUTVAR(NX, NY, NZ)  =  REAL (Returned)
*        Output variance array, if present; otherwise unused.
*     BADPIX  =  INTEGER (Returned)
*        Number of bad pixels encountered.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Copy every point in the cube.  The variance array is copied if
*     it is present.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     13/7/97 (ACD): Original version.
*     3/9/97  (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'PRM_PAR'           ! Starlink Bad values.
*  Arguments Given:
      INTEGER
     :  NX,
     :  NY,
     :  NZ
      REAL
     :  INCUBE(NY, NZ, NX),
     :  INVAR(NY, NZ, NX)
      BYTE
     :  QLCUBE(NY, NZ, NX),
     :  BADBIT
      LOGICAL
     :  VARFLG
*  Arguments Returned:
      REAL
     :  OUCUBE(NX, NY, NZ),
     :  OUTVAR(NX, NY, NZ)
      INTEGER
     :  BADPIX
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      BYTE BIT_ANDUB             ! Asterix quality mask function.
*  Local Variables:
      INTEGER
     :  LOOPX,  ! X loop index in the map grid.
     :  LOOPY,  ! Y  "     "   "   "   "   "  .
     :  LOOPZ   ! Z  "     "   "   "   "   "  .
*.

      IF (STATUS .EQ. SAI__OK) THEN
         BADPIX = 0

*
*       Copy every point, substituting bad values if appropriate.
*       The variance array is copied if it is present.

         DO LOOPZ = 1, NZ
            DO LOOPY = 1, NY
               DO LOOPX = 1, NX
                  IF (BIT_ANDUB(QLCUBE(LOOPY, LOOPZ, LOOPX), BADBIT)
     :               .EQ. 0) THEN
                     OUCUBE(LOOPX, LOOPY, LOOPZ) =
     :                                     INCUBE(LOOPY, LOOPZ, LOOPX)

                     IF (VARFLG) THEN
                        OUTVAR(LOOPX, LOOPY, LOOPZ) =
     :                                     INVAR(LOOPY, LOOPZ, LOOPX)
                     END IF

                  ELSE
                     OUCUBE(LOOPX, LOOPY, LOOPZ) = VAL__BADR

                     IF (VARFLG) THEN
                        OUTVAR(LOOPX, LOOPY, LOOPZ) = VAL__BADR
                     END IF

                  END IF
               END DO
            END DO
         END DO

      END IF

      END
