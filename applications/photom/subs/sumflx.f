************************************************************************
      SUBROUTINE SUMFLX(IMAGE, CLIP, SKY, SIGMA, VSKY, APAR,
     :                  PADU, SATURE, NX, NY, ERROR, BESTN, STAR,
     :                  CODE, STATUS)

*+
*  Name :
*     SUMFLX
*
*  Purpose :
*
*  Language :
*     FORTRAN
*
*  Invocation :
*      CALL SUMFLX(IMAGE, CLIP, SKY, SIGMA, VSKY, APAR,
*     :            PADU, SATURE, NX, NY, ERROR, BESTN, STAR,
*     :            CODE, STATUS)
*
*  Description :
*
*  Arguments :
*     IMAGE( NX, NY ) = REAL (Given)
*        Array containing image
*     CLIP = REAL (Given)
*        Clipping radius for weight map
*     SKY = REAL (Given)
*        Value in sky aperture per pixel
*     SIGMA = REAL (Given)
*        Standard deviation in sky aperture
*     VSKY = REAL (Given)
*        Variance in sky aperture per pixel
*     APAR(6) = REAL (Given and Returned)
*        Parameters defining the shape of the profile
*     PADU = REAL (Given)
*        Photons per data unit
*     SATURE = REAL (Given)
*        User supplied saturation level
*     NX= INTEGER (Given)
*        X dimension of image array
*     NY = INTEGER (Given)
*        Y dimension of image array
*     ERROR = REAL (Returned)
*        Estimate of the error in STAR
*     BESTN = REAL (Returned)
*        Estimate of the best achievable noise
*     STAR = REAL (Returned)
*        Summed flux in star
*     CODE = CHARACTER*2 (Returned)
*        BAD or SATURATED pixel?
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     This calls DOSUM after setting up temporary workspace
*     {algorithm_description}...
*
*  Deficiencies :
*     I'm worried about the quality of the sky determination,
*     Tim's OPPHOT implementation of the same algorithim takes
*     a much more rigorous approach than PHOTOM currently does.
*     {routine_deficiencies}...
*
*  Authors :
*     TN: Tim Naylor (Keele University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     ??-???-1997
*        Original version written in FORTRAN 90 by Tim
*     15-DEC-1998
*        Cut and hack to FORTRAN 77 for Starlink
*        Added NX and NY to calling arguments
*        Now SUBROUTINE instead of FUNCTION
*        Added STAR returned arguement
*     20-JAN-1999
*        Added CODE to passed arguements
*     07-SEP-2004
*        Changed to use CNF pointers
*     10-JAN-2008 (PWD):
*        Keep the MIN1, MIN2, MAX1 and MAX2 limits within the
*        bounds of the IMAGE array.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :

      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'
      INCLUDE 'CNF_PAR'

*  Arguments Given :

      INTEGER NX, NY

      REAL IMAGE(NX, NY)
      REAL CLIP, PADU, SATURE
      REAL SKY, SIGMA, VSKY

*  Arguments Given and Returned :

      REAL APAR(6)

*  Arguments Returned :

      REAL STAR, ERROR, BESTN
      CHARACTER * ( 2 ) CODE

*  Local Variables :

      INTEGER STATUS

      INTEGER I, J

      INTEGER MIN1, MIN2, MAX1, MAX2

*   HDS temporary object variables

      INTEGER DIM(2)
      INTEGER X, Y

      CHARACTER * ( DAT__SZLOC ) VLOC
      CHARACTER * ( DAT__SZLOC ) RLOC
      CHARACTER * ( DAT__SZLOC ) MLOC
      CHARACTER * ( DAT__SZLOC ) DLOC
      INTEGER VVAR, RVAR, MASK, DATA

*.

*   Check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Simulate the functionality of the F90 floor() and
*   ceiling() intrinsic functions. This entire hack
*   is four lines of F90, somebody shoot me!

      IF ( (APAR(5)-CLIP-0.5) .GT. 0.0 ) THEN
            MIN1 = INT(APAR(5)-CLIP-0.5)
      ELSE
            MIN1 = INT(APAR(5)-CLIP-0.5)-1
      ENDIF
      IF ( (APAR(6)-CLIP-0.5) .GT. 0.0 ) THEN
            MIN2 = INT(APAR(6)-CLIP-0.5)
      ELSE
            MIN2 = INT(APAR(6)-CLIP-0.5)-1
      ENDIF
      IF ( (APAR(5)-CLIP-0.5) .GT. 0.0 ) THEN
            MAX1 = INT(APAR(5)+CLIP+0.5)+1
      ELSE
            MAX1 = INT(APAR(5)+CLIP+0.5)
      ENDIF
      IF ( (APAR(6)-CLIP-0.5) .GT. 0.0 ) THEN
            MAX2 = INT(APAR(6)+CLIP+0.5)+1
      ELSE
            MAX2 = INT(APAR(6)+CLIP+0.5)
      ENDIF

*   Clip to array bounds, 1->NX & 1->NY.
      MIN1 = MAX( MIN1, 1 )
      MIN2 = MAX( MIN2, 1 )
      MAX1 = MIN( MAX1, NX )
      MAX2 = MIN( MAX2, NY )

*   Get some temporary workspace for our HDS objects, this is
*   a hack to get around the lack of allocatable arrays in F77

      DIM(1) = MAX1 - MIN1 + 1
      DIM(2) = MAX2 - MIN2 + 1
      CALL DAT_TEMP("_REAL", 2, DIM, VLOC, STATUS)
      CALL DAT_MAPR(VLOC, 'WRITE', 2, DIM, VVAR, STATUS)
      CALL DAT_TEMP("_REAL", 2, DIM, RLOC, STATUS)
      CALL DAT_MAPR(RLOC, 'WRITE', 2, DIM, RVAR, STATUS)
      CALL DAT_TEMP("_REAL", 2, DIM, MLOC, STATUS)
      CALL DAT_MAPR(MLOC, 'WRITE', 2, DIM, MASK, STATUS)
      CALL DAT_TEMP("_REAL", 2, DIM, DLOC, STATUS)
      CALL DAT_MAPR(DLOC, 'WRITE', 2, DIM, DATA, STATUS)

*   Pass the workspace to DOSUM which carries out the summation
*   loop for the optimal extraction algorithim

      X = DIM(1)
      Y = DIM(2)

*   This is a temporary hack while I figure out  a linear version
*   of the algorithim, should work fine, if somewhat more slowly

      CALL DOSUM(IMAGE, NX, NY, %VAL(CNF_PVAL(VVAR)),
     :           %VAL(CNF_PVAL(RVAR)), %VAL(CNF_PVAL(MASK)),
     :           %VAL(CNF_PVAL(DATA)), X, Y, SKY, SIGMA, VSKY, APAR,
     :           PADU, SATURE, CLIP, ERROR, BESTN, STAR,
     :           MIN1, MAX1, MIN2, MAX2, CODE, STATUS)


      CALL DAT_UNMAP( VLOC, STATUS )
      CALL DAT_ANNUL( VLOC, STATUS )

      CALL DAT_UNMAP( RLOC, STATUS )
      CALL DAT_ANNUL( RLOC, STATUS )

      CALL DAT_UNMAP( MLOC, STATUS )
      CALL DAT_ANNUL( MLOC, STATUS )

      CALL DAT_UNMAP( DLOC, STATUS )
      CALL DAT_ANNUL( DLOC, STATUS )


*   End of routine

  99  CONTINUE

      END










