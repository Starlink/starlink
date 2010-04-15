      SUBROUTINE DSA_FMTCON( BAD, CODE1, CODE2, ARRAY1, ARRAY2,
     :                                                NELM, NBAD )
*+
*  Name:
*     DSA_FMTCON

*  Purpose:
*     Convert a vector from one type to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_FMTCON( BAD, CODE1, CODE2, ARRAY1, ARRAY2, NELM, NBAD )

*  Description:
*     This routine converts a vector of numbers from one type to
*     another. The types are identified by their FMTCON_CODEs,
*     Conversion is done by the VEC_xTOx routines.

*  Arguments:
*     BAD = LOGICAL (Given)
*        True if bad values are expected in the input and to be
*        propagated to the output.
*     CODE1 = INTEGER (Given)
*        The code number for the input type. See the FMTCON_CODE array
*        in DSA_TYPES.INC.
*           1: BYTE      2: SHORT
*           3: INT       4: FLOAT
*           5: DOUBLE    6: UBYTE
*           7: USHORT
*     CODE2 = INTEGER (Given)
*        The code number for the output type. See CODE1.
*     ARRAY1 = Array of any type (Given)
*        The pointer to the input array. (The array is also "Given".)
*     ARRAY2 = Array of any type (Given)
*        The pointer to the output array. (The array is "Returned".)
*     NELM = INTEGER (Given)
*        The size of the arrays.
*     NBAD = INTEGER (Returned)
*        The number of errors that occured during conversion. If either
*        of the CODEs is unrecognised, NBAD is returned as equal to
*        NELM.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     KS:  Keith Shortridge (AAO)
*     {enter_new_authors_here}

*  History:
*     25-AUG-1992 (HME):
*        Original version.
*     24-FEB-1993 (KS):
*        Added definition of type 6 (UBYTE) and changed call so that arrays
*        are passed as such, rather than as pointers. This makes the routine
*        more suitable for calling from conventional Fortran programs. Support
*        for UBYTE arrays still has to be added.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      LOGICAL BAD
      INTEGER CODE1
      INTEGER CODE2
      BYTE ARRAY1(*)
      BYTE ARRAY2(*)
      INTEGER NELM

*  Arguments Returned:
      INTEGER NBAD

*  Local Variables:
      INTEGER IERR, STATUS

*.
      STATUS = 0

*  System of two-level nested IF clauses. The outer IF-ELSE-IF construct
*  distinguishes the input type code. The inner IF-ELSE-IF construct
*  distinguishes the output type code. The whole construct just
*  translates any combination (CODE1,CODE2) into the appropriate VEC_
*  routine name.
      IF ( CODE1 .EQ. 1 ) THEN
         IF ( CODE2 .EQ. 1 ) THEN
            CALL VEC_BTOB( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 2 ) THEN
            CALL VEC_BTOW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 3 ) THEN
            CALL VEC_BTOI( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 4 ) THEN
            CALL VEC_BTOR( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 5 ) THEN
            CALL VEC_BTOD( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 7 ) THEN
            CALL VEC_BTOUW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            NBAD = NELM
         END IF
      ELSE IF ( CODE1 .EQ. 2 ) THEN
         IF ( CODE2 .EQ. 1 ) THEN
            CALL VEC_WTOB( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 2 ) THEN
            CALL VEC_WTOW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 3 ) THEN
            CALL VEC_WTOI( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 4 ) THEN
            CALL VEC_WTOR( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 5 ) THEN
            CALL VEC_WTOD( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 7 ) THEN
            CALL VEC_WTOUW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            NBAD = NELM
         END IF
      ELSE IF ( CODE1 .EQ. 3 ) THEN
         IF ( CODE2 .EQ. 1 ) THEN
            CALL VEC_ITOB( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 2 ) THEN
            CALL VEC_ITOW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 3 ) THEN
            CALL VEC_ITOI( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 4 ) THEN
            CALL VEC_ITOR( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 5 ) THEN
            CALL VEC_ITOD( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 7 ) THEN
            CALL VEC_ITOUW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            NBAD = NELM
         END IF
      ELSE IF ( CODE1 .EQ. 4 ) THEN
         IF ( CODE2 .EQ. 1 ) THEN
            CALL VEC_RTOB( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 2 ) THEN
            CALL VEC_RTOW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 3 ) THEN
            CALL VEC_RTOI( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 4 ) THEN
            CALL VEC_RTOR( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 5 ) THEN
            CALL VEC_RTOD( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 7 ) THEN
            CALL VEC_RTOUW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            NBAD = NELM
         END IF
      ELSE IF ( CODE1 .EQ. 5 ) THEN
         IF ( CODE2 .EQ. 1 ) THEN
            CALL VEC_DTOB( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 2 ) THEN
            CALL VEC_DTOW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 3 ) THEN
            CALL VEC_DTOI( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 4 ) THEN
            CALL VEC_DTOR( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 5 ) THEN
            CALL VEC_DTOD( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 7 ) THEN
            CALL VEC_DTOUW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            NBAD = NELM
         END IF
      ELSE IF ( CODE1 .EQ. 7 ) THEN
         IF ( CODE2 .EQ. 1 ) THEN
            CALL VEC_UWTOB( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 2 ) THEN
            CALL VEC_UWTOW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 3 ) THEN
            CALL VEC_UWTOI( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 4 ) THEN
            CALL VEC_UWTOR( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 5 ) THEN
            CALL VEC_UWTOD( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( CODE2 .EQ. 7 ) THEN
            CALL VEC_UWTOUW( BAD, NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            NBAD = NELM
         END IF
      ELSE
         NBAD = NELM
      END IF

*  Return.
      END
