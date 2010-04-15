      SUBROUTINE JCMT_QDISORT( DATA, N, PTRS )
*+
*  Name:
*     JCMT_QDISORT

*  Purpose:
*     Quick sort.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_QDISORT( DATA, N, PTRS )

*  Description:
*     This routine is a workaround necessary since the scrapping of the
*     Figaro routine GEN_QDISORT. This routine will take the given
*     double precision vector, subtract from it the average of its two
*     extrema, truncate that difference to single precision and let
*     Figaro's GEN_QFISORT do the sorting itself.
*
*     Reducing the range of the values to be centred on zero hopefully
*     eliminates the need for double precision in the sorting process.

*  Arguments:
*     DATA( N ) = DOUBLE PRECISION (Given)
*        The array containing the values to be sorted.
*     N = INTEGER (Given)
*        The number of elements in data or ptrs.
*     PTRS( N ) = INTEGER (Returned)
*        Contains the pointers to the sorted array. I.e. the lowest values
*        element is DATA(PTRS(1)), and the highest is DATA(PTRS(N)).

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     12 Oct 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION DATA( N )

*  Arguments Returned:
      INTEGER PTRS( N )

*  Local Constants:
      INTEGER REALSZ
      PARAMETER ( REALSZ = 4 )

*  Local Variables:
      INTEGER I, SLOT, STATUS
      INTEGER FDATA
      DOUBLE PRECISION SMALL, LARGE, CENTRE

*.

*  Get a work space for the single precision data.
      STATUS = 0
      CALL DSA_GET_WORKSPACE( REALSZ * N, FDATA, SLOT, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500

*  Find the extrema of the given data.
      SMALL = DATA(1)
      LARGE = DATA(1)
      DO 1 I = 2, N
         SMALL = MIN( SMALL, DATA(I) )
         LARGE = MAX( LARGE, DATA(I) )
 1    CONTINUE
      CENTRE = ( SMALL + LARGE ) / 2D0

*  Truncate the given data to single precision, subtracting a mid-range
*  value.
      CALL JCMT_QDISORT2( N, DATA, CENTRE, %VAL(FDATA) )

*  Sort.
      CALL GEN_QFISORT( %VAL(FDATA), N, PTRS )

*  Release the work space.
      CALL DSA_FREE_WORKSPACE( SLOT, STATUS )

*  Return.
 500  CONTINUE
      END



      SUBROUTINE JCMT_QDISORT2( N, DATA, CENTRE, FDATA )

      IMPLICIT NONE
      INTEGER N
      DOUBLE PRECISION DATA( N ), CENTRE
      REAL FDATA( N )
      INTEGER I

      DO 1 I = 1, N
         FDATA(I) = SNGL( DATA(I) - CENTRE )
 1    CONTINUE

      END
