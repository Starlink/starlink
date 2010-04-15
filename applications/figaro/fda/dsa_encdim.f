      SUBROUTINE DSA_ENCDIM( CHARS, NDIM, DIMS, IPT )
*+
*  Name:
*     DSA_ENCDIM

*  Purpose:
*     Encode dimension information into a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_ENCDIM( CHARS, NDIM, DIMS, IPT )

*  Description:
*     This routine encodes dimension information into a character string.

*  Arguments:
*     CHARS = CHARACTER * ( * ) (Given and Returned)
*        The string into which to encode the dimensions.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the array.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the array.
*     IPT = INTEGER (Given and Returned)
*        Character number. Passed as the character at which to start the
*        encoding (i.e. where the '(' goes) and returned as the number
*        of the last character used (the one where the ')' went).

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acd: Clive Davenhall (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 Jun 1988 (ks):
*        Original version.
*     02 Sep 1992 (hme):
*        Change declared length MAX(...) to *. Change FIG_ENCDIM to
*        DSA_ENCDIM.
*     21 Feb 1996 (hme):
*        FDA library.
*     21 Dec 2000 (acd):
*        Removed unused variable.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NDIM
      INTEGER DIMS( NDIM )

*  Arguments Given and Returned:
      CHARACTER * ( * ) CHARS
      INTEGER IPT

*  Local Variables:
      INTEGER I
      INTEGER IWPT
      INTEGER LIMPTR
      INTEGER N
      CHARACTER * ( 20 ) WORK

*.

      LIMPTR = LEN(CHARS)
      IF ( NDIM .EQ. 0 ) THEN
         CHARS(IPT:) = '('
         IPT = IPT + 1
      ELSE
         DO I = 1, NDIM
            IF ( I .EQ. 1 ) THEN
               CHARS(IPT:IPT) = '('
            ELSE
               CHARS(IPT:IPT) = ','
            END IF
            IPT = IPT + 1
            N = DIMS(I)
            IWPT = 20
            DO WHILE (N .GT. 0 )
               WORK(IWPT:IWPT) = CHAR( MOD(N,10) + ICHAR('0') )
               IWPT = IWPT - 1
               N = N / 10
            END DO
            N = IPT + 19 - IWPT
            IF ( N .LE. LIMPTR ) THEN
               CHARS(IPT:N) = WORK(IWPT+1:)
               IPT = N + 1
            END IF
         END DO
      END IF
      IF ( IPT .LE. LIMPTR ) CHARS(IPT:IPT) = ')'

      END
