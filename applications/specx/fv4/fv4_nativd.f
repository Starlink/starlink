      SUBROUTINE FV4_NATIVD( BYTES8 )
*+
*  Name:
*     FV4_NATIVD

*  Purpose:
*     Convert VAX DOUBLE to native DOUBLE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_NATIVD( BYTES8 )

*  Description:
*     This routine takes 8 bytes and converts them into a DOUBLE value
*     The idea is as follows:
*     -  A VAX had in memory a DOUBLE,
*     -  it was EQUIVALENCE'd to eight elements of a BYTE array,
*     -  the BYTE array was written into an unformatted direct-access
*        file, which was then copied to the local machine,
*     -  the local machine opened the file as unformatted direct-access
*        and read back the BYTE array,
*     -  the eight VAX bytes are passed to this routine.
*     This routine will return the eight bytes such that they represent
*     the same DOUBLE on this machine as the given bytes did on the
*     VAX.

*  Arguments:
*     BYTES8 = DOUBLE PRECISION (Given and Returned)
*        The given eight bytes are a VAX DOUBLE, the eight returned bytes
*        are a native DOUBLE with the same value.

*  Notes:
*     This routine is written so that it works on any machine and so
*     that is uses only Fortran. It is very inefficient and suitable
*     only for one-off conversions, not for every-day use. No check is
*     made that the value as on the VAX can be represented in native
*     DOUBLE format.

*  Algorithm:
*     Swap each byte pair (i.e. 1 vs. 2, 3 vs. 4, etc.)
*     Copy each byte into an I*4.
*     Bring each extracted byte into range 0...255.
*     If highest (first) byte > 127,
*        Sign is negative.
*        Extract highest 7 bit of exponent.
*     Else,
*        Sign is positive.
*        Extract highest 7 bit of exponent.
*     If second highest (second) byte > 127,
*        Add 1 to exponent (lowest bit of exponent is set).
*        Leave byte as it is (highest mantisse bit set but not stored).
*     Else,
*        Leave exponent as it is (lowest bit of exponent not set).
*        Add 128 to the byte (highest mantisse bit set but not stored).
*     Apply the exponent excess.
*     Assemble mantisse in double precision floating point starting with
*        lowest byte.
*     Apply exponent and sign.
*     Copy value back into routine argument.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     03 Dec 1993 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      INTEGER BYTES8( 2 )

*  Local Variables:
      INTEGER GIVEN( 2 )
      DOUBLE PRECISION DGIVEN
      BYTE BGIVEN( 8 )
      EQUIVALENCE ( BGIVEN(1), DGIVEN, GIVEN )
      INTEGER LOW, LOW2, LOW3, LOW4, HIGH4, HIGH3, HIGH2, HIGH
      INTEGER EXP, MANT1
      DOUBLE PRECISION SIGN, VALUE

*.

*  There is a special case, where all bits are zero.
      IF ( BYTES8(1) .EQ. 0 .AND. BYTES8(2) .EQ. 0 ) THEN
         VALUE = 0D0
      ELSE

*     Copy given (declared I4(2)) to local byte array.
         GIVEN(1) = BYTES8(1)
         GIVEN(2) = BYTES8(2)

*     Swap each byte pair (i.e. 1 vs. 2, 3 vs. 4, etc.)
*     Copy each byte into an I*4.
         HIGH  = BGIVEN(2)
         HIGH2 = BGIVEN(1)
         HIGH3 = BGIVEN(4)
         HIGH4 = BGIVEN(3)
         LOW4  = BGIVEN(6)
         LOW3  = BGIVEN(5)
         LOW2  = BGIVEN(8)
         LOW   = BGIVEN(7)

*     Bring each extracted byte into range 0...255.
         IF ( LOW   .LT. 0 ) LOW   = LOW   + 256
         IF ( LOW2  .LT. 0 ) LOW2  = LOW2  + 256
         IF ( LOW3  .LT. 0 ) LOW3  = LOW3  + 256
         IF ( LOW4  .LT. 0 ) LOW4  = LOW4  + 256
         IF ( HIGH4 .LT. 0 ) HIGH4 = HIGH4 + 256
         IF ( HIGH3 .LT. 0 ) HIGH3 = HIGH3 + 256
         IF ( HIGH2 .LT. 0 ) HIGH2 = HIGH2 + 256
         IF ( HIGH  .LT. 0 ) HIGH  = HIGH  + 256

*     Process highest byte.
         IF ( HIGH .GT. 127 ) THEN
            SIGN = -1D0
            EXP  = 2 * ( HIGH - 128 )
         ELSE
            SIGN = +1D0
            EXP  = 2 * HIGH
         END IF

*     Process second highest byte.
         IF ( HIGH2 .GT. 127 ) THEN
            EXP   = EXP + 1
            MANT1 = HIGH2
         ELSE
            MANT1 = HIGH2 + 128
         END IF

*     Apply the exponent excess.
         EXP = EXP - 128

*     Assemble mantisse in double precision floating point starting with
*     lowest byte.
         VALUE =   DBLE( LOW   )           / 256D0
         VALUE = ( DBLE( LOW2  ) + VALUE ) / 256D0
         VALUE = ( DBLE( LOW3  ) + VALUE ) / 256D0
         VALUE = ( DBLE( LOW4  ) + VALUE ) / 256D0
         VALUE = ( DBLE( HIGH4 ) + VALUE ) / 256D0
         VALUE = ( DBLE( HIGH3 ) + VALUE ) / 256D0
         VALUE = ( DBLE( MANT1 ) + VALUE ) / 256D0

*     Apply exponent and sign.
         VALUE = SIGN * VALUE * 2D0 ** EXP
      END IF

*  Copy value back into routine argument.
      DGIVEN    = VALUE
      BYTES8(1) = GIVEN(1)
      BYTES8(2) = GIVEN(2)

*  Return.
      END
