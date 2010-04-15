      SUBROUTINE FV4_NATIVR( BYTES4 )
*+
*  Name:
*     FV4_NATIVR

*  Purpose:
*     Convert VAX REAL to native REAL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_NATIVR( BYTES4 )

*  Description:
*     This routine takes 4 bytes and converts them into a REAL value
*     The idea is as follows:
*     -  A VAX had in memory a REAL,
*     -  it was EQUIVALENCE'd to four elements of a BYTE array,
*     -  the BYTE array was written into an unformatted direct-access
*        file, which was then copied to the local machine,
*     -  the local machine opened the file as unformatted direct-access
*        and read back the BYTE array,
*     -  the four VAX bytes are passed to this routine.
*     This routine will return the four bytes such that they represent
*     the same REAL on this machine as the given bytes did on the
*     VAX.

*  Arguments:
*     BYTES4 = REAL (Given and Returned)
*        The given four bytes are a VAX REAL, the four returned bytes
*        are a native REAL with the same value.

*  Notes:
*     This routine is written so that it works on any machine and so
*     that is uses only Fortran. It is very inefficient and suitable
*     only for one-off conversions, not for every-day use. No check is
*     made that the value as on the VAX can be represented in native
*     REAL format.

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
      INTEGER BYTES4

*  Local Variables:
      INTEGER GIVEN
      REAL RGIVEN
      BYTE BGIVEN( 4 )
      EQUIVALENCE ( BGIVEN(1), RGIVEN, GIVEN )
      INTEGER LOW, LOW2, HIGH2, HIGH
      INTEGER EXP, MANT1
      DOUBLE PRECISION SIGN, VALUE

*.

*  There is a special case, where all bits are zero.
      IF ( BYTES4 .EQ. 0 ) THEN
         VALUE = 0D0
      ELSE

*     Copy given (declared I4) to local byte array.
         GIVEN = BYTES4

*     Swap each byte pair (i.e. 1 vs. 2, 3 vs. 4, etc.)
*     Copy each byte into an I*4.
         HIGH  = BGIVEN(2)
         HIGH2 = BGIVEN(1)
         LOW2  = BGIVEN(4)
         LOW   = BGIVEN(3)

*     Bring each extracted byte into range 0...255.
         IF ( LOW   .LT. 0 ) LOW   = LOW   + 256
         IF ( LOW2  .LT. 0 ) LOW2  = LOW2  + 256
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
         VALUE = ( DBLE( MANT1 ) + VALUE ) / 256D0

*     Apply exponent and sign.
         VALUE = SIGN * VALUE * 2D0 ** EXP
      END IF

*  Copy value back into routine argument.
      RGIVEN = VALUE
      BYTES4 = GIVEN

*  Return.
      END
