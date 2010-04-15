      SUBROUTINE FV4_NATIVI( BYTES4 )
*+
*  Name:
*     FV4_NATIVI

*  Purpose:
*     Convert VAX INTEGER to native INTEGER.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_NATIVI( BYTES4 )

*  Description:
*     This routine takes 4 bytes and converts them into an INTEGER
*     value. The idea is as follows:
*     -  A VAX had in memory an INTEGER,
*     -  it was EQUIVALENCE'd to four elements of a BYTE array,
*     -  the BYTE array was written into an unformatted direct-access
*        file, which was then copied to the local machine,
*     -  the local machine opened the file as unformatted direct-access
*        and read back the BYTE array,
*     -  the four VAX bytes are passed to this routine.
*     This routine will return the four bytes such that they represent
*     the same INTEGER on this machine as the given bytes did on the
*     VAX.

*  Arguments:
*     BYTES4 = INTEGER (Given and Returned)
*        The given four bytes are a VAX INTEGER, the four returned bytes
*        are a native INTEGER with the same value.

*  Notes:
*     This routine is written so that it works on any machine and so
*     that is uses only Fortran. It is very inefficient and suitable
*     only for one-off conversions, not for every-day use.

*  Algorithm:
*     Copy each byte into an I*4.
*     Bring each extracted byte into range 0...255.
*     If highest (first) byte > 127,
*        Number is negative.
*        Negate all but lowest byte bitwise.
*        Negate lowest byte bitwise and add 1 (two's complement).
*        If lowest byte > 255 subtract 256 (ignore carry bit).
*     Else,
*        Number is positive.
*     Combine bytes to correct absolute value of result.
*     Multiply with sign.
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
      BYTE BGIVEN( 4 )
      EQUIVALENCE ( BGIVEN(1), GIVEN )
      INTEGER SIGN, LOW, LOW2, HIGH2, HIGH, VALUE

*.

      GIVEN = BYTES4
      LOW   = BGIVEN(1)
      LOW2  = BGIVEN(2)
      HIGH2 = BGIVEN(3)
      HIGH  = BGIVEN(4)
      IF ( LOW   .LT. 0 ) LOW   = LOW   + 256
      IF ( LOW2  .LT. 0 ) LOW2  = LOW2  + 256
      IF ( HIGH2 .LT. 0 ) HIGH2 = HIGH2 + 256
      IF ( HIGH  .LT. 0 ) HIGH  = HIGH  + 256

      IF ( HIGH .GT. 127 ) THEN
         SIGN  = -1
         HIGH  = 255 - HIGH
         HIGH2 = 255 - HIGH2
         LOW2  = 255 - LOW2
         LOW   = 256 - LOW
         IF ( LOW .GT. 255 ) LOW = LOW - 256
      ELSE
         SIGN = +1
      END IF

      VALUE  = LOW + 256 * LOW2 + 256*256 * HIGH2 + 256*256*256 * HIGH
      VALUE  = SIGN * VALUE
      BYTES4 = VALUE

      END
