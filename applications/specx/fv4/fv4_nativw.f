      SUBROUTINE FV4_NATIVW( BYTES2 )
*+
*  Name:
*     FV4_NATIVW

*  Purpose:
*     Convert VAX INTEGER*2 to native INTEGER*2.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_NATIVW( BYTES2 )

*  Description:
*     This routine takes 2 bytes and converts them into an INTEGER*2
*     value. The idea is as follows:
*     -  A VAX had in memory an INTEGER*2,
*     -  it was EQUIVALENCE'd to two elements of a BYTE array,
*     -  the BYTE array was written into an unformatted direct-access
*        file, which was then copied to the local machine,
*     -  the local machine opened the file as unformatted direct-access
*        and read back the BYTE array,
*     -  the two VAX bytes are passed to this routine.
*     This routine will return the two bytes such that they represent
*     the same INTEGER*2 on this machine as the given bytes did on the
*     VAX.

*  Arguments:
*     BYTES2 = INTEGER*2 (Given and Returned)
*        The given two bytes are a VAX INTEGER*2, the two returned bytes
*        are a native INTEGER*2 with the same value.

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
      INTEGER*2 BYTES2

*  Local Variables:
      INTEGER*2 GIVEN
      BYTE BGIVEN( 2 )
      EQUIVALENCE ( BGIVEN(1), GIVEN )
      INTEGER SIGN, LOW, HIGH, VALUE

*.

      GIVEN = BYTES2
      LOW   = BGIVEN(1)
      HIGH  = BGIVEN(2)
      IF ( LOW  .LT. 0 ) LOW  = LOW  + 256
      IF ( HIGH .LT. 0 ) HIGH = HIGH + 256

      IF ( HIGH .GT. 127 ) THEN
         SIGN = -1
         HIGH = 255 - HIGH
         LOW  = 256 - LOW
         IF ( LOW .GT. 255 ) LOW = LOW - 256
      ELSE
         SIGN = +1
      END IF

      VALUE  = LOW + 256 * HIGH
      VALUE  = SIGN * VALUE
      BYTES2 = VALUE

      END
