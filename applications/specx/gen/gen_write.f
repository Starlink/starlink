*  History:
*     18 Nov 1993 (hme):
*        Avoid the call that uses %DESCR. Instead return IERR=8.
*-----------------------------------------------------------------------

      SUBROUTINE GEN_WRITE (STRING, OUTBYTE, LOUT, IERR)

*  Routine to print the value of a symbol. The output variable may
*  only be specified by address, so cannot be passed as a string. So
*  use this routine to provide a descriptor for the output variable.

      IMPLICIT  NONE

*     Formal parameter(s):

      CHARACTER STRING*(*)          ! List of items to print
      BYTE      OUTBYTE(*)          ! Output byte array
      INTEGER   LOUT                ! Input: length of OUTBYTE; rtn: filled len
      INTEGER*4 IERR                ! Error return

*     Local variables:

      INTEGER   I
      INTEGER   LS
      CHARACTER OUTSTRING*256       ! Temporary string

*     Functions:

      INTEGER   GEN_ILEN

*  Ok, go...

      IERR = 0

      LS = GEN_ILEN (OUTSTRING)
      CALL GEN_ENCODE (STRING, OUTSTRING, LS, IERR)
      IF (IERR.NE.0) RETURN

      DO I = 1, MIN (LS,LOUT)
        OUTBYTE(I) = ICHAR (OUTSTRING(I:I))
      END DO

      RETURN
      END


