      SUBROUTINE GK2NCL(ICOL,LPREC,KBYTE,KBIT)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) Workstaion Driver
*  Author:             RTP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine gets a bitstream colour from the CGM
*  (indexed only)
*
*  MAINTENANCE LOG
*  ---------------
*     17/04/90  RTP   Original version created
*     Maintenance log is incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*   out  ICOL   : Colour Index
*   in   LPREC  : Local Colour Precision
*   in   KBIT   : Current bit position as power of 2
*   in   KBYTE  : Current byte value
*
      INTEGER ICOL, LPREC, KBIT, KBYTE
*
*  COMMON BLOCKS
*  -------------
*
      INCLUDE '../../include/gkdt.par'

      INCLUDE '../../include/gkwca.cmn'
*
*  LOCAL
*  -----
*        I      : Local Loop variable
*
      INTEGER I
*
*---------------------------------------------------------------------

      ICOL = 0
      DO 10 I = 1, LPREC
         IF ( KBIT .LE. 1 ) THEN
*  Get next byte
            CALL GK2NNC(KBYTE,.TRUE.)
            IF ( KBYTE/64 .EQ. 0 ) THEN
*  End of list
               ICOL = -1
               GOTO 99
            ENDIF
            KBIT = 64
         ENDIF
         KBYTE = MOD(KBYTE,KBIT)
         KBIT = KBIT/2
         ICOL = 2*ICOL + KBYTE/KBIT
 10   CONTINUE

 99   CONTINUE
      RETURN
      END
