      SUBROUTINE GKIOFI (IFUNC, IBYTE, NBYTE, CBUFF)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    SYSTEM UTILITY
*  Author:             RTP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*  This is a sytem dependent routine to get NBYTE characters from a
*  byte-stream file.
*
*  This version is for CMS using fixed legth records only.
*
*
*  MAINTENANCE LOG
*  ---------------
*     17/04/90  RTP  Original version created
*
*  ARGUMENTS
*  ---------
*     INP   IFUNC  : Function code
*                     KIOIT - Initialise
*                     KIORB - Read bytes from file
*     INP   IBYTE  : Number of Bytes to read
*     OUT   NBYTE  : Number of Bytes read
*     OUT   CBUFF  : Character variable to receive data
*
      INTEGER IFUNC, IBYTE, NBYTE
      CHARACTER*(*) CBUFF
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation index
*     Modify /GKYWKD/    Derive workstation I/O channel i.d.
*
      INCLUDE '../include/gkio.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkio.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'

*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*
*     KWCID(KWKIX)On CMS assume Fixed length records
*
*  ERRORS
*  ------
*     162   No item is left in GKS metafile input
*     302   Input/Output error has occurred while reading
*
*---------------------------------------------------------------------

      IF (IFUNC .EQ. KIOIT) THEN
         INQUIRE(UNIT=KWCID(KWKIX),RECL=IBYTE)
      ELSE IF (IFUNC .EQ. KIORB) THEN
         NBYTE = IBYTE
         READ(KWCID(KWKIX),10,ERR=97,END=98) CBUFF(1:NBYTE)
      ENDIF

  10  FORMAT(A)
      GOTO 99

* Error exit
   97 CONTINUE
      KERROR = 302
      GOTO 99

* End of file
   98 CONTINUE
      KERROR = 162

   99 CONTINUE
      RETURN
      END
