C# IL>=a, OL>=0
*
*
*
*
*
      SUBROUTINE GK0MBU(IFUNC,STRING)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation
*  Author:             DSG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Metafile output buffer handler
*
*  MAINTENANCE LOG
*  ---------------
*     01/03/83  DSG  Original version stabilized
*     19/06/86  RMK  Maintenance log incorporated into main driver routine
*
*  ARGUMENTS
*  ---------
*     INP   IFUNC  Function code
*     INP   STRING Character string
*
      INTEGER IFUNC
      CHARACTER*(*) STRING
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKZIO/  CMBUFF - main buffer
*     Read   /GKYWCA/ KWKIX  - workstation index
*     Read   /GKYWKD/ KWCID  - channel number
*                     KWKDAT(14,KWKIX) - buffer pointer
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     MAXBUF   Fixed length of the output buffer
*     NCH      Number of characters in the string to be output
*     I        Loop  variable
*     IP       Buffer pointer
*
      INTEGER MAXBUF,NCH,I,IP
      PARAMETER (MAXBUF=80)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


* Pick up value of buffer pointer
      IP=KWKDAT(14,KWKIX)

      GOTO(10,20,30) IFUNC

* Initialise buffer

   10 IP=1
      GOTO 99

   20 NCH=LEN(STRING)
      DO 25 I= 1,NCH
      CMBUFF(KWKIX)(IP:IP)=STRING(I:I)
        IP=IP+1
        IF(IP.GT.MAXBUF) THEN
          WRITE(KWCID(KWKIX),'(A80)') CMBUFF(KWKIX)(1:80)
          IP=1
        ENDIF
   25 CONTINUE
      GOTO 99

* Pad buffer with blanks, and force out

   30 DO 35 I=IP,MAXBUF
        CMBUFF(KWKIX)(I:I) = ' '
   35 CONTINUE
      WRITE(KWCID(KWKIX),'(A80)') CMBUFF(KWKIX)(1:80)

* Save buffer pointer
   99 KWKDAT(14,KWKIX)=IP

      END
