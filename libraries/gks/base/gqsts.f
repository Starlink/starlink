C# IL>=b, OL>=0
      SUBROUTINE GQSTS (IWKID,IDNR,MLDR,IER,IMODE,IESW,LOSTR,ISTR,
     :                     IPET,EAREA,IBFLEN,INIPOS,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE STRING DEVICE STATE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns string device state.
*
*  MAINTENANCE LOG
*  ---------------
*     03/10/83  AS    Original version stabilized
*     10/02/84  JL    Call to GKPRLG inserted (I89)
*     10/02/84  JL    Change parameters to GKQXXD to make
*                     NRD distinct variables (I114)
*     26/06/84  JRG   To fix compilation error, shorten one line to
*                     below 72 characters (fixing bug S63)
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP IDNR    string device number
*     INP MLDR    dimension of data record
*     OUT IER     error indicator
*     OUT IMODE   operating mode
*     OUT IESW    echo switch
*     OUT LOSTR   number of characters in initial string
*     OUT ISTR    initial string
*     OUT IPET    prompt and echo type
*     OUT EAREA   echo area
*     OUT IBFLEN  buffer length of string
*     OUT INIPOS  initial cursor position
*     OUT LDR     length of data record
*     OUT DATREC  data record
*
      INTEGER IWKID, IDNR, MLDR, IER, IMODE, IESW, LOSTR, IPET, IBFLEN,
     :        INIPOS, LDR
      REAL EAREA(4)
      CHARACTER*(*) ISTR
      CHARACTER*(*) DATREC(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I, LENGTH, IOFF, NRD
      REAL R
*
*  STACK USAGE
*  -----------
*     LENGTH  INTEGER  space for character/integer array
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

* Get space for character/integer array

        IOFF = KNIL
        LENGTH = LEN(ISTR)
        CALL GKSTAL(KINTGS,LENGTH,IOFF)
        IF (KERROR.EQ.0) THEN
          CALL GKQXXD(IWKID,KQSTS,IDNR,LENGTH,1,MLDR,0,IER,IMODE,
     :              IESW,IPET,EAREA,LOSTR,KSTACK(IOFF),NRD,QDAT,QDAT,
     :              LDR,DATREC,IBFLEN,INIPOS,I,R,R,R)

          IF (KERROR.EQ.0 .OR. KERROR.EQ.901) THEN
* Convert integer array back to characters
            I = MIN (LENGTH, LOSTR)
            CALL GKATON(I,KSTACK(IOFF),ISTR)
            IF (KERROR.NE.0) IER = KERROR
          ENDIF
        ELSE
          IER = KERROR
        ENDIF
        CALL GKSTDA(KINTGS,IOFF)

      ELSE
         IER = KERROR
      ENDIF

      END
