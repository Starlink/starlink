C# IL>=a, OL>=0
*
      SUBROUTINE GK0NBU (IFUNC,NCH,STRING)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION
*  Author:             DSG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Buffer handler for metafile input
*
*  MAINTENANCE LOG
*  ---------------
*
*     28/07/89  RTP   Check for end of input buffer before READ. Stops
*                     GKS error 162 if last buffer is full.
*
*  ARGUMENTS
*  ---------
*     INP   IFUNC  1 = initialise, 2 = read in, 3 = skip,
*                  4 = read in (item starts in a new record)
*     INP   NCH    Number of bytes to be input
*     OUT   STRING String of NCH bytes
*
      INTEGER IFUNC, NCH
      CHARACTER *(*) STRING(1)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation index
*     Modify /GKZIO/     CMBUFF
*     Modify /GKYWKD/    Derive workstation I/O channel i.d.
*                        KWKDAT usage is described in GK0NWD.
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     I        Do loop variable
*     IP       Buffer pointer to last character read
*     IUNI     Indicates UNIRAS GKS metafile
*     ITYPE    Metafile Type
*
      INTEGER IUNI
      PARAMETER (IUNI = 3)
      INTEGER I,IP, ITYPE

*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*
*  ERRORS
*  ------
*     162   No item is left in GKS metafile input
*     302   Input/Output error has occurred while reading
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      ITYPE=KWKDAT(10,KWKIX)

* Pick up value of buffer pointer
      IP=KWKDAT(14,KWKIX)

      GOTO(10,20,30,40) IFUNC

* Read in the metafile header

   10 IP=0
      READ(KWCID(KWKIX),'(A80)',ERR=97,END=98) CMBUFF(KWKIX)(1:80)
      GOTO 99

* Read item

   20 DO 25 I= 1,NCH
         IP=IP+1
*    Check for end of logical record
         IF(IP.GT.80) THEN
            READ(KWCID(KWKIX),'(A80)',ERR=97,END=98)
     :        CMBUFF(KWKIX)(1:80)
            IP=1
         ENDIF
         STRING(1)(I:I) = CMBUFF(KWKIX)(IP:IP)
   25 CONTINUE

      GOTO 99

* Skip item

   30 DO 35 I=1,NCH
        IP=IP+1
*    Check for end of logical record
        IF(IP.GT.80) THEN
           READ(KWCID(KWKIX),'(A80)',ERR=97,END=98) CMBUFF(KWKIX)(1:80)
           IP=1
        ENDIF
   35 CONTINUE

      GOTO 99

* Get item

*    Check for end of logical record
   40 IF (IP.NE.0) THEN
         READ(KWCID(KWKIX),'(A80)',ERR=97,END=98) CMBUFF(KWKIX)(1:80)
         IP = 0
      ENDIF

      DO 45 I= 1,NCH
         IP=IP+1
         STRING(1)(I:I) = CMBUFF(KWKIX)(IP:IP)
   45 CONTINUE

      IF( KWKDAT(16,KWKIX).EQ.IUNI ) THEN
*       Check for end of file (this assumes knowledge of the UNIRAS
*        format).
         READ (STRING(1) (5:10),'(I6)') ITYPE
         IF(ITYPE.NE.0) THEN
            READ(KWCID(KWKIX),'(A80)',ERR=97,END=98) CMBUFF(KWKIX)
            IP = 0
         ENDIF

      ENDIF
      GOTO 99

* Error exit
   97 CONTINUE
      KERROR = 302
      GOTO 99

* End of file
   98 CONTINUE
      KERROR = 162

* Save buffer pointer
   99 KWKDAT(14,KWKIX)=IP
      RETURN

      END
