C# IL>=a, OL>=0
      SUBROUTINE GKXON
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             PB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     opens the text data base, performs initialisations.
*
*  MAINTENANCE LOG
*  ---------------
*     16/09/83  CJW   Changed to use GKIOOP
*     30/11/83  AS    Check KERROR after calling GKIOOP
*     12/12/83  AS    Store GKS text font numbers
*     20/01/84  PB    change FNAME to char*6
*                     and only use fonts actually on file
*     21/02/84  JRG   Change "1" to '1' because the former is
*                     non-standard.
*     23/07/84  JRG   Fix bug S??? where font numbers got multiplied by
*                     1000; trailing spaces got interpreted as zeros
*                     (bug S64).
*     13/02/87  PKY   Changed to accomodate the fact that CLWDTH has
*                     changed from CHARACTER*(KMXICN) CLWDTH to
*                     CHARACTER*1 CLWDTH(KMXICN). CRWDTH and CINDEX
*                     changed in a similar way (see GKXFD.CMN)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify   /GKHDB/    Font and character indexes
*     Modify   /GKFLS/    KDBFLU
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkxca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     I
*     NUMREC
*     NRCOFF
*     INDREC
*     NCHARS
*     IGNORE
*     NMFNTS
*     NCH
*     FFORM
*
      INTEGER I,NUMREC,NRCOFF,INDREC,NCHARS,IGNORE,NMFNTS,NCH
      CHARACTER*6 FNAME(KFNTMX)
      CHARACTER*5 FFORM
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     KDBFLU   disk         Input directories
*     IERROR   disk         error message file accessed
*
*  COMMENTS
*  --------
*
*   +------------------------------------------------------+
*   |First data base record                                |
*   +------------------------------------------------------+
*   |word 1         -  record no of 1st stroke definition  |---------+
* +-|  "  2         -  record no of char index             |         |
* | |  "  3         -  number of hershey characters        |         |
* | |  "  4         -  maximum strokes in hershey character|         |
* | |  "  5         -  number of fonts                     |         |
* | |  "  6->          record numbers of font definitions  |-----+   |
* | |  " nfonts+6-> -  names of fonts                      |     |   |
* | +------------------------------------------------------+     |   |
* |                                                              |   |
* |                                                         Each |1st|
* |                                                              |   |
* | +------------------------------------------------------+     |   |
* | |info for font records                                 |<----+ <-+
* | +------------------------------------------------------+
* | |byte 1      -  nominal height of font                 |
* | |byte 2      -  hershey coord of top of capital M      |
* | | "   3      -  hershey coord of bottom of capital M   |
* | | "   4      -  hershey coord of left of capital M     |
* | | "   5      -  hershey coord of right of capital M    |
* | | "   6      -  hershey coord of bottom of lowercase g |
* | | "   7->196 -  list of ICN numbers each 2 bytes       |
* | | " 197->386 -  list of Hershey numbers each 2 bytes   |
* | +------------------------------------------------------+
* |
* |
* |
* |   +------------------------------------------------------+
* +-->|info for char index                                   |
*     +------------------------------------------------------+
*     |will be packed onto data base                         |
*     |byte 1&2  -  Hershey Number                           |
*     | "   3    -  left width(signed,negative)              |
*     | "   4    -  right width                              |
* +---| "   5&6  -  record number of record containing stroke|
* |   |             definitions repeated for each defined    |
* |   |             hershey character in sequence so that    |
* |   |             characters may also be refered to by     |
* |   |             their ICN i.e. Internal Character Number |
* |   |             (which is defined by this sequence)      |
* |   +------------------------------------------------------+
* |
* |
* |
* |
* |   +------------------------------------------------------+
* +-->|char definitions (may not cross record boundaries)    |
*     +------------------------------------------------------+
*     |byte 1&2  -  number of character definitions in record|
*     |byte 3&4  -  CIN of 1st character definition in record|
*     |byte 5->  -  stroke definitions packed 2 bytes / pair |
*     |             byte 1  -  x coordinate(signed)          |
*     |             byte 2  -  y coordinate(signed)          |
*     |      special values are :                            |
*     |  -64   0  =  end of polyline                         |
*     |  -64 -64  =  end of char                             |
*     +------------------------------------------------------+
*
*---------------------------------------------------------------------

*     Open the file

      CALL GKIOOP(KFDATA, KNIL, KDBFLU)
      IF (KERROR.NE.0) GOTO 999

*     read first record

      READ(UNIT=KDBFLU,REC=1) IGNORE,INDREC,NCHARS,IGNORE,
     :                        NMFNTS,KFRECS(1)

* must validate - if error it is still safe to use default font

      IF ((NCHARS.GT.KMXICN).OR.(NMFNTS.GT.KFNTMX)) THEN
        NMFNTS = 1
        FNAME(1) = '1'
        NCHARS = KMXICN
      ELSE
        READ(UNIT=KDBFLU,REC=1) IGNORE,IGNORE,IGNORE,IGNORE,
     :                          IGNORE,(KFRECS(I),I=1,NMFNTS)
     :                         ,(FNAME(I),I=1,NMFNTS)
      ENDIF
*     initialise the Cache as empty

      DO 1 I=1,KFNTMX
        KFNAMS(I) = 0
    1 CONTINUE
      DO 2 I=1,KMXICN
        KCHNAM(I) = 0
    2 CONTINUE
      KFREE = 1

*   Store font numbers in KHFONT array
      DO 20 I=1,NMFNTS

*      Trailing spaces have to be omitted before internal read
        DO 10 NCH = LEN(FNAME(1)),1,-1
          IF( FNAME(I)(NCH:NCH).NE.' ' ) GOTO 12
   10   CONTINUE
*   If all spaces, then set KHFONT(I) to zero (not really satisfactory)
        KHFONT(I)=0
        GOTO 20

*      Convert FNAME to KHFONT, using integer format tailored
*      exactly to the number of significant characters present
   12   WRITE( FFORM, '(A,I2,A)' )  '(I',NCH,')'
        READ( FNAME(I)(1:NCH), FFORM ) KHFONT(I)
   20 CONTINUE

*     read character index into CINDEX
*     and left and right widths

      NUMREC = INDREC
      READ(UNIT=KDBFLU,REC=NUMREC) CHARBF
      NUMREC = NUMREC+1
      NRCOFF = 1
      DO 1000 I=1,NCHARS
        IF (NRCOFF+5.GT.KFRCSZ) THEN
          READ(UNIT=KDBFLU,REC=NUMREC) CHARBF
          NUMREC = NUMREC+1
          NRCOFF = 1
        ENDIF
        CLWDTH(I) = CHARBF(NRCOFF+2:NRCOFF+2)
        CRWDTH(I) = CHARBF(NRCOFF+3:NRCOFF+3)
        CINDEX(I*2-1) = CHARBF(NRCOFF+4:NRCOFF+4)
        CINDEX(I*2) = CHARBF(NRCOFF+5:NRCOFF+5)
        NRCOFF = NRCOFF+6
 1000 CONTINUE

*     set current font to undefined

      KURFON = 0

  999 CONTINUE
      END
