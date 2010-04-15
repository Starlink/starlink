*---------------------------------------------------------------
      SUBROUTINE GK1ATR (IFLAG)
*---------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send PostScript Trailer to the external file
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*      INP IFLAG  Flag to indicate Trailer for whole file or PageTrailer

       INTEGER IFLAG
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
*  DUMMY   - Dummy character, required by the buffering routine.
*  FORMT   - Character variable used to prepare output format.
*  I,JJ    - Count variables
*  IBBXL   - Bounding Box (left)
*  IBBXR   - Bounding Box (right)
*  IBBYB   - Bounding Box (base)
*  IBBYT   - Bounding Box (top)
*  IEND    - Pointer to the fontname's last character in a heap allocation
*  ILEN    - Length of current fontname
*  IPAGE   - Value of flag indicating PageTrailer
*  IREM    - Dummy integer, required by the buffering routine.
*  ISTRT   - Pointer to the fontname's first character in a heap allocation
*  IWHOLE  - Value of flag indication Trailer for whole file
*  NDIG    - Number of digits in the page counter (kept under IPAGES in KWKDAT)
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*

*     Integer workspace offset parameters
      INTEGER    IPAGES,   IFTINT,   IFTNAM,   IFTPSE,     IFTUSD
      PARAMETER (IPAGES=7, IFTINT=8, IFTNAM=9, IFTPSE=10 , IFTUSD=12)
*
      INTEGER    IORIEN,   IFORMT,   IMARGN
      PARAMETER (IORIEN=14,IFORMT=16,IMARGN=10)
*
*     File Formats - Values of KWKDAT(IFORMT,KWKIX)
      INTEGER    NOEPSF,   IIEPSF
      PARAMETER (NOEPSF=0, IIEPSF=1)
*
*     Workstation Orientation - Values of KWKDAT(IORIEN,KWKIX)
      INTEGER    LPORT,     LLAND
      PARAMETER (LPORT = 0, LLAND = 1)
*
*     Values of IFLAG
      INTEGER    IWHOLE,   IPAGE
      PARAMETER (IWHOLE=0, IPAGE=1)
*
*     Number of Points per Metre
      REAL       SCFACT
      PARAMETER (SCFACT=72.0/0.0254)

      INTEGER I, IREM, NDIG, ILEN, ISTRT, IEND, JJ
      INTEGER IBBXL,IBBXR,IBBYB,IBBYT
*
      CHARACTER S*255, FORMT*21, DUMMY
*
*  ALGORITHM
*  ---------
*     We haven't supplied all the information in the (page)header
*     header (see GK1AHD), so it must be done here.
*
*     This information consists of:
*               Fonts used
*               Number of Pages
*               Bounding Box
*
* --------------------------------------------------------------
*
*     Get Bounding Box from Current Workstation Viewport, if required.
*
      IF( (IFLAG.EQ.IWHOLE .AND. KWKDAT(IFORMT,KWKIX).EQ.IIEPSF)
     :.OR.(IFLAG.EQ.IPAGE  .AND. KWKDAT(IFORMT,KWKIX).EQ.NOEPSF))THEN
*       Convert current workstation viewport from DC to Postscript Points
*       in the DEFAULT postscript user coordinate system.
        IF(KWKDAT(IORIEN,KWKIX) .EQ. LPORT)THEN
           IBBXL = NINT(SCFACT*QCWVXL(KWKIX)+QWKDAT(IMARGN,KWKIX))
           IBBXR = NINT(SCFACT*QCWVXR(KWKIX)+QWKDAT(IMARGN,KWKIX))
           IBBYB = NINT(SCFACT*QCWVYB(KWKIX)+QWKDAT(IMARGN,KWKIX))
           IBBYT = NINT(SCFACT*QCWVYT(KWKIX)+QWKDAT(IMARGN,KWKIX))
        ELSE
           IBBXL = NINT(SCFACT*QCWVYB(KWKIX)+QWKDAT(IMARGN,KWKIX))
           IBBXR = NINT(SCFACT*QCWVYT(KWKIX)+QWKDAT(IMARGN,KWKIX))
           IBBYB = NINT(SCFACT*QCWVXL(KWKIX)+QWKDAT(IMARGN,KWKIX))
           IBBYT = NINT(SCFACT*QCWVXR(KWKIX)+QWKDAT(IMARGN,KWKIX))
        ENDIF
      ELSE
        IBBXL = KNIL
        IBBXR = KNIL
        IBBYB = KNIL
        IBBYT = KNIL
      ENDIF
*

*     Determine whether its a Trailer or a PageTrailer that is needed
      IF(IFLAG .EQ. IWHOLE)THEN
*     Trailer for whole file
*
*        Declare that a Trailer follows:
*
         CALL GKFOCO(KIOSN,DUMMY,IREM)
         CALL GKFOCO(KIOPB, '%%Trailer', IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*        Write Bounding Box into trailer, if EPSF
*
         IF(KWKDAT(IFORMT,KWKIX) .EQ. IIEPSF)THEN
           WRITE(S,50) IBBXL,IBBYB,IBBXR,IBBYT
   50      FORMAT( '%%BoundingBox: ',4I5)
           CALL GKFOCO(KIOPB,S(1:35),IREM)
           CALL GKFOCO(KIOSN,DUMMY,IREM)
         ENDIF
*
*        Send information about the fonts used:
*

*        First prepend the comment text:
         CALL GKFOCO(KIOPB,
     :    '%%DocumentFonts:',
     :    IREM)

*
*        Set up a loop to find which fonts were used
         DO 100 I=1,KWKDAT(IFTINT,KWKIX)
            IF(KHP(KHPXI(KWKDAT(IFTUSD,KWKIX))+I-1).EQ.1)THEN
*              Get the fontname information from the heap
               ISTRT = KHP(KHPXI(KWKDAT(IFTPSE,KWKIX))+I-1)
               IEND  = KHP(KHPXI(KWKDAT(IFTPSE,KWKIX))+I)-1
*              Include spacing in length
               ILEN  = IEND-ISTRT+2
*              See whether can append the fontname to this buffer
               CALL GKFOCO(KIOQS,DUMMY,IREM)
               IF(IREM-2.LT.ILEN)THEN
*                  No room - flush this buffer and prepare new
                   CALL GKFOCO(KIOSN,DUMMY,IREM)
                   CALL GKFOCO(KIOPB,
     :             '%%DocumentFonts:',
     :             IREM)
               ENDIF
*              Put the fontname in the buffer
               DO 200 JJ=2,ILEN
                  S(JJ:JJ)=CHP(KHPXC(KWKDAT(IFTNAM,KWKIX))+ISTRT+JJ-3)
  200          CONTINUE
               S(1:1)=' '
               CALL GKFOCO(KIOPB, S(1:ILEN), IREM)
            ENDIF
  100    CONTINUE

*
*        Finish off with comment on total number of pages in the document
*
         IF(KWKDAT(IPAGES,KWKIX).LE.9) THEN
            NDIG=1
         ELSEIF(KWKDAT(IPAGES,KWKIX).LE.99) THEN
            NDIG=2
         ELSEIF(KWKDAT(IPAGES,KWKIX).LE.999) THEN
            NDIG=3
         ELSE
            NDIG=3
         ENDIF

*        Prepare the output format
         CALL GKFOCO(KIOSN,DUMMY,IREM)
         WRITE(FORMT, '(A,I1,A)') '(''%%Pages: '',I',NDIG,')'
*        Write new frame number out to the buffer
         WRITE(S,FORMT)KWKDAT(IPAGES,KWKIX)
*        Send the comment
         CALL GKFOCO(KIOPB,S(1:9+NDIG),IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)

      ELSEIF(IFLAG .EQ. IPAGE)THEN
*     PageTrailer
*
*        Declare that a PageTrailer follows:
*
         CALL GKFOCO(KIOSN,DUMMY,IREM)
         CALL GKFOCO(KIOPB, '%%PageTrailer', IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*        Write Page Bounding Box into Trailer, if not EPSF
*
         IF(KWKDAT(IFORMT,KWKIX) .NE. IIEPSF)THEN
           WRITE(S,300) IBBXL,IBBYB,IBBXR,IBBYT
  300      FORMAT( '%%PageBoundingBox: ', 4I5)
           CALL GKFOCO(KIOPB,S(1:39),IREM)
           CALL GKFOCO(KIOSN,DUMMY,IREM)
         ENDIF
*
      ENDIF

*
      END
