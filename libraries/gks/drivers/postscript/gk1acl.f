*---------------------------------------------------------------
      SUBROUTINE GK1ACL(IFLAG)
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
*     Send Picture Clear commands to the external PostScript file
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*
*     INP IFLAG  - Determines whether New Frame or Closing action required.
*
      INTEGER IFLAG
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
*  DUMMY   - Dummy character, required by the buffering routine.
*  FORMT   - Character variable used to prepare output format.
*  IPAGE   - A flag to indicate that the action concerns the a page
*            and not the whole postscript file.
*  IWHOLE  - A flag to indicate that the action concerns the whole
*            postscript file and not just a page.
*  IREM    - Dummy integer, required by the buffering routine.
*  NDIG    - Number of digits in the page counter (kept under IPAGES in KWKDAT)
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*

*     Integer workspace offset parameters
      INTEGER    ILNTYP,   IMKTYP,   IFASTY,   ICLIND
      PARAMETER (ILNTYP=1, IMKTYP=2, IFASTY=4, ICLIND=5)
      INTEGER    ICHWFT,   IPAGES  , IFORMT
      PARAMETER (ICHWFT=6, IPAGES=7, IFORMT=16)
*     Real  workspace offset parameters
      INTEGER    ILNWID,   IMKSZ,   ICCHHT,    ICCHAN
      PARAMETER (ILNWID=1, IMKSZ=2, ICCHHT=3,  ICCHAN=4)
      INTEGER    ICLPXL,   ICLPYB,   ICLPXR,   ICLPYT,   IMARGN
      PARAMETER (ICLPXL=6, ICLPYB=7, ICLPXR=8, ICLPYT=9, IMARGN=10)
*
      INTEGER IREM
*     Wholefile Page and EPSF flags
      INTEGER    IWHOLE,   IPAGE,    IIEPSF
      PARAMETER (IWHOLE=0, IPAGE=1,  IIEPSF=1)
*
      INTEGER NDIG
      CHARACTER S*40, FORMT*22, DUMMY
*
*
*  ALGORITHM
*  ---------
*     Increment Page Counter, showpage then restore
*
*     If the action is not to close the wholefile (ie, new page),
*        reset all the locally help graphics data
*        If EPSF,
*            End dictionary holding prolog definitions
*            Add trailer, Close the file,
*            Open new file, Add Header and Prolog.
*        Else
*            Add page trailer, Start new page.
*     Elseif non-EPSF
*        Add final page trailer.
*
* --------------------------------------------------------------
*

*
*     Increment the page counter
*
      KWKDAT(IPAGES,KWKIX) = KWKDAT(IPAGES,KWKIX)+1

*
*     Start from a new line in the external file
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)


*     Send the page, supply the matching restore.
      CALL GKFOCO(KIOPB,'showpage restore',IREM)
      CALL GKFOCO(KIOSN, ' ', IREM)

*
*     Find out whether this is a closing (wholefile)
*                         or a new frame (page) action.
*
      IF(IFLAG.NE.IWHOLE) THEN
*        New frame: Since restore will in effect reinstate all the graphics
*        parameters to those at initialisation, must reset all the locally
*        held graphics data (this will force the attributes refresh action
*        as soon as any of the output primitives is called first).
         KWKDAT(ILNTYP,KWKIX) = KNIL
         KWKDAT(IMKTYP,KWKIX) = KNIL
         KWKDAT(IFASTY,KWKIX) = KNIL
         KWKDAT(ICLIND,KWKIX) = KNIL
         KWKDAT(ICHWFT,KWKIX) = KNIL
*
         QWKDAT(ILNWID,KWKIX) = QNIL
         QWKDAT(IMKSZ, KWKIX) = QNIL
         QWKDAT(ICCHHT,KWKIX) = QNIL
         QWKDAT(ICCHAN,KWKIX) = QNIL
         QWKDAT(ICLPXL,KWKIX) = QNIL
         QWKDAT(ICLPYB,KWKIX) = QNIL
         QWKDAT(ICLPXR,KWKIX) = QNIL
         QWKDAT(ICLPYT,KWKIX) = QNIL
*
         IF(KWKDAT(IFORMT,KWKIX) .EQ. IIEPSF)THEN
*          If EPSF,
*          Finish the file (since it may only have one page) and
*          start a new file for the next page.
*
*          End dictionary (began by GK1APS)
           CALL GKFOCO(KIOPB,'end',IREM)
           CALL GKFOCO(KIOSN,DUMMY,IREM)
*          Send trailer
           CALL GK1ATR(IWHOLE)
*          flush the buffer
           CALL GKFOCO(KIOSN,DUMMY,IREM)
*          disconnect ws - to close the file
           CALL GKIOCL(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
*          reconnect ws  - to open new file
           CALL GKIOOP(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
           IF(KERROR .EQ. 0)THEN
*             initialise buffer
              CALL GKFOCO(KIOIT,DUMMY,IREM)
*             initialise page counter
              KWKDAT(IPAGES,KWKIX) = 0
*             send header and prologue
              CALL GK1AHD(IWHOLE)
              CALL GK1APS
           ENDIF
         ELSE
*          If not EPSF,
*          Send Page Trailer
           CALL GK1ATR (IPAGE)
*
*          Comment beginning of a new page - send the right number and no
*          leading spaces.
*
           IF(KWKDAT(IPAGES,KWKIX)+1.LE.9) THEN
              NDIG=1
           ELSEIF(KWKDAT(IPAGES,KWKIX)+1.LE.99) THEN
              NDIG=2
           ELSEIF(KWKDAT(IPAGES,KWKIX)+1.LE.999) THEN
              NDIG=3
           ELSE
              CALL GKBUG(-2004,'GK1ACL')
              RETURN
           ENDIF
*          Prepare the output format
           CALL GKFOCO(KIOSN,DUMMY,IREM)
           WRITE(FORMT, '(A,I1,A)') '(''%%Page: ? '',I',NDIG,')'
*          Write new frame number out to the buffer
           WRITE(S,FORMT)KWKDAT(IPAGES,KWKIX)+1
*          Send the comment
           CALL GKFOCO(KIOPB,S(1:10+NDIG),IREM)
           CALL GKFOCO(KIOSN,DUMMY,IREM)
*          Send Page Header
           CALL GK1AHD (IPAGE)
*          Do a save.
           CALL GKFOCO(KIOPB,'save',IREM)
           CALL GKFOCO(KIOSN,DUMMY,IREM)
         ENDIF
      ELSEIF(KWKDAT(IFORMT,KWKIX) .NE. IIEPSF)THEN
*     Closing Workstation - Final PageTrailer  for non-EPSF
         CALL GK1ATR (IPAGE)
      ENDIF


      END
