*---------------------------------------------------------------------
      SUBROUTINE GK1ACB
*---------------------------------------------------------------------
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
*    Establish Clipping Boundary and send it to the PostScript external file
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*     None
*
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
*  CHANGE  - Logical flag.
*  DUMMY   - Dummy character, required by the buffering routine.
*  IREM    - Dummy integer, required by the buffering routine.
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*

*     Integer workspace offset parameters
      INTEGER    ILNTYP,   IMKTYP,   IFASTY,   ICLIND
      PARAMETER (ILNTYP=1, IMKTYP=2, IFASTY=4, ICLIND=5)
      INTEGER    ICHWFT
      PARAMETER (ICHWFT=6)
*     Real  workspace offset parameters
      INTEGER    ILNWID,   IMKSZ,   ICCHHT,   ICCHAN
      PARAMETER (ILNWID=1, IMKSZ=2, ICCHHT=3, ICCHAN=4)
      INTEGER    ICLPXL,   ICLPYB,   ICLPXR,   ICLPYT
      PARAMETER (ICLPXL=6, ICLPYB=7, ICLPXR=8, ICLPYT=9)
*
      LOGICAL CHANGE
      INTEGER IREM
      CHARACTER S*100, DUMMY
*
*  ALGORITHM
*  ---------
*     In PostScript there is no way to enlarge the current clipping path. To
*     get round this problem prior to setting the new clipping boundary we do
*     a restore, which brings back the largest possible (saved at initialisa-
*     tion) clipping boundary.
*
*---------------------------------------------------------------------
*

*     Initialise the flag
      CHANGE = .FALSE.

*
*     See if locally stored copy of the clipping boundary needs updating.
*

*     Lower left corner:
      IF(QWKDAT(ICLPXL,KWKIX).NE.QWCLXL(KWKIX)) THEN
         QWKDAT(ICLPXL,KWKIX) = QWCLXL(KWKIX)
         CHANGE = .TRUE.
      ENDIF
*
      IF(QWKDAT(ICLPYB,KWKIX).NE.QWCLYB(KWKIX)) THEN
         QWKDAT(ICLPYB,KWKIX) = QWCLYB(KWKIX)
         CHANGE = .TRUE.
      ENDIF

*     Upper right corner:
      IF(QWKDAT(ICLPXR,KWKIX).NE.QWCLXR(KWKIX)) THEN
         QWKDAT(ICLPXR,KWKIX) = QWCLXR(KWKIX)
         CHANGE = .TRUE.
      ENDIF
*
      IF(QWKDAT(ICLPYT,KWKIX).NE.QWCLYT(KWKIX)) THEN
         QWKDAT(ICLPYT,KWKIX) = QWCLYT(KWKIX)
         CHANGE = .TRUE.
      ENDIF

*
*     If change has occurred change the clipping rectangle.
*
      IF (CHANGE) THEN
*
*     Start from a new line in the external file.
*
         CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Restore to the original clipping rectangle to allow for enlarging as
*     well as for shrinking of the clipping rectangle.
*

         CALL GKFOCO(KIOPB,'restore save ',IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Since restore will in effect reinstate all the graphics parameters
*     to those at initialisation, must reset all the locally held graphics
*     data (this will force the attributes refresh action as soon as any of
*     the output primitives is called).

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

*
*     Now send the new clipping rectangle vertices
*

         WRITE(S,110) QWKDAT(ICLPXL,KWKIX), QWKDAT(ICLPYB,KWKIX),
     :                QWKDAT(ICLPXL,KWKIX), QWKDAT(ICLPYT,KWKIX),
     :                QWKDAT(ICLPXR,KWKIX), QWKDAT(ICLPYT,KWKIX),
     :                QWKDAT(ICLPXR,KWKIX), QWKDAT(ICLPYB,KWKIX)
*        the setclipbox procedure will establish the boundary
  110    FORMAT(8F11.3, ' setclipbox')
         CALL GKFOCO(KIOPB, S(1:99), IREM)

      ENDIF



      END
