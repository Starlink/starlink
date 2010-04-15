*---------------------------------------------------------------------
      SUBROUTINE GK1AST(ICOLOR,LNTYPE,RLNWD)
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
*     Set up device colour, device linetype and device linewidth.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*
*     INP ICOLOR - Colour index to be set
*     INP LNTYPE - Linetype to be set
*     INP RLNWD  - Linewidth to be set
*
      INTEGER ICOLOR, LNTYPE
      REAL RLNWD
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
*  CHANGE  - Logical flag.
*  DUMMY   - Dummy character, required by the buffering routine.
*  IREM    - Dummy integer, required by the buffering routine.
*  RVW     - Real for temporary storage
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*  SMALL   - Small real for equality comparisons
*

*     small real
      REAL       SMALL
      PARAMETER (SMALL=1.0E-4)

*     Offsets in KWKDAT and QWKDAT
      INTEGER    ILNTYP,   ILNWID,   ICLIND
      PARAMETER (ILNTYP=1, ILNWID=1, ICLIND=5)
*
      CHARACTER S*50, DUMMY
      INTEGER IREM
      REAL RVW
      LOGICAL CHANGE
*
*  ALGORITHM
*  ---------
*     Background colour may be changed to an arbitrary colour, but
*     only when output page is empty. The attributes for colour,
*     linetype and linewidth are common to nearly all output primitives,
*     write them out if any has changed.
*
*---------------------------------------------------------------------
*

*     Find out whether setting the attributes for the first time in this
*     frame (i.e. before anything has been output) and if yes check whether
*     need to paint the background.
      IF(KWKDAT(ICLIND,KWKIX).EQ.KNIL.AND.
     :   KWKDAT(ILNTYP,KWKIX).EQ.KNIL) THEN
*        Don't bother if background is white.
         IF(
     :       ABS(QHP(KHPXR(KCTBPT(1,KWKIX))) +
     :           QHP(KHPXR(KCTBPT(2,KWKIX))) +
     :           QHP(KHPXR(KCTBPT(3,KWKIX))) - 3.0).GT.SMALL
     :     )  CALL GK1ABG
      ENDIF


*     Initialise the flag
      CHANGE = .FALSE.

*
*     See if locally stored copy of the attributes needs updating.
*
*     Colour index:
      IF(KWKDAT(ICLIND,KWKIX).NE.ICOLOR) THEN
         KWKDAT(ICLIND,KWKIX) = ICOLOR
         CHANGE = .TRUE.
      ENDIF

*     Linestyle:
      IF(KWKDAT(ILNTYP,KWKIX).NE.LNTYPE) THEN
         KWKDAT(ILNTYP,KWKIX) = LNTYPE
         CHANGE = .TRUE.
      ENDIF

*     Width: validate, then check.
      RVW = AMAX1(QMNLNW(KWKIX),RLNWD)
      RVW = AMIN1(QMXLNW(KWKIX),RVW)
*
      IF(ABS(QWKDAT(ILNWID,KWKIX)-RVW).GT.SMALL) THEN
         QWKDAT(ILNWID,KWKIX) = RVW
         CHANGE = .TRUE.
      ENDIF

*
*     If change has occurred write the attributes out.
*
      IF (CHANGE) THEN
*        Start from a new line in the external file
         CALL GKFOCO(KIOSN,DUMMY,IREM)
         WRITE(S, 50) QHP(KHPXR(KCTBPT(1,KWKIX))+ICOLOR),
     :                QHP(KHPXR(KCTBPT(2,KWKIX))+ICOLOR),
     :                QHP(KHPXR(KCTBPT(3,KWKIX))+ICOLOR),
     :                KWKDAT(ILNTYP,KWKIX), QWKDAT(ILNWID,KWKIX)
   50    FORMAT(3F8.5, I3, F8.2, ' clnstat')
         CALL GKFOCO(KIOPB, S(1:43), IREM)
      END IF

      END
