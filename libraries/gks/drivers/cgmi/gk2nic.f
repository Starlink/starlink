      SUBROUTINE GK2NIC
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) Workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     This routine initialises the common block
*     used by the CGM-GKS converter
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*     None

*  COMMON BLOCKS
*  -------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgn.cmn'

*  VARIABLES IN COMMON
*  -------------------
*       KINTPR : Integer Precision (Max Integer code + 1)
*       KLENRC : Record length of the input file
*       KMXRPR : Maximum Real Code + 1
*       KMNRPR : Minimum Real Code (Smallest Exponent)
*       KDEFEX : Default Exponent for reals
*       KEXPAL : Exponents Allowed (Allowed=0 | Forbidden=1)
*       KIXPR  : Index Precision (Max No of bits)
*       KCOLPR : Colour Precision (No of bits for each RGB component)
*       KCIXPR : Colour Index Precision (No of bits for index)

*       KVMXRP : Maximum VDC Real Code + 1
*       KVMNRP : Minimum VDC Real Code (Smallest Exponent)
*       KVDEFX : Default VDC Exponent
*       KVDCIP : VDC Integer Precision
*       KVDCTY : VDC Type
*       KSETAS : Array containing ASFs set by this program (not always
*                 the same, due to CGM lacking a set bundle representn)
*       FIRSTP : First Pass flag
*       METDEF : Metafile replacements flag

*  LOCAL
*  -----
*    I    : Loop Variable
*
      INTEGER I

*---------------------------------------------------------------------

*   Set asfs to individual
      DO 10 I=1,13
          KSETAS(I,KWKIX)=1
  10  CONTINUE

* Initialise first pass flag to true
      FIRSTP(KWKIX)=.TRUE.
* Initialise metafile defaults replacement flag to false
      METDEF(KWKIX)=.FALSE.
      BEGMET(KWKIX)=.TRUE.
      DOREP(KWKIX)=.FALSE.

      KLENRC=80
      KCHANI(KWKIX)=KLENRC+1

      KINTPR(KWKIX)=10
      KMXRPR(KWKIX)=10
      KMNRPR(KWKIX)=-10
      KDEFEX(KWKIX)=-10
      KEXPAL(KWKIX)=1
      KIXPR(KWKIX)=10
      KCOLPR(KWKIX)=6
      KCIXPR(KWKIX)=10
      KMXCIX(KWKIX)=63
      DO 20 I=1,3
         QMNCOL(I)=0.0
         QMXCOL(I)=63.0
  20  CONTINUE
      KVMXRP(KWKIX) = 10
      KVMNRP(KWKIX) = -10
      KVDEFX(KWKIX) = -10
      KVDCIP(KWKIX) = 20
      KVXALL(KWKIX) = 1
      KVDCTY(KWKIX) = 0

*  Initialise Character substitution codes
      KNSUBS(KWKIX)=0

      RETURN
      END
