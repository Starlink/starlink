      SUBROUTINE GK2MIC
*---------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine initialises the common block
*  used by the GKS-CGM converter
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'

      INCLUDE '../../include/gkwca.cmn'
*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgm.cmn'

*  Common block variable set:
*       KCHANO : Pointer to current position in output buffer

*       KVDCTY : VDC Type (integer points=0 | real points=1)
*       KINTPR : Integer Precision (Max Integer code + 1)
*       KMXRPR : Maximum Real Code + 1
*       KMNRPR : Minimum Real Code (Smallest Exponent)
*       KDEFEX : Default Exponent for reals
*       KEXPAL : Exponents Allowed (Allowed=0 | Forbidden=1)
*       KIXPR  : Index Precision (Max No of bits)
*       KCOLPR : Colour Precision (No of bits for each RGB component)
*       KCIXPR : Colour Index Precision (No of bits for index)
*       KMXCIX : Maximum Colour Index

*       KSCAMD : Scaling Mode (abstract=0 | metric=1)
*       KCSLMD : Colour Selection Mode (indexed=0 | direct=1)
*       KLWSMD : Line Width Specification Mode (absolute=0 | scaled=1)
*       KMSSMD : Marker Size Specification Mode(absolute=0 | scaled=1)
*       KEWSMD : Edge Width Specification Mode (absolute=0 | scaled=1)

*       KVDCIP : VDC Integer Precision (largest integer code + 1)
*       KVMXRP : Maximum VDC Real Code + 1
*       KVMNRP : Minimum VDC Real Code (Smallest Exponent)
*       KVDEFX : Default VDC Exponent
*       KVXALL : VDC Exponents allowed (allowed=0 | forbidden=1)
*       KAUXCL : Auxiliary Colour (when transparency off; meaningful?)
*       KTRANS : Transparency (off=0 | on=1)
*       QCLREC : Clip rectangle
*       KCLPIN : Clip indicator (off=0 | on=1)


*       CLRWKN : Clear workstation necessary flag (.TRUE. | .FALSE.)
*       LIXSET : Line Index Set Representation flag (TRUE | FALSE)
*       TIXSET : Text Set Representation flag (TRUE | FALSE)
*       MIXSET : Marker Index Set Representation flag (TRUE | FALSE)
*       ISGDP  : Flag showing whether GDP or not

*       KSTLST(*,1) : Line type
*       QSTLST(*,1) : Linewidth scale factor
*       KSTLST(*,2) : Polyline colour index

*       KSTLST(*,3) : Marker type
*       QSTLST(*,2) : Marker size scale factor
*       KSTLST(*,4) : Polymarker colour index

*       KFABI  : Current Fill Area Bundle index
*       KFAINS : Fill Area interior style
*       KFASIN : Fill Area style index
*       KFACIN : Fill Area colour index
*       KSETAS : Array containing ASFs set by this program (not always
*                 the same, due to CGM lacking a set bundle representn)
*
*  LOCALS
*  ------
      INTEGER  I,J
*
*  Parameters
*  ----------
      INTEGER  OLD,NEW
      PARAMETER (OLD=41,NEW=42)
*
*---------------------------------------------------------------

      KCHANO(KWKIX) = 1
      KVDCTY(KWKIX) = 1
      KINTPR(KWKIX) = 16
      KMXRPR(KWKIX) = 16
      KMNRPR(KWKIX) = -16
      KDEFEX(KWKIX) = -12
      KEXPAL(KWKIX) = 0
      KIXPR (KWKIX) = 10
      KCOLPR(KWKIX) = 8
      KCIXPR(KWKIX) = 10
      KMXCIX(KWKIX) = 256

      KSCAMD(KWKIX) = 0
      KCSLMD(KWKIX) = 0
      KLWSMD(KWKIX) = 1
      KMSSMD(KWKIX) = 1
      KEWSMD(KWKIX) = 1

      KVDCIP(KWKIX) = 16
      KVMXRP(KWKIX) = 16
      KVMNRP(KWKIX) = -16
      KVDEFX(KWKIX) = -12
      KVXALL(KWKIX) = 0
      KAUXCL(KWKIX) = 1
      KTRANS(KWKIX) = 1
      KCLPIN(KWKIX) = 1

      CLRWKN(KWKIX) = .TRUE.
      DO 10 I=1,40
         LIXSET(I,KWKIX) =.FALSE.
         MIXSET(I,KWKIX) =.FALSE.
         TIXSET(I,KWKIX) =.FALSE.
         FASET (I,KWKIX) =.FALSE.
 10   CONTINUE
      DO 20 I=1,13
         KSETAS(OLD,I,KWKIX) =0
         KSETAS(NEW,I,KWKIX) =0
 20   CONTINUE
      KFABI(KWKIX) =0
      ISGDP(KWKIX) =.FALSE.

      DO 40 J=OLD,NEW
         DO 30 I=1,16
            KSTLST(J,I,KWKIX)=1
            QSTLST(J,I,KWKIX)=0.0
 30      CONTINUE
*  Set marker type to asterisk
         KSTLST(J,3,KWKIX)=3
*  Set text precision to string
         KSTLST(J,8,KWKIX)=0
*  Set text path to right
         KSTLST(J,10,KWKIX)=0
*  Set text alignment to normal
         KSTLST(J,11,KWKIX)=0
         KSTLST(J,12,KWKIX)=0
*  Set interior style to hollow
         KSTLST(J,13,KWKIX)=0
*  Set linewidth
         QSTLST(J,1,KWKIX)=1.0
*  Set marker size
         QSTLST(J,2,KWKIX)=1.0
*  Set character expansion factor
         QSTLST(J,3,KWKIX)=1.0
         QSTLST(J,17,KWKIX)=1.0
         QSTLST(J,18,KWKIX)=1.0
*  VDC extent
         QSTLST(J,19,KWKIX)=0.0
         QSTLST(J,20,KWKIX)=0.0
         QSTLST(J,21,KWKIX)=1.0
         QSTLST(J,22,KWKIX)=1.0
 40   CONTINUE
         DO 50 I=0,256
            QCLREP(I,1,KWKIX)=-1.0
 50      CONTINUE

      RETURN
      END
