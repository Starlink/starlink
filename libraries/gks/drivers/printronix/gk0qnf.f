


      SUBROUTINE GK0QNF
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           PLP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*  Draw border around workstation viewport
*
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwsl.cmn'

*
*  LOCALS
*  ------
*
*     Offsets in KWKDAT
*
      INTEGER NVIS
      PARAMETER (NVIS=4)
*
      INTEGER NWSTYP,LLAND
      PARAMETER (NWSTYP=6,LLAND=1)
*
*     Offsets in QWKDAT
*
      INTEGER NXSC,NYSC
      PARAMETER (NXSC=1,NYSC=2)
*
      REAL X(5),Y(5)
*
*  ALGORITHM
*  ---------
*     Read current WS viewport settings and call Polyline primitive
*
*---------------------------------------------------------------

*     Determine the Workstation orientation
      IF(KWKDAT(NWSTYP,KWKIX).EQ.LLAND)THEN
*
*        LANDscape
*        Read current values and set coordinates for Polyline
*
         X(1)=0.0
         X(2)=X(1)
         X(5)=X(1)
         X(3)=QDSDX(KWKIX)*QWKDAT(NXSC,KWKIX)
         X(4)=X(3)
*
         Y(1)=0.0
         Y(4)=Y(1)
         Y(5)=Y(1)
         Y(2)=QDSDY(KWKIX)*QWKDAT(NYSC,KWKIX)
         Y(3)=Y(2)
      ELSE
*
*        PORTrait
*        Adjust current values to comply with PORTrait orientation:
*        X coordinates will be displayed as Y, so scale accordingly.
*        Also translate the origin to what what was normally upper
*        left.
*
         X(1)=0.0
         X(2)=X(1)
         X(5)=X(1)
         X(3)=QDSDX(KWKIX)*QWKDAT(NYSC,KWKIX)
         X(4)=X(3)
*
         Y(1)=0.0
         Y(4)=Y(1)
         Y(5)=Y(1)
         Y(2)=QDSDY(KWKIX)*QWKDAT(NXSC,KWKIX)
         Y(3)=Y(2)
      ENDIF

*     Set the visibility index to "visible"
      KWKDAT(NVIS,KWKIX)=1
*     Now draw the border(we need no clipping)
      CALL GK0QLN(5,X,Y)


      END
