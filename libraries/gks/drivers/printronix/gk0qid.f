


      SUBROUTINE GK0QID
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
*  Initialise device
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
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwsl.cmn'

*
*     Intrinsic functions declaration
*
      INTRINSIC AMIN1,AMAX1
*
*  LOCALS
*  ------
*
*     Offsets in KWKDAT
*
      INTEGER NWSTYP,LPORT,LLAND
      PARAMETER (NWSTYP=6,LPORT=0,LLAND=1)
*
*     Offsets in QWKDAT
*
      INTEGER NXSC,NYSC
      PARAMETER (NXSC=1,NYSC=2)
*
      REAL XSCALE,YSCALE
      PARAMETER (XSCALE=2362.2039,YSCALE=2834.6455)

*
*
*  ALGORITHM
*  ---------
*     Set the WS orientation, create bitmap
*     and initialise where necessary.
*
*--------------------------------------------------------------------

*     Set the WS type (PORTrait or LANDscape)
      IF(KDSRY(KWKIX).GT.KDSRX(KWKIX))THEN
*        PORTrait
         KWKDAT(NWSTYP,KWKIX)=LPORT
      ELSE
*        LANDscape
         KWKDAT(NWSTYP,KWKIX)=LLAND
      ENDIF

*     Set the scale factors(they are different because of the
*     different resolution along the axis of the Printronix).

      QWKDAT(NXSC,KWKIX)=XSCALE
      QWKDAT(NYSC,KWKIX)=YSCALE

*     Create bitmap
      CALL GK0QVM

*     Clean up after error when opening workstation
      IF (KERROR.NE.0) THEN
         CALL GKIOCL(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
      ENDIF

      END
