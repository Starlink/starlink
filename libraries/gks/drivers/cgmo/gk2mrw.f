      SUBROUTINE GK2MRW(NRD,RX,RY,LIST)
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
*  This routine reads is given a list of points,
*  transforms them from WC to VDC (NDC), converts
*  them to hex, and writes them to the CGM
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*        NRD    : Total no of points to be handled (Input variable)
*        RX     : Array containing the x values of the points
*        RY     : Array containing the y values of the points
*        LIST   : Is true if a points list is to be written
*
      INTEGER NRD
      REAL RX(NRD), RY(NRD)
      LOGICAL LIST
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'

      INCLUDE '../../include/gkwca.cmn'
*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgm.cmn'

*  LOCALS
*  ------
*        I      : Loop counter
*        DX     : Variable containing the x value in NDC
*        DY     : Variable containing the y value in NDC

*        VDC    : Is true if the reals are to be VDC coordinates

*        MANTIS : The mantissa of a real
*        EXPONE : The exponent of a real
*        TEMPX  : Temporary variable
*        TEMPY  : Temporary variable

      INTEGER I
      REAL DX, DY, TEMPX,TEMPY
      CHARACTER*80 MANTIS*9, EXPONE*9

* Parameters
      INTEGER  VDCX, VDCY
      PARAMETER(VDCX=3,VDCY=4)

*---------------------------------------------------------------

      TEMPX=0.0
      TEMPY=0.0


*   Convert reals to hex & write to CGM
      KXEXP(KWKIX)=KVDEFX(KWKIX)
      KYEXP(KWKIX)=KVDEFX(KWKIX)
      DO 100 I=1,NRD
*   Convert reals into NDC
         CALL GKTWD(1,RX(I),RY(I),DX,DY)
         CALL GK2MRH((DX-TEMPX),MANTIS,EXPONE,VDCX)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
         CALL GK2MRH((DY-TEMPY),MANTIS,EXPONE,VDCY)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
         IF(LIST) THEN
            TEMPX=DX
            TEMPY=DY
         ENDIF
  100 CONTINUE
      RETURN
      END
