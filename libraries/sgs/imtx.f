      SUBROUTINE sgs_IMTX(HM,IF,IPR,HT,AR,XU,YU,SP,TXJ)
*+
*     - - - - - - -
*        I M T X
*     - - - - - - -
*
*   Inquire marker & text parameters
*
*   Remarks:
*
*     OBSOLETE ROUTINE - RETAINED FOR GKS 6.2 VERSION COMPATABILITY
*
*   Returned:
*        HM      r      Marker height (=width)
*        IF      i      Text font number
*        IPR     i      Text precision
*        HT      r      Text height
*        AR      r      Text aspect ratio (H/W)
*        XU,YU   r      Text orientation direction cosines
*        SP      r      Text spacing
*        TXJ     c      Text justification code
*
*   Read from COMMON:
*        HMK, IFONT, IPREC, HTX, ARTX, XUPTX, YUPTX, STX, CTXJ
*
*   P.T.Wallace, D.L.Terrett  Starlink  Jan 1987
*+
      IMPLICIT NONE

      REAL HM
      INTEGER IF,IPR
      REAL HT,AR,XU,YU,SP
      CHARACTER*2 TXJ

      INCLUDE 'sgscom'




      HM=HMK
      IF=IFONT
      IPR=IPREC
      HT=HTX
      AR=ARTX
      XU=XUPTX
      YU=YUPTX
      SP=STX
      TXJ=CTXJ

      END
