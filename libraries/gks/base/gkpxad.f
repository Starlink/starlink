C# IL>=a, OL>=0
      SUBROUTINE GKPXAD
*
* (C) COPYRIGHT ICL & SERC  1988
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     When called from workstation entry point 120
*     returns pixel array dimensions using the raster dimensions
*     from the workstation description table
*
*  MAINTENANCE LOG
*  ---------------
*     07/04/88  KEVP  Created to fix bug S339.
*     07/04/88  KEVP  Made it ignore error arising from any point
*                     being outside of Normalisation rectangle.
*     06/07/88  KEVP  Made it round upwards for right and top sides
*                     instead of adding one (now superceded)
*     26/07/88  KEVP  Included those pixels, whose centres lie in the box
*                     and made valid for negative coordinates
*                     as required in GKS standard.
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     WCX,WCY  World Coords
*     UCX,UCY  Normalised Device Coords
*     DCX,DCY  Device Coords
*     RCX,RCY  Raster Coords
*
      REAL WCX(2),WCY(2),UCX(2),UCY(2),DCX(2),DCY(2),RCX(2),RCY(2)
*
*  ERRORS
*  ------
*
*  ALGORITHM
*  ---------
*     (1) The box converted from WC to NDC according to the
*     current normalisation transformation (error 152 is ignored).
*
*     (2) It is then converted from NDC to DC to take account of
*     the workstation transformation
*
*     (3) It is then converted from DC to a coordinate system RC
*     that becomes integer raster coordinates when rounded DOWN.
*
*     (4) The coordinates are rounded individually by NINT to
*     ensure that a pixel is counted if and only if its CENTRE
*     is in the box.
*
*  COMMENTS
*  --------
*     GKQWK (entry pt KQPXAD=120) does not work if there is a
*     transformed open segment. In such a case GKPXAD ignores
*     the open segment transformation so the the rectangle
*     remains an unrotated rectangle.
*
*     As the total workstation transformation QWTOTT is not used
*     no assumptions are made about the Normalisation Transformation
*     entry point (KNT=31).
*
*---------------------------------------------------------------------
*
*     Data expected:
*
*     QWR1,QWR2  Coords of one corner P
*     QWR3,QWR4  Coords of opposite corner Q
*
*     Data returned:
*
*     KWI1   Number of pixel columns
*     KWI2   Number of pixel rows
*
      WCX(1)=QWR1
      WCY(1)=QWR2
      WCX(2)=QWR3
      WCY(2)=QWR4

* transform to DC indirectly to avoid QWTOTT
      CALL GKTWN (KCNTN,2,WCX,WCY,UCX,UCY)
      IF(KERROR .EQ. 152)KERROR = 0
      CALL GKTND (2,UCX,UCY,DCX,DCY)
      IF(KERROR .EQ. 152)KERROR = 0
* convert to raster coordinates
      CALL GKTDR(2,DCX,DCY,RCX,RCY)
* round with NINT for pixel centres
      KWI1=ABS(NINT(RCX(2)) - NINT(RCX(1)))
      KWI2=ABS(NINT(RCY(2)) - NINT(RCY(1)))

      RETURN
      END
