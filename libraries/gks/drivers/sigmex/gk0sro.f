      SUBROUTINE GK0SRO(X,Y,NXPIX,NYPIX,NXDIM,ICOLAR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             RSK
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Outputs a NXPIX x NYPIX colour array.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP X,Y    - coordinates of raster origin
*     INP NXPIX  - No of pixels per scanline
*     INP NYPIX  - No of scanlines in raster
*     INP NXDIM  - First dimension of colour array
*     INP ICOLAR - Integer colour array to output
*
      REAL    X, Y
      INTEGER NXPIX, NYPIX, NXDIM, ICOLAR(NXDIM,NYPIX)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      CHARACTER*16 IINDEX
      REAL XA(1), YA(1)
      INTEGER IROW, ICLM, IPXCOL, NLEFT
      DATA IINDEX/'0123456789ABCDEF'/
*
*---------------------------------------------------------------------


      XA(1) = X
      YA(1) = Y + 1.0

* Output each row of raster
      DO 200 IROW=1,NYPIX
* Move to beginning of this row and enable sequential dot plotting.
        YA(1) = YA(1) - 1.0
        CALL GK0SLN(1, XA, YA)
        CALL GKIOCO(KIOQS, ' ', NLEFT)
        IF (NLEFT .LT. 8) THEN
          CALL GKIOCO(KIOPB,'Z',NLEFT)
          CALL GKIOCO(KIOSN,' ',NLEFT)
        ENDIF
        CALL GKIOCO(KIOPB,'DJ',NLEFT)

* Output each pixel in the row
        DO 100 ICLM=1,NXPIX
          IF (NLEFT .LT. 4) THEN
            CALL GKIOCO(KIOPB,'Z',NLEFT)
            CALL GKIOCO(KIOSN,' ',NLEFT)
            CALL GKIOCO(KIOPB,'DJ',NLEFT)
          ENDIF
* Make sure colour index is inside the workstation colour table.
          IPXCOL = MOD(ICOLAR(ICLM,IROW), KPCI(KWKIX)) + 1
          CALL GKIOCO(KIOPB,IINDEX(IPXCOL:IPXCOL),NLEFT)
  100   CONTINUE

  200 CONTINUE

      END



