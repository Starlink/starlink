C-------------------------------------------------------------------------

      SUBROUTINE SPECX_GSD_LIST (IERR)

C   Routine to read GSD file containing a generalized observation
C   and summarize the contents on the terminal

      IMPLICIT  NONE

C   Formal parameters

      INTEGER*4 IERR

C   Include files

      INCLUDE  'FLAGCOMM'
      INCLUDE  'GSD_VAR.INC'

C   Functions

*     REAL*8    ABS

C   Local variables

      INTEGER*4 I                  ! Counter
      INTEGER*4 IRA(4)             ! R.A. of map centre
      INTEGER*4 IDEC(4)            ! Declination of map centre
      INTEGER*4 STATUS             ! ADAM-type status
      CHARACTER DATE*9             ! Date of scan
      CHARACTER TIME*9             ! Time scan started
      CHARACTER RASTRING*12        ! R.A.
      CHARACTER DECSTRING*12       ! Dec.
      CHARACTER XDIR*8, YDIR*8     ! Increment in X, Y

      INTEGER*4 ADAM__OK
      PARAMETER (ADAM__OK=0)

      IF (IERR.NE.0) RETURN
      STATUS = ADAM__OK

      WRITE (ILOUT,'('' Scan #'',I5/X,20(''-'')/)') SCAN_NO
      WRITE (ILOUT,'('' Source_name '',A32,''    Observer: '',A16)')
     &       SOURCE_NAME, OBSERVER

      CALL TRAN_DATE (DDATE, DATE)
      CALL TRAN_TIME (DUT, TIME)
      WRITE (ILOUT,'('' Recorded on '',A9,'' at '',A8,'' (UT)'')')
     &       DATE, TIME
      WRITE (ILOUT,*)

      CALL DEG_TO_DMS    (RADG/15.D0, IRA)
      CALL DMS_TO_STRING (IRA,        RASTRING)
      CALL DEG_TO_DMS    (DECDG,      IDEC)
      CALL DMS_TO_STRING (IDEC,       DECSTRING)
      WRITE (ILOUT,'('' Map centre is at R.A. '',A11,
     &               '' Dec. '',A12)') RASTRING, DECSTRING
      WRITE (ILOUT,*)
      WRITE (ILOUT,'('' Local coordinate system: '',A2,
     &               '';  Position angle of y-axis '', F5.1,
     &               '' (degrees E of N)'')') LOCAL_COSYS(:2), V2Y
      WRITE (ILOUT,'('' Local cell size '',F5.1,'' by '',F5.1,
     &               '' arcsec'')') DX,DY
      IF (ABS(X2Y).NE.90.D0) THEN
        WRITE (ILOUT,*)
        WRITE (ILOUT,'('' ## Grid not rectilinear - XY angle ='',
     &                  F6.1,'' degrees ##'')') X2Y
        WRITE (ILOUT,*)
      END IF

      WRITE (ILOUT,'('' Offset is ('',F7.2,'','',F7.2,
     &               '') raster units'')') XMAP_OFF,YMAP_OFF

C   Map information...

      IF (IXNP*IYNP.GT.1) THEN
        WRITE (ILOUT,'('' Scan direction '',A16,
     &                 '' Scan type is '',A16)') SCAN_DIR, SCAN_TYPE
        WRITE (ILOUT,'('' Map size '',I3,'' by '',I3)') IXNP,IYNP
        WRITE (ILOUT,'('' Map start at ('',F7.2,'','',F7.2,
     &                 '') raster units'')') XGC,YGC

*       XDIR = 'negative'
*       IF (XPOS) XDIR = 'positive'
*       YDIR = 'negative'
*       IF (YPOS) YDIR = 'positive'
*       WRITE (ILOUT,'('' Increment in x is '',A8,
*    &                 '', increment in y is '',A8)') XDIR,YDIR

        WRITE (ILOUT,*)
      END IF

      IF (PPC.LE.0) THEN
        WRITE (ILOUT,'('' Bad header -- phases/cycle = 0'')')
        NNSPEC = 0
      ELSE IF (NCI.LE.0) THEN
        WRITE (ILOUT,'('' Bad header -- cycles/integration = 0'')')
        NNSPEC = 0
      ELSE
        NNSPEC = (NP-1)/(PPC*NCI) + 1
      END IF

      IF (NP.EQ.0) THEN
        WRITE (ILOUT,'('' File does not contain any spectra'')')
      ELSE IF (NNSPEC.EQ.1) THEN
        WRITE (ILOUT,'('' File contains 1 spectrum'')')
      ELSE
        WRITE (ILOUT,'('' File contains'',I4,'' spectra'')') NNSPEC
      END IF
      WRITE (ILOUT,*)

C  Standard return

   99 IF (STATUS.NE.ADAM__OK) THEN
        IERR = 37
        type '('' Status'',2X,I10,4X,''($'',Z8.8,'')'')', status,status
      END IF

      RETURN

      END

C-----------------------------------------------------------------
