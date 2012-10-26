*  History:
*     18 Nov 1993 (hme):
*        Disuse OUTERR_HANDLER error handler.
*     17 Dec 1993 (hme):
*        In order to adapt to new STACKCOMM/PROTOTYPE, no longer use
*        TSYS as start of scan header, since it isn't any more.
*     01 Jan 1994 (rp):
*        Replace original code with new version from VAX 6.3
*        Make changes as above
*        Put IOSTAT=IERR in all WRITE statements to allow for overflows
*     31 Jan 1994 (hme):
*        Disuse <> in formats.
*     20 July 2000 (ajc):
*        Missing commas in FORMAT
*        Unused GEN_ILEN, I, STATUS
C-----------------------------------------------------------------------

      SUBROUTINE LISTMP (IFAIL)

C    Routine to list spectra in a .MAP file, with their positions on
C    the sky and in the file.

      IMPLICIT  NONE

*  Skip asking for seq and offset limits if number of scans less than:
      INTEGER    MXSCAN
      PARAMETER  (MXSCAN=25)

*     Formal parameter:

      INTEGER  IFAIL

*     Include files:

      INCLUDE 'CUBE'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

*     Local variables:

      LOGICAL   DETAILS
      INTEGER   J
      INTEGER   IERR
      INTEGER   IIN, JIN
      INTEGER   IPOS
      INTEGER   NSP
      INTEGER   MAPSIZ
      INTEGER   JDEF               ! Returned by GEN_*
      INTEGER   RSCAN(2), IDUM     ! Scan sequence boundaries
      REAL      RPOS(4), RDUM      ! Offset boundaries
      REAL      R2,  D2
      CHARACTER RASTRING*12, DECSTRING*12

*  Ok, go...

      IFAIL = 0

C     Ascertain if long or short listing needed

      CALL GEN_YESNO ('Give details of individual spectra?',
     &                .FALSE., DETAILS, IFAIL)
      IF (IFAIL.LT.0) THEN
        IFAIL = 18
        RETURN
      ELSE
        IFAIL = 0
      END IF

      WRITE (ILOUT,1010,IOSTAT=IERR) NAMEMP

C   Write out details of map header

      WRITE (ILOUT,1012,IOSTAT=IERR) MAP_ID, MAP_OWNER_NAME

      IF (RAM.ne.0.D0 .and. DECM.ne.0.D0) THEN
        CALL DEG_TO_STRING (RAM/15.D0,  RASTRING)
        CALL DEG_TO_STRING (DECM,       DECSTRING)
        WRITE (ILOUT, *) 'Map centre - from map header:'
        WRITE (ILOUT,'(''    R.A. ''A12)', IOSTAT=IERR) RASTRING
        WRITE (ILOUT,'(''    Dec. ''A11)', IOSTAT=IERR) DECSTRING
      ELSE
        WRITE (ILOUT, *) 'Map centre not set:'
        WRITE (ILOUT, *) ' -- RA and Dec of prototype header used.'
      END IF

      IF (POS_ANGLE.EQ.0.0) THEN
        WRITE (ILOUT,1020,IOSTAT=IERR) MSTEP, CELL_XSIZE,
     &                                 NSTEP, CELL_YSIZE
      ELSE
        WRITE (ILOUT,1021,IOSTAT=IERR) MSTEP, CELL_XSIZE,
     &                                 NSTEP, CELL_YSIZE
        WRITE (ILOUT,1025,IOSTAT=IERR) POS_ANGLE
      END IF

C   Output map and spectrum header

      IF (IHEAD.NE.1) THEN
        WRITE (ILOUT,1030,IOSTAT=IERR) NREDT
        WRITE (ILOUT,1032)
        RETURN
      ENDIF

C   Number of entries in map? (includes deleted ones unfortunately)

      WRITE (ILOUT,1033,IOSTAT=IERR) NSPEC
      WRITE (ILOUT,1030,IOSTAT=IERR) NREDT

      CALL PUSH
      CALL EXTRACT_HEADER (SCAN_HEADER)

      WRITE (ILOUT,1035,IOSTAT=IERR) IST
      WRITE (ILOUT,1040)
      CALL PRSCAN (ILOUT,1)

C   Search INDEX area and output relevant data if scan present

      IF (DETAILS) THEN
        NSP    = 0
        MAPSIZ = MSTEP*NSTEP

*  Get scan and offset (xlo, xhi, ylo, yhi) boundaries.
        RSCAN(1) =  1
        RSCAN(2) =  MAPSIZ
        RPOS(1)  = -99999.0
        RPOS(2)  =  99999.0
        RPOS(3)  = -99999.0
        RPOS(4)  =  99999.0

        IF ( MAPSIZ .GT. MXSCAN ) THEN
           CALL GEN_GETI4A( 'Scan range (low,high)? (All)',
     :                      RSCAN, 2, ' ', RSCAN, JDEF)
           IF ( RSCAN(1) .GT. RSCAN(2) ) THEN
              IDUM = RSCAN(1)
              RSCAN(1) = RSCAN(2)
              RSCAN(2) = IDUM
           ENDIF

           CALL GEN_GETR4A( 'Offset limits (xlo xhi ylo yhi)? (None)',
     :                      RPOS, 4, ' ', RPOS, JDEF)
           IF ( RPOS(1) .GT. RPOS(2) ) THEN
              RDUM = RPOS(1)
              RPOS(1) = RPOS(2)
              RPOS(2) = RDUM
           ENDIF
           IF ( RPOS(3) .GT. RPOS(4) ) THEN
              RDUM = RPOS(3)
              RPOS(3) = RPOS(4)
              RPOS(4) = RDUM
           ENDIF
        ENDIF

        DO J = 1, MAPSIZ

          CALL XCOPY (4,
     :         %VAL(CNF_PVAL(INDEX_ADDRESS)+4*(J-1)),IPOS)

          IF (IPOS.GT.0)   THEN
            NSP = NSP+1
            JIN = (J-1)/MSTEP+1
            IIN = J-(JIN-1)*MSTEP
            R2  = FLOAT ((MSTEP+1-IIN)*2-MSTEP-1)*0.5*CELL_XSIZE
            D2  = FLOAT ((NSTEP+1-JIN)*2-NSTEP-1)*0.5*CELL_YSIZE
            IF (IPOS .GE. RSCAN(1) .AND. IPOS .LE. RSCAN(2) .AND.
     :          R2 .GE. RPOS(1)-0.01 .AND. R2 .LE. RPOS(2)+0.01 .AND.
     :          D2 .GE. RPOS(3)-0.01 .AND. D2 .LE. RPOS(4)+0.01 ) THEN
               WRITE (ILOUT,1000,IOSTAT=IERR) NINT(R2),NINT(D2),IPOS
            ENDIF
            IF (NSP.EQ.NSPEC) GO TO 99
          END IF
        END DO
      END IF

   99 CONTINUE
      CALL POP
      RETURN

 1000 FORMAT(1X,'( ',I5,',',I5,' ) ',6X,'Spectrum # ',I5 )
 1010 FORMAT(1X,'Contents of map file ',A)
 1012 FORMAT(1X,'File name: ',A,/
     &       1X,'Owner:     ',A,/)
 1020 FORMAT(/1X,'R.A. ',I3,' cells @ ',F5.1,' arcsec'
     &       /1X,'Dec. ',I3,' cells @ ',F5.1,' arcsec')
 1021 FORMAT(/1X,'X-axis ',I3,' cells @ ',F5.1,' arsec'
     &       /1X,'Y-axis ',I3,' cells @ ',F5.1,' arsec')
 1025 FORMAT(/1X,'Position angle of map y-axis is ',F6.1,' deg. E of N')
 1030 FORMAT( 1X,I4,' blocks reserved for INDEX area')
 1032 FORMAT(/1X,'No entries in map')
 1033 FORMAT(/1X,I4,' spectrum positions used in map file')
 1035 FORMAT( 1X,I4,' blocks per entry in map'//)
 1040 FORMAT(1X,'Prototype spectrum header')

      END

C-----------------------------------------------------------------------
