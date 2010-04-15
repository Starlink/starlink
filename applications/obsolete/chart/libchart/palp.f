      SUBROUTINE PALP(AP,DP, STATUS )
*+
*   This Routine Finds which Sky Survey
*   Plate a Given Field Centre
*   is on & its' approx. Co-ordinates Relative
*   to the Bottom Left-hand Corner.
*   Centres South of -20 Deg. use
*   ESO/SRC Survey otherwise Palomar.
*
*   NOTE : Palomar/SRC Survey Schmidt Plates are 14 ins.
*            Square with Scale 67.14 Arc. sec./mm.
*          ESO Survey Plates are 30 cms. Square
*            with Scale 67.6 Arc. secs. per mm.
*
*   Gets
*   ----
*      AP,DP      - Given Field Centre.
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONST call
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*-
*
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      DOUBLE PRECISION AP,DP,AL,DL,PCA,PCR
      DOUBLE PRECISION TWOPI,HALFPI,RDSA,RDST,RDDG
      LOGICAL SURVEY(3)
      CHARACTER*10 NAMES(3)
      INTEGER NPW,SPW,FNUM,FNST
      DIMENSION NPW(23),SPW(22),FNST(22)
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG
      DATA NPW/2880,150,92,68,52,44,38,34,30,28,
     : 26,26,24,24,24,24,24,24,24,26,26,28,30/
      DATA SPW/20,20,20,20,20,20,20,21,22,23,24,
     : 26,28,30,33,38,44,52,66,90,144,2880/
      DATA FNST/1039,967,895,823,751,679,607,538,472,
     : 409,349,293,241,193,149,111,78,50,28,12,2,1/
      DATA NAMES/'Palomar','UK Schmidt','ESO'/
*
*   PALF,ESOF = PLATE SIZES IN MM.
*
      DATA PALF,ESOF/355.6,300.0/
      DATA PALSC,ESOSC/67.14,67.6/

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      WRITE (7,995)
995   FORMAT(/,35X,'***************************************',
     : '***************',
     :       /,35X,'* Approximate Field Centre Location on Sky Survey'
     : ,'(s) *',
     :       /,35X,'***************************************',
     : '***************')
      WRITE (7,997)
997   FORMAT(22X,'Survey',5X,' Field Centre ',9X,'X',8X,'Y'/)
*
*   Precess Field Centre to 1855.0 (Equinox of Poss Field Centres)
*
      CALL PRECES(AP,DP,AL,DL,1950.00,1855.00, STATUS )
*
*   Find which Surveys Field Centre is on
*      1 = POSS ; 2=UKST ; 3 = ESO
*
      SURVEY(1) = DL.GT.(-45.0)*RDDG
      SURVEY(2) = DP.LT.(18.0*RDDG)
      SURVEY(3) = DP.LT.(-17.5*RDDG)

      DO 100 K=1,3
         IF (SURVEY(K)) THEN
*
*   Set Plate Spacing (6 Degs. for Palomar, 5 for ESO,SRC)
*
            IF (K.GT.1) NPS = 5
            IF (K.EQ.1) NPS = 6
*
*   Precess Centres to Survey Equinox
*
            IF (K.EQ.1.AND.DL/RDDG.LT.-21.0) THEN
               CALL PRECES(AP,DP,AL,DL,1950.00,1875.00, STATUS )
            ENDIF
            IF (K.GT.1) THEN
               AL = AP
               DL = DP
            ENDIF
            DEC = REAL( DL/RDDG )
            RAMINS = REAL( AL * 4.0/RDDG )
            MRA = NINT(RAMINS)
*
*   Now find which Band the Centre is in
*   +90 =1,.....,-42=23 at 6 Deg. Spacing ( Palomar)
*   +15 = 1......,-90 = 22 at 5 Deg. Spacing (SRC/ESO Southern)
*
            IF (K.EQ.1) THEN
               NBAND = 16 - (IFIX(DEC+SIGN(FLOAT(NPS)/2.0,DEC))/NPS)
            ELSE
               NBAND = 4 - (IFIX(DEC+SIGN(FLOAT(NPS)/2.0,DEC))/NPS)
*
*   Deal with Case at Northern Limit of UKST Survey
*
               IF (K.EQ.2.AND.NBAND.EQ.0) NBAND = 1
            ENDIF
*
*   and how Wide the Plates are for the Band in mins. of RA
*
            IF (K.EQ.1) THEN
               NPWIDE = NPW(NBAND)
            ELSE
               NPWIDE = SPW(NBAND)
            ENDIF
*
*   NPL is Plate no. within Band (RA 0H. = 0)
*   NRAC is Survey Plate Centre in mins. of time
*
            NPL = (MRA+NPWIDE/2)/NPWIDE
*
*   For UK Schmidt can Compute Field Number
*
            IF (K.EQ.2) THEN
               NTOT = FNST(NBAND)
               FNUM = NTOT + NPL
C
C            When the RA is close to 24 hrs it is possible that
C            the field is in the first plate, rather than the last
C            plate in the band. This is detected by checking to see
C            if the plate number found would place it in the next
C            declination band.
C
C            Modification made by K.F.Hartley at RGO on 4-11-83
C            Corrected to make it work by J.V.Carey on 9-2-84
C
               IF (FNUM.GE.FNST(NBAND-1)) THEN
                  FNUM = FNST(NBAND)
               END IF
            ENDIF
            NRAC = NPL * NPWIDE
*
*   Inserted by KFH on 17/12/82 to get round irregular
*   interval between last plate in band and first in band
*
            IF (NRAC.GE.24*60) NRAC=0
*
*   NCDEC - Plate Centre Dec in degs.
*   NCRAH,NCRAM - P.C. RA hrs. and mins.
*   CDEC = Corner Dec. in degs.
*   CRA = Corner RA in mins.
*
            IF (K.EQ.1) THEN
               NCDEC = 90 -((NBAND - 1) * 6)
            ELSE
               NCDEC = 15 -((NBAND-1)*5)
            ENDIF
            PCR = NCDEC * RDDG
            NCRAH = NRAC/60
            NCRAM = NRAC - (NCRAH * 60)
*
*   Correct for fact that last plate in band overlaps
*   first in band (hopefully)!
*   Inserted by K F Hartley 1982 December 17
*
   	    IF (NCRAH.GE.24) THEN
	       NCRAH=0
	       NCRAM=0
	    END IF
            PCA = NRAC * 60.0 * RDST

            CALL CONST(PCA,PCR, STATUS )
            CALL PROJ(1,AL,DL,X,Y, STATUS )
*
*   Scale is 67.14 for Poss & UKST ; 67.6 for ESO
*   Field Widths are PALF,ESOF in mm.
*
            IF (K.LE.2) THEN
               SCALE = PALSC
               FIELD = PALF
            ELSE
               SCALE = ESOSC
               FIELD = ESOF
            ENDIF
            CORNER = REAL( FIELD/2.0 * SCALE * RDSA )
*
*     XC,YC are Co-ords of Centre in mm.
*
            NXC = NINT((CORNER - X)/(RDSA * SCALE))
            NYC = NINT((CORNER + Y)/(RDSA * SCALE))
            IF (K.EQ.2) THEN
               WRITE (7,900) NAMES(K),NCDEC,NCRAH,NCRAM,NXC,NYC,FNUM
            ELSE
               WRITE (7,900) NAMES(K),NCDEC,NCRAH,NCRAM,NXC,NYC
            ENDIF
900         FORMAT(21X,A10,3X,SP,I3,S,2X,I2,'H',I3,'M',6X,I6,3X,I6,
     :             :3X,'(Field Number: ',I4,')')
         ENDIF
100   CONTINUE
      WRITE (7,902)
902   FORMAT(/,' ',10X,'(Co-ordinates are in mm., Relative ',
     : 'to South Eastern Corner of actual plate (not emulsion))')
      END

