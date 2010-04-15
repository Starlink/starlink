	PROGRAM AMCAL

* Program to calculate azimuth and altidude from
* RA and Dec as a function of LST for the JCMT.

* Written by Matt. Mountain
* Alterations by Phil Puxley:  PGPLOT plotting
* Modified for UKIRT Feb 1989: JACH::[JOEL]
* created AMCAL for airmass calculation Mar 1991: JACH::CAA

        INTEGER J, L1, L2, L3, L4, L5
	INTEGER CSTART, CSTART2
	INTEGER MO, DA, YR, DAR, MOR, RAD, RAM, DED, DEM
	INTEGER MONTH( 12), LUNI, LUNO, HR, MI, SE

	REAL RA, DEC, LAT, LONG
	REAL AM, ALT, HST
      	REAL UT, LST, HA, RADI
	REAL JD0, JD, JDR

	CHARACTER*80 INPFILE, OUTFILE, PREFIX, SUFFIX, DLINE, FILTER
	CHARACTER*20 CDA, CMO, CYR, CNU, CHR, CMI, CSE, CAM

	LOGICAL MORE

	DATA LAT /19.826/
	DATA LONG /10.3648055/
	DATA RADI /.017453292/
	DATA MONTH /31,28,31,30,31,30,31,31,30,31,30,31/

  1	TYPE *, 'Right Ascension   (HH MM SS) ? '
	READ( 5, *, ERR=1) XRAD, XRAM, XRAS
  2	TYPE *, 'Declination       (DD MM SS) ? '
	READ( 5, *, ERR=2) XDED, XDEM, XDES
  3	TYPE *, 'Name of input file           ? '
	READ( 5, '(A)', ERR=3) INPFILE
  4	TYPE *, 'Prefix of input images       ? '
	READ( 5, '(A)', ERR=4) PREFIX
  6	TYPE *, 'Name of OUTPUT file          ? '
	READ( 5, '(A)', ERR=6) OUTFILE
  5	TYPE *, 'Suffix of input images       ? '
	READ( 5, '(A)', ERR=5) SUFFIX
!  7	TYPE *, 'Name of filter used          ? '
!	READ( 5, '(A)', ERR=7) FILTER
	filter = 'X'

	LUNI = 142
	OPEN( UNIT=LUNI, FILE=INPFILE, STATUS='OLD', ERR=999)
	LUNO = 143
	OPEN( UNIT=LUNO, FILE=OUTFILE, STATUS='NEW')

	MORE = .TRUE.
	DO WHILE ( MORE)
	  READ( LUNI, '(A)', END=100) DLINE
	  CALL L1 = CHR_LEN( DLINE)
	  CSTART2 = 0
	  J = 1
	  DO WHILE ( J .LE. L1 .AND. CSTART2 .EQ. 0)
	    IF( DLINE( J:J) .EQ. ',') THEN
	      CSTART2 = J
	    END IF
	    J = J + 1
	  END DO
	  CSTART = 0
	  J = 1
	  DO WHILE ( J .LE. L1 .AND. CSTART .EQ. 0)
	    IF( DLINE( J:J) .EQ. '=') THEN
	      CSTART = J
	    END IF
	    J = J + 1
	  END DO
	  IF( CSTART .LE. L1) THEN
	    CNU = DLINE( CSTART2-3:CSTART2-1)
	    IF( CNU( 1:1) .EQ. 'n' .OR. CNU( 1:1) .EQ. 'N') CNU( 1:1) = ' '
	    CDA = DLINE( CSTART+2:CSTART+3)
	    CMO = DLINE( CSTART+5:CSTART+7)
	    CYR = DLINE( CSTART+9:CSTART+12)
	    CHR = DLINE( CSTART+14:CSTART+15)
	    CMI = DLINE( CSTART+17:CSTART+18)
	    CSE = DLINE( CSTART+20:CSTART+21)
	  ELSE
	    GOTO 1
	  END IF
	  IF( INPFILE .EQ. ' ') GOTO 1

          CALL CHR_CTOI( CDA, DA)
	  IF( CMO .EQ. 'JAN') MO = 1
	  IF( CMO .EQ. 'FEB') MO = 2
	  IF( CMO .EQ. 'MAR') MO = 3
	  IF( CMO .EQ. 'APR') MO = 4
	  IF( CMO .EQ. 'MAY') MO = 5
	  IF( CMO .EQ. 'JUN') MO = 6
	  IF( CMO .EQ. 'JUL') MO = 7
	  IF( CMO .EQ. 'AUG') MO = 8
	  IF( CMO .EQ. 'SEP') MO = 9
	  IF( CMO .EQ. 'OCT') MO = 10
	  IF( CMO .EQ. 'NOV') MO = 11
	  IF( CMO .EQ. 'DEC') MO = 12
          CALL CHR_CTOI( CYR, YR)
          IF( DA .GT. 31 .OR. DA .LE. 0 .OR.
     :	      MO .GT. 12 .OR. MO .LE. 0) THEN
	    type *, 'da, mo = ', da, mo
	    TYPE *, 'Illegal date!!!  aborting'
	    CALL EXIT(0)
	  END IF

          CALL CHR_CTOI( CHR, HR)
          CALL CHR_CTOI( CMI, MI)
          CALL CHR_CTOI( CSE, SE)
	  HST = HR+( MI/60.0)+( SE/3600.0)

* Correct for ZULU day (assume observation begins evening of query date)
* 'DAR', 'MOR' are request day, month  (Hawaii date)

	  DA = DA + 1
	  IF ( YR/4 .EQ. INT( YR/4)) MONTH( 2) = 29
	  IF ( DA .GT. MONTH( MO)) THEN
	    MO = MO + 1
	    DA = 1
	  END IF
	  DAR = DA
	  MOR = MO
	  CALL JULIAN DAY( MO, DA, YR, JD)
	  JDR = JD

* 'JDR' is Julian day of request
* now find jul day for January 0.0

	  MO = 1
	  DA = 0
	  CALL JULIAN DAY( MO, DA, YR, JD)

* 'JD0' is JD on Jan 0.0

	  JD0 = JD
	  DOY = JDR - JD0

* get LST at requested HST

	  UT = HST + 10.0
	  CALL UT_LST( YR, JD0, DOY, UT, LONG, LST)

          RAD = XRAD
          RAM = XRAM
          RAS = XRAS
	  RA = RAD + RAM/60. + RAS/3600.

	  DED = XDED
          DEM = XDEM
          DES = XDES
	  IF( DED .LT. 0) DEM = -DEM
	  IF( DED .LT. 0) DES = -DES
          DEC = DED + DEM/60. + DES/3600.

* now calculate ALT

	  HA = LST-RA
	  IF( HA .LT. 0.0) HA = HA+24.0

* convert all angles from degrees i.e. * 0.0174533

	  HA = HA*15
	  ALT = ASIN( SIN( DEC*RADI)*SIN( LAT*RADI)+
     :                COS( DEC*RADI)*COS( LAT*RADI)*COS( HA*RADI))

* convert back to degrees

	  ALT = ALT/RADI

* convert to AIRMASS

	  AM = 1./COSD( 90.-ALT)

	  WRITE( CAM, '(F5.3)') AM

	  L1 = CHR_LEN( PREFIX)
	  L2 = CHR_LEN( SUFFIX)
	  L3 = CHR_LEN( FILTER)
	  L4 = CHR_LEN( CAM)
          CALL CHR_CTOI( CNU, NU)
	  L5 = CHR_LEN( CNU)
	  IF( NU .GE. 1 .AND. NU .LE. 9) THEN
	    DLINE = 'amcorr '
     :	            //PREFIX( 1:L1)//CNU( L5:L5)
     :	            //SUFFIX( 1:L2)//' '//PREFIX( 1:L1)
     :	            //CNU( L5:L5)//SUFFIX( 1:L2)//'A '
     :	            //FILTER( 1:L3)//' '//CAM( 1:L4)
	  ELSE IF( NU .GE. 10 .AND. NU .LE. 99) THEN
	    DLINE = 'amcorr '
     :	             //PREFIX( 1:L1)//CNU( 2:L5)
     :	            //SUFFIX( 1:L2)//' '//PREFIX( 1:L1)
     :	            //CNU( 2:L5)//SUFFIX( 1:L2)//'A '
     :	            //FILTER( 1:L3)//' '//CAM( 1:L4)
	  ELSE
	    DLINE = 'amcorr '
     :	            //PREFIX( 1:L1)//CNU( 1:L5)
     :	            //SUFFIX( 1:L2)//' '//PREFIX( 1:L1)
     :	            //CNU( 1:L5)//SUFFIX( 1:L2)//'A '
     :	            //FILTER( 1:L3)//' '//CAM( 1:L4)
	  END IF

	  L1 = CHR_LEN( DLINE)
	  WRITE( LUNO, '(A)') DLINE( 1:L1)

	END DO
  100	CONTINUE

	CLOSE( LUNI)
	CLOSE( LUNO)

	type *, ' '
	type *, 'Make sure you EDIT the X in the output file to specify real FILTER used'
	type *, ' '

	CALL EXIT(0)
  999	TYPE *, 'Error opening input file ', INPFILE
	CALL EXIT(0)
  998	TYPE *, 'Error opening output file ', OUTFILE
	CALL EXIT(0)

	END


	SUBROUTINE JULIAN DAY( MO, DA, YR, JD)

* Calculate 'JD' = JULIAN DAY GIVEN MO,DA,YR

	INTEGER MO, DA, YR, YRP, AJ, BJ, CJ, DJ

	REAL JD

	IF( YR .LT. 100) YR = YR+1900
	YRP = YR
	MOP = MO

	IF( MO .EQ. 1 .OR. MO .EQ. 2) THEN
	  YRP = YR-1
	  MOP = MOP+12
	END IF

	AJ = INT( YRP/100.)
	BJ = 2-AJ+INT( AJ/4)
	IF( YR .LT. 1583) BJ = 0 ! NOT GOOD FOR 10/15 TO 12/30 1582
	CJ = INT( 365.25*YRP)
	DJ = INT( 30.6001*( MOP+1))
	JD = BJ+CJ+DJ+DA+1720994.5

	END


	SUBROUTINE UT_LST( YR, JD0, DOY, UT, LONG, LST)

* Convert UT Date, Time to LST at UKIRT

	INTEGER	YR

	REAL JD0, LONG, LST

* Find constant 'B' for Siderial calc's

	S = JD0 - 2415020.
	T = S / 36525.
	R = 6.6460656 + ( 2400.051 * T ) + ( 0.00002581 * T * T )
	U = R - ( 24. * ( YR - 1900 ))
	B = 24. - U

* Now get GMT

	A = 0.0657098
	C = 1.002738
	D = 0.99727
	T0 = DOY * A - B
	IF (T0 .LT. 0) T0 = T0 + 24.

	GST = UT/D
	GST = GST + T0
	IF (GST .GT. 24.) GST = GST - 24.

	LST = GST - LONG
	IF (LST .LT. 0.) LST = LST + 24.

	END
