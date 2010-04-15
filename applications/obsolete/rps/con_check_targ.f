*+CON_CHECK_TARG   Consistency check for Rosat Proposal Target form.
      SUBROUTINE CON_CHECK_TARG(REF_NO,MESID,SCREEN,XTYPE,FIELD_NO)

*  History
*     1988 Sep	M.Bush		1st version
*     1989 Feb	M Ricketts	Various mode
*     1991 Jan			Check min. time against total, allow specific S1, S2 filters
*     1992 May  M. Duesterhaus  Port to UNIX
*     1994 Jan  M Ricketts	Only allow epoch = 2000

      IMPLICIT NONE

*   Input :
      INTEGER REF_NO						!Database reference number.
      INTEGER MESID						!Screen number.
      LOGICAL SCREEN						!Set to TRUE if in screen mode.

*   Output :
      CHARACTER*1 XTYPE						! R(eturn), E(xit) or Q(uit)
      INTEGER FIELD_NO						!Field number being referenced.

*  Global Variables
      INCLUDE 'com_form_qual.inc'
      INCLUDE 'com_form_points.inc'		! Gives Constraints field

*   Functions :
      INTEGER DBS_FIELDNO					!Gets field number from the database.
      INTEGER DBS_GETI						!Gets integer value from the database.
      CHARACTER*60 DBS_GETC					!Gets character value from the database.
      REAL DBS_GETR						!Gets real value from the database.
      INTEGER MDH_ENDWORD					!Used to check for blank character variables.
      CHARACTER*1 CON_CHECK_ERR
      LOGICAL DBS_GETL
      CHARACTER*11 MDH_DTOC
*-
*   Local :
      DOUBLE PRECISION DECRADS					!DEC in radians.
      DOUBLE PRECISION RARADS					!RA in radians.
      INTEGER IVAL						!Integer value
      CHARACTER*60 CVAL						!Character value.
      CHARACTER*11 CVALRA, CVALRAL				!RA in characters, old value
      CHARACTER*11 CVALDEC, CVALDECL				!DEC in characters, old value
      CHARACTER*20 FLD						!Field
      CHARACTER*3 WFC_FILTER(8)					!Filter use order.
      CHARACTER*5 PSPC_FILTER(2)				!Filter use order.
      INTEGER WFC_PERCENT(8)					!Filter use percent of time.
      INTEGER PSPC_PERCENT(2)					!Filter use percent of time.
      INTEGER TOTAL                                             !Total of percentage filter tmes.
      INTEGER I							!Array counter.
      INTEGER IERR						!Error flag.
      CHARACTER MESSAGE*35					!Error message output to the screen.
      LOGICAL LSTATUS						! Status flag
      REAL MINT, TOTT						! requested times

      INTEGER HRI,PSPC,WFC					!Instrument codes.

      INTEGER EPCH
      DOUBLE PRECISION DPEPCH
      DOUBLE PRECISION RADEG
      DOUBLE PRECISION DECDEG
      DOUBLE PRECISION PI
      DOUBLE PRECISION PRECRA
      DOUBLE PRECISION PRECDEC
      DOUBLE PRECISION PRADEG
      DOUBLE PRECISION PDECDEG
      DOUBLE PRECISION JD
      Parameter( pi = 3.1415926536D+00 )


      LOGICAL OK(0:2,0:2,0:2)
     & / .FALSE. , .TRUE. , .FALSE. , .TRUE. , 15 * .FALSE.
     & , .TRUE. , .FALSE. , .TRUE. , 5 * .FALSE. /
      CHARACTER*42 MESE2/'R(eturn) to form, E(xit) or Q(uit)         '/

*  __________________________ Executable Code __________________________________

      FIELD_NO=DBS_FIELDNO(REF_NO,'TARGET.NAME')
      CVAL=DBS_GETC(REF_NO,FIELD_NO)
      IF(MDH_ENDWORD(CVAL).EQ.0)THEN
         MESSAGE='Source name'
         GOTO 10
      END IF

      FIELD_NO=DBS_FIELDNO(REF_NO,'TARGET.NUMBER')
      IVAL=DBS_GETI(REF_NO,FIELD_NO)
      IF (IVAL.LE.0 ) THEN
         MESSAGE = 'Target number'
         GOTO 10
      END IF

      FIELD_NO=DBS_FIELDNO(REF_NO,'TARGET.RA')
      CVALRA=DBS_GETC(REF_NO,FIELD_NO)
      CVALRAL = CVALRA

      CALL RA_CONVERT(CVALRA, RARADS, LSTATUS)
      IF (CVALRAL .NE. CVALRA) THEN
         CALL DBS_PUTC(REF_NO,FIELD_NO, CVALRA,IERR)
      END IF

      IF( .NOT. LSTATUS ) THEN
         MESSAGE='Right Ascension'
         GO TO 10
      END IF

      CALL DBS_INSERTD(REF_NO,RARADS,'TARGET.RA')				!Inset value of RA in radians into the record.

      FIELD_NO=DBS_FIELDNO(REF_NO,'TARGET.DEC')
      CVALDEC=DBS_GETC(REF_NO,FIELD_NO)

      CVALDECL = CVALDEC
      CALL DEC_CONVERT(CVALDEC, DECRADS, LSTATUS)
      IF (CVALDECL .NE. CVALDEC) THEN
         CALL DBS_PUTC(REF_NO,FIELD_NO, CVALDEC, IERR)
      END IF

      IF ( .NOT. LSTATUS ) THEN
         MESSAGE='Declination'
         GO TO 10
      END IF
      CALL DBS_INSERTD(REF_NO,DECRADS,'TARGET.DEC')				!Inset value of DEC in radians into the record.

      FIELD_NO=DBS_FIELDNO(REF_NO,'TOTAL.OBS.TIME')
      TOTT = DBS_GETR(REF_NO,FIELD_NO)
      IF(TOTT .LT. 1.5 .OR. TOTT .GT. 1000.0)THEN
         MESSAGE='Total Observation Time'
         GO TO 10
      END IF

      FIELD_NO = DBS_FIELDNO(REF_NO,'NUMBER.OBS')
      IVAL = DBS_GETI(REF_NO, FIELD_NO )
      IF (IVAL .LE. 0 ) THEN
         MESSAGE = 'Number of observations'
         GOTO 10
      END IF
      IF ((IVAL .GT. 1).AND.(.NOT.DBS_GETL(REF_NO,FLD_CONSTRAINTS))) THEN
         MESSAGE = 'time critical flag - not set'
         GOTO 10
      END IF


      TOTAL=0
      DO I=1,8
         WRITE(FLD,5)'WFC.FILT.CODE(',I,')'
5        FORMAT(A,I1,A)
         FIELD_NO=DBS_FIELDNO(REF_NO,FLD)
	 WFC_FILTER(I) = DBS_GETC(REF_NO,FIELD_NO)
	 CALL UPC(WFC_FILTER(I))
         IF (WFC_FILTER(I).EQ. '   ') THEN
            IF (I.EQ.1) THEN
               MESSAGE = 'WFC needs filter 1 setting'
               goto 10
            END IF
         END IF
         IF (WFC_FILTER(I).NE.'S1 ' .AND. WFC_FILTER(I) .NE. 'S2 ' .AND.
     &            WFC_FILTER(I).NE.'S1A' .AND. WFC_FILTER(I) .NE. 'S1B' .AND.
     &            WFC_FILTER(I).NE.'S2A' .AND. WFC_FILTER(I) .NE. 'S2B' .AND.
     &            WFC_FILTER(I).NE.'P1 ' .AND. WFC_FILTER(I) .NE. 'P2 ' .AND.
     &            WFC_FILTER(I) .NE.'OPQ' .AND. WFC_FILTER(I) .NE. '   ') THEN
            MESSAGE = 'Invalid WFC filter code'
            GOTO 10
         END IF
         WRITE(FLD,5)'WFC.FILT.PCNT(',I,')'
         FIELD_NO=DBS_FIELDNO(REF_NO,FLD)
         WFC_PERCENT(I)=DBS_GETI(REF_NO,FIELD_NO)
         IF ( (WFC_FILTER(I).NE.'   ' .AND. WFC_PERCENT(I).EQ.0) .OR.
     &        (WFC_FILTER(I).EQ.'   ' .AND. WFC_PERCENT(I).NE.0) ) THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'WFC.FILT.CODE(1)')
            MESSAGE = 'WFC Filter sequence or percentages'
            GO TO 10
         END IF
         TOTAL=TOTAL+WFC_PERCENT(I)

         WRITE(FLD,5) 'WFC.FILT.MINT(', I, ')'
         FIELD_NO=DBS_FIELDNO(REF_NO, FLD)
         MINT = DBS_GETR(REF_NO,FIELD_NO)
         IF(MINT .GT. TOTT) THEN
            MESSAGE='Min. time too large'
            GO TO 10
         END IF

      END DO
      IF((TOTAL.NE.100).AND.(TOTAL.NE.0)) THEN
         FIELD_NO=DBS_FIELDNO(REF_NO,'WFC.FILT.PCNT(1)')
         MESSAGE=
     &   'WFC Filter percentages'
         GO TO 10
      END IF

*  No errors found
      FIELD_NO = 1
      IF (.NOT.DBS_GETL(REF_NO, FLD_CONSTRAINTS) ) THEN
         QUAL_TARGET(QTARGET) = .TRUE.
      ELSE
         QUAL_TARGET(QTARGET) = .FALSE.
      END IF
      GOTO 20

10    CONTINUE
      XTYPE = CON_CHECK_ERR(MESID,MESSAGE)
      QUAL_TARGET(QTARGET) = .FALSE.
20    CONTINUE

      END
