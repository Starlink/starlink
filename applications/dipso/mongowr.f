       SUBROUTINE MONGOWR(WAVE,FLUX,ASIZE1,NPNTS,BREAKS,MAXBRK,NBREAK,
     :                    PARAMS,SUBCHK,STATUS)
*
*   Writes MONGO readable files, including flags for
*   'bad' data.
*   PARAMS contains output file name and (optional)
*   'y' value for 'bad' data (marking breaks in
*   dataset).
*

       IMPLICIT NONE
       INCLUDE 'SAE_PAR'

       INTEGER ASIZE1, NPNTS, MAXBRK, NBREAK, STATUS

       REAL WAVE(ASIZE1), FLUX(ASIZE1)
       INTEGER BREAKS(MAXBRK)

       INTEGER PRMLEN
       CHARACTER*(*) PARAMS

       LOGICAL SUBCHK

*

       INTEGER I, J, K
       INTEGER ILNGTH
       REAL NULL(2)

*  Check the inherited status.
       IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned DIPSO status flag.
       SUBCHK = .TRUE.

*  Check that there's something to output
       IF (NPNTS.LE.0) THEN
          WRITE (*,'(''   MONGOWR:  no data in current arrays'')')
          SUBCHK = .FALSE.
          GOTO 600
       ENDIF

*  Get output file name, open file
       CALL RDSTR( 'MONGOWR', 'File name', ' ', PARAMS, STATUS )
       IF( STATUS .NE. SAI__OK ) THEN
          SUBCHK = .FALSE.
          GO TO 600
       END IF

*  Check that a value of 'null' hasn't been provided BEFORE filename
       CALL SSTRIP(PARAMS)
       ILNGTH = INDEX(PARAMS,' ') - 1

       DO 200 I = 1, 200
          IF (CHAR(I).EQ.'0') GOTO 300
  200  CONTINUE

  300  CONTINUE
       SUBCHK = .FALSE.
       DO 400 J = 1, ILNGTH
          DO 350 K = I, I + 9, 1
             IF (PARAMS(J:J).NE.CHAR(K) .AND. PARAMS(J:J).NE.'.')
     :       SUBCHK = .TRUE.
  350     CONTINUE
  400  CONTINUE
       IF (.NOT.SUBCHK) THEN
          WRITE (*,'(''   MONGOWR:  invalid filename '',A)')
     :    PARAMS(1:ILNGTH)
          SUBCHK = .FALSE.
          GOTO 600
       ENDIF

*

       OPEN (UNIT=61,IOSTAT=I,STATUS='NEW',FILE=PARAMS(1:ILNGTH))
       IF (I.NE.0) THEN
          WRITE (*,'(''   MONGOWR:  unable to open '',A)')
     :            PARAMS(1:ILNGTH)
          CLOSE (61)
          SUBCHK = .FALSE.
          GOTO 600
       ENDIF

*
*   Find 'NULL' value
*

       PARAMS(1:) = PARAMS((ILNGTH+1):)
       CALL SSTRIP(PARAMS)
       NULL(2) = 0.0
       PRMLEN = LEN( PARAMS )
       PARAMS(3:PRMLEN) = PARAMS(1:PRMLEN-2)
       PARAMS(1:2) = '0 '
       CALL DECODE('MONGOWR',PARAMS,1,2,NULL,'Null_value ',SUBCHK)
       IF (.NOT.SUBCHK) THEN

*
*   Error escape
*

          SUBCHK = .FALSE.
       ELSE

*
*   Output data
*

          J = 1
          DO 450 WHILE (BREAKS(J).LT.1)
             J = J + 1
  450     CONTINUE

          DO 500 I = 1, NPNTS
             WRITE (61,'(1P2E15.6)') WAVE(I), FLUX(I)
             IF (I.GE.BREAKS(J)) THEN
                IF (I.NE.NPNTS) THEN
                   WRITE (61,'(1P2E15.6)')
     :             (0.5*(WAVE(I)+WAVE(I+1))),  NULL(2)
                ENDIF
                DO 460 WHILE (BREAKS(J).LE.I)
                   J = J + 1
  460           CONTINUE
             ENDIF
  500     CONTINUE

          CLOSE (61)

       ENDIF

  600  CONTINUE

       END
