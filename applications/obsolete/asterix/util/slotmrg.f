*+  SLOTMRG - Merge two lists of MJD slots into one
	SUBROUTINE SLOTMRG(STATUS)
*
*    Description :
*
*     Merges two lists of ON-OFF times stored as MJDs.
*
*    Environment parameters :
*
*     SFILE1            CHARACTER          Name of 1st input file
*     SFILE2            CHARACTER          Name of 2nd input file
*     SOFILE            CHARACTER          Name of output file
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Mike Denby   (LTVAD::MD)
*     Richard Saxton   (LTVAD::RDS)
*
*    History :
*
*      7 Jan 92 : Original
*      3 Mar 92 : V1.6-1 Brought into Asterix
*      4 May 93 : V1.7-0 Use FIO for I/O (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER CHR_LEN
*
*    Local constants :
*
      INTEGER           MAXSL                 ! Max number of slots
        PARAMETER       ( MAXSL = 1000 )
*
*    Local variables :
*
      CHARACTER*80	STRING*132
      LOGICAL		MFLAG
      DOUBLE PRECISION	TLO1(MAXSL),THI1(MAXSL),TLO2(MAXSL),THI2(MAXSL)
      DOUBLE PRECISION  TLOM(MAXSL),THIM(MAXSL)
      INTEGER		NP1, NP2, NPM, N
      INTEGER		LS1, LS2, LS3
      INTEGER           IFIL1,IFIL2,OFIL      ! FIO file descriptors
      REAL		TOT
*
*    Version :
*
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'SLOTMRG - version 1.8-0')
*-
      MFLAG = .FALSE.
*

      CALL AST_INIT()

* Open the i/p files to be merged
      CALL FIO_ASSOC( 'SFILE1', 'READ', 'LIST', 0, IFIL1, STATUS )
      CALL FIO_ASSOC( 'SFILE2', 'READ', 'LIST', 0, IFIL2, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 999

* Open the o/p file of marged times
      CALL FIO_ASSOC( 'SOFILE', 'WRITE', 'LIST', 0, OFIL, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 999

*    Read first input list
      NP1 = 1
      DO WHILE ( STATUS .EQ. SAI__OK )
*
        CALL FIO_READF( IFIL1, STRING, STATUS )

        IF ( STATUS .EQ. SAI__OK ) THEN
          LS3 = CHR_LEN(STRING)
          LS1 = INDEX(STRING,'M')
*
          IF (LS1.NE.0) THEN
*
	    MFLAG = .TRUE.
	    LS2 = INDEX(STRING(LS1+1:),'M') + LS1

	    READ(STRING(LS1+1:LS2-1),*) TLO1(NP1)
	    READ(STRING(LS2+1:LS3),*) THI1(NP1)
*
          ELSE
	    READ(STRING(1:LS3),*) TLO1(NP1), THI1(NP1)
          ENDIF
*
          NP1 = NP1 + 1
        END IF

      END DO
      IF ( STATUS .EQ. FIO__EOF ) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 999
      END IF

*    Read second input list
      NP2 = 1
      DO WHILE ( STATUS .EQ. SAI__OK )
*
        CALL FIO_READF( IFIL2, STRING, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
	  LS3 = CHR_LEN(STRING)
	  LS1 = INDEX(STRING,'M')
*
	  IF (LS1.NE.0) THEN
	    LS2 = INDEX(STRING(LS1+1:),'M') + LS1
	    READ(STRING(LS1+1:LS2-1),*) TLO2(NP2)
	    READ(STRING(LS2+1:LS3),*) THI2(NP2)
	  ELSE
	    READ(STRING(1:LS3),*) TLO2(NP2), THI2(NP2)
	  ENDIF
*
	  NP2 = NP2 + 1
*
        END IF

      ENDDO
      IF ( STATUS .EQ. FIO__EOF ) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 999
      END IF

*    Merge the lists
      CALL SLOTMRG_TMERGE (TLO1,THI1,NP1,TLO2,THI2,
     &                                     NP2,TLOM,THIM,NPM)
*
      IF (NPM .GT. 0) THEN
*
         TOT = 0.
*
         DO N = 1,NPM
*
	    IF (MFLAG) THEN
	      WRITE(STRING,300) TLOM(N),THIM(N)
300	      FORMAT(2('M',D21.16,1x))
	    ELSE
	      WRITE(STRING,*) TLOM(N), THIM(N)
	    ENDIF
            CALL FIO_WRITE( OFIL, STRING(1:60), STATUS )
*
	    TOT = (THIM(N)-TLOM(N))*86400. + TOT
*
        END DO
*
        CALL MSG_SETI( 'NS', NPM )
        CALL MSG_FMTR( 'DUR', 'F8.1', TOT )
        CALL MSG_PRNT('   There are ^NS merged slots, total duration '
     :		                                        //'^DUR secs')

      ELSE
        CALL MSG_PRNT( '   There are no merged slots from the '/
     :                                            /'i/p lists' )
      END IF

*    Close inputs and output
      CALL FIO_CLOSE( IFIL1, STATUS )
      CALL FIO_CLOSE( IFIL2, STATUS )
      CALL FIO_CLOSE( OFIL, STATUS )

*    Tidy up
 999  CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+SLOTMRG_TMERGE Merge two series of on/off time windows
	SUBROUTINE SLOTMRG_TMERGE (TS1, TE1, NW1, TS2, TE2,
     &                   NW2, TS12, TE12, NW12)
	IMPLICIT NONE
* Input
	INTEGER		NW1
	DOUBLE PRECISION  TS1(*),TE1(*)
	INTEGER		NW2
	DOUBLE PRECISION  TS2(*),TE2(*)
* output
	INTEGER		NW12
	DOUBLE PRECISION	TS12(*),TE12(*)
*M Denby Jan-88
*M Denby Apr-90 Improve efficiency by removing work arrays
*-
* local
	INTEGER		N, N1, N2, N12, NTOT
	INTEGER		I, J, NCOUNT
	INTEGER		MAXWIN
	DOUBLE PRECISION TC(40000), TEMP
	INTEGER 	STATE(40000), STEMP, ON, OFF
	PARAMETER 	(ON=1, OFF=0)
	PARAMETER	(MAXWIN = 10000)

* Form the windows into single vectors with alternating ON,OFF states
	N1 = 2*NW1
	N2 = 2*NW2

	DO N = 1, NW1
	  TC(2*N-1) = TS1(N)
	  STATE(2*N-1) = ON
	  TC(2*N) = TE1(N)
	  STATE(2*N) = OFF
	ENDDO

	DO N=1, NW2
	  TC((2*N-1)+N1) = TS2(N)
	  STATE((2*N-1)+N1) = ON
	  TC(2*N+N1) = TE2(N)
	  STATE(2*N+N1) = OFF
	ENDDO

* Bubble sort the times - do states at the same time
	NTOT = N1 + N2
	DO J = 2, NTOT
	  DO I = J, 2, -1
	    IF(TC(I) .GE. TC(I-1)) GOTO 10
	      TEMP = TC(I)
	      TC(I) = TC(I-1)
	      TC(I-1) = TEMP
	      STEMP = STATE(I)
	      STATE(I) = STATE(I-1)
	      STATE(I-1) = STEMP
	    ENDDO
10	ENDDO

* Now do the clever bit - sort out the overlaps.
	N12 = 0
	NCOUNT = 0
	DO I = 1, NTOT
	  IF (STATE(I) .EQ. ON) THEN
	    IF (NCOUNT .EQ. 1) THEN
	      NCOUNT = NCOUNT + 1
	      N12 = N12 + 1
	      TC(N12) = TC(I)
	    ELSEIF(NCOUNT .EQ. 0)THEN
	      NCOUNT = NCOUNT + 1
	    ENDIF
	  ELSEIF (STATE(I) .EQ. OFF) THEN
	    IF (NCOUNT .EQ. 2) THEN
	      NCOUNT = NCOUNT - 1
	      N12 = N12 + 1
	      TC(N12) = TC(I)
	    ELSEIF (NCOUNT .EQ. 1) THEN
	      NCOUNT = NCOUNT - 1
	    ENDIF
	  ENDIF
	ENDDO

* Form the processed list of alternating ON,OFFs into windows
	NW12=MIN(N12/2,MAXWIN)
	DO N=1,NW12
	  TS12(N)=TC(2*N-1)
	  TE12(N)=TC(2*N)
	ENDDO

	END
