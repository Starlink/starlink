*+  HWRITE - writes an HDS object into formatted/unformatted file
      SUBROUTINE HWRITE(STATUS)
*    Description :
*     Parameters:
*    Method :
*    Bugs :
*    Authors :
*     Ray Forbes (BHVAD::RJV)
*    History :
*
*      3 Jun 92 : Use ERR_ANNUL (BHVAD::DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    status :
      INTEGER status
*    local Constants :
	CHARACTER*30 VERSION		! Version number
        PARAMETER ( VERSION='HWRITE Version 1.8-0')
*    local variables :
	CHARACTER*80 FILNAM		! name of FORTRAN file
	CHARACTER*(DAT__SZTYP) type	! type descriptor
	CHARACTER*(DAT__SZLOC) loc	! locator to
        CHARACTER*15 FMT

	INTEGER LUN			! I/O channel number
	INTEGER IERR			! I/O error code
        INTEGER PTR
        INTEGER NVAL

        LOGICAL BINARY
        LOGICAL PRIM
*-

*   Write out the version number
      CALL MSG_PRNT(VERSION)

*    Start ASTERIX
      CALL AST_INIT()

*    Obtain object name
      CALL USI_DASSOC('INP','READ',LOC,STATUS)

*    Obtain filename
      CALL USI_GET0C('FILNAM', FILNAM, status)

*    See if formatted or unformatted
      CALL USI_GET0L('BINARY',BINARY,STATUS)

*  If formatted see if format specified
      IF (.NOT.BINARY) THEN
        CALL USI_GET0C('FMT',FMT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          FMT=' '
          CALL ERR_ANNUL(STATUS)
        ENDIF
      ENDIF

* Open file
      CALL FIO_GUNIT(LUN,STATUS)

      IF (STATUS.EQ.SAI__OK) THEN
        IF (BINARY) THEN
          OPEN(UNIT=lun, FILE=filnam, STATUS='NEW',FORM='UNFORMATTED',
     :                                                     IOSTAT=ierr)
        ELSE
          OPEN(UNIT=lun, FILE=filnam, STATUS='NEW',FORM='FORMATTED',
     :                                                     IOSTAT=ierr)
        ENDIF
        IF ( ierr .ne. 0 ) THEN
          CALL MSG_PRNT('! Failed to open input file')
          STATUS=SAI__ERROR
        ENDIF

* Find out type and whether primitive
	CALL DAT_TYPE ( LOC,TYPE,STATUS)
        CALL DAT_PRIM(LOC,PRIM,STATUS)

        IF (PRIM) THEN
          IF ( type .eq. '_LOGICAL' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'READ',PTR,NVAL,STATUS)
	    CALL HWRITE_LPUT ( LUN, BINARY, FMT,%VAL(PTR), NVAL,STATUS)
	  ELSEIF (( type .eq. '_INTEGER' )  .OR.
     &         ( type .eq. '_UWORD'   )  .OR.
     &         ( type .eq. '_WORD'    )  .OR.
     &         ( type .eq. '_UBYTE'   )  .OR.
     &         ( type .eq. '_BYTE'    )) THEN
            CALL DAT_MAPV(LOC,'_INTEGER','READ',PTR,NVAL,STATUS)
            CALL HWRITE_IPUT ( LUN, BINARY, FMT,%VAL(PTR),NVAL,STATUS)
          ELSEIF ( type .eq. '_REAL' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'READ',PTR,NVAL,STATUS)
            CALL HWRITE_RPUT ( LUN, BINARY, FMT,%VAL(PTR),NVAL,STATUS)
          ELSEIF ( type .eq. '_DOUBLE' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'READ',PTR,NVAL,STATUS)
            CALL HWRITE_dput ( lun, BINARY, FMT,%VAL(PTR),NVAL,STATUS)
          ELSEIF ( type(1:5) .eq. '_CHAR' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'READ',PTR,NVAL,STATUS)
            CALL HWRITE_cput ( lun, BINARY, FMT,%VAL(PTR),NVAL,STATUS)
          ENDIF

          CALL DAT_UNMAP(LOC,STATUS)

        ELSEIF (STATUS.EQ.SAI__OK) THEN
          CALL MSG_PRNT('! Not a primitive or primitive array')
          STATUS=SAI__ERROR
        ENDIF



*	  Close the data file
        CLOSE (UNIT=LUN)
        CALL FIO_PUNIT(LUN,STATUS)

      ENDIF

*    Shutdown ASTERIX
      CALL AST_CLOSE()
      CALL AST_ERR(STATUS)

      END



* Write a character string to a file
        SUBROUTINE HWRITE_CPUT ( lun, BINARY, FMT,X,N,STATUS)
*    type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'

	INTEGER        status            ! Error code

	INTEGER        LUN               ! File LU number
        LOGICAL BINARY
        CHARACTER*(*) FMT
        INTEGER N
        CHARACTER*(*) X(N)

        INTEGER        IERR              ! Error flag
        INTEGER I

        INTEGER CHR_LEN

      IF (STATUS.EQ.SAI__OK) THEN


        IF (BINARY) THEN
          I=1
          IERR=0
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            WRITE(LUN,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ELSE
          IF (FMT.EQ.' ') THEN
            FMT='(A)'
          ELSE
            FMT='('//FMT(:CHR_LEN(FMT))//')'
          ENDIF
          I=1
          IERR=0
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            WRITE(LUN,FMT,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF
 	IF ( IERR .ne. 0 ) THEN
           CALL MSG_SETI('CODE',IERR)
	   CALL MSG_PRNT('! File write error, code=^CODE')
           STATUS=SAI__ERROR
	ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HWRITE_CPUT',STATUS)
        ENDIF

      ENDIF

      END

* Read a logical from a file
        SUBROUTINE HWRITE_LPUT ( LUN, BINARY, FMT,X,N,STATUS)
*    type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'

	INTEGER        status           ! Error code

	INTEGER        LUN              ! File LU number
        LOGICAL        BINARY
        CHARACTER*(*)  FMT

        INTEGER N
        LOGICAL X(N)

	INTEGER        IERR             ! Error flag
        INTEGER I

        INTEGER CHR_LEN

      IF (STATUS.EQ.SAI__OK) THEN


        IF (BINARY) THEN
          I=1
          IERR=0
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            WRITE(LUN,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ELSE
          IF (FMT.EQ.' ') THEN
            FMT='(L)'
          ELSE
            FMT='('//FMT(:CHR_LEN(FMT))//')'
          ENDIF
          I=1
          IERR=0
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            WRITE(LUN,FMT,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

 	IF ( IERR .ne. 0 ) THEN
           CALL MSG_SETI('CODE',IERR)
	   CALL MSG_PRNT('! File write error, code=^CODE')
           STATUS=SAI__ERROR
	ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HWRITE_LPUT',STATUS)
        ENDIF

      ENDIF

      END

* Write a real number to a file
        SUBROUTINE HWRITE_RPUT ( LUN, BINARY, FMT,X,N,STATUS)
*    type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'

	INTEGER  	 status         ! Error code

	INTEGER  	 LUN            ! File LU number
        LOGICAL          BINARY
        CHARACTER*(*)    FMT
        INTEGER N
        REAL X(N)

	INTEGER		 IERR           ! Error flag
        INTEGER I

        INTEGER CHR_LEN

      IF (STATUS.EQ.SAI__OK) THEN

        IF (BINARY) THEN
          I=1
          IERR=0
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            WRITE(LUN,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ELSE
          IF (FMT.EQ.' ') THEN
            FMT='(G)'
          ELSE
            FMT='('//FMT(:CHR_LEN(FMT))//')'
          ENDIF
          I=1
          IERR=0
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            WRITE(LUN,FMT,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

        IF ( IERR .ne. 0 ) THEN
           CALL MSG_SETI('CODE',IERR)
	   CALL MSG_PRNT('! File write error, code=^CODE')
           STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HWRITE_RPUT',STATUS)
        ENDIF

      ENDIF

      END

* Write a double precision number to a file
        SUBROUTINE HWRITE_DPUT ( LUN, BINARY, FMT,X,N,STATUS)
*    type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'

	INTEGER  	 status         ! Error code

	INTEGER  	 LUN            ! File LU number
        LOGICAL          BINARY
        CHARACTER*(*)    FMT
        INTEGER N
        DOUBLE PRECISION X(N)

	INTEGER		 IERR           ! Error flag
        INTEGER I

        INTEGER CHR_LEN

      IF (STATUS.EQ.SAI__OK) THEN

        IF (BINARY) THEN
          I=1
          IERR=0
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            WRITE(LUN,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ELSE
          IF (FMT.EQ.' ') THEN
            FMT='(G)'
          ELSE
            FMT='('//FMT(:CHR_LEN(FMT))//')'
          ENDIF
          I=1
          IERR=0
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            WRITE(LUN,FMT,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

        IF ( IERR .ne. 0 ) THEN
           CALL MSG_SETI('CODE',IERR)
	   CALL MSG_PRNT('! File write error, code=^CODE')
           STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HWRITE_DPUT',STATUS)
        ENDIF

      ENDIF

      END

* Write an integer number to a file
        SUBROUTINE HWRITE_IPUT ( LUN, BINARY, FMT,X,N,STATUS)
*    type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'

	INTEGER  	 status         ! Error code

	INTEGER  	 LUN            ! File LU number
        LOGICAL          BINARY
        CHARACTER*(*)    FMT
        INTEGER N
        INTEGER X(N)

	INTEGER		 IERR           ! Error flag
        INTEGER I

        INTEGER CHR_LEN

      IF (STATUS.EQ.SAI__OK) THEN

        IF (BINARY) THEN
          I=1
          IERR=0
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            WRITE(LUN,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ELSE
          IF (FMT.EQ.' ') THEN
            FMT='(I)'
          ELSE
            FMT='('//FMT(:CHR_LEN(FMT))//')'
          ENDIF
          I=1
          IERR=0
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            WRITE(LUN,FMT,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

        IF ( IERR .ne. 0 ) THEN
           CALL MSG_SETI('CODE',IERR)
	   CALL MSG_PRNT('! File write error, code=^CODE')
           STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HWRITE_IPUT',STATUS)
        ENDIF

      ENDIF

      END


