*+  HREAD - Reads a formatted file into an HDS object
      SUBROUTINE HREAD(STATUS)
*    Description :
*     A user-specified file is read and converted into a HDS DATA_RECORD
*     This record can then be manipulated using other HDS editing facilities
*     The input file be formatted or unformatted with sequential access.

*     Parameters:
*    Method :
*    Bugs :
*    Authors :
*     Ray Forbes (BHVAD::RCF)
*    History :
*     02 Jun 87: original (BHVAD::RCF)
*     12 Apr 89: modified for ASTERIX88 (BHVAD::RJV)
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
        PARAMETER ( VERSION='HREAD Version 1.0-1')
*    local variables :
	CHARACTER*80 FILNAM		! name of FORTRAN file
	CHARACTER*(DAT__SZTYP) type	! type descriptor
	CHARACTER*(DAT__SZLOC) loc	! locator to


	INTEGER LUN			! I/O channel number
	INTEGER IERR			! I/O error code
        INTEGER PTR
        INTEGER NVAL

        LOGICAL PRIM
        LOGICAL BINARY
*-

*   Write out the version number
      CALL MSG_PRNT(VERSION)

* Obtain filename and whether binary or ASCII
      CALL PAR_GET0C('FILE', FILNAM,STATUS)
      CALL PAR_GET0L('BINARY',BINARY,STATUS)

* Obtain object name
      CALL DAT_ASSOC('OUT','UPDATE',LOC,STATUS)

* Open file
      CALL FIO_GUNIT(LUN,STATUS)

      IF (STATUS.EQ.SAI__OK) THEN
        IF (BINARY) THEN
          OPEN(UNIT=lun, FILE=filnam, STATUS='OLD', FORM='UNFORMATTED',
     :                                                      IOSTAT=ierr)
        ELSE
          OPEN(UNIT=lun, FILE=filnam, STATUS='OLD', FORM='FORMATTED',
     :                                                    IOSTAT=ierr)
        ENDIF
        IF ( ierr .ne. 0 ) THEN
          CALL MSG_PRNT('! Failed to open input file')
          STATUS=SAI__ERROR
        ENDIF

*  See if primitive
        CALL DAT_PRIM(LOC,PRIM,STATUS)

        IF (PRIM) THEN

          CALL DAT_TYPE ( LOC, TYPE, STATUS)

   	  IF ( type .eq. '_LOGICAL' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'UPDATE',PTR,NVAL,STATUS)
	    CALL HREAD_LPUT ( LUN, BINARY,%VAL(PTR),NVAL,STATUS)
	  ELSEIF (( type .eq. '_INTEGER' )  .OR.
     &         ( type .eq. '_UWORD'   )  .OR.
     &         ( type .eq. '_WORD'    )  .OR.
     &         ( type .eq. '_UBYTE'   )  .OR.
     &         ( type .eq. '_BYTE'    )) THEN
            CALL DAT_MAPV(LOC,'_INTEGER','UPDATE',PTR,NVAL,STATUS)
            CALL HREAD_IPUT ( LUN, BINARY,%VAL(PTR),NVAL,STATUS)
          ELSEIF ( type .eq. '_REAL' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'UPDATE',PTR,NVAL,STATUS)
            CALL HREAD_RPUT ( LUN, BINARY,%VAL(PTR),NVAL,STATUS)
          ELSEIF ( type .eq. '_DOUBLE' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'UPDATE',PTR,NVAL,STATUS)
            CALL HREAD_dput ( lun, BINARY,%VAL(PTR),NVAL,STATUS)
          ELSEIF ( type(1:5) .eq. '_CHAR' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'UPDATE',PTR,NVAL,STATUS)
            CALL HREAD_cput ( lun, BINARY,%VAL(PTR),NVAL,STATUS)
          ENDIF

          CALL DAT_UNMAP(LOC,STATUS)

        ELSE
            CALL MSG_PRNT('! Not a primitive or primitive array')
            STATUS=SAI__ERROR
        ENDIF

*	  Close the data file
	CLOSE (UNIT=LUN)
        CALL FIO_PUNIT(LUN,STATUS)
      ENDIF

      END


* Read a character string from a file
        SUBROUTINE HREAD_CPUT ( lun, BINARY,X,N,STATUS)
*    type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'PAR_ERR'

	INTEGER        status            ! Error code

	INTEGER        LUN               ! File LU number
        LOGICAL BINARY
        INTEGER N
        CHARACTER*(*) X(N)

        INTEGER        IERR              ! Error flag
        INTEGER I

      IF (STATUS.EQ.SAI__OK) THEN

        IF (BINARY) THEN
          IERR=0
          I=1
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            READ(LUN,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ELSE
          IERR=0
          I=1
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            READ(LUN,'(A)',IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

 	IF ( IERR .ne. 0 ) THEN
           CALL MSG_SETI('CODE',IERR)
	   CALL MSG_PRNT('! File read error, code=^CODE')
           STATUS=SAI__ERROR
	ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HREAD_CPUT',STATUS)
        ENDIF

      ENDIF

      END

* Read a logical from a file
        SUBROUTINE HREAD_LPUT ( LUN, BINARY,X,N,STATUS)
*    type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'PAR_ERR'

	INTEGER        status            ! Error code

	INTEGER        LUN               ! File LU number
        LOGICAL BINARY
        INTEGER N
        LOGICAL X(N)

        INTEGER        IERR              ! Error flag
        INTEGER I

      IF (STATUS.EQ.SAI__OK) THEN

        IF (BINARY) THEN
          IERR=0
          I=1
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            READ(LUN,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ELSE
          IERR=0
          I=1
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            READ(LUN,'(L)',IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

 	IF ( IERR .ne. 0 ) THEN
           CALL MSG_SETI('CODE',IERR)
	   CALL MSG_PRNT('! File read error, code=^CODE')
           STATUS=SAI__ERROR
	ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HREAD_LPUT',STATUS)
        ENDIF

      ENDIF

      END

* Read a real numeric string from a file
        SUBROUTINE HREAD_RPUT ( LUN, BINARY,X,N,STATUS)
*    type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'PAR_ERR'

	INTEGER        status            ! Error code

	INTEGER        LUN               ! File LU number
        LOGICAL BINARY
        INTEGER N
        REAL X(N)

        INTEGER        IERR              ! Error flag
        INTEGER I

      IF (STATUS.EQ.SAI__OK) THEN

        IF (BINARY) THEN
          IERR=0
          I=1
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            READ(LUN,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ELSE
          IERR=0
          I=1
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            READ(LUN,'(G)',IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

        IF ( IERR .ne. 0 ) THEN
           CALL MSG_SETI('CODE',IERR)
	   CALL MSG_PRNT('! File read error, code=^CODE')
           STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HREAD_RPUT',STATUS)
        ENDIF

      ENDIF

      END

* Read a real numeric string from a file
        SUBROUTINE HREAD_DPUT ( LUN, BINARY,X,N,STATUS)
*    type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'PAR_ERR'

	INTEGER        status            ! Error code

	INTEGER        LUN               ! File LU number
        LOGICAL BINARY
        INTEGER N
        DOUBLE PRECISION X(N)

        INTEGER        IERR              ! Error flag
        INTEGER I

      IF (STATUS.EQ.SAI__OK) THEN

        IF (BINARY) THEN
          IERR=0
          I=1
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            READ(LUN,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ELSE
          IERR=0
          I=1
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            READ(LUN,'(G)',IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

        IF ( IERR .ne. 0 ) THEN
           CALL MSG_SETI('CODE',IERR)
	   CALL MSG_PRNT('! File read error, code=^CODE')
           STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HREAD_DPUT',STATUS)
        ENDIF

      ENDIF

      END

* Read a real numeric string from a file
        SUBROUTINE HREAD_IPUT ( LUN, BINARY,X,N,STATUS)
*    type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'PAR_ERR'

	INTEGER        status            ! Error code

	INTEGER        LUN               ! File LU number
        LOGICAL BINARY
        INTEGER N
        INTEGER X(N)

        INTEGER        IERR              ! Error flag
        INTEGER I

      IF (STATUS.EQ.SAI__OK) THEN

        IF (BINARY) THEN
          IERR=0
          I=1
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            READ(LUN,IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ELSE
          IERR=0
          I=1
          DO WHILE (I.LE.N.AND.IERR.EQ.0)
            READ(LUN,'(I)',IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

        IF ( IERR .ne. 0 ) THEN
           CALL MSG_SETI('CODE',IERR)
	   CALL MSG_PRNT('! File read error, code=^CODE')
           STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HREAD_IPUT',STATUS)
        ENDIF

      ENDIF

      END

