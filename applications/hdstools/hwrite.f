      SUBROUTINE HWRITE(STATUS)
*+
* Name:
*    HWRITE

* Purpose:
*    Write an HDS object into formatted/unformatted file.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    hwrite inp file [binary=] [fmt=] 

* ADAM Parameters:
*    INP = UNIV (Read)
*       Object to be input and read. <GLOBAL.HDSOBJ>
*    FILE = _CHAR (Read)
*       Output filename.
*    BINARY = _LOGICAL (Read)
*       Whether file to be binary (Fortran unformatted). [NO]
*    FMT = _CHAR (Read)
*       Output format for ASCII file. If null (!) is specified, a default
*       format is used for each different data type. [!]

* Description :
*      Writes values from an HDS primitive object into an ASCII (default)
*      or binary file. One value is written per record.
* 
*      For ASCII file output a default format is used unless explicitly
*      overridden.
 
* Examples:
*    % hwrite cfile.structure.data values.dat
*       Write component DATA to ASCII file values.dat with default format
*
*    % hwrite cfile.structure.data values.dat fmt=F12.6
*       As above but with specified format
* 
*    % hwrite cfile.structure.data values.dat binary
*       Write component DATA in binary form into sequential, unformatted
*       file.

* Method :

* Bugs :

* Authors :
*    RF:  Ray Forbes (Birmingham University)
*    DJA: D.J. Allan (Birmingham University)
*    AJC: A.J. Chipperfield (Starlink, RAL)
*    TIMJ: Tim Jenness (JAC, Hawaii)

* History:
*    ??-??-???? (RF):
*       Original Version
*     3-JUN-1992 (DJA):
*       Use ERR_ANNUL
*    24-NOV-1994 (DJA):
*       V1.8-0 Now use USI for user interface
*     6-SEP-2001 (AJC):
*       V3.0-0 Remove Asterix stuff
*       Add proper prologue
*    11-JAN-2002 (AJC):
*       Change default formats (required for Linux)
*       _REAL G -> G15.7
*       _DOUBLE G -> G25.16
*       _INTEGER I -> I11
*       _LOGICAL L -> L1
*     18-JUL-2007 (TIMJ):
*       Add CNF_PVAL for 64-bit
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'MSG_PAR'
      INCLUDE 'CNF_PAR'

*    Status :
      INTEGER status

*    Local Constants :
	CHARACTER*30 VERSION		! Version number
        PARAMETER ( VERSION='HWRITE Version 3.0-0')

*    Local variables :
	CHARACTER*80 FILNAM		! name of FORTRAN file
	CHARACTER*(DAT__SZTYP) type	! type descriptor
	CHARACTER*(DAT__SZLOC) loc	! locator to
        CHARACTER*15 FMT

	INTEGER LUN			! I/O channel number
	INTEGER IERR			! I/O error code
        INTEGER PTR
        INTEGER NVAL
        INTEGER CLEN                    ! Length of _CHAR

        LOGICAL BINARY
        LOGICAL PRIM
*.

*    Get MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*   Write out the version number
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

*    Obtain object name
      CALL DAT_ASSOC('INP','READ',LOC,STATUS)

*    Obtain filename
      CALL PAR_GET0C('FILE', FILNAM, status)

*    See if formatted or unformatted
      CALL PAR_GET0L('BINARY',BINARY,STATUS)

*  If formatted see if format specified
      IF (.NOT.BINARY) THEN
        CALL PAR_GET0C('FMT',FMT,STATUS)
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
          STATUS = SAI__ERROR
          CALL ERR_REP(' ', 'Failed to open output file', STATUS)
          STATUS=SAI__ERROR
        ENDIF

* Find out type and whether primitive
	CALL DAT_TYPE ( LOC,TYPE,STATUS)
        CALL DAT_PRIM(LOC,PRIM,STATUS)

        IF (PRIM) THEN
          IF ( type .eq. '_LOGICAL' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'READ',PTR,NVAL,STATUS)
	    CALL HWRITE_LPUT ( LUN, BINARY, FMT,%VAL(CNF_PVAL(PTR)), 
     :                  NVAL,STATUS)
	  ELSEIF (( type .eq. '_INTEGER' )  .OR.
     &         ( type .eq. '_UWORD'   )  .OR.
     &         ( type .eq. '_WORD'    )  .OR.
     &         ( type .eq. '_UBYTE'   )  .OR.
     &         ( type .eq. '_BYTE'    )) THEN
            CALL DAT_MAPV(LOC,'_INTEGER','READ',PTR,NVAL,STATUS)
            CALL HWRITE_IPUT ( LUN, BINARY, FMT,%VAL(CNF_PVAL(PTR)),
     :                         NVAL,STATUS)
          ELSEIF ( type .eq. '_REAL' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'READ',PTR,NVAL,STATUS)
            CALL HWRITE_RPUT ( LUN, BINARY, FMT,%VAL(CNF_PVAL(PTR)),
     :                         NVAL,STATUS)
          ELSEIF ( type .eq. '_DOUBLE' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'READ',PTR,NVAL,STATUS)
            CALL HWRITE_DPUT ( lun, BINARY, FMT,%VAL(CNF_PVAL(PTR)),
     :                         NVAL,STATUS)
          ELSEIF ( type(1:5) .eq. '_CHAR' ) THEN
            CALL DAT_CLEN(LOC,CLEN,STATUS)
            CALL DAT_MAPV(LOC,TYPE,'READ',PTR,NVAL,STATUS)
            CALL HWRITE_CPUT ( lun, BINARY,%VAL(CNF_PVAL(PTR)),
     :                         NVAL,FMT,STATUS,
     :                         %VAL(CNF_CVAL(CLEN)))
          ENDIF

          CALL DAT_UNMAP(LOC,STATUS)

        ELSEIF (STATUS.EQ.SAI__OK) THEN
          STATUS=SAI__ERROR
          CALL ERR_REP(' ','Not a primitive or primitive array',STATUS)
        ENDIF

*	  Close the data file
        CLOSE (UNIT=LUN)
        CALL FIO_PUNIT(LUN,STATUS)

      ENDIF

      END



* Write a character string to a file
        SUBROUTINE HWRITE_CPUT ( lun, BINARY, X,N,FMT,STATUS)
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
           STATUS=SAI__ERROR
           CALL MSG_SETI('CODE',IERR)
	   CALL ERR_REP(' ', 'File write error, code=^CODE', STATUS)
	ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ', 'from HWRITE_CPUT', STATUS)
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
            FMT='(L1)'
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
           STATUS=SAI__ERROR
           CALL MSG_SETI('CODE',IERR)
	   CALL ERR_REP(' ', 'File write error, code=^CODE', STATUS)
	ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ', 'from HWRITE_LPUT', STATUS)
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
            FMT='(G15.7)'
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
           STATUS=SAI__ERROR
           CALL MSG_SETI('CODE',IERR)
	   CALL ERR_REP(' ', 'File write error, code=^CODE', STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ', 'from HWRITE_RPUT', STATUS)
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
            FMT='(G25.16)'
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
           STATUS=SAI__ERROR
           CALL MSG_SETI('CODE',IERR)
	   CALL ERR_REP(' ', 'File write error, code=^CODE', STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ', 'from HWRITE_DPUT', STATUS)
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
            FMT='(I11)'
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
           STATUS=SAI__ERROR
           CALL MSG_SETI('CODE',IERR)
	   CALL ERR_REP(' ', 'File write error, code=^CODE', STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ', 'from HWRITE_IPUT', STATUS)
        ENDIF

      ENDIF

      END


