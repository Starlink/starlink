      SUBROUTINE HREAD(STATUS)
*+
* Name:
*    HREAD

* Purpose:
*    Read a file into an HDS object.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    hread file out [binary=]

* ADAM Parameters:
*    FILE = _CHAR (Read)
*       Name of file to be read.
*    OUT = UNIV (Read)
*       HDS object to receive data. <GLOBAL.HDSOBJ>
*    BINARY = _LOGICAL (Read)
*       Whether file is binary. [NO]

* Description:
*    Values are read, one per record, from the file and written to the
*    specified HDS object. The number of values to be read is calculated
*    from the size of the HDS object. If fewer values are found in the file,
*    an error is reported but the given values will have been written, with
*    the remainder unspecified.
*    If the file is ASCII, it is read using a general format appropriate
*    for the type of the specified object. The HDS object must exist and be
*    primitive - it is written as if it were a vector.

* Examples:
*    % hread values.dat cfile.structure.data
*       Reads values from ASCII file values.dat and writes them to object
*       STRUCTURE.DATA in file cfile.sdf.
*
*    % hread values.dat cfile.structure.data binary
*       Reads values from binary file values.dat and writes them to object
*       STRUCTURE.DATA in file cfile.sdf.

* Method:

* Bugs:

* Authors:
*    RCF: R.J. Forbes (Birmingham University)
*    DJA: D.J. Allan (Birmingham University)
*    AJC: A.J. Chipperfield (Starlink, RAL)
*    TIMJ: Tim Jenness (JAC, Hawaii)

* History:
*     02-JUN-1987 (RCF):
*        Original
*     12-APR-1989 (RJV):
*        Modified for ASTERIX88 (BHVAD::RJV)
*     24-NOV-1994 (DJA):
*        V1.8-0 Now use USI for user interface (DJA)
*      6-SEP-2001 (AJC):
*        V3.0-0 Remove Asterix stuff
*        Add proper prologue
*     11-JAN-2002 (AJC):
*        Change default formats (required by Linux)
*        _REAL G -> G20.0
*        _DOUBLE G -> G25.0
*        _INTEGER I -> I11
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
        PARAMETER ( VERSION='HREAD Version 3.0-0')

*    Local variables :
	CHARACTER*80 FILNAM		! name of FORTRAN file
	CHARACTER*(DAT__SZTYP) type	! type descriptor
	CHARACTER*(DAT__SZLOC) loc	! locator to

	INTEGER LUN			! I/O channel number
	INTEGER IERR			! I/O error code
        INTEGER PTR
        INTEGER NVAL
        INTEGER CLEN                    ! Length of _CHAR

        LOGICAL PRIM
        LOGICAL BINARY
*-

*    Get MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*   Write out the version number
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

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
          STATUS=SAI__ERROR
          CALL ERR_REP(' ', 'Failed to open input file', STATUS)
        ENDIF

*  See if primitive
        CALL DAT_PRIM(LOC,PRIM,STATUS)

        IF (PRIM) THEN

          CALL DAT_TYPE ( LOC, TYPE, STATUS)

   	  IF ( type .eq. '_LOGICAL' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'WRITE',PTR,NVAL,STATUS)
	    CALL HREAD_LPUT ( LUN, BINARY,%VAL(CNF_PVAL(PTR)),NVAL,
     &                        STATUS)
	  ELSEIF (( type .eq. '_INTEGER' )  .OR.
     &         ( type .eq. '_UWORD'   )  .OR.
     &         ( type .eq. '_WORD'    )  .OR.
     &         ( type .eq. '_UBYTE'   )  .OR.
     &         ( type .eq. '_BYTE'    )) THEN
            CALL DAT_MAPV(LOC,'_INTEGER','WRITE',PTR,NVAL,STATUS)
            CALL HREAD_IPUT ( LUN, BINARY,%VAL(CNF_PVAL(PTR)),
     :                        NVAL,STATUS)
          ELSEIF ( type .eq. '_REAL' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'WRITE',PTR,NVAL,STATUS)
            CALL HREAD_RPUT ( LUN, BINARY,%VAL(CNF_PVAL(PTR)),
     :                        NVAL,STATUS)
          ELSEIF ( type .eq. '_DOUBLE' ) THEN
            CALL DAT_MAPV(LOC,TYPE,'WRITE',PTR,NVAL,STATUS)
            CALL HREAD_DPUT ( lun, BINARY,%VAL(CNF_PVAL(PTR)),
     :                        NVAL,STATUS)
          ELSEIF ( type(1:5) .eq. '_CHAR' ) THEN
            CALL DAT_CLEN(LOC,CLEN,STATUS)
            CALL DAT_MAPV(LOC,TYPE,'WRITE',PTR,NVAL,STATUS)
            CALL HREAD_CPUT ( lun, BINARY,%VAL(CNF_PVAL(PTR)),
     :                        NVAL,STATUS,
     :                        %VAL(CNF_CVAL(CLEN)))
          ENDIF

          CALL DAT_UNMAP(LOC,STATUS)

        ELSE
            STATUS=SAI__ERROR
            CALL ERR_REP( ' ', 'Not a primitive or primitive array',
     :       STATUS)
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
           STATUS=SAI__ERROR
           CALL MSG_SETI('CODE',IERR)
	   CALL ERR_REP(' ', 'File read error, code=^CODE', STATUS)
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
           STATUS=SAI__ERROR
           CALL MSG_SETI('CODE',IERR)
	   CALL ERR_REP(' ', 'File read error, code=^CODE', STATUS)
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
            READ(LUN,'(G20.0)',IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

        IF ( IERR .ne. 0 ) THEN
           STATUS=SAI__ERROR
           CALL MSG_SETI('CODE',IERR)
	   CALL ERR_REP(' ', 'File read error, code=^CODE', STATUS)
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
            READ(LUN,'(G25.0)',IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

        IF ( IERR .ne. 0 ) THEN
           STATUS=SAI__ERROR
           CALL MSG_SETI('CODE',IERR)
	   CALL ERR_REP(' ', 'File read error, code=^CODE', STATUS)
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
            READ(LUN,'(I11)',IOSTAT=IERR) X(I)
            I=I+1
          ENDDO
        ENDIF

        IF ( IERR .ne. 0 ) THEN
           STATUS=SAI__ERROR
           CALL MSG_SETI('CODE',IERR)
	   CALL ERR_REP(' ', 'File read error, code=^CODE', STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HREAD_IPUT',STATUS)
        ENDIF

      ENDIF

      END

