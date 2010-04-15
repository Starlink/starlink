C+
      SUBROUTINE SLIST(STATUS)
C
C            S L I S T
C
C     Command name:
C        SLIST
C
C     Function:
C        Output a polarization spectrum in the form of an ASCII file
C
C     Description:
C        A polarization spectrum is output in the form of an ASCII file.
C        The file has 6 columns containing the wavelength, I,
C        Q, error on Q, U error on U.
C
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input dataset, a spectrum which must
C                               have Q and U Stokes parameters present.
C    (2) FILE       (File)     Output Ascii File.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 17/2/1993
C
C-
C
C  History:
C    17/2/1993   Original Version.   JAB/AAO
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INTEGER STATUS
      INTEGER IPTR,QPTR,UPTR,QEPTR,UEPTR,LPTR
      INTEGER SIZE,DIMS(3),ACTDIM
      CHARACTER*(DAT__SZLOC) LOC,IDLOC,QDLOC,UDLOC,QELOC,UELOC
      CHARACTER*(DAT__SZLOC) QLOC,ULOC,T1LOC,T2LOC,LLOC
      REAL LSTART,LEND
      INTEGER FD

      INTEGER ICH_LEN

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)
      CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
      SIZE = DIMS(1)

*  Get the Stokes parameter objects

      CALL TSP_GET_STOKES(LOC,'Q',QLOC,STATUS)
      CALL TSP_GET_STOKES(LOC,'U',ULOC,STATUS)

*  Map the data

      CALL TSP_MAP_DATA(LOC,'READ',IPTR,IDLOC,STATUS)
      CALL TSP_MAP_DATA(QLOC,'READ',QPTR,QDLOC,STATUS)
      CALL TSP_MAP_VAR(QLOC,'READ',QEPTR,QELOC,STATUS)
      CALL TSP_MAP_DATA(ULOC,'READ',UPTR,UDLOC,STATUS)
      CALL TSP_MAP_VAR(ULOC,'READ',UEPTR,UELOC,STATUS)

*  Map the wavelength array

      CALL TSP_MAP_LAMBDA(LOC,'READ',LPTR,LLOC,STATUS)

*  Get the output file
      CALL FIO_ASSOC('FILE','WRITE','LIST',0,FD,STATUS)

*  Output the data

      IF (STATUS .EQ. SAI__OK) THEN
        CALL TSP_SLIST(SIZE,%VAL(IPTR),%VAL(QPTR),%VAL(QEPTR),
     :   %VAL(UPTR),%VAL(UEPTR),%VAL(LPTR),FD,STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(IDLOC,STATUS)
      CALL TSP_UNMAP(QDLOC,STATUS)
      CALL TSP_UNMAP(UDLOC,STATUS)
      CALL TSP_UNMAP(QELOC,STATUS)
      CALL TSP_UNMAP(UELOC,STATUS)
      CALL TSP_UNMAP(LLOC,STATUS)
      CALL DAT_ANNUL(QLOC,STATUS)
      CALL DAT_ANNUL(ULOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL FIO_CANCL('FILE',STATUS)
      END


      SUBROUTINE TSP_SLIST(SIZE,I,Q,QE,U,UE,L,FD,STATUS)
*
*  Subroutine to list data to an ASCII file
*
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'
      INTEGER SIZE
      REAL I(SIZE),Q(SIZE),QE(SIZE),U(SIZE),UE(SIZE),L(SIZE)
      REAL QEE,UEE
      INTEGER FD
      INTEGER STATUS
      INTEGER J
      CHARACTER*80 BUFFER

      DO J = 1,SIZE
          IF (Q(J) .NE. VAL__BADR .AND. U(J) .NE. VAL__BADR
     :            .AND. I(J) .NE. VAL__BADR) THEN
              QEE = SQRT(QE(J))
              UEE = SQRT(UE(J))
              WRITE(BUFFER,'(6G12.4)') L(J),I(J),Q(J),QEE,U(J),UEE
              CALL FIO_WRITE(FD,BUFFER,STATUS)
          ENDIF
      ENDDO
      END



