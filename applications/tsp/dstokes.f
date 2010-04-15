C+
      SUBROUTINE DSTOKES(STATUS)
C
C            D S T O K E S
C
C     Command name:
C        DSTOKES
C
C     Function:
C        Delete a Stokes parameter from a dataset.
C
C     Description:
C        Delete a Stokes component from the polarimetry
C        extension of a data structure.
C
C     Parameters:
C    (1) INPUT      (TSP, nD) - The input Stokes dataset.
C    (2) STOKESPAR  (Char) - The Stokes parameter (Q, U or V)
C    (3) OUTPUT     (TSP, nD) - The output dataset.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 8/3/1992
C
C-
C
C  History:
C    8/3/1992   Original Version.   JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC
      LOGICAL QZ,UZ,VZ
      CHARACTER*10 STOKESPAR
      INTEGER NUM

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get the Stokes parameter

      CALL PAR_GET0C('STOKESPAR',STOKESPAR,STATUS)

*  Create the output dataset

      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(LOC,OLOC,STATUS)

*  Find the Stokes parameters

      CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)

      IF (STOKESPAR .EQ. 'Q' .OR. STOKESPAR .EQ. 'q') THEN
          IF (.NOT. QZ) THEN
              CALL MSG_OUT(' ','Q Stokes Parameter not present',
     :             STATUS)
          ELSE
              CALL TSP_DELETE_STOKES(OLOC,'Q',STATUS)
          ENDIF
      ELSE IF (STOKESPAR .EQ. 'U' .OR. STOKESPAR .EQ. 'u') THEN
          IF (.NOT. UZ) THEN
              CALL MSG_OUT(' ','U Stokes Parameter not present',
     :             STATUS)
          ELSE
              CALL TSP_DELETE_STOKES(OLOC,'U',STATUS)
          ENDIF
      ELSE IF (STOKESPAR .EQ. 'V' .OR. STOKESPAR .EQ. 'v') THEN
          IF (.NOT. VZ) THEN
              CALL MSG_OUT(' ','V Stokes Parameter not present',
     :             STATUS)
          ELSE
              CALL TSP_DELETE_STOKES(OLOC,'V',STATUS)
          ENDIF
      ENDIF

*  Tidy up

      CALL DAT_ANNUL(OLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END

