C+
      SUBROUTINE FLIP(STATUS)
C
C            F L I P
C
C     Command name:
C        FLIP
C
C     Function:
C        Invert the sign of the Stokes parameter in a spectrum.
C
C     Description:
C        The Stokes array in the input dataset is sign changed to
C        produce the Stokes array of the output dataset.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D) - The input Stokes dataset.
C    (2) OUTPUT     (TSP, 1D) - The output dataset.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 27/4/1988
C
C-
C
C  History:
C    27/4/1988   Original Version.   JAB/AAO
C    6/12/1991   Handle bad values.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS
      INTEGER SPTR,SIZE

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,SLOC,SDLOC
      REAL QVAL,UVAL
      INTEGER ACTDIM,DIMS(7)
      INTEGER NUM
      LOGICAL QZ,UZ,VZ

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Create the output dataset

      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(LOC,OLOC,STATUS)

*  Get the size of the data

      CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)
      IF (ACTDIM .NE. 1) THEN
          CALL MSG_OUT('OUT','Invalid Dimensions',STATUS)
          STATUS = USER__001
      ENDIF
      SIZE = DIMS(1)

*  Find the Stokes parameters

      CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)

*  Try to get a Stokes parameter (first one found in sequence Q,U,V)

      IF (QZ) THEN
          CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)
      ELSE IF (UZ) THEN
          CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)
      ELSE IF (VZ) THEN
          CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)
      ELSE
          CALL MSG_OUT('MSG','No Stokes Parameter Found',STATUS)
          STATUS = USER__001
      ENDIF

*  Map the Stokes parameter

      CALL TSP_MAP_DATA(SLOC,'UPDATE',SPTR,SDLOC,STATUS)

*  Do the sign flip

      IF (STATUS .EQ. SAI__OK) THEN
        CALL TSP_FLIP(SIZE,%VAL(SPTR))
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(SDLOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END



       SUBROUTINE TSP_FLIP(SIZE,S)
*+
*
*   T S P _ F L I P
*
*   Subroutine to do the polarization sign flip
*
*    (>)  SIZE     (Integer)           The number of spectral points
*    (!)  S        (Real array(SIZE))  The stokes array
*
*    Jeremy Bailey    27/4/1988
*
*    Modified:
*       6/12/1991    Handle bad values
*
*+
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'

*  Parameters

      INTEGER SIZE
      REAL S(SIZE)

*  Local variables

      INTEGER I

          DO I=1,SIZE
            IF (S(I) .NE. VAL__BADR) THEN
              S(I) = -S(I)
            ENDIF
          ENDDO

      END

