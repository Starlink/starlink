C+
      SUBROUTINE RFIGARO(STATUS)
C
C            R F I G A R O
C
C     Command name:
C        RFIGARO
C
C     Function:
C        Read a Stokes Parameter Spectrum from a Figaro image
C
C     Description:
C        RFIGARO reads an n by 3 Figaro image and creates a TSP Stokes
C        parameter spectrum. The First row of the image becomes the
C        intensity spectrum. The second row becomes the Stokes spectrum
C        and the third row becomes the Stokes variance. It allows data
C        files created by the old VISTA BASIC spectropolarimetry package
C        to be read into TSP.
C
C     Parameters:
C    (1) FIGARO     (Char)     The input Figaro data file.
C    (2) STOKESPAR  (Char)     The Stokes parameter (Q,U,V) for the
C                               output data.
C    (3) OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support:
C          Jeremy Bailey, AAO
C
C     Version date:
C          17/03/2000
C
C-
C
C  History:
C    Aug/1987   Original Version.   JAB/AAO
C    26/2/1988   TSP Monolith version.  JAB/AAO
C    17/03/2000  Added DOUBLE PRECISION dummy argument DDUMMY.   BLY/RAL
C    21/08/2005  Fix typo in argument! TIMJ/JACH
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,EPTR,SPTR,XPTR
      INTEGER OIPTR,OEPTR,OSPTR,OXPTR

*  Number of elements
      INTEGER NELM

*  Dimensions of data array
      INTEGER NDIM, DIMS(7)

*  Length of name
      INTEGER LENNAME

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,TLOC,XLOC,SLOC,ELOC,ESLOC,OXLOC

      CHARACTER*80 FNAME,LABEL,UNITS,XLABEL,XUNITS,STOKESPAR,ERRMES
      LOGICAL OK
      INTEGER ISLOT,XSLOT
      CHARACTER*64 STRINGS(2)
      INTEGER DUMMY
      DOUBLE PRECISION DDUMMY

      INTEGER ICH_LEN

*  Access the Figaro frame
      CALL PAR_GET0C('FIGARO',FNAME,STATUS)
      LENNAME = ICH_LEN(FNAME)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME(:LENNAME),STATUS)

*  Get the data array
      IF (STATUS .EQ. SAI__OK) THEN

*  Get dimensions of data array
         CALL DSA_DATA_SIZE('INPUT',7,NDIM,DIMS,NELM,STATUS)

*  Check that it is one dimensional
         IF ((NDIM .NE. 2) .OR. (DIMS(2) .NE. 3)) THEN
            CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :          STATUS)
            STATUS = USER__001
         ELSE

*  Map the data
            CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,ISLOT,
     :            STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_OUT(' ','Error Mapping Input Data',STATUS)
               GOTO 100
            ENDIF

*  Pointers to the Stokes array and the variance
            SPTR = IPTR+4*DIMS(1)
            EPTR = SPTR+4*DIMS(1)

*  Get Label and Units for data

            CALL DSA_GET_DATA_INFO('INPUT',2,STRINGS,1,DDUMMY,STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               LABEL = STRINGS(2)
               UNITS = STRINGS(1)
            ELSE
               LABEL = 'Intensity'
               UNITS = ' '
               STATUS = SAI__OK
            ENDIF

*  Map .X.DATA if it exists

            CALL DSA_MAP_AXIS_DATA('INPUT',1,'READ','FLOAT',XPTR,
     :             XSLOT,STATUS)
            CALL DSA_GET_AXIS_INFO('INPUT',1,2,STRINGS,0,DDUMMY,STATUS)
            XLABEL = STRINGS(2)
            XUNITS = STRINGS(1)

*  Which Stokes Parameter?

             OK = .FALSE.
             DO WHILE ((.NOT. OK) .AND. STATUS .EQ. SAI__OK )
                CALL PAR_GET0C('STOKESPAR',STOKESPAR,STATUS)
                OK = (STOKESPAR .EQ. 'Q' .OR. STOKESPAR .EQ. 'U' .OR.
     :                  STOKESPAR .EQ. 'V')
                IF (.NOT. OK) THEN
                   CALL PAR_CANCL('STOKESPAR',STATUS)
                ENDIF
             ENDDO

*  Get the output file

             CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
             CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Create the structure

             CALL TSP_CREATE_1D(OLOC,DIMS(1),STOKESPAR,.FALSE.,
     :            .TRUE.,STATUS)

*  Map axis array

             CALL TSP_MAP_LAMBDA(OLOC,'WRITE',OXPTR,OXLOC,STATUS)
             CALL TSP_WLU_LAMBDA(OLOC,XLABEL,XUNITS,STATUS)

*  Get Stokes Structure

             CALL TSP_GET_STOKES(OLOC,STOKESPAR,SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'WRITE',OSPTR,ESLOC,STATUS)
             CALL TSP_MAP_VAR(SLOC,'WRITE',OEPTR,ELOC,STATUS)
             CALL TSP_WLU(OLOC,LABEL,UNITS,STATUS)
             CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,ILOC,STATUS)

*  Copy the data

             IF (STATUS .EQ. SAI__OK) THEN
                CALL TSP_READ_COPY(DIMS(1),%VAL(IPTR),%VAL(OIPTR),
     :           %VAL(SPTR),%VAL(OSPTR),%VAL(EPTR),%VAL(OEPTR),
     :           %VAL(XPTR),%VAL(OXPTR))
             ENDIF

*  Unmap output arrays and annul locators

             CALL TSP_UNMAP(ILOC,STATUS)
             CALL TSP_UNMAP(ESLOC,STATUS)
             CALL TSP_UNMAP(XLOC,STATUS)
             CALL TSP_UNMAP(ELOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)
             CALL DAT_ANNUL(OLOC,STATUS)

          ENDIF
100       CONTINUE
          CALL DSA_CLOSE(STATUS)
      ENDIF
      END




      SUBROUTINE TSP_READ_COPY(SIZE,I1,O1,I2,O2,I3,O3,I4,O4)
*+
*
*   T S P _ R E A D _ C O P Y
*
*   RFIGARO command
*
*   Routine to copy data from Figaro input arrays to tSP output
*   arrays
*
*   Parameters:
*
*   (>)  SIZE      (Integer)            Size of arrays to be copied
*   (>)  I1        (Real array(SIZE))   First input array
*   (<)  O1        (Real array(SIZE))   First output array
*   (>)  I2        (Real array(SIZE))   Second input array
*   (<)  O2        (Real array(SIZE))   Second output array
*   (>)  I3        (Real array(SIZE))   Third input array
*   (<)  O3        (Real array(SIZE))   Third output array
*   (>)  I4        (Real array(SIZE))   Fourth input array
*   (<)  O4        (Real array(SIZE))   Fourth output array
*
*   Jeremy Bailey     26/2/1988
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER SIZE
      REAL I1(SIZE),O1(SIZE),I2(SIZE),O2(SIZE),I3(SIZE),O3(SIZE)
      REAL I4(SIZE),O4(SIZE)

*  Local variable
      INTEGER I

*  Loop over points copying each input array to corresponding output array
      DO I=1,SIZE
         O1(I)=I1(I)
         O2(I)=I2(I)
         O3(I)=I3(I)
         O4(I)=I4(I)
      ENDDO
      END

