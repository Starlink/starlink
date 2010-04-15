C+
      SUBROUTINE IPCS2STOKES(STATUS)
C
C            I P C S 2 S T O K E S
C
C     Command name:
C        IPCS2STOKES
C
C     Function:
C        Reduce IPCS spectropolarimetry data.
C
C     Description:
C        IPCS2STOKES reduces data obtained with the AAO Pockels cell
C        spectropolarimeter with the IPCS as detector. The data is read
C        in the form of Figaro files each containing a pair of A and B
C        state frames forming a single observation. Within each A and
C        B frame there are four spectra corresponding to the O and E rays
C        for each of two apertures (A and B). These spectra are combined
C        to derive a Stokes parameter spectrum in TSP format.
C
C     Parameters:
C    (1) FIGARO     (Char)     The input Figaro data file.
C        ASTART     (Integer)  The Start column for the A aperture data.
C        BSTART     (Integer)  The Start column for the B aperture data.
C        OESEP      (Integer)  The distance in columns from the start of the O
C                               spectrum to the start of the E spectrum.
C        WIDTH      (Integer)  The number of columns to include in each
C                               extracted spectrum.
C        APERTURE   (Char)     The aperture containing the star (A or B).
C        STOKESPAR  (Char)     The Stokes parameter (Q,U,V).
C        OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         17/03/2000
C
C-
C
C  History:
C    27/4/1988   Original Version.   JAB/AAO
C    20/11/1991  Use DSA for Figaro access.   JAB/AAO
C    17/03/2000  Added DOUBLE PRECISION dummy argument DDUMMY.   BLY/RAL
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,XPTR
      INTEGER OIPTR,OEPTR,OSPTR,OXPTR

*  Number of elements
      INTEGER NELM

*  Dimensions of array
      INTEGER NDIM, DIMS(7)
      INTEGER LENNAME

*  Parameters giving position of spectra
      INTEGER ASTART,BSTART,OESEP,WIDTH,SEP

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,TLOC,XLOC,SLOC,ELOC,ESLOC,OXLOC
      CHARACTER*64 FNAME,LABEL,UNITS,XLABEL,XUNITS,STOKESPAR,ERRMES,AP

*  Figaro slots
      INTEGER ISLOT,XSLOT
      CHARACTER*64 STRINGS(2)
      LOGICAL OK
      INTEGER DUMMY
      DOUBLE PRECISION DDUMMY

*  Access the Figaro frame

      CALL PAR_GET0C('FIGARO',FNAME,STATUS)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME,STATUS)

*  Get the data array

      IF (STATUS .EQ. SAI__OK) THEN

*  Find size of data

         CALL DSA_DATA_SIZE('INPUT',7,NDIM,DIMS,NELM,STATUS)

*  Check for valid dimensions

         IF (NDIM .NE. 2) THEN
            CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :          STATUS)
            STATUS = USER__001
         ELSE

*  A/B separation is half of array size

            SEP = DIMS(2)/2

*  Map the data

            CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,ISLOT,
     :           STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_OUT(' ','Error Mapping Input Data',STATUS)
               GOTO 100
            ENDIF

*  Get label and units of data

            CALL DSA_GET_DATA_INFO('INPUT',2,STRINGS,1,DDUMMY,STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
                LABEL = STRINGS(2)
                UNITS = STRINGS(1)
            ELSE

*  Set to blank strings if not present in input data

                LABEL = ' '
                UNITS = ' '
                STATUS = SAI__OK
            ENDIF

*  Map .X.DATA if it exists

            CALL DSA_MAP_AXIS_DATA('INPUT',1,'READ','FLOAT',XPTR,
     :           XSLOT,STATUS)

*  Get label and units of X axis

            CALL DSA_GET_AXIS_INFO('INPUT',1,2,STRINGS,0,DDUMMY,STATUS)
            XLABEL = STRINGS(2)
            XUNITS = STRINGS(1)

*  Get information on location of spectra

             CALL PAR_GET0I('ASTART',ASTART,STATUS)
             CALL PAR_GET0I('BSTART',BSTART,STATUS)
             CALL PAR_GET0I('OESEP',OESEP,STATUS)
             CALL PAR_GET0I('WIDTH',WIDTH,STATUS)

*  Check for validity

             IF (ASTART+(WIDTH-1) .GT. SEP .OR.
     :           ASTART+(WIDTH-1)+OESEP .GT. SEP .OR.
     :           BSTART+(WIDTH-1) .GT. SEP .OR.
     :           BSTART+(WIDTH-1)+OESEP .GT. SEP) THEN
                 CALL MSG_OUT(' ','Invalid Position of Spectra',STATUS)
                 GOTO 100
             ENDIF

*  Which aperture

             OK = .FALSE.
             DO WHILE ((.NOT. OK) .AND. STATUS .EQ. SAI__OK)
                 CALL PAR_GET0C('APERTURE',AP,STATUS)
                 OK = (AP .EQ. 'A' .OR. AP .EQ. 'B')
                 IF (.NOT. OK) THEN
                     CALL PAR_CANCL('APERTURE',STATUS)
                 ENDIF
             ENDDO


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
                CALL TSP_IPCS2STOKES(DIMS(1),DIMS(2),%VAL(IPTR),
     :           ASTART,BSTART,OESEP,WIDTH,AP,SEP,%VAL(OIPTR),
     :           %VAL(OSPTR),%VAL(OEPTR),
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



      SUBROUTINE TSP_IPCS2STOKES(SIZE,NY,I1,ASTART,BSTART,OESEP,WIDTH,
     :   AP,SEP,INT,STOKES,VSTOKES,IX,OX)
*+
*
*   T S P _ I P C S 2 S T O K E S
*
*   Reduce IPCS spectropolarimetry
*
*   (>)  NY      (Integer)  Size of spatial axis of input array
*   (>)  SIZE    (Integer)  Number of spectral channels
*   (>)  I1      (Real array(NY,SIZE) First input array
*   (>)  ASTART  (Integer)  Start of A aperture
*   (>)  BSTART  (Integer)  Start of B aperture
*   (>)  OESEP   (Integer)  Separation of O and E spectra
*   (>)  WIDTH   (Integer)  Width of each spectrum
*   (>)  AP      (Char)     Name of aperture (A or B)
*   (>)  SEP     (Integer)  Separation of A and B Pockels states
*   (<)  INT     (Real array(SIZE))  Output intensity
*   (<)  STOKES  (Real array(SIZE))  Output Stokes data
*   (<)  VSTOKES (Real array(SIZE))  Variance on Stokes parameter
*   (>)  IX      (Real array(SIZE))  Input X axis array
*   (<)  OX      (Real array(SIZE))  Output X axis array
*
*
*   Jeremy Bailey   24/9/1988
*
*   Modified:
*      20/11/1991    Handle bad values
*+


      IMPLICIT NONE

*  Parameters
      INTEGER SIZE,NY,ASTART,BSTART,OESEP,WIDTH,SEP
      CHARACTER*(*) AP
      REAL I1(SIZE,NY)
      REAL INT(SIZE),STOKES(SIZE),VSTOKES(SIZE)
      REAL IX(SIZE),OX(SIZE)

*  Local variables
      INTEGER I,J,K
      REAL SPEC2(8)
      INTEGER PT(8)
      REAL STAR,SKY

*  Copy X axis array
      DO I=1,SIZE
         OX(I)=IX(I)
      ENDDO

*  Set up position of eight spectra
      IF (AP .EQ. 'A') THEN
          PT(1) = ASTART
          PT(2) = ASTART+OESEP
          PT(3) = BSTART
          PT(4) = BSTART+OESEP
      ELSE
          PT(1) = BSTART
          PT(2) = BSTART+OESEP
          PT(3) = ASTART
          PT(4) = ASTART+OESEP
      ENDIF
      DO I=5,8
          PT(I) = PT(I-4)+SEP
      ENDDO

*  Loop over spectral points
      DO I=1,SIZE

*  Sum the data within each of the eight spectra
          DO J=1,8
              SPEC2(J) = I1(I,PT(J))
              DO K=1,WIDTH-1
                  SPEC2(J) = SPEC2(J)+I1(I,PT(J)+K)
              ENDDO
          ENDDO

*  Form STAR and SKY data
          STAR = SPEC2(1)+SPEC2(2)+SPEC2(5)+SPEC2(6)
          SKY = SPEC2(3)+SPEC2(4)+SPEC2(7)+SPEC2(8)

*  Calculate Intensity
          INT(I) = STAR-SKY

*  Calculate stokes variance
          VSTOKES(I) = SQRT(STAR*STAR+SKY*SKY)

*  Calculate Stokes parameters
          STAR = SPEC2(2)-SPEC2(1)+SPEC2(5)-SPEC2(6)
          SKY = SPEC2(4)-SPEC2(3)+SPEC2(7)-SPEC2(8)
          STOKES(I) = STAR-SKY
      ENDDO
      END


