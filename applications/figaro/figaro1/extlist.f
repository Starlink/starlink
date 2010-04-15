C+
      SUBROUTINE EXTLIST
C
C     E X T L I S T
C
C     Adds the rows from IMAGE specified by the array of row numbers in
C     SECTIONS and produces a 1D data object called SPECTRUM.
C
C     Command parameters -
C
C     'IMAGE'    The name of the image from which the rows
C                 are to be taken.
C
C     'NROWS'    The number of rows to be added.
C
C     'SECTIONS' The array of row numbers.
C
C     'SPECTRUM' The name of the resulting data.
C
C     Output data -
C
C     SPECTRUM is created with the same structure as IMAGE,
C     except that data array will only have one dimension, and if
C     IMAGE has Y-axis information, this will be omitted.  Any X-axis
C     information will be copied unchanged.
C
C                                   DJA / AAO 10th July 1987
C
C     Modified -
C
C     16th July 1987  DJA / AAO. Revised DSA_ routine calls. Specs
C                     for some have changed.
C     27th July 1987  DJA / AAO. Modified dynamic memory handling -
C                     now uses DYN_ package.
C     22nd June 1993  KS / AAO.  Include file syntax changed for
C                     portable version. Added calls to DSA_RESHAPE_xx
C                     routines.
C     27th July 1996  MJCL / Starlink, UCL.  PAR_ABORT checking.
C     2005 June 7     MJC / Starlink  Use CNF_PVAL for pointers to
C                     mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Maximum number of rows to be added
C
      INTEGER MAXROWS
      PARAMETER (MAXROWS=100)
C
C     Local variables
C
      INTEGER   DIMS(10)         ! The sizes of the dimensions of the data
      INTEGER   DSLOT            ! Map slot number used for data
      INTEGER   DPTR             ! Dynamic-memory pointer to image data
      INTEGER   I                !
      LOGICAL   IGNORE           ! Used to ignore status codes
      INTEGER   NDIM             ! Dimensionality of input data structure
      INTEGER   NELM             ! Total number of elements in the data
      INTEGER   NX               ! Total number of elements per cross-section
      INTEGER   NY               ! Total number of cross-section
      INTEGER   NROWS            ! The number of rows to be added together
      REAL      ROWS(MAXROWS)    ! Temporary real values of row numbers
      INTEGER   ROWNUMS(MAXROWS) ! Final numbers of rows to be added
      INTEGER   SPTR             ! Dynamic-mem pointer to new spectrum data
      INTEGER   SSLOT            ! Map slot number used for data
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      REAL      VALUE            ! Temporary real number
      LOGICAL   YEXIST           ! TRUE if there is y-axis data
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get and open image file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('Input data is not an image',IGNORE)
         GO TO 500
      END IF
      NY=DIMS(2)
      NX=DIMS(1)
C
C     See if we have a data component giving Y-values
C
      CALL DSA_SEEK_AXIS ('IMAGE',2,YEXIST,STATUS)
C
C     Read the number of rows to be entered. Reset temporary array space.
C
      CALL PAR_RDVAL('NROWS',1.,FLOAT(MAXROWS),2.,' ',VALUE)
      NROWS=VALUE
      DO I=1,MAXROWS
         ROWS(I)=1
      END DO
C
C     Get list of row numbers.
C
      CALL PAR_RDARY('SECTIONS',1.,FLOAT(NY),'No order',' ',NROWS,
     :                                               MAXROWS,ROWS)
      IF ( PAR_ABORT() ) GO TO 500
      DO I=1,NROWS
         ROWNUMS(I)=ROWS(I)
      END DO
C
C     Get name of spectrum file to be created.
C
      CALL DSA_OUTPUT ('SPECT','SPECTRUM',' ',0,0,STATUS)
C
C     Force creation of output data array and axis data.
C
      CALL DSA_RESHAPE_DATA ('SPECT','IMAGE',1,NX,STATUS)
      CALL DSA_RESHAPE_AXIS ('SPECT',1,'IMAGE',1,1,NX,STATUS)
C
C     Map the input and output data
C
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      CALL DSA_MAP_DATA ('SPECT','WRITE','FLOAT',SPTR,SSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Perform the extraction
C
      CALL FIG_EXT_LIST(%VAL(CNF_PVAL(DPTR)),NX,NY,ROWNUMS,
     :                  %VAL(CNF_PVAL(SPTR)))
C
C     Close down everything
C
500   CALL DSA_CLOSE (STATUS)
C
      END

C+
      SUBROUTINE FIG_EXT_LIST(DVAL,ND1,ND2,ROWNUMS,SPECTRM)
C
C     F I G _ E X T _ L I S T
C
C     Extracts and adds together the cross-sections of the image whose
C     row numbers are contained in the ROWNUMS array.
C
C     Parameters - (">" input , "<" output)
C
C     (>) DVAL    (Real array (ND1,ND2)) Data values.
C     (>) ND1      (Integer)      The number of data values per cross-section.
C     (>) ND2          (Integer)      The number of cross-sections.
C     (>) ROWNUMS (Integer array ) Row numbers to be added.
C     (>) SPECTRM (Real array (ND1)) Resultant spectrum.
C+
      IMPLICIT NONE
C
C     Maximum number of rows to be added
C
      INTEGER MAXROWS
      PARAMETER (MAXROWS=100)
C
C     Parameters
C
      INTEGER ND1 , ND2 , ROWNUMS(MAXROWS)
      REAL    DVAL(ND1,ND2) , SPECTRM(ND1)
C
C     Local variables
C
      INTEGER  I , J
      REAL       SUM
C
C     For each x-value, sum through each cross-section
C
      DO I=1,ND1
         SUM=0.
         J=1
         DO WHILE (ROWNUMS(J).GT.0)
            SUM=SUM+DVAL(I,ROWNUMS(J))
            J=J+1
         END DO
         SPECTRM(I)=SUM
      END DO

      END
