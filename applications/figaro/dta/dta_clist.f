C+
      SUBROUTINE DTA_CLIST
C
C     This is a debug routine.  It lists the contents of the data
C     structure name cache.  It does not include the hit statistics output by
C     DTA_CACHES.
C
C     Parameters - None
C
C     Common variables used -
C
C     Common block variables used -
C
C     (>) CACHEB   The bottom level cache name array
C     (>) CACHE3   The third level cache name array
C     (>) CACHE2   The second level cache name table
C     (!) CACHET   The top level cache name table
C     (!) CNEXTB   Bottom level next slot pointer
C     (!) CNEXT3   Third level next slot pointer
C     (!) CNEXT2   Second level next slot pointer
C     (>) CACHEI   Cache initialisation flag
C
C     All in common blocks CACHEC and CACHEN
C
C     Subroutines / functions used -
C
C     HDS_TRACE    (HDS_ routine) Provide locator details
C     EMS_BEGIN    (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL    ( "    "     ) Clear current EMS error status.
C     EMS_END      ( "    "     ) End current reporting environment.
C
C                                       KS / CIT 1982
C     Modified:
C
C     31st May 1988.  KS/AAO.  Tidied up and commented a little.
C     10th Jan 1992.  KS / AAO.  Syntax of include statements changed to
C                     remove VMS logical names and to use lower case, to
C                     enable compilation on a SUN.
C     20th Jan 1992.  KS / AAO. HDS_TRACE calls added.
C     24th Jan 1992.  KS / AAO. Calls to EMS added to control error reporting.
C     12th Apr 1993.  HME / UoE, Starlink. For DTACACHE must include SAE_PAR
C                     and DAT_PAR as well.
C+
      IMPLICIT NONE
C
      INTEGER EMSTAT,I,IEND,NLEV,STATUS
      CHARACTER FILE*64, PATH*64
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTACACHE'
C
      PRINT *,' '
      PRINT *,'DTA_ system name cache contents -'
      PRINT *,' '
      IF (.NOT.CACHEI) THEN
         PRINT *,'Cache not initialised'
      ELSE
         PRINT *,'There are ',NCACHT,' top level entries'
         PRINT *,'Those in use are:'
         DO I=1,NCACHT
            IF (CACHET(I)(1:1).NE.'.') THEN
               PRINT '(I5,2X,A)',I,CACHET(I)
               STATUS=0
               EMSTAT=0
               CALL EMS_BEGIN(EMSTAT)
               CALL HDS_TRACE(LOCNT(I),NLEV,PATH,FILE,STATUS)
               IF (STATUS.NE.0) THEN
                  CALL EMS_ANNUL(EMSTAT)
                  PRINT *,'HDS error tracing locator'
               ELSE
                  IEND=INDEX(PATH,' ')
                  IF (IEND.EQ.0) IEND=LEN(PATH)
                  PRINT *,'        HDS Path: ',PATH(:IEND)
                  IEND=INDEX(FILE,' ')
                  IF (IEND.EQ.0) IEND=LEN(FILE)
                  PRINT *,'        File: ',FILE(:IEND)
               END IF
               CALL EMS_END(EMSTAT)
            END IF
         END DO
         PRINT *,'There are ',NCACH2,' 2nd level entries'
         PRINT *,'Those in use are:'
         DO I=1,NCACH2
            IF (CACHE2(I)(1:1).NE.'.') THEN
               PRINT '(I5,2X,A)',I,CACHE2(I)
               STATUS=0
               EMSTAT=0
               CALL EMS_BEGIN(EMSTAT)
               CALL HDS_TRACE(LOCN2(I),NLEV,PATH,FILE,STATUS)
               IF (STATUS.NE.0) THEN
                  CALL EMS_ANNUL(EMSTAT)
                  PRINT *,'HDS error tracing locator'
               ELSE
                  IEND=INDEX(PATH,' ')
                  IF (IEND.EQ.0) IEND=LEN(PATH)
                  PRINT *,'        HDS Path: ',PATH(:IEND)
                  IEND=INDEX(FILE,' ')
                  IF (IEND.EQ.0) IEND=LEN(FILE)
                  PRINT *,'        File: ',FILE(:IEND)
               END IF
               CALL EMS_END(EMSTAT)
            END IF
         END DO
         PRINT *,'There are ',NCACH3,' 3rd level entries'
         PRINT *,'Those in use are:'
         DO I=1,NCACH3
            IF (CACHE3(I)(1:1).NE.'.') THEN
               PRINT '(I5,2X,A)',I,CACHE3(I)
               STATUS=0
               EMSTAT=0
               CALL EMS_BEGIN(EMSTAT)
               CALL HDS_TRACE(LOCN3(I),NLEV,PATH,FILE,STATUS)
               IF (STATUS.NE.0) THEN
                  CALL EMS_ANNUL(EMSTAT)
                  PRINT *,'HDS error tracing locator'
               ELSE
                  IEND=INDEX(PATH,' ')
                  IF (IEND.EQ.0) IEND=LEN(PATH)
                  PRINT *,'        HDS Path: ',PATH(:IEND)
                  IEND=INDEX(FILE,' ')
                  IF (IEND.EQ.0) IEND=LEN(FILE)
                  PRINT *,'        File: ',FILE(:IEND)
               END IF
               CALL EMS_END(EMSTAT)
            END IF
         END DO
         PRINT *,'There are ',NCACHB,' Bottom level entries'
         PRINT *,'Those in use are:'
         DO I=1,NCACHB
            IEND=INDEX(CACHEB(I),' ')
            IF (IEND.EQ.0) IEND=LEN(CACHEB(I))
            IF (CACHEB(I)(1:1).NE.'.') THEN
               PRINT '(I5,2X,A)',I,CACHEB(I)(:IEND)
               STATUS=0
               EMSTAT=0
               CALL EMS_BEGIN(EMSTAT)
               CALL HDS_TRACE(LOCNB(I),NLEV,PATH,FILE,STATUS)
               IF (STATUS.NE.0) THEN
                  CALL EMS_ANNUL(EMSTAT)
                  PRINT *,'HDS error tracing locator'
               ELSE
                  IEND=INDEX(PATH,' ')
                  IF (IEND.EQ.0) IEND=LEN(PATH)
                  PRINT *,'        HDS Path: ',PATH(:IEND)
                  IEND=INDEX(FILE,' ')
                  IF (IEND.EQ.0) IEND=LEN(FILE)
                  PRINT *,'        File: ',FILE(:IEND)
               END IF
               CALL EMS_END(EMSTAT)
            END IF
         END DO
         PRINT *,'The slot pointers are:'
         PRINT '(3(A,I3,2X))','CNEXT2: ',CNEXT2,'CNEXT3: ',CNEXT3,
     :                                              'CNEXTB: ',CNEXTB
      END IF
      END

