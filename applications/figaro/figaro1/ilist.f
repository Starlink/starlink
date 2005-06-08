C+
      SUBROUTINE ILIST
C
C     I L I S T
C
C     Lists the data in a subset of an image.
C
C     Command parameters -
C
C     IMAGE  (Char)    The name of the structure containing the image.
C     YSTART (Numeric) The Y value for the start of the subset.
C     YEND   (Numeric) The Y value for the end of the subset.
C     XSTART (Numeric) The X value for the start of the subset.
C     XEND   (Numeric) The X value for the end of the subset.
C
C     Command keywords -
C
C     HARDCOPY   List data in a file ready for printing.
C
C                                      KS / CIT 16th July 1983
C
C     Modified:
C
C     20 Jul 1987  DJA / AAO. Revised DSA_ routines - some specs
C                  changed. All WRUSER calls converted to PAR_WRUSER's.
C     23 Nov 1987  KS / AAO.  WHOLE was set true in DSA_AXIS_RANGE calls,
C                  so axis limit parameters were being ignored.  Fixed.
C     07 Oct 1992  HME / UoE, Starlink.  Lowercase file name
C                  ilist.lis. INCLUDE changed, TABs removed.
C                  Two remaining WRUSER calls changed.
C                  Must check status after getting data size. If more
C                  than 2-D, it returns 0 dimensions and this routine
C                  divides by NX.
C     16 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  No concurrent mapping. Had to swap axis range before
C                  mapping axis data, for Y and X.
C     18 Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                  file names to 132 chars.
C     26 Jul 1996  MJCL / Starlink, UCL.  Added PAR_ABORT check.
C     2005 June 8  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_LEN
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
C
C     Local variables
C
      INTEGER   DIMS(10)         ! The sizes of the dimensions of arrays
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number used for data
      REAL      DUMMY            ! REAL dummy arguement
      INTEGER   FILE             ! Logical unit number for the output
      CHARACTER FILENAME*132     ! The name of the output file
      LOGICAL   HARD             ! True if the output device is hard
      INTEGER   IGNORE           ! Used to pass ignorable status
      INTEGER   ITEMP            ! Temporary integer storage
      INTEGER   IXEN             ! Last element to be plotted in x-axis
      INTEGER   IXST             ! First element to be plotted in x-axis
      INTEGER   IYEN             ! Last element to be plotted in y-axis
      INTEGER   IYST             ! First element to be plotted in y-axis
      CHARACTER LABELS(3)*64     ! Labels for axes and data
      INTEGER   NDD              ! Dimensionality of input data
                                 ! structure
      INTEGER   NDELM            ! Total number of elements in the data
      INTEGER   NDX              ! Dimensionality of x-axis array
      INTEGER   NX               ! The size of the data's 1st dimension
      INTEGER   NXELM            ! Total number of elements in x-axis
      INTEGER   NY               ! The size of the data's 2nd dimension
      CHARACTER OBJECT*80        ! The name of the astronomical object
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      CHARACTER STRINGS(2)*64    ! Receives data and axis information
      CHARACTER UNITS(3)*32      ! Units on each axis, and of data
      REAL      VALUE            ! Temporary REAL
      LOGICAL   XEXIST           ! TRUE if an x-axis structure exists
      REAL      XMAX             !
      REAL      XMIN             !
      INTEGER   XSLOT            ! Map slot number used for x-axis info
      INTEGER   XPTR             ! Dynamic-memory pointer to x-axis data
      LOGICAL   YEXIST           ! TRUE if a y-axis structure exists
      REAL      YMAX             !
      REAL      YMIN             !
      INTEGER   YPTR             ! Dynamic-memory pointer to y-axis data
      INTEGER   YSLOT            ! Map slot number used for y-axis info
C
C     Index values for UNITS, LABELS and DIMS arrays
C
      INTEGER X,Y,D
      PARAMETER (X=1,Y=2,D=3)
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDD,DIMS,NDELM,STATUS)
      IF (STATUS.NE.0) GOTO 500
      NX=DIMS(1)
      NY=NDELM/NX
C
C     Map input data
C
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Try to get units and label for data array
C
      CALL DSA_GET_DATA_INFO ('IMAGE',2,STRINGS,0,DUMMY,STATUS)
      UNITS(D)=STRINGS(1)
      LABELS(D)=STRINGS(2)
C
C     See if we have a data component giving y-axis values
C
      CALL DSA_SEEK_AXIS ('IMAGE',2,YEXIST,STATUS)
      IF (NDD.GT.1) THEN
C
C        Get limits in Y and Y data.
C
         CALL DSA_AXIS_RANGE ('IMAGE',2,' ',.FALSE.,YMIN,YMAX,IYST,
     :                        IYEN,STATUS)
         CALL DSA_MAP_AXIS_DATA ('IMAGE',2,'READ','FLOAT',YPTR,
     :                           YSLOT,STATUS)
C
C        Now try for label and units
C
         CALL DSA_GET_AXIS_INFO ('IMAGE',2,2,STRINGS,0,DUMMY,STATUS)
         UNITS(Y)=STRINGS(1)
         LABELS(Y)=STRINGS(2)
      ELSE
         IYST=1
         IYEN=1
         UNITS(Y)=' '
         LABELS(Y)=' '
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     And ditto for X..
C
      CALL DSA_SEEK_AXIS ('IMAGE',1,XEXIST,STATUS)
      CALL DSA_AXIS_SIZE ('IMAGE',1,1,NDX,DIMS,NXELM,STATUS)
      CALL DSA_AXIS_RANGE ('IMAGE',1,' ',.FALSE.,XMIN,XMAX,IXST,
     :                     IXEN,STATUS)
      CALL DSA_MAP_AXIS_DATA ('IMAGE',1,'READ','FLOAT',XPTR,
     :                        XSLOT,STATUS)
      CALL DSA_GET_AXIS_INFO ('IMAGE',1,2,STRINGS,0,DUMMY,STATUS)
      UNITS(X)=STRINGS(1)
      LABELS(X)=STRINGS(2)
      IF (STATUS.NE.0) GOTO 500
C
C     Hardcopy to a file?
C
      CALL PAR_RDKEY('HARDCOPY',.FALSE.,HARD)
      IF ( PAR_ABORT() ) GO TO 600
      IF (HARD) THEN
         FILE=1
         OPEN (UNIT=FILE,FILE='ilist.lis',STATUS='NEW',
     :                                           IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Unable to open output file',IGNORE)
            GO TO 500
         END IF
      ELSE
         FILE=0
      END IF
C
C     Try to get object name
C
      CALL DSA_OBJECT_NAME('IMAGE',OBJECT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     List the data
C
      CALL FIG_LIST(OBJECT,%VAL(CNF_PVAL(DPTR)),NX,NY,
     :              %VAL(CNF_PVAL(XPTR)),IXST,IXEN,XEXIST,
     :              %VAL(CNF_PVAL(YPTR)),IYST,IYEN,
     :              YEXIST,UNITS,LABELS,FILE)
C
C     Close down everything
C
  500 CONTINUE
      IF (HARD) THEN
         INQUIRE(UNIT=FILE,NAME=FILENAME)
         CALL PAR_WRUSER('List written to '//
     :                         FILENAME(:ICH_LEN(FILENAME)),STATUS)
         CLOSE(UNIT=FILE,IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error closing down list file',STATUS)
         END IF
      END IF
C
  600 CONTINUE
      CALL DSA_CLOSE (STATUS)
C
      END
C+
      SUBROUTINE FIG_LIST(OBJECT,DATA,NX,NY,XDATA,IXST,IXEN,XISOK,
     :                    YDATA,IYST,IYEN,YISOK,UNITS,LABELS,FILE)
C
C     F I G _ L I S T
C
C     Lists the value of a subset of the elements of a passed data
C     array.
C
C     Parameters -  ( ">" input, "<" output)
C
C     (>) OBJECT    (Character) The name of the object being listed.
C     (>) DATA      (Real array DATA(NX,NY)) The data array to be
C                   listed.
C     (>) NX        (Integer) The first dimension of DATA.
C     (>) NY        (Integer) The second dimension of DATA.
C     (>) XDATA     (Real array XDATA(NX)) The x-values for each point
C                   in DATA.
C     (>) IXST      (Integer) The first X value of the subset to be
C                   listed - the element number, not a value in XDATA.
C     (>) IXEN      (Integer) The last X value of the subset to be
C                   listed.
C     (>) XISOK     (Logical) True if the data in XDATA is to be used -
C                   if not, the element numbers will be used instead.
C     (>) YDATA     (Real array YDATA(NY)) The y-values for each point
C                   in DATA.
C     (>) IYST      (Integer) The first Y value of the subset to be
C                   listed - the element number, not a value in YDATA.
C     (>) IYEN      (Integer) The last Y value of the subset to be
C                   listed.
C     (>) YISOK     (Logical) True if the data in YDATA is to be used -
C                   if not, the element numbers will be used instead.
C     (>) UNITS     (Character array UNITS(3)) The units used for the
C                   data in XDATA, YDATA and DATA.
C     (>) LABELS    (Character array LABELS(3)) The labels used for the
C                   data in XDATA, YDATA and DATA.
C     (>) FILE      (Integer) The logical unit number for the output
C                   file - which should be already open, and is not
C                   closed by this routine.  If this is zero, the
C                   terminal is used instead.
C
C                                           KS / CIT 16th July 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL XISOK,YISOK
      INTEGER NX,NY,IXST,IXEN,IYST,IYEN,FILE
      REAL DATA(NX,NY),XDATA(NX),YDATA(NY)
      CHARACTER*(*) OBJECT,LABELS(3),UNITS(3)
C
C     Functions
C
      LOGICAL GEN_CHKNSF
      INTEGER ICH_CLEAN,ICH_LEN,ICH_ENCODE
C
C     Local variables
C
      LOGICAL FIRST,XOK,YOK
      INTEGER ICH,IX,IXL,IY,K,LINES,NCH,NEXT,NPERL,NXLABEL
      INTEGER NXUNIT,STATUS
      REAL VALUE
      CHARACTER*10 CLABEL,CUNDER,CUNITS
      CHARACTER*14 DLABEL,DUNDER,DUNITS,XLABEL,XUNDER,XUNITS
      CHARACTER*132 STRING
C
      DATA CUNDER,DUNDER,XUNDER/' ---------',' -------------',
     :                                       ' -------------'/
C
C     Right justify the labels and units into the strings
C     for X and for Data, for use in line headings.
C
      XOK=XISOK
      YOK=YISOK
      XLABEL=' '
      NCH=ICH_CLEAN(LABELS(1))
      IF (NCH.GT.0) THEN
         NCH=MIN(NCH,LEN(XLABEL))
         XLABEL(LEN(XLABEL)-NCH:)=LABELS(1)
      END IF
      NXLABEL=NCH
      DLABEL=' '
      NCH=ICH_CLEAN(LABELS(3))
      IF (NCH.GT.0) THEN
         NCH=MIN(NCH,LEN(DLABEL))
         DLABEL(LEN(DLABEL)-NCH+1:)=LABELS(3)
      END IF
      XUNITS=' '
      NCH=ICH_CLEAN(UNITS(1))
      IF (NCH.GT.0) THEN
         NCH=MIN(NCH,LEN(XUNITS))
         XUNITS(LEN(XUNITS)-NCH+1:)=UNITS(1)
      END IF
      NXUNIT=NCH
      DUNITS=' '
      NCH=ICH_CLEAN(UNITS(3))
      IF (NCH.GT.0) THEN
         NCH=MIN(NCH,LEN(DUNITS))
         DUNITS(LEN(DUNITS)-NCH+1:)=UNITS(3)
      END IF
C
C     Check on the possibility that the X data is just the
C     numbers from 1 to N.  In this case, don't use XDATA.
C
      CLABEL='         #'
      CUNITS=' '
      IF (XOK) THEN
         IF (GEN_CHKNSF(XDATA,NX)) THEN
            XOK=.FALSE.
            IF (NXUNIT.NE.0)
     :              CUNITS=XUNITS(LEN(XUNITS)-LEN(CUNITS)+1:)
            IF (NXLABEL.NE.0)
     :              CLABEL=XLABEL(LEN(XLABEL)-LEN(CLABEL)+1:)
         END IF
      END IF
C
C     Set sizes for lines - If XOK then there are 3 columns
C     for each element and 3 elements can be fitted on a file
C     line and 2 on a terminal line.  Otherwise there are 2
C     columns for each element and 5 elements fit on a file
C     line and 3 on a terminal line.
C
      IF (XOK) THEN
         IF (FILE.GT.0) THEN
            NPERL=3
         ELSE
            NPERL=2
         END IF
      ELSE
         IF (FILE.GT.0) THEN
            NPERL=5
         ELSE
            NPERL=3
         END IF
      END IF
C
C     Set line count and output initial heading.
C
      LINES=0
      IF (FILE.GT.0) THEN
         WRITE (FILE,'(//,10X,A,//)',IOSTAT=STATUS) OBJECT
         LINES=LINES+5
      ELSE
         NCH=ICH_CLEAN(OBJECT)
         CALL PAR_WRUSER(' ',STATUS)
         IF (NCH.GT.0) THEN
            CALL PAR_WRUSER(OBJECT(:NCH),STATUS)
            CALL PAR_WRUSER(' ',STATUS)
         END IF
      END IF
C
C     Start loop through Y values
C
      DO IY=IYST,IYEN
C
C        For real 2D data, list the Y information
C        for this cross-section
C
         IF (NY.GT.1) THEN
            NCH=ICH_CLEAN(LABELS(2))
            IF (NCH.EQ.0) THEN
               STRING='Y value '
               NCH=8
            ELSE
               STRING=LABELS(2)
            END IF
            IF (YOK) THEN
               VALUE=YDATA(IY)
            ELSE
               VALUE=IY
            END IF
            STATUS=ICH_ENCODE(STRING,VALUE,NCH+2,3,NEXT)
            STRING(NEXT+2:)=UNITS(2)
            NCH=ICH_CLEAN(UNITS)+NEXT+2
            IF (FILE.GT.0) THEN
               WRITE (FILE,'(//,10X,A,//)',IOSTAT=STATUS)
     :                                         STRING(:NCH)
               LINES=LINES+5
               IF (LINES.GT.60) LINES=0
            ELSE
               CALL PAR_WRUSER(' ',STATUS)
               CALL PAR_WRUSER(STRING(:NCH),STATUS)
               CALL PAR_WRUSER(' ',STATUS)
               LINES=LINES+3
               IF (LINES.GT.60) LINES=0
            END IF
         END IF
C
C        Loop through data row.
C
         FIRST=.TRUE.
         DO IX=IXST,IXEN,NPERL
C
C        Headers for columns (at start of row, or new page).
C
            IF ((LINES.EQ.0).OR.(FIRST))THEN
               IF (LINES.EQ.0) THEN
                  IF (FILE.GT.0) THEN
                     WRITE (FILE,'(1H1)',IOSTAT=STATUS)
                  ELSE
                     CALL PAR_WRUSER(' ',STATUS)
                     CALL PAR_WRUSER(' ',STATUS)
                  END IF
               END IF
               FIRST=.FALSE.
               IF (XOK) THEN
                  STRING=CLABEL//XLABEL//DLABEL
                  STRING(39:76)=STRING(:38)
                  IF (FILE.GT.0) THEN
                     STRING(77:114)=STRING(:38)
                     WRITE (FILE,'(A)',IOSTAT=STATUS) STRING(:114)
                  ELSE
                     CALL PAR_WRUSER(STRING(:76),STATUS)
                  END IF
                  STRING=CUNITS//XUNITS//DUNITS
                  STRING(39:76)=STRING(:38)
                  IF (FILE.GT.0) THEN
                     STRING(77:114)=STRING(:38)
                     WRITE (FILE,'(A)',IOSTAT=STATUS) STRING(:114)
                  ELSE
                     CALL PAR_WRUSER(STRING(:76),STATUS)
                  END IF
                  DO ICH=1,77,38
                     STRING(ICH:ICH+38)=CUNDER//XUNDER//DUNDER
                  END DO
                  IF (FILE.GT.0) THEN
                     WRITE(FILE,'(A)',IOSTAT=STATUS) STRING(:120)
                  ELSE
                     CALL PAR_WRUSER(STRING(:76),STATUS)
                  END IF
               ELSE
                  DO ICH=1,97,24
                     STRING(ICH:ICH+23)=CLABEL//DLABEL
                  END DO
                  IF (FILE.GT.0) THEN
                     WRITE(FILE,'(A)',IOSTAT=STATUS) STRING(:120)
                  ELSE
                     CALL PAR_WRUSER(STRING(:72),STATUS)
                  END IF
                  DO ICH=1,97,24
                     STRING(ICH:ICH+23)=CUNITS//DUNITS
                  END DO
                  IF (FILE.GT.0) THEN
                     WRITE(FILE,'(A)',IOSTAT=STATUS) STRING(:120)
                  ELSE
                     CALL PAR_WRUSER(STRING(:72),STATUS)
                  END IF
                  DO ICH=1,97,24
                     STRING(ICH:ICH+23)=CUNDER//DUNDER
                  END DO
                  IF (FILE.GT.0) THEN
                     WRITE(FILE,'(A)',IOSTAT=STATUS) STRING(:120)
                  ELSE
                     CALL PAR_WRUSER(STRING(:72),STATUS)
                  END IF
               END IF
               LINES=LINES+2
               IF (LINES.GT.60) LINES=0
            END IF
C
C           Output the actual data
C
            IXL=MIN(IX+NPERL-1,IXEN)
            IF (XOK) THEN
               WRITE (STRING,'(3(I10,2(1PG14.7)))',IOSTAT=STATUS)
     :                        (K,XDATA(K),DATA(K,IY),K=IX,IXL)
            ELSE
               WRITE (STRING,'(5(I10,1PG14.7))',IOSTAT=STATUS)
     :                                 (K,DATA(K,IY),K=IX,IXL)
            END IF
            IF (FILE.GT.0) THEN
               WRITE (FILE,'(A)',IOSTAT=STATUS) STRING(:120)
            ELSE
               CALL PAR_WRUSER(STRING(:76),STATUS)
            END IF
            LINES=LINES+1
            IF (LINES.GT.60) LINES=0
         END DO
      END DO
C
      END
