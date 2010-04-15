C+
      SUBROUTINE BCLEAN
C
C     B C L E A N
C
C     This is the non-interactive CCD image cleaning program,
C     which removes bad rows and cosmic rays from images.  Note
C     that it expects the data to be orientated so that bad
C     transfer lines in the chip are horizontal - ie are rows,
C     rather than columns.  The program will detect and blank out
C     the bad data, and optionally fix it up.  Running without the
C     fixup allows the user to see what parts of the image will be
C     affected and provides a chance to modify the cleaning parameters
C     accordingly.  For details of the cleaning algorithms used, see
C     the comments in the listings of FIG_ABROWS, FIG_ZAPRAYS,
C     FIG_VERTICAL, FIG_FIXAREA.  There are four parameters connected
C     with cosmic ray detection, two which affect bad line detection,
C     and one that controls the interpolation used to fix the data.
C
C     WARNING: you are strongly advised to examine the effects of this
C              program on your images.
C
C     Command line parameters -
C
C     IMAGE     (Character) The name of the image to be cleaned.
C     CRSIG     (Numeric) Cosmic Ray SIGma value.  The cosmic ray
C               searcher tests the value of each pixel against the
C               average value of the surrounding pixels.  It must
C               exceed the average value by more than CRSIG*(square
C               root of average value).
C     CRFACT    (Numeric) Cosmic Ray FACTor.  A cosmic ray must also
C               exceed the average value by CRFCT*(the average value).
C     CRMINV    (Numeric) Cosmic Ray MINimum Value.  A cosmic ray
C               must also exceed the average value by CRMINV.
C     CRSHARPNESS (Numeric) Cosmic Ray SHARPNESS. If the SHARPNESS
C               keyword has been specified, then a cosmic ray must also
C               satisfy the sharpness criterion: the height of the cosmic
C               ray above the immediately surrounding sky must exceed the
C               difference between the immediately surrounding sky and the
C               sky a bit further away, by more than a ratio of CRSHARPNESS.
C               Stars tend to have lower values of this ratio than cosmic
C               rays. The default value is 10.
C     BRFACT    (Numeric) The bad row searcher looks through an array
C               formed by collapsing the image along the rows, looking
C               for rows that are lower than their neighbours by a
C               value that is greater than BRFACT*(median difference
C               between neighbours in the neighbourhood).
C     BRPASS    (Numeric) Bad Row PASS value.  The bad row searcher
C               makes BRPASS passes through the data, each time taking
C               a different set of columns evenly distributed through
C               the image.  A bad row must show up in all passes.
C     DEGFIX    (Numeric) The degree of polynomials to be used in
C               interpolating over bad data.
C     OUTPUT    (Character) The name of the output image to be
C               generated.  If this is the same as IMAGE, the
C               correction will be performed in situ.
C     NBROWS    (Numeric) The number of bad rows to be fixed.
C     BROWS     (Numeric vector) The numbers of the bad rows to
C               be fixed.  If NBROWS and BROWS are specified
C               explicitly, then they will be used.  Otherwise
C               an automatic bad line search will be preformed,
C               unless overidden by the setting of the AUTOROW
C               keyword.
C     DIRECTION (Numeric) Indicates along which direction on the CCD
C               the cosmic rays will be interpolated across. 1 means
C               columns, -1 means rows, and 0 means let the computer
C               decide which gives smaller residuals.
C
C     Command keywords -
C
C     AUTOROW   If specified, an automatic bad row search will be
C               performed.  If NBROWS or BROWS are specified explicitly,
C               NOAUTOROW will be assumed.
C     FIX       If specified, the bad data found will be fixed.
C               Otherwise, the output image will simply have the bad
C               pixels flagged by a specific flag value.
C     SHARPNESS If specified, the "sharpness" test for cosmic rays will
C               be performed in addition to the other tests.
C     TEXTFILE  If specified, a text file (BCLEAN.LIS) will be
C               produced giving a summary of the cosmic ray test
C               results. This file is useful when deciding on the
C               cosmic ray selection parameters.
C
C                                         KS / CIT 29th June 1984
C     Modified:
C
C     26th Aug 1987  DJA / AAO. Revised DSA_ routines - some specs changed.
C                    Now uses DYN_ routines for dynamic memory handling
C     15th Dec 1987  KS / AAO. NRAYS initialised to zero properly, fixing
C                    problem if called repeatedly (eg in Callable Figaro).
C                    Now lists position correctly if only one cosmic ray
C                    is found.
C     26th Aug 1988  MCBA / AAO. Added CRSHARPNESS, SHARPNESS, and TEXTFILE.
C     5th  Nov 1990  KS / AAO.  Increased maximum number of cosmic rays.
C     5th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed.
C     13th Jul 1993  SJM/MSSSO Copied the Caltech DIRECTION code to give the
C                    user the option of deciding what the best interpolation
C                    direction is for patching up cosmic rays.
C     15th Apr 1997  JJL / Soton, Starlink. Maps the variance array so
C                    that is is written to the output file.
C     29th Jul 1997  MJCL / Starlink, UCL.  Initialised VEXIST to .FALSE.
C     30th Oct 1998  ACD / Starlink, Edinburgh.  Increased the values
C                    of MAXBROWS and MAXRAYS (necessitated by the
C                    increasing size of CCDs).
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to mapped
C                    data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      LOGICAL PAR_GIVEN
      INTEGER ICH_ENCODE
C
C     Maximum number of cosmic rays and bad rows
C
      INTEGER MAXBROWS, MAXRAYS
      PARAMETER (MAXBROWS=500, MAXRAYS=20000)
C
C     Local variables
C
      LOGICAL      AUTODEF      ! See above
      LOGICAL      AUTOROW      ! See above
      INTEGER      BADROWS(MAXBROWS) !
      INTEGER      BROWS(MAXBROWS)   !
      REAL         BRFACT       !
      REAL         CRFACT       !
      REAL         CRMINV       !
      REAL         CRSHARPNESS  !
      REAL         CRSIG        !
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      LOGICAL      FIX          !
      REAL         FLAG         !
      INTEGER      I            !
      INTEGER      IGNORE       ! Used to pass ignorable status
      CHARACTER    IMAGENAME*255! The name of the input image file
      INTEGER      IX           !
      INTEGER      IY           !
      INTEGER      NBADROW      !
      INTEGER      NBROWS       !
      INTEGER      NCOEFF       !
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NEXT         !
      INTEGER      NPASS        !
      INTEGER      NPTR         !
      INTEGER      NRAYS        !
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      NY           ! Size of 2nd dimension (if present)
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OSLOT        ! Map slot number for output data array
      CHARACTER    POSN*16      !
      INTEGER    RAYS(4,MAXRAYS)!
      REAL       ROWS(MAXBROWS) !
      LOGICAL      SHARPNESS    ! Whether the sharpness test should be done
      INTEGER      STATUS       ! Running status for DSA_ routines
      CHARACTER    STRING*72    ! Output message text
      LOGICAL      TEXTFILE     ! Whether a text file will be produced
      REAL         VALUE        ! Temporary real number
      LOGICAL      VEXIST       ! Does the variance array exist?
      REAL         VMAX         !
      REAL         VMIN         !
      INTEGER      VPTR         ! Dynamic-memory pointer to Variances
      INTEGER      VSLOT        ! Map slot number for output data array
      INTEGER      WPTR         ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT        ! Map slot number of workspace
      INTEGER      DIRECTION    ! direction for CR interpolation
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value of IMAGE and open the file
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get size of image data
C
      CALL DSA_DATA_SIZE('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('Data is not a 2D image',IGNORE)
         GO TO 500
      END IF
      NX=DIMS(1)
      NY=DIMS(2)
C
C     Get the name of the image file. This will be used to annotate the
C     output produced when the TEXTFILE parameter is specified.
C
      CALL DSA_GET_ACTUAL_NAME('IMAGE',IMAGENAME,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get cosmic ray search parameters
C
      CALL PAR_RDVAL('CRSIG',0.,1000.,5.,' ',CRSIG)
      CALL PAR_RDVAL('CRFACT',0.,10000.,.5,' ',CRFACT)
      CALL PAR_RDVAL('CRMINV',0.,1.E30,50.,' ',CRMINV)
C
C     Do we want to include the test for sharpness?
C
      CALL PAR_RDKEY('SHARPNESS',.FALSE.,SHARPNESS)
      IF (SHARPNESS) THEN
         CALL PAR_RDVAL('CRSHARPNESS',0.,1.E30,10.,' ',CRSHARPNESS)
      END IF

      CALL PAR_RDVAL('DIRECTION',-1.,1.,0.,' ',VALUE)
      DIRECTION = NINT(VALUE)
C
C     Do we have to do an automatic bad row search?
C
      AUTODEF=.NOT.(PAR_GIVEN('NBROWS').OR.PAR_GIVEN('BROWS'))
      CALL PAR_RDKEY('AUTOROW',AUTODEF,AUTOROW)
      IF (AUTOROW) THEN
C
C        If so, what row search parameters?
C
         CALL PAR_RDVAL('BRFACT',0.,10000.,1.5,' ',BRFACT)
         CALL PAR_RDVAL('BRPASS',1.,50.,4.,' ',VALUE)
         NPASS=NINT(VALUE)
C
C        An automatic search will need workspace
C
         CALL DSA_GET_WORK_ARRAY(NY*NPASS,'FLOAT',WPTR,WSLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
      ELSE
C
C        If not an autosearch, need to get line numbers
C
         CALL PAR_RDVAL('NBROWS',0.,FLOAT(MAXBROWS),0.,' ',VALUE)
         NBROWS=NINT(VALUE)
         DO I=1,MAXBROWS
            BROWS(I)=1
         END DO
         IF (NBROWS.GT.0) THEN
            CALL PAR_RDARY('BROWS',1.,FLOAT(NY),'None',' ',NBROWS,
     :                                               MAXBROWS,ROWS)
            DO I=1,NBROWS
               BROWS(I)=ROWS(I)
            END DO
         END IF
      END IF
C
C     Are we actually to fix the data?
C
      CALL PAR_RDKEY('FIX',.TRUE.,FIX)
C
C     If we are, what degree polynomials to use?
C
      IF (FIX) THEN
         CALL PAR_RDVAL('DEGFIX',0.,7.,3.,' ',VALUE)
         NCOEFF=NINT(VALUE)+1
      END IF
C
C     Do we want a textfile of the cosmic ray results produced?
C
      CALL PAR_RDKEY('TEXTFILE',.FALSE.,TEXTFILE)
C
C     Get name of OUTPUT file, see if the same as input image.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map the image data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Do a test for a variance array. VEXIST = .TRUE. if there is.
C
      VEXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE('OUTPUT',VEXIST,STATUS)
      IF (VEXIST) THEN
         CALL DSA_MAP_VARIANCE('OUTPUT','UPDATE','FLOAT',VPTR,
     :                          VSLOT,STATUS)
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     Get the range of data in the image, in order to determine a
C     suitable bad pixel flag value.
C
      CALL GEN_RANGEF(%VAL(CNF_PVAL(OPTR)),1,NELM,VMAX,VMIN)
      FLAG=VMIN-1000.0
C
C     If necessary, perform the bad row search.
C
      CALL PAR_WRUSER(' ',IGNORE)
      IF (AUTOROW) THEN
         CALL PAR_WRUSER('Performing automatic bad row search',IGNORE)
         CALL FIG_ABROWS(%VAL(CNF_PVAL(OPTR)),NX,NY,BRFACT,NPASS,
     :                   MAXBROWS,%VAL(CNF_PVAL(WPTR)),NBROWS,BROWS)
         IF (NBROWS.LE.0) THEN
            CALL PAR_WRUSER('No bad rows found',IGNORE)
         ELSE
            STRING='Bad rows are - '
            NEXT=15
            I=1
            DO WHILE(I.LE.NBROWS)
               NPTR=MIN(LEN(STRING),NEXT+1)
               STATUS=ICH_ENCODE(STRING,FLOAT(BROWS(I)),NPTR,0,NEXT)
               IF (STATUS.NE.0) THEN
                  CALL PAR_WRUSER(STRING(:NPTR),IGNORE)
                  STRING=' '
                  NEXT=15
               ELSE
                  I=I+1
               END IF
            END DO
            IF (NEXT.GT.15) CALL PAR_WRUSER(STRING(:NEXT-1),IGNORE)
         END IF
      END IF
C
C     Now zap the bad rows
C
      NBADROW=0
      IF (NBROWS.GT.0) THEN
         CALL FIG_ZAPROW(%VAL(CNF_PVAL(OPTR)),VEXIST,
     :                   %VAL(CNF_PVAL(VPTR)),NX,NY,1,NX,1,NY,NBROWS,
     :                   BROWS,FLAG,MAXBROWS,NBADROW,BADROWS,STATUS)
      END IF
C
C     Then find and zap the cosmic rays
C
      CALL PAR_WRUSER('Searching for cosmic rays',IGNORE)
      NRAYS=0
      CALL FIG_ZAPRAYS(%VAL(CNF_PVAL(OPTR)),VEXIST,%VAL(CNF_PVAL(VPTR)),
     :                 NX,NY,1,NX,1,NY,MAXRAYS,CRSIG,CRFACT,CRMINV,
     :                 SHARPNESS,CRSHARPNESS,TEXTFILE,IMAGENAME,
     :                 FLAG,RAYS,NRAYS,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :      'Too many cosmic rays found for available workspace',IGNORE)
         CALL PAR_WRUSER(
     :    'Parameter values - CRFACT,CRMINV,CRFACT - should be changed',
     :                                                           IGNORE)
         NRAYS=MAXRAYS
      END IF
      IF (NRAYS.EQ.0) THEN
         CALL PAR_WRUSER('No cosmic rays found',IGNORE)
      ELSE
         STATUS=ICH_ENCODE(STRING,FLOAT(NRAYS),1,0,NEXT)
         STRING(NEXT:)=' Cosmic ray(s) found, at - '
         CALL PAR_WRUSER(STRING(:NEXT+24),IGNORE)
         STRING=' '
         NEXT=3
         I=1
         DO WHILE (I.LE.NRAYS)
            IX=(RAYS(1,I)+RAYS(3,I))/2
            IY=(RAYS(2,I)+RAYS(4,I))/2
            POSN=' ('
            STATUS=ICH_ENCODE(POSN,FLOAT(IX),3,0,NPTR)
            POSN(NPTR:NPTR)=','
            STATUS=ICH_ENCODE(POSN,FLOAT(IY),NPTR+1,0,NPTR)
            POSN(NPTR:NPTR)=')'
            IF ((NEXT+11).LE.LEN(STRING)) THEN
               STRING(NEXT:)=POSN(:12)
               NEXT=NEXT+12
               I=I+1
            ELSE
               CALL PAR_WRUSER(STRING(:NEXT-1),IGNORE)
               STRING=' '
               NEXT=3
            END IF
         END DO
         IF (NEXT.GT.14) CALL PAR_WRUSER(STRING(:NEXT-1),IGNORE)
      END IF
C
C     Are we supposed to fix the data?
C
      IF (FIX) THEN
         IF (NBROWS.GT.0) THEN
            CALL FIG_FIXROWS(%VAL(CNF_PVAL(OPTR)),NX,NY,1,NX,1,NY,
     :                       NBROWS,BROWS,NCOEFF,FLAG)
            CALL PAR_WRUSER('Bad rows fixed',STATUS)
         END IF
         DO I=1,NRAYS
            CALL FIG_FIXAREA1(%VAL(CNF_PVAL(OPTR)),NX,NY,RAYS(1,I),
     :                        NCOEFF,FLAG,DIRECTION)
         END DO
         CALL PAR_WRUSER('Cosmic rays fixed',IGNORE)
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
C     Set the bad row numbers
C
      CALL PAR_SDVAL('NBROWS',REAL(NBROWS),STATUS)
      IF (NBROWS.GT.0) THEN
         DO I=1,NBROWS
            ROWS(I)=BROWS(I)
         END DO
         CALL VAR_SETARY('BROWS',NBROWS,ROWS,STATUS)
      END IF
C
C     Close everything down
C
      CALL DSA_CLOSE(STATUS)
C
      END
