C+
      SUBROUTINE POLEXT
C
C     P O L E X T
C
C     Given an image containing Fibre data, and a polynomial file
C     (produced, probably, by FINDSP) that gives the positions of the
C     centres of the spectra, POLEXT extracts the spectra to produce
C     a new image in which each cross-section is one of the fibre
C     spectra.
C
C     Command parameters -
C
C     IMAGE     (Character) The name of the image containing the
C               distorted fibre spectra.
C     PFILE     (Character) The name of the polynomial file that
C               describes the fibre positions.  Default extension
C               is .POL
C     DFILE     (Character) The name of an optional dud fibre file
C               that lists the numbers of any dud fibres.  If blank,
C               no file is used.  Default extension is .DUD.  If the
C               file cannot be opened, this is not regarded as a
C               fatal error, so it is possible to specify a non-existent
C               file, such as 'NONE'
C     EXTWIDTH  (Real) The width of the spectra in pixels.
C     OUTPUT    (Character) The name of the resulting image.
C
C     Command keywords - None
C
C     History -
C
C     Original version by John Lucey (JRL), AAO
C     Adapted for Figaro by K. Shortridge, AAO, 5th March 1987
C
C     Modified:
C
C     16th Jan 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic-memory handling changed to use
C                    DYN_ routines.
C     24th Jan 1991  JMS / AAO. Added PAR_ABORT to support user
C                    requested abort. In routine POLEXT_WORK, included
C                    two further checks on the limits of the variables
C                    FIBTOP and FIBLOW.
C     8th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed. Lowercase
C                    extension .dud. Call PAR_WRUSER rather than
C                    DSA_WRUSER. Lowercase extension .pol. Use
C                    DSA_OPEN_TEXT_FILE for .dud file.
C     15th Sep 1994  HME / UoE, Starlink.  Map output data for write
C                    access rather than update.
C     16th Feb 1995  HME / UoE, Starlink.  In the big workspace move
C                    the DOUBLE workspace to the front. Otherwise the
C                    odd number of FLOAT workspaces combined with an
C                    odd number of channels in the input spectrum
C                    cause the DOUBLE workspace to be misaligned
C                    (memory address and odd multiple of 4).
C     28th Apr 1995  HME / UoE, Starlink.  There are now two possible
C                    formats for .pol files. Version 1 contains the
C                    coefficients of a Chebyshev series, while version
C                    2 contains those of an ordinary polynomial. This
C                    application has to identify the version and to
C                    convert coefficients from Chebyshev to ordinary if
C                    necessary. The driving force behind this change is
C                    to no longer use NAG. In this application this is
C                    now trivial, since an ordinary polynomial is to be
C                    evaluated. An ordinary polynomial can also be
C                    extrapolated, so that the left and right edges of
C                    the fibre frame need no special extrapolation any
C                    more. As a result the extracted spectra may differ
C                    slightly at the end points from earlier versions
C                    of this application.
C     2nd  Apr 1996  BKM / RAL, Starlink. Correct bug due to
C                    uninitialised variable NDUD.
C     18th Jul 1996  MJCL / Starlink, UCL.  Set variables for storage
C                    of file names to 132 chars.
C     2005 June 15   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_ENCODE
      INTEGER ICH_TIDY
C
C     Maximum number of fibres
C
      INTEGER MAXFIB
      PARAMETER (MAXFIB=128)
C
C     Local variables
C
      INTEGER    DIMS(5)         ! Image AXIS(1) dimensions
      INTEGER    DLU             ! Logical unit number for dud fibres
                                 ! file
      LOGICAL    EOF             ! True when end of file reached
      REAL       EX              ! Extraction width
      LOGICAL    EXIST           ! IMAGE AXIS(1) structure exists?
      LOGICAL    FAULT           ! Fault discovered by non DSA methods?
      LOGICAL    FIBOK(MAXFIB)   ! Flags if the fibres are OK
      CHARACTER  FNAME*132       ! Filenames of text files
      INTEGER    FSTAT           ! Status for file I/O
      CHARACTER  FULNAM*132      ! Full name of text files
      INTEGER    I               ! Loop variable
      INTEGER    ID1(10)         ! Dimensions of IMAGE
      INTEGER    ID2(2)          ! Dimensions of OUTPUT
      INTEGER    INVOKE          ! Used to format user messages
      INTEGER    IPDP            ! Dynamic-memory pointer for poly.
                                 ! coeff.
      INTEGER    IPIN            ! Dynamic-memory pointer for IMAGE data
      INTEGER    IPOUT           ! Dynamic-memory pointer for OUTPUT
                                 ! data
      INTEGER    IPX             ! Dynamic-memory pointer for DP X(NPTS)
      INTEGER    IPYL            ! Dynamic-memory pointer for
                                 ! YLEVEL(NPTS)
      INTEGER    IPYR            ! Dynamic-memory pointer for YRUN(NX)
      INTEGER    LENGTH          ! Used to format user messages
      INTEGER    NDEAD           ! Index for dud fibres
      INTEGER    NDIM            ! Number of dimensions
      INTEGER    NDUD            ! Number of dud fibres
      INTEGER    NELM            ! Number of elements in structure -
                                 ! ignored
      INTEGER    NEXT            ! Used to format user messages
      INTEGER    NORDER          ! Order of polynomial used
      INTEGER    NPLUS1          ! No. of coeff. in poly fit (NORDER+1)
      INTEGER    NPTS            ! No. of points used along spectra
      INTEGER    NUMFIB          ! Number of fibres used
      INTEGER    NX              ! First dimension of image
      INTEGER    NY              ! Second dimension of image
      INTEGER    PLU             ! Logical unit number for polynomial
                                 ! file
      INTEGER    SLOT            ! Slot number for mapped data - ignored
      INTEGER    STATUS          ! Running status for DSA routines
      CHARACTER  TEXT*80         ! Used to format informational messages
      CHARACTER  TEXTL*132       ! First line from .pol file
      REAL       VERSION         ! .pol file format version

C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
      FAULT=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Get name of input image and open file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,ID1,NELM,STATUS)
C
C     Map input data
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IPIN,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Get name of polynomial file, and open it.
C
      CALL PAR_RDCHAR('PFILE',' ',FNAME)
      IF (PAR_ABORT()) GOTO 500        ! User requested abort
      CALL DSA_OPEN_TEXT_FILE(FNAME,'.pol','OLD',.FALSE.,PLU,FULNAM,
     :                        STATUS)
      IF(STATUS.NE.0)GOTO 500

      READ (PLU,'(A)') TEXTL
      READ (TEXTL,*,IOSTAT=FSTAT) NX,NY,NUMFIB,NPTS,NORDER,VERSION
      IF (FSTAT.NE.0) THEN
         VERSION=1.
         READ (TEXTL,*,IOSTAT=FSTAT) NX,NY,NUMFIB,NPTS,NORDER
         IF (FSTAT.NE.0) THEN
            CALL GEN_FORTERR(FSTAT,.FALSE.,TEXT)
            CALL PAR_WRUSER(
     :         'Error reading header from polynomial file',STATUS)
            CALL PAR_WRUSER(TEXT,STATUS)
            FAULT=.TRUE.
            GO TO 500
         END IF
      END IF
C
C     Check dimensions match input file
C
      IF(ID1(1).NE.NX.OR.ID1(2).NE.NY)THEN
         TEXT='Polynomial file dimensions ('
         INVOKE=ICH_ENCODE(TEXT,FLOAT(NX),29,0,NEXT)
         TEXT(NEXT:)=' by '
         INVOKE=ICH_ENCODE(TEXT,FLOAT(NY),NEXT+4,0,NEXT)
         TEXT(NEXT:)=') do not match'
         CALL PAR_WRUSER(TEXT//' dimensions of input image data',STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Check maximum number of fibres
C
      IF (NUMFIB.GT.MAXFIB) THEN
         TEXT='File specifies too many fibres ('
         INVOKE=ICH_ENCODE(TEXT,FLOAT(NUMFIB),33,0,NEXT)
         TEXT(NEXT:)=') for this program.'
         CALL PAR_WRUSER(TEXT//' Will use as many as possible.',STATUS)
         NUMFIB=MAXFIB
      END IF
C
C     Tell the user what's going on
C
      WRITE(TEXT,'(''Coefficient file is format version '',F3.1)')
     :   VERSION
      CALL PAR_WRUSER(TEXT,STATUS)
      WRITE(TEXT,'(''Image size is '',I4,'' by '',I3)')NX,NY
      CALL PAR_WRUSER(TEXT,STATUS)
      WRITE(TEXT,'(''No. of fibres is '',I3)')NUMFIB
      CALL PAR_WRUSER(TEXT,STATUS)
      WRITE(TEXT,'(''No. of points along spectra was '',I3)')NPTS
      CALL PAR_WRUSER(TEXT,STATUS)
      WRITE(TEXT,'(''Order of poly. used was '',I2)')NORDER
      CALL PAR_WRUSER(TEXT,STATUS)
C
C     Set dud fibre table
C
      DO I=1,MAXFIB
          FIBOK(I)=.TRUE.
      END DO
C
C     Assume no dud fibres initially
C
      NDUD=0
C
C     See if there is a dud fibre file to be used.
C
      CALL PAR_RDCHAR('DFILE',' ',FNAME)
      IF (FNAME.NE.' ') THEN
C
C        Open dud fibre file
C
         CALL DSA_OPEN_TEXT_FILE(FNAME,'.dud','OLD',.FALSE.,DLU,FULNAM,
     :                        STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Unable to open dud fibre file',STATUS)
            CALL PAR_WRUSER('Will assume all fibres are good.',
     :         STATUS)
         ELSE
C
C           Read numbers of dud fibres from file (treat any I/O error
C           as an end of file).  Treat any records starting '*' as
C           comments.
C
            EOF=.FALSE.
            DO WHILE (.NOT.EOF)
               READ (DLU,'(A)',IOSTAT=FSTAT) TEXT
               EOF=FSTAT.NE.0
               IF (.NOT.EOF) THEN
                  IF ((TEXT(1:1).NE.'*').AND.(TEXT.NE.' ')) THEN
                     READ (TEXT,*,IOSTAT=FSTAT) NDEAD
                     IF (FSTAT.NE.0) THEN
                        CALL PAR_WRUSER(
     :                     'Cannot understand dud fibre record.',STATUS)
                        LENGTH=ICH_TIDY(TEXT)
                        CALL PAR_WRUSER(TEXT(:LENGTH),STATUS)
                     ELSE
                        IF ((NDEAD.GT.0).AND.(NDEAD.LE.MAXFIB)) THEN
                           IF ((NDEAD.LE.NUMFIB).AND.FIBOK(NDEAD)) THEN
                              NDUD=NDUD+1
                           END IF
                           FIBOK(NDEAD)=.FALSE.
                        ELSE
                           TEXT='Dud fibre record specifies'//
     :                                               ' invalid fibre '
                           INVOKE=ICH_ENCODE(TEXT,FLOAT(NDEAD),
     :                                                     42,0,NEXT)
                           CALL PAR_WRUSER(TEXT(:NEXT),STATUS)
                        END IF
                     END IF
                  END IF
               END IF
            END DO
            CALL POLEXT_DUDLIST(FIBOK,MAXFIB,NUMFIB)
         END IF
      END IF
C
C     Sanity check
C
      IF (NUMFIB.LE.NDUD) THEN
         CALL PAR_WRUSER(
     :        'All fibres are dud.  Cannot create output data.',STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Get extraction width
C
      CALL PAR_RDVAL('EXTWIDTH',1.0,FLOAT(NY),6.0,' ',EX)
C
C     Create the iutput file based on IMAGE but without data
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
C
C     Create the data sub-structure of the output image
C
      ID2(1)=ID1(1)
      ID2(2)=NUMFIB-NDUD
      CALL DSA_RESHAPE_DATA('OUTPUT','IMAGE',2,ID2,STATUS)
C
C     Copy any AXIS(1) structure from IMAGE to OUTPUT
C
      CALL DSA_SEEK_AXIS('IMAGE',1,EXIST,STATUS)
      IF(EXIST)THEN
         CALL DSA_AXIS_SIZE('IMAGE',1,5,NDIM,DIMS,NELM,STATUS)
         CALL DSA_RESHAPE_AXIS('OUTPUT',1,'IMAGE',1,NDIM,DIMS,STATUS)
      END IF
C
C     Map the output data
C
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',IPOUT,SLOT,STATUS)
C
C     Get workspace needed.
C
      NPLUS1=NORDER+1
      CALL DSA_GET_WORKSPACE(NPTS,'DOUBLE',IPX,SLOT,STATUS)
      CALL DSA_GET_WORKSPACE(2*NPLUS1,'DOUBLE',IPDP,SLOT,STATUS)
      CALL DSA_GET_WORKSPACE(ID1(1),'FLOAT',IPYR,SLOT,STATUS)
      CALL DSA_GET_WORKSPACE(NPTS,'FLOAT',IPYL,SLOT,STATUS)
      IF(STATUS.NE.0) GOTO 500
C
C     Do the actual extraction
C
      CALL POLEXT_WORK(ID1(1),ID1(2),%VAL(CNF_PVAL(IPIN)),ID2(2),
     :                 %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPYR)),
     :                 %VAL(CNF_PVAL(IPYL)),%VAL(CNF_PVAL(IPX)),PLU,
     :                 NPLUS1,%VAL(CNF_PVAL(IPDP)),NUMFIB,NPTS,NORDER,
     :                 EX,FIBOK,VERSION)
  500 CONTINUE
C
C    Close down everything
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR

      END

C+
      SUBROUTINE POLEXT_DUDLIST(FIBOK,MAXFIB,NUSED)
C
C     P O L E X T _ D U D L I S T
C
C     Lists the dud fibres.
C
C     Parameters -
C
C     (>) FIBOK    (Logical array FIBOK(MAXFIB)) Flags if the fibres
C                  are OK.
C     (>) MAXFIB   (Integer) Maximum number of fibres.
C     (>) NUSED    (Integer) Number of fibres used this run.
C
C                                              KS / AAO 6th March 1987
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER MAXFIB, NUSED
      LOGICAL FIBOK(MAXFIB)
C
C     Local variables
C
      LOGICAL EXCESS
      INTEGER I, INVOKE, IPTR, NDUD, NEXT, STATUS
      CHARACTER*64 TEXT
C
C     Functions
C
      INTEGER ICH_ENCODE
C
      EXCESS=.FALSE.
      TEXT='Dud fibres: '
      IPTR=13
      CALL PAR_WRUSER(' ',STATUS)
      NDUD=0
      DO I=1,MAXFIB
         IF (.NOT.FIBOK(I)) THEN
            NDUD=NDUD+1
            IF (IPTR.GE.60) THEN
               CALL PAR_WRUSER(TEXT(:IPTR),STATUS)
               TEXT=' '
               IPTR=13
            END IF
            IF ((I.GT.NUSED).AND.(.NOT.EXCESS)) THEN
               EXCESS=.TRUE.
               TEXT(IPTR:IPTR)='('
               IPTR=IPTR+1
            END IF
            INVOKE=ICH_ENCODE(TEXT,FLOAT(I),IPTR,0,NEXT)
            IPTR=NEXT+1
         END IF
      END DO
      IF (NDUD.EQ.0) THEN
         TEXT='No dud fibres specified'
         IPTR=24
      ELSE
         IF (EXCESS) THEN
            TEXT(IPTR-1:IPTR-1)=')'
         END IF
      END IF
      IF (IPTR.GT.13) CALL PAR_WRUSER(TEXT(:IPTR),STATUS)
      CALL PAR_WRUSER(' ',STATUS)
C
      END


      SUBROUTINE POLEXT_WORK(NX,NY,RAY,NY1,OUT,
     :        YRUN,YLEVEL,X,PLU,NPLUS1,DPCOEF,
     :        NUMFIB,NPTS,NORDER,EX,
     :        FIBOK,VERSION)
*
*
      CHARACTER TEXT*80
      INTEGER PLU, STATUS
      REAL YRUN(NX),RAY(NX,NY),YLEVEL(NPTS)
      REAL OUT(NX,NY1)
      LOGICAL FIBOK(NUMFIB)
      REAL VERSION

* for NAG routines
      DOUBLE PRECISION DPCOEF(2*NPLUS1),YVALUE,XX
      DOUBLE PRECISION X(NPTS)
*
      CALL PAR_WRUSER('Beginning extraction',STATUS)
      CALL PAR_WRUSER(' ',STATUS)

      HEX=EX/2.
*
      XBIN=REAL(NX)/REAL(NPTS)
      DO I=1,NPTS
         X(I)=DBLE((REAL(I-1)*XBIN+REAL(I)*XBIN+1.)/2.)
      END DO
*
      IFIB=0
      DO IFIBRE=1,NUMFIB
         READ(PLU,*,IOSTAT=STATUS)(DPCOEF(II),II=1,NPLUS1)
         IF (STATUS.NE.0) THEN
            CALL GEN_FORTERR(STATUS,.FALSE.,TEXT)
            CALL PAR_WRUSER('Error reading polynomial file',STATUS)
            CALL PAR_WRUSER(TEXT,STATUS)
            GO TO 500
         END IF

*       If file format is version 1, must convert from Chebyshev to
*       ordinary coefficients.
         IF (VERSION.EQ.1.) THEN
            DO II=1,NPLUS1
               DPCOEF(NPLUS1+II)=DPCOEF(II)
            END DO
            CALL GEN_CHB2NO(NPLUS1-1,X(1),X(NPTS),
     :                      DPCOEF(NPLUS1+1),DPCOEF(1))
         END IF

         IF(FIBOK(IFIBRE))THEN
            IFIB=IFIB+1
            IF (MOD(IFIBRE,10).EQ.0) THEN
               TEXT='Extracting fibre '
               INVOKE=ICH_ENCODE(TEXT,FLOAT(IFIBRE),18,0,NEXT)
               CALL PAR_WRUSER(TEXT(:NEXT-1),STATUS)
            END IF

* ie values at centroids
            DO I=1,NPTS
              XX=DBLE(REAL(I-1)*XBIN+0.5*XBIN+0.5)
              YVALUE = DPCOEF(NPLUS1)
              DO II=NPLUS1-1,1,-1
                 YVALUE=YVALUE*XX
                 YVALUE=YVALUE+DPCOEF(II)
              END DO
              YLEVEL(I)=SNGL(YVALUE)
           END DO
*
           DO IX=1,NX
              XX=DBLE(IX)
              YVALUE = DPCOEF(NPLUS1)
              DO II=NPLUS1-1,1,-1
                 YVALUE=YVALUE*XX
                 YVALUE=YVALUE+DPCOEF(II)
              END DO
              YRUN(IX)=SNGL(YVALUE)
           END DO
*
* perform extraction
           RNY=REAL(NY)+0.5
           DO IX=1,NX
              FIBLOW=MAX(YRUN(IX)-HEX,0.5)
              FIBLOW=MIN(RNY,MAX(YRUN(IX)-HEX,0.5))
              FIBTOP=MIN(YRUN(IX)+HEX,RNY)
              FIBTOP=MIN(RNY,MAX(YRUN(IX)+HEX,0.5))
              IYLOW=NINT(FIBLOW)
              IYTOP=NINT(FIBTOP)
              SUM=(REAL(IYLOW)+0.5-FIBLOW)*RAY(IX,IYLOW)
              DO IY=IYLOW+1,IYTOP-1
                 SUM=SUM+RAY(IX,IY)
              END DO
              OUT(IX,IFIB)=SUM+(FIBTOP-REAL(IYTOP)+0.5)*RAY(IX,IYTOP)
            END DO
         ELSE
            IF (MOD(IFIBRE,10).EQ.0) THEN
               TEXT='Ignoring fibre '
               INVOKE=ICH_ENCODE(TEXT,FLOAT(IFIBRE),16,0,NEXT)
               CALL PAR_WRUSER(TEXT(:NEXT-1),STATUS)
            END IF
         END IF
      END DO

      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('Extracted image now created',STATUS)

  500 CONTINUE

      END
