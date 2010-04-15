      SUBROUTINE ECHFIND
*+
*                               E C H F I N D
*
*  Program name:
*     ECHFIND
*
*  Function:
*     Find orders within an echelle image and optionally write a mask
*     image that can be used for quick-look extraction of orders from a
*     raw echelle image.
*
*  Description:
*     Note: This program is believed to work, but it has not been as
*     extensively used and tested as has the ICUR/SDIST method.
*     ICUR/SDIST is believed to be a superior if slightly less
*     convenient way of locating and tracking orders. Having said this,
*     you are welcome to try this program!
*
*     The program can be run in several different ways. The SDIST
*     keyword controls whether an SDIST.DAT file (which can later be
*     used by CDIST) is created and the DOMASK keyword controls whether
*     a mask image (that can later be applied by MASKEXT) is created.
*
*     The program locates the orders by taking a vertical cut (ie in the
*     cross-dispersion direction) through the data (averaging 7 columns)
*     and then searches for peaks occurring above a user-specified
*     threshhold.  Unfortunately this threshhold has to be a constant
*     and this, plus knowing a sensible value to give for it, is one of
*     the major limitations of the program.
*
*     Having located the orders, they are tracked using a method that is
*     a combination of edge detection and centroiding. Little of the
*     feedback and control that is available with SDIST is available and
*     this is another major problem.
*
*     Having tracked the orders, the SDIST.DAT file is written if
*     requested.  If an SDIST.DAT file is not required, a more
*     user-readable listing file is written. Finally, the mask image is
*     written if requested.  The values in the mask are set to be zero
*     if that pixel in the mask does not lie in an order and to a number
*     derived from the order number otherwise (see below). It is
*     guaranteed that every order is extracted using the same number of
*     rows, but of course the position of these rows may vary along an
*     order so one can expect visible jumps in the extracted data,
*     especially if too fews rows are extracted to take all the data
*     from the object.
*
*     The PERISCOPE keyword (see below) determines whether each order
*     has two separate parts (corresponding to object and sky and due to
*     the special periscope that samples object and sky at a wide
*     spacing and brings them together on the slit) or one part
*     (corresponding simply to the slit).  The data values in the mask
*     are 10 * (true order number) + (sub-order number) where the
*     sub-order number is 0 if there is no periscope fitted, 1 if this
*     is the first part of an order and 2 if this is the second part of
*     the order. The "first" and "second" parts of an order are defined
*     so that the actual data values in the mask are monotonic along a
*     vertical slice through it, ie they might go 412, 411, 402, 401 if
*     the periscope is fitted and they might go 410, 400 if it is not
*     fitted.
*
*     If PERISCOPE is false then, unlike in ECHMASK, the user has no
*     option of splitting the data in an order into object and sky.
*     There is room for enhancement here.
*
*  Parameters:
*
*     (>) IMAGE         (File) The name of the raw echelle image.
*     (>) YSTART        (Integer) The starting and ending Y positions to
*     (>) YEND          (Integer) search for orders. Default entire
*                       image.
*     (>) PERISCOPE     (Keyword) Whether or not the periscope is
*                       fitted. Default TRUE.
*     (>) MSTART        (Integer) The order number of the first
*                       "spectrum" in the coefficient file. Default 1.
*     (>) MDELTA        (Integer) +1 if order numbers increase as
*                       "spectrum number" increased, -1 otherwise.
*                       Default -1.
*     (>) SDIST         (Keyword) Whether to write an SDIST.DAT file.
*                       Default FALSE.
*     (>) OUTFILE       (Character) If SDIST is FALSE, the name of the
*                       listing file.
*     (>) THRESH        (Real) The threshhold above which peaks in the
*                       profile across the orders must lie in order to
*                       be considered as order peaks. Default 1000.
*     (>) MINHW         (Integer) The half width that is used for the
*                       median filter that is passed through the
*                       profiles to remove rogue data before looking for
*                       orders. Default 5.
*     (>) DOMASK        (Keyword) Whether to write a mask image. Default
*                       FALSE.
*     (<) OUTPUT        (File) If DOMASK is TRUE, the name of the mask
*                       image. Default MASK.
*
*  Language:
*     FORTRAN
*
*  External variables used:
*
*     None
*
*  Prior requirements:
*     None
*
*  Support: William Lupton, AAO
*
*  Version date: 23-Nov-89
*-
*  History:
*     31 May 1988  WFL.  Original version.
*     14 Sep 1988  WFL.  Add comments to main routine
*     23 Nov 1989  KS.  Change use of DIMS as arguments to
*                  FIG_FINDTRACK, since new version of Fortran compiler
*                  no longer allows variables used in dummy array
*                  declarators to be array elements.  (I think this is a
*                  bug!)
*     24 Sep 1992  HME.  Lowercase file names. Replace call to
*                  discontinued M01AAF by call to M01DAF. Change CALL
*                  EXIT into RETURN. (!) Output default lowercase,
*                  output lowercase extension (mask.dst). Input
*                  lowercase extension. SDIST file sdist.dat, OPEN
*                  statements without CARRIAGECONTROL keyword.
*     03 Aug 1993  HME.  Convert to DSA, use PAR_ABORT.
*     18 Apr 1995  HME.  No longer use NAG, need more workspace for
*                  FIG_DXYFIT.
*     20 Mar 1996  HME.  Fixed broken format string (the line just broke
*                  in the middle of the string, now two strings are
*                  concatenated.)
*     18 Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
*                  file names to 132 chars.
*     2005 June 14 MJC / Starlink  Use CNF_PVAL for pointers to
*                  mapped data.
*+

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      INTEGER MAXDIMS
      PARAMETER (MAXDIMS=2)
*
*     Functions and local variables.
*
      LOGICAL PAR_ABORT
      INTEGER INPTR,NDIMS,DIMS(2),YSTART,YEND,MSTART
      INTEGER MDELTA,MINHW
      INTEGER NELM
      INTEGER OUTPTR
      INTEGER SLOT
      INTEGER STATUS
      LOGICAL PERISCOPE,SDIST,DOMASK
      REAL THRESH,VALUE
      CHARACTER OUTFILE*132
*
*     Open DSA.
*
      STATUS=0
      CALL DSA_OPEN(STATUS)
*
*     Get input.
*
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      CALL DSA_DATA_SIZE('IMAGE',MAXDIMS,NDIMS,DIMS,NELM,STATUS)
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',INPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 9999
*
*     Determine whether the periscope is mounted. If it then each order
*     is split into a sky and an object part.
*
      CALL PAR_RDKEY('PERISCOPE',.TRUE.,PERISCOPE)
      IF (PAR_ABORT()) GO TO 9999
*
*     Determine the starting and ending Y positions to search for orders.
*     It is the user's responsibility to ensure that the first located track
*     is the first one in a given order.
*
      CALL PAR_RDVAL('YSTART',1.0,FLOAT(DIMS(2)),1.0,' ',VALUE)
      YSTART = NINT(VALUE)
      CALL PAR_RDVAL('YEND',VALUE,FLOAT(DIMS(2)),FLOAT(DIMS(2)),' ',
     +                                                        VALUE)
      YEND = NINT(VALUE)
      IF (PAR_ABORT()) GO TO 9999
*
*     Get number of first order in range
*
      CALL PAR_RDVAL('MSTART',1.0,1000.0,1.0,' ',VALUE)
      MSTART=VALUE
      IF (PAR_ABORT()) GO TO 9999
*
*     Get order number increment
*
      CALL PAR_RDVAL('MDELTA',-1.0,1.0,-1.0,' ',VALUE)
      MDELTA=VALUE
      IF (PAR_ABORT()) GO TO 9999

      CALL PAR_RDKEY ('SDIST',.FALSE.,SDIST)
      IF (PAR_ABORT()) GO TO 9999
      IF(SDIST)THEN
        OPEN(UNIT=10,FILE='sdist.dat',STATUS='NEW')
        WRITE(10,'(''*'')')
        WRITE(10,'(''* Written by ECHFIND'')')
        WRITE(10,'(''*'')')
      ELSE
         CALL PAR_RDCHAR('OUTFILE',' ',OUTFILE)
         IF (PAR_ABORT()) GO TO 9999
         OPEN(UNIT=10,FILE=OUTFILE,STATUS='NEW')
      END IF

      CALL PAR_RDVAL('THRESH',0.0,1.0E37,1000.0,' ',THRESH)
      IF (PAR_ABORT()) GO TO 9999

      CALL PAR_RDVAL('MINHW',0.0,50.0,5.0,' ',VALUE)
      MINHW=NINT(VALUE)
      IF (PAR_ABORT()) GO TO 9999
*
*     Determine whether to create an output file.
*
      CALL PAR_RDKEY ('DOMASK',.FALSE.,DOMASK)
      IF (PAR_ABORT()) GO TO 9999
      IF (DOMASK) THEN
*
*        Get output, modelled on input.
*        The data are supposed to be overwritten, the .Y structure to be
*        deleted.
*
         CALL PAR_SDCHAR('OUTPUT','mask',STATUS)
         CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
         CALL DSA_DELETE_AXIS('OUTPUT',2,STATUS)
         IF (STATUS.NE.0) GO TO 9999
*
*        Map the output data array
*
         CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OUTPTR,
     :                     SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 9999
      END IF

      CALL FIG_FINDTRACK(%VAL(CNF_PVAL(INPTR)),DIMS(1),DIMS(2),
     :                   PERISCOPE,YSTART,YEND,MSTART,MDELTA,SDIST,
     :                   THRESH,MINHW,DOMASK,%VAL(CNF_PVAL(OUTPTR)))


9999  CONTINUE
*
*     Close DSA.
*
      CALL DSA_CLOSE(STATUS)
*
      END

      SUBROUTINE FIG_FINDTRACK(IMAGE,DIMS1,DIMS2,PERISCOPE,YSTART,YEND,
     +                  MSTART,MDELTA,SDIST,THRESH,MINHW,DOMASK,OUTPUT)

*  18 Apr 1995 (hme):
*     Change MIDX, MIDY from REAL*8 to REAL. They are no longer passed
*     to MO1DAF, but to GEN_QFISORT.

      INTEGER XMED,XAVE,IRES,IDEG,MDEG,MAXP,MAXO,MAXY
      PARAMETER (XMED=3,XAVE=3,IRES=5,IDEG=6,MDEG=10)
      PARAMETER (MAXP=4096,MAXO=1000,MAXY=4096)

      INTEGER DIMS1,DIMS2,YSTART,YEND,MSTART,MDELTA,MINHW
      LOGICAL PERISCOPE,SDIST,DOMASK
      REAL IMAGE(DIMS1,DIMS2),THRESH,OUTPUT(DIMS1,DIMS2)

      INTEGER START(MAXO),END(MAXO),IW(MAXP),IP(MAXP)
      INTEGER IMID,NUM,ITRACK,IORD,NP,IHWID,JS,JE,JD,JJ,N,J,L,M,I
      INTEGER ADEG,STATUS,DIMS(2)
      REAL XCUT(MAXY),CENTRE(MAXO),WIDTH(MAXO)
      REAL RMAX,TMAX,CEN,ORHWID
      REAL MIDX(MAXP),MIDY(MAXP)
      REAL*8 SMIDX(MAXP),SMIDY(MAXP),W(MAXP),W1(4*MAXP+3*(MDEG+1))
      REAL*8 AK(MDEG+1),MID,GEN_EPOLYD
      CHARACTER CHARS*80

      DIMS(1)=DIMS1
      DIMS(2)=DIMS2
      IMID=DIMS(1)/2
      DO I=YSTART,YEND
         XCUT(I) = 0.0
         DO J = -XAVE,+XAVE
            XCUT(I) = XCUT(I) + IMAGE(IMID+J,I)
         END DO
         XCUT(I) = XCUT(I) / FLOAT(XAVE+1+XAVE)
*        DO J = -XMED,+XMED
*           XSLICE(J) = IMAGE(IMID+J,I)
*        END DO
*        CALL GEN_QFSORT(XSLICE,XMED+1+XMED)
*        XCUT(I) = XSLICE(0)
      END DO

      CALL PAR_WRUSER('Finding orders...',STATUS)
      CALL FIND_ORDERS(XCUT(YSTART),YEND-YSTART+1,MINHW,THRESH,START,
     :                                                END,CENTRE,NUM)
      DO ITRACK = 1,NUM
         START(ITRACK) = START(ITRACK) + YSTART - 1
         END(ITRACK) = END(ITRACK) + YSTART - 1
         CENTRE(ITRACK) = CENTRE(ITRACK) + YSTART - 1
      END DO

      WRITE(CHARS,'(''Found'',I3,'' tracks'')') NUM
      CALL PAR_WRUSER(CHARS,STATUS)
      IF(SDIST)THEN
         WRITE(10,'(''Number of tracks  = '',I5)') NUM
      ELSE
         WRITE(10,'(''Found'',I3,'' tracks'')') NUM
         WRITE(10,'(2I5,1PG14.6))') (START(ITRACK),END(ITRACK),
     +                             CENTRE(ITRACK),ITRACK=1,NUM)
      END IF

      IF(NUM.GT.0)THEN
         DO ITRACK = 1,NUM
            WIDTH(ITRACK) = END(ITRACK) - START(ITRACK)
         END DO
         CALL GEN_QFSORT(WIDTH,NUM)
         IF(NUM.GT.2)THEN
            ORHWID = WIDTH(NUM-2) / 2.0
         ELSE IF(NUM.EQ.2)THEN
            ORHWID = (WIDTH(1) + WIDTH(2)) / 4.0
         ELSE
            ORHWID = WIDTH(1) / 2.0
         END IF
      END IF

      IF(DOMASK)THEN
         CALL PAR_WRUSER('Clearing output array...',STATUS)
         DO I=1,DIMS(2)
            DO J=1,DIMS(1)
               OUTPUT(J,I) = 0.0
            END DO
         END DO
      END IF

      CALL PAR_WRUSER('Tracking orders...',STATUS)
      DO ITRACK=1,NUM
         IF(PERISCOPE)THEN
            IORD = 10 * (MSTART + MDELTA * ((ITRACK+1)/2 - 1)) + 1 +
     +                                                 MOD(ITRACK,2)
         ELSE
            IORD = 10 * (MSTART + MDELTA * (ITRACK - 1))
         END IF

         WRITE(CHARS,'(''Tracking order '',I5)') IORD
         CALL PAR_WRUSER(CHARS,STATUS)
         IF(.NOT.SDIST)THEN
            WRITE(10,'(''Tracking order '',I5)') IORD
         END IF

         NP=0
         IHWID=(END(ITRACK)-START(ITRACK))/2
         JS=IMID
         JE=DIMS(1)
         JD=IRES
         DO JJ=1,2
            N=NINT(CENTRE(ITRACK))
            J=JS
            DO WHILE(J.GE.1.AND.J.LE.DIMS(1).AND.
     +               (N-IHWID).GE.1.AND.(N+IHWID).LE.DIMS(2))

               RMAX=0.0
               TMAX=0.0

               DO L=N-IHWID,N+IHWID
                 DO M=J-XAVE,J+XAVE
                   RMAX=RMAX+IMAGE(M,L)
                   TMAX=TMAX+IMAGE(M,L)*L
                 END DO
               END DO

               IF(RMAX.GT.20.0)THEN
                  CEN=TMAX/RMAX
                  N=CEN
                  NP=NP+1
                  MIDY(NP)=CEN
                  MIDX(NP)=J
                  J=J+JD
               ELSE
                  J=J+ISIGN(1,JD)
               END IF

            END DO
            JS=IMID-1
            JE=1
            JD=-IRES
         END DO

         IF(.NOT.SDIST)THEN
            WRITE(10,'(''Fitting with '',I5,'' points'')') NP
         END IF
         CALL GEN_QFISORT(MIDX,NP,IP)
         DO I=1,NP
            SMIDX(I)=MIDX(IP(I))
            SMIDY(I)=MIDY(IP(I))
         END DO
         CALL FIG_DXYFIT (SMIDX,SMIDY,NP,IDEG,IW,W,W1,AK,ADEG)

         IF(SDIST)THEN
            WRITE(10,'(''Track    # '',I5,'' X coverage from '','//
     +         'I5,'' to '',I5)')
     +         ITRACK,NINT(SMIDX(1)),NINT(SMIDX(NP))
            WRITE(10,'(''Average Y value '',F13.7)') CENTRE(ITRACK)
            WRITE(10,'(3D23.16)') (AK(I),I=1,ADEG+1),(0.0,I=ADEG+2,11)
         ELSE
            WRITE(10,'(5I5,(1PG14.6))') IORD,(END(ITRACK)-
     +             START(ITRACK)+1),NINT(SMIDX(1)),NINT(SMIDX(NP)),
     +                                      ADEG,(AK(I),I=1,ADEG+1)
         END IF
*
*     If requested, fill in the output array.
*
         IF(DOMASK)THEN
            CALL GEN_REVR8(AK,ADEG+1,1,.TRUE.,AK)
            DO I=NINT(SMIDX(1)),NINT(SMIDX(NP))
               MID = GEN_EPOLYD (DBLE(I),AK,ADEG+1)
               DO J=MAX(YSTART,NINT(MID-ORHWID)),
     +              MIN(YEND,NINT(MID+ORHWID))
                  OUTPUT(I,J)=FLOAT(IORD)
               END DO
            END DO
         END IF
      END DO

      CLOSE(UNIT=10)

      RETURN
      END

      SUBROUTINE FIND_ORDERS(VALS,NV,W,THRESH,START,END,CENTRE,NUM)

      REAL VALS(*),THRESH,CENTRE(*)
      INTEGER NV,W,START(*),END(*),NUM

      INTEGER IST,STATUS,FIND_ORDER

      IST=W+1
      NUM=0
      DO WHILE (IST.LT.NV-W)
         STATUS=FIND_ORDER(VALS,NV,W,THRESH,IST,START(NUM+1),END(NUM+1),
     +                             CENTRE(NUM+1))
         IF(STATUS.EQ.0) NUM=NUM+1
      END DO
      RETURN
      END

      INTEGER FUNCTION FIND_ORDER(VALS,NV,W,THRESH,IP,START,END,CENTRE)

      REAL VALS(*),THRESH,CENTRE
      INTEGER NV,W,START,END

      REAL MED,SUMP,SUMM
      INTEGER IP,I
*
*    First make sure we haven't started on top of the order
*
      MED=THRESH+1
      DO WHILE(MED.GE.THRESH.AND.IP.LE.NV-W)
         CALL MEDIAN(VALS(IP-W),2*W+1,MED)
         IP=IP+1
      END DO
      IF(MED.LT.THRESH)THEN
         IP=IP-1
      ELSE
         FIND_ORDER=1
         RETURN
      END IF
*
*    We are definitely in an inter-order gap now so start looking
*    for the rising edge of the next order profile
*
      DO WHILE(MED.LT.THRESH.AND.IP.LE.NV-W)
         CALL MEDIAN(VALS(IP-W),2*W+1,MED)
         IP=IP+1
      END DO
      IF(MED.GE.THRESH)THEN
         IP=IP-1
      ELSE
         FIND_ORDER=1
         RETURN
      END IF
      START=IP
*
*    Now search for the falling edge at the other side of the profile
*
      DO WHILE(MED.GE.THRESH.AND.IP.LE.NV-W)
         CALL MEDIAN(VALS(IP-W),2*W+1,MED)
         IP=IP+1
      END DO
      IF(MED.LT.THRESH)THEN
         IP=IP-1
      ELSE
         FIND_ORDER=1
         RETURN
      END IF
      END=IP-1
*
*    Having found the two edges of the order we can calculate the
*    centroid (using moments)
*
      SUMP=0.0
      SUMM=0.0
      DO I=START,END
         SUMP=SUMP+VALS(I)
         SUMM=SUMM+VALS(I)*I
      END DO
      IF(SUMM.NE.0.0)THEN
         CENTRE=SUMM/SUMP
      ELSE
         CENTRE=(START+END)/2.0
      END IF
      FIND_ORDER=0

      END

      SUBROUTINE MEDIAN(VALS,NV,MED)
      REAL VALS(*),MED
      INTEGER NV

      REAL DVALS(1000)

*  Must use a duplicate, because GEN_QFMED rearranges the array.
      DO I=1,NV
         DVALS(I)=VALS(I)
      END DO
      MED=GEN_QFMED(DVALS,NV)

      END
