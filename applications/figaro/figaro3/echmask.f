      SUBROUTINE ECHMASK
*+
*                               E C H M A S K
*
*  Program name:
*     ECHMASK
*
*  Function:
*     Write a mask image that can be used for quick-look extraction of
*     orders from a raw echelle image.
*
*  Description:
*     The program reads a file that identifies where the orders are in
*     the image and sets the values in the mask to be zero if that pixel
*     in the mask does not lie in an order and to a number derived from
*     the order number otherwise (see below). It is guaranteed that
*     every order is extracted using the same number of rows, but of
*     course the position of these rows may vary along an order so one
*     can expect visible jumps in the extracted data, especially if too
*     fews rows are extracted to take all the data from the object.
*
*     The coefficient file will normally have been written by SDIST and
*     if so must have been written by the version of SDIST that was
*     modified to support ECHMASK.
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
*     If PERISCOPE is false then the user has the option of splitting
*     the data in an order into object and up to two separate regions of
*     sky. The object is assigned sub-order 1 and the sky is assigned
*     sub-order 2. Note that this assignment may differ from when
*     PERISCOPE is true, since in that case there is no guarantee that
*     sub-order 1 is object - it may be sky!  There may be room for
*     enhancement here.
*
*  Parameters:
*
*     (>) COFILE        (Character) The name of the coefficient file.
*                       Default SDIST.
*     (>) PERISCOPE     (Keyword) Whether or not the periscope is
*                       fitted. Default TRUE.
*     (>) OBJWIDTH      (Integer) The number of rows to be extracted on
*                       behalf of the object per order.  If PERISCOPE is
*                       TRUE then object and sky are not distinguished
*                       between and this width also applies to the sky.
*                       If PERISCOPE is FALSE then this width applies
*                       only to the object and the position of the sky
*                       is specified using the S1* and S2* parameters.
*                       If OBJWIDTH is specified as zero then the width
*                       information from the coefficient file is used to
*                       derive a sensible value. Default 0.
*     (>) OBJOFFSET     (Float) The offset of the centre of the object
*                       data from the centre of each order. If specified
*                       as being non-zero and if OBJWIDTH is zero, the
*                       derived width is adjusted to take account of the
*                       offset. Default 0.
*     (>) S1WIDTH       (Integer) The number of rows to be extracted on
*                       behalf of the first region of sky per order.
*                       This and the other S* parameters are prompted
*                       for only if PERISCOPE is false. If specified as
*                       zero, it is assumed that no sky is to be
*                       extracted and the remaining S* parameters are
*                       not prompted for. Default 0.
*     (>) S1OFFSET      (Float) The offset of the centre of the first
*                       region of sky from the centre of the object data
*                       (not necessarily from the centre of the order).
*                       Default 0.
*     (>) S2WIDTH       (Integer) These parameters are the same as
*     (>) S2OFFSET      (Float)   S1WIDTH and S2OFFSET but they refer to
*                                 the second region of sky if any.
*                                 Defaults 0.
*     (>) MSTART        (Integer) The order number of the first
*                       "spectrum" in the coefficient file. Default 1.
*     (>) MDELTA        (Integer) +1 if order numbers increase as
*                       "spectrum number" increased, -1 otherwise.
*                       Default -1.
*     (<) MASK          (File) The name of the output mask image. This
*                       is created with only a .Z.DATA structure.
*                       Default MASK.
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
*  Version date: 31-May-88
*-
*  History:
*     06 Jun 1988  WFL.  Add S* parameters and change EXWIDTH to be
*                  OBJWIDTH.
*     04 Aug 1988  WFL.  Remove SDIST.DAT-reading code to FIG_READSDIST.
*     25 Sep 1992  HME.  Lowercase default file name sdist.dat.
*                  Lowercase default mask file mask and lowercase
*                  extension .dst.
*     03 Aug 1993  HME.  Convert to DSA, use PAR_ABORT.
*     15 Feb 1996  HME.  Change access for output from update to write.
*     18 Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
*                  file names to 132 chars.
*     2005 June 14 MJC / Starlink  Use CNF_PVAL for pointers to
*                  mapped data.
*+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*
*     Constant parameters
*
      INTEGER MAXTRACKS
      PARAMETER (MAXTRACKS=200)
*
*     Local variables
*
      INTEGER SLOT
      INTEGER NTRACKS,TRACK,DIMS(2),NX,NY,START(MAXTRACKS)
      INTEGER END(MAXTRACKS),MSTART,MDELTA,OBJWIDTH,S1WIDTH,S2WIDTH
      INTEGER MPTR,STATUS,IGNORE
      LOGICAL PERISCOPE
      REAL VALUE,WIDTH(MAXTRACKS),MAXWIDTH,OBJOFFSET,S1OFFSET,S2OFFSET
      REAL*8 COEFF(11,MAXTRACKS),YAVE(MAXTRACKS)
      CHARACTER COFILE*132,CHARS*80
*
*     Functions
*
      LOGICAL PAR_ABORT
*
*     Open DSA
*
      STATUS=0
      CALL DSA_OPEN(STATUS)
*
*     Read the name of the coefficient file that has probably been
*     created by SDIST. Then read the file and validate that it is of
*     the new format by checking that the image dimensions are non-zero.
*     If OK, add the average Y positions to the constant terms and
*     reverse the order of the coeff- icients so that the constant term
*     is last (they are needed this way for the polynomial evaluation
*     routines).
*
      CALL PAR_RDCHAR('COFILE','sdist.dat',COFILE)
      IF (PAR_ABORT()) GOTO 9999
      CALL FIG_READSDIST(COFILE,MAXTRACKS,NTRACKS,NX,NY,START,END,
     :                                    YAVE,WIDTH,COEFF,STATUS)
      IF(STATUS.NE.0)THEN
         GOTO 9999
      ELSE IF (NX.EQ.0.OR.NY.EQ.0) THEN
         CALL PAR_WRUSER('Size of image read from coeff file is zero '//
     :                                      '(old file format?)',IGNORE)
         GOTO 9999
      ELSE IF (NTRACKS.EQ.0) THEN
         CALL PAR_WRUSER('Number of tracks is zero. Not much point '//
     :                                    'in writing a mask!',IGNORE)
         GOTO 9999
      END IF
      DO TRACK=1,NTRACKS
         COEFF(1,TRACK) = COEFF(1,TRACK) + YAVE(TRACK)
      END DO
      CALL GEN_REVR8(COEFF,11,NTRACKS,.TRUE.,COEFF)
*
*     Determine whether the periscope is mounted. If it is then each
*     order is split into a sky and an object part.
*
      CALL PAR_RDKEY('PERISCOPE',.TRUE.,PERISCOPE)
      IF (PAR_ABORT()) GOTO 9999
*
*     Calculate the theoretical maximum width for any of the object or
*     sky regions. This is used as a maximum value for the parameter
*     prompts.
*
      MAXWIDTH = FLOAT(NY/NTRACKS)
*
*     Get object width and offset of object centre from order centre.
*     Zero for the width means use a number derived from the coefficient
*     file.
*
      CALL PAR_RDVAL('OBJWIDTH',0.0,MAXWIDTH,0.0,' ',VALUE)
      OBJWIDTH = NINT(VALUE)
      CALL PAR_RDVAL('OBJOFFSET',0.0,MAXWIDTH,0.0,' ',OBJOFFSET)
      IF (PAR_ABORT()) GOTO 9999
*
*     Determine width to use for extraction of object. If OBJWIDTH is
*     positive, use it as the object width. Otherwise, use the width of
*     the third widest track as determined from the coefficient file
*     with twice OBJOFFSET subtracted from the result to keep all the
*     extracted data within the order). Note that the WIDTH array is
*     sorted here but this diesn't matter because it not used again.
*
      IF (OBJWIDTH.EQ.0) THEN
         CALL GEN_QFSORT(WIDTH,NTRACKS)
         IF(NTRACKS.GT.2)THEN
            VALUE = WIDTH(NTRACKS-2)
         ELSE IF(NTRACKS.EQ.2)THEN
            VALUE = (WIDTH(1) + WIDTH(2)) / 2.0
         ELSE
            VALUE = WIDTH(1)
         END IF
         VALUE = VALUE - 2.0 * OBJOFFSET
         OBJWIDTH = NINT(VALUE)
         WRITE(CHARS,'(A,I2,A)') '*** Will use an OBJWIDTH of ',
     :                                        OBJWIDTH,' pixels'
         CALL PAR_WRUSER(CHARS,IGNORE)
      END IF
*
*     Set the initial default values of zero for all the sky widths and
*     offsets. Only get values if PERISCOPE is false and stop at the
*     first zero width.
*
      S1WIDTH = 0
      S1OFFSET = 0.0
      S2WIDTH = 0
      S2OFFSET = 0.0
      IF (.NOT.PERISCOPE) THEN
         CALL PAR_RDVAL('S1WIDTH',0.0,MAXWIDTH,0.0,' ',VALUE)
         IF (PAR_ABORT()) GOTO 9999
         S1WIDTH = NINT(VALUE)
         IF (S1WIDTH.NE.0) THEN
            CALL PAR_RDVAL('S1OFFSET',-MAXWIDTH,MAXWIDTH,0.0,' ',
     :                                                  S1OFFSET)
            CALL PAR_RDVAL('S2WIDTH',0.0,MAXWIDTH,0.0,' ',VALUE)
            IF (PAR_ABORT()) GOTO 9999
            S2WIDTH = NINT(VALUE)
            IF (S2WIDTH.NE.0) THEN
               CALL PAR_RDVAL('S2OFFSET',-MAXWIDTH,MAXWIDTH,0.0,' ',
     :                                                     S2OFFSET)
               IF (PAR_ABORT()) GOTO 9999
            END IF
         END IF
      END IF
*
*     Check that the object and sky regions do not overlap and that the
*     total width being extracted does not exceed the maximum
*     theoretical width. These checks are not foolproof but they should
*     prevent most inadvertent errors.
*
      IF (S1WIDTH.GT.0) THEN
         IF (ABS(S1OFFSET)-FLOAT(S1WIDTH)/2.0.LT.FLOAT(OBJWIDTH)/2.0)
     :                                                           THEN
            CALL PAR_WRUSER('Object and sky region 1 overlap',IGNORE)
            GOTO 9999
         END IF
      END IF
      IF (S2WIDTH.GT.0) THEN
         IF (ABS(S2OFFSET)-FLOAT(S2WIDTH)/2.0.LT.FLOAT(OBJWIDTH)/2.0)
     :                                                           THEN
            CALL PAR_WRUSER('Object and sky region 2 overlap',IGNORE)
            GOTO 9999
         END IF
      END IF
      IF (S1WIDTH.GT.0.AND.S2WIDTH.GT.0) THEN
         IF ((S1OFFSET.GT.S2OFFSET.AND.
     :        S1OFFSET-FLOAT(S1WIDTH)/2.0.LT.
     :        S2OFFSET+FLOAT(S2WIDTH)/2.0)    .OR.
     :       (S2OFFSET.GT.S1OFFSET.AND.
     :        S2OFFSET-FLOAT(S2WIDTH)/2.0.LT.
     :        S1OFFSET+FLOAT(S1WIDTH)/2.0))   THEN
            CALL PAR_WRUSER('Sky regions 1 and 2 overlap',IGNORE)
            GOTO 9999
         END IF
      END IF
*
*     Get number of first order in range.
*
      CALL PAR_RDVAL('MSTART',1.0,1000.0,1.0,' ',VALUE)
      IF (PAR_ABORT()) GOTO 9999
      MSTART=VALUE
*
*     Get order number increment.
*
      CALL PAR_RDVAL('MDELTA',-1.0,1.0,-1.0,' ',VALUE)
      IF (PAR_ABORT()) GOTO 9999
      MDELTA=VALUE
*
*     Create the mask file.
*
      CALL PAR_SDCHAR('MASK','mask',STATUS)
      CALL DSA_OUTPUT('MASK','MASK',' ',1,1,STATUS)
      IF (STATUS.NE.0) GO TO 9999
      DIMS(1) = NX
      DIMS(2) = NY
      CALL DSA_SIMPLE_OUTPUT('MASK','D','FLOAT',2,DIMS,STATUS)
      IF (STATUS.NE.0) GO TO 9999
*
*     Map the mask data array.
*
      CALL DSA_MAP_DATA('MASK','WRITE','FLOAT',MPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 9999
*
*     Call the routine that actually writes the mask.
*
      CALL FIG_ECHMASK(NX,NY,NTRACKS,PERISCOPE,OBJWIDTH,OBJOFFSET,
     :                 S1WIDTH,S1OFFSET,S2WIDTH,S2OFFSET,MSTART,
     :                 MDELTA,START,END,COEFF,%VAL(CNF_PVAL(MPTR)))
*
*     Always jump here prior to exit. Close the coefficient file and then
*     close down DSA.
*
9999  CONTINUE
      CLOSE(UNIT=10,IOSTAT=IGNORE)
      CALL DSA_CLOSE(STATUS)
*
      END

*+
      SUBROUTINE FIG_ECHMASK(NX,NY,NTRACKS,
     :                       PERISCOPE,OBJWIDTH,OBJOFFSET,
     :                       S1WIDTH,S1OFFSET,S2WIDTH,S2OFFSET,
     :                       MSTART,MDELTA,
     :                       START,END,COEFF,
     :                       MASK)
*
*                            F I G _ E C H M A S K
*
*  Routine name:
*     FIG_ECHMASK
*
*  Function:
*     Write the mask that is to be used to extract echelle orders directly from
*     raw data.
*
*  Description:
*     Clear the output array and then loop through all the tracks from the
*     input image. Determine the order number of each track and then for those
*     pixels that lie in an order, set the mask to a value derived from the
*     order number. This is 10 * (order number) + (one or two) where the one or
*     two is referred to as the sub-order number. If the periscope is fitted,
*     the sub-order either sub-order may be object or sky (it depends on how
*     the user selected the tracks in the first place). If the periscope is not
*     fitted the sub-order number is one for object and two for sky.
*
*     Care is taken to ensure that each order and sub-order contain precisely
*     the requested number of pixels of object and sky.
*
*  Language:
*     FORTRAN
*
*  Call:
*     CALL FIG_ECHMASK (NX,NY,NTRACKS,
*                       PERISCOPE,OBJWIDTH,OBJOFFSET,
*                       S1WIDTH,S1OFFSET,S2WIDTH,S2OFFSET,
*                       MSTART,MDELTA,
*                       START,END,COEFF,
*                       MASK)
*
*  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
*
*     (>) NX,NY         (Integer,ref) Dimensions of mask image
*     (>) NTRACKS       (Integer,ref) Number of tracks in input image
*     (>) PERISCOPE     (Logical,ref) Whether the star / sky periscope is fitted
*     (>) OBJWIDTH      (Integer,ref) The number of rows to be extracted from
*                       each order on behalf of the object
*     (>) OBJOFFSET     (Real,ref) Offset from the centre of the order to the
*                       centre of the object data
*     (>) S1WIDTH       (Integer,ref) The number of rows to be extracted from
*                       each order on behalf of the first sky region
*     (>) S1OFFSET      (Real,ref) Offset from the centre of the object to the
*                       centre of the first sky region
*     (>) S2WIDTH       (Integer,ref) The number of rows to be extracted from
*                       each order on behalf of the second sky region
*     (>) S2OFFSET      (Real,ref) Offset from the centre of the object to the
*                       centre of the second sky region
*     (>) MSTART        (Integer,ref) The order number corresponding to the
*                       first track in the input image
*     (>) MDELTA        (Integer,ref) The order number increment as track number
*                       increases (if the periscope is fitted the order number
*                       changes only every two tracks)
*     (>) START(NTRACKS) (Integer array,ref) The starting (X) pixel number for
*                       which this track was tracked
*     (>) END(NTRACKS)  (Integer array,ref) The ending (X) pixel number for
*                       which this track was tracked
*     (>) COEFF(11,NTRACKS) (Real*8 array,ref) The 10th order polynomial fit
*                       coefficients to the tracks, constant term last
*     (<) MASK(NX,NY)   (Real array,ref) The mask image array that is to be
*                       written
*
*  External variables used:
*
*     None
*
*  External subroutines / functions used:
*
*     GEN_EPOLYD
*     Standard Fortran Functions
*
*  Prior requirements:
*     None
*
*  Support: William Lupton, AAO
*
*  Version date: 06-Jun-88
*-
*  History:

*
*     Parameters
*
      INTEGER NX,NY,NTRACKS,OBJWIDTH,S1WIDTH,S2WIDTH,MSTART,MDELTA
      INTEGER START(NTRACKS),END(NTRACKS)
      LOGICAL PERISCOPE
      REAL OBJOFFSET,S1OFFSET,S2OFFSET,MASK(NX,NY)
      REAL*8 COEFF(11,NTRACKS)
*
*     Local variables
*
      INTEGER TRACK,ORDER,CENTRE,FIRST,LAST,I,J
      REAL FORDER
      REAL*8 MID
*
*     Functions
*
      REAL*8 GEN_EPOLYD
*
*     Clear the mask array.
*
      DO I=1,NY
         DO J=1,NX
            MASK(J,I) = 0.0
         END DO
      END DO
*
*     Fill in the mask values. Simply cycle through each track being very
*     careful to extract exactly the requested number of rows of object
*     and sky.
*
      DO TRACK=1,NTRACKS
*
*        Calculate (ten times) the order number. If the periscope is fitted
*        then there are two tracks per order. One is object and the other is
*        sky but we don't know which is which!
*
         IF(PERISCOPE)THEN
            ORDER = 10 * (MSTART + MDELTA * ((TRACK+1)/2 - 1))
         ELSE
            ORDER = 10 * (MSTART + MDELTA * (TRACK - 1))
         END IF
         FORDER = FLOAT(ORDER)
*
*        Only fill in values in those rows that were successfully tracked.
*
         DO I=START(TRACK),END(TRACK)
*
*           Calculate the Y value of the middle of the track at the current
*           X value.
*
            MID = GEN_EPOLYD (DBLE(I),COEFF(1,TRACK),11)
*
*           Fill in mask values. The range of rows is a function of whether
*           the required width is even or odd. If it is odd the centre is
*           rounded to the nearest pixel and equal numbers of rows are used
*           each side of it. If it is even the centre is rounded down to
*           the nearest half pixel and equal numbers of rows are used each
*           side of the half pixel boundary. First do the object data. The
*           data value to write depends on the value of PERISCOPE. If it
*           is TRUE, it is 10 * (order number) + (one or two). If it is
*           FALSE it is 10 * (order number) + (one).
*
            IF (MOD(OBJWIDTH,2).EQ.1) THEN
               CENTRE = NINT(MID+DBLE(OBJOFFSET))
               FIRST = MAX(1,CENTRE-OBJWIDTH/2)
               LAST = MIN(NY,CENTRE+OBJWIDTH/2)
               IF (PERISCOPE) THEN
                  DO J = FIRST,LAST
                     MASK(I,J) = FORDER + 1.0 + MOD(TRACK,2)
                  END DO
               ELSE
                  DO J = FIRST,LAST
                     MASK(I,J) = FORDER + 1.0
                  END DO
               END IF
            ELSE
               CENTRE = NINT(MID+DBLE(OBJOFFSET)-0.5D0)
               FIRST = MAX(1,CENTRE+1-OBJWIDTH/2)
               LAST = MIN(NY,CENTRE+OBJWIDTH/2)
               IF (PERISCOPE) THEN
                  DO J = FIRST,LAST
                     MASK(I,J) = FORDER + 1.0 + MOD(TRACK,2)
                  END DO
               ELSE
                  DO J = FIRST,LAST
                     MASK(I,J) = FORDER + 1.0
                  END DO
               END IF
            END IF
*
*           Now the two sky regions are each handled inexactly the same
*           way. If PERISCOPE is TRUE the sky widths will be zero and note
*           that since the DO loops are zero trip for zero widths there
*           is no need to check any special cases. Note that the mask data
*           value for sky is 10 * (order number) + (two).
*
            IF (MOD(S1WIDTH,2).EQ.1) THEN
               CENTRE = NINT(MID+DBLE(S1OFFSET))
               FIRST = MAX(1,CENTRE-S1WIDTH/2)
               LAST = MIN(NY,CENTRE+S1WIDTH/2)
               DO J = FIRST,LAST
                  MASK(I,J) = FORDER + 2.0
               END DO
            ELSE
               CENTRE = NINT(MID+DBLE(S1OFFSET)-0.5D0)
               FIRST = MAX(1,CENTRE+1-S1WIDTH/2)
               LAST = MIN(NY,CENTRE+S1WIDTH/2)
               DO J = FIRST,LAST
                  MASK(I,J) = FORDER + 2.0
               END DO
            END IF
            IF (MOD(S2WIDTH,2).EQ.1) THEN
               CENTRE = NINT(MID+DBLE(S2OFFSET))
               FIRST = MAX(1,CENTRE-S2WIDTH/2)
               LAST = MIN(NY,CENTRE+S2WIDTH/2)
               DO J = FIRST,LAST
                  MASK(I,J) = FORDER + 2.0
               END DO
            ELSE
               CENTRE = NINT(MID+DBLE(S2OFFSET)-0.5D0)
               FIRST = MAX(1,CENTRE+1-S2WIDTH/2)
               LAST = MIN(NY,CENTRE+S2WIDTH/2)
               DO J = FIRST,LAST
                  MASK(I,J) = FORDER + 2.0
               END DO
            END IF
         END DO
      END DO
      END
