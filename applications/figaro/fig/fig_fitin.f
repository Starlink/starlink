C+
      SUBROUTINE FIG_FITIN (MTCHN,LU,SWAP,BLOCK,ENAME,FLOAT,
     :                                             STATUS,ERROR)
C
C     F I G _ F I T I N
C
C     Reads a FITS tape file and creates a Figaro data structure
C     for it.  As an alternative option, can also read the data
C     from a disk file.
C
C     Parameters -   (">" input, "<" output )
C
C     (>) MTCHN   (Integer) TIO_ channel number for the input tape.
C                 The tape must have been opened using TIO_OPEN
C                 and FITIN assumes the tape is positioned at the start
C                 of the header record(s). MTCHN is ignored if LU is
C                 non-zero.
C     (>) LU      (Integer) Logical unit for the disk file, if one
C                 is to be used instead of tape.  The file should
C                 have been opened already.  If LU is zero, the
C                 tape specified by MTCHN will be used instead. The
C                 file should have been opened by FIG_DFITS_OPEN.
C     (>) SWAP    (Logical) True if data is to be byte-swapped.
C                 A genuine FITS tape will have byte-swapped data,
C                 so if LU is 0, SWAP should always be true.  With
C                 disk files either swap option is acceptable.
C     (>) BLOCK   (Integer) A code for how blocked data is to be
C                 handled.  0 => Data always starts in the actual
C                 tape record following the last one used for header
C                 information.  1 => Data starts in the 2880-byte
C                 logical block following the header information, and
C                 this will often be in the same physical record used
C                 for the header information.  BLOCK=0 is used by JT's
C                 FITS-like tape format, BLOCK=1 is needed by AIPS and
C                 similar systems.  See below.
C     (>) ENAME   (Character) The name of the structure to be
C                 created.  A file called ENAME//'.ext' where 'ext' is
C                 the default Figaro file extension will be created for
C                 the structure.
C     (>) FLOAT   (Logical) False if data is to be maintained in
C                 the integer form read from tape, as opposed to being
C                 converted to Floating point by use of the BZERO and
C                 BSCALE keyword values.  If the tape actually contains
C                 values for BSCALE and BZERO other than 1.0 and 0.0
C                 then this will override a false value of FLOAT and
C                 floating point data will be created anyway.  (The
C                 special case of BSCALE = 1.0, BZERO = 32768.0 and
C                 BITPIX=16 is trapped and causes unsigned short data
C                 to be generated if FLOAT.
C                 is false.)
C     (<) STATUS  (Integer) A status code for the read. 0 => OK,
C                 1 => error.
C     (<) ERROR   (Character) Returns with a message describing
C                 the error, should one occur.  Note that FIG_FITIN does
C                 generate quite detailed error messages itself, and
C                 this will just be a summary.
C
C     Prior requirements:
C
C     This routine uses the DSA subroutine package to write its output
C     files, and calls DSA_OPEN and DSA_CLOSE itself.  (This is unusual
C     for a Figaro subroutine, and is forced by the requirement that it
C     be compatible with previous versions - which, for historical
C     reasons, deferred the call to DTA_ASFNAM that created the output
C     file until the data size was known - it dates from before the DTA
C     package knew how to extend files!).  This means that although the
C     mag tape or disk file must have been opened by the calling
C     routine, DSA_OPEN should NOT have been called.
C
C     Input -
C
C     The tape must be FITS standard, and should begin 'SIMPLE=T'.
C     It must specify NAXIS, and as many NAXISn values as are
C     implied by the value of NAXIS.  It must also specify
C     BITPIX, which must be one of 8,16, 32 or -32.  (-32 specifying
C     single precision IEEE format real values) The same requirements
C     apply to the header records in disk files.
C
C     Output -
C
C     The data structure created depends on the default file format
C     being used by the DSA routines.  If CRVALn, CRPIXn and CRDELTn
C     axis keywords are included, axis arrays are generated.  Standard
C     keywords such as NAXIS, BITPIX, BSCALE, BZERO, etc are trapped
C     and used by the program, all keywords the program does not
C     understand are put into a FITS-specific structure.
C
C     The program is fairly tolerant - it will ignore header records
C     that it cannot understand.
C
C     Notes:
C
C     1) It is believed that this program can handle any valid FITS tape
C     that conforms to the description in Wells et al.
C     (Astron. Astrophys. Suppl. Ser. 44 (1981) 363-370), and it also
C     supports the blocked FITS extension described in Grosbol et al.
C     (Astron. Astrophys. Suppl. Ser. 73, (1988) 359-364).  It does not
C     support other extensions such as uv-FITS or tables.
C
C     2) This program can handle tapes with records whose sizes are
C     multiples of 2880 bytes.  There are two ways you can block data
C     in this way.  You can say that this is a FITS format with n*2880
C     substituted for 2880 everywhere, or you can say that the data is
C     written in logical records each 2880 bytes long, which may be
C     blocked on the tape into larger physical records.  The only
C     difference is where the actual data starts.  In the former case,
C     (used by JT at Caltech and originally the only case accepted by
C     this routine) the data starts in the physical record following
C     the end of the header data.  In the latter (used now by AIPS and
C     the official way to go as described in Grosbol et al (op. cit.))
C     it starts in the logical record following the header data, and so
C     may be in the same physical record as the header data.  BLOCK=0
C     and BLOCK=1 correspond to these two cases.
C
C     3) Unsigned short data. Some FITS tape writers (like Figaro's
C     WIFITS) treat the special case of unsigned 16-bit integer data -
C     which is  quite common in astronomy - by writing it with a BZERO
C     value of 32768.0 and a BSCALE of 1.0.  In this case, the signed
C     16-bit data that is written to tape is just the original unsigned
C     values with the sign bits flipped (a sneaky way of subtracting
C     32768).  This is quite legal, and this routine can trap that case
C     and reverse the operation (although it only does this in the
C     special case where BSCALE and BZERO and BITPIX have the values
C     32768.0, 1.0, and 16, and where FLOAT was set false).
C
C     Subroutines / functions called -
C
C     ICH_NUMBR    Decode number from string
C     ICH_NUMBD    Like ICH_NUMBR, but double prec.
C     ICH_VERIF    Position of next character not in given list.
C     ICH_DELIM    Position of next character in given list.
C     CNF_PVAL     Full pointer to dynamically allocated memory.
C     DSA_OPEN     Initialise DSA system.
C     DSA_NAMED_OUTPUT Open named file for output by DSA routines.
C     DSA_PUT_FITS_x   Write out a FITS keyword of given type.
C     DSA_SET_OBJECT   Write out the name of the observed object.
C     DSA_SET_AXIS_INFO      Set units and label strings for axis.
C     DSA_COERCE_DATA_ARRAY  Force existence of array of given size &
C                            type.
C     DSA_MAP_AXIS_DATA      Map axis data array in structure.
C     DSA_MAP_DATA     Map main data array in structure.
C     DSA_UNMAP    Close down mapping of data array.
C     DSA_CLOSE    Shut down DSA system.
C     DSA_WRUSER   Buffered output of message strings (flushed by '\N')
C     TIO_READ     Read record from tape
C     TIO_EOF      See if a TIO status code indicates end of file.
C     TIO_ERR      See if a TIO status code indicates an error or not.
C     TIO_GETMSG   Get error message from TIO status value.
C     TIO_SKIP     Skip file mark on tape
C     GEN_MULCAF   Multiply real array by a constant
C     GEN_ADDCAF   Add a constant to a real array
C     GEN_BSWAP    Swop bytes in an array
C     GEN_WBSWAP   Swop words in an array
C     GEN_SFLIP    Flip the sign bits of an array of 16bit integers
C     DSA_FMTCON   Convert data between internal formats
C     FIG_WAVSET   Set an array given coefficient values
C     PAR_WRUSER   Output message to user.
C     FIG_DFITS_HREAD Read a disk FITS header record
C     FIG_DFITS_READ  Read a disk FITS file data record
C
C                                       KS / CIT  13th April 1984
C     Modified:
C
C     1st May 1985   KS / AAO.  .OBS,.X,.Y (etc.) structures added.
C     1st May 1986   KS / AAO.  SWABYT,SWAWRD replaced by calls to
C                    GEN_BSWAP and GEN_WBSWAP.
C     17th Jun 1986  KS / AAO.  SWAP parameter added.
C     1st July 1986  KS / AAO.  Test for SIMPLE=T corrected.  Now
C                    produces an error message, but tries to read.
C     17th Oct 1988  KS / AAO.  BLOCK parameter added.  BUFSIZ
C                    increased.
C     21st Mar 1989  KS / AAO.  PWH (ST-AND)'s mod to handle HISTORY and
C                    COMMENT correctly added.  (Eventually, this will
C                    all use DSA, which will simplify things a lot.)
C     4th  May 1990  KS / AAO.  Re-worked to use the DSA routines
C                    instead of direct calls to DTA routines to create
C                    the output file.  This is a substantial recoding.
C                    FLOAT=.FALSE. is no longer regarded as forcing, if
C                    the data really does need to be scaled up by
C                    BSCALE.  COMMENT, HISTORY and blank keywords are
C                   now recorded properly.  Trap for USHORT data added.
C                    Some attempt made to tidy up comments.
C     7th Aug 1990   KS/AAO.  A previous fix that corrected a problem if
C                    a tape was blocked and the header exactly filled a
C                    number of logical records got lost and has now been
C                    reinstalled.  Also, a new bug connected with
C                    blocked tapes where the data all fits into the
C                    first physical record has been fixed.
C     21st Aug 1990  KS/AAO. FIG_DFITS routines now used to read disk
C                    FITS files.  This allows a greater range of file
C                    types (including UNIX files) to be handled.
C     22nd Mar 1991  KS/AAO. Amazingly, the previous mod removed the
C                    non-standard equivalence between HEADER and BUFFER
C                    and so managed to break the tape-reading version,
C                    and this remained unnoticed.  BUFFER is now copied
C                    into HEADER.
C     24th Feb 1993  KS/AAO (and BDC/UNSW). Replaced use of STL_FMTCON
C                    with use of DSA_FMTCON.
C     5th  Mar 1993  KS/AAO. Changed MTPCKG calls for TIO package calls.
C     9th  Mar 1993  KS/AAO. Added support for BITPIX=-32.
C     18th Mar 1993  KS/AAO. Should now handle tapes with NAXIS set to
C                    zero, and skips over file if tape has been moved.
C      2nd Jun 1993  KS/AAO. Corrected bug in tape moved test.
C      5th Jul 1993  KS/AAO. Removed unused variables.
C      6th Jul 1994  HME/UoE, Starlink. Disabled BITPIX=-32. GEN_IEEETOR
C                    does not work properly, at least not on all
C                    machines.
C      1st Jul 1996  MJC/Starlink, UCL.  Added call to DSA_SIMPLE_OUTPUT
C                    prior to trying to save the FITS header.
C     2005 June 15   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters -
C
      LOGICAL FLOAT,SWAP
      INTEGER MTCHN,LU,BLOCK,STATUS
      CHARACTER*(*) ERROR,ENAME
C
C     Buffer size (multiple of 2880)
C
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=28800)
C
C     Functions used
C
      LOGICAL TIO_EOF,TIO_ERR
      INTEGER ICH_NUMBD,ICH_NUMBR
      INTEGER ICH_VERIF,ICH_DELIM
C
C     Format codes for DSA_FMTCON
C
      INTEGER UBYTE,REALF,WORD,LWORD
      PARAMETER (UBYTE=6,REALF=4,WORD=2,LWORD=3)
C
C     Local variables
C
      LOGICAL   BITP             ! True once BITPIX found in header
      INTEGER   BITPIX           ! Value of BITPIX keyword
      DOUBLE PRECISION BSCALE    ! Value of BSCALE keyword
      BYTE      BUFFER(BUFSIZ)   ! Main input buffer
      INTEGER   BYTES            ! Byte count - in all data,then in one
                                 ! record
      INTEGER   BYTPIX           ! Number of bytes per image pixel
      DOUBLE PRECISION BZERO     ! Value of BZERO keyword
      REAL      CDELTS(6)        ! Values of CRDELTn keywords
      CHARACTER CHARS(2)*64      ! Unit and label strings for axis
      CHARACTER CHR              ! General single character
      REAL      COEFF(2)         ! Coefficients for axis data values
      CHARACTER COMMENT*80       ! Comment for keyword
      INTEGER   CRDMAX           ! Maximum number of header 'cards' in
                                 ! header
      REAL      CRPIX(6)         ! Values of CRPIXn keywords
      REAL      CRVALS(6)        ! Values of CRVALn keywords
      INTEGER   DIMS(9)          ! Value of NAXISn keywords
      INTEGER   DPTR             ! Dynamic-memory pointer for axis data
                                 ! array
      INTEGER   DSA_STATUS       ! Inherited status used by DSA routines
      DOUBLE PRECISION DVALUE    ! Temporary double precision value
      INTEGER   ERRLEN           ! Length of TIO error message
      CHARACTER HEADER*(BUFSIZ)  ! Input buffer treated as character
                                 ! string
      INTEGER   I                ! General loop index
      INTEGER   IAX              ! Index through axes
      INTEGER   ICMTST           ! Star character number for comment
      LOGICAL   IEEE_FPT         ! Input data is IEEE floating point
                                 ! format
      INTEGER   IEND             ! Number of header record with END
                                 ! keyword
      INTEGER   IENSTR           ! Character position of end of string
                                 ! value
      INTEGER   IGNORE           ! Status value we don't care about
      INTEGER   IPT              ! Character position of start of 'card'
      INTEGER   IREC             ! Counter through input records
      INTEGER   ISLSH            ! Position of '/' character -delimits
                                 ! comment
      LOGICAL   ISNEW            ! Is address new to CNF?
      INTEGER   IST              ! Start character when decoding
                                 ! keywords
      INTEGER   ISTAT            ! Status from numeric decoding from
                                 ! string
      INTEGER   ISTART           ! `Card' number we start at in header
      INTEGER   ITEMS            ! Number of array elements in buffer
      LOGICAL   KNOWN            ! Indicates a keyword is recognised
      CHARACTER LABELS(6)*64     ! Labels for each axis
      INTEGER   LASTCH           ! Position of last character for this
                                 ! 'card'
      INTEGER   LENGTH           ! Length of input record from tape
      LOGICAL   MOVED            ! True if tape has been moved
      INTEGER   NAXIS            ! Value of NAXIS keyword
      CHARACTER NAXISN*6         ! Strings 'NAXIS1'..'NAXIS6'
      INTEGER   NBAD             ! Total number of bad IEEE conversions
      INTEGER   NCH              ! Number of characters in string
      INTEGER   NEXT             ! Next character following number in
                                 ! string
      INTEGER   NSFIG            ! Number of significant figures in
                                 ! number
      CHARACTER ONAME*64         ! Name of keyword read from 'card'
      INTEGER   OPTR             ! Dynamic-memory pointer in main data
                                 ! array
      LOGICAL   PISNEW           ! Previous CNF pointer to data new?
      INTEGER   PIXELS           ! Number of pixels in data array
      INTEGER   PIXRC1           ! Number of data pixels in header
                                 ! record
      INTEGER   PIXREC           ! Number of pixels in an input record
      INTEGER   RECLEN           ! Length of input record
      INTEGER   SLOT             ! DSA mapped slot number
      INTEGER   SRCFMT           ! Code for input data type used
      INTEGER   STBYTE           ! First data byte in header record
      INTEGER   TPTR             ! Temporary dynamic mem pointer
      CHARACTER TYPE*8           ! Primitive type to use for output data
      LOGICAL   USED(36)         ! Flags header lines as already used
      REAL      VALUE            ! Temporary real value
C
C     Note that this is actually the first ever Figaro routine written!
C     And it's been messed about quite a bit in its time.  As a result,
C     it isn't quite as pristine a piece of code as one might like.  In
C     particular, the way it handles errors is a bit of a mess - some
C     causing messages to be output by this routine, some just setting
C     STATUS and ERROR.  I'd like to tidy that up sometime.
C
C     Initial values
C
      BSCALE=1.
      BZERO=0.
      ERROR=' '
      IEEE_FPT=.FALSE.
      NBAD=0
      MOVED=.FALSE.
C
C     Start up DSA routines
C
      DSA_STATUS=0
      CALL DSA_OPEN(DSA_STATUS)
C
C     Read in the first header record.
C
      IF (LU.EQ.0) THEN
         CALL TIO_READ(MTCHN,BUFSIZ,BUFFER,LENGTH,STATUS)
         IF (TIO_ERR(STATUS)) THEN
            CALL TIO_GETMSG(STATUS,ERROR,ERRLEN)
            GO TO 500       ! Error exit
         END IF
         MOVED=.TRUE.
         DO I=1,LENGTH
            HEADER(I:I)=CHAR(BUFFER(I))
         END DO
      ELSE
         CALL FIG_DFITS_HREAD (HEADER,BUFFER,STATUS)
         IF (STATUS.NE.0) THEN
            ERROR='Error reading first record'
            GO TO 500            ! Error exit
         END IF
         LENGTH=2880
      END IF
C
C     Check for a 'blocked' tape -
C
      IF (MOD(LENGTH,2880).NE.0) THEN
         ERROR='Tape has illegal length records'
         GO TO 500            ! Error exit
      END IF
      CRDMAX=LENGTH/80
      RECLEN=LENGTH
C
C     First, check that the tape is a 'SIMPLE' FITS tape
C
      IF ((HEADER(1:6).NE.'SIMPLE').OR.(HEADER(30:30).NE.'T')) THEN
         CALL PAR_WRUSER(
     :      'Warning: This is not a ''SIMPLE'' FITS image.',STATUS)
         CALL PAR_WRUSER(
     :      'It will probably not be read correctly.',STATUS)
      END IF
C
C     Look for 'BITPIX'.  Also, use this first pass through the
C     header record to set the used flags and look for the
C     'END' card, if any.
C
      BITP=.FALSE.
      IEND=CRDMAX
      IPT=81
      DO I=2,CRDMAX
         IF (HEADER(IPT:IPT+5).EQ.'BITPIX') THEN
            USED(I)=.TRUE.
            ISTAT=ICH_NUMBR(HEADER,IPT+10,' /',VALUE,NEXT)
            IF (ISTAT.NE.0) THEN
               ERROR='Cannot evaluate BITPIX'
               GO TO 500            ! Error exit
            END IF
*            IF (VALUE.LT.0.0) THEN
*               IEEE_FPT=.TRUE.
*               VALUE=-VALUE
*            END IF
            IF (VALUE.LT.0.0) THEN
               BITPIX=VALUE
               WRITE (ERROR,'(A,I10)')
     :                'Cannot read data with BITPIX =',BITPIX
               GO TO 500            ! Error exit
            END IF
            BITPIX=VALUE
            BYTPIX=BITPIX/8
            IF ((BITPIX.NE.8).AND.(BITPIX.NE.16).AND.
     :                             (BITPIX.NE.32)) THEN
               WRITE (ERROR,'(A,I10)')
     :                'Cannot read data with BITPIX =',BITPIX
               GO TO 500            ! Error exit
            END IF
            BITP=.TRUE.
         ELSE IF (HEADER(IPT:IPT+2).EQ.'END') THEN
            IEND=I-1
            GO TO 320
         ELSE
            USED(I)=.FALSE.
         END IF
         IPT=IPT+80
      END DO
C
  320 CONTINUE
      IF (.NOT.BITP) THEN
         ERROR='BITPIX is not specified in header'
         GO TO 500            ! Error exit
      END IF
C
C     Now look for NAXIS
C
      ISTART=2
      IPT=81
      DO I=2,IEND
         IF (.NOT.USED(I)) THEN
            IF (HEADER(IPT:IPT+5).EQ.'NAXIS ') THEN
               USED(I)=.TRUE.
               ISTAT=ICH_NUMBR(HEADER,IPT+10,' /',VALUE,NEXT)
               IF (ISTAT.NE.0) THEN
                  ERROR='Error evaluating NAXIS'
                  GO TO 500            ! Error exit
               END IF
               IF ((VALUE.LT.0.).OR.(VALUE.GT.9.)) THEN
                  WRITE (ERROR,'(A,G13.4)')
     :                  'Cannot handle data with NAXIS = ',VALUE
                  GO TO 500            ! Error exit
               END IF
               NAXIS=VALUE
               GO TO 340
            END IF
         END IF
         IPT=IPT+80
      END DO
C
      ERROR='NAXIS not specified in header'
      GO TO 500            ! Error exit
C
C     Now look for the NAXIS1 .. NAXISn values
C
  340 CONTINUE
      IF (NAXIS.LT.0) THEN
         BYTES=0
      ELSE
         BYTES=BYTPIX
         DO IAX=1,NAXIS
            NAXISN='NAXIS'//CHAR(IAX+ICHAR('0'))
            IPT=81
            DO I=2,IEND
               IF (.NOT.USED(I)) THEN
                  IF (HEADER(IPT:IPT+5).EQ.NAXISN) THEN
                     ISTAT=ICH_NUMBR(HEADER,IPT+10,' /',
     :                                           VALUE,NEXT)
                     IF (ISTAT.NE.0) THEN
                        ERROR='Error evaluating '//NAXISN
                        GO TO 500            ! Error exit
                     END IF
                     DIMS(IAX)=VALUE
                     BYTES=BYTES*DIMS(IAX)
                     USED(I)=.TRUE.
                     GO TO 360
                  END IF
               END IF
               IPT=IPT+80
            END DO
            ERROR=NAXISN//' not specified in header'
            GO TO 500            ! Error exit
  360       CONTINUE
         END DO
      END IF
C
C     A number of tapes are written with high dimensions set to 1.
C     Check for this and ignore such 'dummy' dimensions.
C
      DO IAX=NAXIS,1,-1
         IF (DIMS(IAX).NE.1)  GO TO 370
      END DO
      IAX=1
  370 CONTINUE
      NAXIS=IAX
C
C     Now that it looks as if the tape has a valid header, create
C     the output file.
C
      CALL DSA_NAMED_OUTPUT('OUTPUT',ENAME,' ',0,0,DSA_STATUS)
      IF (DSA_STATUS.NE.0) GO TO 500     ! Error exit
C
C     Put a data array in place so that the output can be written to.
C
      DIMS( 9 ) = 1
      CALL DSA_SIMPLE_OUTPUT( 'OUTPUT', 'D', 'BYTE', 1, DIMS( 9 ),
     :                        STATUS )
      DIMS( 9 ) = 0

C
C     Zero out the axis delta values and label arrays, so we
C     can see later if they get set by any keywords.
C
      DO IAX=1,NAXIS
         CRVALS(IAX)=0.
         CRPIX(IAX)=0.
         CDELTS(IAX)=0.
         LABELS(IAX)=' '
      END DO
C
C     Now run through the unused 'cards' in the current header
C     record.  If necessary, read another header record and
C     carry on until a 'END' is discovered.
C
  380 CONTINUE
      IPT=(ISTART-1)*80+1
      DO I=ISTART,IEND
         IF (.NOT.USED(I)) THEN
C
C           Get object name (this program uses 'object' as a synonym
C           for 'keyword').
C
            ONAME=HEADER(IPT:IPT+7)
            IF ((ONAME.EQ.' ').OR.(ONAME.EQ.'HISTORY').OR.
     :                             (ONAME.EQ.'COMMENT')) THEN
C
C              This is one of the comment keywords.  We write it out
C              as a character keyword, but don't bother trying to split
C              up the record into value and comment.
C
               CALL DSA_PUT_FITS_C ('OUTPUT',ONAME,HEADER(IPT+8:IPT+63),
     :                              ' ',DSA_STATUS)
               IF (DSA_STATUS.NE.0) GO TO 500     ! Error exit
            ELSE
C
C              This is not a comment keyword.  We attempt to classify it
C              by seeing if it is followed by a valid number.  We also
C              look for a comment (delimited by a '/').
C
               IST=IPT+10
               LASTCH=IST+63
               CHR=HEADER(IPT+29:IPT+29)
               COMMENT=' '
               ISLSH=ICH_DELIM(HEADER(:LASTCH),IST+1,'/')
               IF ((ISLSH.GT.0).AND.(ISLSH.LT.LASTCH)) THEN
                  ICMTST=ICH_VERIF(HEADER(:LASTCH),ISLSH+1,' ')
                  IF (ICMTST.GT.0) COMMENT=HEADER(ICMTST:LASTCH)
               END IF
               ISTAT=ICH_NUMBD(HEADER,IST,' /',DVALUE,NSFIG,NEXT)
               IF (ISTAT.EQ.0) THEN
C
C                 Object is numeric.  We see if it is one of the standard
C                 keywords, and if so we put it to its standard use.
C                 Otherwise, we record it in the FITS-specific structure
C                 of the output file.
C
                  KNOWN=.FALSE.
                  IF (ONAME.EQ.'BSCALE') THEN
                     BSCALE=DVALUE
                     KNOWN=.TRUE.
                  ELSE IF (ONAME.EQ.'BZERO') THEN
                     BZERO=DVALUE
                     KNOWN=.TRUE.
                  ELSE IF ((ONAME.EQ.'NAXIS').OR.(ONAME.EQ.'BITPIX'))
     :                                                            THEN
                     KNOWN=.TRUE.
                  ELSE
                     DO IAX=1,NAXIS
                        CHR=CHAR(IAX+ICHAR('0'))
                        IF (ONAME.EQ.'CRVAL'//CHR) THEN
                           CRVALS(IAX)=DVALUE
                           KNOWN=.TRUE.
                           GO TO 385         ! Break IAX loop
                        ELSE IF (ONAME.EQ.'CRPIX'//CHR) THEN
                           CRPIX(IAX)=DVALUE
                           KNOWN=.TRUE.
                           GO TO 385         ! Break IAX loop
                        ELSE IF (ONAME.EQ.'CDELT'//CHR) THEN
                           CDELTS(IAX)=DVALUE
                           KNOWN=.TRUE.
                           GO TO 385         ! Break IAX loop
                        ELSE IF (ONAME.EQ.'NAXIS'//CHR) THEN
                           KNOWN=.TRUE.
                           GO TO 385         ! Break IAX loop
                        END IF
                     END DO
  385                CONTINUE
                  END IF
                  IF (.NOT.KNOWN) THEN
                     IF (NSFIG.GT.7) THEN
                        CALL DSA_PUT_FITS_D ('OUTPUT',ONAME,
     :                                   DVALUE,COMMENT,DSA_STATUS)
                     ELSE
                        CALL DSA_PUT_FITS_F ('OUTPUT',ONAME,
     :                           REAL(DVALUE),COMMENT,DSA_STATUS)
                     END IF
                     IF (DSA_STATUS.NE.0) GO TO 500   ! Error exit
                  END IF
C
               ELSE IF (HEADER(IST:IST).EQ.'''') THEN
C
C                 Object is a character string.  We handle this rather
C                 like the numeric values, checking for a known keyword
C                 (the only character ones we recognise are 'CTYPEn' and
C                 'OBJECT', which we use as the title for the structure)
C                 We may have to redo the comment finding, in case the
C                 delimiting slash we thought we found was inside the
C                 string.
C
                  IENSTR=INDEX(HEADER(IST+1:IST+63),'''')
                  IF (IENSTR.EQ.0) THEN
                     IENSTR=IST+63
                  ELSE
                     IENSTR=IST+IENSTR-1
                  END IF
                  IF (ISLSH.LT.IENSTR) THEN
                     COMMENT=' '
                     IF (IENSTR.LT.LASTCH) THEN
                        ISLSH=ICH_DELIM(HEADER(:LASTCH),IENSTR+1,'/')
                        IF ((ISLSH.GT.0).AND.(ISLSH.LT.LASTCH)) THEN
                           ICMTST=ICH_VERIF(HEADER(:LASTCH),ISLSH+1,' ')
                           IF (ICMTST.GT.0) THEN
                              COMMENT=HEADER(ICMTST:LASTCH)
                           END IF
                        END IF
                     END IF
                  END IF
                  NCH=MAX(1,IENSTR-IST)
                  KNOWN=.FALSE.
                  IF (ONAME.EQ.'OBJECT') THEN
                     KNOWN=.TRUE.
                     CALL DSA_SET_OBJECT ('OUTPUT',HEADER(IST+1:IENSTR),
     :                                                      DSA_STATUS)
                     IF (DSA_STATUS.NE.0) GO TO 500   ! Error exit
                  ELSE
                     DO IAX=1,NAXIS
                        IF (ONAME.EQ.'CTYPE'//CHAR(IAX+ICHAR('0'))) THEN
                           LABELS(IAX)=HEADER(IST+1:IENSTR)
                           KNOWN=.TRUE.
                        END IF
                     END DO
                  END IF
                  IF (.NOT.KNOWN) THEN
                     CALL DSA_PUT_FITS_C ('OUTPUT',ONAME,
     :                       HEADER(IST+1:IENSTR),COMMENT,DSA_STATUS)
                     IF (DSA_STATUS.NE.0) GO TO 500   ! Error exit
                  END IF
C
               ELSE IF ((CHR.EQ.'T').OR.(CHR.EQ.'F')) THEN
C
C                 Object is a logical quantity.  There are only two
C                 standard logical quantities, 'SIMPLE' and 'BLOCKED'.
C                 We know it won't be SIMPLE because that has to be in
C                 the first record and we aren't looking at that any more
C                 (Since ISTART will be 2).
C
                  IF (ONAME.NE.'BLOCKED') THEN
                     CALL DSA_PUT_FITS_L ('OUTPUT',ONAME,CHR.EQ.'T',
     :                                            COMMENT,DSA_STATUS)
                     IF (DSA_STATUS.NE.0) GO TO 500    ! Error exit
                  END IF
               END IF
            END IF
         END IF
         IPT=IPT+80
      END DO
C
C     Do we have to read in a new record?
C
      IF (IEND.EQ.CRDMAX) THEN
         IF (LU.EQ.0) THEN
            CALL TIO_READ(MTCHN,BUFSIZ,BUFFER,LENGTH,STATUS)
            IF (TIO_ERR(STATUS)) THEN
               CALL TIO_GETMSG(STATUS,ERROR,ERRLEN)
               IF (TIO_EOF(STATUS)) MOVED=.FALSE.
               GO TO 500            ! Error exit
            END IF
            DO I=1,LENGTH
               HEADER(I:I)=CHAR(BUFFER(I))
            END DO
         ELSE
            CALL FIG_DFITS_HREAD (HEADER,BUFFER,STATUS)
            IF (STATUS.NE.0) THEN
               ERROR='I/O error reading extra header record'
               GO TO 500            ! Error exit
            END IF
         END IF
C
C        Look quickly through this new record for an 'END'
C        record.
C
         IPT=1
         ISTART=1
         IEND=CRDMAX
         DO I=1,CRDMAX
            USED(I)=.FALSE.
            IF (HEADER(IPT:IPT+2).EQ.'END') THEN
               IEND=I-1
               GO TO 390
            END IF
            IPT=IPT+80
         END DO
  390    CONTINUE
C
C        Go back and process this new record
C
         GO TO 380
      END IF
C
C     Now we've dealt with the header, and know all we're going to know
C     about the data array, we can decide what format to use for it.
C     We set TYPE to the output type for the array.
C
      IF (FLOAT) THEN
         TYPE='FLOAT'
      ELSE
C
C        We've been asked not to use float.  See if we can comply.
C
         IF ((BITPIX.EQ.16).AND.(BSCALE.EQ.1.0).AND.(BZERO.EQ.32768.0))
     :                                                             THEN
            TYPE='USHORT'
         ELSE
            IF ((BSCALE.NE.1.0).AND.(BZERO.NE.0.0)) THEN
               TYPE='FLOAT'
            ELSE
               IF (BITPIX.EQ.8) THEN
                  TYPE='BYTE'
               ELSE IF (BITPIX.EQ.16) THEN
                  TYPE='SHORT'
               ELSE
                  TYPE='INT'
               END IF
            END IF
         END IF
         IF (TYPE.EQ.'FLOAT') THEN
            CALL DSA_WRUSER (
     :        'The scaling and offset values in the input data are ')
            CALL DSA_WRUSER (
     :        'such that floating point will have to be used for the ')
            CALL DSA_WRUSER ('output data.')
            CALL DSA_WRFLUSH
         END IF
         IF (IEEE_FPT) THEN
            CALL DSA_WRUSER ('The input data is floating point data.')
            CALL DSA_WRUSER
     :             ('Floating point will have to be used for the '//
     :                                               'output data')
            CALL DSA_WRFLUSH
            TYPE='FLOAT'
         END IF
      END IF
C
C     If we are going to use floating point data, then we will have
C     to convert from the source format, so we need to work out
C     what that is. (We treat IEEE floating point data separately.)
C
      IF (TYPE.EQ.'FLOAT') THEN
         IF (BYTPIX.EQ.2) THEN
            SRCFMT=WORD
         ELSE IF (BYTPIX.EQ.4) THEN
            SRCFMT=LWORD
         ELSE
            SRCFMT=UBYTE
         END IF
      END IF
C
C     Now we create the data array in the output structure, and map it.
C
      IF (NAXIS.GT.0) THEN
         CALL DSA_COERCE_DATA_ARRAY ('OUTPUT',TYPE,NAXIS,DIMS,
     :                               DSA_STATUS)
         CALL DSA_MAP_DATA ('OUTPUT','WRITE',TYPE,OPTR,SLOT,DSA_STATUS)
         IF (DSA_STATUS.NE.0) GO TO 500       ! Error exit
      END IF
C
C     Right.  By this stage we have the whole structure set up
C     and all we have to do now is read in the data records.
C
      PIXELS=BYTES/BYTPIX
      PIXREC=LENGTH/BYTPIX
C
C     When we come out of the header processing loop, IEND+1 is the
C     number of the 'card' containing the END statement.  If there may
C     be data in this physical record, see where it might start and
C     if it is actually there.
C
      IF (BLOCK.EQ.0) THEN
         PIXRC1=0
      ELSE
         STBYTE=((IEND/36)+1)*2880+1
         IF (STBYTE.GT.RECLEN) THEN
            PIXRC1=0
         ELSE
C
C           This is the case where some data (PIXRC1 pixels) is in
C           the record we read for the header info).  We now write it
C           into the mapped output array.
C
            IF (BITPIX.EQ.8) THEN
               PIXRC1=RECLEN-STBYTE+1
            ELSE IF (BITPIX.EQ.16) THEN
               PIXRC1=(RECLEN-STBYTE+1)/2
               CALL GEN_BSWAP(BUFFER(STBYTE),PIXRC1)
            ELSE IF (BITPIX.EQ.32) THEN
               PIXRC1=(RECLEN-STBYTE+1)/4
               IF (.NOT.IEEE_FPT) THEN
                  CALL GEN_WBSWAP(BUFFER(STBYTE),PIXRC1)
               END IF
            END IF
            ITEMS=MIN(PIXELS,PIXRC1)
            IF (TYPE.NE.'FLOAT') THEN
               BYTES=ITEMS*(BITPIX/8)
               CALL GEN_MOVE (BYTES,BUFFER(STBYTE),%VAL(CNF_PVAL(OPTR)))
               IF (TYPE.EQ.'USHORT') THEN
                  CALL GEN_SFLIP(%VAL(CNF_PVAL(OPTR)),ITEMS)
               END IF
            ELSE
*               IF (IEEE_FPT) THEN
*                  CALL GEN_IEEETOR(BUFFER(STBYTE),%VAL(CNF_PVAL(OPTR)),
*    :                              ITEMS,COUNT)
*                  NBAD=NBAD+COUNT
*               ELSE
                  CALL DSA_FMTCON(.FALSE.,SRCFMT,REALF,BUFFER(STBYTE),
     :                            %VAL(CNF_PVAL(OPTR)),ITEMS,IGNORE)
                  CALL GEN_MULCAF(%VAL(CNF_PVAL(OPTR)),ITEMS,
     :                            REAL(BSCALE),%VAL(CNF_PVAL(OPTR)))
                  CALL GEN_ADDCAF(%VAL(CNF_PVAL(OPTR)),ITEMS,
     :                            REAL(BZERO),%VAL(CNF_PVAL(OPTR)))
*               END IF
            END IF
            PIXELS=PIXELS-PIXRC1
            CALL DYN_INCAD(OPTR,TYPE,ITEMS,TPTR,ISNEW,STATUS)
            IF (ISNEW) CALL CNF_UNREGP(OPTR)
            OPTR = TPTR
         END IF
      END IF
C
C     So long as there are pixels left to be read in, read in
C     a record and work out which pixel of the data it starts
C     with
C
      PISNEW = .FALSE.
      IREC=0
      DO WHILE (PIXELS.GT.0)
         IF (LU.EQ.0) THEN
            CALL TIO_READ(MTCHN,BUFSIZ,BUFFER,LENGTH,STATUS)
            IF (TIO_ERR(STATUS)) THEN
               IF (TIO_EOF(STATUS)) MOVED=.FALSE.
               CALL TIO_GETMSG(STATUS,ERROR,ERRLEN)
               GO TO 500            ! Error exit
            END IF
         ELSE
            CALL FIG_DFITS_READ (BUFFER,STATUS)
            IF (STATUS.NE.0) THEN
               ERROR='I/O error reading data record'
               GO TO 500            ! Error exit
            END IF
         END IF
C
C        Byte swop buffer.
C
         IF (SWAP) THEN
            IF (BITPIX.EQ.16) THEN
               CALL GEN_BSWAP(BUFFER,LENGTH/2)
            END IF
            IF (BITPIX.EQ.32) THEN
               IF (.NOT.IEEE_FPT) THEN
                  CALL GEN_WBSWAP(BUFFER,LENGTH/4)
               END IF
            END IF
         END IF
         IREC=IREC+1
         ITEMS=MIN(PIXELS,PIXREC)
C
C        If data is not to be floated, it can be written directly
C        to the data object.  Otherwise, it must be converted first.
C
         IF (TYPE.NE.'FLOAT') THEN
            BYTES=ITEMS*(BITPIX/8)
            CALL GEN_MOVE (BYTES,BUFFER,%VAL(CNF_PVAL(OPTR)))
            IF (TYPE.EQ.'USHORT') THEN
               CALL GEN_SFLIP(%VAL(CNF_PVAL(OPTR)),ITEMS)
            END IF
         ELSE
*            IF (IEEE_FPT) THEN
*               CALL GEN_IEEETOR(BUFFER,%VAL(CNF_PVAL(OPTR)),ITEMS,
*    :                           COUNT)
*               NBAD=NBAD+COUNT
*            ELSE
               CALL DSA_FMTCON(.FALSE.,SRCFMT,REALF,BUFFER,
     :                         %VAL(CNF_PVAL(OPTR)),ITEMS,IGNORE)
               CALL GEN_MULCAF(%VAL(CNF_PVAL(OPTR)),ITEMS,REAL(BSCALE),
     :                         %VAL(CNF_PVAL(OPTR)))
               CALL GEN_ADDCAF(%VAL(CNF_PVAL(OPTR)),ITEMS,REAL(BZERO),
     :                         %VAL(CNF_PVAL(OPTR)))
*            END IF
         END IF
         PIXELS=PIXELS-PIXREC
         CALL DYN_INCAD(OPTR,TYPE,ITEMS,TPTR,ISNEW,STATUS)
         IF (PISNEW) CALL CNF_UNREGP(OPTR)
         OPTR = TPTR
         PISNEW = ISNEW
      END DO
      IF (ISNEW) CALL CNF_UNREGP(OPTR)
C
C     If there were any bad conversions from IEEE floating point there will be
C     flagged error values in the data. Indicate this.
C
      IF (NBAD.GT.0) THEN
         CALL DSA_SET_FLAGGED_VALUES('OUTPUT',.TRUE.,DSA_STATUS)
         IF (DSA_STATUS.NE.0) GO TO 500       ! Error exit
      END IF
C
C     Now that we've got the main data, see about the axis structures.
C
      DO IAX=1,NAXIS
         IF (LABELS(IAX).NE.' ') THEN
            CHARS(1)=' '
            CHARS(2)=LABELS(IAX)
            CALL DSA_SET_AXIS_INFO ('OUTPUT',IAX,2,CHARS,0,DVALUE,
     :                              DSA_STATUS)
            IF (DSA_STATUS.NE.0) GO TO 500     ! Error exit
         END IF
         IF (CDELTS(IAX).NE.0.) THEN
            CALL DSA_MAP_AXIS_DATA ('OUTPUT',IAX,'WRITE','FLOAT',
     :                              DPTR,SLOT,DSA_STATUS)
            IF (DSA_STATUS.NE.0) GO TO 500     ! Error exit
            COEFF(1)=CDELTS(IAX)
            COEFF(2)=CRVALS(IAX)-CRPIX(IAX)*CDELTS(IAX)
            CALL FIG_WAVSET(DIMS(IAX),COEFF,2,%VAL(CNF_PVAL(DPTR)))
            CALL DSA_UNMAP (SLOT,DSA_STATUS)
            IF (DSA_STATUS.NE.0) GO TO 500     ! Error exit
         END IF
      END DO
C
C     Exit.  Close down DSA and set status code. If the tape has moved,
C     and has not already hit a file mark, skip to the end of the current file.
C
  500 CONTINUE
C
C     Move on to the next file mark (on tape) if tape has been moved.
C
      IF (MOVED) THEN
         CALL TIO_SKIP(MTCHN,1,STATUS)
         IF (TIO_ERR(STATUS)) THEN
            CALL TIO_GETMSG(STATUS,ERROR,ERRLEN)
         END IF
      END IF
      CALL DSA_CLOSE(DSA_STATUS)
      STATUS=0
      IF (DSA_STATUS.NE.0) STATUS=1
      IF (ERROR.NE.' ') STATUS=1
C
      END
