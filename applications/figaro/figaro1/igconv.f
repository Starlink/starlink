C+
      SUBROUTINE IGCONV
C
C     I G C O N V
C
C     This is a general-purpose convolution program, able to convolve
C     the main data array in a Figaro file with a rectangular array of
C     any dimensions. This is a more flexible routine than ICONV3, where
C     the convolution is with a 3x3 array specified only by a center and
C     an edge value, but the additional flexibility probably makes it
C     noticeably slower, particularly for very large convolution arrays.
C
C     Command parameters -
C
C     IMAGE    The name of the structure containing the image.
C
C     CONVOL   This is the name of a text file that contains the
C              definition of the convolution array. See below for the
C              format.
C
C     OUTPUT   The name of the result of the operation.  This can
C              be the same as for IMAGE.  If not, a new structure
C              is created, with everything but the data a direct
C              copy of the input.
C
C     File format:
C        The text file defining the convolution array should simply
C     contain the values for the array in a free format, one line at a
C     time. Lines beginning with a '*' are ignored, as are blank lines.
C     If the data for a line of the array has to be continued onto
C     another line, a '\' should appear at the end of the line to be
C     continued.
C-
C     History:
C
C     11th Mar 1994  Original version.  KS / AAO
C     18th Jul 1996  MJCL / Starlink, UCL.  Set variables for storage
C                    of file names to 132 chars.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE, ICH_LEN
C
C     Local variables
C
      INTEGER      BPTR          ! Dynamic-memory pointer to box data
      INTEGER      BSLOT         ! Map slot number of box data
      CHARACTER    CONVOL*132    ! Name of convolution definition file
      INTEGER      DIMS(2)       ! Sizes of dimensions of data
      INTEGER      DUMMY         ! Dummy data array on first pass
                                 ! through file
      INTEGER      FNBX          ! X-dimension of box data from 1st pass
      INTEGER      FNBY          ! Y-dimension of box data from 1st pass
      INTEGER      LU            ! Logical unit number for file
      CHARACTER    NAME*256      ! Full name of convolution file
      INTEGER      NBX           ! X-dimension of box data
      INTEGER      NBY           ! Y_dimension of box data
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number output data array
      INTEGER      STATUS        ! Running status for DSA_ routines
      INTEGER      WPTR          ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT         ! Map slot number of workspace
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      IF (NDIM.LT.2) THEN
         NY=1
      ELSE
         NY=DIMS(2)
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     Get the name of the convolution definition file and open it.
C
      CALL PAR_RDCHAR('CONVOL',' ',CONVOL)
      CALL DSA_OPEN_TEXT_FILE (CONVOL,'.cnv','OLD',.FALSE.,LU,NAME,
     :                                                        STATUS)
      IF (STATUS.NE.0) GOTO 500       ! Error exit.
      CALL PAR_WRUSER (' ',STATUS)
      CALL PAR_WRUSER ('Reading convolution array from "'//
     :                          NAME(:ICH_LEN(NAME))//'"',STATUS)
      CALL PAR_WRUSER (' ',STATUS)
C
C     Read the file twice, once to get the array size, then allocate
C     suitable workspace and read in the data.
C
      NBX=1
      NBY=1
      CALL GCONV_READ (LU,.FALSE.,NBX,NBY,DUMMY,FNBX,FNBY,STATUS)
      IF (STATUS.NE.0) GOTO 500       ! Error exit.
      NBX=FNBX
      NBY=FNBY
      IF ((MOD(NBX,2).NE.1).OR.(MOD(NBY,2).NE.1)) THEN
         CALL PAR_WRUSER(
     :     'Note: ideally, a convolution array has odd dimensions',
     :                                                      STATUS)
      END IF
      CALL DSA_GET_WORK_ARRAY (NBX*NBY,'FLOAT',BPTR,BSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500       ! Error exit.
      CALL GCONV_READ (LU,.TRUE.,NBX,NBY,%VAL(CNF_PVAL(BPTR)),
     :                 FNBX,FNBY,STATUS)
      IF (STATUS.NE.0) GOTO 500       ! Error exit.
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C     Map data
C
      CALL DSA_MATCH_SIZES('IMAGE','OUTPUT',STATUS)
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
C
C     GEN_GCONV can't work in situ, so we need a copy of the data array.
C     (We could have mapped the input array and tested to see if the
C     output file is the same and only got workspace if it was, but
C     it's simpler like this, if marginally less efficient).
C
      CALL DSA_GET_WORK_ARRAY(NX*NY,'FLOAT',WPTR,WSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL GEN_MOVE (NX*NY*DSA_TYPESIZE('FLOAT',STATUS),
     :               %VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(WPTR)))
      IF (STATUS.NE.0) GOTO 500
C
C     Pass the filter through the data.
C
      CALL GEN_ICONV (%VAL(CNF_PVAL(WPTR)),NX,NY,%VAL(CNF_PVAL(BPTR)),
     :                NBX,NBY,%VAL(CNF_PVAL(OPTR)))
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE GCONV_READ (LU,READIN,NBX,NBY,BOX,NFBX,NFBY,STATUS)
C
C     G C O N V _ R E A D
C
C     Utility routine for the Figaro application GCONV. This routine
C     reads the convolution filter file. With READIN false, it does a
C     pass through the file and returns FNBX and FNBY to the size of the
C     filter defined in the file. With READIN true it reads the filter
C     data into the array passed as BOX.
C
C     Parameters:  (">" input, "<" output)
C
C     (>) LU     (Integer) The logical unit on which the file is open.
C     (>) READIN (Logical) True if the data is actually to be read.
C     (>) NBX    (Integer) The X-dimension of the array BOX.
C     (>) NBY    (Integer) The Y-dimension of the array BOX.
C     (<) BOX    (Real array BOX(NBX,NBY)) The data in the file.
C     (<) NFBX   (Integer) The X-dimension of the filter as deduced from
C                the contents of the file.
C     (<) NFBY   (Integer) The Y-dimension of the filter as deduced from
C                the contents of the file.
C     (<) STATUS (Integer) Status of the read. Zero => OK.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL READIN
      INTEGER LU,NBX,NBY,NFBX,NFBY,STATUS
      REAL BOX(NBX,NBY)
C
C     Local variables
C
      CHARACTER BACKSLASH*1     ! The backslash character
      LOGICAL   EOF             ! Indicates end of file reached (or error)
      INTEGER   FSTAT           ! I/O status variable.
      INTEGER   IBX             ! X-index for next data value
      INTEGER   IBY             ! Y-index for next data value
      INTEGER   IGNORE          ! Status value we don't care about
      INTEGER   INVOKE          ! Dummy function value
      INTEGER   IST             ! Next character in LINE to process
      INTEGER   ISTAT           ! Status of decode from ICH_NUMBR
      CHARACTER LINE*256        ! Line read from file
      LOGICAL   MORE            ! Indicates more number to process in line
      LOGICAL   NEWLINE         ! True if data ended for current line
      INTEGER   NEXT            ! Position after delimiting character in LINE
      REAL      VALUE           ! Value decoded from line
C
C     Functions used
C
      INTEGER ICH_CLEAN, ICH_NUMBR, ICH_LEN, ICH_VERIF
C
C     Some compilers don't like the backslash character. Defining it in this
C     way avoids that problem, at the expense of assuming an ASCII character
C     set.
C
      BACKSLASH=CHAR(92)
C
C     Rewind the file and start to read through it.
C
      IBX=1
      IBY=1
      STATUS=1
      REWIND (LU,IOSTAT=IGNORE)
      EOF=.FALSE.
      DO WHILE (.NOT.EOF)
         READ (LU,'(A)',IOSTAT=FSTAT) LINE
         INVOKE=ICH_CLEAN(LINE)
         IF (FSTAT.LT.0) THEN
            EOF=.TRUE.
            STATUS=0
            NFBY=IBY-1
         ELSE IF (FSTAT.GT.0) THEN
            CALL PAR_WRUSER(
     :         'I/O error reading from filter definition file',IGNORE)
            CALL GEN_FORTERR (FSTAT,.FALSE.,LINE)
            CALL PAR_WRUSER (LINE(:ICH_LEN(LINE)))
            EOF=.TRUE.
         ELSE
            IST=ICH_VERIF(LINE,1,' ')
            IF (IST.GT.0) THEN
               IF (LINE(IST:IST).NE.'*') THEN
C
C                 This is a data line - not a comment or a blank line.
C                 Work through it looking for numbers.
C
                  MORE=.TRUE.
                  DO WHILE (MORE)
                     ISTAT=ICH_NUMBR(LINE,IST,
     :                                       BACKSLASH//', ',VALUE,NEXT)
                     IF (ISTAT.NE.0) THEN
                        CALL PAR_WRUSER('Error decoding numbers from '//
     :                                  'filter definition file',IGNORE)
                        CALL PAR_WRUSER('"'//LINE(IST:ICH_LEN(LINE))//
     :                                            '" is invalid',IGNORE)
                        IF (ISTAT.LT.0) THEN
                           CALL PAR_WRUSER('(contains a null value)',
     :                                                           IGNORE)
                        END IF
                        GO TO 500            ! Error exit
                     ELSE
                        IF (READIN) BOX(IBX,IBY)=VALUE
                        NEWLINE=.TRUE.
                        IF (NEXT.EQ.0) THEN
                           MORE=.FALSE.
                        ELSE
                           IST=ICH_VERIF(LINE,NEXT,' ')
                           IF (IST.EQ.0) THEN
                              MORE=.FALSE.
                           ELSE IF (LINE(IST:IST).EQ.BACKSLASH) THEN
                              MORE=.FALSE.
                              NEWLINE=.FALSE.
                           END IF
                        END IF
                        IF (.NOT.MORE) THEN
                           IF (IBY.EQ.1) THEN
                              NFBX=IBX
                           ELSE IF (NFBX.NE.IBX) THEN
                              CALL PAR_WRUSER (
     :                                    'Filter definition file'//
     :                          ' has more numbers for some lines'//
     :                          ' than for others',IGNORE)
                              GOTO 500  ! Error exit
                           END IF
                           IF (NEWLINE) THEN
                              IBY=IBY+1
                              IBX=1
                           ELSE
                              IBX=IBX+1
                           END IF
                           MORE=.FALSE.
                        ELSE
                           IBX=IBX+1
                           IST=NEXT
                        END IF
                     END IF
                  END DO
               END IF
            END IF
         END IF
      END DO
C
  500 CONTINUE
C
      END

