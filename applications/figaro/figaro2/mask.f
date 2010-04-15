C+
      SUBROUTINE MASK
C
C     M A S K
C
C     Generates a spectral mask given a spectrum and a mask
C     table file.  The mask table contains a set of central
C     wavelengths and number of anstroms covered, one for each
C     part of the spectrum to be masked.  The program generates
C     a spectrum covering the same wavelength range as the
C     original spectrum, with the masked areas set to the same
C     values as the original spectrum in those areas, and the
C     unmasked areas set to zero.
C
C     Command parameters -
C
C     SPECTRUM   (Character) The spectrum to be used
C     TABLE      (Character) The mask table file to be used - if
C                the file has no extension, .MSK will be assumed.
C                The program searches for the mask file in the
C                standard Figaro search path for such files.
C     MASK       (Character) The output mask name.
C
C     User variables used - None
C
C                                          KS / CIT 4th April 1984
C     Modified:
C
C     Summer 1987    DJA/AAO.  Rewritten to use DSA routines.
C     7th  Sep 1988  KS/AAO. Comments about search path for table
C                    modified.
C     29th Sep 1992  HME / UoE, Starlink.  Lowercase extension .msk.
C                    INCLUDE changed, TABs removed. Map output data for
C                    update access instead of write.
C     18 Jul 1996    MJCL / Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Maximum number of masked areas
C
      INTEGER MAXWAVES
      PARAMETER (MAXWAVES=100)
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      DOUBLE PRECISION DUMMY     ! Dummy flag for magnitude
      LOGICAL      FAULT         ! TRUE if there was an error reading
                                 ! mask table
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      MASKED        ! Actual number of masked areas
      INTEGER      MPTR          ! Dynamic-memory pointer to mask data
                                 ! array
      INTEGER      MSLOT         ! Map slot number of mask data array
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NWAVES        !
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    TABLE*132     ! The name of the mask table file
      INTEGER      TABLU         ! Table logical unit number
      LOGICAL      TBOPEN        ! Mask table file was opened OK?
      CHARACTER    TUNITS*16     ! Units of mask table values
      CHARACTER    UNITS*16      ! Units of input spectrum
      REAL     WAVES(2,MAXWAVES) !
      INTEGER      WPTR          ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT         ! Map slot number of workspace
      INTEGER      XPTR          ! Dynamic-memory pointer to x-axis data
                                 ! array
      INTEGER      XSLOT         ! Map slot number of x-axis data array
C
C     Logical unit for mask table
C
      DATA TABLU/2/
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of spectrum and open it.
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get dimensions
C
      CALL DSA_DATA_SIZE('SPECT',1,NDIM,DIMS,NX,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get name of mask table and open it
C
      CALL PAR_RDCHAR('TABLE',' ',TABLE)
      CALL FIG_OPFILE(TABLE,'msk',TABLU,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to open mask file',IGNORE)
         GO TO 500
      END IF
      TBOPEN=.TRUE.
C
C     Read table
C
      CALL FIG_MSKRD(TABLU,MAXWAVES,WAVES,NWAVES,TUNITS,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of mask file, and open it
C
      CALL DSA_OUTPUT('MSK','MASK','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map the mask data
C
      CALL DSA_MAP_DATA('MSK','UPDATE','FLOAT',MPTR,MSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Check on the units and map values for the X data - note: we don't
C     want to insist on wavelength values, in case someone wants
C     to use this routine for something sneaky.  However, if there
C     are no X values at all, we won't go so far as to generate
C     dummy ones.
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',XPTR,XSLOT,STATUS)
      CALL DSA_GET_AXIS_INFO('MSK',1,1,UNITS,0,DUMMY,STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL ICH_FOLD(TUNITS)
      IF (UNITS.GT.' ') THEN
         CALL ICH_FOLD(UNITS)
         IF (UNITS.NE.TUNITS) THEN
            CALL PAR_WRUSER('Warning - table units are '//
     :              TUNITS(:ICH_LEN(TUNITS))//' and spectrum uses '//
     :              UNITS(:ICH_LEN(UNITS)),IGNORE)
         END IF
      ELSE
         CALL PAR_WRUSER(
     :   'Warning - no X units are specified for the spectrum',IGNORE)
      END IF
C
C     Get workspace for FIG_GMASK
C
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',WPTR,WSLOT,STATUS)
C
C     Blank out the unmasked parts of the spectrum.  (This is where
C     the real work gets done.)
C
      CALL FIG_GMASK(NX,%VAL(CNF_PVAL(XPTR)),NWAVES,WAVES,
     :               %VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(MPTR)),MASKED)
      IF (MASKED.LE.0) THEN
         CALL PAR_WRUSER('Warning - No spectral ranges masked.',IGNORE)
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      IF (TBOPEN) THEN
         CLOSE (UNIT=TABLU,IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error closing mask table file',IGNORE)
            FAULT=.TRUE.
         END IF
      END IF
C
      CALL DSA_CLOSE(STATUS)
C
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_MSKRD(TABLU,MAXWAVES,WAVES,NWAVES,TUNITS,STATUS)
C
C     F I G _ M S K R D
C
C     Reads the mask table file, getting the wavelength ranges for
C     the sections to be masked out.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) TABLU    (Integer) Logical unit the tale file is open on.
C     (>) MAXWAVES (Integer) Maximum number of sections allowed.
C     (<) WAVES    (Real array WAVES(2,MAXWAVES)) Limits for each
C                  section.  WAVES(1,n) holds the starting and
C                  WAVES(2,n) the ending wavelengths for the masked
C                  sections.
C     (<) NWAVES   (Integer) The number of masked sections.
C     (<) TUNITS   (Character) The units used in the table file
C                  (one expects these will be Angstroms, but who knows?)
C     (<) STATUS   (Integer) Return status code.  0 => OK,
C                  1 => some error, either format or I/O.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     ICH_LEN    (ICH_ package) Length of string, ignoring final blanks
C     ICH_NUMGT  ( "     "    ) Decode set of free-format numbers
C     ICH_VERIF  ( "     "    ) Find first char not in given list
C     PAR_WRUSER (PAR_   "    ) Send string to user
C
C     File format -
C
C     The table file read by this routine has one record for each
C     section to be masked, giving the central wavelength and then
C     the wavelength range, in free format.  Blank records, and
C     records whose first non-blank character is a '*' are treated
C     as comments.  The first non-comment record should give the
C     units used - the units are just the first non-blank characters
C     of that record.  The records do not have to be in increasing
C     order of wavelength, and may even overlap.
C
C                                          KS / CIT 5th April 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER TABLU,MAXWAVES,NWAVES,STATUS
      REAL    WAVES(2,MAXWAVES)
      CHARACTER*(*) TUNITS
C
C     Functions
C
      INTEGER ICH_LEN, ICH_NUMGT, ICH_VERIF
C
C     Local variables
C
      LOGICAL FIRST, MORE
      INTEGER ICH, ICODES(2), INVOKE, NEXT, NSTAT
      REAL    VALUES(2)
      CHARACTER*64 RECORD
C
C     Loop through the records.
C
      STATUS=0
      MORE=.TRUE.
      FIRST=.TRUE.
      DO WHILE (MORE)
         READ (TABLU,'(A)',IOSTAT=NSTAT) RECORD
C
C        Check for I/O error or end of file
C
         MORE=NSTAT.EQ.0
         IF (NSTAT.GT.0) THEN
            CALL PAR_WRUSER('I/O error from table file',NSTAT)
            STATUS=1
         END IF
C
C        Check for comment record
C
         IF (MORE) THEN
            ICH=MIN(1,ICH_VERIF(RECORD,1,' '))
            IF ((RECORD(ICH:ICH).NE.'*').AND.(RECORD.NE.' ')) THEN
C
C              If first non-comment record, assume this gives
C              units.
C
               IF (FIRST) THEN
                  FIRST=.FALSE.
                  NWAVES=0
                  TUNITS=RECORD(ICH:)
               ELSE
C
C                 Other records should contain a central wavelength
C                 and a range.
C
                  INVOKE=ICH_NUMGT(RECORD,1,' ,','*;',2,VALUES,ICODES,
     :                                                             NEXT)
                  IF ((ICODES(1).NE.0).OR.(ICODES(2).NE.0)) THEN
                     STATUS=1
                     CALL PAR_WRUSER('Error in mask file format',NSTAT)
                     CALL PAR_WRUSER(RECORD(:ICH_LEN(RECORD)),NSTAT)
                     MORE=.FALSE.
                  ELSE
                     NWAVES=NWAVES+1
                     IF (NWAVES.GT.MAXWAVES) THEN
                        CALL PAR_WRUSER(
     :                       'File contains too many regions',STATUS)
                        MORE=.FALSE.
                     ELSE
                        WAVES(1,NWAVES)=VALUES(1)-VALUES(2)*.5
                        WAVES(2,NWAVES)=WAVES(1,NWAVES)+VALUES(2)
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END DO
C
      END
C+
      SUBROUTINE FIG_GMASK(NX,XDATA,NWAVES,WAVES,WORK,DATA,MASKED)
C
C     F I G _ G M A S K
C
C     Generates a mask spectrum, by setting to zero those parts
C     of a spectrum that are not included in a set of regions to
C     be masked.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) NX     (Integer) The number of elements in the spectrum
C     (>) XDATA  (Real array XDATA(NX)) The X-values for the centers
C                of the pixels of DATA.  (Normally, these will be the
C                wavelength values.)
C     (>) NWAVES (Integer) The number of sections to be masked.
C     (>) WAVES  (Real array WAVES(2,MAXWAVES)) Limits for each
C                section.  WAVES(1,n) holds the starting and
C                WAVES(2,n) the ending wavelengths for the masked
C                sections.
C     (W) WORK   (Real array WORK(NX)) Workspace.
C     (!) DATA   (Real array DATA(NX)) The spectrum.
C     (<) MASKED (Integer) The number of sections actually masked.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     GEN_BSEARCH   (GEN_ package) Binary search through a real array
C     GEN_FILL      ( "     "    ) Fill an array with a byte value
C     GEN_REV2D     ( "     "    ) Reverse the elements in an array
C     ICH_ENCODE    (ICH_   "    ) Encode value into character string
C     PAR_WRUSER    (PAR_   "    ) Output message to user
C
C                                            KS / CIT 5th April 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NWAVES, MASKED
      REAL XDATA(NX), WAVES(2,NWAVES), WORK(NX), DATA(NX)
C
C     Functions
C
      INTEGER GEN_BSEARCH, ICH_ENCODE
C
C     Local variables
C
      LOGICAL MORE, REV, SAME
      INTEGER I, INVOKE, IPTR, MASK, NEXT, STATUS
      REAL    FINISH, START, VMAX, VMIN, WMAX, WMIN
      CHARACTER MESSAGE*64
C
C     Just on the off-chance that the spectrum is in descending
C     order in X, check for that and reverse it if necessary.
C
      SAME=.TRUE.
      REV=XDATA(1).GT.XDATA(NX)
      IF (REV) THEN
         CALL GEN_REV2D(XDATA,NX,1,SAME,XDATA)
         CALL GEN_REV2D(DATA,NX,1,SAME,DATA)
      END IF
C
C     Zero out work array
C
      CALL GEN_FILL(NX*4,0,WORK)
C
C     Get x-limits for spectrum, then loop through all the
C     masked areas.
C
      VMIN=XDATA(1)
      VMAX=XDATA(NX)
      MASKED=0
      DO MASK=1,NWAVES
         WMIN=WAVES(1,MASK)
         WMAX=WAVES(2,MASK)
         IF ((VMIN.LE.WMIN).AND.(VMAX.GE.WMAX)) THEN
            MASKED=MASKED+1
            IPTR=MAX(1,GEN_BSEARCH(XDATA,NX,WMIN))
            START=XDATA(IPTR)
            MORE=.TRUE.
            DO WHILE(MORE)
               WORK(IPTR)=1.
               IPTR=IPTR+1
               IF (IPTR.GT.NX) THEN
                  MORE=.FALSE.
               ELSE
                  MORE=XDATA(IPTR).LE.WMAX
               END IF
            END DO
            FINISH=XDATA(IPTR-1)
            MESSAGE='Masked from '
            INVOKE=ICH_ENCODE(MESSAGE,START,13,3,NEXT)
            MESSAGE(NEXT:)=' to '
            INVOKE=ICH_ENCODE(MESSAGE,FINISH,NEXT+4,3,NEXT)
            CALL PAR_WRUSER(MESSAGE(:NEXT-1),STATUS)
         END IF
      END DO
C
C     Modify DATA according to the mask now held in WORK
C
      DO I=1,NX
         IF (WORK(I).EQ.0) DATA(I)=0.
      END DO
C
C     If we had to reverse the data, reverse it back the way it was
C
      IF (REV) THEN
         CALL GEN_REV2D(XDATA,NX,1,SAME,XDATA)
         CALL GEN_REV2D(DATA,NX,1,SAME,DATA)
      END IF
C
      END
