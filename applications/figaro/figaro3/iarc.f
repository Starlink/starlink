C+
      SUBROUTINE IARC
C
C     I A R C
C
C     Performs a 2D fit to an arc spectrum, given an initial fit
C     to a single arc spectrum as a starting point.  Generally, the
C     starting spectrum will have been extracted from the center of the
C     2D arc.  IARC then starts at a suitable cross-section in the
C     2D arc - usually the central one - and works out from there,
C     fitting each cross-section individually, looking for the lines in
C     the starting spectrum.  For each line, the program looks for a
C     peak close to where it was found in the previous fit.  If a line
C     does not show up in one cross-section, the previous position will
C     be used, but if it fails to show up in the next cross-section, it
C     will be dropped from the search list.  In many cases, there may be
C     strong lines which were not indentified; these cannot be used to
C     improve the fit, but they can be used to 'lock' it down in the
C     regions where there are few or no identified lines.
C
C     Command parameters -
C
C     IMAGE     (Character) The name of the image containing the 2D arc.
C     RSTART    (Numeric) The starting cross-section to be used.
C     RWIDTH    (Numeric) The number of cross-sections to be added
C               together for each fit - if the arc is weak, this will
C               need to be increased.
C     RSIGMA    (Numeric) Normally, the sigma value from the arc line
C               file is used, but this can be overidden by RSIGMA.
C     GAP       (Numeric) Number of cross sections over which IARC can
C               fail to find a line before deleting it from the lists.
C     FILE      (Character) The name of the file to which the results
C               are to be written.  If an extension is not given, .IAR
C               will be used.
C     SIGMIN    (Numeric) The minimum acceptable value for the sigma of
C               an arc line found in the locking process.  Sigma here is
C               the height of the line relative to the square root of
C               the continuum.  Only used if LOCK specified.
C
C     Command keywords -
C
C     LOCK      Indicates that a search is to be made for lines to
C               'lock' the fit.
C     SPREAD    Indicates that IARC looks for lines first with an
C               increased sigma, then with the specified sigma in order
C               to refine the fit.  If NOSPREAD is specified, the search
C               is just with the specified sigma value.
C     DETAIL    Indicates that full details of the fits are to be output
C               (This is mainly a diagnostic tool.)
C     XCORR     If specified, IARC will attempt to determine a linear
C               shift between successive spectra using
C               cross-correlation. This is particularly applicable to
C               fibre data, where such linear shifts may occur. It is
C               probably not useful for cases such as image-tube
C               distortion, where the spectra should change in a
C               constant manner.
C     WEIGHT    Indicates whether the least-squares fit is to be
C               weighted using the peak intensity of each line (so that
C               more weight is given to strong lines).
C     CHANSHIFT A constant shift which is added to all the input channel
C               positions read from the arlines.lis file.  Shifts in the
C               range -1000.0 to 1000.0 are permitted.  If no value is
C               specified no shift is made.  The value is supplied in
C               `units' of pixels.
C
C     User variables used -
C
C     (<) IARC_WMAX   (Numeric) Maximum wavelength for any of the
C                     spectra.
C     (<) IARC_WMIN   (Numeric) Minimum     "       "   "  "   "    "
C     (<) NOFITS      (Numeric) The number of rows that could not be
C                     fitted.
C     (<) ORDER       (Numeric) The order of the original fit.
C     (<) RMSMAX      (Numeric) Maximum RMS error from the fits.
C
C     Input files -
C
C     ARLINES.LIS   Contains the details of the starting fit.  For
C                   format details see comments for subroutine ARGETL,
C                   or the ARC command.
C
C     Output files -
C
C     As named by   Contains the details of the 2D fit.  Format :
C     the FILE      Name of image, (24X,A).
C     parameter.    Dimensions, NX, NY (17X,I5,4X,I5).
C                   # rows not fitted properly, (42X,I5).
C                   Maximum RMS error, (20X,F10.2).
C                   Maximum order used, - ORDER - (33X,I3).
C                   Then, for each row, row number and ORDER+1 polynomial
C                   coefficients, the constant term being the last
C                   non-zero term.  (I14,10X,2D24.16,3(/3D24.16)).
C
C                                    KS / CIT  15th June 1984
C     Modified:
C
C     4th  Feb 1985  KS / AAO.  IARC_WMAX and IARC_WMIN now set.
C     15th Mar 1985  KS / AAO.  DETAIL and RSIGMA added.
C     1st  May 1985  KS / AAO. GAP added.
C     29th Aug 1985  KS / AAO. SPREAD added. Excessive line-deleted
C                    messages bug fixed.  SPR on V4.2 Fortran compiler
C                    submitted, and VOLATILE IR1 included to bypass it.
C                    Output format for linear fits is now correct.
C     24th Sep 1985  KS / AAO. GAP may now be set to zero.
C     30th Dec 1985  KS / AAO.  Fortran 4.3 compiler now available, so
C                    the VOLATILE IR1 statement has been removed.
C     24th Jun 1987  KS / AAO.  Now will accept 3 lines in a
C                    cross-section instead of insisting on at least 4.
C     26th Aug 1987  DJA / AAO. Revised DSA_ routines - some specs
C                    changed. Now uses DYN routines for dynamic-memory
C                    handling.
C     26th Mar 1991  KS / AAO.  XCORR keyword added, controlling
C                    automatic shift analysis of successive spectra.
C                    PAR_ABORT tests added.
C     21st Jul 1992  HME / UoE, Starlink.  Make file name string 64
C                    characters.
C     22nd Sep 1992  HME / UoE, Starlink.  TABs removed, INCLUDE
C                    changed. Lowercase file name. Lowercase extension
C                    .iar.  Changed FIGX_SHIFT to FIG_SHIFT, LINES to
C                    JTY_LINES, XYFIT to FIG_XYFIT.
C     21st Jul 1993  HME / UoE, Starlink.  Use dsa_*_lu.
C     16th Feb 1995  HME / UoE, Starlink. In the big workspace move
C                    the DOUBLE workspace to the front. Otherwise the
C                    odd number of FLOAT workspaces combined with an
C                    odd number of channels in the input spectrum
C                    cause the DOUBLE workspace to be misaligned
C                    (memory address and odd multiple of 4).
C     19th Jul 1995  HME / UoE, Starlink.  Change open status for .iar
C                    file from new to unknown. Hopefully we can then
C                    overwrite files.
C     18th Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     24th Jul 1996  MJCL / Starlink, UCL.  Corrected type of SPBYTES.
C                    Catenations changed for Linux port.
C     9th  Jun 1999  TCDA / Starlink, RAL. Removed unsupported keywords
C                    from OPEN statements.
C     23rd May 2002  ACD / UoE, Starlink.  Add an option for a weighted
C                    fit (in addition to the original unweighted fit)
C                    and output some of the results as ADAM parameters.
C     31st Mar 2003  ACD / UoE, Starlink.  Add an option to add a
C                    constant shift to all the channel positions input
C                    in arlines.lis.
C     2005 June 15   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_LEN,DSA_TYPESIZE
C
C     Maximum number of arc lines
C
      INTEGER NLMAX
      PARAMETER (NLMAX=200)
C
C     Local variables
C
      DOUBLE PRECISION COEFFS(11)!
      REAL      CHANS(NLMAX)     !
      REAL      CHANSN(NLMAX)    !
      REAL      CHANSO(NLMAX)    !
      REAL      CHANPK(NLMAX)    ! Peak intensity of line.
      CHARACTER CHNBUF*75        ! Message buffer for channel shift.
      INTEGER   CHNPOS           ! Length of CHNBUF (excl. trail.
                                 ! blanks)
      REAL      CHNSFT           ! Optional shift added to channel
                                 ! positions
      INTEGER   CPTR             ! Dynamic-memory pointer to workspace
      LOGICAL   DETAIL           ! See above
      INTEGER   DIMS(10)         ! Sizes of dimensions of data
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number of input data array
      LOGICAL   FAULT            ! TRUE if an error occurs
      CHARACTER FILE*132         ! The filename for the output
      LOGICAL   FIRST            !
      INTEGER   FLAGS(NLMAX)     !
      INTEGER   GAP              ! See above
      INTEGER   I                !
      INTEGER   IBRACK           !
      INTEGER   IDOT             !
      INTEGER   IGNORE           ! Used to pass ignorable status
      INTEGER   IL               !
      INTEGER   IRST             !
      INTEGER   IR1              !
      INTEGER   IR1ST            !
      INTEGER   IR2              !
      INTEGER   IRWID            ! See above
      INTEGER   LAST             !
      LOGICAL   LOCK             ! See above
      LOGICAL   MORE             !
      CHARACTER NAME*132         ! The actual name of the image
      INTEGER   NCOEFF           !
      INTEGER   NDIM             ! Number of dimensions in data
      INTEGER   NELM             ! Total number of elements in data
      INTEGER   NL               !
      INTEGER   NLID             !
      INTEGER   NOFITS           !
      INTEGER   NX               ! Size of 1st dimension
      INTEGER   NY               ! Size of 2nd dimension (if present)
      INTEGER   ORDER            !
      REAL      RMS              !
      REAL      RMSMAX           !
       INTEGER   SCRPTR           ! Dynamic-memory pointer to workspace
      REAL      SIGMA            !
      REAL      SIGMIN           !
      INTEGER   SPBYTES          ! Bytes in a single spectrum
      LOGICAL   SPREAD           !
      INTEGER   SPTR             ! Dynamic-memory pointer to spectrum
      INTEGER   S2PTR            ! Dynamic-memory pointer to previous
                                 ! spectrum
      INTEGER   STATUS           ! Running status for DSA_ routines
      CHARACTER STRING*64        ! Output message string
      REAL      VALUE            ! Temporary real number
      REAL      W(NLMAX)         ! Weight for fit.
      REAL      WAVES(NLMAX)     !
      LOGICAL   WEIGHT           ! Flag: .TRUE. for a weighted fit.
      REAL      WMAX             !
      REAL      WMIN             !
      INTEGER   WSLOT            ! Map slot number of workspace
      REAL      WWMAX            !
      REAL      WWMIN            !
      REAL      X(NLMAX)         !
      LOGICAL   XCORR            ! Cross-correlation is to be used?
      REAL      Y(NLMAX)         !
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Initial values
C
      FAULT=.FALSE.
C
C     Get name of IMAGE and open it. Also find the file's actual name
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      CALL DSA_GET_ACTUAL_NAME('IMAGE',NAME,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Read file giving details of previous fit
C
      CALL IRGETL(NLMAX,CHANS,WAVES,NLID,NCOEFF,COEFFS,SIGMA,STATUS)
      IF (STATUS.NE.0) THEN
         FAULT=.TRUE.
         GO TO 500
      END IF
      ORDER=NCOEFF-1
C
C     Obtain the optional channel shift and if it lies in the permitted
C     range then apply it to the channel positions for all the lines.
C
      CALL PAR_RDVAL ('CHANSHIFT', -1.0E3, 1.010E3, 1.005E3, ' ',
     :                CHNSFT)
C
      IF (CHNSFT .GT. -1.0E3  .AND.  CHNSFT .LT. 1.0E3) THEN
         CHNBUF = ' '
         CHNPOS = 0
         CALL CHR_PUTC ('Channel positions for all lines shifted by ',
     :                  CHNBUF, CHNPOS)
         CALL CHR_PUTR (CHNSFT, CHNBUF, CHNPOS)
         CALL CHR_PUTC (' pixels.', CHNBUF, CHNPOS)
         CALL PAR_WRUSER (CHNBUF(1 : CHNPOS), IGNORE)
C
         DO I=1,NLID
            CHANS(I) = CHANS(I) + CHNSFT
         END DO
      END IF
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=NELM/NX
      IF (STATUS.NE.0) GOTO 500
C
C     Get values of RSTART and RWIDTH
C
      CALL PAR_RDVAL('RSTART',1.,FLOAT(NY),FLOAT((NY+1)/2),' ',
     :                                                    VALUE)
      IRST=NINT(VALUE)
      CALL PAR_RDVAL('RWIDTH',1.,FLOAT(NY),1.,' ',VALUE)
      IRWID=NINT(VALUE)
      IF (PAR_ABORT()) GO TO 500
C
C     And name of file for results
C
      STRING=NAME(:ICH_LEN(NAME))//'.iar'
      CALL PAR_SDCHAR('FILE',STRING,STATUS)
      CALL PAR_RDCHAR('FILE',STRING,FILE)
      IF (PAR_ABORT()) GO TO 500
      IBRACK=MAX(1,INDEX(FILE,']'))
      IDOT=INDEX(FILE(IBRACK:),'.')
      IF (IDOT.EQ.0) FILE=FILE(:ICH_LEN(FILE))//'.iar'
C
C     See if we are to overide the file sigma value.
C
      CALL PAR_SDVAL('RSIGMA',SIGMA,STATUS)
      CALL PAR_RDVAL('RSIGMA',0.01,30.,SIGMA,'Pixels',VALUE)
      IF (PAR_ABORT()) GO TO 500
      IF (SIGMA.NE.VALUE) THEN
         CALL PAR_WRUSER('Note: Sigma used differs from that in file.',
     :                                                          IGNORE)
         SIGMA=VALUE
      END IF
C
C     See if we are to search with a wider sigma.
C
      CALL PAR_RDKEY('SPREAD',.TRUE.,SPREAD)
C
C     And get GAP value
C
      CALL PAR_RDVAL('GAP',0.,10000.,1.,'Cross sections',VALUE)
      IF (PAR_ABORT()) GO TO 500
      GAP=VALUE
      IF (GAP.GE.NY) THEN
         CALL PAR_WRUSER(
     :      'The value used for GAP has the effect of disabling '//
     :      'line deletion.',IGNORE)
      END IF
C
C     Are we to look for LOCK lines - if so get SIGMIN
C
      CALL PAR_RDKEY('LOCK',.FALSE.,LOCK)
      IF (LOCK) CALL PAR_RDVAL('SIGMIN',0.,10000.,10.,' ',SIGMIN)
C
C     Use cross-correlation to determine shifts between spectra?
C
      CALL PAR_RDKEY('XCORR',.FALSE.,XCORR)
      IF (PAR_ABORT()) GO TO 500
C
C     Output fit details?
C
      CALL PAR_RDKEY('DETAIL',.FALSE.,DETAIL)
      IF (PAR_ABORT()) GO TO 500
C
C     Weight the fit?
C
      CALL PAR_RDKEY('WEIGHT',.FALSE.,WEIGHT)
      IF (PAR_ABORT()) GO TO 500
C
C     Map the data array
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get the necessary workspace - one array to hold the arc
C     spectra, and one scratch array for IRLOCK, and one array to
C     hold the previous spectrum (used for cross-correlation), all
C     three NX long), and one array for the polynomial fits, NY*11
C     long, in double precision.
C
      CALL DSA_GET_WORK_ARRAY (11*NY,'DOUBLE',CPTR,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX,'FLOAT',SPTR,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX,'FLOAT',S2PTR,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX,'FLOAT',SCRPTR,WSLOT,STATUS)

      IF (STATUS.NE.0) GO TO 500
C
C     Set flags array (used to indicate that a line could not
C     be found).
C
      DO IL=1,NLID
         FLAGS(IL)=0
      END DO
C
C     Work up through the cross-sections, from the middle
C
      SPBYTES=NX*DSA_TYPESIZE('FLOAT',STATUS)
      IR1=MAX(1,IRST-IRWID/2)
      IR2=MIN(NY,IR1+IRWID-1)
      IR1ST=IR1
      NOFITS=0
      RMSMAX=0.
      LAST=0
      FIRST=.TRUE.
      MORE=.TRUE.
      DO WHILE (MORE)
C
C        Form the single arc spectrum
C
         CALL FIG_XTRACT(%VAL(CNF_PVAL(DPTR)),NX,NY,IR1,IR2,
     :                   %VAL(CNF_PVAL(SPTR)))
C
         IF (FIRST) THEN
C
C           First time through, look for extra lines to use to lock the fit
C           if required, and copy search line list (CHANS) into work line
C           list (CHANSO).
C
            IF (LOCK) THEN
               CALL IRLOCK(%VAL(CNF_PVAL(SPTR)),NX,NLMAX,SIGMA,SIGMIN,
     :                     NCOEFF,COEFFS,%VAL(CNF_PVAL(SCRPTR)),
     :                     NLID,CHANS,WAVES)
            END IF
            DO I=1,NLID
               CHANSO(I)=CHANS(I)
            END DO
         ELSE
C
C           For subsequent spectra, see if we are to perform a cross-
C           correlation to determine the shift, and if so, use this
C           shift to modify the channel numbers.
C
            IF (XCORR) THEN
               CALL IRSHIFT (%VAL(CNF_PVAL(SPTR)),
     :                       %VAL(CNF_PVAL(S2PTR)),NX,CHANSO,NLID)
            END IF
         END IF
C
C        Find the line centers
C
         CALL IREFIT(%VAL(CNF_PVAL(SPTR)),NX,CHANSO,NLID,SIGMA,SPREAD,
     :               CHANSN,CHANPK)
C
C        Sort out the lines ready for the next section, deleting
C        any previously flagged as missing
C
         CALL IRLFIX(NLID,WAVES,CHANSN,CHANPK,GAP,CHANSO,FLAGS,X,Y,W,NL)
C
C        Fit the lines and output the results
C
         CALL IRFITL(WEIGHT,NL,X,Y,W,NX,NY,IR1,IR2,DETAIL,LAST,ORDER,
     :               %VAL(CNF_PVAL(CPTR)),RMS,WMAX,WMIN)
         IF (RMS.GT.RMSMAX) RMSMAX=RMS
         IF (LAST.NE.IR1) NOFITS=NOFITS+IR2-IR1+1
         IF (FIRST) THEN
            FIRST=.FALSE.
            WWMAX=WMAX
            WWMIN=WMIN
         ELSE
            IF (WWMAX.LT.WMAX) WWMAX=WMAX
            IF (WWMIN.GT.WMIN) WWMIN=WMIN
         END IF
C
C        If we are using cross-correlation for shift analysis, copy
C        the current spectrum so that it becomes the new previous spectrum.
C
         IF (XCORR) THEN
            CALL GEN_MOVE(SPBYTES,%VAL(CNF_PVAL(SPTR)),
     :                    %VAL(CNF_PVAL(S2PTR)))
         END IF
C
C        And carry on until all the cross-sections are done
C
         IR1=IR2+1
         IR2=MIN(NY,IR2+IRWID)
         MORE=IR1.LE.NY
      END DO
C
C     Now repeat the process going down through the rows
C
      IR2=IR1ST-1
      IR1=MAX(1,IR2-IRWID+1)
      DO I=1,NLID
         CHANSO(I)=CHANS(I)
         FLAGS(I)=0
      END DO
      MORE=IR1ST.GT.1
      LAST=0
      FIRST=.TRUE.
      DO WHILE (MORE)
         CALL FIG_XTRACT(%VAL(CNF_PVAL(DPTR)),NX,NY,IR1,IR2,
     :                   %VAL(CNF_PVAL(SPTR)))
         IF (XCORR.AND.(.NOT.FIRST)) THEN
            CALL IRSHIFT (%VAL(CNF_PVAL(SPTR)),%VAL(CNF_PVAL(S2PTR)),NX,
     :                    CHANSO,NLID)
         END IF
         FIRST=.FALSE.
         CALL IREFIT(%VAL(CNF_PVAL(SPTR)),NX,CHANSO,NLID,SIGMA,
     :                    SPREAD,CHANSN,CHANPK)
         CALL IRLFIX(NLID,WAVES,CHANSN,CHANPK,GAP,CHANSO,FLAGS,X,Y,W,NL)
         CALL IRFITL(WEIGHT,NL,X,Y,W,NX,NY,IR1,IR2,DETAIL,LAST,ORDER,
     :               %VAL(CNF_PVAL(CPTR)),RMS,WMAX,WMIN)
         IF (RMS.GT.RMSMAX) RMSMAX=RMS
         IF (LAST.NE.IR1) NOFITS=NOFITS+IR2-IR1+1
         IF (WWMAX.LT.WMAX) WWMAX=WMAX
         IF (WWMIN.GT.WMIN) WWMIN=WMIN
         IR2=IR1-1
         IR1=MAX(1,IR1-IRWID)
         IF (XCORR) THEN
            CALL GEN_MOVE(SPBYTES,%VAL(CNF_PVAL(SPTR)),
     :                    %VAL(CNF_PVAL(S2PTR)))
         END IF
         MORE=IR2.GE.1
      END DO
C
C     Output results
C
      CALL IROUTP(FILE,NAME,NX,NY,%VAL(CNF_PVAL(CPTR)),NLID,ORDER,
     :            NOFITS,RMSMAX,STATUS)
      IF (STATUS.NE.0) FAULT=.TRUE.
C
C     Set user variables
C
      CALL VAR_SETNUM('IARC_WMAX',0,0,WWMAX,STATUS)
      CALL VAR_SETNUM('IARC_WMIN',0,0,WWMIN,STATUS)
C
C     Tidy up
C
  500 CONTINUE
C
      CALL DSA_CLOSE(STATUS)
C
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE IRGETL (NLMAX,CHANS,WAVES,NLID,NCOEFF,COEFFS,
     :                   SIGMA,STATUS)
C
C     I R G E T L
C
C     IARC utility.  Opens an existing arc line file and
C     reads in the details of the previous fit from it.  The
C     file will be ARLINES.LIS, in the default directory.
C
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NLMAX    (Integer) Maximum possible number of lines.
C     (<) CHANS    (Real array CHANS(NLMAX)) Channel numbers of
C                  identified lines.
C     (<) WAVES    (Real array WAVES(NLMAX)) Wavelengths of
C                  identified lines.
C     (<) NLID     (Integer) Number of identified lines.
C     (<) NCOEFF   (Integer) Number of coefficients used for fit.
C     (<) COEFFS   (Double precision array COEFFS(11)) Polynomial
C                  coefficients used for fit.  Unused elements are
C                  set to zero.  The NCOEFFth term will be the
C                  constant term.
C     (<) SIGMA    (Real) Line width used for fit.
C     (<) STATUS   (Integer) Return status code.  0 => OK, non-zero
C                  values are Fortran I/O error codes.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     PAR_WRUSER    (PAR_ package) Send message to user
C     DSA_FREE_LU   (VMS standard) Release logical unit number
C     DSA_GET_LU    ( "     "    ) Get logical unit number
C
C     Files read -
C
C     ARLINES.LIS   File containing the lines used in the arcline fit.
C                   Format is as follows -
C                   Number of lines used in fit (I5)
C                   1 blank record, then one header record.
C                   Then one record for each line, giving channel
C                   number, wavelength, calculated wavelength and
C                   wavelength discrepancy.  (4F13.4)
C                   Then one blank record, then a record giving the RMS
C                   error and the value of SIGMA used
C                   (12X,F10.2,19X,F5.2).
C                   Then one blank record, then one record giving the
C                   order of fit (ie 1 less than number of coefficients)
C                   (15X,I3), then one blank record, then one or more
C                   records giving the coefficients (3D23.16)
C
C                                              KS / CIT 15th June 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NLMAX,NLID,NCOEFF,STATUS,IGNORE
      REAL CHANS(NLMAX),WAVES(NLMAX),SIGMA
      DOUBLE PRECISION COEFFS(11)
C
C     Local variables
C
      INTEGER I,IN,NFILE,NSTAT
C
C     CALL LIB$GET_LUN(IN)
      IGNORE=0
      CALL DSA_GET_LU(IN,IGNORE)
C
      NLID=0
      OPEN (UNIT=IN,FILE='arlines.lis',STATUS='OLD',IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to open line list file',NSTAT)
      ELSE
         READ (IN,'(I5,//)',IOSTAT=STATUS) NLID
         IF (NLID.GT.NLMAX) THEN
            CALL PAR_WRUSER('Warning: Too many arc lines in file',
     :                                                      STATUS)
            NLID=NLMAX
         END IF
         DO NFILE=1,NLID
            READ (IN,'(2F13.4)',IOSTAT=STATUS)
     :                                CHANS(NFILE),WAVES(NFILE)
            IF (STATUS.NE.0) GO TO 400
         END DO
         READ (IN,'(/41X,F5.2)',IOSTAT=STATUS) SIGMA
         IF (STATUS.NE.0) GO TO 400
         READ (IN,'(/15X,I3/)',IOSTAT=STATUS) NCOEFF
         IF (STATUS.NE.0) GO TO 400
         NCOEFF=NCOEFF+1
         READ (IN,'(3D23.16)',IOSTAT=STATUS) (COEFFS(I),I=1,NCOEFF)
  400    CONTINUE
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('I/O error while reading arc line file',
     :                                                       NSTAT)
         END IF
         CLOSE (IN,IOSTAT=STATUS)
      END IF
C
C     CALL LIB$FREE_LUN(IN)
      IGNORE=0
      CALL DSA_FREE_LU(IN,IGNORE)
C
      END
C+
      SUBROUTINE IREFIT (DATA,NX,CHANSO,NLID,SIGMA,SPREAD,CHANSN,CHANPK)
C
C     I R E F I T
C
C     IARC utility.  Refits a spectrum, given a table of expected
C     arc line channel numbers, to produce a new table of
C     channel numbers for each line.  The program looks for a centroid
C     in the data for each line in the table, and uses the new center
C     if it finds one.  The search is performed twice, once with a
C     larger sigma - in order to stand more chance of catching the
C     lines - then with the original sigma - to refine the centering.
C     This feature can be disabled.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) DATA     (Real array DATA(NX)) The arc spectrum being fitted.
C     (>) NX       (Integer) The number of pixels in the spectrum
C     (>) CHANSO   (Real array CHANSO(NLID)) The channel numbers of the
C                  lines identified previously.  A line will be ignored
C                  if its channel number is zero.
C     (>) NLID     (Integer) Number of lines previously identified.
C     (>) SIGMA    (Real) The line width used for the previous search.
C     (>) SPREAD   (Logical) True if initial is to be made at increased
C                  sigma.
C     (<) CHANSN   (Real array CHANSN(NLID)) The new channel numbers for
C                  the lines.  If a line cannot be found, it is set to
C                  zero.
C     (<) CHANPK   (Real array CHANPK(NLID)) The peak intensity for the
C                  lines.  If a line cannot be found, it is set to zero.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     GEN_CENTROID (GEN_package) Finds peak center by convolution with
C                                derivitave of a gaussian.
C
C                                                KS / CIT 18th June 1984
C     Modified:
C
C     29th Aug 1985  KS / AAO.  SPREAD parameter introduced.
C     23rd May 2002  ACD / UoE. CHANPK parameter array introduced.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL SPREAD
      INTEGER NX, NLID
      REAL    DATA(NX), CHANSO(NLID), SIGMA, CHANSN(NLID), CHANPK(NLID)
C
C     Local variables
C
      INTEGER IL, IRPT, NRPT, STATUS
      REAL    CENTER, SIGVAL, STRENGTH
C
C     Search twice, once using larger sigma value, then with
C     value as passed.  (Unless this is disabled by SPREAD value)
C
      IF (SPREAD) THEN
         SIGVAL=SIGMA*1.5
         NRPT=2
      ELSE
         NRPT=1
         SIGVAL=SIGMA
      END IF
      DO IRPT=1,NRPT
         DO IL=1,NLID
            IF (IRPT.EQ.1) THEN
               CENTER=CHANSO(IL)
            ELSE
               CENTER=CHANSN(IL)
            END IF
            CHANSN(IL)=0.
            CHANPK(IL)=0.
            IF (CENTER.NE.0.) THEN
               CALL GEN_CENTROID(DATA,NX,SIGVAL,CENTER,STRENGTH,STATUS)
               IF (STATUS.EQ.0) THEN
                  CHANSN(IL)=CENTER
                  CHANPK(IL)=STRENGTH
               END IF
            END IF
         END DO
         SIGVAL=SIGMA
      END DO
C
      END
C+
      SUBROUTINE IRLFIX (NLID,WAVES,CHANSN,CHANPK,GAP,CHANSO,FLAGS,
     :  X,Y,W,NL)
C
C     I R L F I X
C
C     After IREFIT has analysed a new spectrum using an original line
C     list, IRLFIX sorts out the wavelength and channel arrays and
C     creates the X and Y arrays ready for the polynomial fit.  If a
C     line is not found in a search, the old value is used.  However, if
C     a line is not found a given number of times in succession (the
C     GAP value), it is dropped from the lists.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (>) NLID     (Integer) The original number of lines identified -
C                  equivalently, the dimensions of the line arrays.
C     (>) WAVES    (Real array WAVES(NLID)) The wavelengths corresponding
C                  to the lines in CHANSO.
C     (>) CHANSN   (Real array CHANSN(NLID)) The channel numbers found by
C                  IREFIT.  Lines not found have zero values.
C     (>) CHANPK   (Real array CHANPK(NLID)) The peak intensity for the
C                  lines.  Lines not found have zero values.
C     (>) GAP      (Integer) The number of lines over which a line will
C                  have its last position used before being dropped from
C                  the line list.
C     (!) CHANSO   (Real array CHANSO(NLID)) Passed as the original
C                  channel list used by IREFIT.  Returned with lines
C                  deleted if necessary to form the list to be used next.
C                  A deleted line has a zero channel number.
C     (!) FLAGS    (Integer array FLAGS(NLID)) Used to indicate how many a
C                  times a line was not found. Should be set all zero
C                  the first time this routine is called.
C     (<) X        (Real array X(NLID)) The channel values to be used in
C                  the polynomial fit.
C     (<) Y        (Real array Y(NLID)) The wavelength values to be used in
C                  the polynomial fit.
C     (<) W        (Real array W(NLID)) Weight for each line to be used in
C                  the polynomial fit, computed from the line strengths.
C     (<) NL       (Integer) The number of lines to be used in the polynomial
C                  fit.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     ICH_ENCODE   (ICH_ package) Encode number into character string
C     GEN_NTH      (GEN_   "    ) Get abbreviation (st,nd,rd,th) for #
C     PAR_WRUSER   (PAR_   "    ) Write character string to user
C
C                                                   KS / CIT 18th June 1984
C     Modified:
C
C     1st May 1985.  KS / AAO.  GAP parameter introduced.
C     29th Aug 1985  KS / AAO.  Bug causing 'line deleted' messages to be
C                    given more than once for a given line now fixed.
C     24th Aug 1985  KS / AAO.  Format of line 'not found message' modified
C                    to show number of times not found.
C     24rd May 2002  ACD /UoE.  Added computation of a weight for each
C                    line from the line strength.
C     31st Mar 2003  ACD /UoE.  Added a check to prevent division by zero
C                    when normalising the weights in the case where all
C                    the weights are zero.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NLID, NL, GAP, FLAGS(NLID)
      REAL WAVES(NLID), CHANSN(NLID), CHANSO(NLID), CHANPK(NLID)
      REAL X(NLID), Y(NLID), W(NLID)
C
C     Functions
C
      INTEGER ICH_ENCODE
      CHARACTER*2 GEN_NTH
C
C     Local variables
C
      INTEGER IL, INVOKE, NEXT, STATUS
      CHARACTER*72 STRING
      REAL SUM, MAXWT, MINWT
C
      NL=0
      DO IL=1,NLID
         IF (CHANSN(IL).NE.0.) THEN
            NL=NL+1
            X(NL)=CHANSN(IL)
            Y(NL)=WAVES(IL)
            W(NL)=CHANPK(IL)
            CHANSO(IL)=CHANSN(IL)
            FLAGS(IL)=0
         ELSE
            FLAGS(IL)=FLAGS(IL)+1
            STRING='Line at wavelength '
            INVOKE=ICH_ENCODE(STRING,WAVES(IL),20,2,NEXT)
            IF (FLAGS(IL).EQ.(GAP+1)) THEN
               CHANSO(IL)=0.
               STRING(NEXT:)=' removed from search list'
               CALL PAR_WRUSER(STRING(:NEXT+24),STATUS)
            ELSE IF (FLAGS(IL).LE.GAP) THEN
               NL=NL+1
               X(NL)=CHANSO(IL)
               Y(NL)=WAVES(IL)
               W(NL)=CHANPK(IL)
               STRING(NEXT:)=' not found ('
               INVOKE=ICH_ENCODE(STRING,FLOAT(FLAGS(IL)),NEXT+12,0,NEXT)
               STRING(NEXT:)=GEN_NTH(FLAGS(IL))//' time)'
               CALL PAR_WRUSER(STRING(:NEXT+7),STATUS)
            END IF
         END IF
      END DO
C
C     Strip out bad weights and replace with the minimum good weight.
C
      MAXWT = 0.0E0
      DO IL = 1, NL
         IF (W(IL) .GT. MAXWT) THEN
            MAXWT = W(IL)
         END IF
      END DO
      MINWT = MAXWT
      DO IL = 1, NL
         IF (W(IL) .GT. 0.0E0) THEN
            IF (W(IL) .LT. MINWT) THEN
               MINWT = W(IL)
            END IF
         END IF
      END DO
      DO IL = 1, NL
         IF (W(IL) .LE. 0.0E0) THEN
            W(IL) = MINWT
         END IF
      END DO
C
C     Normalise the weights.
C
      SUM=0.0E0
      DO IL = 1, NL
         SUM = SUM + W(IL)
      END DO
      IF (SUM .GT. 1.0E-8) THEN
         DO IL = 1, NL
            W(IL) = W(IL) / SUM
         END DO
      END IF
C
      END
C+
      SUBROUTINE IRFITL(WEIGHT,NL,X,Y,W,NX,NY,IR1,IR2,DETAIL,LAST,
     :                                    ORDER,CARRAY,RMS,WMAX,WMIN)
C
C     I R F I T L
C
C     Performs a polynomial fit to the lines found for one
C     spectrum and sets the results into the coefficient array.
C
C     Parameters - (">" input, "<" output, "!' modified)
C
C     (>) WEIGHT (Logical) Flag: weight the fit? (.TRUE. for weighted fit).
C     (>) NL     (Integer) Number of lines identified.
C     (>) X      (Real array X(NL)) Channel numbers for lines.
C     (>) Y      (Real array Y(NL)) Wavelengths for lines.
C     (>) W      (Real array W(NL)) Optional weights for lines.
C     (>) NX     (Integer) Number of columns in image. (Needed for
C                WMAX and WMIN calculation.)
C     (>) NY     (Integer) Number of rows in image.
C     (>) IR1    (Integer) First row in set being fitted.
C     (>) IR2    (Integer) Last row in set being fitted.
C     (>) DETAIL (Logical) True if full details of fit are to be
C                output.
C     (!) LAST   (Integer) Last row for which a decent fit was
C                obtained. If no fit obtained, should be zero.  If
C                this fit is OK, is set to IR1.
C     (>) ORDER  (Integer) Maximum order for fit.
C     (<) CARRAY (Double precision array COEFFS(11,NY)) Polynomial
C                coefficients for the cross-sections.  For each row,
C                the constant term is the last non-zero term.
C     (<) RMS    (Real) RMS for fit.
C     (<) WMAX   (Real) Maximum wavelength for the spectrum.
C     (<) WMIN   (Real) Minimum wavelength for the spectrum.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     ICH_ENCODE (ICH_ package) Encode real number into character string
C     GEN_EPOLYD (GEN_   "    ) Evaluate double precision polynomial
C     FIG_XYFIT      (FIG_   "    ) Polynomial fit
C     FIG_WXYFIT     (FIG_   "    ) Weighted polynomial fit
C
C                                             KS / CIT 20th June 1984
C     Modified:
C
C     4th Jan 1985.  KS / AAO.  WMAX and WMIN added.
C     15th March 1984.  KS / AAO.  DETAIL added.
C     24th June 1987.  KS / AAO.  'Too few lines' test changed to allow
C                      three lines, instead of insisting on at least 4.
C     23rd May 2002. ACD / UoE.  Added option for weighted fit.
C     25th Mar 2003. MJC / RAL, ACD / UoE.  Fixed bug by initialising
C                      local variable MORD which was previously
C                      uninitialised.  Also corrected a couple of typing
C                      mistakes in the prologue.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL DETAIL, WEIGHT
      INTEGER NL, NX, NY, IR1, IR2, LAST, ORDER
      REAL    W(*), X(*), Y(*), WMAX, WMIN
      DOUBLE PRECISION CARRAY(11,NY)
C
C     Functions
C
      INTEGER ICH_ENCODE
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      INTEGER I, IGNORE, INVOKE, IR, MORD, NEXT, STATUS
      REAL    ERR, RMS, TEMP, VALUE
      DOUBLE PRECISION COEFFS(11)
      CHARACTER CHARS*72, STRING*72
C
      MORD=0
C
      IF (IR1.EQ.IR2) THEN
         STRING='Row '
         INVOKE=ICH_ENCODE(STRING,FLOAT(IR1),5,0,NEXT)
      ELSE
         STRING='Rows '
         INVOKE=ICH_ENCODE(STRING,FLOAT(IR1),6,0,NEXT)
         STRING(NEXT:)=' to '
         INVOKE=ICH_ENCODE(STRING,FLOAT(IR2),NEXT+4,0,NEXT)
      END IF
      IF (NL.LT.3) THEN
         STRING(NEXT:)=' Too few lines ('
         INVOKE=ICH_ENCODE(STRING,FLOAT(NL),NEXT+16,0,NEXT)
         IF (LAST.GT.0) THEN
            STRING(NEXT:)=') found.  Last good fit used instead'
            CALL PAR_WRUSER(STRING(:NEXT+35),STATUS)
            DO I=1,11
               COEFFS(I)=CARRAY(I,LAST)
            END DO
         ELSE
            STRING(NEXT:)=') found, and no good fit available **'
            CALL PAR_WRUSER(STRING(:NEXT+36),STATUS)
            DO I=1,11
               COEFFS(I)=0.
            END DO
         END IF
      ELSE
         MORD=MIN(10,NL-2,ORDER)
         DO I=1,11
            COEFFS(I)=0.
         END DO
         IF (.NOT. WEIGHT) THEN
            CALL FIG_XYFIT(X,Y,NL,COEFFS,MORD)
         ELSE
            CALL FIG_WXYFIT(X,Y,W,NL,COEFFS,MORD)
         END IF
         RMS=0.
         IF (DETAIL) THEN
            CALL PAR_WRUSER(' ',STATUS)
            CALL PAR_WRUSER(
     :      '           Line    Wavelength    Calculated   Discrepancy',
     :                                                           STATUS)
            CALL PAR_WRUSER(
     :      '                                 Wavelength'
     :                                                          ,STATUS)
            CALL PAR_WRUSER(' ',STATUS)
         END IF
         DO I=1,NL
            VALUE=GEN_EPOLYD(DBLE(X(I)),COEFFS,MORD+1)
            ERR=Y(I)-VALUE
            RMS=RMS+ERR*ERR
            IF (DETAIL) THEN
               WRITE(CHARS,'(1X,I3,4F12.3)',IOSTAT=IGNORE) I,X(I),
     :                                          Y(I),VALUE,ERR
               CALL PAR_WRUSER(CHARS,STATUS)
            END IF
         END DO
         IF (DETAIL) CALL PAR_WRUSER(' ',STATUS)
         RMS=SQRT(RMS/FLOAT(NL))
         WRITE (STRING(NEXT:),'(A,I4,A,I3,A,F10.2)',IOSTAT=IGNORE)
     :           ' Lines fitted:',NL,', Order of fit:',MORD,' RMS:',RMS
         CALL PAR_WRUSER(STRING,STATUS)
         LAST=IR1
      END IF
      WMIN=GEN_EPOLYD(1.0D0,COEFFS,MORD+1)
      WMAX=GEN_EPOLYD(DBLE(NX),COEFFS,MORD+1)
      IF (WMAX.LT.WMIN) THEN
         TEMP=WMAX
         WMAX=WMIN
         WMIN=TEMP
      END IF
      DO IR=IR1,IR2
        DO I=1,11
            CARRAY(I,IR)=COEFFS(I)
         END DO
      END DO
C
      END
C+
      SUBROUTINE IROUTP(FILE,IMAGE,NX,NY,CARRAY,NLID,ORDER,NOFITS,
     :                                               RMSMAX,STATUS)
C
C     I R O U T P
C
C     Outputs the results of the fit to the terminal, as a summary,
C     and to the results file.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) FILE   (Character) The name of the result file.
C     (>) IMAGE  (Character) The name of the image used for the fit
C     (>) NX     (Integer) The number of pixels in each row of the image
C     (>) NY     (Integer) The number of rows in the image
C     (>) CARRAY (Double precision CARRAY(11,NY)) The coefficients for
C                the fits for each row.
C     (>) NLID   (Integer) The number of lines in the original fit
C     (>) ORDER  (Integer) The order for the original fit
C     (>) NOFITS (Integer) The number of rows that could not be fitted
C     (>) RMSMAX (Real) Maximum RMS error from the fits
C     (<) STATUS (Integer) I/O status code. 0 if OK, non-zero => some
C                error writing to the IARC.LIS file.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     ICH_LEN      (ICH_ package) Position of last non-blank char.
C     DSA_GET_LU   (VMS standard) Get logical unit number from pool
C     DSA_FREE_LU  ( "     "    ) Release logical unit number
C     PAR_PUT0I,   ) Output to ADAM.
C     PAR_PUT0R    )
C
C                                            KS / CIT 20th June 1984
C     Modified:
C
C     30th Aug 1985.  KS / AAO.  Format corrected for case ORDER=1
C     19th Jul 1995.  HME / UoE, Starlink.  Change open status from new
C                     to unknown. Hopefully we can then overwrite files.
C     23rd May 2002.  ACD / UoE, Starlink.  Added output of some parameters
C                     to the ADAM environment.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY, NLID, ORDER, NOFITS, STATUS
      REAL    RMSMAX
      DOUBLE PRECISION CARRAY(11,NY)
      CHARACTER*(*) FILE, IMAGE
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Local variables
C
      LOGICAL FOPEN
      INTEGER I, IGNORE, IY, NSTAT, OUTPUT
      CHARACTER STRING*72
C
C     Open file
C
C     CALL LIB$GET_LUN(OUTPUT)
      IGNORE=0
      CALL DSA_GET_LU(OUTPUT,IGNORE)
      OPEN (UNIT=OUTPUT,FILE=FILE,STATUS='UNKNOWN',
     :                IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         STRING= 'Unable to create result file '
     :                             //FILE(:ICH_LEN(FILE))
         CALL PAR_WRUSER(STRING,NSTAT)
         FOPEN=.FALSE.
         GO TO 700
      END IF
      FOPEN=.TRUE.
C
C     Summary to terminal and to file
C
      CALL PAR_WRUSER(' ',NSTAT)
      CALL PAR_WRUSER('Summary of Image Arc Fit Results - ',NSTAT)
      CALL PAR_WRUSER('-----------------------------------',NSTAT)
      CALL PAR_WRUSER(' ',NSTAT)
      WRITE (OUTPUT,'(2A)',IOSTAT=STATUS)
     :                 '2D fit to data in image ',IMAGE
      WRITE (STRING,'(A,I5,A,I5)',IOSTAT=IGNORE)
     :                     'Image dimensions ',NX,' by ',NY
      CALL PAR_WRUSER(STRING,NSTAT)
      WRITE (OUTPUT,'(A)',IOSTAT=STATUS) STRING
      IF (STATUS.NE.0) GO TO 600
      WRITE (STRING,'(A,I5)',IOSTAT=IGNORE)
     :           'Number of rows that could not be fitted = ',NOFITS
      CALL PAR_WRUSER(STRING,NSTAT)
      WRITE (OUTPUT,'(A)',IOSTAT=STATUS) STRING
      IF (STATUS.NE.0) GO TO 600
      WRITE (STRING,'(A,F10.2)',IOSTAT=IGNORE)
     :            'Maximum RMS error = ',RMSMAX
      CALL PAR_WRUSER(STRING,NSTAT)
      WRITE (OUTPUT,'(A)',IOSTAT=STATUS) STRING
      IF (STATUS.NE.0) GO TO 600
      WRITE (STRING,'(A,I3)',IOSTAT=IGNORE)
     :            'Maximum degree polynomial used = ',ORDER
      CALL PAR_WRUSER(STRING,NSTAT)
      WRITE (OUTPUT,'(A)',IOSTAT=STATUS) STRING
      IF (STATUS.NE.0) GO TO 600
      DO IY=1,NY
         IF (ORDER.GT.1) THEN
            WRITE (OUTPUT,'(I14,10X,2D24.16,3(/3D24.16))',
     :             IOSTAT=STATUS) IY,(CARRAY(I,IY),I=1,ORDER+1)
         ELSE
            WRITE (OUTPUT,'(I14,10X,2D24.16)',
     :             IOSTAT=STATUS) IY,(CARRAY(I,IY),I=1,ORDER+1)
         END IF
         IF (STATUS.NE.0)  GO TO 600
      END DO
      INQUIRE (OUTPUT,NAME=STRING,IOSTAT=IGNORE)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('Fit results written to file '
     :                    //STRING(:ICH_LEN(STRING)),STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Output some parameters to the ADAM environment.
C
      NSTAT = 0
      CALL PAR_PUT0I ('NOFITS', NOFITS, NSTAT)
      CALL PAR_PUT0I ('ORDER', ORDER, NSTAT)
      CALL PAR_PUT0R ('RMSMAX', RMSMAX, NSTAT)
C
      GO TO 700
C
C     I/O error from output file
C
  600 STRING='I/O error writing to file '
     :                             //FILE(:ICH_LEN(FILE))
      CALL PAR_WRUSER(STRING,NSTAT)
C
C     Tidy up
C
  700 CONTINUE
      IF (FOPEN) CLOSE(UNIT=OUTPUT,IOSTAT=NSTAT)
C     CALL LIB$FREE_LUN(OUTPUT)
      IGNORE=0
      CALL DSA_FREE_LU(OUTPUT,IGNORE)
C
      END
C+
      SUBROUTINE IRLOCK (DATA,NX,NLMAX,SIGMA,SIGMIN,NCOEFF,COEFFS,
     :                                     SCRAP,NLID,CHANS,WAVES)
C
C     I R L O C K
C
C     Searches the first arc cross-section, looking for strongish
C     lines not already in the tables that can be used to lock the
C     fit in the sparsely identified regions.
C
C     Parameters -   (">" input, "!" modified, "W" workspace)
C
C     (>) DATA    (Real array DATA(NX)) The arc spectrum
C     (>) NX      (Integer) Number of pixels in arc spectrum
C     (>) NLMAX   (Integer) Maximum number of lines that can be
C                 held in the channel and wavelength arrays.
C     (>) SIGMA   (Real) Line width used to find line centers
C     (>) SIGMIN  (Real) Limit on line strength expressed as
C                 max relative to square root of continuum.
C     (>) NCOEFF  (Integer) Number of polynomial coefficients used
C     (>) COEFFS  (Double precision COEFFS(NCOEFF)) Polynomial
C                 coefficients used in original fit.
C     (W) SCRAP   (Real array SCRAP(NX)) Workspace
C     (!) NLID    (Integer) Number of lines to use for fit - passed
C                 as number originally identified, returned with number
C                 of lock lines added.
C     (!) CHANS   (Real array CHANS(NLMAX)) Channel numbers of lines to
C                 use in fit.
C     (!) WAVES   (Real array WAVES(NLMAX)) Wavelengths of lines to be
C                 used in fit.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     GEN_CENTROID (GEN_ package) Find centroid of an arc line
C     GEN_EPOLY    ( "     "    ) Evaluate double precision polynomial
C     JTY_LINES        (JT routine  ) Find candidate arc lines in a spectrum
C     PAR_WRUSER   (PAR_ package) Send string to user
C
C                                              KS / CIT 26th June 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NLMAX, NCOEFF, NLID
      REAL    DATA(NX), SIGMA, SIGMIN, SCRAP(NX), CHANS(NLMAX)
      REAL    WAVES(NLMAX)
      DOUBLE PRECISION COEFFS(NCOEFF)
C
C     Functions
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Maximum number of extra lines
C
      INTEGER NT
      PARAMETER (NT=200)
C
C     Local variables
C
      LOGICAL NEW
      INTEGER ID, IL, NFOUND, STATUS
      REAL    CENTER, STRENGTH, TABLE(NT,6)
      CHARACTER STRING*72
C
C     Search through spectrum looking for all lines (uses algorithm
C     due to John Tonry.)
C
      CALL JTY_LINES(NX,1,NX,SIGMIN,SIGMIN,DATA,NT,SCRAP,NFOUND,
     :                                                 TABLE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :     'Warning - search found more lines than workspace can hold',
     :                                                          STATUS)
         NFOUND=NT
      END IF
C
C     Now re-analyse these lines with the usual algorithm, and see
C     if these are already in the lists.
C
      DO IL=1,NFOUND
         CENTER=TABLE(IL,1)
         CALL GEN_CENTROID(DATA,NX,SIGMA*1.5,CENTER,STRENGTH,STATUS)
         IF (STATUS.EQ.0) THEN
            CALL GEN_CENTROID(DATA,NX,SIGMA,CENTER,STRENGTH,STATUS)
         END IF
         IF (STATUS.EQ.0) THEN
            NEW=.FALSE.
            DO ID=1,NLID
               IF (ABS(CHANS(ID)-CENTER).LT.5.) GO TO 340
            END DO
            NEW=.TRUE.
  340       CONTINUE
            IF (NEW) THEN
               IF (NLID.LT.NLMAX) THEN
                  NLID=NLID+1
                  CHANS(NLID)=CENTER
                  WAVES(NLID)=GEN_EPOLYD(DBLE(CENTER),COEFFS,NCOEFF)
                  WRITE (STRING,'(A,F10.2,A,F10.2,A)',IOSTAT=STATUS)
     :                 'Line at Channel ',CENTER,', Wavelength ',
     :                 WAVES(NLID),' added to lock fit'
                  CALL PAR_WRUSER(STRING,STATUS)
               END IF
            END IF
         END IF
      END DO
C
      END
C+
      SUBROUTINE IRSHIFT (DATA,DATA2,NX,CHANS,NLID)
C
C     I R S H I F T
C
C     Compares a spectrum with the previous spectrum to determine
C     a relative shift between them and apply this to the channel
C     numbers of the lines identified so far.
C
C     Parameters -   (">" input, "!" modified, "W" workspace)
C
C     (>) DATA    (Real array DATA(NX)) The arc spectrum
C     (>) DATA2   (Real array DATA2(NX)) The previous arc spectrum
C     (>) NX      (Integer) Number of pixels in arc spectrum
C     (>) NLID    (Integer) Number of lines to use for fit - passed
C                 as number originally identified, returned with number
C                 of lock lines added.
C     (!) CHANS   (Real array CHANS(NLMAX)) Channel numbers of lines to
C                 use in fit.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     FIG_SHIFT    Determine shift between two spectra.
C     ICH_LEN      Position of last non-blank char in string.
C     ICH_ENCODE   Encode a real number into a string.
C     PAR_WRUSER   Send string to user.
C
C                                              KS / AAO 26th March 1991
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NLID
      REAL    DATA(NX), DATA2(NX), CHANS(NLID)
C
C     Functions
C
      INTEGER ICH_ENCODE
C
C     Local variables
C
      INTEGER   I, INVOKE, NEXT, STATUS
      REAL      SHIFT
      CHARACTER STRING*64
C
C     Determine the relative shift, log it and apply it.
C
      CALL FIG_SHIFT (DATA,DATA2,NX,SHIFT,STATUS)
      IF (STATUS.EQ.0) THEN
         STRING='Shift of '
         INVOKE=ICH_ENCODE(STRING,SHIFT,10,2,NEXT)
         STRING(NEXT:)=' pixels detected between spectra.'
         CALL PAR_WRUSER(STRING,STATUS)
         DO I=1,NLID
            IF (CHANS(I).GT.0.0) CHANS(I)=CHANS(I)+SHIFT
         END DO
      END IF
C
      END
