C+
      SUBROUTINE SCROSS
C
C     S C R O S S
C
C     Main body of the Figaro SCROSS function.  This computes
C     the cross-correlation of two spectra and the location of the
C     central peak of the cross-correlation.  It can be used to
C     determine a relative shift between two spectra.  The cross
C     correlation function can also be saved in a disk structure.
C
C     SCROSS uses the Fourier cross-correlation technique, which is
C     described in, for example:
C
C       R. W. Hunstead, 1980, Proc. Astron. Soc. Australia, vol. 4,
C       no. 1, pp. 77-80.
C
C       J. Tonry and M. Davis, 1979, Astron. J, vol. 84, pp.1511-1525.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The spectrum to be compared with
C                 a template spectrum.
C     TEMPLATE    (Character) The template spectrum to be used.
C                 The two spectra should be the same length.
C     XSTART      (Numeric) Data with an axis data value less than
C                 XSTART will be ignored in the cross-correlation.
C     XEND        (Numeric) Data with an axis centre value greater than
C                 XEND will also be ignored.  Note that these values are
C                 used to determine the channel numbers to be used
C                 for SPECTRUM, and the same ones will be used for
C                 TEMPLATE, even if TEMPLATE has a  different axis
C                 structure.
C     CBPC        (Numeric) Percentage of spectrum covered by a cosine
C                 bell prior to application of the FFT.
C     CROSS       (Character) the name of the data structure to hold
C                 the cross-correlation, if it is to be saved.
C                 The file created will be cross.dst, and will look
C                 like an ordinary spectrum - ie can be plotted by
C                 SPLOT, etc.  CROSS is ignored if RECORD is not
C                 specified.
C
C     Command keywords -
C
C     FITCONT     If specified, a continuum fit is performed on the
C                 two spectra prior to application of the cosine bell.
C     RECORD      If specified, the cross-correlation of the two
C                 spectra will be recorded as a new data structure.
C
C     User variables used -
C
C     SHIFT       (Numeric) The relative shift of the two spectra.
C
C                                             KS / CIT 3rd Oct 1983
C     Modified:
C
C     6th  May 1985   KS / AAO.  FITCONT and CBPC parameters added.
C     21st Nov 1988   JM / RAL. Modified to use DSA_ routines.
C                     Dynamic memory handling changed to use
C                     DYN_ routines
C     23rd Aug 1990   KS / AAO. Minor tidying. Changed STATUS in
C                     PAR_WRUSER calls to IGNORE, modified calculation
C                     of workspace pointers.
C     16th Jan 1991   JMS / AAO. Included PAR_ABORT and STATUS checks
C                     for user-requested aborts.
C     21st Jan 1991   JMS / AAO. Restricted maximum allowed dimensions
C                     of data arrays to 1D.
C     12th Feb. 1991  JMS / AAO. Included check to test that the data
C                     arrays of the two spectra match in size.
C     13th Feb. 1991  JMS / AAO. Added an extra check to test that the
C                     X-axes match. Now aborts if specified range is of
C                     zero length.
C     14th Feb. 1991  JMS / AAO. Changed minimum range length to two
C                     pixels.
C     22nd Feb. 1991  JMS / AAO. Changed minimum range length to three
C                     pixels.
C     23rd Sep. 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                     PAR_WRUSER rather than DSA_WRUSER.
C     21st Dec. 2000  ACD / UoE, Starlink.  Removed unused variable.
C     21st Feb. 2001  ACD / UoE, Starlink.  Added an information/warning
C                     message when fewer than 20 points are being
C                     correlated.
C     28th Sep. 2001  ACD / UoE, Starlink.  Changed the mapping of work
C                     arrays from the `Figaro style' to the `Starlink
C                     style' in an attempt to cure array overwriting and
C                     initialisation problems.  Note that the resulting
C                     application is a hybrid of the Figaro and Starlink
C                     styles.
C     12th Sep. 2002  ACD / UoE, Starlink.  Added references to the
C                     prologue comments.
C     2005 May 31     MJC / Starlink Use CNF_PVAL for pointers to mapped
C                     data.
C     2005 June 10    MJC / Starlink Use %VAL(CNF_PVAL()) for pointers
C                     instead of DYNAMIC_MEM().
C+
      IMPLICIT NONE
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      LOGICAL PAR_ABORT
      INTEGER DSA_TYPESIZE
C
C     Local variables.  FT = Fourier transform
C
      INTEGER   AR0PTR           ! Pointer to the first data array
      INTEGER   AR1PTR           ! Pointer to the second data array
      INTEGER   BDOUB            ! Number of bytes per double-prec.
                                 ! number
      INTEGER   BFLOAT           ! Number of bytes per floating-point
                                 ! number
      CHARACTER BUFFER*75        ! Output buffer for message
      INTEGER   BUFLEN           ! Length of BUFFER (excl. trail.
                                 ! blanks)
      INTEGER   BYTES            ! Number of bytes
      REAL      CBPC             ! Cosine bell percentage coverage
      LOGICAL   CFIT             ! If false, disables the usual
                                 ! continuum fit
      INTEGER   CFNPTR           ! Pointer to the correlation function
      INTEGER   CPTR             ! Dynamic memory pointer to CORRL data
      INTEGER   DIMS(1)          ! Accommodates data dimensions
      INTEGER   FT0PTR           ! Pointer to the transform of first
                                 ! data array
      INTEGER   FT1PTR           ! Pointer to the transform of
                                 ! second data array
      INTEGER   FTCPTR           ! Pointer to the transform of
                                 ! correlation
      INTEGER   IGNORE           ! Status for VAR_SETNUM and PAR_WRUSER
                                 ! calls
      LOGICAL   ISNEW            ! Is address new to CNF?
      INTEGER   IXST             ! Pixel number associated with XSTART
      INTEGER   IXEN             ! Pixel number associated with XEND
      INTEGER   KZ(4)            ! Defines the cosine bell used to
                                 ! filter the FTs
      INTEGER   NDIM             ! Number of dimensions in data
      INTEGER   NELM             ! Number of data elements in object
      INTEGER   NEXT             ! Used in encoding STRING to report
                                 ! answer
      LOGICAL   NORM             ! Cross-correlation fn. to be
                                 ! normalised?
      INTEGER   NX0              ! Number of elements in the two spectra
      INTEGER   NX               ! Either equal to NX or next highest
                                 ! power of 2
      INTEGER   NXCMP            ! No. of REAL elements in a COMPLEX
                                 ! array
      LOGICAL   RECORD           ! Correlation fn. to be written to
                                 ! file?
      REAL      SHIFT            ! Shift of peak of correlation fn. from
                                 ! zero point
      INTEGER   SLOT             ! Slot number
      INTEGER   SPTR             ! Pointer to SPECT data
      INTEGER   STATUS           ! Running status for DSA_ routines
      CHARACTER STRING*64        ! String used to report answer
      INTEGER   TPTR             ! Pointer to TEMPLATE data
      REAL      WIDTH            ! The width of the correlation function
      INTEGER   WPTR             ! Temporary pointer
      REAL      XEND             ! Last axis data value used
      REAL      XSTART           ! First axis data value used
      REAL      ZPC              ! Percentage of spectrum covered at
                                 ! each end by a cosine bell prior to
                                 ! fourier transformation
      INTEGER   XVPTR            ! Pointer to the work array
C
C     Initial values
C
      STATUS=0
C
C     Open DSA
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Get byte size for float and double precision numbers
C
      BFLOAT=DSA_TYPESIZE('FLOAT',STATUS)
      BDOUB=DSA_TYPESIZE('DOUBLE',STATUS)
C
C     Get the name of SPECTRUM and open the file
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Ditto TEMPLATE
C
      CALL DSA_INPUT('TEMPL','TEMPLATE',STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Get sizes of both data arrays
C
      CALL DSA_DATA_SIZE ('SPECT',1,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
      CALL DSA_DATA_SIZE ('TEMPL',1,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
      CALL DSA_MATCH_SIZES('SPECT','TEMPL',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
      CALL DSA_MATCH_AXIS('SPECT',1,'TEMPL',1,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Use the utility routine DSA_AXIS_RANGE to get XSTART and XEND, then reset
C     NX0 to reflect the length of the spectrum to be actually used
C
      CALL DSA_AXIS_RANGE('SPECT',1,' ',.FALSE.,XSTART,XEND,
     :                     IXST,IXEN,STATUS)
C
C     Check the specified range (exit if less than or equal to three pixels)
C     This is done because if range is too small it causes a stack dump to
C     occur.
C
      IF ((IXEN-IXST).LE.3) THEN
         CALL PAR_WRUSER('The range you have specified is too small.'//
     :      ' Try again with different XSTART and XEND values.',STATUS)
         GOTO 500
      END IF
C
      NX0=IXEN-IXST+1
      IF(STATUS.NE.0)GOTO 500
C
C     Issue a warning message if fewer than 20 points are being correlated.
C
      IF(NX0.LE.20) THEN
         BUFFER = ' '
         BUFLEN = 0
         CALL CHR_PUTC ('The region cross-correlated contains only ',
     :     BUFFER, BUFLEN)
         CALL CHR_PUTI (NX0, BUFFER, BUFLEN)
         CALL CHR_PUTC ('points', BUFFER, BUFLEN)
         CALL PAR_WRUSER (BUFFER(1 : BUFLEN), STATUS)
         CALL PAR_WRUSER ('the results might not be very significant.',
     :     STATUS)
      END IF
C
C     The cross-correlation needs a lot of workspace, so grab that now.
C
      CALL GEN_POWER2(NX0,NX)
C     print4001, nx0,nx
C4001 format(1x, 'nx0,nx: ', i6, i6)
C
      CALL PSX_CALLOC (NX, '_REAL', AR0PTR, STATUS)
      CALL PSX_CALLOC (NX, '_REAL', AR1PTR, STATUS)
      CALL PSX_CALLOC (NX, '_REAL', XVPTR, STATUS)
      CALL PSX_CALLOC (NX, '_REAL', CFNPTR, STATUS)
C
      NXCMP = NX * 2
      CALL PSX_CALLOC (NXCMP, '_REAL', FT0PTR, STATUS)
      CALL PSX_CALLOC (NXCMP, '_REAL', FT1PTR, STATUS)
      CALL PSX_CALLOC (NXCMP, '_REAL', FTCPTR, STATUS)
      IF (STATUS.NE.0) THEN
         CALL ERR_FLUSH (STATUS)
         CALL PAR_WRUSER ('Failed to map work arrays.', STATUS)
         STATUS=1
         GOTO 500
      END IF
C
C     See if continuum is to be fitted, and get cosine bell coverage
C
      CALL PAR_RDKEY('FITCONT',.TRUE.,CFIT)
      CALL PAR_RDVAL('CBPC',0.,100.,10.,'percent',CBPC)
C
C     Find out if the cross-correlation is to be recorded, and if so,
C     get the name of the output file.
C
      CALL PAR_RDKEY('RECORD',.FALSE.,RECORD)
      IF (PAR_ABORT())GOTO 500         ! User requested abort
      IF (RECORD) THEN
         CALL DSA_OUTPUT('CORRL','CROSS',' ',0,0,STATUS)
      END IF
C
C     Read the spectrum and template data into the work arrays
C
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',WPTR,SLOT,STATUS)
      CALL DYN_INCAD(WPTR,'FLOAT',IXST-1,SPTR,ISNEW,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
      BYTES=BFLOAT*NX0
      CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(SPTR)),%VAL(CNF_PVAL(AR0PTR)))
      IF (ISNEW) CALL CNF_UNREGP(SPTR)
C
      CALL DSA_MAP_DATA('TEMPL','READ','FLOAT',WPTR,SLOT,STATUS)
      CALL DYN_INCAD(WPTR,'FLOAT',IXST-1,TPTR,ISNEW,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
      BYTES=BFLOAT*NX0
      CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(TPTR)),%VAL(CNF_PVAL(AR1PTR)))
      IF (ISNEW) CALL CNF_UNREGP(TPTR)
C
C     Pick reasonable values for the fourier domain filter - this
C     section could be refined, but these will do..
C
      ZPC=CBPC
      KZ(1)=5
      KZ(2)=MIN(20,NX-2)
      KZ(3)=MAX(NX/6,KZ(2)+1)
      KZ(4)=MIN(2*KZ(3),NX)

C
C     Perform the cross-correlation
C
      NORM=.TRUE.
C     print4000, 'before FIG_CROSS'
C4000 format(1x, a)
      CALL FIG_CROSS (%VAL(CNF_PVAL(AR0PTR)), %VAL(CNF_PVAL(AR1PTR)),
     :                NX0, NX, CFIT, ZPC, KZ, NORM,
     :                %VAL(CNF_PVAL(FT0PTR)), %VAL(CNF_PVAL(FT1PTR)),
     :                %VAL(CNF_PVAL(FTCPTR)), %VAL(CNF_PVAL(XVPTR)),
     :                %VAL(CNF_PVAL(CFNPTR)), SHIFT, WIDTH)
C     print4000, 'after FIG_CROSS'
C
C     Release work space.
C
      CALL PSX_FREE (AR0PTR, STATUS)
      CALL PSX_FREE (AR1PTR, STATUS)
      CALL PSX_FREE (XVPTR, STATUS)
      CALL PSX_FREE (FT0PTR, STATUS)
      CALL PSX_FREE (FT1PTR, STATUS)
      CALL PSX_FREE (FTCPTR, STATUS)

C
      STRING='Calculated shift is '
      CALL ICH_ENCODE(STRING,SHIFT,21,3,NEXT)
      STRING(NEXT:)=' elements.'
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER(STRING,STATUS)

C
C     Set the user variable SHIFT
C
      CALL VAR_SETNUM('SHIFT',0,0,SHIFT,IGNORE)
C
C     Now, if required, create the output structure for the
C     cross-correlation.
C
      IF (RECORD) THEN
         CALL DSA_RESHAPE_DATA('CORRL','SPECT',1,NX,STATUS)
         CALL DSA_MAP_DATA('CORRL','WRITE','FLOAT',CPTR,SLOT,STATUS)
         IF(STATUS.NE.0)GOTO 500

         BYTES=NX*BFLOAT
         CALL GEN_MOVE(BYTES, %VAL(CNF_PVAL(CFNPTR)),
     :                 %VAL(CNF_PVAL(CPTR)))
      END IF
      CALL PSX_FREE (CFNPTR, STATUS)

  500 CONTINUE
C
C     Close down everything
C
      CALL ERR_CLEAR (STATUS)
      CALL DSA_CLOSE(STATUS)

      END




