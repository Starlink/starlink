*+  POWER - FFT power spectrum program.
      SUBROUTINE POWER( STATUS )
*
*    Description :
*
*     Calculates the power spectrum of a 1D data array using an FFT for
*     speed. Normalization used is:
*                 power=F*conj(F)=(wave amplitude/2)**2
*     where F(j)=(1/N)*sum{f(k)*exp(-i*2*pi*j*k/N)}.
*     Since the data are implicitly assumed to repeat periodically, the
*     jump between the end values introduces spurious high frequency power.
*     This can be reduced by tapering the ends of the data, after removal
*     of the data mean. The user may also wish to remove the data mean in
*     order to avoid a large zero order power component (which may dominate
*     plots - the other values are unaffected by its presence, since the
*     different frequency terms are orthogonal).
*     Although the transform can be calculated for any number of elements,
*     numbers containing large prime factors can take a very long time.
*     The user can therefore elect to truncate the data (by <10%) to avoid
*     this problem.
*
*    Method :
*
*     The basic technique is standard.
*     The data must not be modified by the programme, so a temporary data
*     object is used for scratch storage if mean-removal or tapering is
*     requested.
*     Arrays are mapped so there is no size limitation.
*
*    Deficiencies :
*
*     Errors & Quality ignored.
*     HISTORY should contain more info.
*     Only handles 1d input data.
*
*    Bugs :
*
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*     Phil Andrews   (BHVAD::PLA)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      8 Jan 86 : Original  (TJP)
*     22 Jan 86 : Bug in TIM_FPOWER fixed  (TJP)
*     20 Mar 86 : Array truncation option, COMMENTS removed  (TJP)
*      1 Sep 86 : SGS workstation name updated (TJP)
*     16 Mar 87 : Rewritten to ROSAT specification. Previous name: POWER (PLA)
*      9 Jun 88 : Converted to new STARLINK HDS file standards.
*                 No longer uses a temporary array for the data. (PLA)
*     14 Jun 90 : V1.2-0  Checks for irregular axes and bad quality (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    External references :
*
      INTEGER                CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ILOC          ! Input data object locator.
      CHARACTER*(DAT__SZLOC) OLOC          ! Locator to power spectrum obj.
      CHARACTER*80           DUNITS        ! Data units.
      CHARACTER*80           TUNITS        ! Time units.
      CHARACTER*80           TITLE         ! Of the dataset

      REAL                   BASE          ! Base value of input axis values
      REAL                   DX            ! Data spacing.

      INTEGER                DPTR          ! Pointer to TIM_FPOWER array.
      INTEGER                NBAD          ! # of bad points
      INTEGER                NDIMS         ! Dimensionality of data.
      INTEGER                NDAT          ! No.of data points.
      INTEGER                NEWPTR        ! Pointer to Output array.
      INTEGER                NTRUNC        ! No. of points after truncation.
      INTEGER                NV            ! Number of freqs in power spec.
      INTEGER                QNDIM         ! Number of quality dimensions
      INTEGER                QPTR          !
      INTEGER                YPTR          ! Pointer to input data array.
      INTEGER                LDIM(DAT__MXDIM)! Size of each dimension.
      INTEGER                QDIMS(DAT__MXDIM)!

      LOGICAL                ANYBAD        ! Any bad quality points
      LOGICAL                DEMEAN        ! Remove data mean?
      LOGICAL                INPRIM        ! Input is primitive object.
      LOGICAL                OK            ! General test
      LOGICAL                REG           ! Is the input data regularly spaced?
      LOGICAL                QOK           ! Quality there?
      LOGICAL                TAPER         ! Data taper required?
      LOGICAL                TRUNC         ! Data to be truncated?
*
*    Version id :
*
      CHARACTER*21           VERSION
         PARAMETER           ( VERSION = 'POWER Version 1.8-0' )
*-

*    Check status.
      IF (STATUS .NE. SAI__OK) RETURN

*    Version.
      CALL MSG_PRNT (VERSION)

*    Initialize ASTERIX
      CALL AST_INIT()

*    Obtain data object, access and check it.
      CALL USI_ASSOCI ('INP', 'READ', ILOC, INPRIM, STATUS)

*    Create a POWER_SPECTRUM object.
      CALL USI_ASSOCO ('OUT', 'POWER_SPECTRUM', OLOC, STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Get data object
      CALL BDA_CHKDATA( ILOC, OK, NDIMS, LDIM, STATUS )

      IF ( OK ) THEN
        CALL BDA_MAPDATA( ILOC, 'READ', YPTR, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: No data object found.',
     :                                                  STATUS )
      END IF
      NDAT = LDIM(1)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Check number of dimensions
      IF ( NDIMS .EQ. 1 ) THEN

*       Look for independent variable - if present & correct then map it
         CALL BDA_CHKAXVAL( ILOC, 1, OK, REG, NDAT, STATUS )

         IF ( OK .AND. REG ) THEN
            CALL BDA_GETAXVAL (ILOC, 1, BASE, DX, NDAT, STATUS)
         ELSE IF ( OK .AND. .NOT. REG ) THEN
            CALL MSG_PRNT( 'FATAL ERROR: Cannot operate on irreg'/
     :                                              /'ular data' )
            STATUS = SAI__ERROR
         ELSE
            DX = 1.0
            CALL MSG_PRNT ('WARNING: Invalid axis data - unit '/
     :                                      /'spacing assumed' )
         END IF

*       Check status
         IF (STATUS .NE. SAI__OK) GOTO 99

*       Check for bad quality
         CALL BDA_CHKQUAL( ILOC, QOK, QNDIM, QDIMS, STATUS )
         IF ( QOK ) THEN
            CALL BDA_MAPLQUAL( ILOC, 'READ', ANYBAD, QPTR, STATUS )
            IF ( ANYBAD ) THEN
               CALL ARR_NBAD( NDAT, %VAL(QPTR), NBAD, STATUS )
               CALL MSG_SETI( 'NBAD', NBAD )
               CALL MSG_PRNT( 'WARNING: There are ^NBAD quality '/
     :                            /'points present, aborting...' )
            ELSE
               NBAD = 0
            END IF
            CALL BDA_UNMAPLQUAL( ILOC, STATUS )
            IF ( NBAD .GT. 0 ) THEN
               STATUS = SAI__ERROR
               GOTO 99
            END IF
         END IF

*       Inform user about length of data set
         CALL MSG_SETI('NDAT', NDAT)
         CALL MSG_PRNT( '^NDAT data points entered' )

*       User input
         CALL USI_GET0L('TRUNCATE', TRUNC, STATUS)

*       Check status
         IF (STATUS .NE. SAI__OK) GOTO 99

         IF ( TRUNC ) THEN
            CALL TIM_TRUNC( NDAT, NTRUNC )
            CALL MSG_SETI('NTRUNC', NTRUNC )
            CALL MSG_PRNT( 'Truncated to ^NTRUNC values' )
            NDAT = NTRUNC
         END IF

*       User input
         CALL USI_GET0L ('TAPER', TAPER, STATUS)

         IF ( .NOT. TAPER ) THEN
            CALL USI_GET0L ('REMOVE_MEAN', DEMEAN, STATUS)
         END IF

*       Check status - drop out if bad.
         IF (STATUS .NE. SAI__OK) GOTO 99

         CALL DYN_MAPR (1, NDAT, DPTR, STATUS)

         IF ( TAPER .OR. DEMEAN ) THEN

*          Set up modified data array with mean subtracted
            CALL POWER_MEANSUB (NDAT, %VAL(YPTR), %VAL(DPTR))

         ELSE

*          Original array to be passed across
            CALL ARR_COP1R( NDAT, %VAL(YPTR), %VAL(DPTR), STATUS )

         END IF

*       Check status - drop out if bad.
         IF (STATUS .NE. SAI__OK) GOTO 99

*       Taper 10% of data at each end if required
*       Should the fraction tapered be user selectable??
         IF ( TAPER ) THEN
            CALL ARR_TAPER( %VAL(DPTR), NDAT, 0.1, STATUS )
         END IF

*       Compute power spectrum
         CALL TIM_FPOWER (NDAT, %VAL(DPTR), NV, STATUS)

*       Create components in output file.
*       Start with the axis values
         DX = 1.0 / ( REAL(NDAT) * DX )
         CALL BDA_CREAXES( OLOC, 1, STATUS )
         CALL BDA_PUTAXVAL( OLOC, 1, 0, DX, NV, STATUS )
         CALL BDA_PUTAXLABEL( OLOC, 1, 'Frequency', STATUS )
         CALL BDA_PUTLABEL( OLOC, 'Power', STATUS )

         IF ( .NOT. INPRIM ) THEN
            CALL BDA_GETTITLE( ILOC, TITLE, STATUS )
            CALL BDA_PUTTITLE( OLOC, TITLE, STATUS )
            CALL BDA_GETUNITS( ILOC, DUNITS, STATUS )

            IF (CHR_LEN(DUNITS) .GT. 0) THEN
               DUNITS = '('//DUNITS(1:CHR_LEN(DUNITS))//')**2'
               CALL BDA_PUTUNITS (OLOC, DUNITS, STATUS)
            END IF

            CALL BDA_GETAXUNITS (ILOC, 1, TUNITS, STATUS)
            IF ( CHR_LEN(TUNITS) .GT. 0 ) THEN
               TUNITS = '('//TUNITS(1:CHR_LEN(TUNITS))//')**-1'
               CALL BDA_PUTAXUNITS( OLOC, 1, TUNITS, STATUS)
            END IF

*          Create output data array
            CALL BDA_CREDATA( OLOC, 1, NV, STATUS )
            CALL BDA_MAPDATA( OLOC, 'WRITE', NEWPTR, STATUS )
            CALL ARR_COP1R( NV, %VAL(DPTR), %VAL(NEWPTR), STATUS )

*          Copy MORE structure.
            CALL BDA_COPMORE( ILOC, OLOC, STATUS )

*          Copy History.
            CALL HIST_COPY( ILOC, OLOC, STATUS)

         END IF

*       Add new history record. ! This should contain more info.
         CALL HIST_ADD( OLOC, VERSION, STATUS )

      ELSE IF ( NDIMS .NE. 1 ) THEN
         CALL ERR_REP( ' ', 'FATAL ERROR: Data is not one-dimensional',
     :                                                         STATUS )

      END IF

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  POWER_MEANSUB - Calculates data mean, and subtracts it from the data.
      SUBROUTINE POWER_MEANSUB( N, YIN, YOUT )
*    Description :
*      Subtracts mean from array YIN to give YOUT.
*      Mean is returned in YMEAN.
*    History :
*     8 Jan 86: original version (BHVAD::TJP)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                N             ! Number of data points.
      REAL                   YIN( * )      ! Input data vector.

*    Export :
      REAL                   YOUT( * )     ! Output mean subtracted data vector.

*    Local variables :
      INTEGER                LOOP          ! Dummy variable for DO loops.

      DOUBLE PRECISION       SUM           ! Dummy variable used in calc. of mean.
      DOUBLE PRECISION       YMEAN         ! Mean value of YIN.

*-

      SUM = 0.0D0

      DO LOOP = 1, N
        SUM = SUM + DBLE( YIN( LOOP ) )
      END DO

      YMEAN = SUM / DBLE( N )

      DO LOOP = 1, N
        YOUT( LOOP ) = REAL( DBLE( YIN(LOOP) ) - YMEAN )
      END DO

      CALL MSG_SETD ('YMEAN', YMEAN)
      CALL MSG_PRNT ('Data mean ^YMEAN subtracted')

      END




*+  TIM_TRUNC - converts array length for FFT routines
	SUBROUTINE TIM_TRUNC( NOLD, NNEW )
*    Description :
*	Given an array length NOLD, returns a length NNEW <= NOLD which
*	contains no large prime factors and will therefore allow rapid
*	Fourier transformation by FOURT.
*	No more than 10% of the data will be truncated.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*	  Version 1	(20-Mar-86)
*     5/9/88: ASTERIX88 version (PLA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      INTEGER                NOLD                          ! Number of data points
*    Export :
      INTEGER                NNEW                          ! Number of truncated data points
*    Local variables :
      INTEGER                M
      INTEGER                MULT                          ! Multiplier

      REAL                   BASE                          ! Base value

      LOGICAL                CONTINUE
*-

      IF ( NOLD .LE. 64) THEN
        NNEW = NOLD

      ELSE
*      Calculate D=log2(N)-INT[log2(N)]
	M = INT( ALOG10( REAL(NOLD) ) / ALOG10( 2.0 ) )

*      If already a power of two then return
	IF( NOLD. EQ. 2**M .OR. NOLD .EQ. 2**(M+1) ) THEN
	  NNEW = NOLD

        ELSE
*        Truncate
          CONTINUE = .TRUE.
          BASE     = 2.0 ** (M - 5)
          MULT     = 64

          DO WHILE ( CONTINUE )
            MULT = MULT - 2
            NNEW = REAL(MULT) * BASE

            IF ( NNEW .LE. NOLD ) THEN
              CONTINUE = .FALSE.

            END IF
          END DO
        END IF
      END IF
      END
