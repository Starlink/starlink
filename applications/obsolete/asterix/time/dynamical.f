*+  DYNAMICAL - FFT dynamical power spectrum program
      SUBROUTINE DYNAMICAL( STATUS )
*
*   Description :
*
*     Calculates the power spectrum of successive segments (of length 2**N)
*     of a 1D data array, and stacks the resulting spectra to form a dynamical
*     power spectrum.
*     Normalization of the power spectra is:
*         power = (wave amplitude/2)**2 = F.T.of autocovariance.
*     The zero frequency power is omitted from the output array since it
*     tends to dominate plots if included, and makes taking of logs difficult
*     if zeroed.
*
*   Method :
*
*     Fast FFT routine (FFA8) is used for speed.
*     Arrays are mapped so there is no size limitation.
*
*   Deficiencies :
*
*     The data are assumed to be regularly spaced.
*     Errors & Quality ignored.
*     HISTORY should contain more info.
*     Only handles 1d input data.
*
*   Bugs :
*   Authors :
*
*     Trevor Ponman   (BHVAD::TJP)
*     Phillip Andrews (BHVAD::PLA)
*     David Allan     (BHVAD::DJA)
*
*   History :
*
*     23 Jan 86 : Original
*     11 Feb 86 : New ordering for 2D arrays  (TJP)
*     20 Mar 86 : Zero frequency power excluded (TJP)
*     14 Apr 86 : Bug fixed, continuous display flag inserted (TJP)
*      3 Jun 87 : Rewriten to ROSAT spec. (pla_rosat@uk.ac.bham.sr.star)
*      2 Sep 87 : Copies history file from input (pla)
*      3 Sep 87 : Accepts primitive object as input.
*                 History contains No. & length of sections.
*                 Also, SECTION_SIZE component commented out (pla)
*     19 Jan 88 : History bug fixed. (PLA)
*     14 Jul 88 : Converted to new STARLINK HDS file standards. Now uses DYN_
*                 package to get virtual memory. REG_SPACED removed, no longer
*                 needed : NOISE component commented out. GRAF_LEGEND commented
*                 out, now writes to TITLE. (DJA)
*      8 Dec 89 : V1.0-2 Bug fixed in HIST_PTXT (DJA)
*      8 Oct 92 : V1.7-0 Use common copy of TIM_FPOWER with POWER (DJA)
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
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER		 	STATUS          ! Run-time status
*
*    Functions :
*
      INTEGER               	CHR_LEN		! Logical length of string
*
*   Local Constants :
*
      CHARACTER*24           VERSION
        PARAMETER         ( VERSION=' DYNAMICAL Version 1.8-0' )

      INTEGER                MAXLINES
         PARAMETER           (MAXLINES = 9)
*
*   Local variables :
*
      CHARACTER*(DAT__SZLOC) ILOC         ! Input data object locator
      CHARACTER*(DAT__SZLOC) PLOC         ! Locator to power spectrum obj
      CHARACTER*80           ACTION(MAXLINES)! What the program does.
      CHARACTER*40           DUNITS       ! Data units
      CHARACTER*40           TUNITS       ! Time units
      CHARACTER*80           PUNITS       ! Power units
      CHARACTER*80           FUNITS       ! Frequency units

      REAL                   BASE         ! Base of input axis values
      REAL                   DX           ! Data spacing
      REAL                   X1           ! 1st and 2nd  x values

      INTEGER                TLEN         ! Length of a character string
      INTEGER                NELM         ! No.of data points
      INTEGER                NDIM         ! Dimensionality of data
      INTEGER                NVAL         ! Number of values mapped
      INTEGER                NV           ! Number of freqs in power spec.
      INTEGER                YPTR         ! Pointer to input data array
      INTEGER                USED         ! Number of history records used
      INTEGER                WPTR         ! Pointer to work array
      INTEGER                VPTR         ! Pointer to output freqs
      INTEGER                TPTR         ! Pointer to output times
      INTEGER                DPTR         ! Pointer to output dynamical
                                          ! power spectrum
      INTEGER                LSECT        ! Length of data sections
      INTEGER                NSECT        ! Number of data sections
      INTEGER                DIMS(DAT__MXDIM)! Size of each dimension

      LOGICAL                OK           ! General test
      LOGICAL                PRIM         ! Input primitive object?
      LOGICAL                REG          ! Is the input data regularly spaced?
*-

*    Check status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Initialise ASTERIX system
      CALL AST_INIT

*    Obtain data object, access and check it.
      CALL USI_ASSOCI( 'INP', 'READ', ILOC, PRIM, STATUS )

*    Inform user if quality is present
      CALL BDA_CHKQUAL( ILOC, OK, NDIM, DIMS, STATUS )
      IF ( OK ) THEN
        CALL MSG_PRNT( '** Quality is present in this input file - '/
     :                               /'DYNAMICAL will ignore it **' )
      END IF

      CALL BDA_CHKDATA( ILOC, OK, NDIM, DIMS, STATUS )
      IF ( OK ) THEN
        CALL BDA_MAPDATA( ILOC, 'READ', YPTR, STATUS )
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'ERROR : No numeric data object found.',
     :                                                     STATUS )
      END IF

*    Check status.
      IF ((STATUS .EQ. SAI__OK) .AND. (NDIM .EQ. 1)) THEN

*       Look for independent variable - if present & correct then map it
         IF ( PRIM ) THEN
            OK = .FALSE.
            REG = .FALSE.
            DX = 1.0
            CALL MSG_PRNT( 'Primitive data : Unit spacing assumed.' )

         ELSE
            CALL BDA_CHKAXVAL( ILOC, 1, OK, REG, NVAL, STATUS )

            IF ( OK .AND. REG ) THEN
               CALL BDA_GETAXVAL( ILOC, 1, BASE, DX, NVAL, STATUS )

            ELSE
               DX = 1.0
               CALL MSG_PRNT( 'Invalid axis data - unit spacing'/
     :                                             /' assumed.' )

            END IF
         END IF

*       Inform user about length of data set
         CALL MSG_SETI( 'NDAT', NELM )
         CALL MSG_PRNT( '^NDAT data points entered' )

*       User input
         CALL MSG_PRNT
     :      (' Data set will be split into sections and transformed - ')
         CALL MSG_PRNT
     :      ('enter number of points in each section (this will be '
     :                                //'rounded to nearest 2**N)' )

         CALL USI_GET0I( 'SECTOR', LSECT, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
            LSECT = 2 ** NINT(ALOG10(REAL(LSECT))/ALOG10(2.0))

            IF ( LSECT .GT. NELM ) THEN
               LSECT = NELM

            END IF
            NSECT = INT(NELM/LSECT)
            CALL MSG_SETI( 'NSECT', NSECT )
            CALL MSG_SETI( 'LSECT', LSECT )
            CALL MSG_PRNT( 'Data will be split into ^NSECT'
     :                     //' sections of length ^LSECT' )

            IF ( NSECT .EQ. 1 ) THEN
               CALL MSG_PRNT( 'Only one power spectrum will be'/
     :                                            /' produced' )

            END IF

*          Flush status
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
            END IF

*          Create & map workspace
            CALL DYN_MAPR( 1, LSECT, WPTR, STATUS )

*          Create a POWER_SPECTRUM object
            CALL USI_ASSOCO( 'OUT', 'DYN_SPECTRUM', PLOC, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*          Create components in output file
*          First the data array
            NV = LSECT/2
            DIMS(1) = NV
            DIMS(2) = NSECT
            CALL BDA_CREDATA( PLOC, 2, DIMS, STATUS )
            CALL BDA_MAPDATA( PLOC, 'WRITE', DPTR, STATUS )

*          Read the input units
            IF ( .NOT. PRIM) THEN
               CALL BDA_GETUNITS( ILOC, DUNITS, STATUS )
               CALL BDA_GETAXUNITS( ILOC, 1, TUNITS, STATUS )
            END IF

*          Create the the new 2-axis structure and map the DATA_ARRAYs
            CALL BDA_CREAXES( PLOC, 2, STATUS )
            CALL BDA_CREAXVAL( PLOC, 1, .FALSE., NV, STATUS )
            CALL BDA_MAPAXVAL( PLOC, 'WRITE', 1, VPTR, STATUS )

            CALL BDA_CREAXVAL( PLOC, 2, .FALSE., NSECT, STATUS )
            CALL BDA_MAPAXVAL( PLOC, 'WRITE', 2, TPTR, STATUS )

*          Compute dynamical power spectrum
            CALL DYNAM_DOIT( NELM, %VAL(YPTR), LSECT, NSECT, NV,
     :                               %VAL(WPTR), %VAL(DPTR), STATUS )


*          Check status.
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*          Write things general to all output
            CALL BDA_PUTTITLE( PLOC, 'Dynamical power spectrum',
     :                                                  STATUS )
            CALL BDA_PUTLABEL( PLOC, 'Power', STATUS)

*          Put the frequency and time information into the axis arrays
            CALL DYNAM_FREQS( LSECT, DX, NV, %VAL(VPTR) )
            CALL DYNAM_TIMES( NSECT, LSECT, X1, DX, %VAL(TPTR) )

*          Set axis labels. Units depend on whether input was primitive
            CALL BDA_PUTAXLABEL( PLOC, 1, 'Frequency', STATUS )
            CALL BDA_PUTAXLABEL( PLOC, 2, 'Time', STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*          Do output specific to non-primitive input
            IF ( .NOT. PRIM ) THEN

*             Set units
               IF ( DUNITS .GT. ' ' ) THEN
                  PUNITS = '('//DUNITS(1:CHR_LEN(DUNITS))//')**2'
                  CALL BDA_PUTUNITS( PLOC, PUNITS, STATUS )
               END IF

*             Set the axis units
               IF ( TUNITS .GT. ' ' ) THEN
                  FUNITS = '('//TUNITS(1:CHR_LEN(TUNITS))//')**-1'
                  CALL BDA_PUTAXUNITS( PLOC, 1, FUNITS, STATUS )
                  CALL BDA_PUTAXUNITS( PLOC, 2, TUNITS, STATUS )
               END IF

*             Copy MORE structure
               CALL BDA_COPMORE( ILOC, PLOC, STATUS )

*             Copy HISTORY structure
               CALL HIST_COPY( ILOC, PLOC, STATUS )
               IF (STATUS .NE. SAI__OK) THEN
                 CALL ERR_FLUSH(STATUS)
               END IF

            ELSE

*             Create new HISTORY structure since input is primitive
               CALL HIST_NEW( PLOC, STATUS )

            END IF

            IF ( STATUS .NE. SAI__OK ) GOTO 999

*          Add info to HISTORY structure
            CALL HIST_ADD( PLOC, VERSION, STATUS )

            ACTION(1) = 'Input dataset {INP}'
            CALL MSG_SETI( 'NDAT', NELM )
            CALL MSG_SETI( 'NSECT', NSECT )
            CALL MSG_SETI( 'LSECT', LSECT )

            CALL MSG_MAKE( 'Original ^NDAT data points split into '/
     :                                  /'^NSECT', ACTION(2), TLEN )
            CALL MSG_MAKE( 'sections, each containing ^LSECT values.',
     :                                               ACTION(3), TLEN )

            USED = MAXLINES
            CALL USI_TEXT( 3, ACTION, USED, STATUS )
            CALL HIST_PTXT( PLOC, USED, ACTION, STATUS )

         END IF
      ELSE

         IF ( (NDIM .NE. 1) .AND. (STATUS .EQ. SAI__OK) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('NOT_1D', 'ERROR : Data are not one '/
     :                                  /'dimensional', STATUS)

         END IF
      END IF

*    Exit
 999  CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  DYNAM_FREQS - Generates frequencies at which FFT is calculated.
      SUBROUTINE DYNAM_FREQS(NDAT,DX,NV,V)
*
*    Description :
*
*      Generates an array of frequencies at which FFT is calculated for
*      a set of NDAT data points with spacing DX. (Zero freq. omitted.)
*
*    History :
*     23 Jan 86: Original       (tjp@uk.ac.bham.sr.star)
*      1 Apr 87: Headers added. (pla@uk.ac.bham.sr.star)
*     15 JUL 88: Name changed to conform with STARLINK package naming
*                conventions. (dja@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                NDAT         ! No.of data points
      INTEGER                NV           ! Number of freqs in power spec.

      REAL                   DX           ! Data spacing
*    Import-Export :
*    Export :
      REAL                   V(NV)        ! Array of frequencies at which FFT
                                          ! is found.
*    Local Constants :
*    Local variables :
      INTEGER                I            ! Dummy variable for loop.
*-
      DO I = 1, NV
        V(I) = I / (NDAT*DX)

      END DO

      END





*+   DYNAM_TIMES - Generates an array of start times for the data sections.
      SUBROUTINE DYNAM_TIMES(NSECT,LSECT,X1,DX,T)
*    Description :
*      Generates an array of mean times for the data sections.
*    History :
*     23 Jan 86: Original     (tjp@uk.ac.bham.sr.star)
*      1 Apr 87: Header added (pla@uk.ac.bham.sr.star)
*     15 JUL 88: Name changed to conform with STARLINK package naming
*                conventions. (dja@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                LSECT        ! Length of data sections
      INTEGER                NSECT        ! Number of data sections

      REAL                   X1           ! 1st x value.
      REAL                   DX           ! Data spacing
*    Import-Export :
*    Export :
      REAL                   T(NSECT)     ! Array containing the output times.
*    Local Constants :
*    Local variables :
      INTEGER                I            ! Dummy variable for loop.
      REAL                   FIRSTVALUE   ! First time value.
*-

      FIRSTVALUE = X1 + (LSECT - 1) * DX / 2.0

      DO I = 1, NSECT
        T(I) = FIRSTVALUE + (I - 1) * LSECT * DX

      END DO
      END


*+  DYNAM_DOIT - Dynamical power spectrum routine
      SUBROUTINE DYNAM_DOIT( NDAT, DATA, LSECT, NSECT, NFREQ, WORK,
     :                                                   POWER, STATUS )
*    Description :
*     Computes the power spectrum of NSECT consecutive sections of the DATA
*     array, each of length LSECT. The resulting set of power spectra is
*     returned in array POWER. First array index is frequency, second is time.
*     If NSECT<=0 on entry then as many sections of length LSECT as possible
*     are transformed.
*     The zero frequency power is excluded from the returned array, since
*     it is usually a nuisance, hence LSECT/2 values are returned for each
*     data section.
*    Method :
*     Calls TIM_FPOWER to calculate each power spectrum using an FFT.
*     Normalisation is  power=F*conj(F)=(wave amplitude/2)**2
*     where F(j)=(1/N)*sum{f(k)*exp(-i*2*pi*j*k/N)}.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     23 Jan 86: Original (TJP)
*     11 Feb 86: New data array order implemented (TJP)
*     20 Mar 86: Zero frequency power excluded (TJP)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                NDAT                          ! No. of input data
      INTEGER                LSECT                         ! Length of each data section
      INTEGER                NSECT                         ! No. of data sections
      INTEGER                NFREQ                         ! No. of output freqs.(LSECT/2)

      REAL                   DATA(NDAT)                    ! Input data

*    Export :
      REAL                   WORK(LSECT)                   ! Work array
      REAL                   POWER(NFREQ,NSECT)            ! Dynamical power spectrum

*    Status :
      INTEGER                STATUS

*    Local variables :
      INTEGER                NV                            ! NFREQ check
      INTEGER                I,J                           ! Loop counters
      INTEGER                NBASE                         ! Base array argument
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check NSECT,LSECT
      IF ( LSECT .LE. 0 ) THEN
        STATUS = SAI__ERROR
        GOTO 9999

      ELSE IF ( LSECT .GT. NDAT ) THEN
        LSECT = NDAT
        NSECT = 1

      ELSE IF ( NSECT .LE. 0 ) THEN
        NSECT = NDAT / LSECT

      ELSE IF ( NSECT * LSECT .GT. NDAT ) THEN
        NSECT = NDAT / LSECT

      END IF

*    Check NFREQ
      NV = LSECT / 2

      IF ( NFREQ .NE. NV ) THEN
        STATUS = SAI__ERROR
        GOTO 9999

      END IF

*    Compute dynamical spectrum
      DO I = 1, NSECT
        DO J = 1, LSECT
          NBASE   = (I - 1) * LSECT
          WORK(J) = DATA(NBASE + J)

        END DO
        CALL TIM_FPOWER( LSECT, WORK, NV, STATUS )

        DO J = 1, NFREQ
          POWER(J,I) = WORK(J + 1)

        END DO
      END DO

*    Exit
9999  IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP(' ', '...from DYNAM_DOIT', STATUS )

      END IF
      END
