      SUBROUTINE DYNAMICAL( STATUS )
*+
*  Name:
*     DYNAMICAL

*  Purpose:
*     Produce dynamical power spectrum using FFT

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL DYNAMICAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Calculates the power spectrum of successive segments (of length 2**N)
*     of a 1D data array, and stacks the resulting spectra to form a dynamical
*     power spectrum.
*     Normalization of the power spectra is:
*         power = (wave amplitude/2)**2 = F.T.of autocovariance.
*     The zero frequency power is omitted from the output array since it
*     tends to dominate plots if included, and makes taking of logs difficult
*     if zeroed.

*  Usage:
*     dynamical {parameter_usage}

*  Environment Parameters:
*     {parameter_name}[pdims] = {parameter_type} ({parameter_access_mode})
*        {parameter_description}

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     Fast FFT routine (FFA8) is used for speed.
*     Arrays are mapped so there is no size limitation.

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     The data are assumed to be regularly spaced.
*     Errors & Quality ignored.
*     HISTORY should contain more info.
*     Only handles 1d input data.

*  References:
*     {task_references}...

*  Keywords:
*     dynamical, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1986 (TJP):
*        Original
*     11 Feb 1986 (TJP):
*        New ordering for 2D arrays
*     20 Mar 1986 (TJP):
*        Zero frequency power excluded (TJP)
*     14 Apr 1986 (TJP):
*        Bug fixed, continuous display flag inserted
*      3 Jun 1987 (PLA):
*        Rewriten to ROSAT specification
*      2 Sep 1987 (PLA):
*        Copies history file from input
*      3 Sep 1987 (PLA):
*        Accepts primitive object as input. History contains No. & length
*        of sections. Also, SECTION_SIZE component commented out
*     19 Jan 1988 (PLA):
*        History bug fixed
*     14 Jul 1988 V1.0-0 (PLA):
*        Converted to new STARLINK HDS file standards. Now uses DYN_ for
*        virtual memory. REG_SPACED removed, no longer needed : NOISE
*        component commented out. GRAF_LEGEND commented out, now writes
*        to TITLE.
*      8 Dec 1989 V1.0-2 (DJA):
*        Bug fixed in HIST_PTXT
*      8 Oct 1992 V1.7-0 (DJA):
*        Use common copy of TIM_FPOWER with POWER
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*      8 Dec 1995 V2.0-0 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL                  CHR_LEN
        INTEGER                 CHR_LEN

*  Local Constants:
      INTEGER                   MAXLIN
        PARAMETER               ( MAXLIN = 9 )
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'DYNAMICAL Version V2.0-0' )

*  Local Variables:
      CHARACTER*80              ACTION(MAXLIN)		! What the program does.
      CHARACTER*40           	DUNITS       		! Data units
      CHARACTER*40           	TUNITS       		! Time units
      CHARACTER*80           	PUNITS       		! Power units
      CHARACTER*80           	FUNITS       		! Frequency units

      REAL                   	BASE         		! Base of input axis values
      REAL                   	DX           		! Data spacing
      REAL			SPARR(2)		! Spaced array data
      REAL                   	X1           		! 1st and 2nd  x values

      INTEGER			APTR			! Ptr in i/p data
      INTEGER                	DIMS(ADI__MXDIM)	! Size of each dimension
      INTEGER                	DPTR         		! Pointer to output dynamical
							! power spectrum
      INTEGER			IFID			! Input dataset id
      INTEGER                	LSECT        		! Length of data sections
      INTEGER                	NDIM         		! I/p dimensionality
      INTEGER                	NELM         		! No.of data points
      INTEGER                	NSECT        		! Number of data sections
      INTEGER                	NV           		! Number of freqs in power spec.
      INTEGER			PFID			! Output dataset id
      INTEGER                	TLEN         		! Length of a character string
      INTEGER                	USED         		! Number of history records used
      INTEGER                   WPTR         		! Pointer to work array
      INTEGER                	YPTR         		! Pointer to input data array

      LOGICAL                	OK           		! General test
      LOGICAL                	PRIM         		! Input primitive object?
      LOGICAL                	REG          		! Is the input data regularly spaced?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Obtain data object, access and check it.
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
      CALL ADI_DERVD( IFID, 'Array', PRIM, STATUS )

*  Get input dimensions
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF ( OK ) THEN
        CALL BDI_MAPR( IFID, 'Data', 'READ', YPTR, STATUS )
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'ERROR : No numeric data object found.',
     :                                                     STATUS )
      END IF

*  Inform user if quality is present
      CALL BDI_CHK( IFID, 'Quality', OK, STATUS )
      IF ( OK ) THEN
        CALL MSG_PRNT( '** Quality is present in this input file - '/
     :                               /'DYNAMICAL will ignore it **' )
      END IF

*    Check status.
      IF ((STATUS .EQ. SAI__OK) .AND. (NDIM .EQ. 1)) THEN

*    Look for independent variable - if present & correct then map it
        DX = 1.0
        BASE = 1.0
         IF ( PRIM ) THEN
           OK = .FALSE.
           CALL MSG_PRNT( 'Primitive data : Unit spacing assumed' )

         ELSE
           CALL BDI_AXCHK( IFID, 1, 'Data', OK, STATUS )
           IF ( OK ) THEN
             CALL BDI_AXMAPR( IFID, 1, 'Data', 'READ', APTR, STATUS )
             CALL ARR_CHKREG( %VAL(APTR), DIMS(1), REG, BASE, DX,
     :                        STATUS )
             IF ( .NOT. REG ) THEN
               CALL MSG_PRNT( 'DYNAMICAL requires regularly spaced '/
     :           /'axis values - will continue using pixel numbers' )
               BASE = 1.0
               DX = 1.0
             END IF
           END IF
         END IF

*     Inform user about length of data set
         CALL MSG_SETI( 'NDAT', NELM )
         CALL MSG_PRNT( '^NDAT data points entered' )

*     User input
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

*        Flush any errors
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
            END IF

*        Create & map workspace
            CALL DYN_MAPR( 1, LSECT, WPTR, STATUS )

*        Create the output object
            CALL USI_CREAT( 'OUT', ADI__NULLID, PFID, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Create components in output file
*        First the data array
            NV = LSECT/2
            DIMS(1) = NV
            DIMS(2) = NSECT
            CALL BDI_LINK( 'PowerSpectrum', 2, DIMS, 'REAL',
     :                     PFID, STATUS )
            CALL BDI_MAPR( PFID, 'Data', 'WRITE', DPTR, STATUS )

*        Read the input units
            IF ( .NOT. PRIM ) THEN
              CALL BDI_GET0C( IFID, 'Units', DUNITS, STATUS )
              CALL BDI_AXGET0C( IFID, 1, 'Units', TUNITS, STATUS )
            END IF

*        Compute dynamical power spectrum
            CALL DYNAM_DOIT( NELM, %VAL(YPTR), LSECT, NSECT, NV,
     :                          %VAL(WPTR), %VAL(DPTR), STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Write things general to all output
            CALL BDI_PUT0C( PFID, 'Title',
     :                      'Dynamical power spectrum', STATUS )
            CALL BDI_PUT0C( PFID, 'Label', 'Power', STATUS)

*        Put the frequency and time information into the axis arrays. Where
*        does X1 come from!
            SPARR(1) = 1.0/(DX*LSECT)
            SPARR(2) = SPARR(1)
            CALL BDI_AXPUT1R( PFID, 1, 'SpacedData', 2, SPARR,
     :        STATUS )
            SPARR(1) = X1 + (LSECT-1)*DX/2.0
            SPARR(2) = LSECT * DX
            CALL BDI_AXPUT1R( PFID, 2, 'SpacedData', 2, SPARR,
     :                        STATUS )

*        Set axis labels. Units depend on whether input was primitive
            CALL BDI_AXPUT0C( PFID, 1, 'Label', 'Frequency', STATUS )
            CALL BDI_AXPUT0C( PFID, 2, 'Label', 'Time', STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99

*          Do output specific to non-primitive input
            IF ( .NOT. PRIM ) THEN

*             Set units
               IF ( DUNITS .GT. ' ' ) THEN
                  PUNITS = '('//DUNITS(1:CHR_LEN(DUNITS))//')**2'
                  CALL BDI_PUT0C( PFID, 'Units', PUNITS, STATUS )
               END IF

*             Set the axis units
               IF ( TUNITS .GT. ' ' ) THEN
                  FUNITS = '('//TUNITS(1:CHR_LEN(TUNITS))//')**-1'
                  CALL BDI_AXPUT0C( PFID, 1, 'Units', FUNITS, STATUS )
               END IF

*           Copy ancillaries
               CALL UDI_COPANC( IFID, 'grf', PFID, STATUS )

*           Copy HISTORY structure
               CALL HSI_COPY( IFID, PFID, STATUS )
               IF (STATUS .NE. SAI__OK) THEN
                 CALL ERR_FLUSH(STATUS)
               END IF

            ELSE

*           Create new HISTORY structure since input is primitive
               CALL HSI_NEW( PFID, STATUS )

            END IF

            IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Add info to HISTORY structure
            CALL HSI_ADD( PFID, VERSION, STATUS )

            ACTION(1) = 'Input dataset {INP}'
            CALL MSG_SETI( 'NDAT', NELM )
            CALL MSG_SETI( 'NSECT', NSECT )

            CALL MSG_MAKE( 'Original ^NDAT data points split into '/
     :                                  /'^NSECT', ACTION(2), TLEN )
            CALL MSG_SETI( 'LSECT', LSECT )
            CALL MSG_MAKE( 'sections, each containing ^LSECT values.',
     :                                               ACTION(3), TLEN )

            USED = MAXLIN
            CALL USI_TEXT( 3, ACTION, USED, STATUS )
            CALL HSI_PTXT( PFID, USED, ACTION, STATUS )

         END IF
      ELSE

         IF ( (NDIM .NE. 1) .AND. (STATUS .EQ. SAI__OK) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('NOT_1D', 'ERROR : Data are not one '/
     :                                  /'dimensional', STATUS)

         END IF
      END IF

*  Exit
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  DYNAM_DOIT - Dynamical power spectrum routine
      SUBROUTINE DYNAM_DOIT( NDAT, DATA, LSECT, NSECT, NFREQ, WORK,
     :                                              POWER, STATUS )
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
        GOTO 99

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
        GOTO 99

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

*  Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'DYNAM_DOIT', STATUS )
      END IF

      END
