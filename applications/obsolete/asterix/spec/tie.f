*+  TIE - Adds, removes or displays fitting constraints
      SUBROUTINE TIE( STATUS )
*
*    Description :
*
*     Resets FROZEN flag (and optionally values) for parameters in a fit_model
*     data object.
*
*    Environment parameters :
*
*     MODEL=UNIV(U)
*		Object containing fit model
*     PARAMS=INTEGER()(R)
*		Numbers of parameters to be thawed
*     VALS=REAL()(R)
*               Values for thawed parameters
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     16 Mar 94 : V1.7-0 Original (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*100		CPARS			! Char version of TPARS
      CHARACTER*(DAT__SZLOC)	FLOC			! Fit model spec
      CHARACTER*79		TEXT			! Output text
      CHARACTER*(DAT__SZLOC)	TCLOC			! TIES object cell
      CHARACTER*(DAT__SZLOC)	TLOC			! TIES object
      CHARACTER*(DAT__SZTYP)	TYP			! Fit model spec type

      INTEGER			FSTAT			! i/o status code
      INTEGER			ITIE			! Loop over ties
      INTEGER			NDIM			! Returned by DAT_SHAPE
      INTEGER			NTIE			! Number of ties
      INTEGER			NTPAR			! Number of tied pars
      INTEGER			NUTIE			! Numbers of ties to use
      INTEGER			TIES(NPAMAX)		! Ties to SHOW/CANCEL
      INTEGER			TPARS(NPAMAX)		! Pars to tie

      LOGICAL			ADD			! ADD mode?
      LOGICAL			CANCEL			! CANCEL mode?
      LOGICAL			INPRIM			! Input primitive?
      LOGICAL			PTHERE			! TIES.PARS object exists?
      LOGICAL			SHOW			! SHOW mode?
      LOGICAL			THERE			! TIES object exists?
*
*    Version :
*
      CHARACTER*30 		VERSION
	PARAMETER		(VERSION = 'TIE Version 1.8-0')
*-

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialise Asterix
      CALL AST_INIT()

*    Access and check fit_model object
      CALL USI_ASSOCI( 'MODEL', 'UPDATE', FLOC, INPRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL DAT_TYPE(FLOC,TYP,STATUS)
      IF(TYP.NE.'FIT_MODEL')THEN
	STATUS=SAI__ERROR
	CALL ERR_REP( ' ', 'Not a fit_model data object', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get mode
      CALL USI_GET0L( 'ADD', ADD, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        ADD = .FALSE.
        CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 99
      END IF
      IF ( .NOT. ADD ) THEN
        CALL USI_GET0L( 'CANCEL', CANCEL, STATUS )
        IF ( STATUS .EQ. PAR__NULL ) THEN
          CANCEL = .FALSE.
          CALL ERR_ANNUL( STATUS )
        ELSE IF ( STATUS .NE. SAI__OK ) THEN
          GOTO 99
        END IF
        IF ( .NOT. CANCEL ) THEN
          CALL USI_GET0L( 'SHOW', SHOW, STATUS )
          IF ( STATUS .EQ. PAR__NULL ) THEN
            SHOW = .FALSE.
            CALL ERR_ANNUL( STATUS )
          ELSE IF ( STATUS .NE. SAI__OK ) THEN
            GOTO 99
          END IF
        END IF
      END IF

*    No mode specified
      IF ( .NOT. (ADD.OR.CANCEL.OR.SHOW) ) THEN
        SHOW = .TRUE.
      END IF

*    Does the TIES structure exist?
      CALL DAT_THERE( FLOC, 'TIES', THERE, STATUS )
      IF ( (CANCEL.OR.SHOW) .AND. .NOT. THERE ) THEN
        IF ( SHOW ) THEN
          CALL MSG_BLNK()
          CALL MSG_PRNT( '  * No ties present' )
        ELSE
          CALL MSG_PRNT( 'No ties to cancel!' )
        END IF
        GOTO 50

*    If the TIES structure doesn't exist, create an empty structure
      ELSE IF ( ADD .AND. .NOT. THERE ) THEN
        CALL DAT_NEW( FLOC, 'TIES', 'CONSTRAINT', 1, 1, STATUS )

      END IF

*    Locate the TIES structure
      CALL DAT_FIND( FLOC, 'TIES', TLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get dimensions
      CALL DAT_SHAPE( TLOC, 1, NTIE, NDIM, STATUS )

*    Get ties of interest
      IF ( ADD ) THEN
        IF ( THERE ) THEN
          CALL USI_DEF0I( 'NUM', NTIE+1, STATUS )
        ELSE
          CALL USI_DEF0C( 'NUM', '1', STATUS )
        END IF
        CALL PRS_GETLIST( 'NUM', NTIE+1, TIES, NUTIE, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          GOTO 99
        ELSE IF ( NUTIE .NE. 1 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Only one tie number allowed in ADD mode',
     :                                                         STATUS )
        END IF
      ELSE
        CALL PRS_GETLIST( 'NUM', NTIE, TIES, NUTIE, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Show mode?
      IF ( SHOW ) THEN

*      Loop over interesting ties
        CALL MSG_BLNK()
        CALL MSG_PRNT( ' Tie  Constraint' )
        CALL MSG_BLNK()
        DO ITIE = 1, NUTIE
          CALL DAT_CELL( TLOC, 1, TIES(ITIE), TCLOC, STATUS )
          CALL DAT_THERE( TCLOC, 'PARS', PTHERE, STATUS )
          IF ( PTHERE ) THEN
            CALL CMP_GET1I( TCLOC, 'PARS', NPAMAX, TPARS,
     :                                    NTPAR, STATUS )
            CALL STR_DIMTOC( NTPAR, TPARS, CPARS )
            WRITE( TEXT, '(2X,I2,2X,2A)', IOSTAT=FSTAT ) ITIE,
     :                         'Equal', CPARS(:CHR_LEN(CPARS))
          ELSE
            WRITE( TEXT, '(2X,I2,2X,A)', IOSTAT=FSTAT ) ITIE,'Cancelled'
          END IF
          CALL MSG_PRNT( TEXT )
          CALL DAT_ANNUL( TCLOC, STATUS )
        END DO

*      Free TIES object
        CALL DAT_ANNUL( TLOC, STATUS )

*    Cancel mode
      ELSE IF ( CANCEL ) THEN

*      Erase all the ties?
        IF ( NUTIE .EQ. NTIE ) THEN

*        Free TIES object
          CALL DAT_ANNUL( TLOC, STATUS )
          CALL DAT_ERASE( FLOC, 'TIES', STATUS )

        ELSE
          DO ITIE = 1, NUTIE
            CALL DAT_CELL( TLOC, 1, TIES(ITIE), TCLOC, STATUS )
            CALL DAT_THERE( TCLOC, 'PARS', PTHERE, STATUS )
            IF ( PTHERE ) THEN
              CALL DAT_ERASE( TCLOC, 'PARS', STATUS )
            END IF
            CALL DAT_ANNUL( TCLOC, STATUS )
          END DO

*        Free TIES object
          CALL DAT_ANNUL( TLOC, STATUS )

        END IF

*    ADD mode
      ELSE IF ( ADD ) THEN

*      The new tie number
        ITIE = TIES(1)

*      Extend structure by one element if it existed before we started
        IF ( ITIE .GT. NTIE ) THEN
          CALL DAT_ALTER( TLOC, 1, ITIE, STATUS )
          NTIE = ITIE
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Locate the ITIE'th cell
        CALL DAT_CELL( TLOC, 1, ITIE, TCLOC, STATUS )

*      Get tie info
        CALL USI_GET1I( 'PARS', NPAMAX, TPARS, NTPAR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Remove any existing data
        CALL DAT_THERE( TCLOC, 'PARS', PTHERE, STATUS )
        IF ( PTHERE ) THEN
          CALL MSG_SETC( 'N', ITIE )
          CALL MSG_PRNT( 'Overwriting existing tie number ^N...' )
          CALL DAT_ERASE( TCLOC, 'PARS', STATUS )
        END IF

*      Write to dataset
        CALL DAT_NEW1I( TCLOC, 'PARS', NTPAR, STATUS )
        CALL CMP_PUT1I( TCLOC, 'PARS', NTPAR, TPARS, STATUS )

*      Free the cell
        CALL DAT_ANNUL( TCLOC, STATUS )

*      Free TIES object
        CALL DAT_ANNUL( TLOC, STATUS )

      END IF

*    End of SHOW display
 50   IF ( SHOW ) CALL MSG_BLNK()

*    Exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
