*+  FIT_MODGET_TIES - Get constraints from model file
      SUBROUTINE FIT_MODGET_TIES( FLOC, NTIE, TSTART, TGROUP, STATUS )
*
*    Description :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     19 May 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) 	FLOC			! FIT_MODEL object
*
*    Export
*
      INTEGER 			NTIE      		! Number of ties
      INTEGER			TSTART(*)		! First tie parameter
      INTEGER			TGROUP(*)		! Tie group
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)	TLOC			! TIES object
      CHARACTER*(DAT__SZLOC)	TCLOC			! Particular TIES object

      INTEGER			ITIE			! Loop over ties
      INTEGER			ITPAR			! Loop over tied pars
      INTEGER			NDIM			! Dummy from DAT_SHAPE
      INTEGER			NTIESTR			! Dimensions of TIES
      INTEGER			NTPAR			! No. tied parameters
      INTEGER			TPARS(NPAMAX)		! Tied parameters

      LOGICAL 			THERE                   ! HDS component exists?
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Does a TIEs structure extist
      CALL DAT_THERE( FLOC, 'TIES', THERE, STATUS )
      NTIE = 0
      IF ( THERE ) THEN

*      Initialise
        CALL ARR_INIT1I( 0, NPAMAX, TGROUP, STATUS )

*      Locate structure
        CALL DAT_FIND( FLOC, 'TIES', TLOC, STATUS )

*      Get dimensions
        CALL DAT_SHAPE( TLOC, 1, NTIESTR, NDIM, STATUS )

*      Loop over interesting tie structures
        DO ITIE = 1, NTIESTR

*        Locate the ITIE'th tie
          CALL DAT_CELL( TLOC, 1, ITIE, TCLOC, STATUS )
          CALL DAT_THERE( TCLOC, 'PARS', THERE, STATUS )

*        Does this object contain a valid tie structure
          IF ( THERE ) THEN

*          Get tie data
            CALL CMP_GET1I( TCLOC, 'PARS', NPAMAX, TPARS,
     :                                    NTPAR, STATUS )

*          Increment counter
            NTIE = NTIE + 1
            IF ( NTIE .GT. MAXTIE ) THEN
              STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'Maximum number of ties exceeded',
     :                      STATUS )
              GOTO 99
            END IF

*          Store base parameter
            TSTART(NTIE) = TPARS(1)
            TGROUP(TPARS(1)) = ITIE

*          Mark other parameters as dependent, check for multiple dependency
            DO ITPAR = 2, NTPAR
              IF ( TGROUP(TPARS(ITPAR)) .NE. 0 ) THEN
                STATUS = SAI__ERROR
                CALL MSG_SETI( 'ONE', TGROUP(TPARS(ITPAR)) )
                CALL MSG_SETI( 'TWO', ITIE )
                CALL MSG_SETI( 'PAR', TPARS(ITPAR) )
                CALL ERR_REP( ' ', 'Model ties ^ONE and ^TWO have a'//
     :                       ' multiple dependency on parameter ^PAR',
     :                       STATUS )
                GOTO 99
              ELSE
                TGROUP(TPARS(ITPAR)) = ITIE
              END IF
            END DO

          END IF

*        Release this tie
          CALL DAT_ANNUL( TCLOC, STATUS )

*      Next tie
        END DO

*      Release TIES
        CALL DAT_ANNUL( TLOC, STATUS )

      END IF

*    Exit
 99   IF ( STATUS.NE.SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MODGET_TIES', STATUS )
      END IF

      END
