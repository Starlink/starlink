
      SUBROUTINE GTMLIM( IBOUND, MINSIG, MAXSIG, MINTHR, MAXTHR, MINMIX,
     :                   MAXMIX, STATUS )
*+
*  Name:
*     GTMLIM

*  Purpose:
*     To return the minisation limits required for use in PISAFIT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GTMLIM( IBOUND, MINSIG, MAXSIG, MINTHR, MAXTHR, MINMIX,
*     :            MAXMIX, STATUS )

*  Description:
*     The routine accesses the parameter MINMODE. If the minmode
*     is not returned as one of APM, NONE, or USER (shortened to A, N or
*     U ) then he is reprompted, until an error is returned or a valid
*     value is returned. If the return is APM then the minimisation
*     limits are set to those as in the original APM version of PISAFIT.
*     If the return is NONE then IBOUND is set to 2 which signifies that
*     no limits are to be placed on the minimisation, no other
*     parameters are accessed. If the return is USER then the user is
*     prompted for upper and lower limits for MINSIG, MAXSIG, MINTHR,
*     MAXTHR, MINMIX and MAXMIX, these are then returned.

*  ADAM Parameters:
*     MINMODE = LITERAL (Read)
*        Returns a string which must have a value of A, N, or U.
*     GSIGMRANGE = _REAL (Read)
*        Returns a user defined range for GSIGM values.
*     CROSSRANGE = _REAL (Read)
*        Returns a user defined range for CROSS values.
*     COMIXRANGE = _REAL (Read)
*        Returns a user defined range for COMIX values.

*  Arguments:
*     IBOUND = INTEGER (Returned)
*        Determines which type of minimisation occurs, 0 = bound
*        2 = only bound by being greater than zero.
*     MINSIG = REAL (Returned)
*        The minimum value which the gaussian sigma can take if
*        required. If not required then this value remains set at the
*        APM value
*     MAXSIG = REAL (Returned)
*        The maximum value which the gaussian sigma can take if
*        required. If not required then this value remains set at the
*        APM value
*     MINTHR = REAL (Returned)
*        The minimum which the fractional cross over value can take if
*        required. If not required then this value remains set at the
*        APM value.
*     MAXTHR = REAL (Returned)
*        The maximum which the fractional cross over value can take if
*        required. If not required then this value remains set at the
*        APM value.
*     MINMIX = REAL (Returned)
*        The minimum which the fractional function mixing value can
*        take if required. If not required then this value remains set
*        at the APM value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1990 (PDRAPER):
*        Original version.
*     31-AUG-1996 (PDRAPER):
*        Re-write for use by non-NAG function.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PRM_PAR'         ! Primitive data constants

*  Arguments Returned:
      INTEGER IBOUND
      REAL MINSIG
      REAL MAXSIG
      REAL MINTHR
      REAL MAXTHR
      REAL MINMIX
      REAL MAXMIX

*  Status:
      INTEGER STATUS            ! Global status

*  Local Constants:
      REAL AMNSIG               ! APM minimum sigma value
      PARAMETER ( AMNSIG = 0.5 )
      REAL AMXSIG               ! APM maximum sigma value
      PARAMETER ( AMXSIG = 5.5 )
      REAL AMNTHR               ! APM minimum cross over value
      PARAMETER ( AMNTHR = 0.0001 )
      REAL AMXTHR               ! APM maximum cross over value
      PARAMETER ( AMXTHR = 0.995 )
      REAL AMNMIX               ! APM minimum mixture fraction
      PARAMETER ( AMNMIX = 0.0 )
      REAL AMXMIX               ! APM maximum mixture fraction
      PARAMETER ( AMXMIX = 0.12 )

*  Local Variables:
      CHARACTER MODE*( 4 )      ! buffer to receive chosen option
      INTEGER NRET              ! number of values returned
      REAL SIGMA( 2 )           ! array to receive GSIGM range
      REAL THR( 2 )             ! array to receive CROSS range
      REAL MIX( 2 )             ! array to receive COMIX range

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the mode
 1    CONTINUE
      CALL PAR_GET0C( 'MINMODE', MODE, STATUS )
      CALL CHR_UCASE( MODE )
      IF ( STATUS. EQ. SAI__OK ) THEN

*  Check for valid return, must start with A, U or N
         IF ( MODE( 1:1 ) .NE. 'A'  .AND.  MODE( 1:1 ) .NE. 'U'
     :        .AND.  MODE( 1:1 ) .NE. 'N' ) THEN
             CALL MSG_OUT( 'NOT_MODE', ' That is not a valid return.'//
     :                    ' Valid options are A(PM), U(SER) or N(ONE',
     :                     STATUS)

*  Try again
             CALL PAR_CANCL( 'MINMODE', STATUS )
             GO TO 1
         END IF
      END IF

*  Act on the return
      IF ( MODE( 1:1 ) .EQ. 'U' ) THEN

*  Get values for the minimum and maximum ranges
 2       CONTINUE
         CALL PAR_GET1R( 'GSIGMRANGE', 2, SIGMA, NRET, STATUS )
         IF ( NRET .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_OUT( 'MORE', ' Please supply TWO values', STATUS)
            CALL PAR_CANCL( 'GSIGMRANGE', STATUS )
            GO TO 2
         ENDIF
 3       CONTINUE
         CALL PAR_GET1R( 'CROSSRANGE', 2, THR, NRET, STATUS )
         IF ( NRET .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_OUT( 'MORE', ' Please supply TWO values', STATUS)
            CALL PAR_CANCL( 'CROSSRANGE', STATUS )
            GO TO 3
         ENDIF
 4       CONTINUE
         CALL PAR_GET1R( 'COMIXRANGE', 2, MIX, NRET, STATUS )
         IF ( NRET .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_OUT( 'MORE', ' Please supply TWO values', STATUS)
            CALL PAR_CANCL( 'CROSSRANGE', STATUS )
            GO TO 4
         ENDIF

*  Sort returns, convert cross over into fraction
         MINSIG = MIN( SIGMA( 1 ), SIGMA( 2 ) )
         MINTHR = MIN( THR( 1 ), THR( 2 ) ) * 0.01
         MINMIX = MIN( MIX( 1 ), MIX( 2 ) )
         MAXSIG = MAX( SIGMA( 1 ), SIGMA( 2 ) )
         MAXTHR = MAX( THR( 1 ), THR( 2 ) ) * 0.01
         MAXMIX = MAX( MIX( 1 ), MIX( 2 ) )
         IBOUND = 0
      ELSE IF ( MODE( 1:1 ) .EQ. 'N' ) THEN

*  Set IBOUND to 2 and free all limits to be above zero
         IBOUND = 2
         MINSIG = 0.0
         MINTHR = 0.0
         MINMIX = 0.0
         MAXSIG = VAL__MAXR
         MAXTHR = VAL__MAXR
         MAXMIX = VAL__MAXR
      ELSE

*  Use the APM simulation values.
         MINSIG = AMNSIG
         MAXSIG = AMXSIG
         MINTHR = AMNTHR
         MAXTHR = AMXTHR
         MINMIX = AMNMIX
         MAXMIX = AMXMIX
         IBOUND = 0
      END IF

      END
* $Id$
