      SUBROUTINE ERI0_REBIN_AST( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ERI0_REBIN_AST

*  Purpose:
*     Rebin an AsterixRMF using channel bounds or grouping

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI0_REBIN_AST( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
*     STATUS = INTEGER (given and returned)
*        The global status.

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
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private, rebinning, response

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      7 Apr 1995 (DJA):
*        Original version.
*     28 Mar 1996 (DJA):
*        Added rebinning by bounds
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      REAL			CBND			! Channel bound value
      REAL			CSPEC			! Channel spec value

      INTEGER                   BNDPTR			! Bounds array
      INTEGER			EPTR			! Energy bounds ptr
      INTEGER			I,J			! Loop variables
      INTEGER			ICBPTR			! i/p channel bounds
      INTEGER			ICIPTR			! i/p channel indices
      INTEGER			ICSPTR			! i/p channel specs
      INTEGER			IEIPTR			! i/p energy indices
      INTEGER			IGRP			! Grouping factor
      INTEGER			IRPTR			! i/p response data
      INTEGER                   LOCCH, HICCH            ! Elements of below
      INTEGER			MINCCH, MAXCCH		! Min/max contrib chans
      INTEGER                   NBND                    ! # channel bounds
      INTEGER			NCHAN			! # i/p channel bins
      INTEGER			NENER			! # energy bins
      INTEGER			NRMF			! # i/p response els
      INTEGER			RMFID			! Input RMF
      INTEGER			OCIPTR			! o/p channel indices
      INTEGER			OCBPTR			! o/p channel bounds
      INTEGER			OCSPTR			! o/p channel specs
      INTEGER			OEIPTR			! o/p energy indices
      INTEGER			ONCHAN			! # o/p channel bins
      INTEGER			ONRMF			! # o/p response els
      INTEGER			ORPTR			! o/p response data

      LOGICAL			GROUP			! Rebin by grouping?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get input args
      RMFID = ARGS(1)
      GROUP = (NARG.EQ.2)
      IF ( GROUP ) THEN
        CALL ADI_GET0I( ARGS(2), IGRP, STATUS )
      ELSE
        CALL ADI_GET0I( ARGS(2), NBND, STATUS )
        CALL ADI_MAPR( ARGS(3), 'READ', BNDPTR, STATUS )
      END IF

*  Create output
      CALL ADI_NEW0( 'AsterixRMF', OARG, STATUS )

*  Copy energy stuff which is unchanged
      CALL ADI_CGET0I( RMFID, 'NENERGY', NENER, STATUS )
      CALL ADI_CPUT0I( OARG, 'NENERGY', NENER, STATUS )
      CALL ADI_CMAPR( RMFID, 'Energy', 'READ', EPTR, STATUS )
      CALL ADI_CNEWV1R( OARG, 'Energy', NENER+1, %VAL(EPTR), STATUS )

*  Get old number of channel bins and calculate new
      CALL ADI_CGET0I( RMFID, 'NCHAN', NCHAN, STATUS )
      IF ( GROUP ) THEN
        ONCHAN = (NCHAN-1) / IGRP + 1
      ELSE
        ONCHAN = NBND - 1
      END IF
      CALL ADI_CPUT0I( OARG, 'NCHAN', ONCHAN, STATUS )

*  Create output channel structures
      CALL ADI_CNEW1R( OARG, 'Channels', ONCHAN + 1, STATUS )
      CALL ADI_CNEW1R( OARG, 'ChannelSpec', ONCHAN, STATUS )

*  Map response data
      CALL ADI_CSIZE( RMFID, 'RMF', NRMF, STATUS )
      CALL ADI_CMAPR( RMFID, 'RMF', 'READ', IRPTR, STATUS )

*  Map old energy and channel indices, and the channel bounds
      CALL ADI_CMAPI( RMFID, 'EnergyIndices', 'READ', IEIPTR, STATUS )
      CALL ADI_CMAPI( RMFID, 'ChannelIndices', 'READ', ICIPTR, STATUS )
      CALL ADI_CMAPR( RMFID, 'ChannelSpec', 'READ', ICSPTR, STATUS )

*  Workspace to store min & max contributing input channel numbers
      CALL DYN_MAPI( 1, NCHAN+1, MINCCH, STATUS )
      CALL DYN_MAPI( 1, NCHAN+1, MAXCCH, STATUS )
      CALL ARR_INIT1I( NCHAN+1, NCHAN+1, %VAL(MINCCH), STATUS )
      CALL ARR_INIT1I( 0, NCHAN+1, %VAL(MAXCCH), STATUS )

*  Calculate number of new response elements
      CALL ERI0_REBIN_AST1( NRMF, %VAL(IEIPTR), %VAL(ICIPTR),
     :                      %VAL(ICSPTR), GROUP, IGRP, NBND,
     :                      %VAL(BNDPTR), ONRMF,
     :                      %VAL(MINCCH), %VAL(MAXCCH), STATUS )

*  Define sizes of output arrays, and map them
      CALL ADI_CNEW1I( OARG, 'ChannelIndices', ONRMF, STATUS )
      CALL ADI_CNEW1I( OARG, 'EnergyIndices', ONRMF, STATUS )
      CALL ADI_CNEW1R( OARG, 'RMF', ONRMF, STATUS )
      CALL ADI_CMAPI( OARG, 'ChannelIndices', 'WRITE', OCIPTR, STATUS )
      CALL ADI_CMAPI( OARG, 'EnergyIndices', 'WRITE', OEIPTR, STATUS )
      CALL ADI_CMAPR( OARG, 'RMF', 'WRITE', ORPTR, STATUS )

*  Find new response values and indices
      CALL ERI0_REBIN_AST2( NRMF, %VAL(IRPTR), %VAL(IEIPTR),
     :                      %VAL(ICIPTR), %VAL(ICSPTR), GROUP,
     :                      IGRP, NBND, %VAL(BNDPTR), ONRMF,
     :                      %VAL(ORPTR), %VAL(OEIPTR), %VAL(OCIPTR),
     :                      STATUS )

*  Pick the output channel spec values
      CALL ADI_CMAPR( RMFID, 'Channels', 'READ', ICBPTR, STATUS )
      CALL ADI_CMAPR( OARG, 'Channels', 'WRITE', OCBPTR, STATUS )
      CALL ADI_CMAPR( OARG, 'ChannelSpec', 'WRITE', OCSPTR, STATUS )
      J = 1
      IF ( GROUP ) THEN
        J = 1
        DO I = 1, ONCHAN+1
          IF ( I .LE. ONCHAN ) THEN
            CALL ARR_ELEM1R( ICSPTR, NCHAN+1, J, CSPEC, STATUS )
            CALL ARR_SELEM1R( OCSPTR, ONCHAN+1, I, CSPEC, STATUS )
          END IF
          CALL ARR_ELEM1R( ICBPTR, NCHAN+1, J, CBND, STATUS )
          CALL ARR_SELEM1R( OCBPTR, ONCHAN+1, I, CBND, STATUS )
          J = MIN( NCHAN+1, J + IGRP )
        END DO
      ELSE
        CALL ERI0_REBIN_B2C( ONCHAN, %VAL(BNDPTR), %VAL(OCSPTR),
     :                       STATUS )
        CALL ARR_ELEM1I( MINCCH, NCHAN+1, 1, LOCCH, STATUS )
        CALL ARR_ELEM1R( ICBPTR, NCHAN+1, LOCCH, CSPEC, STATUS )
        CALL ARR_SELEM1R( OCBPTR, ONCHAN+1, 1, CSPEC, STATUS )
        DO I = 1, ONCHAN
          CALL ARR_ELEM1I( MAXCCH, NCHAN+1, I, HICCH, STATUS )
          CALL ARR_ELEM1R( ICBPTR, NCHAN+1, HICCH, CSPEC, STATUS )
          CALL ARR_SELEM1R( OCBPTR, ONCHAN+1, I+1, CSPEC, STATUS )
        END DO
      END IF
      CALL ADI_CUNMAP( RMFID, 'ChannelSpec', ICSPTR, STATUS )
      CALL ADI_CUNMAP( OARG, 'ChannelSpec', OCSPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'Channels', ICBPTR, STATUS )
      CALL ADI_CUNMAP( OARG, 'Channels', OCBPTR, STATUS )

*  Free output response length arrays
      CALL ADI_CUNMAP( OARG, 'ChannelIndices', OCIPTR, STATUS )
      CALL ADI_CUNMAP( OARG, 'EnergyIndices', OEIPTR, STATUS )
      CALL ADI_CUNMAP( OARG, 'RMF', ORPTR, STATUS )

*  Free input arrays
      CALL ADI_CUNMAP( RMFID, 'ChannelIndices', ICIPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'EnergyIndices', IEIPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'RMF', IRPTR, STATUS )

*  Free workspace
      CALL DYN_UNMAP( MINCCH, STATUS )
      CALL DYN_UNMAP( MAXCCH, STATUS )

*  Free mapped bounds
      IF ( .NOT. GROUP ) THEN
        CALL ADI_UNMAP( ARGS(3), BNDPTR, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ERI0_REBIN_AST',
     :                                           STATUS )

      END


*+ ERI0_REBIN_AST1 - Finds number of output response elements
      SUBROUTINE ERI0_REBIN_AST1( N, EI, CI, CSPEC, GROUP, IGRP, NBND,
     :                            BNDS, ON, MINCCH, MAXCCH, STATUS )
*  Description:
*     Loop over response elements finding new channel index. Merge
*     together response elements with identical indices.
*
      IMPLICIT NONE
      INTEGER   N, EI(*), CI(*), ON, STATUS, IGRP, NBND
      REAL		CSPEC(*), BNDS(*)
      INTEGER			MINCCH(*), MAXCCH(*)

      LOGICAL   GROUP
      INTEGER    ERI0_REBIN_NCI

       INTEGER	LAST_E,LAST_NCI,I,NCI

      IF ( STATUS .EQ. 0 ) THEN

*    Loop
        LAST_E = -1
        LAST_NCI = -1
        ON = 0
        DO I = 1, N

*      Find new channel index
          NCI = ERI0_REBIN_NCI( GROUP, IGRP, NBND, BNDS, CSPEC, CI(I) )
          IF ( NCI .GT. 0 ) THEN
            IF ( (EI(I) .NE. LAST_E) .OR. (NCI.NE.LAST_NCI) ) THEN
              ON = ON + 1
            END IF
            LAST_E = EI(I)
            LAST_NCI = NCI
            MAXCCH(NCI) = MAX( MAXCCH(NCI), CI(I) )
            MINCCH(NCI) = MIN( MINCCH(NCI), CI(I) )
          END IF

        END DO

      END IF
      END


      SUBROUTINE ERI0_REBIN_AST2( IN, IR, IEI, ICI, CSPEC, GROUP,
     :                            IGRP, NBND, BNDS, ON,
     :                            OR, OEI, OCI, STATUS )
      IMPLICIT NONE
      INTEGER	IN, IEI(*), ICI(*), ON, OEI(*), OCI(*), STATUS, igrp
      INTEGER   NBND
      REAL		CSPEC(*), BNDS(*)
      LOGICAL   GROUP
      REAL	IR(*), OR(*)

      INTEGER    ERI0_REBIN_NCI
       INTEGER	LAST_E,LAST_NCI,I,NCI

      IF ( STATUS .EQ. 0 ) THEN

        LAST_E = -1
        LAST_NCI = -1
        ON = 0
        DO I = 1, IN

*      Find new channel index
          NCI = ERI0_REBIN_NCI( GROUP, IGRP, NBND, BNDS, CSPEC,
     :                          ICI(I) )

          IF ( NCI .GT. 0 ) THEN
            IF ( (IEI(I) .NE. LAST_E) .OR. (NCI.NE.LAST_NCI) ) THEN
              ON = ON + 1
              OR(ON) = IR(I)
              OEI(ON) = IEI(I)
              OCI(ON) = NCI
            ELSE
              OR(ON) = OR(ON) + IR(I)
            END IF
            LAST_E = IEI(I)
            LAST_NCI = NCI
          END IF

        END DO

      END IF
      END


* Find new channel index given old one
      INTEGER FUNCTION ERI0_REBIN_NCI( GROUP, IGRP, NBND, BNDS,
     :                                 CSPEC, OLDIND )
      IMPLICIT NONE

      LOGICAL GROUP
      INTEGER IGRP, NBND, OLDIND
      REAL    BNDS(*), CSPEC(*)
      INTEGER NCI,IBND
      REAL	CHAN

*  Find new channel index
      IF ( GROUP ) THEN
        NCI = (OLDIND-1)/IGRP + 1

      ELSE

*    Bounds in range?
        CHAN = CSPEC(OLDIND)
        NCI = 0
        IF ( (CHAN .GE. BNDS(1)) .AND.
     :           (CHAN .LE. BNDS(NBND)) ) THEN
	  IBND = 2
          DO WHILE ( (IBND .LE. NBND) .AND. (NCI.EQ.0) )
            IF ( CHAN .LE. BNDS(IBND) ) THEN
              NCI = IBND - 1
            ELSE
              IBND = IBND + 1
            END IF
          END DO
        END IF

      END IF

      ERI0_REBIN_NCI = NCI

      END


      SUBROUTINE ERI0_REBIN_B2C( N, BNDS, CEN, STATUS )
      INTEGER N,STATUS,I
      REAL BNDS(*), CEN(*)
      DO I = 1, N
        CEN(I) = (BNDS(I+1) + BNDS(I))/2.0
      END DO
      END
