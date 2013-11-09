      SUBROUTINE ERI1_READHDS( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ERI1_READHDS

*  Purpose:
*     Read the energy response from an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI1_READHDS( NARG, ARGS, OARG, STATUS )

*  Description:
*     Constructs the data objects required by ERI from the supplied HDS
*     dataset. This must be an ASTERIX format NDF.

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
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Feb 1995 (DJA):
*        Original version.
*      3 AUG 1995 (DJA):
*        Allow channel energy bounds to be missing. Some old Spacelab 2
*        responses don't have this component, and it isn't strictly
*        needed for fitting, but only for plotting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants
      INCLUDE 'DAT_PAR'					! HDS constants

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
C      [external_declaration]
C      {data_type} {external_name} ! [external_description]

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ELOC			! ENERGY_RESP() locator
      CHARACTER*(DAT__SZLOC)	ECLOC			! Response CHANNEL obj
      CHARACTER*(DAT__SZLOC)	EELOC			! Response ENERGY obj
      CHARACTER*(DAT__SZLOC)	ERLOC			! Response RESPONSE obj
      CHARACTER*(DAT__SZLOC)	ESLOC			! ENERGY_RESP locator
      CHARACTER*(DAT__SZLOC)	LOC			! Locator to 1st arg
      CHARACTER*(DAT__SZNAM)	NAME			! Name of 1st arg

      INTEGER			CPTR			! Channel indices
      INTEGER			CBPTR			! Channel energy bounds
      INTEGER			EBPTR			! Energy bounds
      INTEGER			EPTR			! Energy indices
      INTEGER			INDEX			! Response index
      INTEGER			NCBND			! # channel bounds
      INTEGER			NCHAN			! Size of channel axis
      INTEGER			NEBND			! # energy bounds
      INTEGER			NENER			! Size of energy axis
      INTEGER			NRMF			! # response elements
      INTEGER			RID			! Redistribution
      INTEGER			RPTR			! RMF values

      LOGICAL			GOTCEB			! Got chan energy bnds?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID

*  Get the index required, zero if only one argument
      IF ( NARG .EQ. 1 ) THEN
        INDEX = 0
      ELSE
        CALL ADI_GET0I( ARGS(2), INDEX, STATUS )
      END IF

*  We accept either a direct locator to an object whose name is ENERGY_RESP,
*  or a dataset which contains such an object.
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )
      CALL DAT_NAME( LOC, NAME, STATUS )
      IF ( NAME(1:) .EQ. 'ENERGY_RESP' ) THEN
        CALL DAT_CLONE( LOC, ESLOC, STATUS )
      ELSE
        CALL ADI1_LOCERESP( ARGS(1), .FALSE., ESLOC, STATUS )
      END IF

*  If an index is supplied, use it.
      IF ( INDEX .GT. 0 ) THEN
        CALL DAT_CELL( ESLOC, 1, INDEX, ELOC, STATUS )
      ELSE
        CALL DAT_CLONE( ESLOC, ELOC, STATUS )
      END IF

*  Located it ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Locate the 3 structures
        CALL DAT_FIND( ELOC, 'ENERGY', EELOC, STATUS )
        CALL DAT_FIND( ELOC, 'CHANNEL', ECLOC, STATUS )
        CALL DAT_FIND( ELOC, 'RESPONSE', ERLOC, STATUS )

*    Sizes of the ENERGY and CHANNEL boundary arrays
        CALL CMP_SIZE( EELOC, 'ENERGY_BOUNDS', NEBND, STATUS )
        CALL CMP_SIZE( ECLOC, 'CHANNEL_BOUNDS', NCBND, STATUS )
        NCHAN = NCBND - 1
        NENER = NEBND - 1

*    Map in values from instrument response
        CALL CMP_MAPV( EELOC, 'ENERGY_BOUNDS', '_REAL', 'READ', EBPTR,
     :                 NEBND, STATUS )
        CALL CMP_MAPV( EELOC, 'DATA_ARRAY', '_INTEGER', 'READ', EPTR,
     :                 NRMF, STATUS )
        CALL CMP_MAPV( ECLOC, 'DATA_ARRAY', '_INTEGER', 'READ', CPTR,
     :                 NRMF, STATUS )
        CALL CMP_MAPV( ECLOC, 'CHANNEL_BOUNDS', '_REAL', 'READ', CBPTR,
     :                 NCBND, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL MSG_PRNT( 'WARNING : Unable to read channel energy '/
     :                   /'bounds from response' )
          GOTCEB = .FALSE.
        ELSE
          GOTCEB = .TRUE.
        END IF
        CALL CMP_MAPV( ERLOC, 'DATA_ARRAY', '_REAL', 'READ', RPTR,
     :                 NRMF, STATUS )

*    If we got all the data then create the ADI object, poke in the data
*    and unmap the HDS stuff
        IF ( STATUS .EQ. SAI__OK ) THEN

*      An ASTERIX specific response
          CALL ADI_NEW0( 'AsterixRMF', RID, STATUS )

*      Write in dimensions of energy and channel axes
          CALL ADI_CPUT0I( RID, 'NENERGY', NENER, STATUS )
          CALL ADI_CPUT0I( RID, 'NCHAN', NCHAN, STATUS )

*      No threshold for ASTERIX responses
          CALL ADI_CPUT0R( RID, 'Threshold', 0.0, STATUS )

*      Write the big arrays
          CALL ADI_CPUT1R( RID, 'Energy', NEBND, %VAL(EBPTR), STATUS )
          IF ( GOTCEB ) THEN
            CALL ADI_CPUT1R( RID, 'Channels', NCBND, %VAL(CBPTR),
     :                       STATUS )
          END IF
          CALL ADI_CPUT1I( RID, 'ChannelIndices', NRMF, %VAL(CPTR),
     :                     STATUS )
          CALL ADI_CPUT1I( RID, 'EnergyIndices', NRMF, %VAL(EPTR),
     :                     STATUS )
          CALL ADI_CPUT1R( RID, 'RMF', NRMF, %VAL(RPTR), STATUS )

*      Write channel centres if present
          CALL ADI1_CCH2AR( ECLOC, 'CHANNEL_SPEC', RID, 'ChannelSpec',
     :                      STATUS )

*      Structure containing results (the FITS version returns both an RMF
*      and an ARF hence the structure is needed)
          CALL ADI_NEW0( 'STRUC', OARG, STATUS )
          CALL ADI_CPUTID( OARG, 'RMF', RID, STATUS )

        END IF

*    Unmap and release locators
        CALL CMP_UNMAP( ERLOC, 'DATA_ARRAY', STATUS )
        CALL CMP_UNMAP( ECLOC, 'DATA_ARRAY', STATUS )
        CALL CMP_UNMAP( EELOC, 'DATA_ARRAY', STATUS )
        CALL CMP_UNMAP( EELOC, 'ENERGY_BOUNDS', STATUS )
        CALL DAT_ANNUL( ERLOC, STATUS )
        CALL DAT_ANNUL( ECLOC, STATUS )
        CALL DAT_ANNUL( EELOC, STATUS )
        CALL DAT_ANNUL( ELOC, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI1_READHDS', STATUS )
      END IF

      END
