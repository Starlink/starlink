      SUBROUTINE ERI0_INIT( STATUS )
*+
*  Name:
*     ERI0_INIT

*  Purpose:
*     Load ADI definitions required for ERI operation

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI0_INIT( STATUS )

*  Description:
*     Loads those class definitions required by the ERI subroutine group.
*     Results in the following classes being defined,
*
*
*     Methods are defined to read and write response information from HDS and
*     FITS files.

*  Arguments:
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
*     ADI:
*        ADI_REQPKG - Load a package from the load path

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Jan 1995 (DJA):
*        Original version.
*     28 Mar 1996 (DJA):
*        Added boundary rebinning routine
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PKG'

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI
      EXTERNAL			ADI_REQPKG
      EXTERNAL			ERI0_REBIN_AST
      EXTERNAL                  ERI1_READHDS
      EXTERNAL                  ERI1_WRITRMF
      EXTERNAL                  ERI1_WRITRMF_OGIP
      EXTERNAL                  ERI1_WRITE_OGIP_RMFARF
      EXTERNAL                  ERI1_WRITRMF_AST
      EXTERNAL                  ERI2_READ
      EXTERNAL                  ERI2_WRITRMF
      EXTERNAL                  ERI2_WRITRMF_OGIP
      EXTERNAL                  ERI2_WRITRMF_AST

*  Local variables:
      INTEGER			DID			! Ignored identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check not already initialised?
      IF ( .NOT. AST_QPKGI( ERI__PKG ) ) THEN

*    Load the ADI classes
        CALL ADI_REQPKG( 'eresp', STATUS )

*    Define the methods
        CALL ADI_DEFMTH('ReadRMF(_HDSfile)', ERI1_READHDS, DID,
     :                  STATUS )
        CALL ADI_DEFMTH('ReadRMF(_HDSfile,_INTEGER)', ERI1_READHDS,
     :                   DID, STATUS )
        CALL ADI_DEFMTH('ReadRMF(_FITSfile)', ERI2_READ, DID, STATUS )
        CALL ADI_DEFMTH('WriteRMF(_FITSfile,_INTEGER,_INTEGER,'/
     :               /'_RedistributionMatrix)',
     :                                 ERI2_WRITRMF, DID, STATUS )
        CALL ADI_DEFMTH('@WriteRMF(_FITSfile,_INTEGER,_INTEGER,'/
     :                          /'_AsterixRMF)',
     :                                 ERI2_WRITRMF_AST, DID, STATUS )
        CALL ADI_DEFMTH('WriteRMF(_FITSfile,_OGIPcmpRMF)',
     :                                 ERI2_WRITRMF_OGIP, DID, STATUS )

c        CALL ADI_DEFMTH('WriteRMF(_HDSfile,_RedistributionMatrix)',
c     :                                 ERI1_WRITRMF, DID, STATUS )

        CALL ADI_DEFMTH('WriteRMF(_HDSfile,_INTEGER,_INTEGER,'/
     :                         /'_AsterixRMF)',
     :                                 ERI1_WRITRMF_AST, DID, STATUS )
        CALL ADI_DEFMTH('WriteRMF(_HDSfile,'/
     :                       /'_INTEGER,_INTEGER,_OGIPcmpRMF)',
     :                                 ERI1_WRITRMF_OGIP, DID, STATUS )
        CALL ADI_DEFMTH('Write(_HDSfile,_INTEGER,_INTEGER,'/
     :                        /'_OGIPcmpRMF,_AncillaryResponse)',
     :                           ERI1_WRITE_OGIP_RMFARF, DID, STATUS )
        CALL ADI_DEFMTH('Rebin(_AsterixRMF,_INTEGER)',
     :                           ERI0_REBIN_AST, DID, STATUS )
        CALL ADI_DEFMTH('Rebin(_AsterixRMF,_INTEGER,_INTEGER[])',
     :                           ERI0_REBIN_AST, DID, STATUS )

*    Now initialised
        CALL AST_SPKGI( ERI__PKG )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ERI0_INIT', STATUS )

      END
