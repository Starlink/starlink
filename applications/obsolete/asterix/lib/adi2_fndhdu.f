      SUBROUTINE ADI2_FNDHDU( ID, HDU, HDUID, STATUS )
*+
*  Name:
*     ADI2_FNDHDU

*  Purpose:
*     Locate an HDU in a FITSfile

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FNDHDU( ID, HDU, HDUID, STATUS )

*  Description:
*     Locate an HDU in a FITSfile. It is an error if the HDU does not
*     exist. HDUs are located by name, defined by the EXTNAME keyword.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARACTER*(*) (given)
*        Name of the HDU we're loooking for
*     HDUID = INTEGER (returned)
*        ADI identifier of FITShdu object
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Sep 1995 (DJA):
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

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		HDU

*  Arguments Returned:
      INTEGER			HDUID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*50		CMNT			! Keyword comment
      CHARACTER*20		EXTNAM			! Extension name
      CHARACTER*15		LHDU			! HDU name

      INTEGER			FSTAT			! FITSIO status code
      INTEGER			HCID			! HDU container
      INTEGER			HDUTYP			! HDU type
      INTEGER			IHDU			! HDU number
      INTEGER			LUN			! Logical unit number
      INTEGER			UIHDU			! User HDU number

      LOGICAL			FOUND			! Found HDU
      LOGICAL			THERE			! HDU exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the HDU container in the file
      CALL ADI_FIND( ID, 'Hdus', HCID, STATUS )

*  Get user HDU specification
      CALL ADI_CGET0I( ID, 'UserHDU', UIHDU, STATUS )

*  Make local copy of HDU name
      LHDU = HDU

*  Access is at file level?
      IF ( UIHDU .EQ. 0 ) THEN

*    Import HDU name
        IF ( LHDU .EQ. ' ' ) LHDU = 'PRIMARY'
        CALL ADI_THERE( HCID, LHDU, THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_FIND( HCID, LHDU, HDUID, STATUS )
          GOTO 99

        ELSE

*      Extract logical unit
          CALL ADI_CGET0I( ID, 'Lun', LUN, STATUS )

*      Move to start of file
          IF ( LHDU .EQ. 'PRIMARY' ) THEN

*        Move to first HDU
            CALL ADI2_MVAHDU( ID, LUN, 1, HDUTYP, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
              CALL ADI2_ADDHDU( ID, LHDU, 1, HDUTYP, HDUID, STATUS )
            ELSE
              GOTO 99
            END IF

          ELSE

*        Scan HDU for HDU matching requested name
            FOUND = .FALSE.
            IHDU = 2
            DO WHILE ( (STATUS.EQ.SAI__OK) .AND. .NOT. FOUND )

*          Move to next HDU
              CALL ADI2_MVAHDU( ID, LUN, IHDU, HDUTYP, STATUS )
              IF ( STATUS .EQ. SAI__OK ) THEN

                FSTAT = 0
                CALL FTGKYS( LUN, 'EXTNAME', EXTNAM, CMNT, FSTAT )
                IF ( FSTAT .NE. 0 ) THEN
                  FSTAT = 0
                  WRITE( EXTNAM, '(A,I2.2)' ) 'HDU_', IHDU
                END IF

*            Add its description to our list
                CALL ADI2_ADDHDU( ID, EXTNAM, IHDU, HDUTYP, HDUID,
     :                            STATUS )

*            If not found release this HDU
                IF ( EXTNAM .EQ. LHDU ) THEN
                  FOUND = .TRUE.
                ELSE
                  CALL ADI_ERASE( HDUID, STATUS )
                  IHDU = IHDU + 1
                END IF

              END IF

            END DO

*        If we didn't find the HDU, write the number of HDU's in the file
            IF ( .NOT. FOUND ) THEN
              CALL ERR_BEGIN( STATUS )
              CALL ADI_CPUT0I( ID, 'Nhdu', IHDU-1, STATUS )
              CALL ERR_END( STATUS )
            END IF

          END IF

        END IF

      END IF

*  Release the HDU container
 99   CALL ERR_BEGIN( STATUS )
      CALL ADI_ERASE( HCID, STATUS )
      CALL ERR_END( STATUS )

*  Report if not found
      IF ( HDUID .EQ. ADI__NULLID ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'HDU', HDU )
        CALL ERR_REP( ' ', 'HDU /^HDU/ not found in file', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FNDHDU', STATUS )

      END
