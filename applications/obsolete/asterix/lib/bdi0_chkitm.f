      SUBROUTINE BDI0_CHKITM( ID, ITEM, LITEM, LITL, STATUS )
*+
*  Name:
*     BDI0_CHKITM

*  Purpose:
*     Check item name is valid, and return regular copy

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_CHKITM( ID, ITEM, LITEM, LITL, STATUS )

*  Description:
*     Check whether the name supplied is a valid item name. Item names are
*     generally names of the class members of the abstract models being
*     accessed. The principal exceptions are the axis items, which are of
*     the form <Q>_Axis_<subitem> or Axis_<N>_<subitem>. In the first case
*     we must map the quanity code <Q> on to an axis number. In the second
*     we just check that the axis number is valid. The output item for the
*     first form is converted to the second.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of object passed to BDI
*     ITEM = CHARACTER*(*) (given)
*        The type name to validate
*     LITEM = CHARACTER*(*) (returned)
*        The validated item name
*     LITL = INTEGER (returned)
*        Used length of LITEM
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
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
      CHARACTER*(*)		ITEM

*  Arguments Returned:
      CHARACTER*(*)		LITEM
      INTEGER			LITL

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Constants:
      CHARACTER*58		AXSUBS
        PARAMETER		( AXSUBS =
     :   'DATA,WIDTH,LABEL,UNITS,NORMALISED,LOWIDTH,HIWIDTH' )
      CHARACTER*100		SUBS
        PARAMETER		( SUBS =
     :   'DATA,VARIANCE,ERROR,QUALITY,QUALITYMASK,LABEL,UNITS,'/
     :  /'TITLE,LOGICALQUALITY,LOERROR,HIERROR' )

*  Local Variables:
      CHARACTER*(ADI__MXDIM)	AXORDS			! Axis order string
      CHARACTER*20		LCOP			! Local copy

      INTEGER			DIMS(ADI__MXDIM)	! Dimensions of ID
      INTEGER			FSTAT			! i/o status code
      INTEGER			IAX			! Axis number
      INTEGER			ILEN			! Length of ITEM
      INTEGER			NDIM			! Dimensionality of ID

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default output
      LITEM = ITEM
      LITL = LEN(ITEM)

*  Make a copy and capitalise
      LCOP = ITEM
      CALL CHR_UCASE( LCOP )
      ILEN = LEN(ITEM)

*  Data is the only supported by everything, so check the rest
      IF ( LCOP(:ILEN) .EQ. 'DATA' ) THEN

        LITEM = 'Data'

      ELSE

*    Special axis items? These have a leading letter denoting the quantity
*    the axis represents.
        IF ( LCOP(2:) .EQ. '_AXIS_' ) THEN

*      Extract the axis quantity code. To establish the axis number for
*      this quantity we first try to read an 'AxisOrder' data member.
          CALL ADI_THERE( ID, 'AxisOrder', THERE, STATUS )
          IF ( THERE ) THEN
            CALL ADI_CGET0C( ID, 'AxisOrder', AXORDS, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_ANNUL( STATUS )
              THERE = .FALSE.
            END IF
          END IF

*      If the ordering string is ok,
          IF ( THERE ) THEN

*        Look for our axis code in it.
            IAX = INDEX( AXORDS, LCOP(1:1) )

*        Not present is an error because the we know the logical axis
*        quantities allowed in this dataset and we're trying to access
*        something different.
            IF ( IAX .EQ. 0 ) THEN
              STATUS = SAI__ERROR
              CALL MSG_SETC( 'A', LCOP(1:1) )
              CALL MSG_SETC( 'SET', AXORDS )
              CALL ERR_REP( 'BDI0_CHKITM_1', 'The requested axis '/
     :              /'quantity (^A) is not one of those allowed in '/
     :              /'this data model (^SET)', STATUS )
            END IF

*      No axis ordering string...
          ELSE

*         Scan axis labels to match our code
            CALL BDI0_FNDAXC( ID, LCOP(1:1), IAX, STATUS )

          END IF

*      If ok, check the axis subitem
          IF ( STATUS .EQ. SAI__OK ) THEN

*        Must exist in the list
            IF ( CHR_INSET( AXSUBS, LCOP(8:) ) ) THEN

*          Coerce to axis number, and correct case of item
              WRITE( LITEM, '(A,I1.1,A)' ) 'Axis_', IAX, LCOP(7:ILEN)
              CALL CHR_LCASE( LITEM(9:LITL) )

            ELSE
              STATUS = SAI__ERROR
              CALL MSG_SETC( 'NAME', ITEM )
              CALL ERR_REP( 'BDI0_CHKITM_A', 'Axis item name is '/
     :                         /'not recognised (^NAME)', STATUS )
            END IF

          END IF

*    Normal axis items
        ELSE IF ( LCOP(1:5) .EQ. 'AXIS_' ) THEN

*      Extract axis number
          READ( LCOP(6:6), '(I1.1)', IOSTAT=FSTAT ) IAX
          IF ( FSTAT .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', ITEM )
            CALL ERR_REP( 'BDI0_CHKITM_2', 'Error reading axis '/
     :               /'number from item string (^NAME)', STATUS )

*      Check its sensible
          ELSE IF ( (IAX .EQ. 0) .OR. (IAX.GT.ADI__MXDIM) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', ITEM )
            CALL ERR_REP( 'BDI0_CHKITM_3', 'Illegal axis '/
     :                /'number in item string (^NAME)', STATUS )

*      Check against dimensions
          ELSE

*        Get dimensions of object
            CALL BDI_GETSHP( ID, ADI__MXDIM, DIMS, NDIM, STATUS )

*        If ok (ie they're defined),
            IF ( STATUS .EQ. SAI__OK ) THEN

*          Check against NDIM
              IF ( IAX .LE. NDIM ) THEN

*            Must exist in the list
                IF ( CHR_INSET( AXSUBS, LCOP(8:) ) ) THEN

*              Correct case of item
                  WRITE( LITEM, '(A,I1.1,A)' ) 'Axis_', IAX,
     :                       LCOP(7:ILEN)
                  CALL CHR_LCASE( LITEM(9:LITL) )
                  IF ( LCOP(8:ILEN) .EQ. 'LOWIDTH' ) THEN
                    LITEM(8:) = 'LoWidth'
                  ELSE IF ( LCOP(8:ILEN) .EQ. 'HIWIDTH' ) THEN
                    LITEM(8:) = 'HiWidth'
                  END IF

                ELSE
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'NAME', ITEM )
                  CALL ERR_REP( 'BDI0_CHKITM_A', 'Axis item name is '/
     :                             /'not recognised (^NAME)', STATUS )
                END IF

*          Otherwise an error
              ELSE
                STATUS = SAI__ERROR
                CALL MSG_SETI( 'N', IAX )
                CALL MSG_SETC( 'NAME', ITEM )
                CALL MSG_SETI( 'ND', NDIM )
                CALL ERR_REP( 'BDI0_CHKITM_4', 'Illegal axis '/
     :                /'number (^N) in item string (^NAME) exceeds'/
     :                 /' data dimensionality (^ND)', STATUS )
              END IF

            END IF

          END IF

*     Other non-axis items
        ELSE

*      Must exist in the list
          IF ( CHR_INSET( SUBS, LCOP(1:ILEN) ) ) THEN

*        Correct case of item
            IF ( LCOP(1:ILEN) .EQ. 'QUALITYMASK' ) THEN
              LITEM = 'QualityMask'
            ELSE IF ( LCOP(1:ILEN) .EQ. 'LOGICALQUALITY' ) THEN
              LITEM = 'LogicalQuality'
            ELSE IF ( LCOP(1:ILEN) .EQ. 'MASKEDQUALITY' ) THEN
              LITEM = 'MaskedQuality'
            ELSE IF ( LCOP(1:ILEN) .EQ. 'LOERROR' ) THEN
              LITEM = 'LoError'
            ELSE IF ( LCOP(1:ILEN) .EQ. 'HIERROR' ) THEN
              LITEM = 'HiError'
            ELSE
              CALL CHR_UCASE( LITEM(1:1) )
              CALL CHR_LCASE( LITEM(2:LITL) )
            END IF

          ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', ITEM )
            CALL ERR_REP( 'BDI0_CHKITM_I', 'Item name is '/
     :                        /'not recognised (^NAME)', STATUS )
          END IF

        END IF

*  End of switch on 'Data' test
      END IF

      END
