      SUBROUTINE IRG1_FSPEC( FSPEC, DFSPEC, FLDNAM, FIELD, STATUS )
*+
*  Name:
*     IRG1_FSPEC

*  Purpose:
*     Produces a new file specification by modification of an old file
*     specification.

*  Language:
*     VAX/VMS Fortran.

*  Invocation:
*     CALL IRG1_FSPEC( FSPEC, DFSPEC, FLDNAM, FIELD, STATUS )

*  Description:
*     This is a VAX/VMS specific routine. 
*
*     The supplied file specification (FSPEC) is parsed, and the
*     requested fields returned. Any fields missing from FSPEC are
*     defaulted to the corresponding field values from the default file
*     specification (DFSPEC). The full file specification can be
*     returned by supplying a blank value for FLDNAM.

*  Arguments:
*     FSPEC = CHARACTER (Given)
*        The original file specification. 
*     DFSPEC = CHARACTER (Given)
*        The default file specification. 
*     FLDNAM = CHARACTER (Given)
*        A string specifying which fields are to be returned in FIELD.
*        FLDNAM can take any of the values: DEVICE, DIRECTORY, NAME,
*        TYPE, VERSION. In addition, a blank value will return the full
*        file specification in FIELD. Unambiguous abbreviations may be
*        given.
*     FIELD = CHARACTER (Returned)
*        The requested field from the full file specification.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-JUN-1991 (DSB):
*        Original version.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE '($FABDEF)'        ! RMS File Access Block definitions.
      INCLUDE '($NAMDEF)'        ! RMS NAM block definitions.
      INCLUDE 'IRH_PAR'          ! IRH parameters.
      INCLUDE 'IRG_ERR'          ! IRG error values.

*  Arguments Given:
      CHARACTER FSPEC*(*)
      CHARACTER DFSPEC*(*)
      CHARACTER FLDNAM*(*)

*  Arguments Returned:
      CHARACTER FIELD*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Structures:
      RECORD /FABDEF/ FAB        ! RMS File Access Block.
      RECORD /NAMDEF/ NAM        ! RMS NAM block.

*  External References:
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.
      INTEGER STR$COPY_R         ! Function which copies a string by
                                 ! reference.
      INTEGER SYS$PARSE          ! The RMS $PARSE service.


*  Local Variables:
      CHARACTER FULLFS*(NAM$C_MAXRSS)! Full file specification.
      INTEGER ISTAT              ! Status value from RMS $PARSE service.
      CHARACTER UPFLD*2          ! First two characters of FLDNAM
                                 ! converted to upper case.

*  NUM Type Conversion Function Declarations:
      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the variable which will receive the full file 
*  specification to a blank string. This erases any string left in it
*  by the last call to this routine.
      FULLFS = ' '

*  Any unspecified elements of the new file specification are defaulted
*  to the values of the corresponding elements in the old file
*  specification, using the RMS $PARSE service. First, set up the RMS
*  structures (FAB and NAM).
      FAB.FAB$B_BID = FAB$C_BID
      FAB.FAB$B_BLN = FAB$C_BLN
      FAB.FAB$L_DNA = %LOC( DFSPEC )
      FAB.FAB$B_DNS = NUM_ITOUB( MIN( 255, CHR_LEN( DFSPEC ) ) )
      FAB.FAB$L_FNA = %LOC( FSPEC )
      FAB.FAB$B_FNS = NUM_ITOUB( MIN( 255, CHR_LEN( FSPEC ) ) )
      FAB.FAB$L_FOP = 0
      FAB.FAB$W_IFI = 0
      FAB.FAB$L_NAM = %LOC( NAM )

      NAM.NAM$B_BID = NAM$C_BID
      NAM.NAM$B_BLN = NAM$C_BLN
      NAM.NAM$L_ESA = %LOC( FULLFS )
      NAM.NAM$B_ESS = NUM_ITOUB( LEN( FULLFS ) )
      NAM.NAM$B_NOP = NAM$M_SYNCHK
      NAM.NAM$L_RLF = 0

*  Now call the RMS $PARSE service. This produces the required full file
*  specification.
      ISTAT = SYS$PARSE( FAB )

*  Copy the full file specification to the FIELD argument.
      FIELD = FULLFS

*  If all has gone OK and a specific field is required...
      IF( ISTAT .AND. FLDNAM .NE. ' ' ) THEN

*  Convert the first two characters of the supplied field name to
*  upper case.
         CALL STR$UPCASE( UPFLD, FLDNAM(:2) )

*  Copy the relevant field to the FIELD argument.
         IF( INDEX( UPFLD, 'DE') .EQ. 1 ) THEN
            ISTAT = STR$COPY_R( FIELD, NUM_UBTOUW( NAM.NAM$B_DEV ),
     :                          %VAL( NAM.NAM$L_DEV ) )

         ELSE IF( INDEX( UPFLD, 'DI') .EQ. 1 ) THEN
            ISTAT = STR$COPY_R( FIELD, NUM_UBTOUW( NAM.NAM$B_DIR ),
     :                          %VAL( NAM.NAM$L_DIR ) )

         ELSE IF( INDEX( UPFLD, 'N') .EQ. 1 ) THEN
            ISTAT = STR$COPY_R( FIELD, NUM_UBTOUW( NAM.NAM$B_NAME ),
     :                          %VAL( NAM.NAM$L_NAME ) )

         ELSE IF( INDEX( UPFLD, 'T') .EQ. 1 ) THEN
            ISTAT = STR$COPY_R( FIELD, NUM_UBTOUW( NAM.NAM$B_TYPE ),
     :                          %VAL( NAM.NAM$L_TYPE ) )

         ELSE IF( INDEX( UPFLD, 'V') .EQ. 1 ) THEN
            ISTAT = STR$COPY_R( FIELD, NUM_UBTOUW( NAM.NAM$B_VER ),
     :                          %VAL( NAM.NAM$L_VER ) )

*  If the field name was unrecognised, report an error.
         ELSE
            STATUS = IRG__BADFN
            CALL MSG_SETC( 'NAM', FLDNAM )
            CALL ERR_REP( 'IRG1_FSPEC_ERR1',
     :              'IRG1_FSPEC: Unrecognised field requested - "^NAM"',
     :               STATUS )
            GO TO 999
         END IF


      END IF

*  If an error has occured, report it.
      IF( .NOT. ISTAT ) THEN
         STATUS = IRG__VMSER
         CALL ERR_SYSER( 'ERR', ISTAT )
         CALL ERR_REP( 'IRG1_FSPEC_ERR2',
     :                 'IRG1_FSPEC: System error: ^ERR',
     :                 STATUS )
      END IF

 999  CONTINUE

      END
* $Id$
