      SUBROUTINE CCDSCHED( STATUS )
*+
*  Name:
*     CCDSCHED

*  Purpose:
*     Perform an automated CCDPACK reduction.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CCDSCHED( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  [usage]

*  ADAM Parameters:

*  [examples]
*  [optional_A_task_items]...
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-FEB-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN) FILNMS( CCD1__MXINS ) ! FILTER names
      CHARACTER * ( CCD1__NMLEN ) FTYPES( 2, CCD1__MXINS ) ! NDF frame
                                                           ! and filter
                                                           ! types.
      CHARACTER * ( 3 ) STYPE    ! Script type
      INTEGER GIDIN              ! Input NDF group identifier
      INTEGER GIDOUT             ! Output NDF group identfier
      INTEGER NFILS              ! Number of FILTER types
      INTEGER NNDF               ! Number of input NDFs
      INTEGER PTEMP1( CCD1__MXINS ) ! Pointers to various entries in
                                    ! FTYPES
      INTEGER PTEMP2( CCD1__MXINS ) ! Pointers to various entries in
                                    ! FTYPES
      INTEGER SCRFD              ! Script file descriptor
      LOGICAL HVFLAT( CCD1__MXINS ) ! Have FLATs fro this filter
      LOGICAL MKBIAS             ! Need a MASTER_BIAS
      LOGICAL MKDARK             ! Need a MASTER_DARK
      LOGICAL MKFLAS             ! Need a MASTER_FLASH
      LOGICAL MKFLAT( CCD1__MXINS ) ! Need for MASTER_FLATs
      LOGICAL SCROPN             ! Script file open
      LOGICAL VALID( CCD1__MXINS ) ! Valid input NDF mask

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up CCDPACK .
      CALL CCD1_START( 'CCDSCHED', STATUS )

*  Set up global NDF context.
      CALL NDF_BEGIN

*  Access the input name list.
      CALL CCD1_NDFGR( 'IN', 'READ', GIDIN, NNDF, STATUS )

*  Create a list of the NDF frame and filter types. Check compliance.
*  These checks cover the parameters "ADC" "RNOISE" "DIRECTION",
*  "SATURATION" and "DEFERRED".  These may not be different within the
*  same input NDF.  If they are different then it is probably that the
*  dataset is taken with different devices The NDF dimensions are also
*  checked.
      CALL CCD1_FTYPL( GIDIN, NNDF, FTYPES, VALID, STATUS )

*  Set up the processing schedule.
      CALL CCD1_SCHED( FTYPES, NNDF, VALID, PTEMP1, MKBIAS, MKDARK,
     :                 MKFLAS, MKFLAT, HVFLAT, FILNMS, NFILS, STATUS )

*  Get the name of a file to write the script into.
      CALL CCD1_ASFIO( 'SCRIPT', 'WRITE', 'LIST', 0, SCRFD, SCROPN,
     :                 STATUS )

*  Find out what type of script to write.
      CALL PAR_CHOIC( 'STYPE', 'CSH', 'CSH,DCL,ICL', .FALSE., STYPE,
     :                STATUS )

*  Now write the reduction procedure.
      CALL CCD1_AUTO( STYPE, SCRFD, GIDIN, FTYPES, NNDF, VALID, MKBIAS,
     :                MKDARK, MKFLAS, MKFLAT, HVFLAT, FILNMS, NFILS,
     :                PTEMP1, PTEMP2,STATUS )

*  Close the script file.
      IF ( SCROPN ) CALL FIO_CLOSE( SCRFD, STATUS )

*  Close IRH.
      CALL IRH_CLOSE( STATUS )

*  Close NDF.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CCDSCHED_ERR',
     :   'CCDSCHED: Error performing CCDPACK automated reduction.',
     :   STATUS )
      END IF

*  Close CCDPACK.
      CALL CCD1_END( STATUS )

      END
* $Id$
