      SUBROUTINE IRG_NDFPR( GID, INDEX, INDF1, CLIST, INDF2, LOC, 
     :                      STATUS )
*+
*  Name:
*     IRG_NDFPR

*  Purpose:
*     Create an NDF by propagation and get an NDF identifier for it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG_NDFPR( GID, INDEX, INDF1, CLIST, INDF2, LOC, STATUS )

*  Description:
*     The routine creates an NDF with name specified by INDEX and GID
*     by propagation from the NDF identified by INDF1. An NDF
*     identifier for the created NDF is returned which can then be used
*     by all the normal NDF_ routines (see SUN/33). 

*  Arguments:
*     GID = INTEGER (Given)
*        An identifier for the group containing the name of the NDF to
*        be created.
*     INDEX = INTEGER (Given)
*        The index (within the group identified by GID) of the name of
*        the NDF to be created.
*     NDF1 = INTEGER (Given)
*        An NDF identifier for the NDF on which the created NDF is to
*        be based.
*     CLIST = CHARACTER (Given)
*        A list of components to be propagated, in the same format as
*        the CLIST argument for thr NDF_PROP routine (see SUN/33).
*     INDF2 = INTEGER (Returned)
*        An NDF identifier for the created NDF.
*     LOC = CHARACTER (Returned)
*        An HDS locator to the top level object in the .SDF file which 
*        contains the specified NDF. NDF_ANNUL should be called as usual
*        to annul the NDF identifier returned by argument INDF once
*        access is no longer required, but this will not of itself close
*        the .SDF file which contains the NDF. HDS_CLOSE must be called
*        (after NDF_ANNUL) to do this, specifying LOC as the first
*        argument. This is sometimes necessary in order to minimise the
*        number of files open at any one time. Note, any further
*        attempts to access the file (eg through identifiers to other
*        NDFs contained in the file) will fail once the file has been
*        closed. If the file is not explicitly closed using HDS_CLOSE,
*        it will be done automatically by HDS when the application
*        terminates. The character string should have a declared length
*        equal to the symbolic constant DAT__SZLOC.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-JUN-1991 (DSB):
*        Original version.
*     31-JAN-1992 (DSB):
*        Modified to ignore NDF slice specifiers.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR and added DAT_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRG_ERR'          ! IRG error values.

*  Global Variables:
      INCLUDE 'IRG_COM'          ! IRG common blocks.
*        GCM_OUT( IRH__MAXG ) = LOGICAL (Read)
*           If true, then the group is an output group. Otherwise it is
*           an input group.

*  Arguments Given:
      INTEGER GID
      INTEGER INDEX
      INTEGER INDF1
      CHARACTER CLIST*(*)

*  Arguments Returned:
      INTEGER INDF2
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COMP*44          ! List of component to be reset.
      INTEGER CNEXT              ! Position at which next character is
                                 ! to be stored in COMP.
      LOGICAL CPF( NDF__MXCPF )  ! Component propagation flags.
      CHARACTER EXTN(NDF__MXEXT)*(DAT__SZNAM) ! Extensions not to be
                                 ! propagated.
      INTEGER I                  ! Loop count.
      CHARACTER NAME*(IRH__SZNAM)! NDF file name (without file type).
      INTEGER NEXTN              ! Number of excluded extensions.
      INTEGER PLACE              ! NDF placeholder.
      CHARACTER SLICE*50         ! NDF slice specifier.
      INTEGER START              ! Starting position of NDF slice
                                 ! specifier.
      CHARACTER TITLE*50         ! Group title.
*.

*  Set an initial value for the INDF argument.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the group identified by GID is a group created by IRG.
*  This is assumed to be true if the group title starts with the string
*  given by symbolic constant IRG__PREFX and the access mode is legal.
      CALL IRG1_CHECK( GID, .TRUE., STATUS )

*  If the group is designated as an input group, report an error.
      IF( .NOT. GCM_OUT( GID ) .AND. STATUS .EQ. SAI__OK ) THEN
         CALL IRH_GTTL( GID, TITLE, STATUS )
         STATUS = IRG__IN
         CALL MSG_SETC( 'TTL', TITLE )
         CALL ERR_REP( 'IRG_NDFPR_ERR1',
     :                 'IRG_NDFPR: Group "^TTL" is an input group.',
     :                 STATUS )
      END IF

*  Get the required name.
      CALL IRH_GET( GID, INDEX, 1, NAME, STATUS )

*  Remove any NDF slice specifier.
      CALL IRG1_SLICE( NAME, SLICE, START, STATUS )

*  Create the .SDF file to hold the NDF, and get a locator to the top
*  level object (of type NDF).
      CALL HDS_NEW( NAME, 'NDF', 'NDF', 0, 0, LOC, STATUS )

*  Get an NDF place holder. This will be used to create an NDF at the
*  top level of the container file.
      CALL NDF_PLACE( LOC, ' ', PLACE, STATUS )

*-------------------  TEMPORARY FIX ------------------------------------
*  Eventually the NDF_ library will have a selective copy routine,
*  which will be like NDF_COPY but with an additional CLIST argument
*  (as in NDF_PROP).  Until then, the whole NDF is copied, and then
*  unrequired components and extensions are deleted. First, create the
*  NDF by copying the whole input NDF.
      CALL NDF_COPY( INDF1, PLACE, INDF2, STATUS )

*  Parse the component propagation expression. NB, until the NDF library
*  has a sective copy routine, NDF internal routines must be used. These
*  routines are stored in the IRG library (they cannot be accessed in
*  the NDF sharable image).
      CALL NDF_$PSCPX( CLIST, NDF__MXEXT, EXTN, NEXTN, CPF, STATUS )

*  Delete all the extensions which are not to be propagated.
      DO I = 1, NEXTN
         CALL NDF_XDEL( INDF2, EXTN( I ), STATUS )
      END DO

*  Form a list of all the unrequired components.
      COMP = ' '
      CNEXT = 1

      IF( .NOT. CPF( NDF__ACPF ) ) THEN
         COMP = 'Axis,'
         CNEXT = 6
      END IF

      IF( .NOT. CPF( NDF__DCPF ) ) THEN
         COMP( CNEXT: ) = 'Data,'
         CNEXT = CNEXT + 5
      END IF

      IF( .NOT. CPF( NDF__HCPF ) ) THEN
         COMP( CNEXT: ) = 'Hist,'
         CNEXT = CNEXT + 5
      END IF

      IF( .NOT. CPF( NDF__LCPF ) ) THEN
         COMP( CNEXT: ) = 'Label,'
         CNEXT = CNEXT + 6
      END IF

      IF( .NOT. CPF( NDF__QCPF ) ) THEN
         COMP( CNEXT: ) = 'Qual,'
         CNEXT = CNEXT + 5
      END IF

      IF( .NOT. CPF( NDF__TCPF ) ) THEN
         COMP( CNEXT: ) = 'Title,'
         CNEXT = CNEXT + 6
      END IF

      IF( .NOT. CPF( NDF__UCPF ) ) THEN
         COMP( CNEXT: ) = 'Units,'
         CNEXT = CNEXT + 6
      END IF

      IF( .NOT. CPF( NDF__VCPF ) ) THEN
         COMP( CNEXT: ) = 'Var,'
         CNEXT = CNEXT + 4
      END IF

*  If any components are to be reset...
      IF( COMP .NE. ' ' ) THEN

*  ...remove the comma from the end of the list.
         COMP( CNEXT - 1 : CNEXT - 1 ) = ' '

*  ...reset all the unrequired components.
         CALL NDF_RESET( INDF2, COMP, STATUS )

      END IF

*-----------------------------------------------------------------------

*  If an error occured, annul the NDF identifier and add context
*  information.
 999  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_ANNUL( INDF2, STATUS )
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'IRG_NDFPR_ERR2',
     :         'IRG_NDFPR: Unable to get an NDF identifier for "^NAME"',
     :          STATUS )

      END IF

      END
* $Id$
