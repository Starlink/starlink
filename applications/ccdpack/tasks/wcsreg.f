      SUBROUTINE WCSREG( STATUS )
*+
*  Name:
*     WCSREG

*  Purpose:
*     Aligns NDFs using multiple frames from their WCS components.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSREG( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application takes a set of NDFs which have World Coordinate
*     System (WCS) components, and tries to align them all according
*     to an ordered list of frame domains.  If successful, it adds a 
*     new frame to the WCS component of each within which they are 
*     all aligned.  The TRANLIST or TRANNDF applications can then be
*     used on the resulting NDFs.
*
*     This can be of use when different kinds of alignment information
*     are available between different members of a set of NDFs.  By
*     supplying an ordered list of domains within which to align,
*     the best alignment available can be made between different 
*     members of the set, falling back on second or third choices
*     of alignment types where first choices are not available.
*
*     The application operates on a set of NDFs, IN.  A list of 
*     domains DOMAINLIST within which to align, in order of preference,
*     is specified, and a reference NDF is denoted by REFPOS.  On 
*     successful completion, a new frame (which becomes Current), with
*     a domain given by OUTDOMAIN (default CCD_WCSREG) is added to
*     each of the NDFs in the input set.  Any previously existing 
*     frames with this domain will be removed.  
*
*     For the reference NDF, there is a unit mapping between the new
*     frame and its initial Current frame.  For each other NDF, the
*     program attempts to find a mapping from the reference NDF to it.
*     If it and the reference NDF do not share frames in any of the
*     domains given by the DOMAINS parameter, it will try to use the 
*     WCS components of intermediate NDFs to find a path between them,
*     using only the domains given.  The shortest available path which
*     provides a link is chosen, and if there is more than one which
*     meets this criterion, one which uses domains nearest the head of
*     the list is preferred.
*
*     If there is no path from the reference NDF to one of the other
*     NDFs using the domains given, then no new frame will be written
*     to that NDF (and a message indicating failure will be output),
*     but any previously existing frames in the output domain will 
*     be removed.

*  Usage:
*     WCSREG in domainlist

*  ADAM Parameters:
*     DOMAINS( * ) = LITERAL (Read)
*        This parameter should be a list of frame domains, in order 
*        of preference for achieving alignment.  Alignment paths 
*        between NDFs are selected by shortness of path, but in case
*        of a tie, those using domains nearest the start of this list
*        are used by preference.
*     IN = LITERAL (Read)
*        A list of the names of the NDFs which are to be aligned.  The
*        names should be separated by commas and may include wildcards.
*        They may alternatively be specified using an indirection file
*        (the indirection character is "^").
*
*        If the application is successful, a new frame with a domain
*        determined by the OUTDOMAIN parameter will be added containing
*        the alignment information.  This frame will be made the new 
*        Current domain.
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     OUTDOMAIN = LITERAL (Read)
*        This gives the name of the domain for the new frame which is
*        added to the WCS components of the NDFs on successful 
*        completion.  If any frames in the same domain previously exist
*        in the WCS component, they are removed.  The name is converted
*        to upper case, and whitespace is removed.
*        [CCD_WCSREG]
*     OVERRIDE = _LOGICAL (Read)
*        If not all the NDFs can be aligned using the domains given in
*        DOMAINS then the application will report on which sets of NDFs
*        form connectable subsets of the IN list.  In this case, if this
*        parameter is set FALSE, then the application will exit with an
*        error message.  If it is set TRUE however, it will continue and
*        insert new frames in those NDFs which can be reached from the 
*        one indicated by REFPOS, making no change to the others, except
*        to remove any frames in the domain OUTDOMAIN which already exist.
*        [FALSE]
*     REFPOS = _INTEGER (Read)
*        The position within the IN list which corresponds to the 
*        reference NDF.  For each domain in DOMAINLIST, alignment will
*        be attempted with the reference NDF first, and the mapping 
*        between the output frame and the first matching name in 
*        DOMAINLIST is a unit mapping.
*        [1]

*  Examples:
*     wcsreg * "ccd_reg,sky"
*        In this example all the NDFs in the current directory are
*        being aligned.  All contain a frame in their WCS components
*        in the SKY domain with approximate information about the 
*        pointing, added by the telescope system at observation time.
*        Some of the NDFs however overlap, and have been run through 
*        the REGISTER program which has added a frame in the CCD_REG
*        domain containing more accurate alignment information derived
*        from matching objects between different images.  Where two
*        of the images have frames in the CCD_REG domain, this will
*        be used to align them, but where they do not, the program
*        will fall back on the less accurate SKY domain for alignment.
*        The new frame added will be given the default domain name 
*        CCD_WCSREG.
*
*        After this process, the NDFs can be presented to TRANNDF for
*        resampling prior to making a mosaic.
*     wcsreg "obs1_*,obs2_*" outdomain=final 
*            domainlist="ccd_reg,inst_obs1,inst_obs2"
*        NDFs with names starting 'obs1_' and 'obs2_' are aligned.
*        Where they share the CCD_REG domain this will be used for  
*        alignment, but otherwise the domains INST_OBS1 and INST_OBS2
*        will be used.  These domains perhaps contain information about 
*        the relative alignment of CCDs on the focal plane of the 
*        instrument, and may have been added to the WCS component
*        using the ASTIMP application.  The name FINAL is used for the
*        new domain added to the WCS component.

*  Copyright:
*     Copyright (C) 1999 Particle Physics & Astronomy Research Council

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-APR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'FIO_PAR'          ! Standard FIO constants
      INCLUDE 'CCD1_PAR'         ! Private CCDPACK constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER MXDMN              ! Maximum number of domains in list
      PARAMETER ( MXDMN = 12 )

*  Local variables:
      CHARACTER * ( AST__SZCHR ) DMNS( MXDMN ) ! Domains in list
      CHARACTER * ( AST__SZCHR ) OUTDM ! Name of domain for output frames
      CHARACTER * ( CCD1__BLEN ) BUFFER ! Output buffer
      CHARACTER * ( CCD1__BLEN ) DMNLST ! Comma-separated domain list
      CHARACTER * ( FIO__SZFNM ) FNAME ! Name of NDF
      INTEGER I                  ! Loop variable
      INTEGER INDF( CCD1__MXNDF ) ! NDF identifiers for input NDFs
      INTEGER INGRP              ! GRP identifier for group IN 
      INTEGER IPGRA              ! Pointer to graph array
      INTEGER IPWK1              ! Pointer to workspace array
      INTEGER IPWK2              ! Pointer to workspace array
      INTEGER IPWK3              ! Pointer to workspace array
      INTEGER IPWK4              ! Pointer to workspace array
      INTEGER IWCS( CCD1__MXNDF ) ! AST pointers to WCS components of NDFs
      INTEGER J                  ! Loop variable
      INTEGER JLST               ! Index of frame in DMNLST in reference NDF
      INTEGER JNEW               ! Index of the newly added output frame
      INTEGER JREF               ! Index of reference frame in reference NDF
      INTEGER MAP                ! AST pointer to mapping between framesets
      INTEGER NNDF               ! Number of NDFs in input set
      INTEGER NEDGE              ! Number of edges in the conversion graph
      INTEGER NSTEP              ! Number of steps in subgraph PATH
      INTEGER NDMN               ! Number of domains in list
      INTEGER OUTFR              ! AST pointer to output frame
      INTEGER PATH( 4, CCD1__MXNDF ) ! Subgraph giving mapping path
      INTEGER REFPOS             ! Index of reference NDF in input list
      INTEGER WCSOUT             ! WCS component with new frame added
      INTEGER WORK( 4, CCD1__MXNDF ) ! Workspace array
      INTEGER WORK2( CCD1__MXNDF ) ! Workspace array
      LOGICAL COMPL              ! Completeness of graph
      LOGICAL OVERRD             ! Whether to continue if graph is incomplete

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up the CCDPACK logging system.
      CALL CCD1_START( 'WCSREG', STATUS )

*  Begin AST context.
      CALL AST_BEGIN( STATUS )

*  Begin NDF context.
      CALL NDF_BEGIN

*  Get group of NDFs to operate on.
      NNDF = 0
      CALL CCD1_NDFGR( 'IN', 'UPDATE', INGRP, NNDF, STATUS )

*  Write the names and node indices of the NDFs to the user.
*  Write the names of the associated NDFs out to the user.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    NDFs with graph node indices', STATUS )
      CALL CCD1_MSG( ' ', '    ----------------------------', STATUS )
      DO 6 I = 1, NNDF
         CALL IRH_GET( INGRP, I, 1, FNAME, STATUS )
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL MSG_SETI( 'N', I )
         IF ( I .EQ. REFPOS ) THEN 
            CALL MSG_SETC( 'REF', '(reference)' )
         ELSE
            CALL MSG_SETC( 'REF', ' ' )
         END IF
         CALL CCD1_MSG( ' ', '  ^N) ^FNAME ^REF', STATUS )
 6    CONTINUE
      CALL CCD1_MSG( ' ', ' ', STATUS )
 
*  Get the list of domains and normalise to upper case without blanks.
      CALL PAR_GET1C( 'DOMAINS', MXDMN, DMNS, NDMN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      DO 1 I = 1, NDMN
         CALL CHR_RMBLK( DMNS( I ) )
         CALL CHR_UCASE( DMNS( I ) )
 1    CONTINUE

*  Get the name of the output frame domain and normalise to upper case
*  without blanks
      CALL PAR_GET0C( 'OUTDOMAIN', OUTDM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL CHR_RMBLK( OUTDM )
      CALL CHR_UCASE( OUTDM )

*  Encode the list of domains as a comma-separated list in a single 
*  string.
      CALL CCD1_DLCAT( DMNS, NDMN, ',', DMNLST, STATUS )

*  Report an error if the domain list is empty.
      IF ( DMNLST .EQ. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL CCD1_ERREP( 'WCSREG_NODMNS', 
     :        'WCSREG: No valid domains were specified.', STATUS )
         GO TO 99
      END IF

*  Read in the NDF identifier and WCS component for each NDF.
      DO 3 I = 1, NNDF
         CALL IRG_NDFEX( INGRP, I, INDF( I ), STATUS )
         CALL CCD1_GTWCS( INDF( I ), IWCS( I ), STATUS )
 3    CONTINUE

*  Get the index of the reference NDF.
      CALL PAR_GET0I( 'REFPOS', REFPOS, STATUS )
      IF ( REFPOS .GT. NNDF .OR. REFPOS .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NNDF', NNDF )
         CALL ERR_REP( 'WCSREG_BADREFPOS', 
     :    'WCSREG: REFPOS must be in the range 1 to ^NNDF.', STATUS )
         GO TO 99
      END IF

*  Set the index of the reference frame in the reference WCS frameset.
*  It takes the value of the frameset's initial Current frame.
      JREF = AST_GETI( IWCS( REFPOS ), 'Current', STATUS )

*  Check that the reference NDF has at least one frame in the supplied
*  domain list.
      CALL CCD1_FRDM( IWCS( REFPOS ), DMNLST, JLST, STATUS )
      IF ( JLST .EQ. 0 ) THEN

*  No frame with a domain in the domain list could be found in the 
*  reference NDF.  Output error message and exit with error status.
         CALL MSG_SETC( 'DMNLST', DMNLST )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'WCSREG_NOREFDMN', 
     : 'WCSREG: No frame in any of the domains ^DMNLST', STATUS )
         CALL NDF_MSG( 'IRNDF', INDF( REFPOS ) )
         CALL ERR_REP( ' ', 
     : '        could be found in the reference NDF ^IRNDF.', STATUS )
         GO TO 99
      END IF

*  Set the reference frame in the reference WCS frameset.
      CALL AST_SETI( IWCS( REFPOS ), 'Current', JREF, STATUS )

*  Allocate memory for the conversion graph.
      CALL CCD1_MALL( NNDF * ( NNDF - 1 ) / 2 * 4, '_INTEGER', IPGRA,
     :                STATUS )

*  Create a graph of all possible conversions between the WCS framesets.
      CALL CCD1_CNVGR( IWCS, NNDF, DMNS, NDMN, %VAL( IPGRA ), NEDGE,
     :                 STATUS )

*  Allocate some temporary workspace.
      CALL CCD1_MALL( 4 * NEDGE, '_INTEGER', IPWK1, STATUS )
      CALL CCD1_MALL( NNDF, '_INTEGER', IPWK2, STATUS )
      CALL CCD1_MALL( NNDF, '_LOGICAL', IPWK3, STATUS )
      CALL CCD1_MALL( NNDF, '_LOGICAL', IPWK4, STATUS )

*  Check the graph for completeness and report accordingly.
      CALL CCD1_GRREP( %VAL( IPGRA ), NEDGE, NNDF, REFPOS, 
     :                 %VAL( IPWK1 ), %VAL( IPWK2 ), %VAL( IPWK3 ),
     :                 %VAL( IPWK4 ), COMPL, STATUS )

*  Free temporary workspace.
      CALL CCD1_MFREE( IPWK1, STATUS )
      CALL CCD1_MFREE( IPWK2, STATUS )
      CALL CCD1_MFREE( IPWK3, STATUS )
      CALL CCD1_MFREE( IPWK4, STATUS )

*  If the graph is not complete, we may wish to exit.
      IF ( .NOT. COMPL ) THEN
         OVERRD = .FALSE.
         CALL PAR_GET0L( 'OVERRIDE', OVERRD, STATUS )
         IF ( .NOT. OVERRD ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'WCSREG_NOCOMPL', 
     :'WCSREG: Graph incomplete: ' //
     :'not all the NDFs could be linked by WCS components.', STATUS )
            GO TO 99
         END IF
      END IF

*  Construct the new frame.
      OUTFR = AST_FRAME( 2, ' ', STATUS )
      CALL AST_SETC( OUTFR, 'Title', 'Added by WCSREG', STATUS )
      CALL AST_SETC( OUTFR, 'Domain', OUTDM, STATUS )

*  Exit if there's trouble.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Loop through all the input NDFs adding the new frame connected by
*  an appropriate mapping from the reference NDF.
      DO 4 I = 1, NNDF

*  Output the name of the NDF being considered.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL IRH_GET( INGRP, I, 1, FNAME, STATUS )
         CALL MSG_SETI( 'I', I )
         CALL MSG_SETC( 'NDF', FNAME )
         CALL CCD1_MSG( ' ', '  ^I) ^NDF:', STATUS )

*  Find the best path through the graph from the Current frame of this
*  frameset to the reference (Current) frame in the reference frameset.
         CALL CCD1_GRPTH( %VAL( IPGRA ), NEDGE, I, REFPOS, WORK, WORK2,
     :                    PATH, NSTEP, STATUS )

*  Check whether a successful path was found.
         IF ( NSTEP .GT. 0 ) THEN 

*  Use the path through the graph to find the mapping.
            CALL CCD1_PTHMP( IWCS, PATH, NSTEP, DMNLST, MAP, STATUS )

*  Prepare a copy of the old WCS frameset of the NDF and ensure it has
*  a Base frame in the GRID domain (otherwise the NDF system will
*  complain).
            WCSOUT = AST_COPY( IWCS( I ), STATUS )
            CALL AST_SETI( WCSOUT, 'Base', 1, STATUS )

*  Add the new frame to the WCS frameset.
            CALL AST_ADDFRAME( WCSOUT, AST__CURRENT, MAP, OUTFR,
     :                         STATUS )

*  Get index of newly added frame.
            JNEW = AST_GETI( WCSOUT, 'Current', STATUS )

*  Output a message indicating how the alignment was achieved.
            IF ( I .EQ. REFPOS ) THEN
               CALL CCD1_MSG( ' ', '          (reference NDF)', STATUS )
            ELSE
               DO 5 J = 1, NSTEP
                  WRITE( BUFFER, '( 8X, I4, ''  ->'', I4, 12X, A )' )
     :               PATH( 1, J ), PATH( 2, J ), DMNS( PATH( 3, J ) )
                  CALL CCD1_MSG( ' ', BUFFER, STATUS )
 5             CONTINUE
            END IF
         ELSE

*  No new frame has been added.
            JNEW = 0
            WCSOUT = IWCS( I )

*  Output failure message.
            CALL CCD1_MSG( ' ', 
     : '        ** No alignment path to reference NDF could be found',
     :                     STATUS )
         END IF

*  Purge the frameset of any pre-existing frames in the output domain.
         CALL CCD1_DMPRG( WCSOUT, OUTDM, JNEW, STATUS )

*  Write out the modified WCS frameset to the NDF.
         CALL NDF_PTWCS( WCSOUT, INDF( I ), STATUS )

 4    CONTINUE

*  Error exit label.
 99   CONTINUE

*  Release allocated memory.
      CALL CCD1_MFREE( -1, STATUS )

*  End NDF context.
      CALL NDF_END( STATUS )

*  Close IRH.
      CALL IRH_CLOSE( STATUS )

*  End AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'WCSREG','WCSREG: Alignment failed.',
     :     STATUS )
      END IF

*  Close down logging system.
      CALL CCD1_END( STATUS )
      
      END
* Id: astimp.ifl,v 1.3 1999/04/07 15:27:42 mbt Exp $
