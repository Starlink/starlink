      SUBROUTINE SETQUAL( STATUS )
*+
*  Name:
*     SETQUAL

*  Purpose:
*     Assign a specified quality to selected pixels within a group of
*     NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETQUAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine assigns (or optionally removes) the quality
*     specified by parameter QNAME to (or from) selected pixels in each
*     NDF specified by parameter NDF. For more information about using
*     quality within the IRAS90 package see the help on
*     "Quality_in_IRAS90".
*
*     The user can select the pixels to be operated on in one of three
*     ways (see parameter SELECT):
*
*     1) By giving a group of `mask' NDFs (one for each input NDF).
*     Pixels with bad values in the mask NDFs will be selected from the
*     corresponding input NDFs.
*
*     2) By giving a list of pixel indices for the pixels which are to
*     be selected (a single list is provided which is used for all input
*     NDFs).
*
*     3) By giving an ``ARD description'' for the regions of the NDFs
*     which are to be selected. The ARD system (see SUN/183) uses a
*     textual language to describe geometric regions of an array. Text
*     files containing ARD description suitable for use with this
*     routine can be created interactively using the KAPPA routine
*     ARDGEN.
*
*     The operation to be performed on the pixels is specified by
*     parameter FUNCTION. The given quality may be assigned to or
*     removed from pixels within the NDFs. The pixels operated on
*     can either be those selected by the user (as described above),
*     or those not selected.  The quality of all other pixels is left
*     unchanged (unless the parameter FUNCTION is given the value NS+HU
*     or NU+HS). Thus for instance if pixel (1,1) already held the
*     quality specified by QNAME, and the quality was then assigned to
*     pixel (2,2) this would not cause the quality to be removed from
*     pixel (1,1).
*
*     All the input NDFs are presumed to be aligned with each other, but
*     can be of different extent (but they must all have the same number
*     of dimensions).

*  Usage:
*     SETQUAL NDF QNAME COMMENT MASK

*  ADAM Parameters:
*     ARD = LITERAL (Read)
*        An ARD description for the `selected' regions of the input
*        NDFs.  This should be supplied in the form of a group
*        expression (see help on "Group Expressions"). See SUN/183 or
*        KAPPA:ARDGEN for further details of the syntax of an ARD
*        description.  This routine expects positions within the ARD
*        description to be given in terms of pixel co-ordinates. The
*        same ARD description is used for all input NDFs.  The ARD
*        parameter is only prompted for if the SELECT parameter is
*        given the value "ARD".
*     COMMENT = LITERAL (Read)
*        A comment to store with the quality name.  This parameter is
*        only prompted for if an NDF does not already contain a
*        definition of the quality name.  The same comment is used for
*        any other subsequent NDFs which do not contain a definition of
*        the quality name.
*     FUNCTION = LITERAL (Read)
*        This parameter specifies what function is to be performed on
*        the "selected" pixels specified using parameters MASK, LIST or
*        ARD. It can take any of the values "HS", "HU", "NS", "NU",
*        "HS+NU" or "HU+NS"
*
*        HS - Ensure that the quality specified by QNAME is held by
*             all the selected pixels. The quality of all other
*             pixels is left un-changed.
*
*        HU - Ensure that the quality specified by QNAME is held by all
*             the pixels which have not been selected. The quality of
*             the selected pixels is left un-changed.
*
*        NS - Ensure that the quality specified by QNAME is not held by
*             any of the selected pixels. The quality of all other
*             pixels is left un-changed.
*
*        NU - Ensure that the quality specified by QNAME is not held by
*             any of the pixels which have not been selected. The
*             quality of the selected pixels is left un-changed.
*
*        HS+NU - Ensure that the quality specified by QNAME is held by
*             all the selected pixels and not held by any of the other
*             pixels.
*
*        HU+NS - Ensure that the quality specified by QNAME is held by
*             all the pixels which have not been selected and not held
*             by any of the selected pixels.
*                                                                   [HS]
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be added to the NDFs.
*        See help on "History_in_IRAS90" for more information on
*        history.                              [current history setting]
*     LIST = LITERAL (Read)
*        A list of the pixels within the input NDFs which are to be
*        `selected' (see parameter FUNCTION). This should be in the
*        form of a group expression (see help on "Group_expressions")
*        giving a list of pixel indices (eg X1, Y1, X2, Y2,...  for a
*        two dimensional NDF). The same list is used for all input
*        NDFs.  LIST is only prompted for if parameter SELECT is given
*        the value LIST.
*     MASK = NDF (Read)
*        A group of mask NDFs. Each mask is used to define the
*        `selected' pixels within the corresponding input NDF (see
*        parameter FUNCTION). The masks should be aligned
*        pixel-for-pixel with the input NDFs.  Pixels which are bad in
*        the mask NDFs are `selected'. The quality of any pixels which
*        lie outside the bounds of the corresponding mask NDF is left
*        unaltered.  This parameter is only prompted for if the
*        parameter SELECT is given the value MASK.
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     NDF = NDF (Update)
*        A group of NDFs in which the quality information is to be
*        stored. This should be in the form of a group expression (see
*        help on "Group_expresssions").
*     QNAME = LITERAL (Read)
*        The quality name. If the supplied name is not already defined
*        within any of the input NDFs, then a definition of the name is
*        added to the NDF. The user is warned if the quality name is
*        already defined within any of the NDFs.
*     SELECT = LITERAL (Read)
*        This parameter determines how the pixels are selected, and can
*        take the values MASK, LIST or ARD (see parameters MASK, LIST
*        and ARD).
*                                                                 [MASK]
*     XNAME = LITERAL (Read)
*        If an NDF already contains any quality name definitions then
*        new quality names are put in the same extension as the old
*        names.  If no previous quality names have been stored in the
*        NDF then parameter XNAME will be used to obtain the name of an
*        NDF extension in which to store the new quality name.  The
*        extension will be created if it does not already exist (see
*        parameter XTYPE). The same extension is used for all input
*        NDFs.                                           [QUALITY_NAMES]
*     XTYPE = LITERAL (Read)
*        If a new NDF extension is created to hold quality names (see
*        parameter XNAME), then parameter XTYPE is used to obtain the
*        HDS data type for the created extension. The run time default
*        is to give the extension a type identical to its name.       []

*  Examples:
*     SETQUAL M51 SATURATED "Saturated pixels" M51_CUT
*        This example ensures that the quality "SATURATED" is defined
*        within the NDF "M51". The comment "Saturated pixels" is stored
*        with the quality name if it did not already exist in the NDF.
*        The quality SATURATED is then assigned to all pixels for which
*        the corresponding pixel in NDF M51_CUT is bad. The quality of
*        all other pixels is left unchanged.
*
*     SETQUAL "M51,CENA" SOURCE_A SELECT=LIST LIST=^SOURCE_A.LIS FUNCTION=HS+NU
*        This example ensures that pixels within the two NDFs M51 and
*        CENA which are included in the list of pixel indices held in
*        text file SOURCE_A.LIS, have the quality "SOURCE_A", and also
*        ensures that none of the pixels which were not included in
*        SOURCE_A.LIS have the quality.
*
*     SETQUAL M51 SOURCE_B SELECT=ARD ARD="CIR(80,80,9).AND.CIR(70,70,7)"
*        This example ensures that all pixels within the region of
*        intersection of two circles have the quality "SOURCE_B". The
*        first circle is centred on pixel co-ordinates (80,80) and has
*        a radius of 9 pixels. The second circle is centred at (70,70)
*        and has a radius of 7 pixels.
*
*     SETQUAL M51 SOURCE_B SELECT=ARD ARD=^BACKGROUND.ARD
*        This example assigns the quality "SOURCE_B" to pixels of the
*        NDF "M51" as described by an ARD description stored in the text
*        file "BACKGROUND.ARD". This text file could for instance have
*        been created using KAPPA routine ARDGEN.

*  Notes:
*     -  All the quality names which are currently defined within an
*     NDF can be listed by application SHOWQUAL.  Quality name
*     definitions can be removed from an NDF using application REMQUAL.
*     If there is no room for any more quality names to be added to the
*     NDF then REMQUAL can be used to remove a quality name in order to
*     make room for the new quality names.
*
*     -  Only the QUALITY and (optionally) HISTORY components, and the
*     extension specified by parameter XNAME are altered by this
*     application. All other NDF components remain unaltered.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-OCT-1991 (DSB):
*        Original version.
*     12-DEC-1994 (DSB):
*        Added facility to specify selected pixels using an ARD description.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'PAR_ERR'          ! PAR status constants
      INCLUDE 'MSG_PAR'          ! MSG constants.
      INCLUDE 'NDF_PAR'          ! NDF constants.
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ status constants.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns the used length of a string.

*  Local Variables:
      INTEGER ADDED              ! No. of pixels added to the group as a
                                 ! result of the current call to
                                 ! GRP_GROUP.
      INTEGER ALBNDE( NDF__MXDIM )! Lower bounds of excluded area (ARD)
      INTEGER ALBNDI( NDF__MXDIM )! Lower bounds of included area (ARD)
      INTEGER AUBNDE( NDF__MXDIM )! Upper bounds of excluded area (ARD)
      INTEGER AUBNDI( NDF__MXDIM )! Upper bounds of included area (ARD)
      INTEGER BIT                ! The quality bit corresponding to the
                                 ! quality name in range 1-8.
      CHARACTER COMMNT*(IRQ__SZCOM)! Descriptive comment to store with
                                 ! the supplied quality name.
      INTEGER ELMASK             ! No. of elements in mapped DATA
                                 ! component of the used mask.
      LOGICAL FIXED              ! True if the quality is the same at
                                 ! every pixel.
      LOGICAL FLAG               ! True if a group expression was
                                 ! terminated with a minus sign.
      LOGICAL FOUND              ! True if requested NDF extension
                                 ! exists within the input NDF.
      CHARACTER FUNC*2           ! Value of parameter FUNCTION.
      CHARACTER HSTORY*(IRQ__SZQNM+40)! History record added to NDF.
      INTEGER I                  ! Loop count.
      INTEGER IGRP1              ! Identifier for group holding pixel
                                 ! indices.
      INTEGER IGRP2              ! Identifier for group holding NDF
                                 ! names.
      INTEGER IGRP3              ! Identifier for group holding mask NDF
                                 ! names.
      INTEGER IGRP4              ! Identifier for group holding ARD
                                 ! description.
      INTEGER IPARD              ! Pointer to an ARD pixel mask
      INTEGER IPLIST             ! Pointer to the list of pixel
                                 ! positions.
      INTEGER IPMASK             ! Pointer to the used mask.
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds for input NDF.
      INTEGER LBNDI( NDF__MXDIM )! Lower bounds for first input NDF.
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators used to access quality
                                 ! name information in the input NDF.
      INTEGER LQNAME             ! Length of the quality name string.
      INTEGER NC                 ! No. of pixel positions store in
                                 ! the input text file.
      INTEGER NDFIN              ! NDF identifier for input NDF.
      INTEGER NDFMSK             ! NDF identifier for mask NDF.
      INTEGER NDIM               ! No. of dimensions in input NDF.
      INTEGER NDIMI              ! No. of dimensions in first input NDF.
      INTEGER NIN                ! The total number of NDFs to be
                                 ! processed.
      INTEGER NINDEX             ! The total number of pixel indices
                                 ! obtained.
      CHARACTER QNAME*(IRQ__SZQNM)! Supplied quality name.
      INTEGER REGVAL             ! Highest value in ARD mask
      CHARACTER SELECT*4         ! Value of parameter SELECT.
      INTEGER SET                ! The number of pixels which hold the
                                 ! quality.
      LOGICAL THERE              ! True if quality name is already
                                 ! defined within the input NDF.
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds for input NDF.
      INTEGER UBNDI( NDF__MXDIM )! Upper bounds for first input NDF.
      LOGICAL VALUE              ! If FIXED is true, then VALUE is true
                                 ! if all pixel shold the quality, and
                                 ! false if no pixels hold the quality.
      CHARACTER XLOC*(DAT__SZLOC) ! Locator for the NDF extension
                                 ! containing quality name information
                                 ! in the input NDF.
      CHARACTER XNAME*(DAT__SZNAM)! Name of NDF extension containing
                                 ! quality name information in the
                                 ! input NDF.
      CHARACTER XTYPE*(DAT__SZTYP) ! HDS type for the NDF extension
                                 ! containing quality name information
                                 ! in the input NDF.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      THERE = .FALSE.

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get a group containing the names of the NDFs to be processed.
      CALL IRM_RDNDF( 'NDF',  0, 1, '  Give more NDF names...',
     :                IGRP2, NIN, STATUS )

*  Get the number of dimensions in the first NDF in the list. All NDFs
*  in the list must have the same number of dimensions.
      CALL NDG_NDFAS( IGRP2, 1, 'READ', NDFIN, STATUS )
      CALL NDF_BOUND( NDFIN, NDF__MXDIM, LBNDI, UBNDI, NDIMI, STATUS )
      CALL NDF_ANNUL( NDFIN, STATUS )

*  Get a value for parameter QNAME.
      CALL PAR_GET0C( 'QNAME', QNAME, STATUS )
      LQNAME = CHR_LEN( QNAME )

*  Get a value for parameter SELECT.
      CALL PAR_CHOIC( 'SELECT', 'MASK', 'MASK,LIST,ARD', .FALSE.,
     :                 SELECT, STATUS )

*  Get a value for parameter FUNCTION.
      CALL PAR_CHOIC( 'FUNCTION', 'HS', 'HS,HU,NS,NU,HS+NU,HU+NS',
     :                 .FALSE., FUNC, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Start an NDF context.
      CALL NDF_BEGIN

*  If the pixels which are to have their quality modified are specified
*  by means of an ARD description, get the ARD description and store it
*  in a GRP group.
      IF( SELECT .EQ. 'ARD' ) THEN
         CALL ARD_GROUP( 'ARD', GRP__NOID, IGRP4, STATUS )

*  If the pixels which are to have their quality modified are specified
*  by means of a mask image, get a group of mask NDFs.
      ELSE IF( SELECT .EQ. 'MASK' ) THEN
         CALL IRM_RDNDF( 'MASK',  NIN, NIN, '  Give more mask NDFs...',
     :                   IGRP3, NIN, STATUS )

*  Pixels may alternatively be specified by a list of pixel indices
*  supplied by the user. If this option is selected, create a GRP group
*  in which to store the list of pixels.
      ELSE
         CALL GRP_NEW( ' ', IGRP1, STATUS )

*  Each character string stored within the group corresponds to a single
*  pixel index value. The indices should be supplied in the order
*  x1, y1, (z1 etc), x2, y2, (z2 etc).... The user specifies these
*  indices in the form of a set of group expressions. If a group
*  expression is terminated with a minus sign (i.e. "flagged"), then
*  the user is prompted again for another group expression which is
*  expanded into a further list of pixel indices which are appended to
*  the list already obtained.
         FLAG = .TRUE.
         DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )

            CALL GRP_GROUP( 'LIST', GRP__NOID, IGRP1, NINDEX, ADDED,
     :                       FLAG, STATUS )

            IF( FLAG ) THEN
               CALL PAR_CANCL( 'LIST', STATUS )
               CALL MSG_OUTIF( MSG__NORM, 'SETQUAL_MSG1',
     :                        '  Give more pixel indices...',
     :                         STATUS )
            END IF

         END DO

*  If a null value was given for a group expression, annul the error.
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Calculate the number of pixels for which indices have been given.
*  Each pixel requires NDIMI indices.
         NC = NINDEX/NDIMI

*  Report an error if there were insufficient indices to make up a
*  complete list of pixel positions.
         IF( NC*NDIMI .NE. NINDEX .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'NINDEX', NINDEX )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SETQUAL_ERR1',
     :       'SETQUAL: Supplied list of pixel indices is incomplete. '//
     :       '^NINDEX indices given.', STATUS )
         END IF

*  Obtain workspace to hold the numerical indices.
         CALL PSX_CALLOC( NINDEX, '_INTEGER', IPLIST, STATUS )

*  Abort if an error has occured.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round, converting the character values stored in the group into
*  integer values stored within the workspace.
         DO I = 1, NC
            CALL SETQA0( IGRP1, I, NDIMI, NC, 
     :                   %VAL( CNF_PVAL( IPLIST ) ), STATUS )
         END DO

*  The character versions of the pixel indices are no longer needed, so
*  delete the group which holds them.
         CALL GRP_DELET( IGRP1, STATUS )

      END IF

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each NDF to be processed.
      DO I = 1, NIN
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP2, I, 'UPDATE', NDFIN, STATUS )

*  Tell the user which NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', NDFIN )
         CALL MSG_OUTIF( MSG__NORM, 'SETQUAL_MSG2',
     :                   '  Processing ^NDF...', STATUS )

*  Check that the NDF has the same no. of dimensions as the first NDF.
         CALL NDF_BOUND( NDFIN, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

         IF( NDIM .NE. NDIMI .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDFIN )
            CALL MSG_SETI( 'ND', NDIM )
            CALL MSG_SETI( 'NDI', NDIMI )
            CALL ERR_REP( 'SETQUAL_ERR2',
     :        'SETQUAL: ^NDF is ^ND dimensional. Other NDFs are ^NDI '//
     :        'dimensional', STATUS )
         END IF

*  Attempt to locate any existing quality name information in the input
*  NDF. If such information is found, LOCS is returned holding a set of
*  five HDS locators which identify the NDF and various items of
*  quality information. XNAME is returned holding the name of the NDF
*  extension in which the information was found. If no quality name
*  information is found, then an error is reported.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL IRQ_FIND( NDFIN, LOCS, XNAME, STATUS )

*  If existing quality information was found, tell the user which
*  extension it came from.
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETC( 'X', XNAME )
               CALL MSG_OUTIF( MSG__VERB, 'SETQUAL_MSG3',
     :         '    Using quality names defined in NDF extension "^X"',
     :                         STATUS )

*  If no quality name information was found, annul the error and
*  determine the name of the NDF extension which is to hold such
*  information.
            ELSE
               CALL ERR_ANNUL( STATUS )
               CALL PAR_GET0C( 'XNAME', XNAME, STATUS )

*  If the specified extension does not exist, create it with HDS data
*  type specified by parameter XTYPE. The new extension is a scalar
*  object.
               CALL NDF_XSTAT( NDFIN, XNAME, FOUND, STATUS )
               IF( .NOT. FOUND ) THEN
                  CALL PAR_DEF0C( 'XTYPE', XNAME, STATUS )
                  CALL PAR_GET0C( 'XTYPE', XTYPE, STATUS )
                  CALL NDF_XNEW( NDFIN, XNAME, XTYPE, 0, 0, XLOC,
     :                           STATUS )
                  CALL DAT_ANNUL( XLOC, STATUS )
               END IF

*  Create a new structure to hold quality information in the named NDF
*  extension.
               CALL IRQ_NEW( NDFIN, XNAME, LOCS, STATUS )

            END IF

         END IF

*  Attempt to find the quality name within the NDF.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL IRQ_GETQN( LOCS, QNAME, FIXED, VALUE, BIT, COMMNT,
     :                      STATUS )

*  If the name does not exist, annul the error, get an associated
*  descriptive comment, and add the supplied quality name to the
*  quality name information stored in the input NDF. Tell the user that
*  the name has been added to the NDF.
            IF( STATUS .EQ. IRQ__NOQNM ) THEN
               CALL ERR_ANNUL( STATUS )
               THERE = .FALSE.

               CALL PAR_GET0C( 'COMMENT', COMMNT, STATUS )
               CALL IRQ_ADDQN( LOCS, QNAME, .FALSE., COMMNT, STATUS )

               CALL MSG_SETC( 'QN', QNAME )
               CALL NDF_MSG( 'NDF', NDFIN )
               CALL MSG_OUTIF( MSG__NORM, 'SETQUAL_MSG4',
     :             '    Definition of quality name "^QN" added to ^NDF',
     :                         STATUS )

*  If the name was already defined in the NDF, tell the user.
            ELSE
               THERE = .TRUE.

               CALL MSG_SETC( 'QN', QNAME )
               CALL MSG_SETC( 'COM', COMMNT )
               CALL MSG_OUTIF( MSG__NORM, 'SETQUAL_MSG5',
     :      '    Using pre-existing definition of quality "^QN" - ^COM',
     :                        STATUS )

            END IF

         END IF

*  If the function "HS+NU" has been selected, remove the quality from
*  all pixels, and change the function to "HS".
         IF( FUNC .EQ. 'HS+NU' ) THEN
            CALL IRQ_RESQ( LOCS, QNAME, STATUS )
            FUNC = 'HS'

*  Otherwise, if the function "HU+NS" has been selected, remove the
*  quality from all pixels, and change the function to "HU".
         ELSE IF( FUNC .EQ. 'HU+NS' ) THEN
            CALL IRQ_RESQ( LOCS, QNAME, STATUS )
            FUNC = 'HU'

         END IF

*  If the selected pixels are specified by an ARD description...
         IF( SELECT .EQ. 'ARD' ) THEN

*  Get an _INTEGER work array to hold the pixel mask corresponding to
*  the ARD description.
            CALL NDF_SIZE( NDFIN, ELMASK, STATUS )
            CALL PSX_CALLOC( ELMASK, '_INTEGER', IPARD, STATUS )

*  Fill the array with zeros at all pixels not selected by the supplied
*  ARD description, and positive values (2 or greater) at all selected
*  pixels.
            REGVAL = 2
            CALL ARD_WORK( IGRP4, NDIM, LBND, UBND, VAL__BADR, .FALSE.,
     :                     REGVAL, %VAL( CNF_PVAL( IPARD ) ), 
     :                     ALBNDI, AUBNDI,
     :                     ALBNDE, AUBNDE, STATUS )

*  Now produce a _REAL work array in which selected pixels hold bad
*  values, and non selected pixels hold positive values
            CALL PSX_CALLOC( ELMASK, '_REAL', IPMASK, STATUS )
            CALL SETQA1( ELMASK, %VAL( CNF_PVAL( IPARD ) ), 
     :                   %VAL( CNF_PVAL( IPMASK ) ), STATUS )

*  Release the integer work array.
            CALL PSX_FREE( IPARD, STATUS )

*  Modify the QUALITY component of the input NDF section, to perform the
*  requested function.
            IF( STATUS .EQ. SAI__OK ) THEN

               IF( FUNC .EQ. 'HS' ) THEN
                  CALL IRQ_SETQM( LOCS, .TRUE., QNAME, ELMASK,
     :                            %VAL( CNF_PVAL( IPMASK ) ), 
     :                            SET, STATUS )

               ELSE IF( FUNC .EQ. 'HU' ) THEN
                  CALL IRQ_SETQM( LOCS, .FALSE., QNAME, ELMASK,
     :                            %VAL( CNF_PVAL( IPMASK ) ), 
     :                            SET, STATUS )

               ELSE IF( FUNC .EQ. 'NS' ) THEN
                  CALL IRQ_RESQM( LOCS, .TRUE., QNAME, ELMASK,
     :                            %VAL( CNF_PVAL( IPMASK ) ), 
     :                            SET, STATUS )

               ELSE IF( FUNC .EQ. 'NU' ) THEN
                  CALL IRQ_RESQM( LOCS, .FALSE., QNAME, ELMASK,
     :                            %VAL( CNF_PVAL( IPMASK ) ), 
     :                            SET, STATUS )

               END IF

            END IF

*  Release the _REAL work array.
            CALL PSX_FREE( IPMASK, STATUS )

*  If the selected pixels are specified by a mask...
         ELSE IF( SELECT .EQ. 'MASK' ) THEN

*  Get an NDF identifier for the mask NDF.
            CALL NDG_NDFAS( IGRP3, I, 'READ', NDFMSK, STATUS )

*  Create sections from the mask and input NDFs which have matching
*  bounds. This is done by trimming away the areas not covered by both
*  NDFs.
            CALL NDF_MBND( 'TRIM', NDFIN, NDFMSK, STATUS )

*  Map the DATA component of the mask section.
            CALL NDF_MAP( NDFMSK, 'DATA', '_REAL', 'READ', IPMASK,
     :                    ELMASK, STATUS )

*  Modify the QUALITY component of the input NDF section, to perform the
*  requested function.
            IF( STATUS .EQ. SAI__OK ) THEN

               IF( FUNC .EQ. 'HS' ) THEN
                  CALL IRQ_SETQM( LOCS, .TRUE., QNAME, ELMASK,
     :                            %VAL( CNF_PVAL( IPMASK ) ), 
     :                            SET, STATUS )

               ELSE IF( FUNC .EQ. 'HU' ) THEN
                  CALL IRQ_SETQM( LOCS, .FALSE., QNAME, ELMASK,
     :                            %VAL( CNF_PVAL( IPMASK ) ), 
     :                            SET, STATUS )

               ELSE IF( FUNC .EQ. 'NS' ) THEN
                  CALL IRQ_RESQM( LOCS, .TRUE., QNAME, ELMASK,
     :                            %VAL( CNF_PVAL( IPMASK ) ), 
     :                            SET, STATUS )

               ELSE IF( FUNC .EQ. 'NU' ) THEN
                  CALL IRQ_RESQM( LOCS, .FALSE., QNAME, ELMASK,
     :                            %VAL( CNF_PVAL( IPMASK ) ), 
     :                            SET, STATUS )

               END IF

            END IF

*  Annul the locator to mask.
            CALL NDF_ANNUL( NDFMSK, STATUS )

*  If the selected pixels were specified by a list, modify the QUALITY
*  component of the input NDF, to perform the requested function.
         ELSE

            IF( STATUS .EQ. SAI__OK ) THEN

               IF( FUNC .EQ. 'HS' ) THEN
                  CALL IRQ_SETQL( LOCS, .TRUE., QNAME, NDIMI, NC,
     :                            %VAL( CNF_PVAL( IPLIST ) ), 
     :                            SET, STATUS )

               ELSE IF( FUNC .EQ. 'HU' ) THEN
                  CALL IRQ_SETQL( LOCS, .FALSE., QNAME, NDIMI, NC,
     :                            %VAL( CNF_PVAL( IPLIST ) ), 
     :                            SET, STATUS )

               ELSE IF( FUNC .EQ. 'NS' ) THEN
                  CALL IRQ_RESQL( LOCS, .TRUE., QNAME, NDIMI, NC,
     :                            %VAL( CNF_PVAL( IPLIST ) ), 
     :                            SET, STATUS )

               ELSE IF( FUNC .EQ. 'NU' ) THEN
                  CALL IRQ_RESQL( LOCS, .FALSE., QNAME, NDIMI, NC,
     :                            %VAL( CNF_PVAL( IPLIST ) ), 
     :                            SET, STATUS )
               END IF

            END IF

         END IF

*  Tell the user how many pixels hold the quality.
         CALL MSG_SETI( 'SET', SET )
         CALL MSG_SETC( 'QN', QNAME )

         IF( THERE ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'SETQUAL_MSG6',
     :                    '    ^SET pixels now hold the quality "^QN"',
     :                    STATUS )
         ELSE
            CALL MSG_OUTIF( MSG__NORM, 'SETQUAL_MSG7',
     :                    '    ^SET pixels hold the quality "^QN"',
     :                    STATUS )
         END IF

*  Add a history record to the NDF.
         HSTORY = 'Quality "'//QNAME( : LQNAME )//
     :            '" assigned to selected pixels'
         CALL CHR_LDBLK( HSTORY( 10: ) )
         CALL IRM_HIST( 'HISTORY', NDFIN, 'IRAS90:SETQUAL', 1, HSTORY,
     :                  STATUS )

*  If an error has occured, attempt to remove the quality name if it
*  was not defined in the NDF on entry.
         IF( .NOT. THERE .AND. STATUS .NE. SAI__OK ) THEN
            CALL ERR_BEGIN( STATUS )
            CALL IRQ_REMQN( LOCS, QNAME, STATUS )
            CALL ERR_END( STATUS )
         END IF

*  Release the quality name information.
         CALL IRQ_RLSE( LOCS, STATUS )

*  Annul the NDF identifier.
         CALL NDF_ANNUL( NDFIN, STATUS )

*  If a parameter abort or null status exists, abort.
         IF( STATUS .EQ. PAR__ABORT .OR.
     :       STATUS .EQ. PAR__NULL ) GO TO 999

*  If any other error occured processing the current NDF, flush it.
         IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Process the next NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Annul any workspace used to hold pixel indices
 999  CONTINUE
      IF( SELECT .NE. 'MASK' ) CALL PSX_FREE( IPLIST, STATUS )

*   Store a list of the processed files for use by later applications.
      CALL IRM_LISTN( 'NDFLIST', IGRP2, 'SETQUAL', STATUS )

*  Delete the group used to hold the NDF names.
      CALL GRP_DELET( IGRP2, STATUS )

*  Delete any group used to hold an ARD description, or mask NDF names.
      IF( SELECT .EQ. 'ARD' ) THEN
         CALL GRP_DELET( IGRP4, STATUS )

      ELSE IF( SELECT .EQ. 'MASK' ) THEN
         CALL GRP_DELET( IGRP3, STATUS )

      END IF

*  End the NDF context. This causes all mapped arrays to be unmapped,
*  and NDF identifiers to be annulled.
      CALL NDF_END( STATUS )

*  If an error has occured, clear up.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was requested,
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            CALL ERR_REP( 'SETQUAL_ERR3',
     :      'SETQUAL: Error assigning a quality to selected pixels.',
     :                    STATUS )
         END IF

      END IF

      END
