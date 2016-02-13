      SUBROUTINE SETQUAL( STATUS )
*+
*  Name:
*     SETQUAL

*  Purpose:
*     Assign a specified quality to selected pixels within an NDF.

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
*     specified by Parameter QNAME to (or from) selected pixels in an
*     NDF.  For more information about using quality within KAPPA see
*     the appendix "Using Quality Names" within SUN/95.
*
*     The user can select the pixels to be operated on in one of three
*     ways (see Parameter SELECT).
*
*     - By giving a `mask' NDF.  Pixels with bad values in the mask NDF
*     will be selected from the corresponding input NDF.
*
*     - By giving a list of pixel indices for the pixels which are to
*     be selected.
*
*     - By giving an ARD file containing a description of the regions of
*     the NDF which are to be selected.  The ARD system (see SUN/183)
*     uses a textual language to describe geometric regions of an array.
*     Text files containing ARD description suitable for use with this
*     routine can be created interactively using the routine ARDGEN.
*
*     The operation to be performed on the pixels is specified by
*     Parameter FUNCTION.  The given quality may be assigned to or
*     removed from pixels within the NDF.  The pixels operated on
*     can either be those selected by the user (as described above),
*     or those not selected.  The quality of all other pixels is left
*     unchanged (unless the Parameter FUNCTION is given the value NS+HU
*     or NU+HS).  Thus for instance if pixel (1,1) already held the
*     quality specified by QNAME, and the quality was then assigned to
*     pixel (2,2) this would not cause the quality to be removed from
*     pixel (1,1).
*
*     This routine can also be used to copy all quality information from
*     one NDF to another (see Parameter LIKE).

*  Usage:
*     setqual ndf qname comment mask

*  ADAM Parameters:
*     ARDFILE = FILENAME (Read)
*        The name of the ARD file containing a description of the parts
*        of the NDF to be `selected'.  The ARD parameter is only
*        prompted for if the SELECT parameter is given the value "ARD".
*        The co-ordinate system in which positions within this file are
*        given should be indicated by including suitable COFRAME or WCS
*        statements within the file (see SUN/183), but will default to
*        pixel co-ordinates in the absence of any such statements.  For
*        instance, starting the file with a line containing the text
*        "COFRAME(SKY,System=FK5)" would indicate that positions are
*        specified in RA/DEC (FK5,J2000).  The statement
*        "COFRAME(PIXEL)" indicates explicitly that positions are
*        specified in pixel co-ordinates.
*     COMMENT = LITERAL (Read)
*        A comment to store with the quality name.  This parameter is
*        only prompted for if the NDF does not already contain a
*        definition of the quality name.
*     FUNCTION = LITERAL (Read)
*        This parameter specifies what function is to be performed on
*        the "selected" pixels specified using Parameters MASK, LIST or
*        ARD.  It can take any of the following values.
*
*        - "HS" -- Ensure that the quality specified by QNAME is held by
*                  all the selected pixels. The quality of all other
*                  pixels is left unchanged.
*
*        - "HU" -- Ensure that the quality specified by QNAME is held by
*                 all the pixels that have not been selected.  The
*                 quality of the selected pixels is left unchanged.
*
*        - "NS" -- Ensure that the quality specified by QNAME is not
*                 held by any of the selected pixels.  The quality of
*                 all other pixels is left unchanged.
*
*        - "NU" -- Ensure that the quality specified by QNAME is not
*                  held by any of the pixels that have not been
*                  selected.  The quality of the selected pixels is left
*                  unchanged.
*
*        - "HS+NU" -- Ensure that the quality specified by QNAME is held
*                  by all the selected pixels and not held by any of
*                  the other pixels.
*
*        - "HU+NS" -- Ensure that the quality specified by QNAME is
*                  held by all the pixels that have not been selected
*                  and not held by any of the selected pixels.
*        ["HS"]
*     LIKE = NDF (Read)
*        An existing NDF from which the QUALITY component and quality names are
*        to be copied. These overwrite any corresponding information in the
*        NDF given by Parameter NDF. If null (!), then the operation of
*        this command is instead determined by Parameter SELECT. [!]
*     LIST = LITERAL (Read)
*        A group of pixels positions within the input NDF listing the
*        pixels that are to be `selected' (see Parameter FUNCTION).
*        Each position should be giving as a list of pixel indices
*        (eg X1, Y1, X2, Y2,...  for a two dimensional NDF). LIST is
*        only prompted for if Parameter SELECT is given the value LIST.
*     MASK = NDF (Read)
*        A mask NDF used to define the `selected' pixels within the
*        input NDF (see Parameter FUNCTION).  The mask should be aligned
*        pixel-for-pixel with the input NDF.  Pixels that are bad in
*        the mask NDF are `selected'. The quality of any pixels that
*        lie outside the bounds of the mask NDF are left unaltered.
*        This parameter is only prompted for if the Parameter SELECT is
*        given the value MASK.
*     NDF = NDF (Update)
*        The NDF in which the quality information is to be stored.
*     QNAME = LITERAL (Read)
*        The quality name. If the supplied name is not already defined
*        within the input NDF, then a definition of the name is
*        added to the NDF. The user is warned if the quality name is
*        already defined within the NDF.
*     READONLY = _LOGICAL (Read)
*        If TRUE, then an error will be reported if any attempt is
*        subsequently made to remove the quality name (e.g. using
*        REMQUAL). [FALSE]
*     SELECT = LITERAL (Read)
*        If Parameter LIKE is null, then this parameter determines how
*        the pixels are selected, and can take the values "Mask",
*        "List" or "ARD" (see Parameters MASK, LIST, and ARD). ["Mask"]
*     QVALUE = _INTEGER (Read)
*        If not null, then the whole Quality array is filled with the
*        constant value given by QVALUE, which must be in the range 0 to
*        255. No other changes are made to the NDF. [!]
*     XNAME = LITERAL (Read)
*        If an NDF already contains any quality name definitions then
*        new quality names are put in the same extension as the old
*        names.  If no previous quality names have been stored in the
*        NDF then Parameter XNAME will be used to obtain the name of an
*        NDF extension in which to store the new quality name.  The
*        extension will be created if it does not already exist (see
*        Parameter XTYPE). [QUALITY_NAMES]
*     XTYPE = LITERAL (Read)
*        If a new NDF extension is created to hold quality names (see
*        Parameter XNAME), then Parameter XTYPE is used to obtain the
*        HDS data type for the created extension. The run time default
*        is to give the extension a type identical to its name.  []

*  Examples:
*     setqual m51 saturated "Saturated pixels" m51_cut
*        This example ensures that the quality "SATURATED" is defined
*        within the NDF "M51". The comment "Saturated pixels" is stored
*        with the quality name if it did not already exist in the NDF.
*        The quality SATURATED is then assigned to all pixels for which
*        the corresponding pixel in NDF M51_CUT is bad. The quality of
*        all other pixels is left unchanged.
*     setqual "m51,cena" source_a select=list list=^source_a.lis function=hs+nu
*        This example ensures that pixels within the two NDFs m51 and
*        cena which are included in the list of pixel indices held in
*        text file source_a.lis, have the quality "SOURCE_A", and also
*        ensures that none of the pixels which were not included in
*        source_a.lis have the quality.
*     setqual m51 source_b select=ard ard=background.ard
*        This example assigns the quality "source_b" to pixels of the
*        NDF "m51" as described by an ARD description stored in the text
*        file "background.ard". This text file could for instance have
*        been created using routine ARDGEN.

*  Notes:
*     -  All the quality names which are currently defined within an
*     NDF can be listed by application SHOWQUAL.  Quality name
*     definitions can be removed from an NDF using application REMQUAL.
*     If there is no room for any more quality names to be added to the
*     NDF then REMQUAL can be used to remove a quality name in order to
*     make room for the new quality names.

*  Related Applications:
*     KAPPA: QUALTOBAD, REMQUAL, SHOWQUAL.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 2002, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008,2013 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-OCT-1991 (DSB):
*        Original version.
*     12-DEC-1994 (DSB):
*        Added facility to specify selected pixels using an ARD
*        description.
*     16-JAN-2002 (DSB):
*        Brought into KAPPA.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variables and wrapped long lines.
*     15-FEB-2008 (DSB):
*        Add READONLY parameter.
*     10-DEC-2013 (DSB):
*        Add LIKE parameter.
*     25-NOV-2014 (DSB):
*        Add QVALUE parameter.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'PAR_ERR'          ! PAR status constants
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
      CHARACTER COMMNT*(IRQ__SZCOM) ! Descriptive comment to store with
                                 ! the supplied quality name.
      LOGICAL CONT               ! ARD description to continue?
      INTEGER EL                 ! No. of elements in mapped array
      INTEGER ELMASK             ! No. of elements in mapped DATA
                                 ! component of the used mask.
      INTEGER FD                 ! File descriptor
      CHARACTER FILNAM*132       ! Name of ARD file
      LOGICAL FIXED              ! True if the quality is the same at
                                 ! every pixel.
      LOGICAL FLAG               ! True if a group expression was
                                 ! terminated with a minus sign.
      LOGICAL FOUND              ! True if requested NDF extension
                                 ! exists within the input NDF.
      CHARACTER FUNC*2           ! Value of parameter FUNCTION.
      INTEGER I                  ! Loop count
      INTEGER IERR               ! Index of first conversion error
      INTEGER IGRP1              ! Identifier for group holding pixel
                                 ! indices.
      INTEGER IGRP2              ! Identifier for group holding ARD
                                 ! description.
      INTEGER IPARD              ! Pointer to an ARD pixel mask
      INTEGER IPLIST             ! Pointer to the list of pixel
                                 ! positions.
      INTEGER IPIX               ! Index of PIXEL Frame within IWCS
      INTEGER IPMASK             ! Pointer to the used mask
      INTEGER IPQIN              ! Pointer to the input quality array
      INTEGER IPQOUT             ! Pointer to the output quality array
      INTEGER IWCS               ! NDF WCS FrameSet
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds for input NDF
      CHARACTER LOCS(5)*(DAT__SZLOC) ! Locators used to access quality
                                 ! name information in the input NDF
      CHARACTER QLOC*(DAT__SZLOC) ! Locator for quality names structure
      CHARACTER XLOC1*(DAT__SZLOC) ! Locator for quality name extension
      CHARACTER XLOC2*(DAT__SZLOC) ! Locator for quality name extension
      INTEGER LQNAME             ! Length of the quality name string
      INTEGER NC                 ! No. of pixel positions store in
                                 ! the input text file
      INTEGER NDFIN              ! NDF identifier for input NDF
      INTEGER NDFLIK             ! NDF identifier for template
      INTEGER NDFMSK             ! NDF identifier for mask NDF
      INTEGER NDFSEC             ! NDF identifier for section of template
      INTEGER NDIM               ! Number of dimensions in input NDF
      INTEGER NERR               ! Number of conversion errors
      INTEGER NINDEX             ! The total number of pixel indices
                                 ! obtained.
      CHARACTER QNAME*(IRQ__SZQNM)! Supplied quality name.
      INTEGER QVALUE             ! Constant quality value to assign
      LOGICAL RDONLY             ! Read-only flag for quality name
      INTEGER REGVAL             ! Highest value in ARD mask
      CHARACTER SELECT*4         ! Value of parameter SELECT
      INTEGER SET                ! The number of pixels which hold the
                                 ! quality
      LOGICAL THERE              ! True if quality name is already
                                 ! defined within the input NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds for input NDF.
      LOGICAL VALUE              ! If FIXED is true, then VALUE is true
                                 ! if all pixel shold the quality, and
                                 ! false if no pixels hold the quality.
      CHARACTER XLOC*(DAT__SZLOC) ! Locator for the NDF extension
                                 ! containing quality name information
                                 ! in the input NDF
      CHARACTER XNAME*(DAT__SZNAM)! Name of NDF extension containing
                                 ! quality name information in the
                                 ! input NDF
      CHARACTER XTYPE*(DAT__SZTYP) ! HDS type for the NDF extension
                                 ! containing quality name information
                                 ! in the input NDF
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the input NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDFIN, STATUS )

*  Get the bounds and number of dimensions.
      CALL NDF_BOUND( NDFIN, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  See if quality info is to be copied from another NDF.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL LPG_ASSOC( 'LIKE', 'READ', NDFLIK, STATUS )

*  If not, annull the error.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  Otherwise copy the info.
         ELSE

*  Get a section of the template NDF that matches the main NDF.
            CALL NDF_SECT( NDFLIK, NDIM, LBND, UBND, NDFSEC, STATUS )

*  If the template NDF has a Quality component, copy it to the main NDF.
            CALL NDF_STATE( NDFSEC, 'QUALITY', THERE, STATUS )
            IF( THERE ) THEN
               CALL NDF_MAP( NDFSEC, 'QUALITY', '_UBYTE', 'READ',
     :                       IPQIN, EL, STATUS )
               CALL NDF_MAP( NDFIN, 'QUALITY', '_UBYTE', 'WRITE',
     :                       IPQOUT, EL, STATUS )
               CALL VEC_UBTOUB( .FALSE., EL, %VAL( CNF_PVAL( IPQIN ) ),
     :                          %VAL( CNF_PVAL( IPQOUT ) ), IERR, NERR,
     :                          STATUS )

*  If the template NDF does not have a Quality component, delete any
*  Quality component in the main NDF.
            ELSE
               CALL NDF_RESET( NDFIN, 'QUALITY', STATUS )
            END IF

*  Delete any quality name info in the main NDF.
            CALL IRQ_DELET( NDFIN, STATUS )

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to locate any existing quality name information in the template
*  NDF. If such information is found, LOCS is returned holding a set of
*  five HDS locators which identify the NDF and various items of
*  quality information. XNAME is returned holding the name of the NDF
*  extension in which the information was found. If no quality name
*  information is found, then an error is reported.
            CALL IRQ_FIND( NDFSEC, LOCS, XNAME, STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN

*  Release the above locators.
               CALL IRQ_RLSE( LOCS, STATUS )

*  Get a locator to the extension containing the template's quality names.
               CALL NDF_XLOC( NDFSEC, XNAME, 'READ', XLOC1, STATUS )

*  Get a locator to the quality name structure in the template.
               CALL DAT_FIND( XLOC1, IRQ__QINAM, QLOC, STATUS )

*  Ensure the main NDF has an extension with the same name, and get a
*  locator to it.
               CALL NDF_XSTAT( NDFIN, XNAME, THERE, STATUS )
               IF( .NOT. THERE ) THEN
                  CALL DAT_TYPE( XLOC1, XTYPE, STATUS )
                  CALL NDF_XNEW( NDFIN, XNAME, XTYPE, 0, 0, XLOC2,
     :                           STATUS )
               ELSE
                  CALL NDF_XLOC( NDFIN, XNAME, 'UPDATE', XLOC2, STATUS )
               END IF

*  Copy the quality names structure from the template to the main NDF.
               CALL DAT_COPY( QLOC, XLOC2, IRQ__QINAM, STATUS )

*  Annul locators.
               CALL DAT_ANNUL( QLOC, STATUS )
               CALL DAT_ANNUL( XLOC1, STATUS )
               CALL DAT_ANNUL( XLOC2, STATUS )

*  If no quality name info was found in the template, just annull the
*  error.
            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF

*  Nothing more to do.
            GO TO 999

         END IF
      END IF

*  See if the Quality array is to be filled with a constant value.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GDR0I( 'QVALUE', 0, 0, 255, .FALSE., QVALUE, STATUS )

*  If not, annull the error.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  Otherwise store the constant value in the NDF.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Map the NDF quality array.
            CALL NDF_MAP( NDFIN, 'QUALITY', '_UBYTE', 'WRITE',
     :                    IPQOUT, EL, STATUS )

*  Fill it with the requested value.
            CALL KPG1_FILLUB( QVALUE, EL, %VAL( CNF_PVAL( IPQOUT ) ),
     :                        STATUS )

*  Unmap the NDF quality array.
            CALL NDF_UNMAP( NDFIN, '*', STATUS )

*  Nothing more to do.
            GO TO 999

         END IF
      END IF

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

*  If the pixels which are to have their quality modified are specified
*  by means of an ARD file, get the ARD description and store it
*  in a GRP group.
      IF( SELECT .EQ. 'ARD' ) THEN

*  Use a literal parameter to obtain the value to avoid having to give
*  the indirection and continuation.  Call FIO to open the file to
*  ensure that the obtained file exists.  Get the name and add the
*  indirection symbol so that ARD does not treat the filename as a
*  literal ARD description.
         CALL FIO_ASSOC( 'ARDFILE', 'READ', 'LIST', 0, FD, STATUS )
         CALL AIF_FLNAM( 'ARDFILE', FILNAM( 2: ), STATUS )
         FILNAM( 1:1 ) = '^'
         CALL FIO_ANNUL( FD, STATUS )

*  Identify the ARD file name.  Use a literal parameter to obtain the
         IGRP2 = GRP__NOID
         CALL ARD_GRPEX( FILNAM, GRP__NOID, IGRP2, CONT, STATUS )

*  If the pixels which are to have their quality modified are specified
*  by means of a mask image, get a mask NDF.
      ELSE IF( SELECT .EQ. 'MASK' ) THEN
         CALL LPG_ASSOC( 'MASK', 'READ', NDFMSK, STATUS )

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
               CALL MSG_OUT( 'SETQUAL_MSG1', '  Give more pixel '//
     :                       'indices...', STATUS )
            END IF

         END DO

*  If a null value was given for a group expression, annul the error.
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Calculate the number of pixels for which indices have been given.
*  Each pixel requires NDIM indices.
         NC = NINDEX/NDIM

*  Report an error if there were insufficient indices to make up a
*  complete list of pixel positions.
         IF( NC*NDIM .NE. NINDEX .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'NINDEX', NINDEX )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SETQUAL_ERR1', 'Supplied list of pixel '//
     :                    'indices is incomplete. ^NINDEX indices '//
     :                    'given.', STATUS )
         END IF

*  Obtain workspace to hold the numerical indices.
         CALL PSX_CALLOC( NINDEX, '_INTEGER', IPLIST, STATUS )

*  Abort if an error has occured.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round, converting the character values stored in the group into
*  integer values stored within the workspace.
         DO I = 1, NC
            CALL KPS1_STQA0( IGRP1, I, NDIM, NC,
     :                       %VAL( CNF_PVAL( IPLIST ) ),
     :                       STATUS )
         END DO

*  The character versions of the pixel indices are no longer needed, so
*  delete the group which holds them.
         CALL GRP_DELET( IGRP1, STATUS )

      END IF

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to locate any existing quality name information in the input
*  NDF. If such information is found, LOCS is returned holding a set of
*  five HDS locators which identify the NDF and various items of
*  quality information. XNAME is returned holding the name of the NDF
*  extension in which the information was found. If no quality name
*  information is found, then an error is reported.
      CALL IRQ_FIND( NDFIN, LOCS, XNAME, STATUS )

*  If no quality name information was found, annul the error and
*  determine the name of the NDF extension which is to hold such
*  information.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL PAR_GET0C( 'XNAME', XNAME, STATUS )

*  If the specified extension does not exist, create it with HDS data
*  type specified by parameter XTYPE. The new extension is a scalar
*  object.
         CALL NDF_XSTAT( NDFIN, XNAME, FOUND, STATUS )
         IF( .NOT. FOUND ) THEN
            CALL PAR_DEF0C( 'XTYPE', XNAME, STATUS )
            CALL PAR_GET0C( 'XTYPE', XTYPE, STATUS )
            CALL NDF_XNEW( NDFIN, XNAME, XTYPE, 0, 0, XLOC, STATUS )
            CALL DAT_ANNUL( XLOC, STATUS )
         END IF

*  Create a new structure to hold quality information in the named NDF
*  extension.
         CALL IRQ_NEW( NDFIN, XNAME, LOCS, STATUS )

      END IF

*  Attempt to find the quality name within the NDF.
      CALL IRQ_GETQN( LOCS, QNAME, FIXED, VALUE, BIT, COMMNT, STATUS )

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
         CALL MSG_OUT( 'SETQUAL_MSG2', '  Definition of quality '//
     :                 'name "^QN" added to ^NDF', STATUS )

*  If the name was already defined in the NDF, tell the user.
      ELSE
         THERE = .TRUE.

         CALL MSG_SETC( 'QN', QNAME )
         CALL MSG_SETC( 'COM', COMMNT )
         CALL MSG_OUT( 'SETQUAL_MSG3', ' Using pre-existing '//
     :                 'definition of quality "^QN" - ^COM', STATUS )
      END IF

*  Flag the name as read-only if required.
      CALL PAR_GET0L( 'READONLY', RDONLY, STATUS )
      CALL IRQ_RWQN( LOCS, QNAME, .TRUE., RDONLY, RDONLY, STATUS )

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

*  Get the WCS FrameSet from the NDF and use it to establish the WCS
*  information used by the following cal to ARD_WORK. Select PIXEL
*  coords as the current Frame first (this means that the default
*  cord system in the ard file will be pixel coords).
         CALL KPG1_GTWCS( NDFIN, IWCS, STATUS )
         CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )
         CALL AST_SETI( IWCS, 'CURRENT', IPIX, STATUS )
         CALL ARD_WCS( IWCS, ' ', STATUS )

*  Fill the array with zeros at all pixels not selected by the supplied
*  ARD description, and positive values (2 or greater) at all selected
*  pixels.
         REGVAL = 2
         CALL ARD_WORK( IGRP2, NDIM, LBND, UBND, VAL__BADR, .FALSE.,
     :                  REGVAL, %VAL( CNF_PVAL( IPARD ) ),
     :                  ALBNDI, AUBNDI,
     :                  ALBNDE, AUBNDE, STATUS )

*  Now produce a _REAL work array in which selected pixels hold bad
*  values, and non selected pixels hold positive values
         CALL PSX_CALLOC( ELMASK, '_REAL', IPMASK, STATUS )
         CALL KPS1_STQA1( ELMASK, %VAL( CNF_PVAL( IPARD ) ),
     :                    %VAL( CNF_PVAL( IPMASK ) ),
     :                    STATUS )

*  Release the integer work array.
         CALL PSX_FREE( IPARD, STATUS )

*  Modify the QUALITY component of the input NDF section, to perform the
*  requested function.
         IF( STATUS .EQ. SAI__OK ) THEN

            IF( FUNC .EQ. 'HS' ) THEN
               CALL IRQ_SETQM( LOCS, .TRUE., QNAME, ELMASK,
     :                         %VAL( CNF_PVAL( IPMASK ) ), SET, STATUS )

            ELSE IF( FUNC .EQ. 'HU' ) THEN
               CALL IRQ_SETQM( LOCS, .FALSE., QNAME, ELMASK,
     :                         %VAL( CNF_PVAL( IPMASK ) ), SET, STATUS )

            ELSE IF( FUNC .EQ. 'NS' ) THEN
               CALL IRQ_RESQM( LOCS, .TRUE., QNAME, ELMASK,
     :                         %VAL( CNF_PVAL( IPMASK ) ), SET, STATUS )

            ELSE IF( FUNC .EQ. 'NU' ) THEN
               CALL IRQ_RESQM( LOCS, .FALSE., QNAME, ELMASK,
     :                         %VAL( CNF_PVAL( IPMASK ) ), SET, STATUS )

            END IF

         END IF

*  Release the _REAL work array.
         CALL PSX_FREE( IPMASK, STATUS )

*  If the selected pixels are specified by a mask...
      ELSE IF( SELECT .EQ. 'MASK' ) THEN

*  Create sections from the mask and input NDFs which have matching
*  bounds. This is done by trimming away the areas not covered by both
*  NDFs.
         CALL NDF_MBND( 'TRIM', NDFIN, NDFMSK, STATUS )

*  Map the DATA component of the mask section.
         CALL NDF_MAP( NDFMSK, 'DATA', '_REAL', 'READ', IPMASK,
     :                 ELMASK, STATUS )

*  Modify the QUALITY component of the input NDF section, to perform the
*  requested function.
         IF( STATUS .EQ. SAI__OK ) THEN

            IF( FUNC .EQ. 'HS' ) THEN
               CALL IRQ_SETQM( LOCS, .TRUE., QNAME, ELMASK,
     :                         %VAL( CNF_PVAL( IPMASK ) ), SET, STATUS )

            ELSE IF( FUNC .EQ. 'HU' ) THEN
               CALL IRQ_SETQM( LOCS, .FALSE., QNAME, ELMASK,
     :                         %VAL( CNF_PVAL( IPMASK ) ), SET, STATUS )

            ELSE IF( FUNC .EQ. 'NS' ) THEN
               CALL IRQ_RESQM( LOCS, .TRUE., QNAME, ELMASK,
     :                         %VAL( CNF_PVAL( IPMASK ) ), SET, STATUS )

            ELSE IF( FUNC .EQ. 'NU' ) THEN
               CALL IRQ_RESQM( LOCS, .FALSE., QNAME, ELMASK,
     :                         %VAL( CNF_PVAL( IPMASK ) ), SET, STATUS )

            END IF

         END IF

*  Annul the NDF identifier for the mask.
         CALL NDF_ANNUL( NDFMSK, STATUS )

*  If the selected pixels were specified by a list, modify the QUALITY
*  component of the input NDF, to perform the requested function.
      ELSE

         IF( STATUS .EQ. SAI__OK ) THEN

            IF( FUNC .EQ. 'HS' ) THEN
               CALL IRQ_SETQL( LOCS, .TRUE., QNAME, NDIM, NC,
     :                         %VAL( CNF_PVAL( IPLIST ) ), SET, STATUS )

            ELSE IF( FUNC .EQ. 'HU' ) THEN
               CALL IRQ_SETQL( LOCS, .FALSE., QNAME, NDIM, NC,
     :                         %VAL( CNF_PVAL( IPLIST ) ), SET, STATUS )

            ELSE IF( FUNC .EQ. 'NS' ) THEN
               CALL IRQ_RESQL( LOCS, .TRUE., QNAME, NDIM, NC,
     :                         %VAL( CNF_PVAL( IPLIST ) ), SET, STATUS )

            ELSE IF( FUNC .EQ. 'NU' ) THEN
               CALL IRQ_RESQL( LOCS, .FALSE., QNAME, NDIM, NC,
     :                         %VAL( CNF_PVAL( IPLIST ) ), SET, STATUS )
            END IF

         END IF

      END IF

*  Tell the user how many pixels hold the quality.
      CALL MSG_SETI( 'SET', SET )
      CALL MSG_SETC( 'QN', QNAME )

      IF( THERE ) THEN
         CALL MSG_OUT( 'SETQUAL_MSG4', '  ^SET pixels now hold '//
     :                 'the quality "^QN"', STATUS )
      ELSE
         CALL MSG_OUT( 'SETQUAL_MSG5', '  ^SET pixels hold the '//
     :                 'quality "^QN"', STATUS )
      END IF

*  If an error has occured, attempt to remove the quality name if it
*  was not defined in the NDF on entry.
      IF( .NOT. THERE .AND. STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )
         CALL IRQ_REMQN( LOCS, QNAME, STATUS )
         CALL ERR_END( STATUS )
      END IF

*  Release the quality name information.
      CALL IRQ_RLSE( LOCS, STATUS )

*  Tidy up
*  =======
 999  CONTINUE

*  Annul any workspace used to hold pixel indices
      IF( SELECT .NE. 'MASK' ) CALL PSX_FREE( IPLIST, STATUS )

*  Delete any group used to hold an ARD description.
      IF( SELECT .EQ. 'ARD' ) CALL GRP_DELET( IGRP2, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error has occured, clear up.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETQUAL_ERR3', 'SETQUAL: Error assigning a '//
     :                 'quality to selected pixels.', STATUS )

      END IF

      END
