      SUBROUTINE SETEXT( STATUS )
*+
*  Name:
*     SETEXT

*  Purpose:
*     Manipulates the contents of a specified NDF extension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETEXT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task enables the contents of a specified NDF extension to be
*     edited.  It can create a new extension or delete an existing one,
*     can create new scalar components within an extension, or modify
*     or display the values of existing scalar components within the
*     extension.  The task operates on only one extension at a
*     time, and must be closed down and restarted to work on a new
*     extension.
*
*     The task may operate in one of two modes, according to the
*     LOOP parameter.  When LOOP=FALSE only a single option is
*     executed at a time, making the task suitable for use from
*     an ICL procedure.  When LOOP=TRUE several options may be
*     executed at once, making it easier to modify several extension
*     components interactively in one go.

*  Usage:
*     setext ndf xname option cname { ok
*                                   { ctype=? shape=? ok
*                                   { newname=?
*                                   { xtype=?
*                                 option

*  ADAM Parameters:
*     CNAME = LITERAL (Read)
*        The name of component (residing within the extension) to be
*        examined or modified.  It is only accessed when OPTION="Erase",
*        "Get", "Put", or "Rename".
*     CTYPE = LITERAL (Read)
*        The type of component (residing within the extension) to be
*        created.  Allowed values are "LITERAL", "_LOGICAL", "_DOUBLE",
*        "_REAL", "_INTEGER", "_CHAR", "_BYTE", "_UBYTE", "_UWORD",
*        "_WORD".  The length of the character type may be defined by
*        appending the length, for example, "_CHAR*32" is a
*        32-character component.  "LITERAL" and "_CHAR" generate
*        80-character components.  CTYPE is only accessed when
*        OPTION="Put".
*     CVALUE = LITERAL (Read)
*        The value(s) for the component.  Each value is converted to the
*        appropriate data type for the component.  CVALUE is only
*        accessed when OPTION="Put".  Note that for an array of values
*        the list must be enclosed in brackets, even in response to a
*        prompt.  For convenience, if LOOP=TRUE, you are prompted for
*        each string.
*     LOOP = _LOGICAL (Read)
*        LOOP=FALSE requests that only one operation be performed.
*        This allows batch and non-interactive processing or use in
*        procedures.  LOOP=TRUE makes SETEXT operate in a looping mode
*        that allows several modifications and/or examinations to be
*        made to the NDF for one activation.  Setting OPTION to "Exit"
*        will end the looping.  [TRUE]
*     NDF = NDF (Update)
*        The NDF to modify or examine.
*     NEWNAME = LITERAL (Read)
*        The new name of a renamed extension component.  It is only
*        accessed when OPTION="Rename".
*     OK = _LOGICAL (Read)
*        This parameter is used to seek confirmation before a component
*        is erased or overwritten.  A TRUE value permits the operation.
*        A FALSE value leaves the existing component unchanged.  This
*        parameter is ignored when LOOP=FALSE.
*     OPTION = LITERAL (Read)
*        The operation to perform on the extension or a component
*        therein.  The recognised options are:
*
*        - "Delete"  -- Delete an existing NDF extension, and exit the
*                       task (when LOOP=TRUE).
*
*        - "Erase"   -- Erase a component within an NDF extension
*
*        - "Exit"    -- Exit from the task (when LOOP=TRUE)
*
*        - "Get"     -- Display the value of a component within an NDF
*                       extension.  The component must exist.
*
*        - "Put"     -- Change the value of a component within an NDF
*                       extension or create a new component.
*
*        - "Rename"  -- Renames a component.  The component must exist.
*
*        - "Select"  -- Selects another extension.  If the extension
*                       does not exist a new one is created.  This
*                       option is not allowed when LOOP=FALSE.
*
*        The suggested default is the current value, except for the
*        first option where there is no default.
*     SHAPE( ) = _INTEGER (Read)
*        The shape of the component.  Thus 3,2 would be a 2-dimensional
*        object with three elements along each of two lines.  0 creates
*        a scalar.  The suggested default is the shape of the object
*        if it already exists, otherwise it is the current value.  It
*        is only accessed when OPTION="Put".
*     XNAME = LITERAL (Given)
*        The name of the extension to modify.
*     XTYPE = LITERAL (Given)
*        The type of the extension to create.  The suggested default is
*        the current value or "EXT" when there is no current value.

*  Examples:
*     setext hh50 fits delete noloop
*        This deletes the FITS extension in the NDF called hh50.
*     setext myndf select xtype=mytype noloop
*        This creates the extension MYEXT of data type MYTYPE in the
*        NDF called myndf.
*     setext xname=ccdpack ndf=abc erase cname=filter noloop
*        This deletes the FILTER component of the CCDPACK extension in
*        the NDF called abc.
*     setext abc ccdpack put cname=filter cvalue=B ctype=_char noloop
*        This assigns the character value "B" to the FILTER component
*        of the CCDPACK extension a the NDF called abc.
*     setext virgo plate put cname=pitch shape=2 cvalue=[32,16]
*     ctype=_byte noloop
*        This sets the byte 2-element vector of component PITCH
*        of the PLATE extension in the NDF called virgo.  The first
*        element of PITCH is set to 32 and the second to 16.
*     setext virgo plate rename cname=filter newname=waveband noloop
*        This renames the FILTER component of the PLATE extension in
*        the NDF called virgo to WAVEBAND.

*  Notes:
*     -  The "PUT" option allows the creation of extension
*     components with any of the primitive data types.
*     -  The task creates the extension automatically if it does not
*     exist and only allows one extension to be modified at a time.

*  Related Applications:
*     KAPPA: FITSIMP, FITSLIST, NDFTRACE; CCDPACK: CCDEDIT; Figaro:
*     FITSKEYS; HDSTRACE; IRAS90: IRASTRACE, PREPARE.

*  Implementation Deficiencies:
*     The task can only deal with scalar NDF extensions.  It cannot
*     deal with an array of extensions, or with structures within an
*     extension.
*
*     The operation of this utility is rather clunky and tedious.
*     Something better could be designed, perhaps using object-oriented
*     techniques.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1999-2000, 2004 Central Laboratory of the
*     Research Councils. 
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     SMB: Steven M Beard (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-Mar-1993: (SMB):
*        Original version.
*     11-May-1993 (SMB):
*        Included DAT_PAR.
*     07-Dec-1993 (SMB):
*        Comments tidied up.
*     1995 May 9 (MJC):
*        Rewrote and rearranged the prologue.  Wrote the description of
*        the parameters, Examples, Related Applications, Notes and
*        Usage.  Allowed access to arrays and all integer data types.
*        Added rename option.  Allowed more than one extension to be
*        processed.
*     3-SEP-1999 (DSB):
*        Corrected description of DELETE option. Do not cancel parameters
*        when not looping.
*     31-OCT-2000 (DSB):
*        Corrected use of trailing character string length arguments
*        passed by value in the "loop=no option=put ctype=_char" case for
*        non-scalars. Also changed DAT_PUTVL to DAT_PUTVC in the same case.
*        Also removed references to unused parameter RVALUE.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     30-SEP-2004 (PWD):
*        Moved CNF_PAR to declaration section.
*     2010 August 25 (MJC):
*        Used KPG_DIMLS instead of old DIMLST.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! HDS symbolic constants
      INCLUDE 'DAT_ERR'          ! DAT__ error constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER * ( 2 ) CHR_NTH  ! Ordinal string st, nd, th

*  Local Variables:
      CHARACTER * ( VAL__SZI ) CI ! Element number
      INTEGER CLEN               ! Length of character component
      CHARACTER * ( DAT__SZLOC ) CLOC ! HDS locator to a component
      CHARACTER * ( DAT__SZNAM ) CNAME ! Name of component
      INTEGER CPNTR              ! Pointer to array of values to put
      INTEGER CPOS               ! Location within the buffer
      LOGICAL CTHERE             ! Component is present?
      CHARACTER * ( DAT__SZTYP ) CTYPE ! Type of component (should be primitive)
      CHARACTER * ( 132 ) CVALUE ! Character component value
      INTEGER DIMS( DAT__MXDIM ) ! Dimensions of a component
      DOUBLE PRECISION DVALUE    ! Double-precision component value
      INTEGER EL                 ! Number of array elements
      CHARACTER * ( DAT__SZLOC ) ELOC ! HDS locator to component element
      LOGICAL FIRST              ! First time around the loop?
      INTEGER I                  ! Loop counter
      INTEGER INDENT             ! No. of characters indentation
      INTEGER INDF               ! NDF identifier
      INTEGER IVALUE             ! Integer component value
      LOGICAL IFXLOC             ! Extension locator exists?
      CHARACTER * ( 80 ) LINE    ! Buffer
      LOGICAL LOOP               ! Running in loop mode?
      LOGICAL LVALUE             ! Logical component value
      INTEGER NC                 ! Number of characters in element no.
      INTEGER NCOMP              ! Number of components within structure
      INTEGER NDIM               ! Dimensionality of a component
      CHARACTER * ( DAT__SZNAM ) NNAME ! New name of component
      CHARACTER * ( 8 ) OPTION   ! Option to be performed (see above)
      REAL RVALUE                ! Real component value
      LOGICAL RUNING             ! Continue to loop?
      INTEGER SIZE               ! Size of an HDS component or structure
      LOGICAL STRUC              ! HDS component is a structure?
      CHARACTER * ( 8 ) SUGOPT   ! Suggested default option
      LOGICAL VALID              ! Valid HDS locator?
      LOGICAL VERIFY             ! OK to erase extension?
      CHARACTER * ( DAT__SZLOC ) VLOC ! HDS locator to a vectorised
                                 ! component
      CHARACTER * ( DAT__SZLOC ) WKLOC ! HDS locator to character
                                 ! workspace
      CHARACTER * ( DAT__SZLOC ) XLOC ! HDS locator to the extension
                                 ! structure
      CHARACTER * ( DAT__SZNAM ) XNAME ! Name of extension
      LOGICAL XTHERE             ! Extension is present?
      CHARACTER * ( DAT__SZTYP ) XTYPE ! Type of extension (should be
                                 ! structure)

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise flags.
      XTHERE = .FALSE.
      VERIFY = .FALSE.
      FIRST = .TRUE.

*  We do not want to annul a locator before accessing the first
*  extension.
      IFXLOC = .FALSE.

*  Open the NDF context and open the NDF for UPDATE access.  (The NDF
*  must already exist or an error will result).
      CALL NDF_BEGIN
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Find whether or not to loop.
      CALL PAR_GET0L( 'LOOP', LOOP, STATUS )

*  Main loop.
*  =========

*  Loop until told to finish or until a bad status is generated.  The
*  completion conditions for the loop are as follows:
*  (1) Bad status
*  (2) EXIT command obtained
*  (3) DELETE command obtained, but only if verified.
*  (4) Task is not in LOOP mode.
*  Conditions (2), (3) and (4) are controlled via the RUNING flag.
*
*  If LOOP=.FALSE., two passes are made through this loop, not one.
*  The initial pass always performs a "SELECT" action. The second pass
*  then performs the action requested by the user.
      RUNING = .TRUE.

      DO WHILE ( STATUS .EQ. SAI__OK .AND. RUNING )

*  Obtain the option.  The first pass through must select and access
*  an extension.
         IF ( FIRST ) THEN
            OPTION = 'SELECT'
            SUGOPT = 'Junk'

         ELSE IF ( LOOP ) THEN
            CALL PAR_CHOIC( 'OPTION', SUGOPT, 'Delete,Erase,Exit,Get,'/
     :                      /'Put,Rename,Select', .FALSE., OPTION,
     :                      STATUS )
            SUGOPT = OPTION
         ELSE
            CALL PAR_CHOIC( 'OPTION', SUGOPT, 'Delete,Erase,Get,'/
     :                      /'Put,Rename', .FALSE., OPTION, STATUS )
            SUGOPT = OPTION
         END IF

*  Carry out the option specified...

*  Select option
*  =============
         IF ( OPTION .EQ. 'SELECT' ) THEN

*  Annul the previous locator to the extension, if it is valid.
            IF ( IFXLOC ) THEN
               CALL DAT_ANNUL( XLOC, STATUS )
               IFXLOC = .FALSE.
            END IF

*  Obtain the name of the extension to be modified and check whether it
*  exists.
            CALL PAR_GET0C( 'XNAME', XNAME, STATUS )
            CALL CHR_UCASE( XNAME )
            CALL NDF_XSTAT( INDF, XNAME, XTHERE, STATUS )

            IF ( STATUS .EQ. SAI__OK .AND. ( .NOT. XTHERE ) ) THEN

*  The extension does not exist.  Obtain the required type for the
*  extension and create it.  Inform the user what's going on unless
*  it's likely to be in a non-interactive mode.
               CALL PAR_GET0C( 'XTYPE', XTYPE, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

                  IF ( LOOP ) THEN
                     CALL MSG_SETC( 'TXNAME', XNAME )
                     CALL MSG_SETC( 'TXTYPE', XTYPE )
                     CALL MSG_OUT( ' ', 'Creating new scalar '/
     :                 /'extension named ^TXNAME of type ^TXTYPE.',
     :                 STATUS )
                  END IF

                  CALL NDF_XNEW( INDF, XNAME, XTYPE, 0, 0, XLOC,
     :                           STATUS )
                  IFXLOC = .TRUE.
               END IF

            ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  The extension already exists.  Obtain a locator to it.
               CALL NDF_XLOC( INDF, XNAME, 'UPDATE', XLOC, STATUS )
               IFXLOC = .TRUE.

*  Enquire the type and size and ensure the extension is a scalar
*  structure.
               CALL DAT_TYPE( XLOC, XTYPE, STATUS )
               CALL DAT_SIZE( XLOC, SIZE, STATUS )
               CALL DAT_STRUC( XLOC, STRUC, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*  Report what is happening unless it is a single operation.
                  IF ( LOOP. AND. STRUC .AND. SIZE .LE. 1 ) THEN
                     CALL MSG_SETC( 'TXNAME', XNAME )
                     CALL MSG_SETC( 'TXTYPE', XTYPE )
                     CALL MSG_OUT( ' ', 'Opening existing scalar '/
     :                 /'extension named ^TXNAME of type ^TXTYPE.',
     :                 STATUS )

                  ELSE IF ( SIZE .GT. 1 ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'TXNAME', XNAME )
                     CALL ERR_REP( ' ' , 'Extension ^TXNAME is not '/
     :                 /'a scalar structure.', STATUS )
                  END IF

               ELSE
                  CALL MSG_SETC( 'TXNAME', XNAME )
                  CALL ERR_REP( ' ', 'Error accessing existing '/
     :              /'extension, ^TXNAME.', STATUS )
               END IF
            END IF

*  Delete option
*  =============
         ELSE IF ( OPTION .EQ. 'DELETE' ) THEN

*  Delete an existing NDF extension.

*  This is rather a drastic thing to do, so seek verification from the
*  user if the NDF extension is not empty, unless not looping.
            CALL DAT_NCOMP( XLOC, NCOMP, STATUS )

            IF ( STATUS .EQ. SAI__OK .AND. NCOMP .GT. 0 .AND.
     :           LOOP ) THEN
               CALL MSG_SETC( 'TXNAME', XNAME )
               CALL MSG_OUT( ' ', 'Warning.  Extension ^TXNAME '/
     :                       /'is not empty.', STATUS )
               CALL PAR_GET0L( 'OK', VERIFY, STATUS )
            ELSE

               VERIFY = .TRUE.
            END IF

*  Only proceed if everything is ok and the user has given the
*  go-ahead.
            IF ( STATUS .EQ. SAI__OK .AND. VERIFY ) THEN

*  Annul the locator to the extension and then delete the extension.
               IF ( IFXLOC ) THEN

                  CALL DAT_ANNUL( XLOC, STATUS )
                  IFXLOC = .FALSE.
               END IF

               CALL NDF_XDEL( INDF, XNAME, STATUS )

*  Give a warning if the extension did not originally exist (this
*  routine will simply have created it and deleted it again).
               IF ( .NOT. XTHERE ) THEN

                  CALL MSG_SETC( 'TXNAME', XNAME )
                  CALL MSG_OUT( ' ', 'N.B. extension ^TXNAME '/
     :              /'did not originally exist.', STATUS )
               END IF

*  If the extension has been deleted, no further processing will be
*  possible, so the loop should now terminate.
               RUNING = .FALSE.
            END IF

*  Erase option
*  ============
         ELSE IF ( OPTION .EQ. 'ERASE' ) THEN

*  Erase a component within an NDF extension.

*  Obtain the name of the component to be erased, and then erase it.
            CALL PAR_GET0C( 'CNAME', CNAME, STATUS )
            CALL DAT_ERASE( XLOC, CNAME, STATUS )

*  Exit option
*  ===========
         ELSE IF ( OPTION .EQ. 'EXIT' ) THEN

*  Exit from the task. Terminate the loop.
            RUNING = .FALSE.

*  Get option
*  ==========
         ELSE IF ( OPTION .EQ. 'GET' ) THEN

*  Display the value of a component within an NDF extension.

*  Obtain the name of the component.
            CALL PAR_GET0C( 'CNAME', CNAME, STATUS )
            CALL CHR_UCASE( CNAME )

*  Check that the component exists.
            CALL DAT_THERE( XLOC, CNAME, CTHERE, STATUS )

*  Report an error if the component does not exist.
            IF ( .NOT. CTHERE ) THEN
               CALL MSG_SETC( 'XN', XNAME )
               CALL MSG_SETC( 'TCNAME', CNAME )
               CALL MSG_OUT( ' ', 'Extension ^XN does not contain '/
     :         /'a component called ^TCNAME.  Operation aborted.',
     :         STATUS )
            ELSE

*  Find out the type of the component to be displayed (if it exists),
*  and check everything has worked.
               CALL CMP_TYPE( XLOC, CNAME, CTYPE, STATUS )

*  Obtain the size of the object to see whether it is scalar or an
*  array.
               CALL CMP_SIZE( XLOC, CNAME, SIZE, STATUS )

*  Find out whether the object is a structure.
               STRUC = .FALSE.
               CALL CMP_STRUC( XLOC, CNAME, STRUC, STATUS )

*  Warn the user if the object is a structure, hence has no value.
               IF ( STRUC ) THEN
                  CALL MSG_SETC( 'TCNAME', CNAME )
                  CALL MSG_SETC( 'TCTYPE', CTYPE )
                  CALL MSG_OUT( ' ', '  ^TCNAME    '/
     :                          /'^TCTYPE     <structure>.', STATUS )

               ELSE IF ( STATUS .EQ. SAI__OK .AND. SIZE .EQ. 1 ) THEN

*  Obtain the value of the component and display it.  How this is done
*  depends on whether the component type is character, logical or
*  numeric.

*  Character type.
                  IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN
                     CALL NDF_XGT0C( INDF, XNAME, CNAME, CVALUE,
     :                               STATUS )
                     CALL MSG_SETC( 'TCNAME', CNAME )
                     CALL MSG_SETC( 'TCTYPE', CTYPE )
                     CALL MSG_SETC( 'TCVALUE', CVALUE )
                     CALL MSG_OUT( ' ', '  ^TCNAME    ^TCTYPE     '/
     :                             /'^TCVALUE', STATUS )

*  Logical type.
                  ELSE IF ( CTYPE .EQ. '_LOGICAL' ) THEN
                     CALL NDF_XGT0L( INDF, XNAME, CNAME, LVALUE,
     :                               STATUS )
                     CALL MSG_SETC( 'TCNAME', CNAME )
                     CALL MSG_SETC( 'TCTYPE', CTYPE )
                     CALL MSG_SETL( 'TLVALUE', LVALUE )
                     CALL MSG_OUT( ' ', '  ^TCNAME    '/
     :                             /'^TCTYPE     ^TLVALUE', STATUS )

*  Real type.  Allow for bad values.
                  ELSE IF ( CTYPE .EQ. '_REAL' ) THEN
                     CALL NDF_XGT0R( INDF, XNAME, CNAME, RVALUE,
     :                               STATUS )
                     CALL MSG_SETC( 'TCNAME', CNAME )
                     CALL MSG_SETC( 'TCTYPE', CTYPE )
                     IF ( RVALUE .EQ. VAL__BADR ) THEN
                        CALL MSG_OUT( ' ', '  ^TCNAME    '/
     :                                /'^TCTYPE     *', STATUS )
                     ELSE
                        CALL MSG_SETR( 'TRVALUE', RVALUE )
                        CALL MSG_OUT( ' ', '  ^TCNAME    '/
     :                                /'^TCTYPE     ^TRVALUE', STATUS )
                     END IF

*  Double-precision type.  Allow for bad values.
                  ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN
                     CALL NDF_XGT0D( INDF, XNAME, CNAME, DVALUE,
     :                               STATUS )
                     CALL MSG_SETC( 'TCNAME', CNAME )
                     CALL MSG_SETC( 'TCTYPE', CTYPE )
                     IF ( DVALUE .EQ. VAL__BADD ) THEN
                        CALL MSG_OUT( ' ', '  ^TCNAME    '/
     :                                /'^TCTYPE     *', STATUS )
                     ELSE
                        CALL MSG_SETD( 'TDVALUE', DVALUE )
                        CALL MSG_OUT( ' ', '  ^TCNAME    '/
     :                                /'^TCTYPE     ^TDVALUE', STATUS )
                     END IF

*  Integer types.  Allow for bad values.
                  ELSE
                     CALL NDF_XGT0I( INDF, XNAME, CNAME, IVALUE,
     :                               STATUS )
                     CALL MSG_SETC( 'TCNAME', CNAME )
                     CALL MSG_SETC( 'TCTYPE', CTYPE )
                     IF ( ( CTYPE .EQ. '_INTEGER' .AND.
     :                      IVALUE .EQ. VAL__BADI ) .OR.
     :                    ( CTYPE .EQ. '_BYTE' .AND.
     :                      IVALUE .EQ. VAL__BADB ) .OR.
     :                    ( CTYPE .EQ. '_UBYTE' .AND.
     :                      IVALUE .EQ. VAL__BADUB ) .OR.
     :                    ( CTYPE .EQ. '_UWORD' .AND.
     :                      IVALUE .EQ. VAL__BADUW ) .OR.
     :                    ( CTYPE .EQ. '_WORD' .AND.
     :                      IVALUE .EQ. VAL__BADW ) ) THEN
                        CALL MSG_OUT( ' ', '  ^TCNAME    '/
     :                                /'^TCTYPE     *', STATUS )
                     ELSE
                        CALL MSG_SETI( 'TIVALUE', IVALUE )
                        CALL MSG_OUT( ' ', '  ^TCNAME    '/
     :                                /'^TCTYPE     ^TIVALUE', STATUS )
                     END IF

                  END IF

*  Deal with vectors.
               ELSE

*  Get a locator to the component.
                  CALL DAT_FIND( XLOC, CNAME, CLOC, STATUS )

*  Obtain the shape of the component.
                  CALL DAT_SHAPE( CLOC, DAT__MXDIM, DIMS, NDIM, STATUS )

*  Create a string of the dimensions.
                  CALL KPG_DIMLS( NDIM, DIMS, CPOS, LINE, STATUS )

*  Just put the name, type and dimensions on a line.
                  CALL MSG_SETC( 'TCNAME', CNAME )
                  CALL MSG_SETC( 'TCTYPE', CTYPE )
                  CALL MSG_SETC( 'DIMS', LINE )
                  CALL MSG_OUT( ' ', '  ^TCNAME^DIMS    ^TCTYPE',
     :                          STATUS )

*  Write the first few and last few values on the following line.
                  INDENT = 4
                  CPOS = INDENT
                  LINE = ' '
                  CALL KPS1_PUTVL( CLOC, CTYPE, NDIM, DIMS, SIZE, 1,
     :                             INDENT, .TRUE., .FALSE., 0, LINE,
     :                             CPOS, STATUS )
                  CALL MSG_OUT( ' ', LINE, STATUS )

*  Annul the locator.
                  CALL DAT_ANNUL( CLOC, STATUS )
               END IF
            END IF

*  Put option
*  ==========
         ELSE IF ( OPTION .EQ. 'PUT' ) THEN

*  Change the value of a component within an NDF extension, or create
*  a new component.

*  Obtain the name for the component, and ensure this has worked.
            CALL PAR_GET0C( 'CNAME', CNAME, STATUS )
            CALL CHR_UCASE( CNAME )

*  Check whether or not the component already exists.
            CALL DAT_THERE( XLOC, CNAME, CTHERE, STATUS )

*  Assume that the object is not a structure.
            STRUC = .FALSE.
            IF ( CTHERE ) THEN

*  Obtain a locator to the object.
               CALL DAT_FIND( XLOC, CNAME, CLOC, STATUS )

*  Find out whether or not the object is a structure.
               CALL DAT_STRUC( CLOC, STRUC, STATUS )

            END IF

*  Warn the user if the object is a structure.
            IF ( STRUC .AND. STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETC( 'TCNAME', CNAME )
               CALL MSG_OUT( ' ', '  ^TCNAME is a structure and '/
     :           /'cannot be assigned a value.', STATUS )

            ELSE IF ( STATUS .EQ. SAI__OK .AND. CTHERE ) THEN

*  Obtain the size of the object to see whether it is scalar or an
*  array.  Obtain its type too.
               CALL DAT_SIZE( CLOC, SIZE, STATUS )
               CALL DAT_TYPE( CLOC, CTYPE, STATUS )

*  Use the type obtained as a default for the new type.
               CALL PAR_DEF0C( 'CTYPE', CTYPE, STATUS )

*  Obtain the shape of the component.
               CALL DAT_SHAPE( CLOC, DAT__MXDIM, DIMS, NDIM, STATUS )

*  Use the existing shape as a default for the new shape.
               IF ( SIZE .LE. 1 ) THEN
                  CALL PAR_DEF0I( 'SHAPE', 0, STATUS )
               ELSE
                  CALL PAR_DEF1I( 'SHAPE', NDIM, DIMS, STATUS )
               END IF

*  Seek verification from the user.
               IF ( LOOP ) THEN
                  CALL MSG_SETC( 'TCNAME', CNAME )
                  CALL MSG_SETC( 'TCTYPE', CTYPE )
                  CALL MSG_OUT( ' ', 'Warning.  Component ^TCNAME of '/
     :              /'type ^TCTYPE already exists.', STATUS )
                  CALL PAR_GET0L( 'OK', VERIFY, STATUS )
               END IF
            ELSE

               VERIFY = .TRUE.
            END IF

*  Only proceed if everything is ok and the user has given the
*  go-ahead.
            IF ( STATUS .EQ. SAI__OK .AND. VERIFY ) THEN

*  Obtain the HDS primitive data type.
               CALL KPG1_GETYP( 'CTYPE', CTYPE, STATUS )

*  Obtain the new shape for the component.
               CALL PAR_GDRVI( 'SHAPE', DAT__MXDIM, 0, VAL__MAXI,
     :                         DIMS, NDIM, STATUS )

*  Cancel the parameter default.
               CALL PAR_UNSET( 'SHAPE', 'DEFAULT', STATUS )

*  Look for a scalar denoted by a zero dimension.
               IF ( DIMS( 1 ) .EQ. 0 .AND. NDIM .EQ. 1 ) THEN
                  NDIM = 0
               END IF

               IF ( STATUS .EQ. SAI__OK .AND. NDIM .EQ. 0 ) THEN

*  Obtain the value of the component and attempt to write it.  How this
*  is done depends on the specified type for the component.

*  Character type.  To obtain the correct length pass the length of
*  a string.
                  IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN

*  Create the object explicitly as there are no NDF_XPTOx
*  instantiations for the other integer data types.
                     IF ( .NOT. CTHERE ) THEN
                        CALL DAT_NEW( XLOC, CNAME, CTYPE, 0, 0, STATUS )
                        CALL DAT_FIND( XLOC, CNAME, CLOC, STATUS )
                     END IF

*  Obtain the value.
                     CALL PAR_GET0C( 'CVALUE', CVALUE, STATUS )

*  Assign the value to the type.
                     CALL DAT_PUTC( CLOC, 0, 0, CVALUE, STATUS )

*  Free the locator.
                     IF ( .NOT. CTHERE ) CALL DAT_ANNUL( CLOC, STATUS )

*  Logical type.
                  ELSE IF ( CTYPE .EQ. '_LOGICAL' ) THEN
                     CALL PAR_GET0L( 'CVALUE', LVALUE, STATUS )
                     CALL NDF_XPT0L( LVALUE, INDF, XNAME, CNAME,
     :                               STATUS )

*  Double-precision type.
                  ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN
                     CALL PAR_GET0D( 'CVALUE', DVALUE, STATUS )
                     CALL NDF_XPT0D( DVALUE, INDF, XNAME, CNAME,
     :                               STATUS )

*  Real type.
                  ELSE IF ( CTYPE .EQ. '_REAL' ) THEN
                     CALL PAR_GET0R( 'CVALUE', RVALUE, STATUS )
                     CALL NDF_XPT0R( RVALUE, INDF, XNAME, CNAME,
     :                               STATUS )

*  Integer type.
                  ELSE IF ( CTYPE .EQ. '_INTEGER' ) THEN
                     CALL PAR_GET0I( 'CVALUE', IVALUE, STATUS )
                     CALL NDF_XPT0I( IVALUE, INDF, XNAME, CNAME,
     :                               STATUS )

*  Other integer types.  Do not set up limits for the data value
*  appropriate to the chosen data type, becuse this causes problems in
*  the type converison for the literal parameter CVALUE.  If the user
*  gives a bad value, the conversion will set the value to be bad.
                  ELSE

*  Obtain the value.
                     CALL PAR_GET0I( 'CVALUE', IVALUE, STATUS )

*  Create the object explicitly as there are no NDF_XPTOx
*  instantiations for the other integer data types.
                     IF ( .NOT. CTHERE ) THEN
                        CALL DAT_NEW( XLOC, CNAME, CTYPE, 0, 0, STATUS )
                        CALL DAT_FIND( XLOC, CNAME, CLOC, STATUS )
                     END IF

*  Assign the value to the type.  Here we can use an integer type, as
*  HDS will do the type conversion for us.
                     CALL DAT_PUTI( CLOC, 0, 0, IVALUE, STATUS )

*  Free the locator.
                     IF ( .NOT. CTHERE ) CALL DAT_ANNUL( CLOC, STATUS )
                  END IF

*  Make an error report as HDS does not.
                  IF ( STATUS .EQ. DAT__CONER ) THEN
                     CALL MSG_SETC( 'TCTYPE', CTYPE )
                     CALL ERR_REP( 'SETEXT_CONER',
     :                 'Error converting the value to type '/
     :                 /'^TCTYPE.  Stored as a bad value.', STATUS )
                  END IF

*  Deal with arrays.
               ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Create the object explicitly as there are no NDF_XPT1x routines to do
*  it.
                  IF ( .NOT. CTHERE ) THEN
                     CALL DAT_NEW( XLOC, CNAME, CTYPE, NDIM, DIMS,
     :                             STATUS )
                     CALL DAT_FIND( XLOC, CNAME, CLOC, STATUS )
                  END IF

*  Find the number of elements required.
                  EL = 1
                  DO I = 1, NDIM
                     EL = EL * DIMS( I )
                  END DO

*  Obtain the value of the component and attempt to write it.  How this
*  is done depends on the specified type for the component.

*  Character type
*  --------------
                  IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN

*  Get all the values one at a time if prompting.
                     IF ( LOOP ) THEN

*  We want to address a vectorised array.
                        CALL DAT_VEC( CLOC, VLOC, STATUS )

*  Process the array a value at a time to avoid using HDS workspace.
                        DO I = 1, EL

*  Set a new prompt for each character value.
                           CALL CHR_ITOC( I, CI, NC )
                           CALL PAR_PROMT( 'CVALUE', 'Give the '/
     :                       /CI( :NC)//CHR_NTH( I )//' value.',
     :                       STATUS )

*  Obtain the value.
                           CALL PAR_GET0C( 'CVALUE', CVALUE, STATUS )

*  Get a locator to the element.
                           CALL DAT_CELL( VLOC, 1, I, ELOC, STATUS )

*  Put the value into the element.
                           CALL DAT_PUT0C( ELOC, CVALUE, STATUS )

*  Annul the locator to the element.
                           CALL DAT_ANNUL( ELOC, STATUS )

*  Cancel the parameter value.
                           CALL PAR_CANCL( 'CVALUE', STATUS )
                        END DO

*  Annul the locator to the vectorised element.
                        CALL DAT_ANNUL( VLOC, STATUS )

*  Reset the prompt.
                        CALL PAR_PROMT( 'CVALUE', 'Component value',
     :                                  STATUS )

*  Get all the values at one go for noloop mode.
                     ELSE

*  Find the character length.
                        CALL CHR_CTOI( CTYPE( 7: ), CLEN, STATUS )

*  Obtain workspace for the array.
                        CALL AIF_GETVM( '_BYTE', 1, EL * CLEN, CPNTR,
     :                                  WKLOC, STATUS )

*  Obtain the requested number of values.
                        CALL PAR_EXACC( 'CVALUE', EL,
     :                                  %VAL( CNF_PVAL( CPNTR ) ),
     :                                  STATUS, %VAL( CNF_CVAL( 6 ) ),
     :                                  %VAL( CNF_CVAL( CLEN ) ) )

*  Write the array to the component.
                        CALL DAT_PUTVC( CLOC, EL,
     :                                  %VAL( CNF_PVAL( CPNTR ) ),
     :                                  STATUS,
     :                                  %VAL( CNF_CVAL( DAT__SZLOC ) ),
     :                                  %VAL( CNF_CVAL( CLEN ) ) )

*  Free the workspace.
                        CALL AIF_ANTMP( WKLOC, STATUS )
                     END IF

*  Logical type
*  ------------
                  ELSE IF ( CTYPE .EQ. '_LOGICAL' ) THEN

*  Obtain workspace for the array.
                     CALL PSX_CALLOC( EL, CTYPE, CPNTR, STATUS )

*  Obtain the requested number of values.
                     CALL PAR_EXACL( 'CVALUE', EL,
     :                               %VAL( CNF_PVAL( CPNTR ) ),
     :                               STATUS )

*  Write the array to the component.
                     CALL DAT_PUTVL( CLOC, EL,
     :                               %VAL( CNF_PVAL( CPNTR ) ), STATUS )

*  Free the workspace.
                     CALL PSX_FREE( CPNTR, STATUS )

*  Double-precision type
*  ---------------------
                  ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN

*  Obtain workspace for the array.
                     CALL PSX_CALLOC( EL, CTYPE, CPNTR, STATUS )

*  Obtain the requested number of values.
                     CALL PAR_EXACD( 'CVALUE', EL,
     :                               %VAL( CNF_PVAL( CPNTR ) ),
     :                               STATUS )

*  Write the array to the component.
                     CALL DAT_PUTVD( CLOC, EL,
     :                               %VAL( CNF_PVAL( CPNTR ) ), STATUS )

*  Free the workspace.
                     CALL PSX_FREE( CPNTR, STATUS )

*  Real type
*  ---------
                  ELSE IF ( CTYPE .EQ. '_REAL' ) THEN

*  Obtain workspace for the array.
                     CALL PSX_CALLOC( EL, CTYPE, CPNTR, STATUS )

*  Obtain the requested number of values.
                     CALL PAR_EXACR( 'CVALUE', EL,
     :                               %VAL( CNF_PVAL( CPNTR ) ),
     :                               STATUS )

*  Write the array to the component.
                     CALL DAT_PUTVR( CLOC, EL,
     :                               %VAL( CNF_PVAL( CPNTR ) ), STATUS )

*  Free the workspace.
                     CALL PSX_FREE( CPNTR, STATUS )

*  Integer type
*  ------------
                  ELSE IF ( CTYPE .EQ. '_INTEGER' ) THEN

*  Obtain workspace for the array.
                     CALL PSX_CALLOC( EL, CTYPE, CPNTR, STATUS )

*  Obtain the requested number of values.
                     CALL PAR_EXACI( 'CVALUE', EL,
     :                               %VAL( CNF_PVAL( CPNTR ) ),
     :                               STATUS )

*  Write the array to the component.
                     CALL DAT_PUTVI( CLOC, EL,
     :                               %VAL( CNF_PVAL( CPNTR ) ), STATUS )

*  Free the workspace.
                     CALL PSX_FREE( CPNTR, STATUS )

*  Other integer types
*  -------------------
*
                  ELSE

*  Obtain workspace for the array.
                     CALL PSX_CALLOC( EL, '_INTEGER', CPNTR, STATUS )

*  Obtain the requested number of values.
                     CALL PAR_EXACI( 'CVALUE', EL,
     :                               %VAL( CNF_PVAL( CPNTR ) ),
     :                               STATUS )

*  Write the array to the component.  The values will be converted to
*  the appropriate type.
                     CALL DAT_PUTVI( CLOC, EL,
     :                               %VAL( CNF_PVAL( CPNTR ) ), STATUS )

*  Free the workspace.
                     CALL PSX_FREE( CPNTR, STATUS )

                  END IF

*  Make an error report as HDS does not.
                  IF ( STATUS .EQ. DAT__CONER ) THEN
                     CALL MSG_SETC( 'TCTYPE', CTYPE )
                     CALL ERR_REP( 'SETEXT_CONER',
     :                 'Error converting one or more values to type '/
     :                 /'^TCTYPE.  Each is assigned the bad value.',
     :                 STATUS )
                  END IF

               END IF

*  Release the locator to the component.
               CALL DAT_VALID( CLOC, VALID, STATUS )
               IF ( VALID ) CALL DAT_ANNUL( CLOC, STATUS )
            END IF

*  Rename option
*  =============
         ELSE IF ( OPTION .EQ. 'RENAME' ) THEN

*  Rename a component within an NDF extension.

*  Obtain the name for the component and ensure this has worked.
            CALL PAR_GET0C( 'CNAME', CNAME, STATUS )
            CALL CHR_UCASE( CNAME )

*  Check whether the component already exists.  If it does obtain its
*  type and seek verification from the user.
            CALL DAT_THERE( XLOC, CNAME, CTHERE, STATUS )

            IF ( STATUS .EQ. SAI__OK .AND. CTHERE ) THEN

*  Obtain the name for the component and ensure this has worked.
               CALL PAR_GET0C( 'NEWNAME', NNAME, STATUS )
               CALL CHR_UCASE( NNAME )

*  Obtain a locator to the component.  Rename the component.  Free the
*  locator.
               CALL DAT_FIND( XLOC, CNAME, CLOC, STATUS )
               CALL DAT_RENAM( CLOC, NNAME, STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )

*  Warn the user if the component does not exist.
            ELSE IF ( STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETC( 'TCNAME', CNAME )
               CALL MSG_OUT( ' ', 'Warning. Component ^TCNAME does '/
     :           /'not exist and therefore cannot be renamed.', STATUS )
            END IF

         END IF

*  Do another pass through the main loop if LOOP is TRUE or if this
*  was the first pass through.
         IF ( FIRST ) THEN
            RUNING = .TRUE.
         ELSE
            RUNING = RUNING .AND. LOOP
         END IF

*  If another loop will be performed, and we are looping, flush any error
*  messages, reset the status, and cancel any parameters which may need
*  to be reprompted for on the next pass.
         IF ( RUNING .AND. LOOP ) THEN

            IF ( STATUS .NE. PAR__ABORT ) THEN
               IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
            END IF

            IF ( FIRST ) THEN
               CALL PAR_CANCL( 'XNAME', STATUS )
               CALL PAR_CANCL( 'XTYPE', STATUS )
            ELSE
               CALL PAR_CANCL( 'XNAME', STATUS )
               CALL PAR_CANCL( 'XTYPE', STATUS )
               CALL PAR_CANCL( 'OPTION', STATUS )
               CALL PAR_CANCL( 'CNAME', STATUS )
               CALL PAR_CANCL( 'CTYPE', STATUS )
               CALL PAR_CANCL( 'CVALUE', STATUS )
               CALL PAR_CANCL( 'SHAPE', STATUS )
               CALL PAR_CANCL( 'OK', STATUS )
               CALL PAR_CANCL( 'NEWNAME', STATUS )
            END IF

         END IF

*  The next pass will not be the first pass.
         FIRST = .FALSE.

      END DO

*  Regardless of the status, annul any locator to the NDF extension
*  and close the NDF context.
      IF ( IFXLOC ) CALL DAT_ANNUL( XLOC, STATUS )
      CALL NDF_END( STATUS )

      END
