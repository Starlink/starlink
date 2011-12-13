      SUBROUTINE WCSEDIT( STATUS )
*+
*  Name:
*     WCSEDIT

*  Purpose:
*     Modifies or examines image coordinate system information.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSEDIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task performs one of a set of modifications to the WCS
*     (World Coordinate System) components of a list of NDFs.
*     According to the value of the MODE parameter it will:
*        -  Set the Current coordinate system
*        -  Add a new coordinate system
*        -  Remove a coordinate system
*        -  Set an attribute for a coordinate system
*        -  Show the coordinate system which currently exist
*
*     The routine does not fail if some of the requested edits cannot
*     be performed, but a file whose name is given by the NAMELIST
*     parameter records which NDFs were successfully accessed.

*  Usage:
*     WCSEDIT in mode frame

*  ADAM Parameters:
*     COEFFS( * ) = _DOUBLE (Read)
*        If MODE is ADD, this parameter is a list of the coefficients
*        used for the mapping from the target frame to the new frame.
*        Its meaning and the number of values required depend on the
*        value of MAPTYPE:
*           -  UNIT       -- No values are required
*                X' = X
*                Y' = Y
*
*           -  LINEAR     -- Six values C1-C6 are required:
*                X' = C1 + C2 * X + C3 * Y
*                Y' = C4 + C5 * X + C6 * Y
*
*           -  PINCUSHION -- Three values C1-C3 are required:
*                X' = X + C1 * (X - C2) * ( (X - C2)**2 + (Y - C3)**2 ) )
*                Y' = Y + C1 * (Y - C3) * ( (X - C2)**2 + (Y - C3)**2 ) )
*
*     DOMAIN = LITERAL (Read)
*        If MODE is ADD this gives the Domain (name) to be used for the
*        new frame.  Spaces in the name are ignored and letters are
*        folded to upper case.  If the new frame is successfully added
*        and any frame with the same domain name already exists, the
*        old one will be removed, and a message will be printed to that
*        effect.
*        [CCD_WCSEDIT]
*     EPOCH = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        parameter BASEFRAME) for a celestial co-ordinate system, then
*        an epoch value is needed to qualify it. This is the epoch at
*        which the supplied sky positions were determined. It should be
*        given as a decimal years value, with or without decimal places
*        ("1996.8" for example). Such values are interpreted as a
*        Besselian epoch if less than 1984.0 and as a Julian epoch
*        otherwise.
*     FOREXP * ( * ) = LITERAL (Read)
*        If MODE=ADD and MAPTYPE=MATH, this gives the expressions to
*        be used for the forward transformation to be added.  There
*        must be at least two expressions (for the two coordinates)
*        but there may be more if intermediate expressions are to
*        be used.  Expression syntax is fortran-like; see the
*        AST_MATHMAP documentation in SUN/210 for details.
*     FRAME = LITERAL (Read)
*        This parameter specifies the 'target frame', which has the
*        following meaning according to the value of the MODE parameter:
*           -  MODE = CURRENT -- The frame to be made Current
*           -  MODE = REMOVE  -- The frame to remove; if it is a domain
*                                name (see below) then all frames with
*                                that domain will be removed.
*           -  MODE = ADD     -- The new frame will be a copy of the
*                                target frame (though Domain and Title
*                                will be changed), and will be mapped
*                                from it using the mapping given.
*           -  MODE = SET     -- The frame whose attributes are to be set
*           -  MODE = SHOW    -- This parameter is ignored
*
*        The value of this parameter can be one of the following:
*           -  A domain name such as SKY, AXIS, PIXEL, etc.
*           -  An integer value giving the index of the required Frame
*              within the WCS component.
*           -  A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000)
*              (see section "Sky Co-ordinate Systems" in SUN/95).
*           - The Null (!) value; in this case the Current frame is used.
*        A domain name, or !, is usually the most suitable choice.
*        [!]
*     IN = LITERAL (Read)
*        A list specifying the names of the NDFs whose WCS components
*        are to be modified or examined.  The NDF names should be
*        separated by commas and may include wildcards.
*     INVERT = _LOGICAL (Read)
*        If set TRUE the mapping defined by COEFFS will be applied in
*        the reverse direction.
*        [FALSE]
*     INVEXP * ( * ) = LITERAL (Read)
*        If MODE=ADD and MAPTYPE=MATH, this gives the expressions to
*        be used for the inverse transformation to be added.  There
*        must be at least two expressions (for the two coordinates)
*        but there may be more if intermediate expressions are to
*        be used.  Expression syntax is fortran-like; see the
*        AST_MATHMAP documentation in SUN/210 for details.
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
*     MAPTYPE = LITERAL (Read)
*        This parameter is required when MODE is ADD, and specifies the
*        type of mapping which maps from the target frame to the new frame.
*        It may take one of the following values:
*           -  UNIT       -- A Unit mapping
*           -  LINEAR     -- A linear mapping
*           -  PINCUSHION -- A pincushion distortion
*           -  MATH       -- A general algebraic mapping
*        [UNIT]
*     MODE = LITERAL (Read)
*        The action to be performed.  It may take one of the following
*        values:
*           -  ADD      -- Add a new frame (which becomes Current)
*           -  CURRENT  -- Set the Current frame
*           -  REMOVE   -- Remove a frame (Current frame is not changed
*                          unless the Current one is removed)
*           -  SET      -- Set frame attributes (Current frame is not
*                          changed)
*           -  SHOW     -- Display a list of the frames which exist
*        [CURRENT]
*     NAMELIST = LITERAL (Read)
*        The name of an output file in which to write the names of all
*        the NDFs which were successfully accessed.  In particular, if
*        MODE is CURRENT, this list will include all the NDFs which
*        contained the specified frame, but exclude any which did not.
*        [WCSEDIT.LIS]
*     SET = LITERAL (Read)
*        If MODE is SET, then this gives a string of the form
*        "attribute=value" which is to be applied to the frame.  The
*        string is passed straight to the AST_SET routine (see SUN/210).
*     SIMPFI = _LOGICAL (Read)
*        If MODE=SET and MAPTYPE=MATH, this gives the value of the
*        mapping's SimpFI attribute (whether it is legitimate to simplify
*        the forward followed by the inverse transformation to a unit
*        transformation).
*        [TRUE]
*     SIMPIF = _LOGICAL (Read)
*        If MODE=SET and MAPTYPE=MATH, this gives the value of the
*        mapping's SimpIF attribute (whether it is legitimate to simplivy
*        the inverse followed by the forward transformation to a unit
*        transformation).
*        [TRUE]

*  Examples:
*     wcsedit * current ccd_reg
*        This sets the Current coordinate system of all the NDFs in
*        the current directory to 'CCD_REG'.  The names of all the
*        NDFs which had this coordinate system are written to the
*        file WCSEDIT.LIS.  Any which do not appear in this file were
*        not modified by the program.
*
*     wcsedit data* remove frame=4
*        The fourth coordinate frame in the WCS component of each NDF
*        'data*.sdf' is removed.
*
*     wcsedit "first,second" mode=add frame=GRID maptype=pincushion
*             coeffs=[-6.8e-8,0,0] domain=NEW
*        A new coordinate system, called 'NEW', is added to the NDFs
*        first  and second.  It is connected to the previously
*        existing GRID domain by a pincushion distortion mapping
*        centred at the origin with a distortion coefficient of
*        -6.8e-8.  If any frames with domain NEW already exist in
*        those NDFs they are removed.
*
*     wcsedit ndf1 set ! set="domain=NEW,title=New frame"
*        This changes the value of the Domain attribute of the Current
*        coordinate frame in the WCS component of NDF1 to the name
*        "NEW" and  sets the Title attribute of the frame to "New
*        frame".
*
*     wcsedit image1 show
*        This displays all the coordinate frames in image1 with their
*        Domains and titles, and indicates which one is Current.
*
*     wcsedit frm mode=add frame=pixel maptype=math simpif simpfi
*             forexp=["r=sqrt(x*x+y*y)","theta=atan2(y,x)"]
*             invexp=[x=r*cos(theta),y=r*sin(theta)]
*        Adds a frame giving a polar coordinate view of the PIXEL frame.

*  Notes:
*     This routine provides similar functionality to that provided by
*     KAPPA applications WCSADD, WCSREMOVE and WCSFRAME, but allows
*     use of CCDPACK-style NDF lists.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-MAY-1999 (MBT):
*        Original version.
*     2-NOV-1999 (MBT):
*        Added SHOW mode.
*     13-NOV-1999 (MBT):
*        Added MathMaps.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     7-DEC-2000 (MBT):
*        Removed the MODIFIED parameter, which was not too useful since
*        only 132 characters could be stored.  Replaced it by the more
*        CCDPACK-like and more robust NAMELIST parameter, which can
*        provide the same functionality.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'PAR_ERR'          ! PAR system error constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Used length of string

*  Local Constants:
      INTEGER MAXEXP             ! Maximum number of expressions for MathMap
      PARAMETER( MAXEXP = 12 )
      INTEGER MAXELN             ! Maximum length of expressions for MathMap
      PARAMETER( MAXELN = 512 )

*  Local Variables:
      CHARACTER * ( 80 ) BUFFER  ! Buffer for line output
      CHARACTER * ( 16 ) MAPTYP  ! Type of mapping to use
      CHARACTER * ( 16 ) MODE    ! Action to perform
      CHARACTER * ( GRP__SZNAM ) NAME ! Name of NDF
      CHARACTER * ( AST__SZCHR ) DMTARG ! Domain of target frame
      CHARACTER * ( AST__SZCHR ) DMRMV ! Domain to remove
      CHARACTER * ( AST__SZCHR ) DOMAIN ! Domain of new frame
      CHARACTER * ( AST__SZCHR ) FRAME ! Target frame as specified
      CHARACTER * ( AST__SZCHR ) SET ! String pass to AST_SET
      CHARACTER * ( MAXELN ) FOREXP( MAXEXP ) ! Forward expressions for MathMap
      CHARACTER * ( MAXELN ) INVEXP( MAXEXP ) ! Inverse expressions for MathMap
      DOUBLE PRECISION COEFFS( 6 ) ! Coefficients of mapping
      INTEGER BL                 ! Buffer length
      INTEGER FD                 ! File descriptor for namelist file
      INTEGER FRTARG             ! AST pointer to target frame
      INTEGER FRM                ! AST pointer to frame
      INTEGER FRNEW              ! AST pointer to new frame
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER INDF               ! NDF identifier
      INTEGER INGRP              ! Group identifier for NDF group
      INTEGER IWCS               ! AST pointer to WCS component
      INTEGER JTARG              ! Index of target frame
      INTEGER JCUR               ! Index of Current frame in unedited WCS comp
      INTEGER JNEW               ! Index of new frame in WCS component
      INTEGER MAP                ! AST pointer to mapping
      INTEGER NEXP               ! Number of expressions got so far
      INTEGER NFEXP              ! Number of expressions for forward transforms
      INTEGER NIEXP              ! Number of expressions for inverse transforms
      INTEGER NFRAME             ! Number of frames in unedited WCS component
      INTEGER NNDF               ! Number of NDFs
      LOGICAL FIBOTH             ! Do we have both forward and inverse mappings?
      LOGICAL INVERT             ! Is mapping to be applied backwards
      LOGICAL OPEN               ! Is namelist file open?
      LOGICAL SIMPFI             ! SimpFI attribute of MathMap
      LOGICAL SIMPIF             ! SimpIF attribute of MathMap
      LOGICAL SUCCES             ! Whether WCS has been successfully modified

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up the CCDPACK logging system.
      CALL CCD1_START( 'WCSEDIT', STATUS )

*  Begin AST context.
      CALL AST_BEGIN( STATUS )

*  Begin NDF context.
      CALL NDF_BEGIN

*  Get group of NDFs to operate on.
      CALL CCD1_NDFGR( 'IN', INGRP, NNDF, STATUS )

*  Get mode.
      CALL PAR_CHOIC( 'MODE', 'CURRENT', 'CURRENT,ADD,REMOVE,SET,SHOW',
     :                .FALSE., MODE, STATUS )

*  Get frame specification.
      IF ( MODE .EQ. 'SHOW' ) THEN
         FRAME = ' '
      ELSE
         CALL PAR_GET0C( 'FRAME', FRAME, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            FRAME = ' '
         END IF
      END IF

*  Open file for writing names of successfully accessed files.
      CALL CCD1_ASFIO( 'NAMELIST', 'WRITE', 'LIST', 0, FD, OPEN,
     :                 STATUS )

*  Print out any requisite header information.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      IF ( MODE .EQ. 'SHOW' ) THEN
         BUFFER = ' '
         BUFFER( 1: ) = 'Index'
         BUFFER( 7: ) = 'Cur'
         BUFFER( 12: ) = 'Domain'
         BUFFER( 30: ) = 'Title'
         CALL MSG_SETC( 'BUFFER', BUFFER )
         CALL CCD1_MSG( ' ', '    ^BUFFER', STATUS )
         BUFFER = ' '
         BUFFER( 1: ) = '-----'
         BUFFER( 7: ) = '---'
         BUFFER( 12: ) = '------'
         BUFFER( 30: ) = '-----'
         CALL MSG_SETC( 'BUFFER', BUFFER )
         CALL CCD1_MSG( ' ', '    ^BUFFER', STATUS )
      END IF

*  Get ready to loop over NDFs.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Loop over NDFs.
      DO 1 I = 1, NNDF

*  Get NDF and WCS component.
         CALL NDG_NDFAS( INGRP, I, 'UPDATE', INDF, STATUS )
         CALL CCD1_GTWCS( INDF, IWCS, STATUS )

*  Log to user.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL GRP_GET( INGRP, I, 1, NAME, STATUS )
         CALL MSG_SETC( 'NDF', NAME )
         CALL CCD1_MSG( ' ', '^NDF:', STATUS )

*  Store some facts about the initial state of the WCS component.
         NFRAME = AST_GETI( IWCS, 'Nframe', STATUS )
         JCUR = AST_GETI( IWCS, 'Current', STATUS )
         SUCCES = .FALSE.

*  Get the index of the Target frame, that is the one specified by the
*  FRAME (and possibly EPOCH) parameter, without disturbing the Current
*  frame.  If FRAME is null, then use the Current frame.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         IF ( FRAME .NE. ' ' ) THEN
            CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IWCS, ' ', ' ', .FALSE.,
     :                       ' ', STATUS )
         END IF
         JTARG = AST_GETI( IWCS, 'Current', STATUS )
         DMTARG = AST_GETC( IWCS, 'Domain', STATUS )
         CALL AST_SETI( IWCS, 'Current', JCUR, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            SUCCES = .FALSE.
            CALL ERR_ANNUL( STATUS )
            CALL MSG_SETC( 'FRM', FRAME )
            CALL CCD1_MSG( ' ', '      Target frame ''^FRM'' not found',
     :                     STATUS )

*  The requested frame exists.  Now we act according to MODE type.
         ELSE

*  Set Current frame.
            IF ( MODE .EQ. 'CURRENT' ) THEN
               CALL AST_SETI( IWCS, 'Current', JTARG, STATUS )
               CALL MSG_SETC( 'DOM', DMTARG )
               CALL CCD1_MSG( ' ',
     :                        '      Current frame set to domain ^DOM',
     :                        STATUS )
               SUCCES = .TRUE.

*  Set frame attributes.
            ELSE IF ( MODE .EQ. 'SET' ) THEN
               FRTARG = AST_GETFRAME( IWCS, JTARG, STATUS )
               CALL PAR_GET0C( 'SET', SET, STATUS )
               CALL AST_SET( FRTARG, SET( :CHR_LEN( SET ) ), STATUS )
               CALL MSG_SETC( 'DOM', DMTARG )
               CALL MSG_SETC( 'SET', SET )
               CALL CCD1_MSG( ' ',  '      Setting "^SET"' //
     :                        ' applied to domain ^DOM', STATUS )
               SUCCES = .TRUE.

*  Show state of WCS component.
            ELSE IF ( MODE .EQ. 'SHOW' ) THEN

*  Loop through each frame in frameset.
               DO 2 J = 1, AST_GETI( IWCS, 'Nframe', STATUS )

*  Construct a string with Current indicator, domain and title.
                  FRM = AST_GETFRAME( IWCS, J, STATUS )
                  BUFFER = ' '
                  CALL MSG_FMTI( 'IND', 'I3', J )
                  CALL MSG_LOAD( ' ', '^IND', BUFFER( 1: ), BL, STATUS )
                  IF ( J .EQ. JCUR ) BUFFER( 8:8 ) = '*'
                  BUFFER( 12: ) = AST_GETC( FRM, 'Domain', STATUS )
                  IF ( CHR_LEN( BUFFER ) .GT. 28 ) BUFFER( 26: ) = '...'
                  BUFFER( 30: ) = AST_GETC( FRM, 'Title', STATUS )
                  IF ( CHR_LEN( BUFFER ) .GT. 72 ) BUFFER( 70: ) = '...'

*  Output buffer.
                  CALL MSG_SETC( 'BUFFER', BUFFER )
                  CALL CCD1_MSG( ' ', '    ^BUFFER', STATUS )
 2             CONTINUE
               SUCCES = .TRUE.

*  Remove a frame.
            ELSE IF ( MODE .EQ. 'REMOVE' ) THEN

*  Method depends slightly on how it was specified - if a domain was given
*  we want to remove all frames with that domain value, otherwise we can
*  just remove the single frame specified.
*  First try to remove it as a domain name.  This leaves the Current
*  frame intact where possible.
               IF ( FRAME .NE. ' ' ) THEN
                  CALL CCD1_DMPRG( IWCS, FRAME, .TRUE., 0, STATUS )

*  If this has reduced the number of frames then it has succeeded.
                  SUCCES = AST_GETI( IWCS, 'Nframe', STATUS )
     :                     .LT. NFRAME
               END IF
               IF ( .NOT. SUCCES ) THEN

*  Otherwise we can just remove the target frame.
                  CALL AST_REMOVEFRAME( IWCS, JTARG, STATUS )
                  CALL MSG_SETC( 'DOM', DMTARG )
                  CALL CCD1_MSG( ' ',
     :                           '      Removed frame in domain ^DOM',
     :                           STATUS )
                  SUCCES = .TRUE.
               END IF

*  Add a frame.
            ELSE IF ( MODE .EQ. 'ADD' ) THEN

*  Get additional required parameters.
               CALL PAR_CHOIC( 'MAPTYPE', 'UNIT',
     :                         'UNIT,LINEAR,PINCUSHION,MATH', .FALSE.,
     :                         MAPTYP, STATUS )
               CALL PAR_GET0C( 'DOMAIN', DOMAIN, STATUS )
               CALL CHR_RMBLK( DOMAIN )
               CALL CHR_UCASE( DOMAIN )

*  Generate the mapping.
               MAP = AST__NULL

*  Unit mapping (null transformation).
               IF ( MAPTYP .EQ. 'UNIT' ) THEN
                  MAP = AST_UNITMAP( 2, ' ', STATUS )

*  Linear transformation.
               ELSE IF ( MAPTYP .EQ. 'LINEAR' ) THEN
                  CALL PAR_EXACD( 'COEFFS', 6, COEFFS, STATUS )
                  CALL CCD1_LNMAP( COEFFS, MAP, STATUS )

*  Pincushion transformation.
               ELSE IF ( MAPTYP .EQ. 'PINCUSHION' ) THEN
                  CALL PAR_EXACD( 'COEFFS', 3, COEFFS, STATUS )
                  MAP = AST_PCDMAP( COEFFS( 1 ), COEFFS( 2 ), ' ',
     :                              STATUS )

*  General algebraic transformation.
               ELSE IF ( MAPTYP .EQ. 'MATH' ) THEN

*  Get the algebraic expressions for the forward transformation.
                  NFEXP = 0
 5                CONTINUE
                  CALL PAR_GET1C( 'FOREXP', MAXEXP, FOREXP( NFEXP + 1 ),
     :                            NEXP, STATUS )
                  NFEXP = NFEXP + NEXP
                  IF ( NFEXP .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
                     CALL MSG_OUT( ' ', 'At least two expressions '
     :                             // 'required - enter more', STATUS )
                     CALL PAR_CANCL( 'FOREXP', STATUS )
                     GO TO 5
                 END IF

*  Get the algebraic expressions for the inverse transformation.
                  NIEXP = 0
 6                CONTINUE
                  CALL PAR_GET1C( 'INVEXP', MAXEXP, INVEXP( NIEXP + 1 ),
     :                            NEXP, STATUS )
                  NIEXP = NIEXP + NEXP
                  IF ( NIEXP .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
                     CALL MSG_OUT( ' ', 'At least two expressions '
     :                             // 'required - enter more', STATUS )
                     CALL PAR_CANCL( 'INVEXP', STATUS )
                     GO TO 6
                  END IF

*  See whether it looks as if non-dummy expressions have been supplied
*  for both forward and inverse transformations.  This is not foolproof,
*  but the consequences of getting it wrong are not serious.
                  FIBOTH = INDEX( FOREXP( NFEXP ), '=' ) .NE. 0
     :               .AND. INDEX( INVEXP( NFEXP ), '=' ) .NE. 0

*  Get values of SimpFI and SimpIF attributes for mapping if a forward
*  and inverse transformations both exist.
                  IF ( FIBOTH ) THEN
                     CALL PAR_GET0L( 'SIMPFI', SIMPFI, STATUS )
                     CALL PAR_GET0L( 'SIMPIF', SIMPIF, STATUS )
                  ELSE
                     SIMPFI = .FALSE.
                     SIMPIF = .FALSE.
                  END IF

*  Construct the mapping.
                  MAP = AST_MATHMAP( 2, 2, NFEXP, FOREXP, NIEXP, INVEXP,
     :                               ' ', STATUS )

*  Add simplification attributes.
                  CALL AST_SETL( MAP, 'SimpFI', SIMPFI, STATUS )
                  CALL AST_SETL( MAP, 'SimpIF', SIMPIF, STATUS )
               END IF

*  Use inverse mapping if required.
               IF ( MAPTYP .NE. 'UNIT' ) THEN
                  CALL PAR_GET0L( 'INVERT', INVERT, STATUS )
                  IF ( INVERT ) CALL AST_INVERT( MAP, STATUS )
               END IF

*  Copy the target frame, and add the new domain name and title, to
*  create the new frame.
               FRTARG = AST_GETFRAME( IWCS, JTARG, STATUS )
               FRNEW = AST_COPY( FRTARG, STATUS )
               CALL AST_SETC( FRNEW, 'Domain',
     :                        DOMAIN( : CHR_LEN( DOMAIN ) ), STATUS )
               CALL AST_SETC( FRNEW, 'Title', 'Added by WCSEDIT',
     :                        STATUS )

*  Add the new frame to the WCS frameset.
               CALL AST_ADDFRAME( IWCS, JTARG, MAP, FRNEW, STATUS )
               CALL CCD1_MSG( ' ', '      Added new frame', STATUS )
               SUCCES = .TRUE.

*  Remove any previously existing frames in the same domain.
               JNEW = AST_GETI( IWCS, 'Current', STATUS )
               CALL CCD1_DMPRG( IWCS, DOMAIN, .TRUE., JNEW, STATUS )

            END IF

*  Write out the modified WCS component or complain if required.
            IF ( SUCCES .AND. MODE .NE. 'SHOW' ) THEN
               CALL NDF_PTWCS( IWCS, INDF, STATUS )
            END IF
         END IF

*  Write the name of the accessed NDF to the name list, if successful,
*  or log to the user if not.
         IF ( SUCCES ) THEN
            CALL FIO_WRITE( FD, NAME( : CHR_LEN( NAME ) ), STATUS )
         ELSE
            IF ( MODE .EQ. 'SHOW' ) THEN
               CALL CCD1_MSG( ' ', '      NDF not modified', STATUS )
            ELSE
               CALL CCD1_MSG( ' ',
     :         '      NDF not successfully accessed', STATUS )
            END IF
         END IF
 1    CONTINUE

*  Close name list file.
      CALL FIO_CLOSE( FD, STATUS )

*  Exit with error label.  Tidy up after this.
 99   CONTINUE

*  End NDF context.
      CALL NDF_END( STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( INGRP, STATUS )

*  End AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'WCSEDIT_ERR',
     :     'WCSEDIT: Modification of WCS components failed.',
     :     STATUS )
      END IF

*  Close down logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
