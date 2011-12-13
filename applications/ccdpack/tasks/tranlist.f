      SUBROUTINE TRANLIST( STATUS )
*+
*  Name:
*     TRANLIST

*  Purpose:
*     Transforms lists of positions.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TRANLIST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine transforms positions stored in position lists.
*     Transformations are defined either by a set of 6 coefficients
*     for the linear transform, by an algebraic expression given by
*     you, by using a forward or inverse mapping from a TRANSFORM
*     structure, or by a mapping between two coordinate sytems
*     in the WCS component of the NDF.

*  Usage:
*     tranlist inlist outlist trtype

*  ADAM Parameters:
*     EPOCHIN = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        parameter FRAMEIN) for a celestial co-ordinate system, then
*        an epoch value is needed to qualify it. This is the epoch at
*        which the supplied sky positions were determined. It should be
*        given as a decimal years value, with or without decimal places
*        ("1996.8" for example). Such values are interpreted as a
*        Besselian epoch if less than 1984.0 and as a Julian epoch
*        otherwise.
*        [Dynamic]
*     EPOCHOUT = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        parameter FRAMEOUT) for a celestial co-ordinate system, then
*        an epoch value is needed to qualify it. This is the epoch at
*        which the supplied sky positions were determined. It should be
*        given as a decimal years value, with or without decimal places
*        ("1996.8" for example). Such values are interpreted as a
*        Besselian epoch if less than 1984.0 and as a Julian epoch
*        otherwise.
*        [Dynamic]
*     FA-FZ = LITERAL (Read)
*        These parameters supply the values of "sub-expressions" used in
*        the expressions XFOR and YFOR. These parameters
*        should be used when repeated expressions are present in complex
*        transformations. Sub-expressions may contain references to
*        other sub-expressions and constants (PA-PZ).
*        An example of using sub-expressions is:
*           XFOR > PA*ASIND(FA/PA)*X/FA
*           YFOR > PA*ASIND(FA/PA)*Y/FA
*           FA > SQRT(X*X+Y*Y)
*           PA > 100D0
*     FORWARD = _LOGICAL (Read)
*        If TRTYPE="STRUCT" then this parameter's value controls whether
*        the forward or inverse mapping in the transform structure is
*        used.
*        [TRUE]
*     FRAMEIN = LITERAL (Read)
*        If TRTYPE="WCS" then the transformation is a mapping from the
*        frame specified by this parameter to that specified by the
*        FRAMEOUT parameter.  The value of this parameter can be one of
*        the following:
*        - A domain name such as SKY, AXIS, PIXEL, etc.
*        - An integer value giving the index of the required Frame
*          within the WCS component.
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000)
*          (see section "Sky Co-ordinate Systems" in SUN/95).
*        [PIXEL]
*     FRAMEOUT = LITERAL (Read)
*        If TRTYPE="WCS" then the transformation is a mapping from the
*        coordinate frame specified by the FRAMEIN parameter to that
*        specified by this parameter.  The value of this parameter can
*        be one of the following:
*        - A domain name such as SKY, AXIS, PIXEL, etc.
*        - An integer value giving the index of the required Frame
*          within the WCS component.
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000)
*          (see section "Sky Co-ordinate Systems" in SUN/95).
*        - Null (!), indicating the Current frame.
*        [!]
*     INEXT = _LOGICAL (Read)
*        If NDFNAMES is TRUE and the transformation is to be specified
*        using a WCS component (TRTYPE="WCS"), then this parameter
*        controls whether or not the WCS component should be located
*        in each of the NDFs.  If set FALSE, the WCSFILE parameter will
*        be used.
*
*        If NDFNAMES is TRUE and the transformation is to be specified
*        using a TRANSFORM structure (TRTYPE="STRUCT") then this
*        parameter controls whether or not the structure should be
*        located in the CCDPACK extension of each of the NDFs.  If
*        set FALSE, the TRANSFORM parameter will be used.
*
*        If this option is chosen then the WCS component or transform
*        structure in EACH NDF will be applied to the associated
*        position list. So for instance if you have a set of registered
*        NDFs and positions these may be transformed all at once to and
*        from the reference coordinate system.
*        [TRUE]
*     INLIST = LITERAL (Read)
*        This parameter is used to access the names of the lists which
*        contain the positions and, if NDFNAMES is TRUE, the names of
*        the associated NDFs. If NDFNAMES is TRUE the names of the
*        position lists are assumed to be stored in the extension of
*        the NDFs (in the CCDPACK extension item CURRENT_LIST) and the
*        names of the NDFs themselves should be given in response (and
*        may include wildcards).
*
*        If NDFNAMES is FALSE then the actual names of the position
*        lists should be given. These may not use wildcards but may be
*        specified using indirection (other CCDPACK position list
*        processing routines will write the names of their results
*        files into a file suitable for use in this manner) the
*        indirection character is "^".
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
*     NAMELIST = _FILENAME
*        Only used if NDFNAMES is FALSE. This specifies the name of a
*        file to contain a listing of the names of the output lists.
*        This file may then be used to pass the names onto another
*        CCDPACK application using indirection.
*        [TRANLIST.LIS]
*     NDFNAMES = _LOGICAL (Read)
*        If TRUE then the routine will assume that the names of the
*        position lists are stored in the NDF CCDPACK extensions under
*        the item "CURRENT_LIST". The names will be present in the
*        extension if the positions were located using a CCDPACK
*        application (such as FINDOBJ). Using this facility allows the
*        transparent propagation of position lists through processing
*        chains.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [TRUE]
*     OUTLIST = FILENAME (Write)
*        A list of names specifying the result files. The names of the
*        lists may use modifications of the input names (NDF names if
*        available otherwise the names of the position lists). So if
*        you want to call the output lists the same name as the input
*        NDFs except to add a type use.
*
*           OUTLIST > *.FIND
*
*        If no NDF names are given (NDFNAMES is FALSE) then if you want
*        to change the extension of the files (from ".CENT" to ".TRAN"
*        in this case) use
*
*           OUTLIST > *|CENT|TRAN|
*
*        Or alternatively you can use an explicit list of names.
*        These may use indirection elements as well as names separated
*        by commas.
*     PA-PZ = _DOUBLE (Read)
*        These parameters supply the values of constants used in the
*        expressions XFOR and YFOR. Using parameters allows
*        the substitution of repeated constants (with extended
*        precisions?) using one reference. It allows easy modification
*        of parameterised expressions (expressions say with an
*        adjustable centre) provided the application has not been used
*        to apply a new transform using expressions. The parameter PI
*        has a default value of 3.14159265359D0. An example of using
*        parameters is:
*           XFOR > SQRT(FX*FX+FY*FY)
*           YFOR > ATAN2D(-FY,FX)
*           FX > X-PA
*           FY > Y-PB
*           PA > X-centre-value
*           PB > Y-centre-value
*        This maps (X,Y) to (R,THETA) about a specified centre.
*     TRTYPE = LITERAL (Read)
*        The form of the transformation which is to be applied to the
*        positions in the input lists. This can take the values
*
*           - COEFF
*           - EXPRES
*           - WCS
*           - STRUCT
*
*        or unique abbreviations of.
*
*        COEFF means that a linear transformation of the form
*              X' = A + B*X + C*Y
*              Y' = D + E*X + F*Y
*        is to be applied to the data. In this case a prompt for the
*        values of the coefficients A-F is made.
*
*        EXPRES indicates that you want to supply algebraic-like
*        expressions to transform the data. In this case the parameters
*        XFOR and YFOR are used to obtain the expressions. Things like
*            XFOR > 2.5*COS(X)+LOG10(Y)
*            YFOR > 2.5*SIN(X)+EXP(Y)
*        are allowed. The expression functions must be in terms of X
*        and Y. For a full set of possible functions see SUN/61
*        (TRANSFORM).
*
*        WCS means that the transformation will be taken from the WCS
*        component of an NDF.  In this case the name of the NDF
*        containing the WCS component should be supplied (this will be
*        picked up automatically through the association of an NDF
*        and a position list if NDFNAMES and INEXT are both TRUE).
*        The transformation will be that between the coordinate systems
*        defined by the FRAMEIN and FRAMEOUT parameters.
*
*        STRUCT signifies that a transform structure (probably created
*        by REGISTER or CCDEDIT) is to be applied to the data. In this
*        case the name of the object containing the structure should
*        be supplied (this will be picked up automatically through the
*        association of an NDF and a position list if NDFNAMES and
*        INEXT are both TRUE) and whether to use the forward or inverse
*        mappings (the FORWARD parameter).
*        [COEFF]
*     TR( 6 ) = _DOUBLE (Read)
*        If TRTYPE="COEFF" is chosen then the values of this parameter
*        are the 6 coefficients of a linear transformation of the type.
*              X' = PA + PB*X + PC*Y
*              Y' = PD + PE*X + PF*Y
*        The default is the identity transformation.
*        [0,1,0,0,0,1] [PA,PB,PC,PD,PE,PF]
*     TRANSFORM = TRN (Read)
*        If TYPE="STRUCT" and INEXT=FALSE then this parameter is used to
*        access the HDS object which contains the transform structure.
*        The standard place to store a transform structure (in CCDPACK)
*        is
*
*            - NDF_NAME.MORE.CCDPACK.TRANSFORM
*
*        Only one structure can be used at a time.
*     WCSFILE = NDF (Read)
*        If TRTYPE="WCS" and INEXT is false, then this parameter gives
*        the name of the NDF containing the WCS component containing
*        coordinate systems to be used for the transformation.
*     XFOR = LITERAL (Read)
*        If TRTYPE="EXPRES" is chosen then this parameter specifies the
*        transformation that maps to the new X coordinate. The
*        expression can contain constants, arithmetic operators
*        (+,-,/,*,**) and the functions described in SUN/61
*        (SIN,COS,TAN, etc.).
*
*        As an inverse mapping is not required in this application
*        there is no need to use the X'=func(X,Y) form only func(X,Y)
*        is required, however, the variables must be given as
*        "X" and "Y".
*     YFOR = LITERAL (Read)
*        If TRTYPE="EXPRES" is chosen then this parameter specifies the
*        transformation that maps to the new Y coordinate. The
*        expression can contain constants, arithmetic operators
*        (+,-,/,*,**) and the functions described in SUN/61
*        (SIN,COS,TAN, etc.).
*
*        As an inverse mapping is not required in this application
*        there is no need to use the Y'=func(X,Y) form only func(X,Y)
*        is required, however, the variables must be given as
*        "X" and "Y".

*  Examples:
*     tranlist inlist='*' outlist='*.reg' trtype=wcs framein=pixel
*        In this example all the NDFs in the current directory are
*        accessed and their associated position lists are opened.
*        The WCS component of each NDF is used to transform the
*        coordinates in the position lists from pixel coordinates to
*        coordinates in the Current coordinate system.  The output
*        lists are called ndf-name.reg and are associated with the
*        NDFs.
*     tranlist inlist='*' outlist='*.tran' trtype=struct forward=false
*        In this example transform structures in each of the NDFs in
*        the current directory are used to transform their associated
*        position lists.  The inverse mappings are used.
*     tranlist inlist='*_reduced' outlist='*.off' trtype=coeff
*              tr='[10,1,0,20,0,1]'
*        In this example the position lists associated with the NDFs
*        *_reduced are transformed using the linear fit coefficients
*        [10,1,0,20,0,1] resulting in a shift of all the positions in
*        these lists of +10 in X and +20 in Y. The output lists are
*        called ndf_name.off and are now associated with the NDFs.
*     tranlist inlist='*_resam' outlist='*.rot' trtype=coeff
*              tr='[0,0.707,-0.707,0,0.707,0.707]'
*        In this example a linear transformation is used to rotate the
*        positions by 45 degrees about [0,0]. The linear coefficients
*        for a rotation are specified as [0, cos, -sin, 0, sin, cos].
*     tranlist inlist=here outlist=reflected.dat trtype=express
*              xfor=-x yfor=-y
*        In this example a transformation expression is used to reflect
*        the positions stored in the list associated with NDF here
*        about the X and Y axes. A similar effect could be achieved
*        with trtype=coeff and tr=[0,-1,0,0,0,-1].
*     tranlist inlist=ndf_with_list outlist='*.tran' trtype=express
*              xfor='(fx*(1d0+pa*(fx*fx+fy*fy)))*ps+px'
*              yfor='(fy*(1d0+pa*(fx*fx+fy*fy)))*ps+py'
*              fx='(x-px)/ps' fy='(y-py)/ps'
*              pa=pincushion_distortion_factor px=X-centre-value
*              py=Y-centre-value ps=scale_factor
*        In this example a general transformation (which is of the type
*        used when applying pin cushion distortions) is applied to the
*        position list associated with the NDF ndf_with_list. The
*        transformation is parameterised with an offset and scale
*        (converts pixel coordinates to one projection radius units)
*        applied to the input coordinates and a pincushion distortion
*        parameter pa.
*     tranlist ndfnames=false inlist='"list1,list2,list3"'
*              outlist='"outlist1,outlist2,outlist3"' namelist=newfiles
*        In this example the input position lists are not associated
*        with NDFs (ndfnames=false) And have to be specified by name
*        (no wildcards allowed). The output lists are also specified in
*        this fashion, but, the same effect could have been achieved
*        with outlist=out* as the input list names are now used as as
*        modifiers for the output list names (the NDF names are always
*        used when they are available -- see previous examples). The
*        names of the output lists are written to the file newfiles,
*        this could be used to specify the names of these files to
*        another application using indirection (e.g inlist=^newfiles,
*        with ndfnames=false again).  The transformation type is not
*        specified in this example and will be obtained by prompting.

*  Notes:
*     - Position list formats.
*
*       CCDPACK supports data in two formats.
*
*       CCDPACK format - the first three columns are interpreted as the
*       following.
*
*          - Column 1: an integer identifier
*          - Column 2: the X position
*          - Column 3: the Y position
*
*       The column one value must be an integer and is used to identify
*       positions which are the same but which have different locations
*       on different images. Values in any other (trailing) columns are
*       usually ignored.
*
*       EXTERNAL format - positions are specified using just an X and
*       a Y entry and no other entries.
*
*         - Column 1: the X position
*         - Column 2: the Y position
*
*       This format is used by KAPPA applications such as CURSOR.
*
*       Comments may be included in a file using the characters "#" and
*       "!". Columns may be separated by the use of commas or spaces.
*
*     - NDF extension items.
*
*       If NDFNAMES is TRUE then the item "CURRENT_LIST" of the
*       .MORE.CCDPACK structure of the input NDFs will be located
*       and assumed to contain the names of the lists whose positions
*       are to be transformed. On exit this item will be updated to
*       reference the name of the transformed list of positions.
*
*       This application may also access the item "TRANSFORM" from
*       the NDF extensions if NDFNAMES and INEXT are TRUE and
*       TRTYPE="STRUCT".
*
*     - In this application data following the third column are copied
*       without modification into the results files.

*  Behaviour of Parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets or after a break of sometime.  The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE and NDFNAMES) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line.  Global values may
*     be set and reset using the CCDSETUP and CCDCLEAR commands.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997, 1999-2001 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JUL-1992 (PDRAPER):
*        Original version.
*     19-JUL-1995 (PDRAPER):
*        Removed AIF_ calls.
*     2-AUG-1995 (PDRAPER):
*        Fixed so that correct transformation is compiled when getting
*        transform information from an external file (worked ok for NDFs).
*     3-MAR-1997 (PDRAPER):
*        Removed control of top-level locators (foreign data access
*        upgrade) and added DAT_ANNULs for LOCTR.
*     1-APR-1999 (MBT):
*        Added new TRTYPE of WCS.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     22-MAY-2001 (MBT):
*        Changed to cope with position lists which contain no data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'FIO_PAR'          ! FIO parameterisations
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'GRP_PAR'          ! GRP stysem constants
      INCLUDE 'PAR_ERR'          ! PAR system error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER CHR_LEN            ! True length of string
      EXTERNAL CHR_LEN
      LOGICAL FIO_TEST           ! Tests generic file access errors
      EXTERNAL FIO_TEST

*  Local Variables:
      CHARACTER * ( 1 ) INV( 2 ) ! Inverse transformation
      CHARACTER * ( 32 ) TRTYPE  ! Transformation type
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for reading data files
      CHARACTER * ( CCD1__SZTRN ) MAP( 2 ) ! Forward map expressions
      CHARACTER * ( CCD1__SZTRN - 3 ) XFOR ! X mapping expression
      CHARACTER * ( CCD1__SZTRN - 3 ) YFOR ! Y mapping expression
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! Locator to NDF extension
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator to transform
      CHARACTER * ( FIO__SZFNM ) FNAME  ! File name
      DOUBLE PRECISION TR( 6 )   ! Transformation coefficients
      INTEGER FDIN               ! FIO file descriptor
      INTEGER FDOUT              ! FIO file descriptor
      INTEGER FIOGRP             ! Group of input list names
      INTEGER I                  ! Loop/dummy variable
      INTEGER IAT                ! Position in string
      INTEGER IDIN               ! NDF identifier
      INTEGER IDTR               ! Pointer to compiled transformation
      INTEGER INDEX              ! Loop/dummy variable
      INTEGER IPDAT              ! Pointer to input data
      INTEGER IPIND              ! Pointer to input identifiers
      INTEGER IPWORK             ! Pointer to workspace for output data
      INTEGER IWCS               ! AST pointer to WCSFILE WCS component
      INTEGER IWCSF              ! NDF identifier for WCSFILE
      INTEGER JCUR               ! Index of original current frame in frameset
      INTEGER JFROM              ! Index of source frame in frameset
      INTEGER MAPAST             ! AST pointer to mapping for transformation
      INTEGER NDFGRP             ! Group of input NDF names
      INTEGER NFOR               ! Number of forward variables
      INTEGER NINV               ! Number of inverse functions =2 X+Y
      INTEGER NLGRP              ! Group of NDFs with no associated lists
      INTEGER NNOLIS             ! Number of NDFs with no associated lists
      INTEGER NOPEN              ! Number of input files
      INTEGER NREC               ! Number of input records
      INTEGER NRET               ! Number coefficients supplied
      INTEGER NSUBS              ! Number of tokens substituted
      INTEGER NTRY               ! Number of attempts to get value
      INTEGER NVAL               ! Number of values in data file record
      INTEGER OUTGRP             ! Group of output file names
      INTEGER XLEN               ! Length of XFOR expression
      INTEGER YLEN               ! Length of YFOR expression
      LOGICAL FORWRD             ! Whether to use the forward or inverse transformation in structure
      LOGICAL HAVXX              ! Flags indicating
      LOGICAL HAVXY              ! where X and Y
      LOGICAL HAVYX              ! located
      LOGICAL HAVYY              ! references are
      LOGICAL INEXT              ! Find structure in NDF extension
      LOGICAL NDFS               ! True if list names accessed via NDFs
      LOGICAL THERE              ! HDS object exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise XLEN and YLEN to avoid problems with 0 sized strings.
      XLEN = 1
      YLEN = 1

*  Locator to TRANSFORM structure is bad.
      LOCTR = DAT__NOLOC

*  Start the CCDPACK log system.
      CALL CCD1_START( 'TRANLIST', STATUS )

*  Begin AST context.
      CALL AST_BEGIN( STATUS )

*  See how the user wants to supply the positions list information. This
*  may be provided using either the named extension item CURRENT_LIST
*  within the NDFs or by directly supplying the names.
      NDFS = .TRUE.
      CALL PAR_GET0L( 'NDFNAMES', NDFS, STATUS )

*  Now get the list names. If NDFS is true then we require a group of
*  NDFs are well as the group of list names.
      CALL CCD1_GTLIG( NDFS, 'CURRENT_LIST', 'INLIST', 1, CCD1__MXLIS,
     :                 .FALSE., NOPEN, FIOGRP, NDFGRP, NNOLIS, NLGRP,
     :                 STATUS )
      CALL CCD1_GRDEL( NLGRP, STATUS )

*  If not all supplied NDFs have position lists, warn the user of
*  this fact and continue.
      IF ( NNOLIS .GT. 0 ) THEN
         CALL CCD1_MSG( ' ', '  NDFs with no associated position '//
     :                  'lists will be ignored.', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
      END IF

*  Write the names of the input lists out to the user.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Input position lists:', STATUS )
      CALL CCD1_MSG( ' ', '    ---------------------', STATUS )
      DO 6 I = 1, NOPEN
         CALL GRP_GET( FIOGRP, I, 1, FNAME, STATUS )
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL MSG_SETI( 'N', I )
         CALL CCD1_MSG( ' ', '  ^N) ^FNAME', STATUS )
 6    CONTINUE

*  Where the position list names originated.
      IF ( NDFS ) THEN
         CALL CCD1_MSG( ' ',
     :'  Position list names extracted from NDF extensions.', STATUS )

*  Write the names of the NDFs out to the user.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '    Associated NDFs:', STATUS )
         CALL CCD1_MSG( ' ', '    ----------------', STATUS )
         DO 7 I = 1, NOPEN
            CALL GRP_GET( NDFGRP, I, 1, FNAME, STATUS )
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL MSG_SETI( 'N', I )
            CALL CCD1_MSG( ' ', '  ^N) ^FNAME', STATUS )
 7       CONTINUE
      END IF

*  Get the names of the corresponding output lists. Use the input NDFs
*  as modification elements if appropriate. Otherwise use the names of
*  the position lists.
      IF  ( NDFS ) THEN
         CALL CCD1_STRGR( 'OUTLIST', NDFGRP, NOPEN, NOPEN, OUTGRP, NRET,
     :                    STATUS )
      ELSE
         CALL CCD1_STRGR( 'OUTLIST', FIOGRP, NOPEN, NOPEN, OUTGRP, NRET,
     :                    STATUS )
      END IF

*  Stop now if problems are occurring.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  How is the user going to supply the transformation data? As
*  coefficients (COEFF), as a transform formula (EXPRESSION), using
*  a transform structure, or using a WCS component.
      CALL PAR_CHOIC( 'TRTYPE', ' ', 'COEFF,EXPRES,WCS,STRUCT', .FALSE.,
     :                TRTYPE, STATUS )

*  Setting up transformation section....................................
      IF ( TRTYPE .EQ. 'COEFF' )THEN

*  Supplying transform as linear coefficients.
         NTRY = 0
 1       CONTINUE
         CALL PAR_GET1D( 'TR', 6, TR, NRET, STATUS )
         IF ( NRET .NE. 6 ) THEN
            NTRY = NTRY + 1
            IF ( NTRY .LT. 6 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'TRANLIST_NEED6',
     :         '  A linear transformation requires 6 coefficients' ,
     :         STATUS )
               CALL PAR_CANCL('TR', STATUS )
               GO TO 1
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'TRANLIST_NOTR',
     : '  Too few transformation coefficiencts given', STATUS )
               GO TO 99
            END IF
         END IF
      ELSE IF ( TRTYPE .EQ. 'EXPRES' ) THEN

*  User wants to supply a formula for transforming X and Y.
         CALL PAR_GET0C( 'XFOR', XFOR, STATUS )
         CALL PAR_GET0C( 'YFOR', YFOR, STATUS )
         CALL CCD1_GASTC( 'F', XFOR, STATUS )
         CALL CCD1_GASTC( 'F', YFOR, STATUS )
         CALL CCD1_GASTD( 'P', XFOR, STATUS )
         CALL CCD1_GASTD( 'P', YFOR, STATUS )
         XLEN = MIN( CHR_LEN( XFOR ), 1 )
         YLEN = MIN( CHR_LEN( YFOR ), 1 )

*  Look for the coordinates X and Y in both transformations.
         HAVXX = .FALSE.
         HAVXY = .FALSE.
         CALL TRN_STOK( 'X', 'X', XFOR( :XLEN ), NSUBS, STATUS )
         IF ( NSUBS .NE. 0 ) HAVXX = .TRUE.
         CALL TRN_STOK( 'Y', 'Y', XFOR( :XLEN ), NSUBS, STATUS )
         IF ( NSUBS .NE. 0 ) HAVXY = .TRUE.
         HAVYX = .FALSE.
         HAVYY = .FALSE.
         CALL TRN_STOK( 'X', 'X', YFOR( :YLEN ), NSUBS, STATUS )
         IF ( NSUBS .NE. 0 ) HAVYX = .TRUE.
         CALL TRN_STOK( 'Y', 'Y', YFOR( :YLEN ), NSUBS, STATUS )
         IF ( NSUBS .NE. 0 ) HAVYY = .TRUE.

*  Look for an X and a Y transformation reference.
         NINV = 0
         IF ( HAVXX .OR. HAVYX ) NINV = NINV + 1
         IF ( HAVXY .OR. HAVYY ) NINV = NINV + 1

*  If no references to X and Y exist do not proceed.
         IF ( NINV .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'TRANLIST_NOXORY',
     :      '  Neither transformation contains a reference to X or Y',
     :      STATUS )
            GO TO 99
         END IF

*  Add the XX= and YY= to finish the mappings.
         MAP( 1 ) = 'XX='
         IAT = 3
         CALL CHR_APPND( XFOR( :CHR_LEN( XFOR ) ), MAP( 1 ), IAT )
         MAP( 2 ) = 'YY='
         IAT = 3
         CALL CHR_APPND( YFOR( :CHR_LEN( YFOR ) ), MAP( 2 ), IAT )

*  Set up the inverse transformations.
         INV( 1 ) = 'X'
         INV( 2 ) = 'Y'

*  Create the transformation.
         CALL TRN_NEW( 2, NINV, MAP, INV, '_DOUBLE', ' ', ' ', ' ',
     :                 LOCTR, STATUS )

*  Compile it.
         CALL TRN_COMP( LOCTR, .TRUE., IDTR, STATUS )
      ELSE IF ( TRTYPE .EQ. 'STRUCT' ) THEN

*  Using a transform structure.  Does the user want us to look
*  for a standard transformations in the NDF extensions (only if
*  ndfnames are true).
         INEXT = .FALSE.
         IF ( NDFS ) CALL PAR_GET0L( 'INEXT', INEXT, STATUS )
         IF ( .NOT. INEXT ) THEN

*  User supplies a single locator to a transformation structure.
            CALL DAT_ASSOC( 'TRANSFORM', 'READ', LOCTR, STATUS )
         END IF

*  Does he want to use the forward or inverse transformation.
         CALL PAR_GET0L( 'FORWARD', FORWRD, STATUS )

*  If we have this already then compile the transformation.
         IF ( .NOT. INEXT ) THEN
            CALL TRN_COMP( LOCTR, FORWRD, IDTR, STATUS )
         END IF
      ELSE IF ( TRTYPE .EQ. 'WCS' ) THEN

*  Using a mapping between frames in a WCS component.  Does the user
*  want to use the WCS component of each NDF (only if NDFNAMES is true)
*  or to use a single NDF for all.
         INEXT = .FALSE.
         IF ( NDFS ) CALL PAR_GET0L( 'INEXT', INEXT, STATUS )
         IF ( .NOT. INEXT ) THEN

*  User supplies a single NDF in which is located the WCS component.
            CALL NDF_ASSOC( 'WCSFILE', 'READ', IWCSF, STATUS )
          END IF

      END IF

*  Report the options to the user.
      CALL CCD1_RTRAN( TRTYPE, INEXT, TR, XFOR( :XLEN ), YFOR( :YLEN ),
     :                 LOCTR, FORWRD, 'FRAMEIN', 'FRAMEOUT', IWCSF,
     :                 STATUS )

*  Stop now if problems are occurring.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*=======================================================================
*  Main loop. Open the input lists, extract the data, transform it and
*  write it out to a new list.
*=======================================================================
      DO 3 INDEX = 1, NOPEN

*  Get the name of the input list and open the file.
         CALL GRP_GET( FIOGRP, INDEX, 1, FNAME, STATUS )
         CALL CCD1_OPFIO( FNAME, 'READ', 'LIST', 0, FDIN, STATUS )

*  Report error message if open failed.
         IF ( FIO_TEST( 'OPEN error', STATUS ) ) THEN
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL ERR_REP( 'TRANLIST_PFERR',
     :      '  Failed to input file ^FNAME', STATUS )
            GO TO 98
         END IF

*  Write informational message about it.
         CALL CCD1_MSG( ' ',  ' ', STATUS )
         CALL MSG_SETC( 'CURRENT_LIST', FNAME )
         CALL CCD1_MSG( ' ', '  +++ Processing file: ^CURRENT_LIST',
     :                  STATUS )

*  Inform user how many files we've processed out of the total number.
         CALL MSG_SETI( 'CURRENT_NUM', INDEX )
         CALL MSG_SETI( 'MAX_NUM', NOPEN )
         CALL CCD1_MSG( ' ', '  (Number ^CURRENT_NUM of ^MAX_NUM)',
     :                  STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )

*  If inputs are associated with NDFs then access the NDFs.
         IF ( NDFS ) THEN
            CALL NDG_NDFAS( NDFGRP, INDEX, 'UPDATE', IDIN, STATUS )
         END IF

*  Test the input file to see how many values the first line has.
         CALL CCD1_LTEST( FDIN, LINE, CCD1__BLEN, 2, 0, NVAL, STATUS )

*  Skip most processing if there were no data lines in the list file.
         IF ( NVAL .GT. 0 ) THEN

*  If only two values are given then interpret these as the X and Y
*  positions. Otherwise assume that the file is a standard file and map
*  in the first column as identifiers then the data area.
            IF ( NVAL .EQ. 2 ) THEN
               CALL CCD1_NLMAP( FDIN, LINE, CCD1__BLEN, IPDAT, NREC,
     :                          NVAL, STATUS )

*  Generate some dummy identfiers for the output file.
               CALL CCD1_MALL( NREC, '_INTEGER', IPIND, STATUS )
               CALL CCD1_GISEQ( 1, 1, NREC, %VAL( CNF_PVAL( IPIND ) ),
     :                          STATUS )
            ELSE
               CALL CCD1_LMAP( FDIN, LINE, CCD1__BLEN, IPIND, IPDAT,
     :                         NREC, NVAL, STATUS )
            END IF

*  Transformation section...............................................
*  Get workspace for storing the transformed data.
            CALL CCD1_MALL( NREC * NVAL, '_DOUBLE', IPWORK, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 98

*  Transform the data.
            IF ( TRTYPE .EQ. 'COEFF' ) THEN

*  Linear transformation from supplied coefficients.
               CALL CCD1_LXYT( %VAL( CNF_PVAL( IPDAT ) ),
     :                         NREC, NREC, NVAL, TR,
     :                         %VAL( CNF_PVAL( IPWORK ) ), STATUS )
            ELSE IF ( TRTYPE .EQ. 'EXPRES' .OR. TRTYPE .EQ. 'STRUCT' )
     :      THEN

*  Using a tranformation structure. If these are stored in the NDF
*  extensions then we may still need to access them.
               IF ( INEXT .AND. STATUS .EQ. SAI__OK ) THEN

*  Get a CCDPACK extension.
                  CALL CCD1_CEXT( IDIN, .FALSE., 'UPDATE', LOCEXT,
     :                            STATUS )

*  Now Look for a transformation structure.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL DAT_THERE( LOCEXT, 'TRANSFORM', THERE,
     :                               STATUS )

*  If have one get a locator to it.
                     IF ( THERE ) THEN
                        CALL DAT_FIND( LOCEXT, 'TRANSFORM', LOCTR,
     :                                 STATUS )

*  And compile it.
                        CALL TRN_COMP( LOCTR, FORWRD, IDTR, STATUS )
                        CALL DAT_ANNUL( LOCEXT, STATUS )
                     ELSE

*  Failed to find component TRANSFORM.
                        CALL DAT_ANNUL( LOCEXT, STATUS )
                        STATUS = SAI__ERROR
                        CALL NDF_MSG( 'NDF', IDIN )
                        CALL ERR_REP( 'TRANLIST_NOEXT', '  NDF ^NDF '//
     : 'does not have a TRANSFORM component in its CCDPACK extension',
     :                                STATUS )
                        GO TO 99
                     END IF
                  ELSE

*  No extension. Set status and abort.
                      STATUS = SAI__ERROR
                      CALL NDF_MSG( 'NDFNAME', IDIN )
                      CALL ERR_REP( 'CCDTRAN_NOEXT',
     :'  NDF ^NDFNAME does not have a CCDPACK extension', STATUS )
                      GO TO 99
                  END IF
               END IF

*  Find out how many transformation variables there are. We need 2 for
*  our purposes.
               CALL TRN_GTNV( LOCTR, NINV, NFOR, STATUS )
               IF (         FORWRD .AND. NINV .NE. 2  .OR.
     :              ( .NOT. FORWRD .AND. NFOR .NE. 2 ) ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'TRANLIST_BADDIR',
     :         '  Transformation does not have the required number '//
     :         'variables (2)', STATUS )
                  GO TO 99
               END IF

*  Finally transform the data.
               CALL TRN_TRND( .FALSE., NREC, 2, NREC,
     :                        %VAL( CNF_PVAL( IPDAT ) ),
     :                        IDTR, NREC, 2, %VAL( CNF_PVAL( IPWORK ) ),
     :                        STATUS )

*  Copy any further data into the result array.
               IF ( NVAL .GT. 2 ) THEN
                  DO 2 I = 3, NVAL
                     CALL CCD1_LCC( %VAL( CNF_PVAL( IPDAT ) ),
     :                              NREC, NVAL, I, I,
     :                              %VAL( CNF_PVAL( IPWORK ) ), STATUS )
 2                CONTINUE
               END IF
               IF ( INEXT ) CALL DAT_ANNUL( LOCTR, STATUS )
            ELSE IF ( TRTYPE .EQ. 'WCS' ) THEN

*  Using WCS component of one or all NDFs for the mapping.
*  Generate the mapping from the NDF identifier.  If INEXT is set we
*  need to do this every time, otherwise just first time round the loop.
               IF ( INEXT .OR. INDEX .EQ. 1 ) THEN

*  Get the WCS component.
                  IF ( INEXT ) THEN
                     CALL CCD1_GTWCS( IDIN, IWCS, STATUS )
                  ELSE
                     CALL CCD1_GTWCS( IWCSF, IWCS, STATUS )
                  END IF

*  Save the index of the original Current frame.
                  JCUR = AST_GETI( IWCS, 'Current', STATUS )

*  Get the index of the source frame.
                  CALL KPG1_ASFRM( 'FRAMEIN', 'EPOCHIN', IWCS, ' ', ' ',
     :                             .FALSE., ' ', STATUS )
                  JFROM = AST_GETI( IWCS, 'Current', STATUS )

*  Set the WCS frameset Current frame to the destination frame (if
*  parameter is null then it has the right value already).
                  IF ( STATUS .NE. SAI__OK ) GO TO 99
                  CALL PAR_GET0C( 'FRAMEOUT', LINE, STATUS )
                  IF ( STATUS .EQ. PAR__NULL ) THEN
                     CALL ERR_ANNUL( STATUS )
                     CALL AST_SETI( IWCS, 'Current', JCUR, STATUS )
                  ELSE
                     CALL KPG1_ASFRM( 'FRAMEOUT', 'EPOCHOUT', IWCS, ' ',
     :                                ' ', .FALSE., STATUS )
                  END IF

*  Generate the mapping to use.
                  MAPAST = AST_GETMAPPING( IWCS, JFROM, AST__CURRENT,
     :                                     STATUS )
               END IF

*  Transform the coordinates with the given mapping.
               CALL AST_TRANN( MAPAST, NREC, 2, NREC,
     :                         %VAL( CNF_PVAL( IPDAT ) ),
     :                         .TRUE., 2, NREC,
     :                         %VAL( CNF_PVAL( IPWORK ) ), STATUS )

*  Dispose of one-use mappings.
               IF ( INEXT ) CALL AST_ANNUL( MAPAST, STATUS )

*  Copy any further data into the result array.
               IF ( NVAL .GT. 2 ) THEN
                  DO 4 I = 3, NVAL
                     CALL CCD1_LCC( %VAL( CNF_PVAL( IPDAT ) ),
     :                              NREC, NVAL, I, I,
     :                              %VAL( CNF_PVAL( IPWORK ) ), STATUS )
 4                CONTINUE
               END IF
            END IF

*  No data in file, so no transformed points.
         ELSE
            NREC = 0
         END IF

*  Open a results file.
         CALL GRP_GET( OUTGRP, INDEX, 1, FNAME, STATUS )
         CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FDOUT, STATUS )

*  Report error message if open failed.
         IF ( FIO_TEST( 'OPEN error', STATUS ) ) THEN
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL ERR_REP( 'TRANLIST_PFERR',
     : '  Failed to open results file ^FNAME', STATUS )
         END IF

*  Write the results
         CALL CCD1_FIOHD( FDOUT, 'Output from TRANLIST', STATUS )
         CALL CCD1_WLIS( FDOUT, %VAL( CNF_PVAL( IPIND ) ),
     :                   %VAL( CNF_PVAL( IPWORK ) ), NREC,
     :                   NVAL, NREC, LINE, CCD1__BLEN, STATUS )

*  Tell the user the name of the output file
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL CCD1_MSG( ' ', '  Results written to file: ^FNAME',
     :                  STATUS )

*  If the names of the positions lists were accessed using NDF extension
*  information then update the extension.
         IF ( NDFS ) THEN
            CALL CCG1_STO0C( IDIN, 'CURRENT_LIST', FNAME, STATUS )

*  And now close this NDF.
            CALL NDF_ANNUL( IDIN, STATUS )
         END IF

 98      CONTINUE

*  Close all things open up this loop.
         CALL FIO_CLOSE( FDIN, STATUS )
         CALL FIO_CLOSE( FDOUT, STATUS )

*  Free memory.
         CALL CCD1_MFREE( IPDAT, STATUS )
         CALL CCD1_MFREE( IPIND, STATUS )
         CALL CCD1_MFREE( IPWORK, STATUS )

*  Trap bad STATUS's and exit.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
 3    CONTINUE

*  Write a list of the output file names if they have not already been
*  written to NDF extensions.
      IF ( .NOT. NDFS .AND. STATUS .EQ. SAI__OK ) THEN
         CALL CCD1_LNAM( 'NAMELIST', 1, NOPEN,
     :   '# TRANLIST - output position lists', OUTGRP, GRP__NOID,
     :                   .TRUE., STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ', '  No namelist written ', STATUS )
         END IF
      END IF

*  Error label - clean up after this.
 99   CONTINUE

*  Release group resources.
      CALL CCD1_GRDEL( FIOGRP, STATUS )
      CALL CCD1_GRDEL( NDFGRP, STATUS )

*  Free any memory still accessed.
      CALL CCD1_MFREE( -1, STATUS )

*  Close TRANSFORM.
      IF( TRTYPE .EQ. 'STRUCT' .OR. TRTYPE .EQ. 'EXPRES' ) THEN
         IF ( LOCTR .NE. DAT__NOLOC ) CALL DAT_ANNUL( LOCTR, STATUS )
         CALL TRN_CLOSE( STATUS )
      END IF

*  End AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'TRANLIST_ERR',
     :   'TRANLIST: Error transforming position list.',
     :   STATUS )
      END IF

*  Close the log system.
      CALL CCD1_END( STATUS )

      END
* $Id$
