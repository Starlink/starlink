      SUBROUTINE OUTSET( STATUS )
*+
*  Name:
*     OUTSET

*  Purpose:
*     Mask pixels inside or outside a specified circle in a
*     two-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL OUTSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine assigns a specified value (which may be "bad") to
*     either the outside or inside of a specified circle within a
*     specified component of a given two-dimensional NDF.

*  Usage:
*     outset in out centre diam

*  ADAM Parameters:
*     CENTRE = LITERAL (Read)
*        The co-ordinates of the centre of the circle. The position must
*        be given in the current co-ordinate Frame of the NDF (supplying
*        a colon ":" will display details of the current co-ordinate
*        Frame). The position should be supplied as a list of formatted
*        axis values separated by spaces or commas. See also parameter
*        USEAXIS. The current co-ordinate Frame can be changed using
*        KAPPA:WCSFRAME.
*     COMP = LITERAL (Read)
*        The NDF array component to be masked.  It may be "Data", or
*        "Variance", or "Error" (where "Error" is equivalent to
*        "Variance"). ["Data"]
*     CONST = LITERAL (Given)
*        The constant numerical value to assign to the masked pixels, or
*         the string "bad". ["bad"]
*     DIAM = LITERAL (Read)
*        The diameter of the circle. If the current co-ordinate Frame of
*        the NDF is a SKY Frame (e.g. RA and DEC), then the value should be
*        supplied as an increment of celestial latitude (e.g. DEC). Thus,
*        "10.2" means 10.2 arc-seconds, "30:0" would mean 30 arc-minutes,
*        and "1:0:0" would mean 1 degree. If the current co-ordinate
*        Frame is not a SKY Frame, then the diameter should be specified
*        as an increment along axis 1 of the current co-ordinate Frame.
*        Thus, if the current Frame is PIXEL, the value should be given
*        simply as a number of pixels.
*     IN = NDF (Read)
*        The name of the source NDF.
*     INSIDE = _LOGICAL (Read)
*        If a TRUE value is supplied, the constant value is assigned to the
*        inside of the circle. Otherwise, it is assigned to the outside. [FALSE]
*     OUT = NDF (Write)
*        The name of the masked NDF.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the current co-ordinate Frame of
*        the NDF has more than two axes. A group of strings should be
*        supplied designating the axes that are to be used when
*        specifying the circle via Parameters CENTRE and DIAM.  Each axis
*        can be specified using one of the following options.
*
*        - Its integer index within the current Frame of the input
*        NDF (in the range 1 to the number of axes in the current
*        Frame).
*        - Its symbol string such as "RA" or "VRAD".
*        - A generic option where "SPEC" requests the spectral axis,
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*        sky longitude and latitude axes respectively.  Only those axis
*        domains present are available as options.
*
*        A list of acceptable values is displayed if an illegal value is
*        supplied.  If a null (!) value is supplied, the axes with the
*        same indices as the two used pixel axes within the NDF are
*        used. [!]

*  Examples:
*     outset neb1 nebm "13.5,201.3" 20 const=0
*        This copies NDF "neb1" to "nebm", setting pixels to zero in the
*        DATA array if they fall outside the specified circle. Assuming the
*        current co-ordinate Frame of neb1 is PIXEL, the circle is centred
*        at pixel co-ordinates (13.5, 201.3) and has a diameter of 20 pixels.
*     outset neb1 nebm "15:23:43.2 -22:23:34.2" "10:0" inside comp=var
*        This copies NDF "neb1" to "nebm", setting pixels bad in the
*        variance array if they fall inside the specified circle. Assuming
*        the current co-ordinate Frame of neb1 is a SKY Frame describing RA
*        and DEC, the aperture is centred at RA 15:23:43.2 and
*        DEC -22:23:34.2, and has a diameter of 10 arc-minutes.

*  Related Applications:
*     KAPPA: ARDMASK, REGIONMASK.

*  Implementation Status:
*     -  This routine correctly processes the WCS, AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, and VARIANCE components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  Bad pixels and automatic quality masking are supported.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-NOV-2001 (DSB):
*        Original NDF version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2010 August 1 (MJC):
*        Fix bug: DIAM was setting a radius not a diameter.
*     2012 May 9 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constant
      INCLUDE 'GRP_PAR'          ! GRP_ data constants
      INCLUDE 'PRM_PAR'          ! VAL_ data constants
      INCLUDE 'AST_PAR'          ! AST_ data constants and functions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COMP*8           ! Name of array component to mask
      CHARACTER CONTXT*40        ! Text version of constant value
      CHARACTER TEXT*(GRP__SZNAM)! General text bufferName of ARD file
      CHARACTER TYPE*( NDF__SZTYP )  ! Numeric type for processing
      DOUBLE PRECISION BC( 2 )   ! CENTRE base Frame axis value
      DOUBLE PRECISION CC( 2 )   ! CENTRE current Frame axis value
      DOUBLE PRECISION CONST     ! The constant to assign
      DOUBLE PRECISION DIAM      ! The diameter of circular aperture
      INTEGER CURFRM             ! AST pointer to current WCS Frame
      INTEGER DAX                ! Index of axis for measuring diameter
      INTEGER EL                 ! Total number of pixels in the image
      INTEGER IAT                ! Used length of a string
      INTEGER IGRP               ! Group identifier
      INTEGER INDF1              ! Identifier for the source NDF
      INTEGER INDF2              ! Identifier for the output NDF
      INTEGER IPMASK             ! Pointer to the ARD logical mask
      INTEGER IPOUT              ! Pointer to the data component of for the output NDF
      INTEGER IWCS               ! NDF WCS FrameSet
      INTEGER LBNDE( NDF__MXDIM )! Lower bounds of a box encompassing all external array elements
      INTEGER LBNDI( NDF__MXDIM )! Lower bounds of a box encompassing all internal array elements
      INTEGER NVAL               ! Number of supplied values
      INTEGER REGVAL             ! Value assignied to the first ARD region
      INTEGER SDIM( NDF__MXDIM ) ! Indices of significant axes
      INTEGER SLBND( NDF__MXDIM )! Lower limit for input NDF
      INTEGER SUBND( NDF__MXDIM )! Upper limit for input NDF
      INTEGER UBNDE( NDF__MXDIM )! Upper bounds of a box encompassing all external array elements
      INTEGER UBNDI( NDF__MXDIM )! Upper bounds of a box encompassing all internal array elements
      LOGICAL BAD                ! Assign bad values to the region?
      LOGICAL INSIDE             ! Assign value to inside of region?
      LOGICAL THERE              ! Does the requested NDF component exist?
      REAL TRCOEF( ( NDF__MXDIM + 1 ) * NDF__MXDIM ) ! Data to world co-ordinate conversions
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF structure to be examined.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Determine which array component is to be masked, converting 'ERROR' into
*  'VARIANCE'.
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Error,Variance', .FALSE.,
     :                COMP, STATUS )
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Check that the required component exists and report an error if it
*  does not.
      CALL NDF_STATE( INDF1, COMP, THERE, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. THERE ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'COMP', COMP )
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL ERR_REP( 'OUTSET_ERR1', 'The ^COMP component is '//
     :                 'undefined in the NDF structure ^NDF', STATUS )
      END IF

*  Obtain the numeric type of the NDF array component to be masked.
      CALL NDF_TYPE( INDF1, COMP, TYPE, STATUS )

*  Get the WCS FrameSet and the bounds of the significant axes.
*  Obtains Parameter: USEAXIS.
      CALL KPG1_ASGET( INDF1, 2, .FALSE., .TRUE., .TRUE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Get a pointer to the current Frame.
      CURFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Get the CENTRE parameter value.
      CC( 1 ) = AST__BAD
      CALL KPG1_GTPOS( 'CENTRE', IWCS, .FALSE., CC, BC, STATUS )

*  Choose the axis to use, and then get the DIAM parameter value.
      IF( AST_ISASKYFRAME( CURFRM, STATUS ) ) THEN
         DAX = AST_GETI( CURFRM, 'LATAXIS', STATUS )
      ELSE
         DAX = 1
      END IF
      DIAM= AST__BAD
      CALL KPG1_GTAXV( 'DIAM', 1, .TRUE., CURFRM, DAX, DIAM, NVAL,
     :                 STATUS )

*  Create the ARD description.
      TEXT = 'CIRCLE('
      IAT = 8
      CALL CHR_APPND( AST_FORMAT( CURFRM, 1, CC( 1 ), STATUS ), TEXT,
     :                IAT )
      CALL CHR_APPND( ',', TEXT, IAT )
      IAT = IAT + 1
      CALL CHR_APPND( AST_FORMAT( CURFRM, 2, CC( 2 ), STATUS ), TEXT,
     :                IAT )
      CALL CHR_APPND( ',', TEXT, IAT )
      IAT = IAT + 1
      CALL CHR_APPND( AST_FORMAT( CURFRM, DAX, 0.5D0 * DIAM, STATUS ),
     :                TEXT, IAT )
      CALL CHR_APPND( ' )', TEXT, IAT )

*  Put this text into a GRP group.
      CALL GRP_NEW( ' ', IGRP, STATUS )
      CALL GRP_PUT( IGRP, 1, TEXT( : IAT ), 0, STATUS )

*  Store the WCS FrameSet. Since no COFRAME or WCS statements were included
*  in the ARD description above, positions will be interpreted as being in
*  the current Frame of the WCS FrameSet.
      CALL ARD_WCS( IWCS, 'PIXEL', STATUS )

*  Allocate the memory needed for the logical mask array.
      CALL NDF_SIZE( INDF1, EL, STATUS )
      CALL PSX_CALLOC( EL, '_INTEGER', IPMASK, STATUS )

*  Create the mask.  Value 2 should be used to represent pixels
*  specified by the first keyword in the ARD description. TRCOEF is
*  ignored because we have previously called ARD_WCS.
      REGVAL = 2
      CALL ARD_WORK( IGRP, 2, SLBND, SUBND, TRCOEF, .FALSE., REGVAL,
     :               %VAL( CNF_PVAL( IPMASK ) ),
     :               LBNDI, UBNDI, LBNDE, UBNDE,
     :               STATUS )

*  Propagate the bits of the source NDF required.
      CALL LPG_PROP( INDF1, 'Data,Variance,Quality,Axis,Units,WCS',
     :               'OUT', INDF2, STATUS )

*  Get the title for the output NDF.
      CALL NDF_CINP( 'TITLE', INDF2, 'Title', STATUS )

*  Map the output NDF array for updating.
      CALL NDF_MAP( INDF2, COMP, TYPE, 'UPDATE', IPOUT, EL,
     :              STATUS )

*  Get the string representing the constant value to assign.
      CALL PAR_MIX0D( 'CONST', 'Bad', VAL__MIND, VAL__MAXD, 'Bad',
     :                 .FALSE., CONTXT, STATUS )

*  Get the appropriate numerical value from the string.
      BAD = ( CONTXT .EQ. 'BAD' )
      IF( .NOT. BAD ) CALL CHR_CTOD( CONTXT, CONST, STATUS )

*  See if the value is to be assigned to the inside or the outside of the
*  region.
      CALL PAR_GET0L( 'INSIDE', INSIDE, STATUS )

*  Correct the output image to have bad pixels where indicated on the
*  mask.  Call the appropriate routine for the data type.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL KPS1_ARDMR( BAD, CONST, INSIDE, EL,
     :                    %VAL( CNF_PVAL( IPMASK ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )

      ELSE IF( TYPE .EQ. '_BYTE' ) THEN
         CALL KPS1_ARDMB( BAD, CONST, INSIDE, EL,
     :                    %VAL( CNF_PVAL( IPMASK ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_ARDMD( BAD, CONST, INSIDE, EL,
     :                    %VAL( CNF_PVAL( IPMASK ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPS1_ARDMI( BAD, CONST, INSIDE, EL,
     :                    %VAL( CNF_PVAL( IPMASK ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )

      ELSE IF( TYPE .EQ. '_INT64' ) THEN
         CALL KPS1_ARDMK( BAD, CONST, INSIDE, EL,
     :                    %VAL( CNF_PVAL( IPMASK ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )

      ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPS1_ARDMUB( BAD, CONST, INSIDE, EL,
     :                     %VAL( CNF_PVAL( IPMASK ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )

      ELSE IF( TYPE .EQ. '_UWORD' ) THEN
         CALL KPS1_ARDMUW( BAD, CONST, INSIDE, EL,
     :                     %VAL( CNF_PVAL( IPMASK ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )

      ELSE IF( TYPE .EQ. '_WORD' ) THEN
         CALL KPS1_ARDMW( BAD, CONST, INSIDE, EL,
     :                    %VAL( CNF_PVAL( IPMASK ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )

      END IF

*  Set the bad-pixel flag.
      IF( BAD ) CALL NDF_SBAD( .TRUE., INDF2, COMP, STATUS )

*  Free the dynamic array space of the logical mask.
      CALL PSX_FREE( IPMASK, STATUS )

*  Close down the group used to hold the pixel mask.
      CALL GRP_DELET( IGRP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'OUTSET_ERR', 'OUTSET: Failed to apply a '//
     :                 'circular mask to a two-dimensional NDF.',
     :                 STATUS )
      END IF

      END
