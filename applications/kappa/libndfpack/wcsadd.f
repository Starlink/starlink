      SUBROUTINE WCSADD( STATUS )
*+
*  Name:
*     WCSADD

*  Purpose:
*     Creates a Mapping and optionally adds a new co-ordinate Frame into
*     the WCS component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSADD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application can be used to create a new AST Mapping and
*     optionally use the Mapping to add a new co-ordinate Frame into
*     the WCS component of an NDF (see Parameter NDF). An output text
*     file may also be created holding a textual representation of the
*     Mapping for future use by other applications such as REGRID (see
*     Parameter MAPOUT). A number of different types of Mapping can be
*     used (see Parameter MAPTYPE).
*
*     When adding a new Frame to a WCS component, the Mapping is used
*     to connect the new Frame to an existing one (called the "basis"
*     Frame: see Parameter FRAME). The specific type of Frame to add is
*     specified using Parameter FRMTYPE (the default is to simply copy
*     the basis Frame). Optionally (see Parameter TRANSFER), attributes
*     which have been assigned an explicit value in the basis Frame are
*     transferred to the new Frame (but only if they are relevant to the
*     type of the new Frame). The value of the Domain attribute for the
*     new Frame can be specified using Parameter DOMAIN. Other attribute
*     values for the new Frame may be specified using Parameter ATTRS.
*     The new Frame becomes the current co-ordinate Frame in the NDF
*     (unless Parameter RETAIN is set TRUE).
*
*     WCSADD will only generate Mappings with the same number of
*     input and output axes; this number is determined by the number
*     of axes in the basis Frame if an NDF is supplied, or by the
*     NAXES parameter otherwise.

*  Usage:
*     wcsadd ndf frame domain maptype

*  ADAM Parameters:
*     ATTRS = GROUP (Read)
*        A group of attribute settings to be applied to the new Frame
*        before adding it into the NDF.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text
*        file preceded by an up-arrow character "^". Such text files
*        should contain further comma-separated lists which will be read
*        and interpreted in the same manner. Attribute settings are
*        applied in the order in which they occur within the list, with
*        later settings over-riding any earlier settings given for the
*        same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of an attribute appropriate to the
*        type of Frame specified by Parameter FRMTYPE (see SUN/210 for
*        a complete description of all attributes), and <value> is the
*        value to assign to the attribute. Default values will be used
*        for any unspecified attributes---these defaults are inherited
*        from the basis Frame.  Any unrecognised attributes are ignored
*        (no error is reported).
*     CENTRE( 2 ) = _DOUBLE (Read)
*        The co-ordinates of the centre of a pincushion distortion.
*        It is only used when MAPTYPE="PINCUSHION". See also DISCO.
*        [0,0]
*     DIAG( ) = _DOUBLE (Read)
*        The elements along the diagonal of the linear transformation
*        matrix. There will be as many of these as there are axes in the
*        basis Frame. Each effectively gives the factor by which
*        co-ordinates on the corresponding axis should be multiplied.
*        This parameter is only used when MAPTYPE="DIAG".
*     DISCO = _DOUBLE (Read)
*        The distortion coefficient of a pincushion distortion. Used
*        in conjunction with the CENTRE parameter, this defines the
*        forward transformation to be used as follows:
*
*                XX = X + D * (X - C1) * ( (X - C1)**2 + (Y - C2)**2 )
*
*                YY = Y + D * (Y - C2) * ( (X - C1)**2 + (Y - C2)**2 )
*
*        where (X,Y) are the input co-ordinates, (XX,YY) the output
*        co-ordinates, D is DISCO, and C1 and C2 are the two elements of
*        CENTRE.  DISCO is only used when MAPTYPE=PINCUSHION.
*     DOMAIN = LITERAL (Read)
*        The value for the Domain attribute for the new Frame. Care
*        should be taken to ensure that domain names are used
*        consistently. This will usually mean avoiding any domain names
*        that are already in use within the WCS component, particularly
*        the standard domain names such as GRID, FRACTION, PIXEL, AXIS,
*        and GRAPHICS. The supplied value is stripped of spaces, and
*        converted to upper case before being used.
*
*        Note, if Parameter MAPTYPE is set to "REFNDF", then the value
*        supplied for Parameter DOMAIN indicates the Domain of the
*        Frame within the reference NDF that is to be copied (see
*        Parameter REFNDF).
*     EPOCH = _DOUBLE (Read)
*        If the basis Frame is specified using a "Sky Co-ordinate
*        System" specification for a celestial co-ordinate system (see
*        Parameter FRAME), then an epoch value is needed to qualify it.
*        This is the epoch at which the supplied sky positions were
*        determined. It should be given as a decimal-years value, with
*        or without decimal places ("1996.8" for example). Such values
*        are interpreted as a Besselian epoch if less than 1984.0 and
*        as a Julian epoch otherwise. The suggested default is the
*        value stored in the basis Frame.
*     FOREXP = LITERAL (Read)
*        A group of expressions to be used for the forward co-ordinate
*        transformations in a MathMap. There must be at least as many
*        expressions as the number of axes of the Mapping, but there
*        may be more if intermediate expressions are to be used. The
*        expressions may be given directly in response to the prompt, or
*        read from a text file, in which case the name of the file
*        should be given, preceded by a "^" character. Individual
*        expression should be separated by commas or, if they are
*        supplied in a file, new-lines (see SUN/95 section "Specifying
*        Groups of Objects" which is within the section "Parameters").
*        The syntax for each expression is Fortran-like; see the
*        "Examples" section below, and the Appendix entitled "Using
*        MathMaps" in SUN/95 for details.  FOREXP is only used when
*        MAPTYPE="MATH".
*     FRAME = LITERAL (Read)
*        A string specifying the basis Frame. If a null value is
*        supplied the current co-ordinate Frame in the NDF is used. The
*        string can be one of the following:
*
*        - A domain name such as SKY, AXIS, PIXEL, etc. The two
*        "pseudo-domains" WORLD and DATA may be supplied and will be
*        translated into PIXEL and AXIS respectively, so long as the WCS
*        component of the NDF does not contain Frames with these
*        domains.
*
*        - An integer value giving the index of the required Frame
*        within the WCS component.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000)
*        (see section "Sky Co-ordinate Systems" in SUN/95).
*
*     FRMTYPE = LITERAL (Read)
*        The type of Frame to add to the NDF. If a null (!) value is
*        supplied, a copy of the basis Frame is used (as modified by
*        Parameters ATTRS and DOMAIN). The allows values are:
*
*        - "FRAME"     -- A simple Cartesian Frame (the number of axes is
*                         equal to the number of outputs from the
*                         Mapping)
*
*        - "SKYFRAME"  -- A two-dimensional Frame representing positions
*                         on the celestial sphere.
*
*        - "SPECFRAME" -- A one-dimensional Frame representing positions
*                         within an electromagnetic spectrum.
*
*        - "TIMEFRAME" -- A one-dimensional Frame representing moments
*                         in time.
*
*        Note, if Parameter MAPTYPE is set to "REFNDF", then Parameter
*        "FRMTYPE" will not be used---the Frame used will instead always
*        be a copy of the Frame from the reference NDF (as selected by
*        Parameter DOMAIN). [!]
*     INVEXP = LITERAL (Read)
*        The expressions to be used for the inverse co-ordinate
*        transformations in a MathMap. See FOREXP.  INVEXP is only used
*        when MAPTYPE="MATH".
*     MAPIN = FILENAME (Read)
*        The  name of a file containing an AST Mapping with which to
*        connect the basis Frame to the new one. The file may be a text
*        file which contains the textual representation of an AST
*        Mapping, or a FITS file which contains the Mapping as an AST
*        object encoded in its headers, or an NDF. If it is an NDF, the
*        Mapping from its base (GRID-domain) to current Frame will be
*        used. Only used when MAPTYPE="FILE".
*     MAPOUT = FILENAME (Write)
*        The name of a text file in which to store a textual
*        representation of the Mapping. This can be used, for instance,
*        by the REGRID application. If a null (!) value is supplied, no
*        file is created. [!]
*     MAPTYPE = LITERAL (Read)
*        The type of Mapping to be used to connect the new Frame to the
*        basis Frame. It must be one of the following strings, each
*        of which require some additional parameters as indicated:
*
*        - "DIAGONAL" -- A linear mapping with no translation
*                        of off-diagonal coefficients (see Parameter
*                        DIAG)
*
*        - "FILE"     -- A mapping defined by an AST Mapping supplied
*                        in a separate file (see Parameter MAPIN)
*
*        - "LINEAR"   -- A general linear mapping (see Parameter TR)
*
*        - "MATH"     -- A general algebraically defined mapping
*                        (see Parameters FOREXP, INVEXP, SIMPFI, SIMPIF)
*
*        - "PINCUSHION" -- A pincushion/barrel distortion (see Parameters
*                        DISCO and CENTRE)
*
*        - "REFNDF"   -- The Mapping is obtained by aligning the NDF
*                        with a second reference NDF (see Parameters
*                        REFNDF)
*
*        - "SHIFT"    -- A translation (see Parameter SHIFT)
*
*        - "UNIT"     -- A unit mapping
*
*        - "ZOOM"     -- A uniform expansion/contraction (see Parameter
*                        ZOOM)
*
*        ["LINEAR"]
*     NAXES = _INTEGER (Read)
*        The number of input and output axes which the Mapping will
*        have. Only used if a null value is supplied for Parameter NDF.
*     NDF = NDF (Read and Write)
*        The NDF in which to store a new co-ordinate Frame. Supply a
*        null (!) value if you do not wish to add a Frame to an NDF (you
*        can still use the MAPOUT parameter to write the Mapping to a
*        text file).
*     REFNDF = NDF (Read)
*        A reference NDF from which to obtain the Mapping and Frame. The
*        NDFs specified by Parameters NDF and REFNDF are aligned in a
*        suitable coordinate system (usually their current Frames - an
*        error is reported if the two NDFs cannot be aligned). The Mapping
*        from the basis Frame in "NDF" (specified by Parameter FRAME) to
*        the required Frame in "REFNDF" (specified by Parameter DOMAIN) is
*        then found and used. The Frame added into "NDF" is always a copy
*        of the reference Frame - regardless of the setting of Parameter
*        FRMTYPE. Parameter REFNDF is only used when Parameter MAPTYPE is
*        set to "REFNDF", in which case a value must also be supplied for
*        Parameter NDF (an error will be reported otherwise).
*     RETAIN = _LOGICAL (Read)
*        Indicates whether the original current Frame should be retained
*        within the WCS FrameSet of the modified NDF (see Parameter NDF).
*        If FALSE, the newly added Frame is the current Frame on exit.
*        Otherwise, the original current Frame is retained on exit. [FALSE]
*     SHIFT( ) = _DOUBLE (Read)
*        A vector giving the displacement represented by the
*        translation. There must be one element for each axis.  SHIFT
*        is only used when MAPTYPE="SHIFT".
*     SIMPFI = _LOGICAL (Read)
*        The value of the Mapping's SimpFI attribute (whether it is
*        legitimate to simplify the forward followed by the inverse
*        transformation to a unit transformation). This parameter is
*        only used when MAPTYPE="MATH".  [TRUE]
*     SIMPIF = _LOGICAL (Read)
*        The value of the Mapping's SimpIF attribute (whether it is
*        legitimate to simplify the inverse followed by the forward
*        transformation to a unit transformation). This parameter is
*        only used when MAPTYPE="MATH".  [TRUE]
*     TR( ) = _DOUBLE (Read)
*        The values of this parameter are the coefficients of a linear
*        transformation from the basis Frame specified by Parameter
*        FRAME to the new Frame. This parameter is only used when
*        MAPTYPE="LINEAR". For instance, if a feature has co-ordinates
*        (X,Y,Z,...) in the basis Frame, and co-ordinates (U,V,W,...) in
*         the new Frame, then the following transformations would be
*        used, depending on  how many axes the two Frames have:
*
*        - one-dimensional:
*
*              U = TR(1) + TR(2)*X
*
*        - two-dimensional:
*
*              U = TR(1) + TR(2)*X + TR(3)*Y
*
*              V = TR(4) + TR(5)*X + TR(6)*Y
*
*        - three-dimensional:
*
*              U = TR(1) + TR(2)*X + TR(3)*Y + TR(4)*Z
*
*              V = TR(5) + TR(6)*X + TR(7)*Y + TR(8)*Z
*
*              W = TR(9) + TR(10)*X + TR(11)*Y + TR(12)*Z
*
*        The correct number of values must be supplied (that is, N*(N+1)
*        where N is the number of axes in the new and old Frames). If a
*        null value (!) is given it is assumed that the new Frame and
*        the basis Frame are connected using a unit mapping (i.e.
*        corresponding axis values are identical in the two Frames).
*        This parameter is only used when MAPTYPE="LINEAR". [!]
*     TRANSFER = _LOGICAL (Read)
*        If TRUE, attributes which have explicitly set values in the basis
*        Frame (specified by Parameter FRAME) are transferred to the new
*        Frame (specified by Parameter FRMTYPE), if they are applicable
*        to the new Frame. If FALSE, no attribute values are transferred.
*        The dynamic default is TRUE if and only if the two Frames are
*        of the same class and have the same value for their Domain
*        attributes. []
*     ZOOM = _DOUBLE (Read)
*        The scaling factor for a ZoomMap; every coordinate will be
*        multiplied by this factor in the forward transformation.
*        ZOOM is only used when MAPTYPE="ZOOM".

*  Examples:
*     wcsadd speca axis frmtype=specframe maptype=unit \
*            attrs="'system=wave,unit=Angstrom'"
*        This example assumes the NDF "speca" has an Axis structure
*        describing wavelength in Angstroms. It adds a corresponding
*        SpecFrame into the WCS component of the NDF. The SpecFrame
*        is connected to the Frame describing the NDF Axis structure
*        using a unit Mapping. Subsequently, WCSATTRIB  can be used to
*        modify the SpecFrame so that it describes the spectral axis
*        value in some other system (frequency, velocities of various
*        forms, energy, wave number, etc).
*     wcsadd ngc5128 pixel old_pixel unit
*        This adds a new co-ordinate Frame into the WCS component of the
*        NDF called ngc5128. The new Frame is given the domain OLD_PIXEL
*        and is a copy of the existing PIXEL Frame. This OLD_PIXEL
*        Frame will be retained through further processing and can be
*        used as a record of the original pixel co-ordinate Frame.
*     wcsadd my_data dist-lum dist(au)-lum linear tr=[0,2.0628E5,0,0,0,1]
*        This adds a new co-ordinate Frame into the WCS component of the
*        NDF called my_data. The new Frame is given the domain
*        DIST(AU)-LUM and is a copy of an existing Frame with domain
*        DIST-LUM. The first axis in the new Frame is derived from the
*        first axis in the basis Frame but is in different units (AU
*        instead of parsecs). This change of units is achieved by
*        multiplying the old Frame axis 1 values by 2.0628E5. The values
*        on the second axis are copied without change. You could then
*        use application WCSATTRIB to set the "Unit" attribute for axis
*        1 of the new Frame to "AU".
*     wcsadd my_data dist-lum dist(au)-lum diag diag=[2.0628E5,1]
*        This does exactly the same as the previous example.
*     wcsadd ax322 ! shrunk zoom zoom=0.25 mapout=zoom.ast
*        This adds a new Frame to the WCS component of ax322 which is a
*        one-quarter-scale copy of its current co-ordinate Frame. The
*        Mapping is also stored in the text file "zoom.ast".
*     wcsadd cube grid slid shift shift=[0,0,1024]
*        This adds a new Frame to the WCS component of the NDF cube
*        which matches the GRID-domain co-ordinates in the first two
*        axes, but is translated by 1024 pixels on the third axis.
*     wcsadd plane pixel polar math simpif simpfi
*           forexp="'r=sqrt(x*x+y*y),theta=atan2(y,x)'"
*           invexp="'x=r*cos(theta),y=r*sin(theta)'"
*        A new Frame is added which gives pixel positions in polar
*        co-ordinates. Fortran-like expressions are supplied which
*        define both the forward and inverse transformations of the
*        Mapping. The symbols "x" and "y" are used to represent the two
*        input Cartesian pixel co-ordinate axes, and the symbols "r" and
*        "theta" are used to represent the output polar co-ordinates.
*        Note, the single quotes are needed when running from the Unix
*        shell in order to prevent the shell interpreting the
*        parentheses and commas within the expressions.
*     wcsadd plane pixel polar math simpif simpfi forexp=^ft invexp=^it
*        As above, but the expressions defining the transformations are
*        supplied in two text files called "ft" and "it", instead of
*        being supplied directly. Each file could contain the two
*        expression on two separate lines.
*     wcsadd ndf=! naxes=2 mapout=pcd.ast maptype=pincushion
*           disco=5.3e-10
*        This constructs a pincushion-type distortion Mapping centred
*        on the origin with a distortion coefficient of 5.3e-10,
*        and writes out the Mapping as a text file called pcd.ast.
*        This file could then be used by REGRID to resample
*        the pixels of an NDF according to this transformation.
*        No NDF is accessed.
*     wcsadd qmosaic frame=grid domain=polanal maptype=refndf refndf=imosaic
*        This adds a new co-ordinate Frame into the WCS component of the
*        NDF called qmosaic. The new Frame has domain "POLANAL" and is
*        copied from the NDF called imosaic (an error is reported if
*        there is no such Frame with imosaic). The new co-ordinate Frame
*        is attached to the base Frame (i.e. GRID co-ordinates) within
*        qmosaic using a Mapping that produces alignment between qmosaic
*        and imosaic.

*  Notes:
*     -  The new Frame has the same number of axes as the basis Frame.
*     -  An error is reported if the transformation supplied using
*     Parameter TR is singular.

*  Related Applications:
*     KAPPA: NDFTRACE, REGRID, WCSFRAME, WCSREMOVE, WCSATTRIB;
*     CCDPACK: WCSEDIT.

*  Copyright:
*     Copyright (C) 1998-1999, 2001-2003 Central Laboratory of the
*     Research Councils. Copyright (C) 2005-2006 Particle Physics &
*     Astronomy Research Council. All Rights Reserved.

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
*     MBT: Mark Taylor (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1998 (DSB):
*        Original version.
*     25-AUG-1999 (DSB):
*        Add TOKEN arg in call to KPG1_ASFRM
*     14-DEC-2001 (MBT):
*        Added MAPTYPE, MAPOUT and other parameters and several new
*        Mapping types.
*     8-JAN-2002 (DSB):
*        Minor prologue changes. Change some parameter logic. Use GRP to
*        get the MATHMAP expressions.
*     9-JAN-2003 (DSB):
*        Added support for SpecFrames (parameters FRMTYPE and ATTRS).
*     4-MAR-2005 (DSB):
*        Moved assignment of ATTR attributes to before the point where
*        attributes are transferred from the basis Frame to the new
*        Frame. This is so that ATTR can be used to describe the
*        existing Frame (as in the case where a SpecFrame is being
*        created on the basis of an AXIS Frame). As it was, the existing
*        AXIS Frame was always described by default SpecFrame, with
*        default attributes.
*     2006 March 3 (MJC):
*        Created tokens for an error message.
*     2006 April 12 (MJC):
*        Remove unused variables and wrapped long lines.
*     30-MAY-2006 (DSB)
*        Changed ATL1 prefix to ATL.
*     29-JUN-2006 (DSB)
*        Added support for TimeFrames.
*     15-NOV-2012 (DSB):
*        Added parameter TRANSFER.
*     13-JAN-2016 (DSB):
*        - Added MAPTYPE=REFNDF option.
*        - Added parameter RETAIN.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      EXTERNAL AST_ISAMAPPING    ! AST function to classify Object type

*  Local Constants:
      INTEGER MAXEXP             ! Maximum number of expressions for
      PARAMETER( MAXEXP = 100 )  ! MathMap

*  Local Variables:
      CHARACTER ATTRIB*20        ! Attribute name
      CHARACTER DOM*40           ! Domain for new Frame
      CHARACTER DOM0*40          ! Domain for basis Frame
      CHARACTER FRMTYP*16        ! Type of Frame to add
      CHARACTER MAPTYP*16        ! Type of transformation to add
      CHARACTER FOREXP( MAXEXP ) * ( GRP__SZNAM ) ! Forward expressions
                                 ! for MathMap
      CHARACTER INVEXP( MAXEXP ) * ( GRP__SZNAM ) ! Inverse expressions
                                 ! for MathMap
      DOUBLE PRECISION CENTRE( 2 ) ! Pincushion distortion centre
      DOUBLE PRECISION DET       ! Matrix determinant
      DOUBLE PRECISION DIAG( NDF__MXDIM ) ! Diagonal matrix elements
      DOUBLE PRECISION DISCO     ! Pincushion distortion coefficient
      DOUBLE PRECISION INA( NDF__MXDIM ) ! Corner "A" of window in input
                                 ! Frame
      DOUBLE PRECISION INB( NDF__MXDIM ) ! Corner "B" of window in input
                                 ! Frame
      DOUBLE PRECISION MATRIX( NDF__MXDIM*NDF__MXDIM ) ! Pure matrix
                                 ! (no offset)
      DOUBLE PRECISION MTEST( NDF__MXDIM*NDF__MXDIM ) ! Pure matrix
                                 ! (no offset)
      DOUBLE PRECISION OFFSET( NDF__MXDIM ) ! Pixel offset vector
      DOUBLE PRECISION OTEST( NDF__MXDIM )  ! Pixel offset vector
      DOUBLE PRECISION OUTA( NDF__MXDIM )! Corner "A" of window in
                                 ! output Frame
      DOUBLE PRECISION OUTB( NDF__MXDIM )! Corner "B" of window in
                                 ! output Frame
      DOUBLE PRECISION SHIFT( NDF__MXDIM ) ! Translation coefficients
      DOUBLE PRECISION TR( NDF__MXDIM*( NDF__MXDIM + 1 ) ) ! Mapping
                                 ! co-effs
      DOUBLE PRECISION ZOOM      ! Scaling factor for ZoomMap
      INTEGER ACTVAL             ! No. of transformation coefficients
                                 ! supplied
      INTEGER FRMB               ! Pointer to basis Frame
      INTEGER FRMN               ! Pointer to new Frame
      INTEGER I                  ! General loop count
      INTEGER IAT                ! Used length of a string
      INTEGER IBASIS             ! Index of basis Frame
      INTEGER ICOPY              ! Index of Frame to be copied
      INTEGER ICUR0              ! Index of original current Frame in IWCS
      INTEGER ICURR              ! Index of original current Frame in IWCSR
      INTEGER IGRP               ! GRP group for MATHMAP expresssions
      INTEGER INDF               ! NDF identifier for NDF being modified
      INTEGER INDFR              ! NDF identifier for reference NDF
      INTEGER IWCS               ! Pointer to WCS FrameSet for INDF
      INTEGER IWCS2              ! Merged WCS FrameSet
      INTEGER IWCSR              ! Copy of WCS FrameSet for INDFR
      INTEGER J                  ! Column index
      INTEGER K                  ! Index within supplied list of
                                 ! coefficients
      INTEGER L                  ! Index within vectorised matrix array
      INTEGER MAP                ! Pointer to old->new Mapping
      INTEGER MTRMAP             ! MatrixMap implied by given
                                 ! coefficients
      INTEGER NAXB               ! Number of axes in basis Frame
      INTEGER NCOEF              ! Required number of transformation
                                 ! coefficients
      INTEGER NFEXP              ! Number of expressions for forward
                                 ! transforms
      INTEGER NFRM               ! Number of Frames in original FrameSet
      INTEGER NIEXP              ! Number of expressions for inverse
                                 ! transforms
      INTEGER RESULT             ! Pointer to result FrameSet
      INTEGER SING               ! Non-zero if matrix is singular
      INTEGER WINMAP             ! WinMap implied by given coefficients
      INTEGER WORK( NDF__MXDIM ) ! Work space
      LOGICAL FIBOTH             ! Do we have both forward and inverse
                                 ! mappings?
      LOGICAL RETAIN             ! Retain the original current Frame?
      LOGICAL SIMPFI             ! SimpFI attribute of MathMap
      LOGICAL SIMPIF             ! SimpIF attribute of MathMap
      LOGICAL XFER               ! Transfer attributes from basis to new frame?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get required information from NDF or parameters.
*  ================================================

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain an identifier for the NDF to be modified.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  If a null value was supplied annull the error and indicate that
*  we have no ndf.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         INDF = NDF__NOID
         FRMB = AST__NULL

*  If we have no NDF we need to get the number of axes for the Mapping
*  from the user.
         CALL PAR_GDR0I( 'NAXES', -1, 1, VAL__MAXI, .FALSE., NAXB,
     :                   STATUS )

*  If we are adding a new Frame to a WCS FrameSet, we need to extract
*  some information about the FrameSet.
      ELSE

*  Get the WCS FrameSet associated with the NDF.
         CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Record the index of the original current Frame.
         ICUR0 = AST_GETI( IWCS, 'Current', STATUS )

*  Get the existing Frame which is to be used as the basis for the new
*  Frame.   The selected Frame becomes the Current Frame.
         CALL NDF_MSG( 'NDF', INDF )
         CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IWCS, 'PIXEL', 'AXIS',
     :                    .TRUE., '^NDF', STATUS )

*  Get its index, get a pointer to it, and save the number of axes in
*  it.
         IBASIS = AST_GETI( IWCS, 'CURRENT', STATUS )
         FRMB = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
         NAXB = AST_GETI( FRMB, 'NAXES', STATUS )

      END IF

*  Construct the Mapping.
*  ======================

*  Get the type of Mapping which is to be used.
      CALL PAR_CHOIC( 'MAPTYPE', 'LINEAR', 'DIAGONAL,FILE,LINEAR,'//
     :                'MATH,PINCUSHION,REFNDF,SHIFT,UNIT,ZOOM', .FALSE.,
     :                MAPTYP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Create a MatrixMap from diagonal coefficients.
      IF ( MAPTYP .EQ. 'DIAGONAL' ) THEN

*  Get the coefficients.
         CALL PAR_EXACD( 'DIAG', NAXB, DIAG, STATUS )

*  Construct the MatrixMap.
         MAP = AST_MATRIXMAP( NAXB, NAXB, 1, DIAG, ' ', STATUS )

*  Use a Mapping stored in an external file.
      ELSE IF ( MAPTYP .EQ. 'FILE' ) THEN

*  Get the Mapping from the file named by the MAPIN parameter.
         CALL KPG1_GTOBJ( 'MAPIN', 'Mapping', AST_ISAMAPPING, MAP,
     :                    STATUS )

*  Create a Mapping representing a general linear transformation.
      ELSE IF ( MAPTYP .EQ. 'LINEAR' ) THEN

*  Get the coefficients of the linear transformation from the basis
*  Frame to the new Frame.  Ensure the exact required number are
*  supplied.
         ACTVAL = 0
         NCOEF = ( NAXB + 1 )*NAXB
         DO WHILE( ACTVAL .NE. NCOEF .AND. STATUS .EQ. SAI__OK )
            CALL PAR_GET1D( 'TR', NCOEF, TR, ACTVAL, STATUS )
            IF( ACTVAL .NE. NCOEF .AND. STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETI( 'N', NCOEF )
               CALL MSG_OUT( 'WCSADD_MSG1', 'Please supply exactly '//
     :                       '^N coefficient values (or a null '//
     :                       'value) for parameter %TR.', STATUS )
               CALL PAR_CANCL( 'TR', STATUS )
            END IF
         END DO

*  If a null value was given, annul the error and create a unit
*  MatrixMap.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MAP = AST_MATRIXMAP( NAXB, NAXB, 2, 0.0D0, ' ', STATUS )

*  Otherwise, if no error has occurred, extract the offset and matrix
*  from the supplied list of coefficients.
         ELSE

*  Extract the offset into a separate vector, making two copies.
            DO I = 1, NAXB
               OFFSET( I ) = TR( 1 + ( I - 1 )*( NAXB + 1 ) )
               OTEST( I ) = OFFSET( I )
            END DO

*  Extract the matrix into a separate vector, making two copies.
            K = 1
            L = 1
            DO I = 1, NAXB
               K = K + 1

               DO J = 1, NAXB
                  MATRIX( L ) = TR( K )
                  MTEST( L ) = TR( K )
                  L = L + 1
                  K = K + 1
               END DO

            END DO

*  See if the matrix is singular. The MTEST and OTEST arrays
*  are changed by this call, This is why we took two copies above.
            CALL SLA_DMAT( NAXB, MTEST, OTEST, DET, SING, WORK )

*  Report an error if the matrix is singular.
            IF( SING .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'WCSADD_ERR1', 'The supplied '//
     :                       'transformation matrix is singular, and '//
     :                       'therefore cannot be inverted.', STATUS )
            END IF

*  Create a MatrixMap from the supplied MATRIX array.
            MTRMAP = AST_MATRIXMAP( NAXB, NAXB, 0, MATRIX, ' ', STATUS )

*  Create a WinMap which gives the required shift of pixel origin.
            DO I = 1, NAXB
               INA( I ) = 0.0D0
               INB( I ) = MAX( ABS( OFFSET( I ) ), 1.0D0 )
               OUTA( I ) = INA( I ) + OFFSET( I )
               OUTB( I ) = INB( I ) + OFFSET( I )
            END DO

            WINMAP = AST_WINMAP( NAXB, INA, INB, OUTA, OUTB, ' ',
     :                           STATUS )

*  Concatenate these two mappings in series to get the mapping from the
*  basis Frame to the new Frame.
            MAP = AST_CMPMAP( MTRMAP, WINMAP, .TRUE., ' ', STATUS )

         END IF

*  Create a MathMap from algebraic expressions supplied by the user.
      ELSE IF ( MAPTYP .EQ. 'MATH' ) THEN

*  Get a GRP group holding the algebraic expressions for the forward
*  transformation.
         IGRP = GRP__NOID
         CALL KPG1_GTGRP( 'FOREXP', IGRP, NFEXP, STATUS )
         DO WHILE( NFEXP .LT. NAXB .AND. STATUS .EQ. SAI__OK )
            CALL MSG_SETI( 'N', NAXB )
            CALL MSG_OUT( 'WCSADD_MSG2', 'At least ^N forward '//
     :                    'expressions are required - please enter '//
     :                    'them again.', STATUS )
            CALL PAR_CANCL( 'FOREXP', STATUS )
            CALL KPG1_GTGRP( 'FOREXP', IGRP, NFEXP, STATUS )
         END DO

*  Report an error if too many expressions were given.
         IF( NFEXP .GT. MAXEXP .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MX', MAXEXP )
            CALL MSG_SETI( 'N', NFEXP )
            CALL ERR_REP( 'WCSADD_MSG3', 'Too many (^N) forward '//
     :                    'expressions given. No more than ^MX '//
     :                    'should be supplied.', STATUS )
         END IF

*  Copy the expressions into a local array.
         CALL GRP_GET( IGRP, 1, NFEXP, FOREXP, STATUS )

*  Similarly, get the algebraic expressions for the inverse
*  transformation.
         CALL KPG1_GTGRP( 'INVEXP', IGRP, NIEXP, STATUS )
         DO WHILE( NIEXP .LT. NAXB .AND. STATUS .EQ. SAI__OK )
            CALL MSG_SETI( 'N', NAXB )
            CALL MSG_OUT( 'WCSADD_MSG4', 'At least ^N inverse '//
     :                    'expressions are required - please enter '//
     :                    'them again.', STATUS )
            CALL PAR_CANCL( 'INVEXP', STATUS )
            CALL KPG1_GTGRP( 'INVEXP', IGRP, NIEXP, STATUS )
         END DO

         IF( NIEXP .GT. MAXEXP .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MX', MAXEXP )
            CALL MSG_SETI( 'N', NIEXP )
            CALL ERR_REP( 'WCSADD_MSG3', 'Too many (^N) inverse '//
     :                    'expressions given. No more than ^MX '//
     :                    'should be supplied.', STATUS )
         END IF

         CALL GRP_GET( IGRP, 1, NIEXP, INVEXP, STATUS )

*  Delete the group.
         CALL GRP_DELET( IGRP, STATUS )

*  See whether it looks as if non-dummy expressions have been supplied
*  for both forward and inverse transformations.  This is not foolproof,
*  but the consequences of getting it wrong are not serious.
         FIBOTH = INDEX( FOREXP( NFEXP ), '=' ) .NE. 0
     :      .AND. INDEX( INVEXP( NFEXP ), '=' ) .NE. 0

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
         MAP = AST_MATHMAP( NAXB, NAXB, NFEXP, FOREXP, NIEXP, INVEXP,
     :                      ' ', STATUS )

*  Add simplification attributes.
         CALL AST_SETL( MAP, 'SimpFI', SIMPFI, STATUS )
         CALL AST_SETL( MAP, 'SimpIF', SIMPIF, STATUS )

*  Create a Mapping to a Frame in a different NDF, aligning the two NDFs
*  in an appropriate Frame.
      ELSE IF ( MAPTYP .EQ. 'REFNDF' ) THEN

*  Obtain an identifier for the reference NDF.
         CALL LPG_ASSOC( 'REFNDF', 'READ', INDFR, STATUS )

*  Get the WCS FrameSet from the reference NDF.
         CALL KPG1_GTWCS( INDFR, IWCSR, STATUS )

*  Get the Frame within REFNDF that is to be added into NDF. It becomes
*  the current Frame in IWCSR. We want to retain the original current
*  Frame, so note its index first.
         ICURR = AST_GETI( IWCSR, 'Current', STATUS )
         CALL NDF_MSG( 'NDF', INDFR )
         CALL KPG1_ASFRM( 'DOMAIN', 'EPOCH', IWCSR, 'PIXEL', 'AXIS',
     :                    .TRUE., '^NDF', STATUS )

*  Get the index of the Frame to be copied, and then reinstate the
*  original current Frame within IWCSR.
         ICOPY = AST_GETI( IWCSR, 'Current', STATUS )
         CALL AST_SETI( IWCSR, 'Current', ICURR, STATUS )

*  Merge the two FrameSets by aligning them in a common Frame. The IWCS
*  FrameSet is modified on exit to include copies of all the Frames in
*  IWCSR. First note the original number of Frames in IWCS so that we can
*  identify which Frame is which afterwards. Also, use a copy of IWCS so
*  that we do not modify the original.
         IWCS2 = AST_COPY( IWCS, STATUS )
         NFRM = AST_GETI( IWCS2, 'NFrame', STATUS )
         CALL KPG1_ASMRG( IWCS2, IWCSR, ' ', .FALSE., 3, STATUS )

*  Modify ICOPY so that it gives the index within IWCS2 of the Frame being
*  copied.
         ICOPY = ICOPY + NFRM

*  Get the reuqired Mapping, and simplify it.
         MAP = AST_GETMAPPING( IWCS2, IBASIS, ICOPY, STATUS )
         MAP = AST_SIMPLIFY( MAP, STATUS )

*  Get the Frame to be added into the returned NDF.
         FRMN = AST_GETFRAME( IWCS2, ICOPY, STATUS )

*  Create a PcdMap.
      ELSE IF ( MAPTYP .EQ. 'PINCUSHION' ) THEN

*  Check that the basis Frame has exactly two axes.
         IF ( NAXB .NE. 2 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'WCSADD_ERR2', 'A pincushion distortion is '//
     :                    'valid on a 2-dimensional basis Frame.',
     :                    STATUS )
            GO TO 999
         END IF

*  Get the DISCO and CENTRE coefficients required for the Mapping.
         CALL PAR_GET0D( 'DISCO', DISCO, STATUS )
         CALL PAR_EXACD( 'CENTRE', 2, CENTRE, STATUS )

*  Construct the PcdMap.
         MAP = AST_PCDMAP( DISCO, CENTRE, ' ', STATUS )

*  Create a translated linear Mapping.
      ELSE IF ( MAPTYP .EQ. 'SHIFT' ) THEN

*  Get the coefficients for the translation.
         CALL PAR_EXACD( 'SHIFT', NAXB, SHIFT, STATUS )

*  Construct the corresponding WinMap.
         DO I = 1, NAXB
            INA( I ) = 0D0
            INB( I ) = 1D0
            OUTA( I ) = INA( I ) + SHIFT( I )
            OUTB( I ) = INB( I ) + SHIFT( I )
         END DO
         MAP = AST_WINMAP( NAXB, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Create a UnitMap.
      ELSE IF ( MAPTYP .EQ. 'UNIT' ) THEN
         MAP = AST_UNITMAP( NAXB, ' ', STATUS )

*  Create a ZoomMap.
      ELSE IF ( MAPTYP .EQ. 'ZOOM' ) THEN
         CALL PAR_GET0D( 'ZOOM', ZOOM, STATUS )
         MAP = AST_ZOOMMAP( NAXB, ZOOM, ' ', STATUS )
      END IF

*  Simplify the Mapping
      MAP = AST_SIMPLIFY( MAP, STATUS )

*  Get the Frame to add.
*  =====================

*  We only need to do this if a basis Frame was supplied. Also, if we are
*  copying a Frame from a reference NDF, we will already have the required
*  Frame pointer (FRMN).
      IF( FRMB .NE. AST__NULL .AND. MAPTYP .NE. 'REFNDF' ) THEN

*  Choose the suggested default for parameter FRMTYPE. Make it equal to
*  the class of the basis Frame.
         IF( AST_ISASPECFRAME( FRMB, STATUS ) ) THEN
            FRMTYP = 'SpecFrame'
         ELSE IF( AST_ISASKYFRAME( FRMB, STATUS ) ) THEN
            FRMTYP = 'SkyFrame'
         ELSE IF( AST_ISATIMEFRAME( FRMB, STATUS ) ) THEN
            FRMTYP = 'TimeFrame'
         ELSE
            FRMTYP = 'Frame'
         END IF

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the class for the new Frame.
         CALL PAR_CHOIC( 'FRMTYPE', FRMTYP, 'Frame,SkyFrame,'//
     :                   'SpecFrame,TimeFrame', .FALSE., FRMTYP,
     :                   STATUS )

*  If a null value was supplied, simply copy the basis Frame */
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            FRMN = AST_COPY( FRMB, STATUS )

*  Allow the user to modify the attributes of the Frame.
            CALL KPG1_ASSET( 'WCSADD', 'ATTRS', FRMN, STATUS )

*  Otherwise, create a default Frame of the specified class.
         ELSE
            IF( FRMTYP .EQ. 'SKYFRAME' ) THEN
               FRMN = AST_SKYFRAME( ' ', STATUS )

            ELSE IF( FRMTYP .EQ. 'SPECFRAME' ) THEN
               FRMN = AST_SPECFRAME( ' ', STATUS )

            ELSE IF( FRMTYP .EQ. 'TIMEFRAME' ) THEN
               FRMN = AST_TIMEFRAME( ' ', STATUS )

            ELSE
               FRMN = AST_FRAME( AST_GETI( MAP, 'NOUT', STATUS ), ' ',
     :                           STATUS )
            END IF

*  Allow the user to modify the attributes of the Frame.
            CALL KPG1_ASSET( 'WCSADD', 'ATTRS', FRMN, STATUS )

*  See if attributes that are set in the basis Frame should be
*  transferred to the new Frame. The dynamnic default is to do so, but only
*  if the two Frames are off the same class, and have the same Domain.
            XFER = .FALSE.
            IF( AST_GETC( FRMN, 'CLASS', STATUS ) .EQ.
     :          AST_GETC( FRMB, 'CLASS', STATUS ) ) THEN
               IF( AST_GETC( FRMN, 'Domain', STATUS ) .EQ.
     :             AST_GETC( FRMB, 'Domain', STATUS ) ) THEN
                  XFER = .TRUE.
               END IF
            END IF
            CALL PAR_DEF0L( 'TRANSFER', XFER, STATUS )
            CALL PAR_GET0L( 'TRANSFER', XFER, STATUS )

*  If required, transfer the values of attributes which have been set in
*  the basis Frame to the new Frame, and modify the Mapping. We temporarily
*  clear the Domain in the basis Frame (if set) so that we can transfer
*  attribute values to Frames with other Domains.
            IF( XFER ) THEN
               IF( AST_TEST( FRMB, 'Domain', STATUS ) ) THEN
                  DOM0 = AST_GETC( FRMB, 'Domain', STATUS )
                  CALL AST_CLEAR( FRMB, 'Domain', STATUS )
               ELSE
                  DOM0 = ' '
               END IF

*  Do the transferring.
               RESULT = AST_FINDFRAME( FRMN, FRMB, ' ', STATUS )
               IF( DOM0 .NE. ' ' ) CALL AST_SETC( FRMB, 'Domain', DOM0,
     :                                            STATUS )

*  If succesful, use the modified new Frame and modify the Mapping.
               IF( RESULT .NE. AST__NULL ) THEN
                  FRMN = AST_GETFRAME( RESULT, AST__CURRENT, STATUS )
                  MAP = AST_CMPMAP( MAP, AST_GETMAPPING( RESULT,
     :                                          AST__BASE, AST__CURRENT,
     :                                          STATUS ), .TRUE.,
     :                              ' ', STATUS )
                  MAP = AST_SIMPLIFY( MAP, STATUS )

*  If not succesful, warn the user.
               ELSE
                  CALL MSG_SETC( 'B', AST_GETC( FRMB, 'CLASS', STATUS ))
                  CALL MSG_SETC( 'N', AST_GETC( FRMN, 'CLASS', STATUS ))
                  CALL MSG_OUT( ' ', 'WARNING: attribute values '//
     :                          'could not be transferered from the '//
     :                          'basis ^B to the new ^N.', STATUS )
               END IF
            END IF
         END IF

*  Check the number of axes in the Frame matches the number of outputs
*  from the Mapping.
         IF( AST_GETI( FRMN, 'NAXES', STATUS ) .NE.
     :       AST_GETI( MAP, 'NOUT', STATUS ) .AND.
     :       STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'NOUT', AST_GETI( MAP, 'NOUT', STATUS ) )
            CALL MSG_SETI( 'NAX', AST_GETI( MAP, 'NAXES', STATUS ) )
            CALL ERR_REP( 'WCSMAP_ERR3', 'Mapping has ^NOUT outputs '//
     :                    'but new Frame has ^NAX axes. These number '//
     :                    'should be equal.', STATUS )
            GO TO 999
         END IF

*  Note the current Domain in the Frame.
         DOM0 = AST_GETC( FRMN, 'Domain', STATUS )

*  If the new Frame has active Unit attributes, check that all Unit
*  attributes have non-blank values.
         IF( AST_GETACTIVEUNIT( FRMN, STATUS ) ) THEN
            ATTRIB = 'Unit('
            DO I = 1, AST_GETI( FRMN, 'Naxes', STATUS )
               IAT = 5
               CALL CHR_PUTI( I, ATTRIB, IAT )
               CALL CHR_PUTC( ')', ATTRIB, IAT )
               IF( AST_GETC( FRMN, ATTRIB( : IAT ), STATUS ) .EQ.
     :             ' ' .AND. STATUS .EQ. SAI__OK ) THEN
                  CALL MSG_SETC( 'AT', ATTRIB( : IAT ) )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'WCSADD_ERR', 'The ^AT attribute in '//
     :                          'the new Frame has a blank value. The'//
     :                          ' units must be specified for all '//
     :                          'axes of the Frame.', STATUS )
                  GO TO 999
                END IF
             END DO
         END IF

*  Get the Domain for the new Frame.  If the current Domain is non-blank
*  and different to the Domain of the basis Frame, use it as the dynamic
*  default.
         DOM = AST_GETC( FRMN, 'Domain', STATUS )
         IF( DOM .NE. ' ' .AND.
     :       DOM .NE. AST_GETC( FRMB, 'Domain', STATUS ) ) THEN
            CALL PAR_DEF0C( 'DOMAIN', DOM, STATUS )
         END IF

         CALL PAR_GET0C( 'DOMAIN', DOM, STATUS )

*  Remove spaces, and convert to upper case.
         CALL CHR_RMBLK( DOM )
         CALL CHR_UCASE( DOM )

*  Clear the Domain attribute.
         CALL AST_CLEAR( FRMN, 'DOMAIN', STATUS )

*  Store the new Domain if it is different to the default Domain.
         IF( DOM .NE. AST_GETC( FRMN, 'DOMAIN', STATUS ) ) THEN
            CALL AST_SETC( FRMN, 'DOMAIN',
     :                     DOM( : MAX( 1, CHR_LEN( DOM ) ) ), STATUS )
         END IF

      END IF

*  Use the constructed Mapping.
*  ============================

*  Write the Mapping out to a text file.
      IF( STATUS .NE. SAI__OK ) GO TO 999
      CALL ATL_CREAT( 'MAPOUT', MAP, STATUS )

*  If a null value was supplied, annull the error
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  If we do not have an NDF, cancel the parameter and try once more to
*  create an output file.  We do this because the default value for
*  MAPOUT in the ifl file is a null.
         IF( INDF .EQ. NDF__NOID ) THEN
            CALL PAR_CANCL( 'MAPOUT', STATUS )
            CALL ATL_CREAT( 'MAPOUT', MAP, STATUS )
         END IF
      END IF

*  If we have an NDF, add the new Frame into the FrameSet, and store
*  the modified FrameSet in the NDF.
      IF( INDF .NE. NDF__NOID ) THEN

*  Add the new Frame into the FrameSet. It becomes the current Frame.
         CALL AST_ADDFRAME( IWCS, IBASIS, MAP, FRMN, STATUS )

*  See if the original current Frame is to be retained. If so, reinstate
*  the original current Frame.
         CALL PAR_GET0L( 'RETAIN', RETAIN, STATUS )
         IF( RETAIN ) CALL AST_SETI( IWCS, 'Current', ICUR0, STATUS )

*  Store the FrameSet in the NDF.
         CALL NDF_PTWCS( IWCS, INDF, STATUS )
      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSADD_ERR4', 'WCSADD: Failed to add a new '//
     :                 'co-ordinate Frame into an NDF WCS component.',
     :                 STATUS )
      END IF

      END
