      SUBROUTINE PASTE( STATUS )
*+
*  Name:
*     PASTE

*  Purpose:
*     Pastes a series of NDFs upon each other.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PASTE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application copies a series of NDFs, in the order supplied
*     and taking account of origin information, on to a `base' NDF to
*     produce an output NDF.  The output NDF is therefore a copy of the
*     base NDF obscured wholly or partially by the other input NDFs.
*     This operation is analogous to pasting in publishing.  It is
*     intended for image editing and the creation of insets.
*
*     The dimensions of the NDFs may be different, and indeed so may
*     their dimensionalities.  The output NDF can be constrained to
*     have the dimensions of the base NDF, so the pasted NDFs are
*     clipped.  Normally, the output NDF will have dimensions such
*     that all the input NDFs are accommodated in full.
*
*     Bad values in the pasted NDFs are by default transparent, so the
*     underlying data are not replaced during the copying.
*
*     Input NDFs can be shifted in pixel space before pasting them into
*     the output NDF (see parameter SHIFT).

*  Usage:
*     paste in p1 [p2] ... [p25] out=?

*  ADAM Parameters:
*     CONFINE = _LOGICAL (Read)
*        This parameter controls the dimensions of the output NDF.  If
*        CONFINE is FALSE the output NDF just accommodates all the input
*        NDFs.  If CONFINE is TRUE, the output NDF's dimensions matches
*        those of the base NDF. [FALSE]
*     IN = NDF (Read)
*        This parameter is either:
*        a) the base NDF on to which the other input NDFs supplied via
*        parameters P1 to P25 will be pasted; or
*        b) a group of input NDFs (of any dimensionality) comprising all
*        the input NDFs, of which the first is deemed to be the base
*        NDF, and the remainder are to be pasted in the order supplied.
*
*        The group should be given as a comma-separated list, in which
*        each list element can be:
*
*        - an NDF name, optionally containing wild-cards and/or regular
*        expressions ("*", "?", "[a-z]" etc.).
*
*        - the name of a text file, preceded by an up-arrow character
*        "^".  Each line in the text file should contain a
*        comma-separated list of elements, each of which can in turn be
*        an NDF name (with optional wild-cards, etc.), or another file
*        specification (preceded by an up-arrow).  Comments can be
*        included in the file by commencing lines with a hash character
*        "#".
*
*        If the value supplied for this parameter ends with a hyphen
*        "-", then you are re-prompted for further input until a value
*        is given which does not end with a hyphen.  All the NDFs
*        given in this way are concatenated into a single group.
*
*        The group can contain no more than 1000 names.
*     OUT = NDF (Write)
*        The NDF resulting from pasting of the input NDFs onto the base
*        NDF.  Its dimensions may be different from the base NDF.  See
*        parameter CONFINE.
*     P1-P25 = NDF (Read)
*        The NDFs to be pasted on to the base NDF.  The NDFs are pasted
*        in the order P1, P2, ... P25.  There can be no missing NDFs,
*        e.g. in order for P3 to be processed there must be a P2 given
*        as well.  A null value (!) indicates that there is no NDF.
*        NDFs P2 to P25 are defaulted to !.  At least one NDF must be
*        pasted, therefore P1 may not be null.
*
*        P1 to P25 are ignored if the group specified through parameter IN
*        comprises more than one NDF.
*     SHIFT( * ) = _INTEGER (Read)
*        An incremental shift to apply to the pixel origin of each input
*        NDF before pasting it into the output NDF. If supplied, this
*        parameter allows a set of NDFs with the same pixel bounds to be
*        placed "side-by-side" in the output NDF. For instance, this
*        allows a set of images to be pasted into a cube. The first input
*        NDF is not shifted. The pixel origin of the second NDF is shifted
*        by the number of pixels given in SHIFT. The pixel origin of the
*        third NDF is shifted by twice the number of pixels given in
*        SHIFT. Each subsequent input NDF is shifted by a further
*        multiple of SHIFT. If null (!) is supplied, no shifts are
*        applied. (!)
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the base NDF to the output NDF. [!]
*     TRANSP = _LOGICAL (Read)
*        If TRANSP is TRUE, bad values within the pasted NDFs are not
*        copied to the output NDF as if the bad values were transparent.
*        If TRANSP is FALSE, all values are copied during the paste
*        and a bad value will obscure an underlying value.  [TRUE]

*  Examples:
*     paste aa inset out=bb
*        This pastes the NDF called inset on to the arrays in the NDF
*        called aa to produce the NDF bb.  Bad values are transparent.
*        The bounds and dimensionality of bb may be larger than those of
*        aa.
*     paste aa inset out=bb notransp
*        As above except that bad values are copied from the NDF inset
*        to NDF bb.
*     paste aa inset out=bb confine
*        As the first example except that the bounds of NDF bb match
*        those of NDF aa.
*     paste in="aa,inset" out=bb
*        The same as the first example.
*     paste in="aa,inset,inset2,inset3" out=bb
*        Similar to first example, but now two further NDFs inset2 and
*        inset3 are also pasted.
*     paste ccd fudge inset out=ccdc
*        This pastes the NDF called fudge, followed by NDF inset on to
*        the arrays in the NDF called ccd to produce the NDF ccdc.  Bad
*        values are transparent.  The bounds and dimensionality of ccd
*        may be larger than those of ccdc.
*     paste in="canvas,^shapes.lis" out=collage confine
*        This pastes the NDFs listed in the text file shapes.lis in
*        the order given on the NDF called canvas.  Bad values are
*        transparent.  The bounds of NDF collage match those of NDF
*        canvas.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY, components of an
*     NDF data structure and propagates all extensions.  Propagation is
*     from the base NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1998, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2005 Particle Physics & Astronomy
*     Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1991 November 15 (MJC):
*        Original version.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     22-JUN-2004 (DSB):
*        Correct bounds matching logic for CONFINE=YES case.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2005 November 2 (MJC):
*        Allowed parameter IN to be a group.  Added examples to
*        demonstrate this option.
*     2010 December 15 (MJC):
*        Increase maximum number of NDFs defined through Parameter IN
*        to 1000.
*     2-MAY-2012 (DSB):
*        Added parameter SHIFT.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF public constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDFMXC             ! Maximum number of input NDFs
      PARAMETER ( NDFMXC = 26 )  ! Classic method

      INTEGER NDFMAX             ! Maximum number of input NDFs
      PARAMETER ( NDFMAX = 1000 )

*  Local Variables:
      LOGICAL BAD                ! Input NDFs' data arrays may have bad
                                 ! values
      LOGICAL BADQUA             ! Input NDFs' quality may have bad
                                 ! values
      LOGICAL BADVAR             ! Input NDFs' variance may have bad
                                 ! values
      CHARACTER * ( 2 ) CIN      ! The number of the input NDF
      LOGICAL CONFIN             ! Output NDF is confined to bounds of
                                 ! the principal input NDF
      INTEGER DIMSI( NDF__MXDIM, NDFMAX ) ! Dimensions of input NDFs
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Processing type of the data
                                 ! array
      CHARACTER * ( NDF__SZFTP ) DTYPEV ! Processing type of the
                                 ! variance array
      INTEGER ELI                ! Number of elements in an input array
      INTEGER ELO                ! Number of elements in an output array
      LOGICAL EXCLUD( NDFMAX )   ! Input NDF is entirely outside the
                                 ! bounds of the output NDF
      INTEGER GRPIN              ! GRP id. for group holding input NDFs
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER IDIMS( NDF__MXDIM ) ! Dimensions of an input array
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing type of the data
                                 ! array
      CHARACTER * ( NDF__SZTYP ) ITYPEV ! Processing type of the
                                 ! variance array
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of an input NDF
      INTEGER LBNDI( NDF__MXDIM, NDFMAX ) ! Lower bounds of input NDFs
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER NC                 ! Number of characters in input NDF
                                 ! number
      INTEGER NDFI( NDFMAX )     ! Identifiers for input NDFs
      INTEGER NDFIC( NDFMAX )    ! Cloned identifiers for input NDFs
      INTEGER NDFIP              ! Identifier for cloned principal input
                                 ! NDF
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NDFS               ! Identifier for input NDF section
      INTEGER NDIM               ! Number of dimensions (not used)
      INTEGER NSHIFT             ! Number of axes with supplied shifts
      INTEGER NUMNDF             ! Number of input NDFs
      INTEGER ODIMS( NDF__MXDIM ) ! Dimensions of output array
      INTEGER OFFSET( NDF__MXDIM ) ! Offsets of an input NDF wrt output
                                 ! array's origin
      CHARACTER * ( 4 ) PNIN     ! Parameter names of each of the input
                                 ! NDFS
      INTEGER PNTRI( 1 )         ! Pointer to input array component
      INTEGER PNTRO( 1 )         ! Pointer to output data array
                                 ! component
      INTEGER PNTROQ( 1 )        ! Pointer to output quality component
      INTEGER PNTROV( 1 )        ! Pointer to output variance component
      LOGICAL QUAPRS             ! Variance is present in the NDF
      INTEGER SHIFT( NDF__MXDIM )! The shift of origin between each pair
                                 ! of adjacent input NDFs
      LOGICAL TRANSP             ! Bad values in the input NDFs are
                                 ! transparent when pasted
      INTEGER TSHIFT( NDF__MXDIM )! The total shift of origin for the
                                 ! next input NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of an input NDF
      INTEGER UBNDI( NDF__MXDIM, NDFMAX ) ! Upper bounds of input NDFs
      INTEGER UBNDO( NDF__MXDIM ) ! Upper bounds of output NDF
      LOGICAL VARPRS             ! Variance is present in the NDF

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine whether bad values are transparent when pasted onto an
*  NDF.
      CALL PAR_GTD0L( 'TRANSP', .TRUE., .FALSE., TRANSP, STATUS )

*  Determine whether the output NDF's bounds are to be constrained to
*  those of the principal (first or BASE) NDF.
      CALL PAR_GTD0L( 'CONFINE', .FALSE., .FALSE., CONFIN, STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDFs.
*  ======================

*  Get a group containing the names of the NDFs to be processed.
      CALL KPG1_RGNDF( 'IN', NDFMAX, 1, '  Give more NDFs to paste...',
     :                 GRPIN, NUMNDF, STATUS )

*  The options are a) classic method where one principal NDF is supplied
*  via parameter IN and those to be pasted through parameters P1 to P25.
*  The modern alternative is to supply a list via GRP, where the first
*  file is the base and the subsequent frames are pasted in the order
*  supplied.

*  Classic
*  -------
      IF ( NUMNDF .EQ. 1 ) THEN

*  First the principal NDF.
         CALL NDG_NDFAS( GRPIN, 1, 'READ', NDFI( 1 ), STATUS )

*  Make a loop to input the NDFs, via parameters P1, P2,...  Start an
*  error context because a null is used to end the list of NDFs.  Since
*  the order and bounds given after the NDF name are important, IRG
*  cannot be used safely.  Limit to the constrained maximum number of
*  NDFs.
         CALL ERR_MARK
         I = 0
         DO WHILE ( STATUS .EQ. SAI__OK .AND. I .LT. NDFMXC - 1 )
            I = I + 1
            CALL CHR_ITOC( I, CIN, NC )
            PNIN = 'P'//CIN( :NC )
            CALL LPG_ASSOC( PNIN, 'READ', NDFI( I + 1 ), STATUS )
         END DO

*  Look for the expected null.  Note the first pasted NDF may not be
*  null as there must be at least one pasted NDF.
         IF ( STATUS .EQ. PAR__NULL .AND. I .GT. 1 ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE

*  Record the number of input NDFs.  Make it at least one to prevent
*  problems exiting.  The number is I, not I-1 because the subtraction
*  for the extra loop is counteracted by plus one for the principal
*  NDF, except for the last input NDF as the loop is not entered after
*  it has been obtained.
         IF ( I .GE. NDFMXC - 1 ) THEN
            NUMNDF = NDFMXC
         ELSE
            NUMNDF = MAX( 1, I )
         END IF

*  Modern
*  ------
      ELSE
         DO I = 1, NUMNDF

*  Obtain identifiers for each NDF.
            CALL NDG_NDFAS( GRPIN, I, 'READ', NDFI( I ), STATUS )
         END DO
      END IF


*  Apply any requested shifts of origin to the input NDFs.
*  ======================================================

* Get the incremental shift of origin to apply to each sucessive input
* NDF. If a null value is supplied annull the error and continue.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET1I( 'SHIFT', NDF__MXDIM, SHIFT, NSHIFT, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  If shifts were supplied, shift each input NDF in turn, except for
*  the first one. Initialise the shift and then loop over NDFs.
         ELSE
            DO J = 1, NSHIFT
               TSHIFT( J ) = SHIFT( J )
            END DO

            DO J = NSHIFT + 1, NDF__MXDIM
               TSHIFT( J ) = 0
            END DO

            DO I = 2, NUMNDF

*  First get a section identifier for the whole of the input NDF. We
*  need to do this so that we can change the NDFs origin without
*  affecting the input NDF on disk.
               CALL NDF_BOUND( NDFI( I ), NDF__MXDIM, LBND, UBND, NDIM,
     :                         STATUS )
               CALL NDF_SECT( NDFI( I ), NDIM, LBND, UBND, NDFS,
     :                        STATUS )

*  Apply the shift
               CALL NDF_SHIFT( NDIM, TSHIFT, NDFS, STATUS )

*  Replace the stored input NDF identifier with the shifted section
*  identifier. We rely on the NDF context to annul the original identifier.
               NDFI( I ) = NDFS

*  Get the shift for the next input NDF.
               DO J = 1, NSHIFT
                  TSHIFT( J ) = I*SHIFT( J )
               END DO

            END DO
         END IF
      END IF

*  Determine the bounds of the output NDF.
*  =======================================
*
*  Match the bounds of the input NDFs.  If the output NDF's bounds are
*  confined to be within the principal NDF's, we first match bounds by
*  trimming the second and later NDFs with those of the principal NDF.
*  The principal NDF identifier must be cloned each time because the
*  call to NDF_MBND will modify its bounds.
      IF ( CONFIN ) THEN
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO I = 2, NUMNDF
               CALL ERR_MARK
               CALL NDF_CLONE( NDFI( 1 ), NDFIP, STATUS )
               CALL NDF_MBND( 'TRIM', NDFIP, NDFI( I ), STATUS )
               CALL NDF_ANNUL( NDFIP, STATUS )

*  Watch for the special case where the NDF is not within the bounds of
*  the output, and so cannot be pasted.  This is done by starting a new
*  error context and annulling the error, but recording the fact that
*  pasting will not be required.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  EXCLUD( I ) = .TRUE.
               ELSE
                  EXCLUD( I ) = .FALSE.
               END IF
               CALL ERR_RLSE
            END DO
         END IF

*  Clone then pad all the input NDFs.  Cloning is required so that
*  original NDFs are not lost because their identifiers may be annulled
*  during the matching process.  Hence the original NDFs are pasted.
*  The principal NDF is cloned for convenience in the coding, but need
*  not be, since it is the padded clone which is propagated to for the
*  output NDF.  Since padding is being performed all NDFs all included.
      ELSE
         DO I = 1, NUMNDF
            CALL NDF_CLONE( NDFI( I ), NDFIC( I ), STATUS )
            EXCLUD( I ) = .FALSE.
         END DO
         CALL NDF_MBNDN( 'PAD', NUMNDF, NDFIC, STATUS )

*  All but the principal NDF's clone can be dispensed with to release
*  resources.
         DO I = 2, NUMNDF
            CALL NDF_ANNUL( NDFIC( I ), STATUS )
         END DO
      END IF

*  Record the bounds of the NDFs.
*  ==============================
*
*  For efficiency reasons we only want to paste the data areas, not the
*  regions padded by NDF_.  Also the transparency flag applies only to
*  bad pixels within the original NDFs.  Therefore find the bounds and
*  dimensions of the NDFs to be pasted.  Note that at this point the
*  NDFIs identifiers belong to the NDFs that are going to be pasted
*  independent of whether confinement is operating or not.  So the
*  confined NDFs only include the data areas.
*
*  To be general we want to obtain the dimensions up to the maximum,
*  even though many of these may be one.
      DO I = 1, NUMNDF
         CALL NDF_BOUND( NDFI( I ), NDF__MXDIM, LBND, UBND, NDIM,
     :                   STATUS )

*  Store the bounds in convenient arrays.
         DO J = 1, NDF__MXDIM
            LBNDI( J, I ) = LBND( J )
            UBNDI( J, I ) = UBND( J )
            DIMSI( J, I ) = UBND( J ) - LBND( J ) + 1
         END DO
      END DO

*  Match the bounds and data types of the input NDFs.
*  ==================================================

*  Determine which array components are present.
      CALL NDF_STATE( NDFI( 1 ), 'Variance', VARPRS, STATUS )
      CALL NDF_STATE( NDFI( 1 ), 'Quality', QUAPRS, STATUS )

*  Match the bad-pixel flags.
      CALL NDF_MBADN( .TRUE., NUMNDF, NDFI, 'Data', .FALSE., BAD,
     :                STATUS )

      IF ( VARPRS ) CALL NDF_MBADN( .TRUE., NUMNDF, NDFI, 'Variance',
     :   .FALSE., BADVAR, STATUS )

      IF ( QUAPRS ) CALL NDF_MBADN( .TRUE., NUMNDF, NDFI, 'Quality',
     :   .FALSE., BADQUA, STATUS )

*  Match the data types.  Quality must have type _UBYTE.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'/
     :  /'_DOUBLE', NUMNDF, NDFI, 'Data', ITYPE, DTYPE, STATUS )

      IF ( VARPRS ) CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,'/
     :  /'_INTEGER,_REAL,_DOUBLE', NUMNDF, NDFI, 'Variance', ITYPEV,
     :  DTYPEV, STATUS )

*  Create the output NDF.
*  ======================
*
*  At this point any of array of identifiers or the principal NDF will
*  have the bounds of the output pasted NDF.  Want to propagate from
*  the principal array.
      IF ( CONFIN ) THEN
         CALL LPG_PROP( NDFI( 1 ), 'WCS,AXIS,UNITS', 'OUT', NDFO,
     :                  STATUS )
      ELSE
         CALL LPG_PROP( NDFIC( 1 ), 'WCS,AXIS,UNITS', 'OUT', NDFO,
     :                  STATUS )
      END IF

*  Set the array types.
      CALL NDF_STYPE( DTYPE, NDFO, 'Data', STATUS )
      IF ( VARPRS ) CALL NDF_STYPE( DTYPEV, NDFO, 'Variance', STATUS )

*  Find the bounds and dimensions of the output NDF.
      CALL NDF_BOUND( NDFO, NDF__MXDIM, LBNDO, UBNDO, NDIM, STATUS )

      DO J = 1, NDF__MXDIM
         ODIMS( J ) = UBNDO( J ) - LBNDO( J ) + 1
      END DO

*  Cloned identifier is no longer required so annul it to release
*  resources.
      IF ( .NOT. CONFIN ) THEN
         CALL NDF_ANNUL( NDFIC( 1 ), STATUS )
      END IF

*  Obtain a title and assign it to the output NDF.
*  ===============================================

*  A null results in the output title being the same as the input
*  title.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', NDFI( 1 ), NDFO, STATUS )

*  Map the output data array component.
*  ====================================
      CALL KPG1_MAP( NDFO, 'Data', ITYPE, 'WRITE/BAD', PNTRO, ELO,
     :              STATUS )
      IF ( VARPRS ) CALL KPG1_MAP( NDFO, 'Variance', ITYPEV,
     :                            'WRITE/BAD', PNTROV, ELO, STATUS )
      IF ( QUAPRS ) CALL KPG1_MAP( NDFO, 'Quality', '_UBYTE',
     :                            'WRITE/ZERO', PNTROQ, ELO, STATUS )

*  Main loop to paste in the NDFs in turn.
*  =======================================

*  Pasting will apply to all the NDF array components in turn.  First
*  the data array.
      DO I = 1, NUMNDF

*  Ignore if the input NDF is outside the output NDF.
         IF ( .NOT. EXCLUD( I ) ) THEN

*  Derive the offsets of the original input NDFs with respect to the
*  origin of the output NDF.  Also extract the dimensions of the
*  current NDF.
            DO J = 1, NDF__MXDIM
               OFFSET( J ) = LBNDI( J, I ) - LBNDO( J )
               IDIMS( J ) = DIMSI( J, I )
            END DO

*  Paste the data array.
*  =====================

*  Map the input NDF data array.
            CALL KPG1_MAP( NDFI( I ), 'Data', ITYPE, 'READ', PNTRI, ELI,
     :                    STATUS )

*  Call the appropriate routine that performs the pasting of the data
*  array.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_PASTR( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPG1_PASTB( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_PASTD( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPG1_PASTI( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               CALL KPG1_PASTUB( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                           %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                           ODIMS, ELO,
     :                           %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                           STATUS )

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               CALL KPG1_PASTUW( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                           %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                           ODIMS, ELO,
     :                           %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                           STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL KPG1_PASTW( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

            END IF

*  Unmap the data array, as we may already have three other arrays
*  mapped.
            CALL NDF_UNMAP( NDFI( I ), 'Data', STATUS )

*  Paste the variance array.
*  =========================
            IF ( VARPRS ) THEN

*  Map the input NDF variance, obtaining an array of bad values if
*  there is no variance array in this NDF.
               CALL KPG1_MAP( NDFI( I ), 'Variance', ITYPE, 'READ/BAD',
     :                       PNTRI, ELI, STATUS )

*  Call the appropriate routine that performs the pasting of the data
*  array.
               IF ( ITYPEV .EQ. '_REAL' ) THEN
                  CALL KPG1_PASTR( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             ODIMS, ELO,
     :                             %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( ITYPEV .EQ. '_BYTE' ) THEN
                  CALL KPG1_PASTB( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             ODIMS, ELO,
     :                             %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( ITYPEV .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_PASTD( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             ODIMS, ELO,
     :                             %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( ITYPEV .EQ. '_INTEGER' ) THEN
                  CALL KPG1_PASTI( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             ODIMS, ELO,
     :                             %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( ITYPEV .EQ. '_UBYTE' ) THEN
                  CALL KPG1_PASTUB( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                              %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              ODIMS, ELO,
     :                              %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                              STATUS )

               ELSE IF ( ITYPEV .EQ. '_UWORD' ) THEN
                  CALL KPG1_PASTUW( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                              %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              ODIMS, ELO,
     :                              %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                              STATUS )

               ELSE IF ( ITYPEV .EQ. '_WORD' ) THEN
                  CALL KPG1_PASTW( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             ODIMS, ELO,
     :                             %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                             STATUS )
               END IF

*  Unmap the variance array, as we may already have three other arrays
*  mapped.
               CALL NDF_UNMAP( NDFI( I ), 'Variance', STATUS )
            END IF

*  Paste the quality array.
*  ========================
            IF ( QUAPRS ) THEN

*  Map the input NDF quality, which has type unsigned byte.  This will
*  be an array of zeroes if there is no quality array in this NDF.
               CALL KPG1_MAP( NDFI( I ), 'Quality', '_UBYTE',
     :                       'READ/ZERO', PNTRI, ELI, STATUS )

*  Call the routine that performs the pasting.
               CALL KPG1_PASTUB( TRANSP, BADQUA, OFFSET, IDIMS, ELI,
     :                           %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                           ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTROQ( 1 ) ) ),
     :                          STATUS )

*  Unmap the quality array, as we may already have three other arrays
*  mapped.
               CALL NDF_UNMAP( NDFI( I ), 'Quality', STATUS )
            END IF
         END IF

*  Release the input NDF identifier.
         CALL NDF_ANNUL( NDFI( I ), STATUS )
      END DO

*  Delete the group.
      CALL GRP_DELET( GRPIN, STATUS )

*  Tidy the NDF system.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PASTE_ERR',
     :      'PASTE: Unable to paste NDFs upon others.', STATUS )
      END IF

      END
