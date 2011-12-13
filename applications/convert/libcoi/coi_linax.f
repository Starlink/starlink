      SUBROUTINE COI_LINAX( IMDESC, NDF, USEDEF, BUFFER, STATUS )
*+
*  Name:
*     COI_LINAX

*  Purpose:
*     Creates axis centres within an NDF from FITS-axis information in
*     an IRAF header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_LINAX( IMDESC, NDF, USEDEF, BUFFER, STATUS )

*  Description:
*     The routine searches the IRAF headers for the keywords that
*     describe a linear axis structure.  At least one complete set of
*     keywords defining the axis must be present.  These keywords are
*     CRVALn, the reference value; CRPIXn, the reference pixel to which
*     the reference value corresponds; and CDELTn (the step size between
*     axis values), or CDi_j (the axis rotation/scaling matrix).  If
*     they all exist for axis n, then an axis component is created and
*     filled with the appropriate values.  If any of CRVALn, or CRPIXn,
*     or either CDELTn and CDn_n are absent, and USEDEF=.FALSE., the
*     missing keywords are assigned 1.0D0.  This option allows
*     conversion from the Multispec system, which omits these keywords.
*     When only some of the axes have defined centres, the remaining
*     axes are assigned pixel co-ordinates.  If CTYPEn is in the header
*     it is used to assign a value to the nth axis's label component.
*     If CUNITn is present, its value is stored in the nth axis's units
*     component.
*
*     The precision of the output axis-centre array depends on the
*     absolute relative size of the offset to the scale.  Single
*     precision is used if this ratio is greater than one hundred times
*     the floating-point precision.

*  Arguments:
*     IMDESC = INTEGER (Given)
*        The IMFORT file descriptor.
*     NDF = INTEGER (Given)
*        The identifier for the NDF to contain the axis structure.
*     USEDEF = LOGICAL (Given)
*        Whether or not to use default values of CRVALn, CRPIXn, and
*        CDELTn/CDn_n if any are absent.  This should currently only be
*        set to .TRUE. when the MWCS system is Equispec.
*     BUFFER = CHARACTER * ( * ) (Returned)
*        Workspace to store concatenated value from multi-line headers
*        WATd_nnn.  In this case it is used to obtain the axis labels
*        and units.  It should be at least 68 characters long (for a
*        single line), and to be on the safe side, tens of times this.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The axis structure is deleted if not all the axes have
*     defined axis centres.

*  Deficiencies:
*     Does not handle log-linear case by checking DC-FLAG keyword.

*  Copyright:
*     Copyright (C) 1997-1998, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2008 Science & Technology Facilities
*     Council. All Rights Reserved.

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
*     1997 July 24 (MJC):
*        Original version.
*     26-JAN-1998 (DSB):
*        Modified to take account of CD matrices which indicate a
*        rotation of 90 degrees.
*     6-FEB-1998 (DSB):
*        Modified to ignore CDELT if CD is found.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2008 March 15 (MJC):
*        Use KAPLIBS routines instead of their cloned CON equivalents.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IMDESC
      INTEGER NDF
      LOGICAL USEDEF

*  Arguments Returned:
      CHARACTER * ( * ) BUFFER

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string less
                                 ! trailing blanks

*  Local Constants:
      INTEGER IMOK               ! IMFORT OK status
      PARAMETER( IMOK = 0 )

      DOUBLE PRECISION PRECF     ! The factor times the machine
                                 ! precision above which the ratio of
                                 ! the scale factor to offset must lie
                                 ! for single-precision axis centres
                                 ! to be created
      PARAMETER ( PRECF = 100.0D0 )

      DOUBLE PRECISION EPS       ! Tan of the largest angle allowable
                                 ! between axes before they are
                                 ! considered not to be parallel
      PARAMETER ( EPS = 1.0E-6 )


*  Local Variables:
      INTEGER BUFLEN             ! Character length of the multi-line
                                 ! buffer for WATd_nnn headers
      DOUBLE PRECISION CD_I( NDF__MXDIM ) ! CDi_j values
      DOUBLE PRECISION CDELTI    ! CDELT value
      CHARACTER * ( 1 ) CNDIM    ! The axis number
      DOUBLE PRECISION DELT      ! The co-ordinate increment between
                                 ! pixels
      LOGICAL DFAXIS             ! At least one axis-centre array
                                 ! defined?
      INTEGER DIMS( NDF__MXDIM ) ! The dimensions of the NDF
      INTEGER DTYPE              ! Data code (0=linear,1=log-linear)
      INTEGER EL                 ! Dimension of the current axis
      INTEGER ERR                ! IMFORT status
      LOGICAL GCD                ! Got at least one CDi_j value?
      LOGICAL GCRVAL             ! Got a CRVAL value?
      LOGICAL GCRPIX             ! Got a CRPIX value?
      LOGICAL GCDELT             ! Got a CDELT value?
      INTEGER I                  ! Axis index in IRAF WCS
      INTEGER IAT                ! Index at which to insert next
                                 ! character
      INTEGER J                  ! Axis index in NDF pixel co-ordinate
                                 ! system
      INTEGER JMAX               ! Index of NDF axis parallel to current
                                 ! IRAF axis
      CHARACTER * ( DAT__SZTYP ) ITYPE ! Data type of the axis centres
      CHARACTER * ( 8 ) KEYWRD   ! FITS keyword
      CHARACTER * ( 70 ) LABEL   ! Axis label
      DOUBLE PRECISION MAXCD     ! Largest CDi_j value found for current
                                 ! axis
      INTEGER NC                 ! Number of characters in keyword
      INTEGER NCL                ! Number of characters in label
      INTEGER NCU                ! Number of characters in units
      INTEGER NDIM               ! Number of dimensions of the NDF
      LOGICAL NONPAR             ! Were any non-parallel axes found?
      INTEGER NSDIG              ! Number of significant digits
      DOUBLE PRECISION OFFSET    ! Offset after allowing for the
                                 ! position of reference pixel
      INTEGER PNTR( 1 )          ! Pointer the mapped CENTRE component
      DOUBLE PRECISION REFP      ! Pixel position of the reference pixel
      DOUBLE PRECISION REFV      ! Co-ordinate at the reference pixel
      LOGICAL THERE              ! The nominated FITS keyword is present
                                 ! in the header?
      CHARACTER * ( 70 ) UNITS   ! Axis units
      CHARACTER * ( 70 ) VALUE   ! Keyword value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Obtain the dimensions of the NDF.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Create a default axis system in case only some of the FITS axes are
*  defined in the headers.
      CALL NDF_ACRE( NDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Initialise flag to indicate whether any axis has been written.
      DFAXIS = .FALSE.

*  Obtain the DC-FLAG which indicates if the data are linear, log-linear
*  or non-linear.
      CALL IMGKWI( IMDESC, 'DC-FLAG', DTYPE, ERR )
      IF ( ERR .NE. IMOK ) DTYPE = 0

*  Loop for each axis in the IRAF world co-ordinate system.  This is
*  assumed to be the same as the number of axes in the NDF.
      DO I = 1, NDIM

*  Read the header for the four axis parameters.
*  =============================================
*
*  Note rotation by angles other than 90 degrees cannot be mapped on to
*  an axis structure and requires a more-sophisticated astrometric
*  system.

*  Create the extension to the keyword name for the current dimension.
         CALL CHR_ITOC( I, CNDIM, NC )

*  Obtain the value of the physical co-ordinate at the reference pixel.
         KEYWRD = ' '
         KEYWRD = 'CRVAL'//CNDIM( :NC )
         CALL IMGKWD( IMDESC, KEYWRD, REFV, ERR )
         GCRVAL = ERR .EQ. IMOK

*  If not found, use a default of 1.0 for CRVAL if required.
         IF ( .NOT. GCRVAL .AND. USEDEF ) REFV = 1.0D0

*  Initialise the largest absolute CD value found so far for the current
*  world co-ordinate axis.
         MAXCD = -1.0

*  Go through each pixel axis (i.e. each axis of the NDF) to find the
*  one which is most nearly parallel to the current world co-ordinate
*  axis.  This is taken to be the axis with the largest absolute CDi_j
*  value.
         GCD = .FALSE.
         DO J = 1, NDIM

*  Get the value of CDi_j (the rate of change of world co-ordinate axis
*  i with respect to pixel axis j).
            KEYWRD = 'CD'
            IAT = 2
            CALL CHR_PUTI( I, KEYWRD, IAT )
            CALL CHR_APPND( '_', KEYWRD, IAT )
            CALL CHR_PUTI( J, KEYWRD, IAT )
            CALL IMGKWD( IMDESC, KEYWRD, CD_I( J ), ERR )
            THERE = ERR .EQ. IMOK

*  If CDi_j was not found, use a default of 1.0 for diagonal terms and
*  zero for off diagonal.
            IF ( .NOT. THERE ) THEN
               IF ( I .EQ. J ) THEN
                  CD_I( J ) = 1.0
               ELSE
                  CD_I( J ) = 0.0
               END IF

*  Note if any CD matrix elements were found.
            ELSE
               GCD = .TRUE.
            END IF

*   Note the largest CD term for this world axis.
            IF ( ABS( CD_I( J ) ) .GT. MAXCD ) THEN
               MAXCD = ABS( CD_I( J ) )
               JMAX = J
            END IF

         END DO

*  We now have the index of the NDF axis which contributes most to the
*  current world co-ordinate axis. See if any other NDF axes make
*  significant contributions to it.
         IF ( GCD ) THEN
            DO J = 1, NDIM
               IF ( J .NE. JMAX ) THEN

*  If we find another significant axis, the world co-ordinate axes are
*  not parallel to the NDF axes. In this case, leave the loop without
*  creating any further AXIS structures. EPS should be a small constant
*  value, but how small?
                  NONPAR = ( ABS( CD_I( J ) ) .GT. MAXCD*EPS )
                  IF ( NONPAR ) GO TO 10

               END IF
            END DO
         END IF

*  We arrive here only if the CD matrix indicates that the NDF and world
*  co-ordinate axes are parallel.

*  If no CD matrix was obtained, attempt to get a value for CDELTi.
*  This is a scaling factor for values on the i'th world co-ordinate
*  axis.
         IF ( .NOT. GCD ) THEN
            KEYWRD = 'CDELT'//CNDIM( :NC )
            CALL IMGKWD( IMDESC, KEYWRD, CDELTI, ERR )
            GCDELT = ERR .EQ. IMOK
         ELSE
            GCDELT = .FALSE.
         END IF

*  Use a default of 1.0 for CDELT if required.  Note, if a CD matrix
*  was found, then we can default CDELT even if USEDEF is .FALSE.
*  since the CD matrix implies a CDELT value.
         IF ( .NOT. GCDELT .AND. ( USEDEF .OR. GCD ) ) CDELTI = 1.0D0

*  Find the total scaling term from pixels on NDF axis JMAX to world
*  co-ordinates on axis I.
         DELT = CDELTI * CD_I( JMAX )

*  Obtain the value of the element number of the reference pixel on the
*  NDF axis parallel to the current world axis.
         KEYWRD = 'CRPIX'
         IAT = 5
         CALL CHR_PUTI( JMAX, KEYWRD, IAT )
         CALL IMGKWD( IMDESC, KEYWRD, REFP, ERR )
         GCRPIX = ERR .EQ. IMOK

*  If not found, use a default of 1.0 for CRPIX if required.
         IF ( .NOT. GCRPIX .AND. USEDEF ) REFP = 1.0D0

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Decide the data type of the axis centres.
*  =========================================
         IF ( ( GCRVAL .AND. GCRPIX .AND. ( GCD .OR. GCDELT ) )
     :        .OR. USEDEF ) THEN

*  Set the flag to indicate that at least one axis-centre array has been
*  defined.
            DFAXIS = .TRUE.

*  Find the offset for a scale-and-offset calculation of the axis
*  information.
            OFFSET = REFV - ( REFP - 1.0D0 ) * DELT

*  Determine the data type of the axis centres when there is only a
*  constant value.
            IF ( ABS( DELT ) .LT. VAL__SMLD ) THEN

*  Obtain the value as a string.  There is no need to test for an error
*  because it would have been detected earlier.
               IF ( GCRVAL ) THEN
                  KEYWRD = 'CRVAL'//CNDIM( :NC )
                  CALL IMGKWC( IMDESC, KEYWRD, VALUE, ERR )
               ELSE
                  VALUE = '1.0'
               END IF

*  Find the number of significant digits in the numerical value.
               CALL KPG1_SGDIG( VALUE, NSDIG, STATUS )

*  Determine the appropriate type by comparing the number of
*  significant digits present with the maximum number of significant
*  digits afforded by a real number.
               IF ( NSDIG .LE. -INT( LOG10( VAL__EPSR ) ) ) THEN
                  ITYPE = '_REAL'
               ELSE
                  ITYPE = '_DOUBLE'
               END IF

*  Define the criterion for deciding whether or not single-precision
*  axis centres are adequate.  It is important that the increments
*  between elements are greater than the floating-point precision.  To
*  reduce possible errors the fractional difference is scaled by a
*  constant.  However, the values may conspire to give a zero offset
*  (e.g. US-style co-ordinates of pixels).  A zero ratio is tested
*  also, since the type required for a constant depends on the number of
*  significant figures.
            ELSE
               IF ( ABS( OFFSET ) .LT. VAL__EPSD ) THEN
                  ITYPE = '_REAL'
               ELSE

                  IF ( ABS( DELT / OFFSET ) .GT. DBLE( VAL__EPSR ) *
     :                 PRECF ) THEN
                     ITYPE = '_REAL'
                  ELSE
                     ITYPE = '_DOUBLE'
                  END IF
               END IF
            END IF

*  Fill the axis centres.
*  ======================
            IF ( ITYPE .EQ. '_REAL' ) THEN

*  Map the centre array in the axis structure.
               CALL NDF_AMAP( NDF, 'Centre', JMAX, '_REAL', 'WRITE',
     :                        PNTR, EL, STATUS )

*  Test status before accessing the pointer.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL KPG1_SSAZR( EL, DELT, OFFSET,
     :                             %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                             STATUS )

*  Exponentiate a log-linear axis.
                  IF ( DTYPE .EQ. 1 )
     :              CALL COI_ALOGR( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                              STATUS )

*  Unmap the axis array.
                  CALL NDF_AUNMP( NDF, 'Centre', JMAX, STATUS )
               END IF

*  Double precision is required.
            ELSE

*  Ensure that the type is double precision.
               CALL NDF_ASTYP( '_DOUBLE', NDF, 'Centre', JMAX, STATUS )

*  Map the centre array in the axis structure.
               CALL NDF_AMAP( NDF, 'Centre', JMAX, '_DOUBLE', 'WRITE',
     :                        PNTR, EL, STATUS )

*  Test status before accessing the pointer.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL KPG1_SSAZD( EL, DELT, OFFSET,
     :                             %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                             STATUS )

*  Exponentiate a log-linear axis.
                  IF ( DTYPE .EQ. 1 )
     :              CALL COI_ALOGD( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                              STATUS )

*  Unmap the axis array.
                  CALL NDF_AUNMP( NDF, 'Centre', JMAX, STATUS )
               END IF
            END IF

*  Obtain the axis label and units from `standard' headers.
*  ========================================================

*  Obtain the label of the axis.  The label is not mandatory, so the
*  presence flag will only determine whether or not the axis label
*  component is written.
            KEYWRD = 'CTYPE'//CNDIM( :NC )
            CALL IMGKWC( IMDESC, KEYWRD, LABEL, ERR )

            IF ( ERR .EQ. IMOK .AND. LABEL .NE. ' ' ) THEN

*  Find the length of the label.
               NCL = CHR_LEN( LABEL )

*  Write the label to the axis structure, using only the length actually
*  required.
               CALL NDF_ACPUT( LABEL( :NCL ), NDF, 'Label', JMAX,
     :                         STATUS )
            END IF

*  Obtain the units of the axis.  Axis units is not part of the FITS
*  standard yet, but the CUNITn keywords are widely used and they are
*  in the WCS proposal.  The presence flag will only determine whether
*  or not the axis units component is written.
            KEYWRD = 'CUNIT'//CNDIM( :NC )
            CALL IMGKWC( IMDESC, KEYWRD, UNITS, ERR )

            IF ( ERR .EQ. IMOK .AND. UNITS .NE. ' ' ) THEN

*  Find the length of the units.
               NCU = CHR_LEN( UNITS )

*  Write the units to the axis structure.
               CALL NDF_ACPUT( UNITS( :NCU ), NDF, 'Units', JMAX,
     :                         STATUS )
            END IF

*  Obtain the axis label and units from IRAF MWCS headers.
*  =======================================================

*  Concatenate the WAT<dim>_nnn header, which may contain the axis label
*  and units.
            CALL ERR_MARK
            CALL COI_WCCAT( IMDESC, I, BUFFER, BUFLEN, STATUS )

*  Extract the label.  Watch for a null string.
            IF ( STATUS .EQ. SAI__OK .AND. BUFLEN .GT. 1 ) THEN

*  Extract the label.
               CALL COI_WCWRD( BUFFER( :BUFLEN ), 'label', LABEL,
     :                         THERE, STATUS )

*  If a label is present, write it to the NDF AXIS strucutre for the
*  current axis.  Pass only the used length not to waste space in the
*  NDF.
               IF ( THERE ) THEN
                  NCL = CHR_LEN( LABEL )
                  CALL NDF_ACPUT( LABEL( :NCL ), NDF, 'Label', JMAX,
     :                            STATUS )
               END IF

*  Extract the units.
               CALL COI_WCWRD( BUFFER( :BUFLEN ), 'units', UNITS, THERE,
     :                         STATUS )

*  If units value is present, write it to the NDF AXIS strucutre for
*  the current axis.  Pass only the used length not to waste space in
*  the NDF.
               IF ( THERE ) THEN
                  NCU = CHR_LEN( UNITS )
                  CALL NDF_ACPUT( UNITS( :NCU ), NDF, 'Units', JMAX,
     :                            STATUS )
               END IF

*  It is not serious if we cannot obtain the header card, as the
*  character components of the axis may have been provided
*  through`standard' FITS keywords.
            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF
            CALL ERR_RLSE

*  End of a check for a physical co-ordinate.
         END IF

*  End of the loop for each dimension.
      END DO

*  Delete the axis structure if it is just the default created earlier.
  10  CONTINUE
      IF ( NONPAR .OR. .NOT. DFAXIS ) CALL NDF_RESET( NDF, 'Axis',
     :                                                STATUS )

  999 CONTINUE

*  Close the new NDF context.
      CALL NDF_END( STATUS )

      END
