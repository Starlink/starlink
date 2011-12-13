      SUBROUTINE COI_AXEXP( NDF, IMDESC, SYSTEM, STATUS )
*+
*  Name:
*     COI_AXEXP

*  Purpose:
*     Writes IRAF headers derived from axis components of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_AXEXP( NDF, IMDESC, SYSTEM, STATUS )

*  Description:
*     This routine writes IRAF Mini World Co-ordinate System (MWCS)
*     headers to an OIF file.  The values for the headers are derived
*     for the axis centres, labels and units in the supplied NDF.  It
*     uses MWCS linear, equispec or multispec systems where the axis
*     centres conform to the rules of these formats.  See the Notes for
*     more details.  The system used is returned.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     IMDESC = INTEGER (Given)
*        The IRAF IMFORT image descriptor.
*     SYSTEM = CHARACTER * ( * ) (Returned)
*        The MWCS co-ordinate system used.  It can have the following
*        values and meanings
*           'NONE'      --- no MWCS co-ordinate system was used;
*           'REGULAR'   --- standard CRVARn, CRPIXn, CDELTn were used;
*           'EQUISPEC'  --- used the IRAF equispec co-ordinate system
*           'MULTISPEC' --- used the IRAF multispec co-ordinate system
*        The length of SYSTEM must be at least 9 characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The IRAF Mini World Co-ordinate System (MWCS) is used to
*     record axis information whenever one of the following criteria
*     is satisfied:
*
*       1) the dataset has some linear axes;
*
*       2) the dataset is one-dimensional with a non-linear axis;
*
*       3) the dataset has a linear dispersion axis along the first
*          dimension and all other dimensions are pixel indices.
*
*     For case 1) the routine writes the regular CRPIXi, CDELTi,
*     CRVALi, and CDi_i keywords, as well as WAT0_nnn (system=world)
*     and WATi_nnn (wtype=linear) cards, where i is the axis number.
*     CDi_i equals CDELTi.  By definition the reference pixel is one
*     and the scale and offset are derived for each linear axis.  Axis
*     label and units are written to the WATi_nnn keyword.  The CTYPEi
*     keyword is set to 'LINEAR'.
*
*     For case 3), the routine behaves as for case 1) except the system
*     is equispec.  The dispersion axis is deemed to be one with the
*     case-insensitive axis label being one of the following options:
*     "LAMBDA", "FREQ", "WAVELENGTH", "VELO", "VELO-LSR", "VELO-HEL",
*     "VELO-OBS".
*
*     For case 2), the routine writes CDELTi being an approximation
*     to the average interval, and by definition CDi_i set to 1.
*     CTYPEn is set to "MULTISPE'. WAT1_nnn contains wtype=multispec
*     and any axis label and units.  WAT2__nnn contains the explicit
*     wavelengths in multispec specN format.
*
*     LTVi and LTMi_i keywords are *not* written.  WCSDIM keyword is
*     written, being set to two for multispec.  DC-FLAG is also written
*     for non-multispec format.  Keyword WAXMAP01 is set to 1 0 0 1 "
*     for multispec.

*  Copyright:
*     Copyright (C) 1997, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. Copyright (C) 2008, 2009 Science & Technology
*     Facilities Council. All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1997 March 28 (MJC):
*        Original version.
*     1997 April-May  (MJC):
*        Various fixes as learnt more about MWCS.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 13 (MJC):
*        Remove unused variables.
*     2008 March 15 (MJC):
*        Use KAPLIBS routines instead of their cloned CON equivalents.
*     2009 June 29 (MJC):
*        Replace cloned CON_FKEYx with KAPLIBS FTS1_WKEYx.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF                ! NDF identifier
      INTEGER IMDESC             ! Logical-unit number of FITS file

*  Arguments Returned:
      CHARACTER * ( * ) SYSTEM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! String length sans trailing blanks

*  Local Constants:
      INTEGER IMOK               ! Good status for IMFORT
      PARAMETER( IMOK = 0 )

      INTEGER SZKEY              ! Length of keywords
      PARAMETER( SZKEY = 8 )

      INTEGER SZVAL              ! Maximum length of header values
      PARAMETER( SZVAL = 60 )

*  Local Variables:
      LOGICAL ANYLIN             ! At least one of the axes is linear?
      INTEGER APNTR( NDF__MXDIM ) ! Pointers to NDF axis arrays
      CHARACTER * ( NDF__SZTYP ) ATYPE( NDF__MXDIM ) ! Data types of the
                                 ! axis-centre arrays
      LOGICAL AXLFND             ! NDF contains axis label?
      LOGICAL AXUFND             ! NDF contains axis units?
      CHARACTER * ( 1 ) C        ! Axis number
      CHARACTER * ( 3 ) CA       ! Aperture number
      CHARACTER * ( 4 ) CN       ! 1000 plus line number
      INTEGER CPOS               ! Column position for concatening
                                 ! strings
      DOUBLE PRECISION DEND( NDF__MXDIM ) ! End values for axis-centre arrays
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      DOUBLE PRECISION DINCRE( NDF__MXDIM ) ! Incremental values for
                                 ! axis arrays
      LOGICAL DISPAX             ! Dispersion axis present?
      DOUBLE PRECISION DSTART( NDF__MXDIM ) ! Start values for an
                                 ! axis-centre arrays
      REAL END( NDF__MXDIM )     ! End values for an axis-centre arrays
      CHARACTER HEADER * ( 80 )  ! FITS-like header
      INTEGER HEADNO             ! WATi_nnn header counter
      LOGICAL HIGLIN             ! Higher axes are pixels or apertures?
      INTEGER I                  ! Loop variable
      INTEGER IAXIS              ! Axis number
      REAL INCREM( NDF__MXDIM )  ! Incremental values for axis arrays
      INTEGER J                  ! Loop counter
      CHARACTER KEYWRD * ( SZKEY ) ! Accommodates keyword name
      LOGICAL LINEAR( NDF__MXDIM ) ! Axes are linear?
      INTEGER NC                 ! Number of characters
      INTEGER NCHAR              ! Length of a character string
      INTEGER NDIM               ! Number of dimensions
      INTEGER NELM               ! Number of elements
      REAL START( NDF__MXDIM )   ! Start values for an axis structures
      LOGICAL THERE              ! NDF has FITS extension?
      CHARACTER VALUE * ( SZVAL ) ! Accommodates keyword value
      INTEGER WAVEAX             ! Dispersion/spectral axis

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the number of dimensions.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Initialise variables.
*  =====================
      WAVEAX = 0
      IAXIS = 1
      DISPAX = .FALSE.
      AXLFND = .FALSE.

*  Find the first wavelength axis.
*  ===============================

*  Use the label to determine whether or not an axis is a wavelength
*  scale.

*  Start of `DO WHILE' loop to find the first, if any, wavelength (or
*  equivalent) axis.
  100 CONTINUE
      IF ( .NOT. DISPAX .AND. IAXIS .LE. NDIM ) THEN

*  See whether an axis label is present or not.
         CALL NDF_ASTAT( NDF, 'Label', IAXIS, AXLFND, STATUS )
         IF ( AXLFND ) THEN

*  Obtain the label's value and length.
            CALL NDF_ACGET( NDF, 'Label', IAXIS, VALUE, STATUS )
            CALL NDF_ACLEN( NDF, 'Label', IAXIS, NCHAR, STATUS )

*  Remove unprintable characters that are not permitted in FITS.
            CALL CHR_CLEAN( VALUE )
            CALL CHR_UCASE( VALUE )

*  Is this a wavelength axis?
            DISPAX = VALUE .EQ. 'WAVELENGTH' .OR.
     :               VALUE .EQ. 'LAMBDA' .OR.
     :               VALUE(1:4) .EQ. 'FREQ' .OR.
     :               VALUE .EQ. 'VELO' .OR.
     :               VALUE .EQ. 'VELO-LSR' .OR.
     :               VALUE .EQ. 'VELO-HEL' .OR.
     :               VALUE .EQ. 'VELO-OBS'

            IF ( DISPAX ) WAVEAX = IAXIS
         END IF

*  Skip to the next axis.
         IAXIS = IAXIS + 1

*  Loop to start of the `DO WHILE' loop.
         IF ( .NOT. DISPAX .AND. IAXIS .LE. NDIM ) GOTO 100
      END IF

*  Determine whether or not axes are linear.
*  =========================================

*  Initialise flag to determine if any axis is linear.
      ANYLIN = .FALSE.

*  First determine which, if any, of the axes is linear.  For linear
*  axes calculate a scale and offset.
      DO I = 1, NDIM
         CALL NDF_ASTAT( NDF, 'Centre', I, THERE, STATUS )

         IF ( THERE ) THEN

*  Determine the data type of the axis array.
            CALL NDF_ATYPE( NDF, 'Centre', I, ATYPE( I ), STATUS )

*  The axis structure is found, so map it using an appropriate data
*  type.  Use _REAL for all but double-precision centres.  See if the
*  axis is linear.
            IF ( ATYPE( I ) .EQ. '_DOUBLE' ) THEN
               CALL NDF_AMAP( NDF, 'Centre', I, '_DOUBLE', 'READ',
     :                        APNTR( I ), NELM, STATUS )

               IF ( NELM .GT. 1 ) THEN
                  CALL KPG1_AXLID( NELM, %VAL( CNF_PVAL( APNTR( I ) ) ),
     :                             DSTART( I ), DEND( I ), LINEAR( I ),
     :                             STATUS )

*  We can ignore bad status, but then we assume a non-linear axis.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     LINEAR( I ) = .FALSE.
                  END IF

*  Derive the increment between values.  This is only an approximation
*  for non-linear axes, but it is required by the MULTISPEC headers.
                  DINCRE( I ) = ( DEND( I ) - DSTART( I ) ) /
     :                          DBLE( NELM - 1 )

*  Deal with the special case of a one-element axis.  Just obtain the
*  start value.
               ELSE
                  DINCRE( I ) = 1.0D0
                  CALL KPG1_AXBND( NELM, %VAL( CNF_PVAL( APNTR( I ) ) ),
     :                             DSTART( I ), DEND( I ), STATUS )
               END IF

*  Repeat for all other axis-centre data types mapped as real.
            ELSE
               CALL NDF_AMAP( NDF, 'Centre', I, '_REAL', 'READ',
     :                        APNTR( I ), NELM, STATUS )

               IF ( NELM .GT. 1 ) THEN
                  CALL KPG1_AXLIR( NELM, %VAL( CNF_PVAL( APNTR( I ) ) ),
     :                             START( I ), END( I ), LINEAR( I ),
     :                             STATUS )

*  We can ignore bad status, but then we assume a non-linear axis.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     LINEAR( I ) = .FALSE.
                  END IF

*  Derive the increment between values.  This is only an approximation
*  for non-linear axes, but it is required by the MULTISPEC headers.
                  INCREM( I ) = ( END( I ) - START( I ) ) /
     :                          REAL( NELM - 1 )

*  Deal with the special case of a one-element axis.  Just obtain the
*  start value.
               ELSE
                  INCREM( I ) = 1.0
                  CALL KPG1_AXBNR( NELM, %VAL( CNF_PVAL( APNTR( I ) ) ),
     :                             START( I ), END( I ), STATUS )
               END IF
            END IF
         END IF

*  Record if any of the axes is linear.
         ANYLIN = ANYLIN .OR. LINEAR( I )
      END DO

*  Decide which axis-recording system to be used.
*  ==============================================

*  Can cope with:
*     a dataset that has some linear axes using the regular CRPIXn,
*     CDELTn, and CRVALn keywords;

*     a one-dimensional dataset with a non-linear axis or a
*     two-dimensional NDF with the first axis non-linear and the second
*     being some aperture number or index; or

*     "spatial" datasets where the first axis is linear and higher axes
*     are pixel indices.

*  The first uses slightly modified and extended FITS keywords to
*  define the co-ordinate system (system=world).  The second and third
*  require the MULTISPEC and EQUISPEC formats respectively to store the
*  axis information in the headers.  MULTISPEC demands a two-dimensional
*  co-ordinate system.

*  Initialise output system.  This corresponds to not recording the
*  axis.  Other options are 'EQUISPEC' for IRAF equispec, 'MULTISPEC'
*  for IRAF multispec, and 'REGULAR' for traditional FITS axis
*  keywords.
      SYSTEM = 'NONE'

*  First check is whether the first axis is wavelength.
      IF ( WAVEAX .EQ. 1 ) THEN

*  Assume for the moment that any higher dimensions are linear at pixel
*  intervals.
         HIGLIN = .TRUE.

*  Are there higher dimensions?
         IF ( NDIM .GT. 1 ) THEN

*  Loop through the higher dimensions.
            DO J = 2, NDIM

*  For each array type check whether or not the co-ordinates are pixel
*  co-ordinates or indices.  The equispec and multispec formats require
*  that the higher (non-wavelength) dimensions are just some
*  observation or line index.  So the increment should be 1, and double
*  the co-ordinates should be integer.  This might need some fuzzy
*  logic to allow for rounding.
               IF ( ATYPE( J ) .EQ. '_DOUBLE' ) THEN
                  HIGLIN = HIGLIN .AND. DINCRE( J ) .EQ. 1.0D0 .AND.
     :                     MOD( DSTART( J ) * 2.0D0, 1.0D0 ) .EQ. 0.0D0

               ELSE
                  HIGLIN = HIGLIN .AND. INCREM( J ) .EQ. 1.0 .AND.
     :                     MOD( START( J ) * 2.0, 1.0 ) .EQ. 0.0

               END IF
            END DO

*  If the higher axes are line numbers, then the system is one of the
*  two IRAF special definitions.  Otherwise use the standard method
*  when any of the axes is linear.
            IF ( HIGLIN ) THEN
               IF ( LINEAR( 1 ) ) THEN
                  SYSTEM = 'EQUISPEC'
               ELSE
                  IF ( ANYLIN ) THEN
                     SYSTEM = 'REGULAR'
                  END IF
               END IF

            ELSE
               IF ( ANYLIN ) SYSTEM = 'REGULAR'
            END IF

*  If the dataset is one-dimensional and non-linear the system is
*  multispec.  However the definition demands that the dataset be
*  two-dimensional.  Otherwise use the standard method.
         ELSE
            IF ( LINEAR( 1 ) ) THEN
               SYSTEM = 'REGULAR'
            ELSE
               SYSTEM = 'MULTISPEC'
               NDIM = 2
               ATYPE( NDIM ) = ATYPE( 1 )
            END IF
         END IF

*  If the first axis is not the wavelength use the regular keywords
*  whenever any of the axes is linear.  If the dataset is
*  one-dimensional and non-linear the system is multispec (after
*  converting to two-dimensions to follow the multispec rules).
*  A two-dimensional data with a non-linear first axis and pixel index
*  second axis is also multispec
      ELSE
         IF ( LINEAR( 1 ) ) THEN
            SYSTEM = 'REGULAR'

         ELSE IF ( NDIM .EQ. 1 ) THEN
            SYSTEM = 'MULTISPEC'
            NDIM = 2

         ELSE IF ( NDIM .EQ. 2 ) THEN
            IF ( LINEAR( 2 ) ) THEN
               SYSTEM = 'REGULAR'
            END IF
         END IF
      END IF

*  Record the global headers.
*  ==========================

*  There are no headers to write if the axis system cannot be
*  accommodated by the IRAF MWCS.
      IF ( SYSTEM .NE. 'NONE' ) THEN

*  Create the WCS number of dimensions header, WCSDIM.  It has no
*  comment.
         CALL FTS1_WKEYI( 'WCSDIM', NDIM, ' ', ' ', HEADER, STATUS )
         IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Create the DC-FLAG header if there is a linear wavelength axis.  It
*  has no comment.  Its value is 0 for a linear axis, and 2 for a
*  non-linear axis.
         IF ( WAVEAX .GT. 0 ) THEN
            IF ( LINEAR( WAVEAX ) ) THEN
               CALL FTS1_WKEYI( 'DC-FLAG', 0, ' ', ' ', HEADER, STATUS )
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )
            ELSE IF ( SYSTEM .NE. 'MULTISPEC' ) THEN
               CALL FTS1_WKEYI( 'DC-FLAG', 2, ' ', ' ', HEADER, STATUS )
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )
            END IF
         END IF

*  Specify the dimensional reduction card.
         IF ( SYSTEM .EQ. 'MULTISPEC' ) THEN
            CALL FTS1_WKEYC( 'WAXMAP01', '1 0 0 1 ', ' ', ' ',
     :                       .FALSE., HEADER, STATUS )
            IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )
         END IF

*  Specify the type of co-ordinate system.
         IF ( SYSTEM .EQ. 'REGULAR' ) THEN

*  Create the header (there is no comment) and append it to the headers.
            CALL COI_FKEYC( 'WAT0_001', 'system=linear ', ' ', ' ',
     :                      .FALSE., HEADER, STATUS )

         ELSE IF ( SYSTEM .EQ. 'EQUISPEC' ) THEN
            CALL COI_FKEYC( 'WAT0_001', 'system=equispec ', ' ', ' ',
     :                      .FALSE., HEADER, STATUS )

         ELSE IF ( SYSTEM .EQ. 'MULTISPEC' ) THEN
            CALL COI_FKEYC( 'WAT0_001', 'system=multispec ', ' ',
     :                      ' ', .FALSE., HEADER, STATUS )

         END IF
         IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Append the APNUMi header.
*  =========================

*  Equispec format appears to need the old APNUM headers albeit some
*  token value for each element along the second axis.  The first number
*  is the aperture value; it must be distinct.
         IF ( SYSTEM .EQ. 'EQUISPEC' ) THEN
            DO I = 1, DIMS( 2 )
               CALL CHR_ITOC( I, CA, NCHAR )
               CALL FTS1_WKEYC( 'APNUM'//CA, CA( :NCHAR )//' 1',
     :                          ' ', ' ', .FALSE., HEADER, STATUS )
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )
            END DO
         END IF

*  Create headers for each axis.
*  =============================
         DO I = 1, NDIM

*  Start the WATi_nnn cards from nnn=001.
            HEADNO = 1

*  Make a character versiuon of the axis number.
            CALL CHR_ITOC( I, C, NCHAR )

*  Start the WATi_nnn header.
*  ==========================
*
*  This records the axis type, label and units along axis i.  The
*  values may span multiple lines, hence the nnn which starts at 001
*  and increments by one for each line.
*
*  Start with the axis type.
            IF ( SYSTEM .NE. 'MULTISPEC' ) THEN

*  Create the keyword.
               KEYWRD = 'WAT'//C//'_'
               CPOS = 5
               CALL CHR_ITOC( 1000 + HEADNO, CN, NC )
               CALL CHR_APPND( CN( 2:4 ), KEYWRD, CPOS )

*  Write the axis type to the WATi_nnn header.
               CALL COI_FKEYC( KEYWRD, 'wtype=linear ',
     :                         ' ', ' ', .FALSE., HEADER, STATUS )
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Increment the header number for the next WAT1_nnn card.
               HEADNO = HEADNO + 1

*  Append the axis label to the WATi_nnn keyword.
*  ==============================================

*  See whether an axis label is present or not.
               CALL NDF_ASTAT( NDF, 'Label', I, AXLFND, STATUS )
               IF ( AXLFND ) THEN

*  Obtain the label's value.
                  CALL NDF_ACGET( NDF, 'Label', I, VALUE, STATUS )

*  Remove unprintable characters that are not permitted in FITS.
                  CALL CHR_CLEAN( VALUE )
                  NCHAR = CHR_LEN( VALUE )

*  Create the keyword.
                  KEYWRD = 'WAT'//C//'_'
                  CPOS = 5
                  CALL CHR_ITOC( 1000 + HEADNO, CN, NC )
                  CALL CHR_APPND( CN( 2:4 ), KEYWRD, CPOS )

*  Write the label to the WATi_nnn header.
                  CALL COI_FKEYC( KEYWRD, 'label="'/
     :                            /VALUE( :MIN( NCHAR, SZVAL ) )//'" ',
     :                            ' ', ' ', .FALSE., HEADER, STATUS )
                  IF ( STATUS .EQ. SAI__OK )
     :              CALL ADLINE( IMDESC, HEADER )

*  Increment the header number for the next WAT1_nnn card.
                  HEADNO = HEADNO + 1

               END IF

*  Append the axis units to the WATi_nnn keyword.
*  ==============================================

*  See whether an axis units is present or not.
               AXUFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Units', I, AXUFND, STATUS )
               IF ( AXUFND ) THEN

*  Obtain the units' value.
                  CALL NDF_ACGET( NDF, 'Units', I, VALUE, STATUS )

*  Remove unprintable characters that are not permitted in FITS.
                  CALL CHR_CLEAN( VALUE )
                  NCHAR = CHR_LEN( VALUE )

*  Create the keyword.
                  KEYWRD = 'WAT'//C//'_'
                  CPOS = 5
                  CALL CHR_ITOC( 1000 + HEADNO, CN, NC )
                  CALL CHR_APPND( CN( 2:4 ), KEYWRD, CPOS )

*  Write the units to the WATi_nnn header.
                  CALL COI_FKEYC( KEYWRD, 'units="'/
     :                            /VALUE( :MIN( NCHAR, SZVAL ) )//'" ',
     :                            ' ', ' ', .FALSE., HEADER, STATUS )
                  IF ( STATUS .EQ. SAI__OK )
     :              CALL ADLINE( IMDESC, HEADER )

*  Increment the header number for the next WAT1_nnn card.
                  HEADNO = HEADNO + 1

               END IF

*  Write the CTYPEn header.
*  ========================

*  Form the keyword name.
               KEYWRD = 'CTYPE'//C

*  Create the header (there is no comment) and append it to the headers.
               CALL FTS1_WKEYC( KEYWRD, 'LINEAR', ' ', ' ', .FALSE.,
     :                          HEADER, STATUS )
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Write the start value to keyword CRVALn.
*  ========================================

*  Form keyword.
               KEYWRD = 'CRVAL'//C

*  Create the CRVALn header (there is no comment) and append it to the
*  OIF headers using the appropriate data type.
               IF ( ATYPE( I ) .EQ. '_DOUBLE' ) THEN
                  CALL FTS1_WKEYD( KEYWRD, DSTART( I ), ' ', ' ',
     :                             HEADER, STATUS )
               ELSE
                  CALL FTS1_WKEYR( KEYWRD, START( I ), ' ', ' ', HEADER,
     :                             STATUS )
               END IF
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Write the incremental value to keyword CDELTn.
*  ==============================================

*  Form keyword.
               KEYWRD = 'CDELT'//C

*  Create the CDELTn header (there is no comment) and append it to the
*  OIF headers using the appropriate data type.
               IF ( ATYPE( I ) .EQ. '_DOUBLE' ) THEN
                  CALL FTS1_WKEYD( KEYWRD, DINCRE( I ), ' ', ' ',
     :                             HEADER, STATUS )
               ELSE
                 CALL FTS1_WKEYR( KEYWRD, INCREM( I ), ' ', ' ', HEADER,
     :                            STATUS )
               END IF
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Write the incremental value to keyword CDn_n.
*  =============================================

*  Form the keyword name.  CDELTn value is folded into CD matrix.
               KEYWRD = 'CD'//C//'_'//C

*  Create the CDn_n header (there is no comment) and append it to the
*  OIF headers using the appropriate data type.
               IF ( ATYPE( I ) .EQ. '_DOUBLE' ) THEN
                  CALL FTS1_WKEYD( KEYWRD, DINCRE( I ), ' ', ' ',
     :                             HEADER, STATUS )
               ELSE
                  CALL FTS1_WKEYR( KEYWRD, INCREM( I ), ' ', ' ',
     :                             HEADER, STATUS )
               END IF
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Write the reference pixel to keyword CRPIXn.
*  ============================================

*  Form the keyword name.
               KEYWRD = 'CRPIX'//C

*  Create the CRPIXn header (there is no comment) and append it to the
*  OIF headers using the appropriate data type.
               IF ( ATYPE( I ) .EQ. '_DOUBLE' ) THEN
                  CALL FTS1_WKEYD( KEYWRD, 1.0D0, ' ', ' ', HEADER,
     :                             STATUS )
               ELSE
                  CALL FTS1_WKEYR( KEYWRD, 1.0, ' ', ' ', HEADER,
     :                             STATUS )
               END IF
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Deal with non-linear one-dimensional data using MULTISPEC headers.
*  ==================================================================

*  This does not assume that the NDF is a spectrum, only that the NOAO
*  spectral packages will be able to obtain its non-linear axis
*  co-ordinates.
            ELSE

*  Start the WATi_nnn header.
*  ==========================
*
*  This records the axis type, label and units along axis i.  The
*  values may span multiple lines, hence the nnn which starts at 001
*  and increments by one for each line.
*
*  Start with the axis type.

*  Create the keyword.
               KEYWRD = 'WAT'//C//'_'
               CPOS = 5
               CALL CHR_ITOC( 1000 + HEADNO, CN, NC )
               CALL CHR_APPND( CN( 2:4 ), KEYWRD, CPOS )

*  Write the axis type to the WATi_nnn header.
               CALL COI_FKEYC( KEYWRD, 'wtype=multispec ',
     :                         ' ', ' ', .FALSE., HEADER, STATUS )
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Increment the header number for the next WAT1_nnn card.
               HEADNO = HEADNO + 1

*  Append the axis label to the WATi_nnn keyword.
*  ==============================================

*  Only record the axis label and units for the first dimension.
               IF ( I .EQ. 1 ) THEN

*  See whether an axis label is present or not.
                  CALL NDF_ASTAT( NDF, 'Label', I, AXLFND, STATUS )
                  IF ( AXLFND ) THEN

*  Obtain the label's value.
                     CALL NDF_ACGET( NDF, 'Label', I, VALUE, STATUS )

*  Remove unprintable characters that are not permitted in FITS.
                     CALL CHR_CLEAN( VALUE )
                     NCHAR = CHR_LEN( VALUE )

*  Create the keyword.
                     KEYWRD = 'WAT'//C//'_'
                     CPOS = 5
                     CALL CHR_ITOC( 1000 + HEADNO, CN, NC )
                     CALL CHR_APPND( CN( 2:4 ), KEYWRD, CPOS )

*  Write the label to the WATi_nnn header.
                     CALL COI_FKEYC( KEYWRD, 'label="'/
     :                               /VALUE( :MIN( NCHAR, SZVAL ) )/
     :                               /'" ', ' ', ' ', .FALSE., HEADER,
     :                               STATUS )
                     IF ( STATUS .EQ. SAI__OK )
     :                 CALL ADLINE( IMDESC, HEADER )

*  Increment the header number for the next WAT1_nnn card.
                     HEADNO = HEADNO + 1
                  END IF

*  Append the axis units to the WATi_nnn keyword.
*  ==============================================

*  See whether an axis units is present or not.
                  CALL NDF_ASTAT( NDF, 'Units', I, AXUFND, STATUS )
                  IF ( AXUFND ) THEN

*  Obtain the units' value.
                     CALL NDF_ACGET( NDF, 'Units', I, VALUE, STATUS )

*  Remove unprintable characters that are not permitted in FITS.
                     CALL CHR_CLEAN( VALUE )
                     NCHAR = CHR_LEN( VALUE )

*  Create the keyword.
                     KEYWRD = 'WAT'//C//'_'
                     CPOS = 5
                     CALL CHR_ITOC( 1000 + HEADNO, CN, NC )
                     CALL CHR_APPND( CN( 2:4 ), KEYWRD, CPOS )

*  Write the units to the WATi_nnn header.
                     CALL COI_FKEYC( KEYWRD, 'units="'/
     :                               /VALUE( :MIN( NCHAR, SZVAL ) )/
     :                               /'" ', ' ', ' ', .FALSE., HEADER,
     :                               STATUS )
                     IF ( STATUS .EQ. SAI__OK )
     :                 CALL ADLINE( IMDESC, HEADER )

*  Increment the header number for the next WAT1_nnn card.
                     HEADNO = HEADNO + 1
                  END IF

*  Create the spec1 string for the MULTISPEC format.
*  =================================================
*
*  This is written to the WAT2_nnn headers.
               ELSE IF ( I .EQ. 2 ) THEN

*  Use the appropriate data type.
                  IF ( ATYPE( I ) .EQ. '_DOUBLE' ) THEN

*  Append the specN attributes to the WAT1_nnn headers.
                     CALL COI_WMS5D( IMDESC, 1, 1, 2, DSTART, DINCRE,
     :                               0.0, VAL__BADR, VAL__BADR,
     :                               1.0, 0.0, NELM,
     :                               %VAL( CNF_PVAL( APNTR( 1 ) ) ),
     :                               HEADNO, STATUS )

                  ELSE

*  Append the specN attributes to the WAT1_nnn headers.
                     CALL COI_WMS5R( IMDESC, 1, 1, 2, START, INCREM,
     :                               0.0, VAL__BADR, VAL__BADR,
     :                               1.0, 0.0, NELM,
     :                               %VAL( CNF_PVAL( APNTR( 1 ) ) ),
     :                               HEADNO, STATUS )
                  END IF

               END IF

*  Write a blank line.
               HEADER = ' '
               CALL ADLINE( IMDESC, HEADER )

*  Write the CTYPEn header.
*  ========================

*  Form the keyword name.
               CALL CHR_ITOC( I, C, NCHAR )
               KEYWRD = 'CTYPE'//C

*  Create the header (there is no comment) and append it to the headers.
               CALL FTS1_WKEYC( KEYWRD, 'MULTISPE', ' ', ' ', .FALSE.,
     :                          HEADER, STATUS )
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Write the incremental value to keyword CDELTn.
*  ==============================================

*  Form the keyword name.
               KEYWRD = 'CDELT'//C

*  Create the CDELTn header (there is no comment) and append it to the
*  OIF headers using the appropriate data type.
               IF ( ATYPE( I ) .EQ. '_DOUBLE' ) THEN
                  CALL FTS1_WKEYD( KEYWRD, DINCRE( I ), ' ', ' ',
     :                             HEADER, STATUS )
               ELSE
                  CALL FTS1_WKEYR( KEYWRD, INCREM( I ), ' ', ' ',
     :                             HEADER, STATUS )
               END IF
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Write the keyword CDn_n.
*  ========================

*  Form the keyword name.  CDELTn value is folded into CD matrix.
               KEYWRD = 'CD'//C//'_'//C

*  Create the CDn_n header (there is no comment) and append it to the
*  OIF headers using the appropriate data type.
               IF ( ATYPE( I ) .EQ. '_DOUBLE' ) THEN
                  CALL FTS1_WKEYD( KEYWRD, 1.0D0, ' ', ' ', HEADER,
     :                             STATUS )
               ELSE
                  CALL FTS1_WKEYR( KEYWRD, 1.0, ' ', ' ', HEADER,
     :                             STATUS )
               END IF
               IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )
            END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP('COI_AXEXP_ERR2',
     :           'Error writing axis'//C//' header '//'cards.', STATUS )
               GOTO 999
            END IF
         END DO
      END IF

  999 CONTINUE

*  Unmap the axis-centre arrays.
      CALL NDF_AUNMP( NDF, 'Centre', 0, STATUS )

      END
