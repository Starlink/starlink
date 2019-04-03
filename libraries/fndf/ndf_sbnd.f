      SUBROUTINE NDF_SBND( NDIM, LBND, UBND, INDF, STATUS )
*+
*  Name:
*     NDF_SBND

*  Purpose:
*     Set new pixel-index bounds for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_SBND( NDIM, LBND, UBND, INDF, STATUS )

*  Description:
*     The routine sets new pixel-index bounds for an NDF (or NDF
*     section). The number of NDF dimensions may also be changed. If a
*     base NDF is specified, then a permanent change is made to the
*     actual data object and this will be apparent through any other
*     NDF identifiers which refer to it. However, if an identifier for
*     an NDF section is specified, then its bounds are altered without
*     affecting other identifiers.

*  Arguments:
*     NDIM = INTEGER (Given)
*        New number of NDF dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        New lower pixel-index bounds of the NDF.
*     UBND( NDIM ) = INTEGER (Given)
*        New upper pixel-index bounds of the NDF.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The bounds of an NDF section cannot be altered while any of
*     its array components (or any of its axis arrays) is mapped for
*     access through the identifier supplied to this routine.
*     -  The bounds of a base NDF cannot be altered while any part of
*     any of its array components (or any of its axis arrays) is mapped
*     for access, even through another identifier.
*     -  The pixel values of any defined NDF array component will be
*     retained if those pixels lie within both the old and new bounds.
*     Any pixels lying outside the new bounds will be lost and cannot
*     later be recovered by further changes to the NDF's bounds. Any
*     new pixels introduced where the new bounds extend beyond the old
*     ones will be assigned the "bad" value, and subsequent enquiries
*     about the presence of bad pixels will reflect this.
*     -  If the new NDF bounds extend beyond the bounds of the
*     associated base NDF and any of the NDF's axis arrays have defined
*     values, then these values will be extrapolated as necessary.
*     -  If the bounds of a base NDF are to be altered and retention of
*     the pixel values of any of its components is not required, then a
*     call to NDF_RESET should be made before calling this routine.
*     This will eliminate any unnecessary processing which might be
*     needed to retain the existing values. This step is not necessary
*     with an NDF section, as no processing of pixel values takes place
*     in this case.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-MAR-1990 (RFWS):
*        Original version.
*     21-MAR-1990 (RFWS):
*        Re-structured the checks on whether NDF components are mapped.
*     19-OCT-1990 (RFWS):
*        Installed support for the axis component.
*     2-NOV-1990 (RFWS):
*        Changed to ensure that default storage form entries in the DCB
*        take correct account of changes in the NDF's bounds.
*     28-NOV-1990 (RFWS):
*        Removed conversion of default array storage form, which now
*        remains fixed at its initial value, regardless of changes to
*        the NDF's bounds.
*     11-FEB-1992 (RFWS):
*        Fixed bug: IDCB used where IACB intended.
*     11-JUL-1997 (RFWS):
*        Added support for the WCS component.
*     3-FEB-1999 (RFWS):
*        Changed handling of WCS component so that the new WCS
*        information is obtained at the start, but only written back at
*        the end. This overcomes the problem of WCS information being
*        incompatible with the NDF if the number of dimensions is
*        modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public interface
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read)
*           Number of mapped accesses to the data object.
*        DCB_QFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read and
*        Write)
*           Storage form used for the quality array.
*        DCB_QID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the quality array.
*        DCB_VFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read and
*        Write)
*           Storage form used for the variance array.


      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF is a section.
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's data array is mapped for access.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's quality array.
*        ACB_QMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's quality array is mapped for access.
*        ACB_VID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's variance array.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's variance array is mapped for access.

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( * )
      INTEGER UBND( * )
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( ARY__SZFRM ) FORM ! Array storage form
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IAX                ! Loop counter for dimensions
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IWCS               ! AST_ pointer to WCS information
      INTEGER NDIMI              ! Initial number of NDF dimensions
      LOGICAL MAPPED             ! Is an NDF component mapped?
      LOGICAL THERE              ! Whether component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the new bounds for validity.
      CALL NDF1_VBND( NDIM, LBND, UBND, STATUS )

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that BOUNDS access to the NDF is available.
         CALL NDF1_CHACC( IACB, 'BOUNDS', STATUS )

*  Determine the initial number of NDF dimensions from the ARY_ system
*  identifier for the data array, held in the ACB.
         CALL ARY_NDIM( ACB_DID( IACB ), NDIMI, STATUS )

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that quality and variance information is available in the DCB
*  and ACB (note that this must be done at the start because once one
*  component has had new bounds set, the bounds of subsequent
*  components will no longer match it, so an error would result due to
*  the checks which take place when information is obtained about these
*  subsequent components).
         CALL NDF1_QIMP( IACB, STATUS )
         CALL NDF1_VIMP( IACB, STATUS )

*  Similarly, ensure that information is available about all of the
*  NDF's axis array components.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO 1 IAX = 1, NDIMI
               CALL NDF1_DAD( IAX, IDCB, STATUS )
               CALL NDF1_DAV( IAX, IDCB, STATUS )
               CALL NDF1_DAW( IAX, IDCB, STATUS )
 1          CONTINUE
         END IF
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that none of the NDF's components is mapped for access through
*  the current ACB entry.
            MAPPED = ACB_DMAP( IACB ) .OR.
     :               ACB_QMAP( IACB ) .OR.
     :               ACB_VMAP( IACB )

*  Check all the axis arrays for current mappings as well if necessary.
            IF ( .NOT. MAPPED ) THEN
               DO 2 IAX = 1, NDIMI
                  IF ( ACB_ADMAP( IAX, IACB ) .OR.
     :                 ACB_AVMAP( IAX, IACB ) .OR.
     :                 ACB_AWMAP( IAX, IACB ) ) THEN
                     MAPPED = .TRUE.
                     GO TO 3
                  END IF
 2             CONTINUE
 3             CONTINUE
            END IF

*  If any component is mapped, then report an error.
            IF ( MAPPED ) THEN
               STATUS = NDF__ISMAP
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF_SBND_MAP1',
     :         'The NDF structure ^NDF is already mapped for access ' //
     :         'through the specified identifier (possible ' //
     :         'programming error).', STATUS )

*  If this is a base NDF, then also check that no part of any component
*  is mapped. Report an error if it is.
            ELSE IF ( ( .NOT. ACB_CUT( IACB ) ) .AND.
     :                ( DCB_NMAP( IDCB ) .NE. 0 ) ) THEN
               STATUS = NDF__ISMAP
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF_SBND_MAP2',
     :         'The NDF structure ^NDF is already mapped for access ' //
     :         'through another identifier (possible programming ' //
     :         'error).', STATUS )
            END IF
         END IF

*  WCS component:
*  ==============
*  We first obtain the new WCS information which will apply to the
*  modified NDF, since this depends on the AXIS component and the NDF's
*  main data array having their original shape. Initialise the AST
*  pointer to this information.
         IWCS = AST__NULL

*  We can omit this stage if the NDF is a section, since the WCS
*  information does not then need changing. Also check if the WCS
*  component exists and do nothing if it does not.
         IF ( .NOT. ACB_CUT( IACB ) ) THEN
            CALL NDF1_WSTA( IACB, THERE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( THERE ) THEN

*  If we have a base NDF with WCS information present, then obtain a
*  copy of the WCS information which will apply to the NDF once its
*  bounds have been changed.
                  CALL NDF1_WSBND( NDIM, LBND, UBND, IACB, IWCS,
     :                             STATUS )
               END IF
            END IF
         END IF

*  AXIS component:
*  ==============
*  The bounds of the axis component must be handled next, since
*  extrapolation of axis array values depends on the NDF's main data
*  array having its original shape.
         CALL NDF1_ASBND( NDIM, LBND, UBND, IACB, STATUS )

*  DATA component:
*  ==============
*  Set the new bounds for the data component.
         CALL ARY_SBND( NDIM, LBND, UBND, ACB_DID( IACB ), STATUS )
         CALL NDF1_CMPAC( ACB_IDCB( IACB ), 'DATA', STATUS )

*  QUALITY component:
*  =================
*  See if the ARY_ system identifier for the quality component is valid.
*  If not, then the component is not defined.
         CALL ARY_VALID( ACB_QID( IACB ), THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is defined, then set new bounds for it.
            IF ( THERE ) THEN
               CALL ARY_SBND( NDIM, LBND, UBND, ACB_QID( IACB ),
     :                        STATUS )
               CALL NDF1_CMPAC( ACB_IDCB( IACB ), 'QUALITY', STATUS )

*  Since this may have caused the array's bad pixel flag to be set, a
*  value of .FALSE. must be re-established. This is not done if the
*  array has a primitive storage form. Otherwise, the .FALSE. value is
*  set via the base array in the DCB (since it will not affect the
*  actual data object if set via an array section).
               CALL ARY_FORM( ACB_QID( IACB ), FORM, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( FORM .NE. 'PRIMITIVE' ) THEN
                     CALL ARY_SBAD( .FALSE., DCB_QID( IDCB ), STATUS )
                  END IF
               END IF

*  If the array is not defined, and this is a base NDF, then convert its
*  default storage form to take account of the new bounds, if necessary.
            ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN
               CALL NDF1_CBFRM( NDIM, LBND, UBND, DCB_QFRM( IDCB ),
     :                          STATUS )
            END IF
         END IF

*  VARIANCE component:
*  ==================
*  See if the ARY_ system identifier for the variance component is
*  valid. If not, then the component is not defined.
         CALL ARY_VALID( ACB_VID( IACB ), THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is defined, then set new bounds for it.
            IF ( THERE ) THEN
               CALL ARY_SBND( NDIM, LBND, UBND, ACB_VID( IACB ),
     :                        STATUS )
               CALL NDF1_CMPAC( ACB_IDCB( IACB ), 'VARIANCE', STATUS )

*  If the array is not defined, and this is a base NDF, then convert its
*  default storage form to take account of the new bounds, if necessary.
            ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN
               CALL NDF1_CBFRM( NDIM, LBND, UBND, DCB_VFRM( IDCB ),
     :                          STATUS )
            END IF
         END IF

*  WCS component:
*  =============
*  We now return to the WCS component and write the modified WCS
*  information back to the NDF, if necessary, to over-write the
*  original information.
         IF ( IWCS .NE. AST__NULL ) THEN
            CALL NDF1_WRWCS( IWCS, IACB, STATUS )

*  Annul the AST_ pointer to the WCS information.
            CALL AST_ANNUL( IWCS, STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_SBND_ERR',
     :   'NDF_SBND: Error setting new pixel-index bounds for an ' //
     :   'NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_SBND', STATUS )
      END IF

      END
