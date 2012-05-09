      SUBROUTINE CCD1_GTMSK( PARAM, UBND, LBND, GID, ITYPE, ID, MSKNAM,
     :                       STATUS )
*+
*  Name:
*     CCD1_GTMSK

*  Purpose:
*     Access or create an NDF mask file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GTMSK( PARAM, UBND, LBND, GID, ITYPE, ID, MSKNAM,
*                      STATUS )

*  Description:
*     This routine attempts to access an NDF containing a mask of BAD
*     pixels to use when flagging defective areas on a CCD. If access to
*     such an NDF fails then it assumed that the given string is either
*     a file name of a formatted file containing ARD descriptions, or an
*     ARD description. If the mask file is an NDF then its identifier
*     is just passed back. If the file is an ARD region description file
*     a new temporary NDF is created in which the mask data is entered.
*     If lower and upper bounds are given then any created NDFs will be
*     of this size. If lower and upper bounds are not given, then the
*     routine will use the NDG group identifier to check the size of the
*     first NDF in the group. This will be used for the upper and lower
*     limits and the type of the output mask.  Finally the name of the
*     MASK file and its type (ARD or NDF) are returned in the string
*     MSKNAM.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter via which the file name is to be
*        accessed.
*     LBND( 2 ) = INTEGER (Given and Returned)
*        On entry the lower bounds of any possible created MASK NDF.
*        Set to VAL__BADI if not to be used. On exit the lower bounds of
*        the mask NDF.
*     UBND( 2 ) = INTEGER (Given and Returned)
*        Upper bounds of any possible created MASK NDF. Set to
*        VAL__BADI if not to be used. On exit the upper bounds of the
*        mask NDF.
*     GID = INTEGER (Given)
*        NDG group identifier for NDFs whose data the mask is to be
*        applied to. If necessary the first member of the group is used
*        to define the size of the output mask data.
*     ITYPE = CHARACTER * ( * ) (Given and Returned)
*        The type of the output MASK data. If this is given as ' ' then
*        the type of the NDF given as the first member of the NDG group
*        GID will be used and returned.
*     ID = INTEGER (Returned)
*        NDF identifier of the mask data array.
*     MSKNAM = CHARACTER *( * ) (Returned)
*        The name and type of the mask file expressed as a string.
*        'FILENAME (TYPE)'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If the size of the input group NDFs change, then it is possible
*     that some mask information will be lost. This is thought not to
*     out weight the overhead in accessing every input NDF to test its
*     size, although this may change if demand ever occurs. One
*     possible alternative is to delay the mask processing until later,
*     although this will have the overhead of either reprocessing the
*     ARD file for every NDF, or, devising a scheme for storing ARD
*     information in a quick access method - this will need to be
*     dynamic. MUST BE A MORE EFFICIENT WAY OF LOOKING AFTER ARD FILES.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997, 2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     25-OCT-1991 (PDRAPER):
*        Original Version.
*     12-DEC-1991 (PDRAPER):
*        Added MASK typing.
*     10-APR-1995 (PDRAPER):
*        Added top-level stack locator control for NDF as a mask.
*     5-SEP-1995 (PDRAPER):
*        Converted to official ARD release.
*     3-MAR-1997 (PDRAPER):
*        Removed LOC from IRG_NDFEX call and all associated code,
*        including common block for controlling open files.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'PRM_PAR'          ! Primdat constants
      INCLUDE 'MSG_PAR'          ! Size of message string.. maximum size
                                 ! of NDF file name
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER GID

*  Arguments Given and Returned:
      INTEGER UBND( 2 )
      INTEGER LBND( 2 )
      CHARACTER * ( * ) ITYPE

*  Arguments Returned:
      INTEGER ID
      CHARACTER MSKNAM * ( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of string excluding trailing
      EXTERNAL CHR_LEN           ! blanks

*  Local variables:
      CHARACTER * ( MSG__SZMSG ) FNAME ! FIO filename string
      INTEGER EL                 ! Number of pixels in mask NDF
      INTEGER IAT                ! Position within name string
      INTEGER IDMSK              ! NDF identifier of created mask
      INTEGER IDSIZE             ! NDF identifier to first NDF
      INTEGER IDWRK              ! NDF identifier for logical mask W/S
      INTEGER NEX                ! Number of ARD lines
      INTEGER I                  ! Loop variable
      INTEGER IPDAT              ! Pointer to mask data component
      INTEGER IPWRK              ! Pointer to INTEGER workspace used by ARD
      INTEGER LBNDE( 2 )         ! Lower external bounding box
      INTEGER LBNDI( 2 )         ! Lower internal bounding box
      INTEGER NDIM               ! Dummy
      INTEGER PLACE              ! NDF place for temporary mask data
      INTEGER REGVAL             ! Starting value for masked pixels
      INTEGER SLEN               ! Length of MSKNAM string
      INTEGER UBNDE( 2 )         ! Upper external bounding box
      INTEGER UBNDI( 2 )         ! Upper internal bounding box
      LOGICAL ISARD              ! Set true if input file is an ARD description
      REAL TRCOEF( 0:1,2 )     ! Dummy transformation

*.

*  Check global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to access the mask NDF or ARD expression. If this fails return immediately.
      CALL CCD1_ACMSK( PARAM, ID, ISARD, FNAME, STATUS )
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If it's an ARD description (ISARD) then need to create an NDF to
*  put the mask information into.
      IF ( ISARD ) THEN

*  Check for any given bounds and for a MASK type.
         IF ( LBND( 1 ) .EQ. VAL__BADI .OR. ITYPE .EQ. ' ' ) THEN

*  Need to derive some bounds and/or a mask type.
            CALL NDG_NDFAS( GID, 1, 'READ', IDSIZE, STATUS )

*  Get the required data.
            IF ( LBND( 1 ) .EQ. VAL__BADI ) THEN
               CALL NDF_BOUND( IDSIZE, 2, LBND, UBND, NDIM, STATUS )
            END IF
            IF ( ITYPE .EQ. ' ' ) THEN
               CALL NDF_TYPE( IDSIZE, 'Data', ITYPE, STATUS )
            END IF

*  Release the NDF.
            CALL NDF_ANNUL( IDSIZE, STATUS )
         END IF

*  Create an NDF to contain the actual MASK (this has invalid values to
*  show the bad pixels and is of the same data type as the reference NDF.)
         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEW( ITYPE, 2, LBND, UBND, PLACE, IDMSK, STATUS )

*  Create an NDF to contain the ARD mask.
         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEW( '_INTEGER', 2, LBND, UBND, PLACE, IDWRK, STATUS )

*  Map in the data component
         CALL NDF_MAP( IDWRK, 'Data', '_INTEGER', 'WRITE/ZERO', IPWRK,
     :                 EL, STATUS )

*  Add the ARD mask.
         REGVAL = 2
         TRCOEF( 0, 1 ) = VAL__BADR
         CALL ARD_WORK( ID, 2, LBND, UBND, TRCOEF, .FALSE., REGVAL,
     :                  %VAL( CNF_PVAL( IPWRK ) ),
     :                  LBNDI, UBNDI, LBNDE, UBNDE,
     :                  STATUS )

*  Map in the data component of the output mask.
         CALL NDF_MAP( IDMSK, 'Data', ITYPE, 'WRITE', IPDAT, EL,
     :                 STATUS )

*  Apply ARD mask to the output mask data component.
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL CCG1_CMSKB( %VAL( CNF_PVAL( IPWRK ) ), EL, 0.0D0,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL CCG1_CMSKUB( %VAL( CNF_PVAL( IPWRK ) ), EL, 0.0D0,
     :                        %VAL( CNF_PVAL( IPDAT ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL CCG1_CMSKW( %VAL( CNF_PVAL( IPWRK ) ), EL, 0.0D0,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL CCG1_CMSKUW( %VAL( CNF_PVAL( IPWRK ) ), EL, 0.0D0,
     :                        %VAL( CNF_PVAL( IPDAT ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL CCG1_CMSKI( %VAL( CNF_PVAL( IPWRK ) ), EL, 0.0D0,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL CCG1_CMSKR( %VAL( CNF_PVAL( IPWRK ) ), EL, 0.0D0,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL CCG1_CMSKD( %VAL( CNF_PVAL( IPWRK ) ), EL, 0.0D0,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL CCG1_CMSKK( %VAL( CNF_PVAL( IPWRK ) ), EL, 0.0D0,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       STATUS )
         END IF

*  Unmap the mask NDF, ready for use.
         CALL NDF_UNMAP( IDMSK, '*', STATUS )

*  Erase ARD mask W/S
         CALL NDF_ANNUL( IDWRK, STATUS )

*  If no filename was given construct a string with the ARD description in.
         IF ( FNAME .EQ. ' ' ) THEN
            CALL GRP_GRPSZ( ID, NEX, STATUS )
            IAT = 1
            DO 2 I = 1, NEX
               CALL GRP_GET( ID, I, 1, FNAME( IAT: ), STATUS )
               IAT = CHR_LEN( FNAME ) + 2
 2          CONTINUE
         END IF
         IAT = CHR_LEN( FNAME )

*  Release the ARD group.
         CALL CCD1_GRDEL( ID, STATUS )

*  Pass out the mask NDF identifier.
         ID = IDMSK
      ELSE

*  Just a NDF, get the NDF bounds.
         CALL NDF_BOUND( ID, 2, LBND, UBND, NDIM, STATUS )

*  Get the NDF name.
         CALL NDF_MSG( 'NDFNAME', ID )
         CALL MSG_LOAD( ' ', '^NDFNAME', FNAME, IAT, STATUS )

*  Get the type for completeness.
         CALL NDF_TYPE( ID, 'Data', ITYPE, STATUS )
      END IF

*  Append the appropriate type to the name string, watching for
*  truncation.
      SLEN = LEN( MSKNAM ) - 7
      IF ( IAT .GT. SLEN ) THEN
         CALL CCD1_PLOF( FNAME ( :IAT ), SLEN, MSKNAM, STATUS )
      ELSE
         MSKNAM = FNAME
      END IF

*  Add the type.
      IF ( ISARD ) THEN

*  ARD file or expression.
         MSKNAM( IAT + 2 : ) = '(ARD)'
      ELSE

*  NDF file.
         MSKNAM( IAT + 2 : ) = '(NDF)'
      END IF

      END
* $Id$
