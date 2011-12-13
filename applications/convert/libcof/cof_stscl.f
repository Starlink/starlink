      SUBROUTINE COF_STSCL( FUNITH, NDFO, COMP, TYPE, STATUS )
*+
*  Name:
*     COF_STSCL

*  Purpose:
*     Sets scaling requirements for native scaled form of NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_STSCL( FUNITH, NDFO, COMP, TYPE, STATUS )

*  Description:
*     This routine sets the scale and offset of an NDF data array
*     in scaled form, either using the requested data type, or
*     if none is specified from the precision of the BSCALE and BZERO
*     FITS keywords.  A scaled array is not created if both
*     BSCALE and BZERO have the default values.

*  Arguments:
*     FUNITH = INTEGER (Given)
*        The FITS unit number associated with the header.  This may
*        differ from that associated with the data if a merged header
*        has been constructed.
*     NDFO = INTEGER (Given)
*        The identifier to the NDF to have scale and offset applied.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component of the NDF that it is be stored in scaled
*        form.  Allowed values are "DATA" or VARIANCE".
*     TYPE = CHARACTER * ( NDF__SZTYP ) (Given)
*        Given as the type of the NDF scaled array.  If it is a blank
*        the effective data type for the data array based upon keywords
*        BSCALE and BZERO.  It is either '_REAL' or '_DOUBLE' if the
*        scale and offset do not take their defaults of one and zero
*        respectively.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  It should be called after the data have been copied from
*     the FITS file to the NDF, and unmapped.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research
*     Council.  All Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     2007 January 3 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL__EPSD

*  Arguments Given:
      INTEGER FUNITH             ! FITS unit for the (merged?) FITS
                                 ! header
      INTEGER NDFO               ! NDF identifier
      CHARACTER * ( * ) COMP     ! NDF array component
      CHARACTER * ( * ) TYPE     ! Desired scaled data type

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 48 ) COMENT  ! FITS header comment
      DOUBLE PRECISION OFFSET    ! Value of BZERO keyword
      DOUBLE PRECISION SCALE     ! Value of BSCALE keyword
      LOGICAL SCAPRE             ! If scaling keyword present
      CHARACTER * ( NDF__SZTYP ) STYPE ! Data type derived from the
                                 ! scaling keywords' precision

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the data type required for the output array based upon the
*  number of significant digits in the BSCALE and BZERO keywords.  If
*  these have values of 1.0D0 and 0.0D0 respectively either explicitly,
*  or because one or both are absent, then the data type can be set to
*  the null string.
      IF ( TYPE .EQ. ' ' ) THEN
         CALL COF_DSTYP( FUNITH, 'BSCALE', 'BZERO', STYPE, STATUS )
      ELSE
         STYPE = TYPE
      END IF

*  Obtain the scale and offset from the FITS headers, defaulting
*  missing values.
      CALL COF_GKEYD( FUNITH, 'BSCALE', SCAPRE, SCALE, COMENT, STATUS )
      IF ( .NOT. SCAPRE ) SCALE = 1.0D0

      CALL COF_GKEYD( FUNITH, 'BZERO', SCAPRE, OFFSET, COMENT, STATUS )
      IF ( .NOT. SCAPRE ) OFFSET = 0.0D0

*  Set the scale and offset in the chosen NDF's array component unless
*  the default/null values are present.  This avoids later unnecessary
*  scaling when accessing the array.
      IF ( ABS( SCALE - 1.0D0 ) .GT. VAL__EPSD .AND.
     :     ABS( OFFSET ) .GT. VAL__EPSD ) THEN

*  If either keyword is absent, the user probably has made a mistake
*  in selecting native mode.  Assume that it's a real array.
         IF ( STYPE .EQ. '_REAL' .OR. STYPE .EQ. ' ' ) THEN
            CALL NDF_PTSZR( REAL( SCALE ), REAL( OFFSET ), NDFO, COMP,
     :                      STATUS )
         ELSE
            CALL NDF_PTSZD( SCALE, OFFSET, NDFO, COMP, STATUS )
         END IF
      END IF

      END
