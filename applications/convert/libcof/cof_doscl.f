      SUBROUTINE COF_DOSCL( FUNITH, FUNITD, FMTCNV, TYPE, STATUS )
*+
*  Name:
*     COF_DOSCL

*  Purpose:
*     Sets scaling requirements for FITS2NDF

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_DOSCL( FUNITH, FUNITD, FMTCNV, TYPE, STATUS )

*  Description:
*     This routine sets the scale and offset of a FITS data array
*     for FITSIO, and obtains the data type of the scaled
*     floating-point array.

*  Arguments:
*     FUNITH = INTEGER (Given)
*        The FITS unit number associated with the header.  This may
*        differ from that associated with the data if a merged header
*        has been constructed.
*     FUNITD = INTEGER (Given)
*        The FITS unit number associated with the data.
*     FMTCNV = LOGICAL (Given)
*        This specifies whether or not format conversion will occur.
*        The conversion applies the values of the FITS keywords BSCALE
*        and BZERO to the FITS data to generate the "true" data values.
*        This applies to IMAGE extensions, as well as the primary data
*        array.  If BSCALE and BZERO are not given in the FITS header,
*        they are taken to be 1.0 and 0.0 respectively.
*
*        If FMTCNV=.FALSE., the HDS type of the data array in the NDF
*        will be the equivalent of the FITS data format on tape (e.g.
*        BITPIX = 16 creates a _WORD array).  If FMTCNV=.TRUE., the
*        data array in the NDF will be converted from the FITS data
*        type on tape to _REAL or _DOUBLE in the NDF.  The choice of
*        floating-point data type depends on the number of significant
*        digits in the BSCALE and BZERO keywords.
*     TYPE = CHARACTER * ( NDF__SZTYP ) (Given and returned)
*        Given as the type of the NDF component.  It is returned as the
*        effective data type for the data array based upon keywords
*        BSCALE and BZERO.  It is either '_REAL' or '_DOUBLE' if the
*        scale and offset do not take their defaults of one and zero
*        respectively, and TYPE is returned blank otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     AJC: Alan J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-DEC-2000 (AJC):
*        Original version extracted from COF_F2NDF.
*     2006 April 7 (MJC):
*        Correct the prologue's overall and TYPE descriptions.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER FUNITH             ! FITS unit for the (merged?) FITS
                                 ! header
      INTEGER FUNITD             ! FITS unit for the FITS file
      LOGICAL FMTCNV             ! Format conversion required?
      CHARACTER * ( NDF__SZTYP ) TYPE ! NDF array's data type

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER ( FITSOK = 0 )

*  Global Variables:

*  Local Variables:
      CHARACTER * ( 200 ) BUFFER ! Buffer for error messages
      CHARACTER * ( 48 ) COMENT  ! FITS header comment
      INTEGER FSTAT              ! FITSIO error status
      LOGICAL SCAPRE             ! If scaling keyword present
      DOUBLE PRECISION SCALE     ! Value of BSCALE keyword
      DOUBLE PRECISION OFFSET    ! Value of BZERO keyword
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  The FMTCNV flag decides whether or not the data scaling is required.
*  The FITSIO routines that obtain the data array(s) will apply the
*  block floating-point scaling as prescribed by the BSCALE and BZERO
*  keywords.
      IF ( FMTCNV ) THEN

*  Scaling is to be applied.  Find the data type required for the
*  output array based upon the number of significant digits in the
*  BSCALE and BZERO keywords.  If these have values of 1.0D0 and 0.0D0
*  respectively either explicitly, or because one or both are absent,
*  then the data type can be set to the null string.  This instructs
*  later routines like COF_STYPC to use the data type specified by the
*  FITSIO data-type code (based on BITPIX).
         CALL COF_DSTYP( FUNITH, 'BSCALE', 'BZERO', TYPE, STATUS )

*  If the header and data FUNITs are different, set the scaling factors
*  explicitly for the data unit according to the header unit just in case
*  it inherited them from the primary.
*  Get BSCALE, BZERO from the merged header.
         IF ( FUNITH .NE. FUNITD ) THEN
            CALL COF_GKEYD( FUNITH, 'BSCALE', SCAPRE, SCALE, COMENT,
     :                      STATUS )
            IF ( .NOT. SCAPRE ) SCALE = 1.0D0

            CALL COF_GKEYD( FUNITH, 'BZERO', SCAPRE, OFFSET, COMENT,
     :                      STATUS )
            IF ( .NOT. SCAPRE ) OFFSET = 0.0D0

*  And set scaling of the original HDU.
            CALL FTPSCL( FUNITD, SCALE, OFFSET, FSTAT )
         END IF

*  To prevent scaling, the scale and offset must be set to one and zero
*  respectively.  Note that this does not affect the keywords in the
*  header of the input FITS file.  Note that the values are double
*  precision.
      ELSE
         CALL FTPSCL( FUNITD, 1.0D0, 0.0D0, FSTAT )

*  Set the recommended data type to a null string.  This instructs later
*  routines like COF_STYPC to use the data type specified by the FITSIO
*  data-type code (based on BITPIX).
         TYPE = ' '

      END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         BUFFER = 'Error defaulting the scale and offset '
         CALL COF_FIOER( FSTAT, 'COF_F2NDF_SCOF', 'FTPSCL', BUFFER,
     :                   STATUS )
      END IF

      END
