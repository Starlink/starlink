      SUBROUTINE COF_DSTYP( FUNIT, SCAKEY, ZERKEY, TYPE, STATUS )
*+
*  Name:
*     COF_DSTYP

*  Purpose:
*     Determines the effective data type after application of scale and
*     offset.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_DSTYP( FUNIT, SCAKEY, ZERKEY, TYPE, STATUS )

*  Description:
*     This routine determines the HDS data type needed to represent
*     FITS data, after any block-floating-point scaling and offset is
*     applied.  The supplied header keywords indicate which scale and
*     offset values to use.
*
*     If these values are one and zero respectively either explicitly,
*     or because one or both are absent from the header, then the data
*     type is set to a blank string.  This implies that the actual data
*     type in the FITS file may be used.  On the other hand, if the
*     keyword values do scale and/or offset the values, this routine
*     determines the data type for the scaled data based upon the number
*     of significant digits in the value.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     SCAKEY = INTEGER (Given)
*        The keyword for the scaling value.  It should be either
*        'BSCALE' or 'TFORMn', where n is an integer between 1 and 999.
*     ZERKEY = INTEGER (Given)
*        The keyword for the offset value.  It should be either
*        'BSCALE' or 'TZERn', where n is an integer between 1 and 999.
*     TYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The effective data type for the data array.  It is either
*        '_REAL' or '_DOUBLE' if the scale and offset do not take their
*        defaults of one and zero respectively, and TYPE is blank
*        otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The current header and data unit must either be primary or an
*     IMAGE extension.  The routine aborts with an error status if this
*     requirement is not satisfied.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science & Technology Facilities Council. All
*     Rights Reserved.

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
*     1996 April 25 (MJC):
*        Original version.
*     2008 March 15 (MJC):
*        Use KAPLIBS routines instead of their cloned CON equivalents.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) SCAKEY
      CHARACTER * ( * ) ZERKEY

*  Arguments Returned:
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      CHARACTER * ( 80 ) BUFFER  ! Used to form error messages
      CHARACTER * ( 48 ) COMENT  ! Keyword's comment (not used)
      INTEGER FSTAT              ! FITSIO error status
      DOUBLE PRECISION OFFSET    ! Offset for block floating point
      DOUBLE PRECISION SCALE     ! Scale factor for block floating point
      LOGICAL SCAPRE             ! Keyword for scale is present?
      INTEGER SDIGIT             ! Number of significant digits in scale
                                 ! factor
      CHARACTER * ( 70 ) VALUE   ! Keyword value
      INTEGER ZDIGIT             ! Number of significant digits in
                                 ! offset
      LOGICAL ZERPRE             ! Keyword for offset is present?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Initialise the returned data type.
      TYPE = ' '

*  Obtain the values of the scale and offset keywords.
*  ===================================================

*  Obtain the scale keyword.
      CALL COF_GKEYD( FUNIT, SCAKEY, SCAPRE, SCALE, COMENT, STATUS )
      IF ( .NOT. SCAPRE ) SCALE = 1.0D0

*  Obtain the scale keyword.
      CALL COF_GKEYD( FUNIT, ZERKEY, ZERPRE, OFFSET, COMENT, STATUS )
      IF ( .NOT. ZERPRE ) OFFSET = 0.0D0

*  Assign the returned data type.
*  ==============================
      IF ( SCALE .NE. 1.0D0 .OR. OFFSET .NE. 0.0D0 ) THEN

*  Obtain the value string for the scale keyword.
         IF ( SCAPRE ) THEN
            CALL FTGKEY( FUNIT, SCAKEY, VALUE, COMENT, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( FSTAT .GT. FITSOK ) THEN
               BUFFER = 'Error obtaining the string value of keyword '/
     :                  /SCAKEY
               CALL COF_FIOER( FSTAT, 'COF_DSTAB', 'FTGKEY', BUFFER,
     :                         STATUS )
               GOTO 999
            END IF

*  Find the number of significant digits in the numerical value.
            CALL KPG1_SGDIG( VALUE, SDIGIT, STATUS )

*  Report the error context if there was a problem reading a numerical
*  value from the header card.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'KEY', SCAKEY )
               CALL ERR_REP( 'COF_DSTYP_SCAERR',
     :           'The value of the ^KEY keyword is invalid.', STATUS )
               GOTO 999
            END IF
         ELSE
            SDIGIT = 1
         END IF

*  Obtain the value string for the offset keyword.
         IF ( ZERPRE ) THEN
            CALL FTGKEY( FUNIT, ZERKEY, VALUE, COMENT, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( FSTAT .GT. FITSOK ) THEN
               BUFFER = 'Error obtaining the string value of keyword '/
     :                  /ZERKEY

               CALL COF_FIOER( FSTAT, 'COF_DSTAB', 'FTGKEY',
     :                         BUFFER, STATUS )
               GOTO 999
            END IF

*  Find the number of significant digits in the numerical value.
            CALL KPG1_SGDIG( VALUE, ZDIGIT, STATUS )

*  Report the error context if there was a problem reading a numerical
*  value from the header card.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'KEY', ZERKEY )
               CALL ERR_REP( 'COF_DSTYP_ZERERR',
     :           'The value of the ^KEY keyword is invalid.', STATUS )
               GOTO 999
            END IF
         ELSE
            ZDIGIT = 1
         END IF

*  Determine the appropriate type by comparing the number of
*  significant digits present with the maximum number of significant
*  digits afforded by a real number.
         IF ( MAX( SDIGIT, ZDIGIT ) .LE.
     :        -INT( LOG10( VAL__EPSR ) ) ) THEN
            TYPE = '_REAL'
         ELSE
            TYPE = '_DOUBLE'
         END IF

      END IF

  999 CONTINUE

      END
