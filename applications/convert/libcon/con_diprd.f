      SUBROUTINE CON_DIPRD (UNIT, NPTS, NBREAK, BREAK, NMAX, WAVE,
     :                      FLUX, WAVE1, FLUX1, NCOR, STATUS )
*+
*  Name:
*     CON_DIPRD

*  Purpose:
*     Reads the main data and axis arrays from a Dipso file, and
*     inserts bad pixels at the Dipso break points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CON_DIPRD (UNIT, NPTS, NBREAK, BREAK, NMAX, WAVE, FLUX,
*    :                      WAVE1, FLUX1, NCOR, STATUS )

*  Description:
*     The routine reads in the wavelength and flux data arrays from
*     a file produced by the DIPSO `WRITE' command.  These data arrays
*     contain `breaks' where one or more bad data elements have been
*     removed. This routine estimates how many values have been removed
*     from the initial data by comparing the gap in the wavelength data
*     to the wavelength increment within the previous `good patch' of
*     data. At least one bad pixel is inserted at every break point.
*     The input wavelength and flux data are always of Fortran REAL
*     type,  the output data arrays are of HDS type '_REAL'.

*  Arguments:
*     UNIT = INTEGER (Given)
*        Logical unit number for Dipso file.
*     NPTS = INTEGER (Given)
*        Number of elements in original Dipso data.
*     NBREAK = INTEGER (Given)
*        Number of breaks in original Dipso data.
*     BREAK( NBREAK ) = INTEGER (Given)
*        Array containing position of break points.
*     NMAX = INTEGER (Given)
*        Size of output arrays for padded data.
*     WAVE( NPTS ) = REAL (Returned)
*        Original wavelength array in Dipso data.
*     FLUX( NPTS ) = REAL (Returned)
*        Original flux array in Dipso data.
*     WAVE1( NPTS ) = REAL (Returned)
*        Wavelength array with interpolated values at bad pixels.
*     FLUX1(NPTS) = REAL (Returned)
*        Flux array with bad values inserted at breaks.
*     NCOR = INTEGER (Returned)
*        Number of data elements after insertion of bad pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The Dipso file must have been opened, and the Title and break
*     information read.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
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

*  Author:
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     1991 Feb 8 (JM):
*        Original version.
*     2009 June 29 (MJC):
*        Used modern coding style and competed the prologue.
*     {enter_changes_here}

*-

*  Type definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! ADAM symbolic constants
      INCLUDE 'PRM_PAR'         ! PRIMDAT symbolic constants

*  Arguments Given:
      INTEGER UNIT
      INTEGER NPTS
      INTEGER NBREAK
      INTEGER BREAK (NBREAK)
      INTEGER NMAX

*  Arguments Returned:
      REAL WAVE( NPTS )
      REAL FLUX( NPTS )
      REAL WAVE1( NMAX )
      REAL FLUX1( NMAX )
      INTEGER NCOR

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      REAL    DELX              ! Gap between successive data in break
      INTEGER I                 ! Loop variable
      INTEGER IB                ! Loop variable
      INTEGER IFIRST            ! Index of first good datum in a break
      INTEGER ILAST             ! Index of last good datum in a break
      INTEGER IOS               ! I/O status
      INTEGER KBAD              ! Index in padded array
      INTEGER KOUNT             ! Loop variable
      INTEGER LASTBR            ! Size of last break
      INTEGER NADD              ! Number of bad pixels added at a break
      INTEGER NADDSM            ! Cummulative total of bad pixels added

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If first break occurs straightaway, ignore it.
      IF ( BREAK( 1 ) .EQ. 0 ) THEN
         NBREAK = NBREAK - 1
         DO I = 1, NBREAK
            BREAK( I ) = BREAK( I + 1 )
         END DO
      END IF

*  Read the data array.
      READ ( UNIT, IOSTAT = IOS ) ( WAVE( I ), FLUX( I ), I = 1, NPTS )
      IF ( IOS .NE. 0 ) GOTO 999

*  Correct the data array by adding in an appropriate number of bad
*  pixels at the break points.  At least one bad pixel is always
*  inserted.  An appropriate value for the wavelength appropriate to the
*  bad pixel is derived by linear interpolation in the region of the
*  break.
      IF ( NBREAK .EQ. 1 ) THEN
         DO I = 1, NPTS
            WAVE1( I ) = WAVE( I )
            FLUX1( I ) = FLUX( I )
         END DO
         NCOR = NPTS

*  Insert bad values as the break points.
      ELSE IF ( NBREAK .GT. 1 ) THEN

*  First, initialize the variable which count the bad-pixel insertions.
         NADDSM = 0
         NADD = 0

*  For each break read in the good values and insert bad pixels.
         DO IB = 1, NBREAK
            IF ( IB .EQ. 1 ) THEN
               IFIRST = 1
               ILAST = BREAK( IB )
               LASTBR = ILAST - IFIRST + 1
            ELSE
               IFIRST = ILAST + 1
               ILAST = BREAK( IB )

*  Guess how many elements are missing and add them in.
               IF ( LASTBR .GT. 1 ) THEN
                  DELX = ( WAVE( IFIRST - 1) -
     :                     WAVE( IFIRST - LASTBR ) ) / ( LASTBR - 1 )

                  NADD = NINT( ( WAVE( IFIRST ) - WAVE( IFIRST - 1 ) ) /
     :                   DELX ) - 1
                  IF ( NADD .LT. 1 ) NADD = 1
               ELSE
                  NADD = 1
               END IF

*  Keep track of how many bad pixels have been inserted.
               NADDSM = NADDSM + NADD

*  Insert the NADD bad pixels
               DO I = 1, NADD
                  KBAD = IFIRST + NADDSM - NADD - 1
                  WAVE1( KBAD + I ) = WAVE( IFIRST - 1 ) +
     :                                REAL( I ) * DELX
                  FLUX1( KBAD + I ) = VAL__BADR
               END DO
               LASTBR = ILAST - IFIRST + 1
            END IF

*  Copy in the good pixels from this break.
            DO KOUNT = IFIRST, ILAST
               WAVE1( KOUNT + NADDSM ) = WAVE( KOUNT )
               FLUX1( KOUNT + NADDSM ) = FLUX( KOUNT )
            END DO

         END DO

*  Evaluate total number of elements in corrected data array.
         NCOR = NPTS + NADDSM
      END IF


*   Check workspace array was big enough for padded array.
      IF ( NCOR .GT. NMAX ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CON_DIPRD_WORKSPACE',
     :                 'Ran out of workspace', STATUS )
      END IF

  999 CONTINUE

*  Report any I/O error.
      IF ( IOS .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_FIOER( 'MSG', IOS )
         CALL ERR_REP( 'CON_DIPRD_FIOER',
     :                 'Error reading Dipso data. ^MSG', STATUS )
      END IF

      END
