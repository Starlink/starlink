      SUBROUTINE CON_DIPWR( UNIT, TITLE, NPTS, FLUX, WAVE, FLUX1, WAVE1,
     :                  BAD, BREAK, MAXBRK, STATUS )
*+
*  Name:
*     DIPWR

*  Purpose:
*     Writes the data from an NDF array to a DIPSO format file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DIPWR( UNIT, TITLE, NPTS, FLUX, WAVE, FLUX1, WAVE1, BAD, 
*                 BREAK, MAXBRK, STATUS )

*  Description:
*     The routine writes the main data and axis(1) arrays from an NDF
*     to a Dipso format file. Bad pixels are treated by inserting
*     `breaks' in the data. The number and position of these breaks
*     is recorded in the Dipso file.

*  Arguments:
*     UNIT = INTEGER (Given)
*        Logical unit number for the output Fortran file.
*     TITLE = CHARACTER (Given)
*        Title of data set.
*     NPTS = INTEGER (Given)
*        Number of array elements to process.
*     FLUX( NPTS ) = REAL (Given)
*        Input NDF data array.
*     WAVE( NPTS ) = REAL (Given)
*        Input NDF AXIS(1) array.
*     FLUX1( NPTS ) = REAL (Returned)
*        Data array with bad elements removed.
*     WAVE( NPTS ) = REAL (Returned)
*        Wavelength array with bad elements removed.
*     BAD = LOGICAL (Given)
*        True if NDF data array  contained bad pixels.
*     BREAK(MAXBRK) =
*     MAXBRK
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council. All
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     Jo Murray (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 August 20th(JM):
*        Original version.
*     {enter_further_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

      
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad pixel values

       
*  Arguments Given:
      CHARACTER*(*) TITLE
      INTEGER       NPTS
      REAL          FLUX(NPTS) 
      REAL          WAVE(NPTS)            ! Input wavelength array
      REAL          FLUX1(NPTS)
      REAL          WAVE1(NPTS)
      LOGICAL       BAD                ! True if data array has bad pixels
      INTEGER       STATUS             ! Global status
      INTEGER       UNIT             ! Logical unit number for Dipso file

*   Local variables.
      INTEGER MAXBRK
      INTEGER NPTS1
      INTEGER IOS
      INTEGER NBREAK
      REAL WORV
      INTEGER I
      INTEGER BREAK(MAXBRK)
      INTEGER  KOUNT
      LOGICAL       BREAKCUR

*  Check inherited global status.
      
      IF ( STATUS .NE. SAI__OK ) RETURN

      
       WORV=1.0

*  Work out breaks from bad pixels.
      IF(.NOT.BAD)THEN
*      No bad pixels
         NBREAK=1
      ELSE
*      Check each value to find bad pixels.
         NBREAK=0
         KOUNT=0
         BREAKCUR=.FALSE.
         DO I=1,NPTS
*         Good value
            IF(FLUX(I).NE.VAL__BADR)THEN
               KOUNT=KOUNT+1
               FLUX1(KOUNT)=FLUX(I)
               WAVE1(KOUNT)=WAVE(I)
               BREAKCUR=.FALSE.
            ELSE
*         Bad value
               IF(.NOT.BREAKCUR)THEN
                  NBREAK=NBREAK+1
                  BREAK(NBREAK)=KOUNT
                  BREAKCUR=.TRUE.
               ENDIF
            ENDIF
         ENDDO
         NBREAK=NBREAK+1
         BREAK(NBREAK)=KOUNT
         NPTS1=KOUNT
      ENDIF


*  Check the number of BREAKS does not exceed the DIPSO limit of 1000.
      IF(NBREAK.GT.1000)THEN
         STATUS=SAI__ERROR
         CALL ERR_REP('NDF2DIPSO_TOO_MANY_BREAKS', 
     :                'Too many breaks in data.',
     :                 STATUS)
         GOTO 999
      ENDIF    

*   Write data out in format suitable for DIPSO 'READ' command.
      WRITE (UNIT, IOSTAT=IOS) TITLE
      IF (IOS.EQ.0) THEN
         WRITE (UNIT, IOSTAT=IOS) NBREAK, (BREAK(I),I=1,NBREAK)
         IF (IOS.EQ.0) THEN
            IF(.NOT.BAD)THEN
               WRITE (UNIT, IOSTAT=IOS) 
     :                  (WAVE(I),FLUX(I),I=1,NPTS)
            ELSE
               WRITE (UNIT, IOSTAT=IOS) 
     :                   (WAVE1(I),FLUX1(I),I=1,NPTS1)
            ENDIF
            IF (IOS.EQ.0) THEN
               WRITE (UNIT,  IOSTAT=IOS) WORV
            ENDIF
         ENDIF
      ENDIF

*   If I/O error has occurred set STATUS.
      IF (IOS.NE.0) THEN
         STATUS=SAI__ERROR   
*      Translate IOS into a message token and make an error report.
         CALL ERR_FIOER ('MSG', IOS)
         CALL ERR_REP ('DIPWR_IOERR', '^MSG', STATUS)         
      ENDIF

999   CONTINUE

      END
