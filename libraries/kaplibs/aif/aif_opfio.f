      SUBROUTINE AIF_OPFIO( FILNAM, ACMODE, FORM, RECSZ, FD, OPEN,
     :                      STATUS )
*+
*  Name:
*     AIF_OPFIO

*  Purpose:
*     Opens a Fortran sequential file by name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AIF_OPFIO( FILNAM, ACMODE, FORM, RECSZ, FD, OPEN, STATUS )

*  Description:
*     This routine opens a sequential file via FIO_OPEN.  Up to four
*     attempts may be made to open the file.  If a null response is
*     supplied the file is not opened, and the flag returned indicates
*     this fact.

*  Arguments:
*     FILNAM = CHARACTER*(*)
*         The name of the file to be opened.
*     ACMODE = CHARACTER*(*)
*         Expression giving the required access mode.
*           Valid modes are: 'READ', 'WRITE', 'UPDATE' and 'APPEND'.
*           For details, see FIO_OPEN.
*     FORM = CHARACTER*(*)( READ )
*         Expression giving the required formatting of the file.
*           Valid formats are: 'FORTRAN', 'LIST', 'NONE' and
*           'UNFORMATTED'. For details, see FIO_OPEN.
*     RECSZ = INTEGER( READ )
*         Expression giving the maximum record size in bytes.
*           Set it to zero if the Fortran default is required.
*     FD = INTEGER( WRITE )
*         Variable to contain the file descriptor.
*     OPEN = LOGICAL( WRITE )
*         If true the file has been opened.
*     STATUS = INTEGER( READ, WRITE )
*         Global status value
*
*  Method:
*     Check for error on entry - return if not o.k.
*     Initialise looping flag
*     Do while no error obtaining the name and opening the output file
*       and maximum number of attempts not exceeded
*        Get file name and open file
*        If error occurred then
*           If abort requested, do so
*           Increment loop counter
*           If maximum number of attempts not exceeded then
*              Report error
*           Else
*              Set looping flag to exit
*           Endif
*        Else
*           Set flag to indicate that the file has been opened
*           Set looping flag to false
*        Endif
*     Enddo
*     If error then
*        Report and abort
*     Endif
*     Return

*  Bugs:
*     None known.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     Malcolm Currie   STARLINK (RAL::CUR)

*  History:
*     1989 Jul 25: Original (RAL::CUR).
*     1990 Feb 20: Obtains the file by name instead of by parameter
*                  (RAL::CUR).
*-

*    Type definitions :

      IMPLICIT  NONE           ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'PAR_ERR'       ! parameter-system errors

*    Import :

      CHARACTER*(*)
     :  FILNAM,                ! File Name
     :  ACMODE,                ! File access mode
     :  FORM                   ! Required form of carriagecontrol

      INTEGER
     :  RECSZ                  ! File record size

*    Export :

      INTEGER
     :  FD                     ! File descriptor

*    Status :

      INTEGER  STATUS

*    Local Constants :

      INTEGER
     :    MXLOOP               ! Maximum number of attempts at
                               ! opening a data file
      PARAMETER ( MXLOOP = 4 )

      INTEGER
     :    LOOP                 ! Number of attempts to open the file

      LOGICAL                  ! true if:
     :    LOOPAG,              ! Loop again to open output file
     :    OPEN                 ! File opened successfully

*-

*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      LOOP = 0
      LOOPAG = .TRUE.
      OPEN = .FALSE.
      DO WHILE ( LOOPAG )

*       attempt to obtain and open a file to output listing

         CALL FIO_OPEN( FILNAM, ACMODE, FORM, RECSZ, FD, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN

            IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*         Here if filename is not allowed or file is not opened
*         - try again
*         Need to flush error here, as not quitting routine

            LOOP = LOOP + 1
            IF ( LOOP .LE. MXLOOP ) THEN
               CALL MSG_SETC( 'FILNAM', FILNAM )
               CALL ERR_REP( 'ERR_AIF_OPFIO_NOFI',
     :           'AIF_OPFIO: Could not open file ^FILNAM - try again',
     :           STATUS )
               CALL ERR_FLUSH( STATUS )
            ELSE

*             end looping as user is having serious problems

               LOOPAG = .FALSE.
            END IF

         ELSE

*          no problem, so exit loop

            LOOPAG = .FALSE.
            OPEN = .TRUE.

*       end of file-opened-successfully check

         END IF
      END DO

*    abort for repeated error

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_AIF_OPFIO_NOOPEN',
     :     'AIF_OPFIO: Repeatedly unable to open a file.', STATUS )
      END IF

 999  CONTINUE

      END
