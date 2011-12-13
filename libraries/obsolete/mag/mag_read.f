      SUBROUTINE MAG_READ(TP, BUFSIZ, BUFFER, NREAD, STATUS)
*+
*  Name:
*     MAG_READ

*  Purpose:
*     Read a block from tape.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_READ(TD, MAXVAL, VALUES, ACTVAL, STATUS)

*  Description:
*     Read a tape block into the array provided, and return its length
*     in basic machine units (bytes).

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     MAXVAL=INTEGER (Given)
*        Expression specifying the size of the array (in bytes)
*        into which data are to be read from tape.
*     VALUES(MAXVAL)=BYTE (Returned)
*        Array to receive the binary contents of a tape block.
*        It must be of sufficient size to contain the entire block.
*     ACTVAL=INTEGER (Returned)
*        Variable to receive the actual number of bytes read.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If this variable is not SAI__OK on input, then the routine
*        will return without action.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.

*  Algorithm:
*     Obtain the tape descriptor for the physical tape drive and read
*     in a block. Re-adjust the current tape position if necessary.

*  Copyright:
*     Copyright (C) 1980, 1981, 1983, 1986, 1989, 1991, 1993 Science & Engineering Research Council.
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
*     Sid Wright (UCL::SLW)
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     14-APR-1980:  Original.  (UCL::SLW)
*     11-SEP-1981:  Added code to remember tape position.  (UCL::JRG)
*     01-FEB-1983:  Fortran 77 Version. (UCL::JRG)
*     11-Jun-1983:  Remove machine-dependent I/O and tidy up. (UCL::SLW)
*      6-Nov-1986:  Shorten comment lines for DOMAN  (RAL::AJC)
*     24-Jan-1989:  Improve documentation  (RAL::AJC)
*     14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*    22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definition:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MIO_ERR'         ! MIO Errors

*  Arguments Given:
      INTEGER TP                ! tape descriptor
      INTEGER BUFSIZ            ! size of buffer in bytes

*  Arguments Returned:
      BYTE BUFFER(BUFSIZ)       ! buffer to take bytes read
      INTEGER NREAD             ! number of bytes read
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGIO_CMN'       ! MAG library states

*  External References:
      EXTERNAL MAG1_BLK          ! Block data subprogram that
                                 ! initializes MAGINT
*  Local Variables:
      INTEGER TD                ! Physical tape descriptor

*.


      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MAG1_GETTD(TP, TD, STATUS)
         IF ( STATUS.EQ.SAI__OK ) THEN
            CALL MIO_BREAD(TD, BUFSIZ, BUFFER, NREAD, STATUS)
            TMOD(TP) = .TRUE.
            IF ( STATUS.EQ.SAI__OK .OR. STATUS.EQ.MIO__PARIT .OR.
     :           STATUS.EQ.MIO__DATOV ) THEN
               IF ( TBLOCK(TP).GT.0 ) THEN
                  IF ( TSTART(TP) ) THEN
                     TBLOCK(TP) = TBLOCK(TP) + 1
                  ELSE IF ( .NOT.TSTART(TP) ) THEN
                     TBLOCK(TP) = TBLOCK(TP) - 1
                  END IF
               END IF
            ELSE IF ( STATUS.EQ.MIO__EOF .OR. STATUS.EQ.MIO__EOV ) THEN
               IF ( TFILE(TP).GT.0 ) TFILE(TP) = TFILE(TP) + 1
               TSTART(TP) = .TRUE.
               TBLOCK(TP) = 1
            ELSE
               TFILE(TP) = 0
               TBLOCK(TP) = 0
            END IF
            IF ( STATUS.NE.SAI__OK ) CALL MAG1_ERRTP(TP, STATUS)
         END IF
      END IF

      RETURN
      END
