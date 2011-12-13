
      SUBROUTINE MAG1_CODE(STATUS)
*+
*  Name:
*     MAG1_CODE

*  Purpose:
*     convert MIO status to MAG status value.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_CODE(STATUS)

*  Description:
*     An MAG status value is found corresponding to the supplied
*     MIO status value.

*  Arguments:
*     STATUS=INTEGER (Given and Returned)
*        On import it contains an error code from the MIO package which is
*        converted to the equivalent MAG error code for export.

*  Algorithm:
*     Go through the list of MIO codes, and return the associated MAG
*     status.

*  Copyright:
*     Copyright (C) 1983, 1985, 1991, 1993 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     21-Jul-1983:  Original. (UCL::SLW)
*     24-Sep-1985:  Revised and additional MIO codes. (RAL::AJC)
*     14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*    22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MAG_$CODE

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definition:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_ERR'         ! MAG Errors
      INCLUDE 'MIO_ERR'         ! MIO Errors

*  Status:
      INTEGER STATUS            ! status value suitable for applications

*  Local Constants:
      INTEGER MAXCDS            ! Maximum number of status values
      PARAMETER (MAXCDS=43)

*  Local Variables:
      INTEGER CODES(MAXCDS)     ! MIO codes
      INTEGER STATES(MAXCDS)    ! MAG status values
      INTEGER I                 ! Loop index

*  Local Data:
      DATA STATES(1)/MAG__TOOTD/, CODES(1)/MIO__TOOTD/
      DATA STATES(2)/MAG__ILLTD/, CODES(2)/MIO__ILLTD/
      DATA STATES(3)/MAG__ILLAC/, CODES(3)/MIO__ILLAC/
      DATA STATES(4)/MAG__NTOPN/, CODES(4)/MIO__NTOPN/
      DATA STATES(5)/MAG__IVACM/, CODES(5)/MIO__IVACM/
      DATA STATES(6)/MAG__BUFTB/, CODES(6)/MIO__BUFTB/
      DATA STATES(7)/MAG__IVRSZ/, CODES(7)/MIO__IVRSZ/
      DATA STATES(8)/MAG__IVBSZ/, CODES(8)/MIO__IVBSZ/
      DATA STATES(9)/MAG__NTMUL/, CODES(9)/MIO__NTMUL/
      DATA STATES(10)/MAG__IVBUF/, CODES(10)/MIO__IVBUF/
      DATA STATES(11)/MAG__ICBSZ/, CODES(11)/MIO__ICBSZ/
      DATA STATES(12)/MAG__NOREC/, CODES(12)/MIO__NOREC/
      DATA STATES(13)/MAG__ERROR/, CODES(13)/MIO__ERROR/
      DATA STATES(14)/MAG__NTSUP/, CODES(14)/MIO__NTSUP/
      DATA STATES(15)/MAG__DVALL/, CODES(15)/MIO__DVALL/
      DATA STATES(16)/MAG__DVNAL/, CODES(16)/MIO__DVNAL/
      DATA STATES(17)/MAG__NSHDV/, CODES(17)/MIO__NSHDV/
      DATA STATES(18)/MAG__DVASS/, CODES(18)/MIO__DVASS/
      DATA STATES(19)/MAG__ACVIO/, CODES(19)/MIO__ACVIO/
      DATA STATES(20)/MAG__BUFOV/, CODES(20)/MIO__BUFOV/
      DATA STATES(21)/MAG__DVMNT/, CODES(21)/MIO__DVMNT/
      DATA STATES(22)/MAG__REMOT/, CODES(22)/MIO__REMOT/
      DATA STATES(23)/MAG__NOPRV/, CODES(23)/MIO__NOPRV/
      DATA STATES(24)/MAG__INVCH/, CODES(24)/MIO__INVCH/
      DATA STATES(25)/MAG__EXQUO/, CODES(25)/MIO__EXQUO/
      DATA STATES(26)/MAG__EXCCH/, CODES(26)/MIO__EXCCH/
      DATA STATES(27)/MAG__INVDV/, CODES(27)/MIO__INVDV/
      DATA STATES(28)/MAG__INVLG/, CODES(28)/MIO__INVLG/
      DATA STATES(29)/MAG__CTLER/, CODES(29)/MIO__CTLER/
      DATA STATES(30)/MAG__DTCHK/, CODES(30)/MIO__DTCHK/
      DATA STATES(31)/MAG__DRVER/, CODES(31)/MIO__DRVER/
      DATA STATES(32)/MAG__EOF/, CODES(32)/MIO__EOF/
      DATA STATES(33)/MAG__EOT/, CODES(33)/MIO__EOT/
      DATA STATES(34)/MAG__EOV/, CODES(34)/MIO__EOV/
      DATA STATES(35)/MAG__MDOFL/, CODES(35)/MIO__MDOFL/
      DATA STATES(36)/MAG__PARIT/, CODES(36)/MIO__PARIT/
      DATA STATES(37)/MAG__UNSAF/, CODES(37)/MIO__UNSAF/
      DATA STATES(38)/MAG__VLINV/, CODES(38)/MIO__VLINV/
      DATA STATES(39)/MAG__WTLCK/, CODES(39)/MIO__WTLCK/
      DATA STATES(40)/MAG__DATOV/, CODES(40)/MIO__DATOV/
      DATA STATES(41)/MAG__DVNMT/, CODES(41)/MIO__DVNMT/
      DATA STATES(42)/MAG__UNKSS/, CODES(42)/MIO__UNKSS/
      DATA STATES(43)/MAG__BADSS/, CODES(43)/MIO__BADSS/

*.


      DO 100 I = 1, MAXCDS
         IF ( STATUS.EQ.CODES(I) ) THEN
            STATUS = STATES(I)
            GO TO 1
         END IF
 100  CONTINUE
 1    CONTINUE

      RETURN
      END
