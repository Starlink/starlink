      SUBROUTINE HSTREP( HIST, HRMIN, HRMAX, NUMBIN, BINFAC, STATUS )
*+
*  Name:
*     HSTREP

*  Purpose:
*     Produces a summary of a histogram.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*     CALL HSTREP( HIST, HRMIN, HRMAX, NUMBIN, BINFAC, STATUS )

*  Description:
*     This routine produces a summary of the histogram of
*     an image or a sub-section of an image. The histogram
*     is compressed to 16 bins and is displayed to the user.

*  Arguments:
*     HIST( NUMBIN ) = INTEGER( READ )
*           The array holding the histogram.
*     HRMIN = REAL( READ )
*           The minimum value of the histogram.
*     HRMAX = REAL( READ )
*           The maximum value of the histogram.
*     NUMBIN = INTEGER( READ )
*           The number of bins in the histogram.
*     BINFAC = INTEGER( READ )
*           The number of bins in the compressed histogram.
*     STATUS = INTEGER( READ, WRITE )
*           The status value on entry to this subroutine.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     If there are insufficient bins for a compression to take place
*       then
*        Report error and set bad status
*     Else
*        Calculate the relative size of the bins for the report
*        Initialise output bin number and compressed histogram
*        For all input bins
*           If input bin number less than the current scaled bin number
*             then
*              Add input bin into compressed bin for current output bin
*                number
*           Else
*              No more input bins will fit into current compressed bin,
*               so store in next compressed bin and increment scaled and
*               output bin numbers
*           Endif
*        Enddo
*        Report title
*        For each compressed histogram bin
*           Report range and number in compressed bin
*        End do
*     Endif
*     Return

*  Type Definitions:
*     IMPLICIT NONE

*  Copyright:
*     Copyright (C) 1983, 1986, 1988 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     S.Chan ( RGVAD::KFH )
*     Malcolm Currie RAL ( UK.AC.RL.STAR::CUR )
*     {enter_new_authors_here}

*  History:
*     10 September 1983: Original ( RGVAD::KFH )
*     1986 Sep 20 : Renamed from KFH_HSTREP. Standardised to RAPI2D
*                   style; renamed parameters section to arguments and
*                   added access; relocated 'local' variables to import
*                   etc.; and tidied(RL.STAR::CUR).
*     1988 Jul 7  : BINFAC now an argument, constrained to work only
*                   when there really is a compression (old code was
*                   not doing the job), added error report; fixed range
*                   bug and expanded the "Method :" section
*                   (RL.STAR::CUR).
*     {enter_further_changes_here}

*-

*  Global Constants:

      INCLUDE 'SAE_PAR'

*  Arguments Given:

      INTEGER
     :    BINFAC,
     :    NUMBIN,
     :    HIST( NUMBIN )

      REAL
     :    HRMIN,
     :    HRMAX

*  Status:

      INTEGER STATUS

*  Local Constants:

      INTEGER
     :  BINMAX                  ! maximum number of compressed bins for
                                ! histogram summary
      PARAMETER ( BINMAX = 30 )

*  Local Variables:

      REAL
     :    BINSZ,                ! The size of the bins in the histogram
                                ! report
     :    NBINWD,               ! Width of the report bins
     :    RANGE,                ! The difference between the maximum
                                ! and the minimum values
     :    SIZBIN,               ! Size of summary bins
     :    SBIN,                 ! Size of summary bins used in an
                                ! incremental capacity
     :    SZBIN                 ! Count of the summary bins

      INTEGER
     :    I, J, K,              ! general variables
     :    NHIST( BINMAX )       ! Array holding the histogram summary

      CHARACTER*50 TEXT         ! summary of the histogram

*.

*    If the status is bad, then return

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that there can be a compression

      IF ( NUMBIN .LT. BINFAC ) THEN

*       Report error and set a bad status

         STATUS = SAI__ERROR
         CALL MSG_SETI( 'BINFAC', BINFAC )
         CALL ERR_REP( 'ERR_HSTREP_ISFBIN',
     :     'HSTREP: Insufficient bins in the histogram. Cannot '/
     :     /'compress histogram. Must have at least ^BINFAC bins',
     :     STATUS )

      ELSE

*       Calculate the relative size of the bins for the report.

         SIZBIN = REAL( NUMBIN )/ REAL( BINFAC )
         SBIN = SIZBIN

         SZBIN = SIZBIN

*       Initialise the new compressed histogram.

         DO  I = 1, BINFAC, 1
            NHIST( I ) = 0
         END DO

*       Compression mechanism.

         I = 1

         DO  J = 1, NUMBIN, 1

*          include bins less than the current scaled bin number, which
*          in general is not an integer

            IF ( J .LE. INT( SZBIN ) ) THEN

               NHIST( I ) = NHIST( I ) + HIST( J )

            ELSE

*             no more input bins will fit into current compressed bin,
*             so store in next compressed bin and increment scaled and
*             output bin numbers

               NHIST( I+1 ) = NHIST( I+1 ) + HIST( J )
               SZBIN = SZBIN + SBIN
               I = I + 1

            END IF

         END DO

*       Output results

         CALL MSG_OUT( 'LINE', ' ', STATUS )
         CALL MSG_OUT( 'TITLE', '           Summary of histogram',
     :                 STATUS )
         CALL MSG_OUT( 'LINE', ' ', STATUS )

         RANGE = HRMAX - HRMIN
         BINSZ = RANGE / REAL( BINFAC )
         NBINWD = BINSZ

         DO  K = 1, BINFAC, 1

            WRITE ( TEXT, 10 ) HRMIN + ( K-1 )*NBINWD,
     :              HRMIN + K*NBINWD, NHIST( K )
  10        FORMAT( ' ', 1PG15.7, ' to', 1PG15.7, ' ', I8, ' pixels' )
            CALL MSG_OUT( 'OUTLINE', TEXT, STATUS )

         END DO

         CALL MSG_OUT( 'LINE', ' ', STATUS )
      END IF

      END
