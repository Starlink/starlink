*-----------------------------------------------------------------------
*+  DAOCURS - Put up cursor over an image created by DAOGREY
      subroutine daocurs( status )
*
*    Description :
*     Display a cursor over an image created by DAOGREY. Positions are
*     measured and output to the results file until the break key is
*     pressed.
*
*    Invocation :
*     call daocurs( status )
*
*    Parameters :
*     rfile=character*(30)(given)
*           name of file for results
*     device=device(given)
*           The image display
*
*    Method :
*     Check status on entry.
*     Get the file name for the results from the parameter system and
*     open it for writing with FIO.
*     Get the last database entry of style DAOGREY.
*     Create a new zone equal in size and shape to the database entry.
*     Loop around requesting the cursor position and outputting the
*     results until the break key is pressed.
*     Close the results file and SGS.
*
*    Bugs :
*     None known.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     26 May 1988
*     22 Nov 1988  Use released versions of AGI and FIO
*     21 Jan 1991  Use new version of AGI
*     19 Aug 1991  Added call to OUTMEN
*    endhistory
*
*    Type Definitions :
      implicit none

*    Global constants :
      include 'SAE_PAR'

*    Status :
      integer status

*    Local Constants :
      integer fcols
      parameter ( fcols = 24 )

*    Local variables :
      character text * ( fcols )
      integer basid, fout, ich, izone, n, picid1, picid2
      real x, y
*-

* Check the input status value
      if ( status .ne. SAI__OK ) goto 99

* Get the name of the file for the results
      call FIO_ASSOC( 'RFILE', 'WRITE', 'LIST', fcols, fout, status )

* Open up AGI and recall the last picture of name 'DAOGREY'
      call AGI_BEGIN
      call AGI_ASSOC( 'DEVICE', 'UPDATE', picid1, status )
      call AGI_IBASE( basid, status )
      call AGI_SELP( basid, status )
      call AGI_RCL( 'DAOGREY', picid2, status )

* Create an SGS zone for this picture
      call AGS_ACTIV( status )
      call AGS_NZONE( izone, status )
      if ( status .ne. SAI__OK ) goto 99

* Indicate the selection options
      call OUTMEN

* Request cursor position until break key is pressed
      ich = 99
      n = 0
      do while ( ich .gt. 0 )
         call SGS_REQCU( x, y, ich )
         if ( ich .gt. 0 ) then
            n = n + 1
            write( text, '( 1x, i5, 2f9.2 )' ) n, x, y
            call FIO_WRITE( fout, text, status )
            call MSG_OUT( ' ', text, status )
         endif
      enddo

* Close results file and SGS
      call FIO_CANCL( 'RFILE', status )
      call FIO_DEACT( status )

      call AGS_DEACT( status )
      call AGI_CANCL( 'DEVICE', status )
      call AGI_END( -1, status )

  99  continue

      end

*-----------------------------------------------------------------------

      SUBROUTINE OUTMEN

*+
*   ------
*   OUTMEN
*   ------
*
*   This informs which buttons operate the cursor selection.
*
*   External
*       MSG_OUT
*
*   Nick Eaton  Aug 1991
*+

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

      INTEGER NCHOI, STATUS

      STATUS = SAI__OK

*   Inquire the number of choices on the current device
      CALL SGS_INCHO( 1, NCHOI )

*   If the number of choices is 4 or less then assume the device has
*   a mouse
      IF ( NCHOI .LE. 4 ) THEN
         CALL MSG_OUT( ' ',
     :    'Select position - Press left hand mouse button', STATUS )
         CALL MSG_OUT( ' ',
     :    'Exit            - Press right hand mouse button', STATUS )

*   Otherwise assume it is running from on a terminal
      ELSE
         CALL MSG_OUT( ' ',
     :    'Select position - Press number 1 on keyboard', STATUS )
         CALL MSG_OUT( ' ',
     :    'Exit            - Press number 0 on keyboard', STATUS )
      ENDIF

      END

