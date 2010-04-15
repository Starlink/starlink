*+  KFH_HELP - Help routine for the INSPECT application.
      SUBROUTINE KFH_HELP
*    Description :
*     This routine lists the options that are available
*     in the INSPECT application.
*    Invocation :
*     CALL KFH_HELP
*    Method :
*     It is simply a listing of the options together with
*     a brief description of what the various options do.
*    Author :
*     S.Chan (RGVAD::KFH)
*    History :
*     11 October 1983: Original (RGVAD::KFH)
*-

*
*    List the OPTIONS available.
*

      CALL MSG_OUT('MSG1',' ',STATUS)
      CALL MSG_OUT('MSG2','  The OPTIONS that are'/
     : /' available are :-',STATUS)
      CALL MSG_OUT('MSG3',' ',STATUS)

      CALL MSG_OUT('MSG4','     ''RE'' - This defines a'/
     : /' region or sub-section of the image on which',STATUS)
      CALL MSG_OUT('MSG4','            the options'/
     : /' LIST,STATS and HIST will operate.',STATUS)
      CALL MSG_OUT('MSG4',' ',STATUS)

      CALL MSG_OUT('MSG5','     ''SA'' - This allows the storage'/
     : /' of the current region.',STATUS)
      CALL MSG_OUT('MSG5',' ',STATUS)

      CALL MSG_OUT('MSG6','     ''DE'' - This allows the'/
     : /' selection of a graphics device.',STATUS)
      CALL MSG_OUT('MSG6',' ',STATUS)

      CALL MSG_OUT('MSG7','     ''VA'' - This determines'/
     : /' the value of a specified pixel of the image.',STATUS)
      CALL MSG_OUT('MSG7',' ',STATUS)

      CALL MSG_OUT('MSG8','     ''PE'' - This produces a'/
     : /' formatted listing of a 9x9 section of the image',STATUS)
      CALL MSG_OUT('MSG8','            centred around'/
     : /' a specified pixel.',STATUS)
      CALL MSG_OUT('MSG8',' ',STATUS)

      CALL MSG_OUT('MSG9','     ''LI'' - This gives a'/
     : /' formatted listing of the region selected.',STATUS)
      CALL MSG_OUT('MSG9',' ',STATUS)

      CALL MSG_OUT('MSG10','     ''ST'' - This calculates'/
     : /' the key statistical parameters of the chosen',STATUS)
      CALL MSG_OUT('MSG10','            region.',STATUS)
      CALL MSG_OUT('MSG10',' ',STATUS)

      CALL MSG_OUT('MSG11','     ''HI'' - This calculates'/
     : /' the histogram of the region and gives a summary',STATUS)
      CALL MSG_OUT('MSG11','            of it.',STATUS)
      CALL MSG_OUT('MSG11',' ',STATUS)

      CALL MSG_OUT('MSG12','     ''SL'' - This produces a'/
     : /' slice through the image between two points',STATUS)
      CALL MSG_OUT('MSG12','            provided by the user.',
     : STATUS)
      CALL MSG_OUT('MSG12',' ',STATUS)

      CALL MSG_OUT('MSG13','     ''EX'' - This allows the'/
     : /' user to exit from the program.',STATUS)
      CALL MSG_OUT('MSG13',' ',STATUS)

      CALL MSG_OUT('MSG14',' ',STATUS)

      END
