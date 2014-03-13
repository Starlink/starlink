#include "help.h"
#include "hlpsys.h"

char* hlpErrmes ( int nerror )
/*
**  - - - - - - - - - -
**   h l p E r r m e s
**  - - - - - - - - - -
**
**  Translate HELP system error code into message.
**
**  Given (argument):
**     nerror    int         error code
**
**  Returned (function value)
**               char*       message (may be up to 50 characters)
**
**  The HELP system error codes are defined in the hlpsys.h header
**  file.
**
**  Last revision:   14 January 2008
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   switch ( nerror ) {
   case 0:
      return "HLP: OK";
   case hlp_ILLEGAL_STATE:
      return "HLP: help system in wrong state";
   case hlp_OPEN_ERROR:
      return "HLP: help library error on open";
   case hlp_WRITE_ERROR:
      return "HLP: help library error on write";
   case hlp_READ_ERROR:
      return "HLP: help library error on read";
   case hlp_CLOSE_ERROR:
      return "HLP: help library error on close";
   case hlp_WRITE_WIDE:
      return "HLP: attempt to write outside help library";
   case hlp_READ_WIDE:
      return "HLP: attempt to read outside help library";
   case hlp_RECORD_OVERSIZE:
      return "HLP: help record overflows supplied string";
   case hlp_CREATION_FAILURE:
      return "HLP: help library creation failure";
   case hlp_INTERNAL_ERROR:
      return "HLP: help illegal status";
   case hlp_ILLEGAL_LEVEL:
      return "HLP: illegal current level";
   case hlp_LINE_OUTPUT_BAD:
      return "HLP: line output failure";
   case hlp_LINE_INPUT_BAD:
      return "HLP: line input failure";
   case hlp_BAD_INDEX:
      return "HLP: invalid index entry";
   case hlp_SELF_REF:
      return "HLP: attempted switch to current library";
   case hlp_STRING_OVERFLOW:
      return "HLP: string too small";
   case hlp_TRANSLATE_ERROR:
      return "HLP: file name translation error";
   default:
      return "HLP: illegal status";
   }
}
