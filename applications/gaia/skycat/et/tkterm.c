/*
** The Tk-Term terminal program
**
** By: D. Richard Hipp
**
** For the public domain...
*/
#include "tk.h"
#include <stdlib.h>
#include <stdio.h>

/* The following is the file descriptor used for communicating with the
** pseudo-tty and the process running on the other side of the pty. */
static int iChildFd;

/* This is the default command to run on the other side of the pty
** if no command is specified on the tkterm command line */
static char *defaultArgv[] = {"/bin/sh", 0};

/* The following variables hold the width and height of the visible
** terminal screen, in characters.  These variables are linked to
** variables with the same name (expect the leading "i") in the Tcl/Tk
** code */
static int iWidth = 80;
static int iHeight = 24;

/* The following variables hold information about the current position
** of the cursor and the state of the virtual VT100 terminal.
**
** iCurX, iCurY     The location of the cursor in the text widget.  The
**                  text widgets address space is used.  This means that
**                  the top line is where iCurY==1 and the leftmost
**                  character is iCurX==0.  (The cursor is actually in the
**                  space immediately prior to iCurX,iCurY.)
**
** iBufX, iBufY     The place in the text widget where the text currently
**                  accumulating in zLineBuf[] should be written.
**
** iCurSavedX,      The saved location of the cursor.  Used by
** iCurSavedY       ESC-7 and ESC-8
**
** iBtm             The bottom line of text in the whole text widget.
**                  (This is NOT the bottom line of visible text.  The
**                  bottom line of visible text is iTop + iHeight).  This
**                  is always the same as "ET_INT(.t index end) - 1".
**
** iTop             The first line above the topmost visible line of text.
**                  This value can be 0 if the text widget is scrolled
**                  all the way to the top.  (This variable doesn't really
**                  exists.  It is used for commenting only.)
**
** xProcess         Pointer to the routine used to process the next
**                  character which is input to the vt100 emulator.
**
** iScrollTop,      The top and bottom of the scrolling region on the
** iScrollBtm       screen.  The numbers are stored as vt100 indices,
**                  so 1 means the top line and iHeight means the bottom
**                  line.   iScrollTop==1 && iScrollBtm==iHeight means
**                  the whole screen scrolls.
**
** bReplaceMode     If True, then new characters overwrite old ones.  When
**                  False, the new characters are inserted.
**
** iMaxLines        The number of lines of text to preserve in the scrollback.
*/
static int iBtm;               /* Bottom line of text in the text widget */
static int iCurX, iCurY;       /* X and Y coordinates of the cursor */
static int iBufX, iBufY;       /* X and Y coordinates at the beginning of
                               ** the line insertion buffer */
static int iCurSavedX,         /* Saved X and Y coordinates */
           iCurSavedY;
static void (*xProcess)(char); /* Use this routine to process the next char */
static int iScrollTop = 1,     /* The bounds of the scrolling region */
           iScrollBtm = 24;  
static int bReplaceMode = 1;   /* True to overwrite characters */
static int iMaxLines = 1000;   /* Number of lines to preserve */ 
extern int iChildPid;          /* Process number of the shell */

/* A NOTE ON INDEXING:
**
** The text widget calls its top line 1 and its leftmost character 0, but
** The vt100 escape codes use 1 for both the topmost and leftmost position.
** Furthermore, the vt100 position is relative to the topmost visible
** line of the screen, but the text widget refers to the topmost line 
** in the scrollback text.
**
** The following are invariants:
**
**        TextWidgetY = iCurY
**             Vt100Y = iCurY + iTop
**
**        TextWidgetX = iCurX
**             Vt100X = iCurX + 1
*/

/* Sanity checking
*/
#ifdef DEBUG
#define ASSERT(X)  if(!(X)){ShouldntHappen(__LINE__,__FILE__);}
#else
#define ASSERT(X)
#endif

/* The virtual VT100 mechanism uses line buffering to reduce the amount
** of calls to the text widget.  The following variables hold the line
** buffer.
*/
static char zLineBuf[200];
static int iBufWidth = 0;     /* Number of characters in the buffer */

/* The next variables are used to hold parameters specified in an
** ESC[Ps;Ps;...  control sequence.
*/
static int nParam;            /* Number of parameters seen */
static int aiParam[10];       /* Value of each parameter.  -1==default */

/* If the following variable is true, each character received by
** the vt100 emulator is printed on standard output.  This variable
** is linked to the Tcl/Tk variable named "Debug" so that it can
** be turned on and off using the "send" command. */
#ifdef DEBUG
static int bDebug = 0;
#endif

/* The following variables holds attributes of all characters which are
** to be inserted into the text widget
*/
static enum _BufTag {
  BT_Normal,
  BT_Underline,
  BT_Bold,
  BT_Inverse,
} eBufTag = BT_Normal;

#ifdef DEBUG
/* This routine should never be called */
static void ShouldntHappen(int line, char *file){
  fprintf(stderr,"Assertion failed on line %d of %s\n",line,file);
}
#endif

/* Forward declarations */
static void HandleInput(void*,int);

/* Procedures defined in getpty.c */
extern int RunCommand(char *, char **, int);
extern void KillChildProcess(void);

/* Tcl/Tk will sometimes attempt to change environment variables.  Hence,
** the following string must be writeable -- it cannot be a string literal
** because string literals are read-only.
*/
static char zTermEnv[] = "TERM=xterm";

/* If this variable is TRUE it causes the GUI to hang around after
** the shell dies.
*/
static int persist = 0;

/*
** Print a usage comment and die
*/
static void usage(char *argv0){
  fprintf(stderr,"Usage: %s [options]\n",argv0);
  fprintf(stderr,
    "Options:\n"
    "   -title TITLE        Set the initial window title\n"
    "   -iconname NAME      Text seen when window is iconified\n"
    "   -iconic             Window is initially an icon\n"
    "   -width WIDTH        Set width of window in characters\n"
    "   -height HEIGHT      Set height of window in characters\n"
    "   -display DISPLAY    Specify the X server\n"
    "   -shell COMMAND...   Use an alternative shell\n"
    "   -persist            Keep the window visible after COMMAND exists\n"
  );
  exit(1);
}

int main(int argc, char **argv){
  static void ProcessNormal(char);
  int i;
  int initIconic = 0;

  Et_Init(&argc,argv);
  ET(wm withdraw .);
  Tcl_LinkVar(Et_Interp,"Width",(char*)&iWidth,TCL_LINK_INT);
  Tcl_LinkVar(Et_Interp,"Height",(char*)&iHeight,TCL_LINK_INT);
  Tcl_LinkVar(Et_Interp,"CurX",(char*)&iCurX,TCL_LINK_INT);
  Tcl_LinkVar(Et_Interp,"CurY",(char*)&iCurY,TCL_LINK_INT);
  Tcl_LinkVar(Et_Interp,"ChildPid",(char*)&iChildPid,TCL_LINK_INT);
#ifdef DEBUG
  Tcl_LinkVar(Et_Interp,"Debug",(char*)&bDebug,TCL_LINK_BOOLEAN);
  Tcl_LinkVar(Et_Interp,"Btm",(char*)&iBtm,TCL_LINK_INT);
  Tcl_LinkVar(Et_Interp,"ScrollTop",(char*)&iScrollTop,TCL_LINK_INT);
  Tcl_LinkVar(Et_Interp,"ScrollBtm",(char*)&iScrollBtm,TCL_LINK_INT);
#endif
  iScrollTop = iCurY = iBtm = 1;
  iScrollBtm = iHeight;
  ET_INSTALL_COMMANDS;
  ET_INCLUDE( tkterm.tcl );
  putenv(zTermEnv);
  for(i=1; i<argc; i++){
    if( strcmp(argv[i],"-title")==0 && i<argc-1 ){
      ET( wm title . "%q(argv[++i])" );
    }else if( strcmp(argv[i],"-iconname")==0 && i<argc-1 ){
      ET( wm iconname . "%q(argv[++i])" );
    }else if( strcmp(argv[i],"-iconic")==0 ){
      initIconic = 1;
    }else if( strcmp(argv[i],"-shell")==0 ){
      i++;
      break;
    }else if( strcmp(argv[i],"-width")==0 && i<argc-1 ){
      int x = atoi(argv[++i]);
      if( x<20 ) x = 20;
      if( x>200 ) x = 200;
      ET( ChangeWidth %d(x) );
    }else if( strcmp(argv[i],"-height")==0 && i<argc-1 ){
      int x = atoi(argv[++i]);
      if( x<4 ) x = 4;
      if( x>100 ) x = 100;
      ET( ChangeHeight %d(x) );
    }else if( strcmp(argv[i],"-persist")==0 ){
      persist = 1;
    }else if( strcmp(argv[i],"-display")==0 ){
      i++;
    }else{
      usage(argv[0]);
    }
  }
  if( i<argc ){
    iChildFd = RunCommand(argv[i],&argv[i],0);
  }else{
    iChildFd = RunCommand("/bin/sh",defaultArgv,0);
  }
  xProcess = ProcessNormal;
  if( iChildFd>=0 ){
    Tk_CreateFileHandler(iChildFd,TK_READABLE,HandleInput,0);
    chdir("/");  /* This effects the GUI only -- not the shell */
    if( initIconic ){
      ET(wm iconify .);
    }else{
      ET(wm deiconify .);
    }
    Et_MainLoop();
    KillChildProcess();
  }
  return 0;
}

/* This routine is called when the child process dies in order to
** do a clean shutdown of the application */
void CleanShutdown(void){
  Tk_DeleteFileHandler(iChildFd);
  /* ET( destroy . ); */
  KillChildProcess();
  iChildFd = 0;
  if( !persist ) exit(0);
}

/* Make sure the text position iX,iY really exists.  If the line iY
** doesn't exist, then add newlines until it does.  If character
** iX doesn't exists on line iY, then add spaces until it does.
** Return the actual length of line iY.
*/
static int MakePositionExist(int iX, int iY){
  int lineNum;
  int lineLen;
  int i;
  char zBuf[1000];  /* Number of elements must be greater than max width
                    ** and height of the text widget */

  if( iX > iWidth ) iX = iWidth;
  ASSERT( iY >= 1 && iX >= 0 );

  /* If line iY doesn't exists, then we'll have to create it. */  
  if( iBtm < iY ){
    for(i=0; i<iY - iBtm; i++) zBuf[i] = '\n';
    zBuf[i] = 0;
    ET(.t insert end {%s(zBuf)});
    iBtm = iY;
    lineLen = 0;
  }else{
    /* Find the amount of text on line iY. */
    sscanf( ET_STR(.t index {%d(iY).0 lineend}), "%d.%d", &lineNum,&lineLen);
    ASSERT( lineNum==iY );
    ASSERT( ET_INT(.t index end)==iBtm+1 );
  }

  /* If character position iX doesn't exists, add spaces until it does */
  if( iX > lineLen ){
    for(i=0; i<iX - lineLen; i++) zBuf[i] = ' ';
    zBuf[i] = 0;
    ET(.t insert %d(iY).%d(lineLen) {%s(zBuf)});
    lineLen = iX;
  }

  /* Return the actual length of line iY. */
  return lineLen;
}

/* Adjust the text widget so that it is scrolled all the way to the
** bottom.
*/
static void JumpToBottom(void){
  ET( .t yview moveto 1 );
}

/* Clip the number of lines in the text widget to keep the number
** of lines at or below iMaxLines
*/
static void ClipScrollback(void){
  if( iBtm > iMaxLines ){
    int extra = iBtm - iMaxLines - 1;
    iCurY -= extra;
    iBtm -= extra;
    if( iCurY<1 ) iCurY = 1;
    ET(.t delete 1.0 %d(extra+1).0);
  }
}

/* Transfer the text in zLineBuf[0..iBufWidth] into the text widget.
** at position iBufY.iBufX.
**
** If the number of lines in the text widget exceeds iMaxLines because of
** this operation, then lines are deleted from the beginning of the
** widget to get the total number of lines back to iMaxLines.
**
** This is the only way to remove characters from zLineBuf.  This is
** also the only way to add non-white-space characters to the screen.
*/
static void FlushBuffer(void){
  int lineLen;  /* The length of the line iBufY */

  /* Early out if nothing to do */
  if( iBufWidth==0 ) return;

  /* Make sure the position iBufX,iBufY really exists */
  lineLen = MakePositionExist(iBufX,iBufY);

  /* Prepare the line to receive text.  This may involve deleting some
  ** of the existing text (in overwrite mode), or deleting some text
  ** that will scroll off the right side of the screen in insert mode.
  */
  if( iBufX < lineLen ){
    if( bReplaceMode ){
      int end = iBufX + iBufWidth;
      if( end>lineLen ) end = lineLen;
      ET(.t delete %d(iBufY).%d(iBufX) %d(iBufY).%d(end));
    }else if( lineLen + iBufWidth > iWidth ){
      ET(.t delete %d(iBufY).%d(lineLen-iBufWidth) %d(iBufY).%d(lineLen));
    }
  }

  /* Add zLineBuf to the text widget at the appropriate spot.
  */
  zLineBuf[iBufWidth] = 0;
  ET(.t insert %d(iBufY).%d(iBufX) "%q(zLineBuf)");

  /* Tag the newly inserted text according to the current character mode. */
  /* ET(
    foreach t [.t tag names %d(iBufY).%d(iBufX)] {
      .t tag remove $t %d(iBufY).%d(iBufX) %d(iBufY).%d(iBufX+iBufWidth)
    }
  ); */
  switch( eBufTag ){
    case BT_Normal:
      break;
    case BT_Underline:
      ET( .t tag add ul %d(iBufY).%d(iBufX) %d(iBufY).%d(iBufX+iBufWidth) );
      break;
    case BT_Bold:
      ET( .t tag add bd %d(iBufY).%d(iBufX) %d(iBufY).%d(iBufX+iBufWidth) );
      break;
    case BT_Inverse:
      ET( .t tag add iv %d(iBufY).%d(iBufX) %d(iBufY).%d(iBufX+iBufWidth) );
      break;
  }
  iBufWidth = 0;
}

/* Display the cursor at its appropriate position
*/
static void DisplayCursor(void){
  MakePositionExist(iCurX,iCurY);
  ET(.t mark set insert %d(iCurY).%d(iCurX));
}

/* Index forward.  This means that the cursor needs to move down
** by one line.  Things can get complex here, due to the VT100
** scrolling region logic.  Only the lines between iScrollTop and
** iScrollBtm, inclusive, should scroll.  
**
** Remember that both iScrollTop and iScrollBtm are vt100-indices.
** That is to say, the top line of the visible part of text is
** line 1.  The cursor is on the top line of the scrolling region if:
**
**           iScrollTop == iCurY - iTop
**
** This routine prevents texts from flowing into the scrollback region
** (of the top of the viewing area) unless the scrolling region is the
** whole viewing area.
**
** The buffer should have already been flushed by the time this
** function is called.
*/
static void IndexForward(void){
  int vt100Y;            /* Cursor Y coordinate in VT100 space */
  int iTop;              

  iTop = iBtm - iHeight;
  if( iTop < 0 ) iTop = 0;
  vt100Y = iCurY - iTop;
  if( vt100Y<iScrollBtm ){
    iCurY++;
  }else if( iScrollBtm==vt100Y ){
    ET(.t insert {%d(iCurY).0 lineend} \n);
    if( iScrollTop>1 || iScrollBtm<iHeight ){
      int y = iScrollTop + iTop;
      ET(.t delete %d(y).0 %d(y+1).0);
    }else{
      iBtm++;
      iCurY++;
    }
  }else if( vt100Y<iHeight ){
    iCurY++;
  }else{
    /* Do nothing.  Let the next line of text overwrite the bottom line. */
  }
}

/* Index backwards.  Like the previous function, just in the opposite
** direction.
*/
static void IndexBackward(void){
  int vt100Y;   /* Y coordinate in VT100 space */
  int iTop;

  iTop = iBtm - iHeight;
  if( iTop < 0 ) iTop = 0;
  vt100Y = iCurY - iTop;
  if( vt100Y>iScrollTop ){
    iCurY--;
  }else if( vt100Y==iScrollTop ){
    int y;
    ET(.t insert %d(iCurY).0 \n);
    y = iScrollBtm + iTop;
    ET(.t delete {%d(y).0 lineend} {%d(y+1).0 lineend});
  }else if( vt100Y>1 ){
    iCurY--;
  }else{
    /* Do nothing.  Let the next line overwrite the top line */
  }
}

/* Add printable characters to the cache.  "c" is always a printable
** character here -- never a newline or tab or other control character.
**
** This is the only way in which text can be added to zLineBuf[].
*/
static void InsertChar(char c){
  static void ProcessNormal(char c);
  if( iCurX>=iWidth ){
    FlushBuffer();
    IndexForward();
    iCurX = 0;
  }
  if( iBufWidth >= sizeof(zLineBuf)-1 ){
    FlushBuffer();
  }
  if( iBufWidth==0 ){
    iBufX = iCurX;
    iBufY = iCurY;
  }
  zLineBuf[iBufWidth++] = c;
  iCurX++;
}

/* Process a character received while in the normal mode of operation.
**
** Add the character to the character cache.  If the character cache is
** full, then add the cache to the text widget.
*/
static void ProcessNormal(char c){
  static void ProcessEsc(char c);
  switch( c ){
    case 033:   /* ESC -- Escape */
      xProcess = ProcessEsc;
      FlushBuffer();
      break;
    case 007:   /* BEL -- Bell */
      XBell(Et_Display,0);
      break;
    case 010:   /* BS  -- Backspace */
      if( iCurX>0 ){
        FlushBuffer();
        iCurX--;
      }
      break;
    case 011:   /* HT  -- Horizontal Tab */
      FlushBuffer();
      iCurX = (iCurX+8)&~0x7;
      if( iCurX >= iWidth ){
        FlushBuffer();
        IndexForward();
        iCurX = 0;
      }
      break;
    case 012:   /* NL  -- New line */
    case 013:   /* VT  -- Vertical Tab */
    case 014:   /* FF  -- Form feed */
      FlushBuffer();
      IndexForward();
      break;
    case 015:   /* CR  -- Carriage return */
      FlushBuffer();
      iCurX = 0;
      break;
    default:
      if( c>=' ' && c<='~' ){
        InsertChar(c);
      }
      break;
  }
}

/* Ingore one character
*/
static void IgnoreOne(char c){
  xProcess = ProcessNormal;
}

/* Wait until an ESC '\\' is seen
*/
static void WaitEscSlash(char c){
  static int escSeen = 0;
  if( c==033 ){
    escSeen = 1;
  }else if( c=='\\' && escSeen ){
    escSeen = 0;
    xProcess = ProcessNormal;
  }else{
    escSeen = 0;
  }
}

/* Wait until the BEL character is seen 
*/
static void WaitBel(char c){
  if( c==007 ) xProcess = ProcessNormal;
}

/* The previous character was an ESC (escape).  Process the next
** character.
**
** When this function is called, we know that the buffer has been
** flushed.
*/
static void ProcessEsc(char c){
  int i;
  static void ProcessEscBrace(char c);

  ASSERT( iBufWidth==0 );
  xProcess = ProcessNormal;
  switch( c ){
    case '[':
      xProcess = ProcessEscBrace;
      nParam = 0;
      for(i=0; i<sizeof(aiParam)/sizeof(aiParam[0]); i++) aiParam[i] = -1;
      break;
    case '#':    /* DEC test sequence */
    case '(':    /* Designate G0 Character Set */
    case ')':    /* Designate G1 Character Set */
    case '*':    /* Designate G2 Character Set */
    case '+':    /* Designate G3 Character Set */
      xProcess = IgnoreOne;
      break;
    case '7':    /* Save Cursor */
      iCurSavedX = iCurX;
      iCurSavedY = iCurY;
      break;
    case '8':    /* Restore Cursor */
      iCurX = iCurSavedX;
      iCurY = iCurSavedY;
      break;
    case '=':    /* Application Keypad */
    case '>':    /* Normal Keypad */
      break;
    case 'D':    /* Index */
    case 'E':    /* NextLine */
      IndexForward();
      break;
    case 'F':    /* An HP bug */
      break;
    case 'H':    /* Tab set */
      break;
    case 'M':    /* Reverse index */
      IndexBackward();
      break;
    case 'N':    /* Use G2 for next character only */
      break;
    case 'O':    /* Use G3 for next character only */
      break;
    case 'P':    /* "ESC P text ESC \" implements Device Control String */
      xProcess = WaitEscSlash;
      break;
    case 'Z':    /* Return the Terminal ID */
      ET( SendToTTY \033\[?1\;2c );
      break;
    case ']':
      xProcess = WaitBel;
      break;
    case '^':
    case '_':
      xProcess = WaitEscSlash;
      break;
    default:
      break;
  }
}

/* We have seen an ESC followed by a '['.  Process subsequent characters.
**
** When this function is called, we know that the buffer has been
** flushed.
*/
static void ProcessEscBrace(char c){
  int n, x, y;
  int i;
  int end;
  int iTop;
  int bOldMode;
  char zBuf[1000];
  static void ProcessEscBraceQuestion(char);

  ASSERT( iBufWidth==0 );
  xProcess = ProcessNormal;
  switch( c ){
    case '?':
      xProcess = ProcessEscBraceQuestion;
      break;
    case '@':
      n = aiParam[0];
      if( n<0 ) n = 1;
      x = iCurX;
      y = iCurY;
      bOldMode = bReplaceMode;
      bReplaceMode = 0;
      for(i=0; i<n; i++){
        InsertChar(' ');
      }
      FlushBuffer();
      bReplaceMode = bOldMode;
      iCurX = x;
      iCurY = y;
      ET(.t mark set insert %d(y).%d(x));
      break;
    case 'A':
      n = aiParam[0];
      if( n<0 ) n = 1;
      iCurY -= n;
      if( iCurY < 1 ) iCurY = 1;
      break;
    case 'B':
      n = aiParam[0];
      if( n<0 ) n = 1;
      iCurY += n;
      if( iCurY > iBtm && iCurY > iHeight ) iCurY = iBtm;
      break;
    case 'C':
      n = aiParam[0];
      if( n<0 ) n = 1;
      iCurX += n;
      if( iCurX > iWidth ) iCurX = iWidth;
      break;
    case 'D':
      n = aiParam[0];
      if( n<0 ) n = 1;
      iCurX -= n;
      if( iCurX<0 ) iCurX = 0;
      break;
    case 'H':
      y = aiParam[0];
      if( y<1 ) y = 1;
      x = aiParam[1];
      if( x<1 ) x = 1;
      iCurX = x-1;
      iTop = iBtm - iHeight;
      if( iTop<0 ) iTop = 0;
      iCurY = iTop + y;
      break;
    case 'J':
      iTop = iBtm - iHeight;
      if( iTop<0 ) iTop = 0;
      switch( aiParam[0] ){
        case 1:     /* Clear above the cursor */
          if( iCurY > iTop + 1 ){
            for(i=0; i<iCurY - iTop - 1; i++) zBuf[i] = '\n';
            zBuf[i] = 0;
            ET( .t delete %d(iTop+1).0 %d(iCurY).0 );
            if( zBuf[0] ) ET( .t insert %d(iTop+1).0 {%s(zBuf)} );
          }
          ET(.t delete %d(iCurY).0 %d(iCurY).%d(iCurX));
          break;
        case 2:     /* Clear everything */
          for(i=0; i<iBtm - iTop - 1; i++) zBuf[i] = '\n';
          zBuf[i] = 0;
          ET(.t delete %d(iTop+1).0 {%d(iBtm).0 lineend});
          if( zBuf[0] ) ET(.t insert %d(iTop+1).0 {%s(zBuf)});
          break;
        default:    /* Clear below the cursor */
          for(i=0; i<iBtm - iCurY; i++) zBuf[i] = '\n';
          zBuf[i] = 0;
          ET( .t delete %d(iCurY).%d(iCurX) {%d(iBtm).0 lineend} );
          if( zBuf[0] ) ET( .t insert end {%s(zBuf)} );
          break;
      }
      break;
    case 'K':
      switch( aiParam[0] ){
        case 1:     /* Clear to left of cursor */
          ET(.t delete %d(iCurY).0 %d(iCurY).%d(iCurX));
          break;
        case 2:     /* Clear the whole line */
          ET(.t delete %d(iCurY).0 {%d(iCurY).0 lineend});
          break;
        default:    /* Clear to the right of the cursor */
          ET(.t delete %d(iCurY).%d(iCurX) {%d(iCurY).0 lineend});
          break;
      }
      break;
    case 'L':       /* Insert lines */
      n = aiParam[0];
      if( n<1 ) n = 1;
      for(i=0; i<n; i++) zBuf[i] = '\n';
      zBuf[i] = 0;
      ET(.t insert %d(iCurY).0 {%s(zBuf)});
      iTop = iBtm - iHeight;
      if( iTop<0 ) iTop = 0;
      y = iScrollBtm + iTop;
      ET(.t delete {%d(y).0 lineend} {%d(y+n).0 lineend});
      iBtm = ET_INT(.t index end) - 1;
      break;
    case 'M':       /* Delete lines */
      n = aiParam[0];
      if( n<1 ) n = 1;
      for(i=0; i<n; i++) zBuf[i] = '\n';
      zBuf[i] = 0;
      ET(.t delete %d(iCurY).0 %d(iCurY+n).0);
      iTop = iBtm - iHeight;
      if( iTop<0 ) iTop = 0;
      y = iScrollBtm + iTop;
      if( iCurY < y ){
        ET(.t insert {%d(y-n).0 lineend} {%s(zBuf)});
      }else{
        ET(.t insert end {%s(zBuf)});
      }
      break;
    case 'P':       /* Delete characters */
      n = aiParam[0];
      if( n<0 ) n = 1;
      sscanf( ET_STR(.t index {%d(iCurY).0 lineend}), "%*d.%d", &end);
      if( iCurX + n > end ) n = end - iCurX;
      if( n>0 ){
        ET(.t delete %d(iCurY).%d(iCurX) %d(iCurY).%d(iCurX+n));
      }
      break;
    case 'T':       /* Initiate hilite mouse tracking */
      break;
    case 'c':       /* Send device attributes */
      break;
    case 'f':       /* horizontal and vertical position */
      break;
    case 'g':       /* Tab clear */
      break;
    case 'h':       /* Set mode */
      switch( aiParam[0] ){
        case 4:
          bReplaceMode = 0;
        default:
          break;
      }
      break;
    case 'l':       /* reset mode */
      switch( aiParam[0] ){
        case 4:
          bReplaceMode = 1;
        default:
          break;
      }
      break;
    case 'm':       /* character attributes */
      switch( aiParam[0] ){
        case 1:    /* Bold */
        case 5:    /* Blink (appears as bold) */
          eBufTag = BT_Bold;
          break;
        case 4:    /* Underscore */
          eBufTag = BT_Underline;
          break;
        case 7:    /* Inverse */
          eBufTag = BT_Inverse;
          break;
        default:   /* Normal */
          eBufTag = BT_Normal;
          break; 
      }
      break;
    case 'n':       /* Device status report */
      break;
    case 'r':       /* Set scrolling region */
      iScrollTop = aiParam[0];
      if( iScrollTop<1 ) iScrollTop = 1;
      if( iScrollTop>iHeight ) iScrollTop = iHeight;
      iScrollBtm = aiParam[1];
      if( iScrollBtm<iScrollTop ) iScrollBtm = iScrollTop;
      if( iScrollBtm>iHeight ) iScrollBtm = iHeight;
      iTop = iBtm - iHeight;
      if( iTop<0 ) iTop = 0;
      iCurY = iTop + 1;
      iCurX = 0;
      break;
    case 'x':       /* Request terminal parameters */
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      if( aiParam[nParam]<0 ){
        aiParam[nParam] = c - '0';
      }else{
        aiParam[nParam] = aiParam[nParam]*10 + c - '0';
      }
      xProcess = ProcessEscBrace;
      break;
    case ';':
      if( nParam < sizeof(aiParam)/sizeof(aiParam[0]) - 1 ){
        nParam++;
      }
      xProcess = ProcessEscBrace;
      break;
    default:
      break;
  }
}

/* We have seen an ESC followed by a '['.  Process subsequent characters.
**
** When this function is called, we know that the buffer has been
** flushed and the text widget has scrolled all the way to the
** bottom.
*/
static void ProcessEscBraceQuestion(char c){
  switch( c ){
    case 'h':       /* DEC private mode set */
      xProcess = ProcessNormal;
      break;
    case 'l':       /* DEC private mode reset */
      xProcess = ProcessNormal;
      break;
    case 'r':       /* Restore DEC private mode values */
      xProcess = ProcessNormal;
      break;
    case 's':       /* Save DEC private mode values */
      xProcess = ProcessNormal;
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case ' ':
    case ';':
      break;
    default:
      xProcess = ProcessNormal;
      break;
  }
}

/* This routine is called to deal with data written by the child process
** on the other side of the pty.  Its job is to interpret characters as
** if it where a VT100 terminal, and make appropriate changes to the
** text widget.
*/
static void HandleInput(void *clientData, int mask){
  int nByte;
  char zBuf[2000];

  nByte = read(iChildFd,zBuf,sizeof(zBuf)-1);
  if( nByte>0 ){
    int i;

    /* Process all characters received */
    for(i=0; i<nByte; i++){
#ifdef DEBUG
      if( bDebug ){
        printf("Recv: %03o %c\n",(int)zBuf[i],
          (zBuf[i]>=' ' && zBuf[i]<='~')? zBuf[i] : ' ');
      }
#endif
      (*xProcess)(zBuf[i]);
    }

    /* It might be a while before we process additional input, so 
    ** flush the buffer to synchronize the screen display. */
    FlushBuffer();
    ClipScrollback();
    DisplayCursor();
    JumpToBottom();

    /* Tk4.0 requires two "update idletasks" in order to redisplay
    ** the text widget.  This line is not strictly necessary.  If you
    ** remove it, the terminal will scroll faster, but you won't get to
    ** watch is scroll by.  With this line in place, the screen will
    ** update at least every 2000 characters.
    */
    ET(update idletasks; update idletasks);
  }else if( nByte<0 ){
    /* Perhaps the child process has died */
    extern int IsChildAlive();
    if( !IsChildAlive() ){
      CleanShutdown();
    }
  }

}

/* Process the text of the first argument as if it had been received
** through the pseudo-TTY.
*/
ET_PROC( SimulatedInput ){
  int i;
  if( argc!=2 ){
    interp->result = "wrong # args";
    return ET_ERROR;
  }
  for(i=0; argv[1][i]; i++){
#ifdef DEBUG
    if( bDebug ){
      printf("Sim: %03o %c\n",(int)argv[1][i],
        (argv[1][i]>=' ' && argv[1][i]<='~')? argv[1][i] : ' ');
    }
#endif
    (*xProcess)(argv[1][i]);
  }
  FlushBuffer();
  ClipScrollback();
  DisplayCursor();
  JumpToBottom();
  return ET_OK;
}

/* This procedure is used to send characters to the process running
** on the other size of the pseudo-TTY.
*/
ET_PROC( SendToTTY ){
  int c;
  char z[1];
  if( argc!=2 ){
    interp->result = "wrong # args";
    return ET_ERROR;
  }
  Tcl_GetInt(interp,argv[1],&c);
  *z = c;
  if( c>0 && c<=0x7f ){
    while( write(iChildFd,z,1)<1 ){}
  }
  return ET_OK;
}

/* A special procedure is needed to send zeros.
*/
ET_PROC( SendZeroToTTY ){
  char z[1];
  *z = 0;
  while( write(iChildFd,z,1)<1 ){}
  return ET_OK;
}

/* Do a paste operation
*/
ET_PROC( Paste ){
  int i;
  if( argc!=2 ){
    interp->result = "wrong # args";
    return ET_ERROR;
  }
  for(i=0; argv[1][i]; i++){
    char c = argv[1][i];
    while( write(iChildFd,&c,1)<1 ){}

    /* Some Pseudo-TTYs are broken (ex: Linux) and can't receive too
    ** many characters too quickly.  Hence we need to pause every so
    ** often on a big paste to let the Pseudo-TTY catch up. */
    if( i%100 == 99 ) ET( after 10; update);
  }
  return ET_OK;
}

/* Ring the bell.  Needed in Tk4.0 because the built-in "bell" causes
** the screen-saver to be reset, which in turn causes an unsightly flicker
** on the screen on some systems. */
ET_PROC( bell ){
  XBell(Et_Display,0);
  return ET_OK;
}

/* This procedure is called after the window changes size.  Its
** job is to tell the Pseudo-TTY about the new window size.
*/
ET_PROC( WindowSizeChangeNotify ){
  static int previousHeight = 24;
  extern void SetSizeOfTTY(int,int,int);

  JumpToBottom();
  SetSizeOfTTY(iChildFd,iWidth,iHeight);
  if( iScrollBtm == previousHeight ){
    iScrollBtm = iHeight;
  }
  previousHeight = iHeight;
  return ET_OK;
}
