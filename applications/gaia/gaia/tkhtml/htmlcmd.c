static char const rcsid[] = "@(#) $Id$";
/*
** Routines to implement the HTML widget commands
**
** Copyright (C) 1997-2000 D. Richard Hipp
**
** This library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public
** License as published by the Free Software Foundation; either
** version 2 of the License, or (at your option) any later version.
**
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
**
** You should have received a copy of the GNU Library General Public
** License along with this library; if not, write to the
** Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
** Boston, MA  02110-1301, USA.
**
** Author contact information:
**   drh@acm.org
**   http://www.hwaci.com/drh/
*/
#include <tk.h>
#include <stdlib.h>
#include <string.h>
#include "htmlcmd.h"

/*
** WIDGET resolve ?URI ...?
**
** Call the TCL command specified by the -resolvercommand option
** to resolve the URL.
*/
int HtmlResolveCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  return HtmlCallResolver(htmlPtr, argv+2);
}

/*
** WIDGET cget CONFIG-OPTION
**
** Retrieve the value of a configuration option
*/
int HtmlCgetCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  TestPoint(0);
  return Tk_ConfigureValue(interp, htmlPtr->tkwin, HtmlConfigSpec(),
		(char *) htmlPtr, argv[2], 0);
}

/*
** WIDGET clear
**
** Erase all HTML from this widget and clear the screen.  This is
** typically done before loading a new document.
*/
int HtmlClearCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  HtmlClear(htmlPtr);
  htmlPtr->flags |= REDRAW_TEXT | VSCROLL | HSCROLL;
  HtmlScheduleRedraw(htmlPtr);
  TestPoint(0);
  return TCL_OK;
}

/*
** WIDGET configure ?OPTIONS?
**
** The standard Tk configure command.
*/
int HtmlConfigCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  if (argc == 2) {
     TestPoint(0);
     return Tk_ConfigureInfo(interp, htmlPtr->tkwin, HtmlConfigSpec(),
        (char *) htmlPtr, (char *) NULL, 0);
  } else if (argc == 3) {
     TestPoint(0);
     return Tk_ConfigureInfo(interp, htmlPtr->tkwin, HtmlConfigSpec(),
        (char *) htmlPtr, argv[2], 0);
  } else {
     TestPoint(0);
     return ConfigureHtmlWidget(interp, htmlPtr, argc-2, argv+2,
                                TK_CONFIG_ARGV_ONLY, 0);
  }
}

/*
** WIDGET href X Y
**
** Returns the URL on the hyperlink that is beneath the position X,Y.
** Returns {} if there is no hyperlink beneath X,Y.
*/
int HtmlHrefCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  int x, y;
  char *z;

  if( Tcl_GetInt(interp, argv[2], &x) != TCL_OK
   || Tcl_GetInt(interp, argv[3], &y) != TCL_OK
  ){
    TestPoint(0);
    return TCL_ERROR;
  }
  z = HtmlGetHref(htmlPtr, x + htmlPtr->xOffset, y + htmlPtr->yOffset);
  if( z ){
    HtmlLock(htmlPtr);
    z = HtmlResolveUri(htmlPtr, z);
    if( !HtmlUnlock(htmlPtr) ){
      Tcl_SetResult(interp, z, TCL_DYNAMIC);
    }
  }
  return TCL_OK;
}

/*
** WIDGET names
**
** Returns a list of names associated with <a name=...> tags.
*/
int HtmlNamesCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  HtmlElement *p;
  char *z;
  TestPoint(0);
  for(p=htmlPtr->pFirst; p; p=p->pNext){
    if( p->base.type!=Html_A ) continue;
    z = HtmlMarkupArg(p,"name",0);
    if( z ){
      Tcl_AppendElement(interp,z);
    }else{
      z = HtmlMarkupArg(p,"id",0);
      if( z ){
        Tcl_AppendElement(interp,z);
      }
    }
  }
  return TCL_OK;
}

/*
** WIDGET parse HTML
**
** Appends the given HTML text to the end of any HTML text that may have
** been inserted by prior calls to this command.  Then it runs the
** tokenizer, parser and layout engine as far as possible with the
** text that is available.  The display is updated appropriately.
*/
int HtmlParseCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  HtmlElement *endPtr;
  endPtr = htmlPtr->pLast;
  HtmlLock(htmlPtr);
  HtmlTokenizerAppend(htmlPtr, argv[2]);
  if( HtmlIsDead(htmlPtr) ){
    return TCL_OK;
  }
  if( endPtr ){
    if( endPtr->pNext ){
      HtmlAddStyle(htmlPtr, endPtr->pNext);
    }
  }else if( htmlPtr->pFirst ){
    htmlPtr->paraAlignment = ALIGN_None;
    htmlPtr->rowAlignment = ALIGN_None;
    htmlPtr->anchorFlags = 0;
    htmlPtr->inDt = 0;
    htmlPtr->anchorStart = 0;
    htmlPtr->formStart = 0;
    htmlPtr->innerList = 0;
    HtmlAddStyle(htmlPtr, htmlPtr->pFirst);
    TestPoint(0);
  }
  if( !HtmlUnlock(htmlPtr) ){
    htmlPtr->flags |= EXTEND_LAYOUT;
    HtmlScheduleRedraw(htmlPtr);
    TestPoint(0);
  }
  return TCL_OK;
}

/*
** WIDGET xview ?SCROLL-OPTIONS...?
**
** Implements horizontal scrolling in the usual Tk way.
*/
int HtmlXviewCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  if( argc==2 ){
      HtmlComputeHorizontalPosition(htmlPtr,Tcl_GetStringResult(interp));
    TestPoint(0);
  }else{
    int count;
    double fraction;
    int maxX = htmlPtr->maxX;
    int w = HtmlUsableWidth(htmlPtr);
    int offset = htmlPtr->xOffset;
    int type = Tk_GetScrollInfo(interp,argc,argv,&fraction,&count);
    switch( type ){
      case TK_SCROLL_ERROR:
        TestPoint(0);
        return TCL_ERROR;
      case TK_SCROLL_MOVETO:
        offset = fraction * maxX;
        TestPoint(0);
        break;
      case TK_SCROLL_PAGES:
        offset += (count * w * 9)/10;
        TestPoint(0);
        break;
      case TK_SCROLL_UNITS:
        offset += (count * w)/10;
        TestPoint(0);
        break;
    }
    if( offset + w > maxX ){
      offset = maxX - w;
      TestPoint(0);
    }else{
      TestPoint(0);
    }
    if( offset < 0 ){
      offset = 0;
      TestPoint(0);
    }else{
      TestPoint(0);
    }
    HtmlHorizontalScroll(htmlPtr, offset);
  }
  return TCL_OK;
}

/*
** WIDGET yview ?SCROLL-OPTIONS...?
**
** Implements vertical scrolling in the usual Tk way, but with one
** enhancement.  If the argument is a single word, the widget looks
** for a <a name=...> tag with that word as the "name" and scrolls
** to the position of that tag.
*/
int HtmlYviewCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  if( argc==2 ){
    HtmlComputeVerticalPosition(htmlPtr,Tcl_GetStringResult(interp));
    TestPoint(0);
  }else if( argc==3 ){
    char *z;
    HtmlElement *p;
    for(p=htmlPtr->pFirst; p; p=p->pNext){
      if( p->base.type!=Html_A ) continue;
      z = HtmlMarkupArg(p,"name",0);
      if( z==0 ){
        TestPoint(0);
        continue;
      }
      if( strcmp(z,argv[2])!=0 ){
        TestPoint(0);
        continue;
      }
      HtmlVerticalScroll(htmlPtr, p->anchor.y);
      TestPoint(0);
      break;
    }
  }else{
    int count;
    double fraction;
    int maxY = htmlPtr->maxY;
    int h = HtmlUsableHeight(htmlPtr);
    int offset = htmlPtr->yOffset;
    int type = Tk_GetScrollInfo(interp,argc,argv,&fraction,&count);
    switch( type ){
      case TK_SCROLL_ERROR:
        TestPoint(0);
        return TCL_ERROR;
      case TK_SCROLL_MOVETO:
        offset = fraction * maxY;
        TestPoint(0);
        break;
      case TK_SCROLL_PAGES:
        offset += (count * h * 9)/10;
        TestPoint(0);
        break;
      case TK_SCROLL_UNITS:
        offset += (count * h)/10;
        TestPoint(0);
        break;
    }
    if( offset + h > maxY ){
      offset = maxY - h;
      TestPoint(0);
    }else{
      TestPoint(0);
    }
    if( offset < 0 ){
      offset = 0;
      TestPoint(0);
    }else{
      TestPoint(0);
    }
    HtmlVerticalScroll(htmlPtr, offset);
  }
  return TCL_OK;
}

/*
** WIDGET token handler TAG ?SCRIPT?
*/
int HtmlTokenHandlerCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  int type = HtmlNameToType(argv[3]);
  if( type==Html_Unknown ){
    Tcl_AppendResult(interp,"unknown tag: \"", argv[3], "\"", 0);
    return TCL_ERROR;
  }
  if( argc==4 ){
    if( htmlPtr->zHandler[type]!=0 ){
      Tcl_SetResult(interp, htmlPtr->zHandler[type], TCL_VOLATILE);
    }
  }else{
    if( htmlPtr->zHandler[type]!=0 ){
      HtmlFree(htmlPtr->zHandler[type]);
    }
    htmlPtr->zHandler[type] = HtmlAlloc( strlen(argv[4]) + 1 );
    if( htmlPtr->zHandler[type] ){
      strcpy(htmlPtr->zHandler[type],argv[4]);
    }
  }
  return TCL_OK;
}

/*
** WIDGET index INDEX
*/
int HtmlIndexCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  HtmlElement *p;
  int i;

  HtmlLock(htmlPtr);
  if( HtmlGetIndex(htmlPtr, argv[2], &p, &i)!=0 ){
    if( !HtmlUnlock(htmlPtr) ){
      Tcl_AppendResult(interp,"malformed index: \"", argv[2], "\"", 0);
    }
    TestPoint(0);
    return TCL_ERROR;
  }
  if( !HtmlUnlock(htmlPtr) && p ){
    char buffer[80];
    sprintf(buffer, "%d.%d", HtmlTokenNumber(p), i);
    Tcl_SetResult(interp, buffer, TCL_VOLATILE);
    TestPoint(0);
  }else{
    TestPoint(0);
  }
  return TCL_OK;
}

/* The pSelStartBlock and pSelEndBlock values have been changed.
** This routine's job is to loop over all HtmlBlocks and either
** set or clear the HTML_Selected bits in the .base.flags field
** as appropriate.  For every HtmlBlock where the bit changes,
** mark that block for redrawing.
*/
static void UpdateSelection(HtmlWidget *htmlPtr){
  int selected = 0;
  HtmlIndex tempIndex;
  HtmlBlock *pTempBlock;
  int temp;
  HtmlBlock *p;

  for(p=htmlPtr->firstBlock; p; p=p->pNext){
    if( p==htmlPtr->pSelStartBlock ){
      selected = 1;
      HtmlRedrawBlock(htmlPtr, p);
      TestPoint(0);
    }else if( !selected && p==htmlPtr->pSelEndBlock ){
      selected = 1;
      tempIndex = htmlPtr->selBegin;
      htmlPtr->selBegin = htmlPtr->selEnd;
      htmlPtr->selEnd = tempIndex;
      pTempBlock = htmlPtr->pSelStartBlock;
      htmlPtr->pSelStartBlock = htmlPtr->pSelEndBlock;
      htmlPtr->pSelEndBlock = pTempBlock;
      temp = htmlPtr->selStartIndex;
      htmlPtr->selStartIndex = htmlPtr->selEndIndex;
      htmlPtr->selEndIndex = temp;
      HtmlRedrawBlock(htmlPtr, p);
      TestPoint(0);
    }else{
      TestPoint(0);
    }
    if( p->base.flags & HTML_Selected ){
      if( !selected ){
        p->base.flags &= ~HTML_Selected;
        HtmlRedrawBlock(htmlPtr,p);
        TestPoint(0);
      }else{
        TestPoint(0);
      }
    }else{
      if( selected ){
        p->base.flags |= HTML_Selected;
        HtmlRedrawBlock(htmlPtr,p);
        TestPoint(0);
      }else{
        TestPoint(0);
      }
    }
    if( p==htmlPtr->pSelEndBlock ){
      selected = 0;
      HtmlRedrawBlock(htmlPtr, p);
      TestPoint(0);
    }else{
      TestPoint(0);
    }
  }
}

/* Given the selection end-points in htmlPtr->selBegin
** and htmlPtr->selEnd, recompute pSelBeginBlock and
** pSelEndBlock, then call UpdateSelection to update the
** display.
**
** This routine should be called whenever the selection
** changes or whenever the set of HtmlBlock structures
** change.
*/
void HtmlUpdateSelection(HtmlWidget *htmlPtr, int forceUpdate){
  HtmlBlock *pBlock;
  int index;
  int needUpdate = forceUpdate;
  int temp;

  if( htmlPtr->selEnd.p==0 ){
    htmlPtr->selBegin.p = 0;
    TestPoint(0);
  }else{
    TestPoint(0);
  }
  HtmlIndexToBlockIndex(htmlPtr, htmlPtr->selBegin, &pBlock, &index);
  if( needUpdate || pBlock != htmlPtr->pSelStartBlock ){
    needUpdate = 1;
    HtmlRedrawBlock(htmlPtr, htmlPtr->pSelStartBlock);
    htmlPtr->pSelStartBlock = pBlock;
    htmlPtr->selStartIndex = index;
    TestPoint(0);
  }else if( index != htmlPtr->selStartIndex ){
    HtmlRedrawBlock(htmlPtr, pBlock);
    htmlPtr->selStartIndex = index;
    TestPoint(0);
  }else{
    TestPoint(0);
  }
  if( htmlPtr->selBegin.p==0 ){
    htmlPtr->selEnd.p = 0;
    TestPoint(0);
  }else{
    TestPoint(0);
  }
  HtmlIndexToBlockIndex(htmlPtr, htmlPtr->selEnd, &pBlock, &index);
  if( needUpdate || pBlock != htmlPtr->pSelEndBlock ){
    needUpdate = 1;
    HtmlRedrawBlock(htmlPtr, htmlPtr->pSelEndBlock);
    htmlPtr->pSelEndBlock = pBlock;
    htmlPtr->selEndIndex = index;
    TestPoint(0);
  }else if( index != htmlPtr->selEndIndex ){
    HtmlRedrawBlock(htmlPtr, pBlock);
    htmlPtr->selEndIndex = index;
    TestPoint(0);
  }else{
    TestPoint(0);
  }
  if( htmlPtr->pSelStartBlock
  && htmlPtr->pSelStartBlock==htmlPtr->pSelEndBlock
  && htmlPtr->selStartIndex > htmlPtr->selEndIndex
  ){
    temp = htmlPtr->selStartIndex;
    htmlPtr->selStartIndex = htmlPtr->selEndIndex;
    htmlPtr->selEndIndex = temp;
    TestPoint(0);
  }else{
    TestPoint(0);
  }
  if( needUpdate ){
    UpdateSelection(htmlPtr);
    TestPoint(0);
  }else{
    TestPoint(0);
  }
}

/*
** WIDGET selection set INDEX INDEX
*/
int HtmlSelectionSetCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv           /* List of all arguments */
){
  HtmlIndex selBegin, selEnd;

  HtmlLock(htmlPtr);
  if( HtmlGetIndex(htmlPtr, argv[3], &selBegin.p, &selBegin.i) ){
    if( !HtmlUnlock(htmlPtr) ){
      Tcl_AppendResult(interp,"malformed index: \"", argv[3], "\"", 0);
    }
    TestPoint(0);
    return TCL_ERROR;
  }
  if( HtmlIsDead(htmlPtr) ) return TCL_OK;
  if( HtmlGetIndex(htmlPtr, argv[4], &selEnd.p, &selEnd.i) ){
    if( !HtmlUnlock(htmlPtr) ){
      Tcl_AppendResult(interp,"malformed index: \"", argv[4], "\"", 0);
    }
    TestPoint(0);
    return TCL_ERROR;
  }
  if( HtmlUnlock(htmlPtr) ) return TCL_OK;
  htmlPtr->selBegin = selBegin;
  htmlPtr->selEnd = selEnd;
  HtmlUpdateSelection(htmlPtr,0);
  TestPoint(0);
  return TCL_OK;
}

/*
** WIDGET selection clear
*/
int HtmlSelectionClearCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv           /* List of all arguments */
){
  htmlPtr->pSelStartBlock = 0;
  htmlPtr->pSelEndBlock = 0;
  htmlPtr->selBegin.p = 0;
  htmlPtr->selEnd.p = 0;
  UpdateSelection(htmlPtr);
  TestPoint(0);
  return TCL_OK;
}

/*
** Recompute the position of the insertion cursor based on the
** position in htmlPtr->ins.
*/
void HtmlUpdateInsert(HtmlWidget *htmlPtr){
  HtmlIndexToBlockIndex(htmlPtr, htmlPtr->ins,
                        &htmlPtr->pInsBlock, &htmlPtr->insIndex);
  HtmlRedrawBlock(htmlPtr, htmlPtr->pInsBlock);
  if( htmlPtr->insTimer==0 ){
    htmlPtr->insStatus = 0;
    HtmlFlashCursor(htmlPtr);
    TestPoint(0);
  }else{
    TestPoint(0);
  }
}

/*
** WIDGET insert INDEX
*/
int HtmlInsertCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv           /* List of all arguments */
){
  HtmlIndex ins;
  if( argv[2][0]==0 ){
    HtmlRedrawBlock(htmlPtr, htmlPtr->pInsBlock);
    htmlPtr->insStatus = 0;
    htmlPtr->pInsBlock = 0;
    htmlPtr->ins.p = 0;
    TestPoint(0);
  }else{
    HtmlLock(htmlPtr);
    if( HtmlGetIndex(htmlPtr, argv[2], &ins.p, &ins.i) ){
      if( !HtmlUnlock(htmlPtr) ){
        Tcl_AppendResult(interp,"malformed index: \"", argv[2], "\"", 0);
      }
      TestPoint(0);
      return TCL_ERROR;
    }
    if( HtmlUnlock(htmlPtr) ) return TCL_OK;
    HtmlRedrawBlock(htmlPtr, htmlPtr->pInsBlock);
    htmlPtr->ins = ins;
    HtmlUpdateInsert(htmlPtr);
    TestPoint(0);
  }
  return TCL_OK;
}

/*
** WIDGET token list START END
*/
int HtmlTokenListCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  HtmlElement *pStart, *pEnd;
  int i;

  if( HtmlGetIndex(htmlPtr, argv[3], &pStart, &i)!=0 ){
    Tcl_AppendResult(interp,"malformed index: \"", argv[3], "\"", 0);
    return TCL_ERROR;
  }
  if( HtmlGetIndex(htmlPtr, argv[4], &pEnd, &i)!=0 ){
    Tcl_AppendResult(interp,"malformed index: \"", argv[4], "\"", 0);
    return TCL_ERROR;
  }
  if( pStart ){
    HtmlTclizeList(interp,pStart,pEnd ? pEnd->base.pNext : 0);
  }
  return TCL_OK;
}

#ifdef DEBUG
/*
** WIDGET debug dump START END
*/
int HtmlDebugDumpCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  HtmlElement *pStart, *pEnd;
  int i;

  if( HtmlGetIndex(htmlPtr, argv[3], &pStart, &i)!=0 ){
    Tcl_AppendResult(interp,"malformed index: \"", argv[3], "\"", 0);
    return TCL_ERROR;
  }
  if( HtmlGetIndex(htmlPtr, argv[4], &pEnd, &i)!=0 ){
    Tcl_AppendResult(interp,"malformed index: \"", argv[4], "\"", 0);
    return TCL_ERROR;
  }
  if( pStart ){
    HtmlPrintList(pStart,pEnd ? pEnd->base.pNext : 0);
  }
  return TCL_OK;
}

/*
** WIDGET debug testpt FILENAME
*/
int HtmlDebugTestPtCmd(
  HtmlWidget *htmlPtr,   /* The HTML widget */
  Tcl_Interp *interp,    /* The interpreter */
  int argc,              /* Number of arguments */
  char **argv            /* List of all arguments */
){
  HtmlTestPointDump(argv[3]);
  return TCL_OK;
}
#endif
