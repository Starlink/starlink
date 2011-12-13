static char const rcsid[] = "@(#) $Id$";
/*
** Routines used to compute the style and size of individual elements.
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
#include <string.h>
#include <stdlib.h>
#include "htmlsizer.h"

/*
** Get the current rendering style.  In other words, get the style
** that is currently on the top of the style stack.
*/
static HtmlStyle GetCurrentStyle(HtmlWidget *htmlPtr){
  HtmlStyle style;
  if( htmlPtr->styleStack ){
    style = htmlPtr->styleStack->style;
  }else{
    style.font = NormalFont(2);
    style.color = COLOR_Normal;
    style.bgcolor = COLOR_Background;
    style.subscript = 0;
    style.align = ALIGN_Left;
    style.flags = 0;
  }
  return style;
}

/*
** Push a new rendering style onto the stack.
*/
static void PushStyleStack(
  HtmlWidget *htmlPtr,   /* Widget on which to push the style */
  int tag,               /* Tag for this style.  Normally the end-tag such
                         ** as </h3> or </em>. */
  HtmlStyle style        /* The style to push */
){
  HtmlStyleStack *p;

  p = HtmlAlloc(sizeof(*p));
  p->pNext = htmlPtr->styleStack;
  p->type = tag;
  p->style = style;
  htmlPtr->styleStack = p;
}

/*
** Pop a rendering style off of the stack.
**
** The top-most style on the stack should have a tag equal to "tag".
** If not, then we have an HTML coding error.  Perhaps something
** like this:  "Some text <em>Enphasized</i> more text".  It is an
** interesting problem to figure out how to respond sanely to this
** kind of error.  Our solution it to keep popping the stack until
** we find the correct tag, or until the stack is empty.
*/
HtmlStyle HtmlPopStyleStack(HtmlWidget *htmlPtr, int tag){
  int type;
  HtmlStyleStack *p;
  static Html_u8 priority[Html_TypeCount+1];

  if( priority[Html_TABLE]==0 ){
    int i;
    for(i=0; i<=Html_TypeCount; i++) priority[i] = 1;
    priority[Html_TD] = 2;
    priority[Html_EndTD] = 2;
    priority[Html_TH] = 2;
    priority[Html_EndTH] = 2;
    priority[Html_TR] = 3;
    priority[Html_EndTR] = 3;
    priority[Html_TABLE] = 4;
    priority[Html_EndTABLE] = 4;
  }
  if( tag<=0 || tag>Html_TypeCount ){
    CANT_HAPPEN;
    return GetCurrentStyle(htmlPtr);
  }
  while( (p=htmlPtr->styleStack)!=0 ){
    type = p->type;
    if( type<=0 || type>Html_TypeCount ){
      CANT_HAPPEN;
      return GetCurrentStyle(htmlPtr);
    }
    if( type!=tag && priority[type]>priority[tag] ){
      return GetCurrentStyle(htmlPtr);
    }
    htmlPtr->styleStack = p->pNext;
    HtmlFree(p);
    if( type==tag ){ break; }
  }
  return GetCurrentStyle(htmlPtr);
}

/*
** Change the font size on the given style by the delta-amount given
*/
static void ScaleFont(HtmlStyle *pStyle, int delta){
  int size = FontSize(pStyle->font) + delta;
  if( size<0 ){
    delta -= size;
  }else if( size>6 ){
    delta -= size-6;
  }
  pStyle->font += delta;
}

/*
** Lookup an argument in the given markup with the name given.
** Return a pointer to its value, or the given default
** value if it doesn't appear.
*/
char *HtmlMarkupArg(HtmlElement *p, const char *tag, char *zDefault){
  int i;
  if( !HtmlIsMarkup(p) ){ TestPoint(0); return 0; }
  for(i=0; i<p->base.count; i+=2){
    if( strcmp(p->markup.argv[i],tag)==0 ){
      return  p->markup.argv[i+1];
    }
  }
  return zDefault;
}

/*
** Return an alignment or justification flag associated with the
** given markup.  The given default value is returned if no alignment is
** specified.
*/
static int GetAlignment(HtmlElement *p, int dflt){
  char *z = HtmlMarkupArg(p,"align",0);
  int rc = dflt;
  if( z ){
    if( stricmp(z,"left")==0 ){
      rc = ALIGN_Left;
    }else if( stricmp(z,"right")==0 ){
      rc = ALIGN_Right;
    }else if( stricmp(z,"center")==0 ){
      rc = ALIGN_Center;
    }
  }
  return rc;
}

/*
** The "type" argument to the given element might describe the type
** for an ordered list.  Return the corresponding LI_TYPE_* entry
** if this is the case, or the default value if it isn't.
*/
static int GetOrderedListType(HtmlElement *p, int dflt){
  char *z;

  z = HtmlMarkupArg(p,"type",0);
  if( z ){
    switch( *z ){
      case 'A':  TestPoint(0); dflt = LI_TYPE_Enum_A; break;
      case 'a':  TestPoint(0); dflt = LI_TYPE_Enum_a; break;
      case '1':  TestPoint(0); dflt = LI_TYPE_Enum_1; break;
      case 'I':  TestPoint(0); dflt = LI_TYPE_Enum_I; break;
      case 'i':  TestPoint(0); dflt = LI_TYPE_Enum_i; break;
      default:   TestPoint(0); break;
    }
  }else{
    TestPoint(0);
  }
  return dflt;
}

/*
** The "type" argument to the given element might describe a type
** for an unordered list.  Return the corresponding LI_TYPE entry
** if this is the case, or the default value if it isn't.
*/
static int GetUnorderedListType(HtmlElement *p, int dflt){
  char *z;

  z = HtmlMarkupArg(p,"type",0);
  if( z ){
    if( stricmp(z,"disc")==0 ){
      dflt = LI_TYPE_Bullet1;
    }else if( stricmp(z,"circle")==0 ){
      dflt = LI_TYPE_Bullet2;
    }else if( stricmp(z,"square")==0 ){
      dflt = LI_TYPE_Bullet3;
    }
  }
  return dflt;
}

/*
** Add the STY_Invisible style to every token between pFirst and pLast.
*/
static void MakeInvisible(HtmlElement *pFirst, HtmlElement *pLast){
  if( pFirst==0 ) return;
  pFirst = pFirst->pNext;
  while( pFirst && pFirst!=pLast ){
    pFirst->base.style.flags |= STY_Invisible;
    pFirst=pFirst->pNext;
  }
}



/*
** For the markup <a href=XXX>, find out if the URL has been visited
** before or not.  Return COLOR_Visited or COLOR_Unvisited, as
** appropriate.
**
** This routine may invoke a callback procedure which could delete
** the HTML widget.  The calling function should call HtmlLock()
** if it needs the widget structure to be preserved.
*/
static int GetLinkColor(HtmlWidget *htmlPtr, char *zURL){
  char *zCmd;
  int result;
  int isVisited;

  if( htmlPtr->tkwin==0 ){
    TestPoint(0);
    return COLOR_Normal;
  }
  if( htmlPtr->zIsVisited==0 || htmlPtr->zIsVisited[0]==0 ){
    TestPoint(0);
    return COLOR_Unvisited;
  }
  zCmd = HtmlAlloc( strlen(htmlPtr->zIsVisited) + strlen(zURL) + 10 );
  if( zCmd==0 ){
    TestPoint(0);
    return COLOR_Unvisited;
  }
  sprintf(zCmd,"%s {%s}",htmlPtr->zIsVisited, zURL);
  HtmlLock(htmlPtr);
  result = Tcl_GlobalEval(htmlPtr->interp,zCmd);
  HtmlFree(zCmd);
  if( HtmlUnlock(htmlPtr) ){
    return COLOR_Unvisited;
  }
  if( result!=TCL_OK ){
    TestPoint(0);
    goto errorOut;
  }
  result = Tcl_GetBoolean(htmlPtr->interp,
                          Tcl_GetStringResult(htmlPtr->interp), &isVisited);
  if( result!=TCL_OK ){
    TestPoint(0);
    goto errorOut;
  }
  TestPoint(0);
  return isVisited ? COLOR_Visited : COLOR_Unvisited;

  errorOut:
  Tcl_AddErrorInfo(htmlPtr->interp,
    "\n    (\"-isvisitedcommand\" command executed by html widget)");
  Tcl_BackgroundError(htmlPtr->interp);
  TestPoint(0);
  return COLOR_Unvisited;
}

/*
** This routine adds information to the input texts that doesn't change
** when the display is resized or when new fonts are selected, etc.
** Mostly this means adding style attributes.  But other constant
** information (such as numbering on <li> and images used for <IMG>)
** is also obtained.  The key is that this routine is only called
** once, where the sizer and layout routines can be called many times.
**
** This routine is called whenever the list of elements grows.  The
** style stack is stored as part of the HTML widget so that we can
** always continue where we left off the last time.
**
** In addition to adding style, this routine will invoke callbacks
** needed to acquire information about a markup.  The htmlPtr->zIsVisitied
** callback is called for each <a> and the htmlPtr->zGetImage is called
** for each <IMG> or for each <LI> that has a SRC= field.
**
** This routine may invoke a callback procedure which could delete
** the HTML widget.
**
** When a markup is inserted or deleted from the token list, the
** style routine must be completely rerun from the beginning.  So
** what we said above, that this routine is only run once, is not
** strictly true.
*/
void HtmlAddStyle(HtmlWidget *htmlPtr, HtmlElement *p){
  HtmlStyle style;          /* Current style */
  int size;                 /* A new font size */
  int i;                    /* Loop counter */
  int paraAlign;            /* Current paragraph alignment */
  int rowAlign;             /* Current table row alignment */
  int anchorFlags;          /* Flags associated with <a> tag */
  int inDt;                 /* True if within <dt>..</dt> */
  HtmlStyle nextStyle;      /* Style for next token if useNextStyle==1 */
  int useNextStyle = 0;     /* True if nextStyle is valid */
  char *z;                  /* A tag parameter's value */

  /* The size of header fonts relative to the current font size */
  static int header_sizes[] = {+2, +1, 1, 1, -1, -1};

  /* Don't allow recursion */
  if( htmlPtr->flags & STYLER_RUNNING ){ TestPoint(0); return; }
  htmlPtr->flags |= STYLER_RUNNING;

  /* Load the style state out of the htmlPtr structure and into local
  ** variables.  This is purely a matter of convenience... */
  style = GetCurrentStyle(htmlPtr);
  paraAlign = htmlPtr->paraAlignment;
  rowAlign = htmlPtr->rowAlignment;
  anchorFlags = htmlPtr->anchorFlags;
  inDt = htmlPtr->inDt;

  /* Loop over tokens */
  while( p ){
    switch( p->base.type ){
      case Html_A:
        if( htmlPtr->anchorStart ){
          style = HtmlPopStyleStack(htmlPtr, Html_EndA);
          htmlPtr->anchorStart = 0;
          anchorFlags = 0;
        }
        z = HtmlMarkupArg(p,"href",0);
        if( z ){
          HtmlLock(htmlPtr);
          style.color = GetLinkColor(htmlPtr, z);
          if( htmlPtr->underlineLinks ){
            style.flags |= STY_Underline;
          }
          if( HtmlUnlock(htmlPtr) ) return;
          anchorFlags |= STY_Anchor;
          PushStyleStack(htmlPtr, Html_EndA, style);
          htmlPtr->anchorStart = p;
        }
        break;
      case Html_EndA:
        if( htmlPtr->anchorStart ){
          p->ref.pOther = htmlPtr->anchorStart;
          style = HtmlPopStyleStack(htmlPtr, Html_EndA);
          htmlPtr->anchorStart = 0;
          anchorFlags = 0;
        }
        break;
      case Html_ADDRESS:
      case Html_EndADDRESS:
      case Html_BLOCKQUOTE:
      case Html_EndBLOCKQUOTE:
        paraAlign = ALIGN_None;
        TestPoint(0);
        break;
      case Html_APPLET:
        if( htmlPtr->zAppletCommand && *htmlPtr->zAppletCommand ){
          nextStyle = style;
          nextStyle.flags |= STY_Invisible;
          PushStyleStack(htmlPtr, Html_EndAPPLET, nextStyle);
          useNextStyle = 1;
        }else{
          PushStyleStack(htmlPtr, Html_EndAPPLET, style);
        }
        TestPoint(0);
        break;
      case Html_B:
        style.font = BoldFont( FontSize(style.font) );
        PushStyleStack(htmlPtr, Html_EndB, style);
        TestPoint(0);
        break;
      case Html_EndAPPLET:
      case Html_EndB:
      case Html_EndBIG:
      case Html_EndCENTER:
      case Html_EndCITE:
      case Html_EndCODE:
      case Html_EndCOMMENT:
      case Html_EndEM:
      case Html_EndFONT:
      case Html_EndI:
      case Html_EndKBD:
      case Html_EndMARQUEE:
      case Html_EndNOBR:
      case Html_EndNOFRAME:
      case Html_EndNOSCRIPT:
      case Html_EndS:
      case Html_EndSAMP:
      case Html_EndSMALL:
      case Html_EndSTRIKE:
      case Html_EndSTRONG:
      case Html_EndSUB:
      case Html_EndSUP:
      case Html_EndTITLE:
      case Html_EndTT:
      case Html_EndU:
      case Html_EndVAR:
        style = HtmlPopStyleStack(htmlPtr, p->base.type);
        TestPoint(0);
        break;
      case Html_BASE:
        z = HtmlMarkupArg(p,"href",0);
        if( z ){
          HtmlLock(htmlPtr);
          z = HtmlResolveUri(htmlPtr, z);
          if( HtmlUnlock(htmlPtr) ) return;
          if( z!=0 ){
            if( htmlPtr->zBaseHref ){
              HtmlFree(htmlPtr->zBaseHref);
            }
            htmlPtr->zBaseHref = z;
          }
        }
        break;
      case Html_EndDIV:
        paraAlign = ALIGN_None;
        style = HtmlPopStyleStack(htmlPtr, p->base.type);
        TestPoint(0);
        break;
      case Html_EndBASEFONT:
        style = HtmlPopStyleStack(htmlPtr, Html_EndBASEFONT);
        style.font = FontFamily(style.font) + 2;
        TestPoint(0);
        break;
      case Html_BIG:
        ScaleFont(&style,1);
        PushStyleStack(htmlPtr, Html_EndBIG, style);
        TestPoint(0);
        break;
      case Html_CAPTION:
        paraAlign = GetAlignment(p, paraAlign);
        TestPoint(0);
        break;
      case Html_EndCAPTION:
        paraAlign = ALIGN_None;
        TestPoint(0);
        break;
      case Html_CENTER:
        paraAlign = ALIGN_None;
        style.align = ALIGN_Center;
        PushStyleStack(htmlPtr, Html_EndCENTER, style);
        TestPoint(0);
        break;
      case Html_CITE:
        PushStyleStack(htmlPtr, Html_EndCITE, style);
        TestPoint(0);
        break;
      case Html_CODE:
        style.font = CWFont( FontSize(style.font) );
        PushStyleStack(htmlPtr, Html_EndCODE, style);
        TestPoint(0);
        break;
      case Html_COMMENT:
        style.flags |= STY_Invisible;
        PushStyleStack(htmlPtr, Html_EndCOMMENT, style);
        TestPoint(0);
        break;
      case Html_DD:
        if( htmlPtr->innerList && htmlPtr->innerList->base.type==Html_DL ){
          p->ref.pOther = htmlPtr->innerList;
          TestPoint(0);
        }else{
          p->ref.pOther = 0;
          TestPoint(0);
        }
        inDt = 0;
        break;
      case Html_DIR:
      case Html_MENU:
      case Html_UL:
        p->list.pPrev = htmlPtr->innerList;
        p->list.cnt = 0;
        htmlPtr->innerList = p;
        if( p->list.pPrev==0 ){
          p->list.type = LI_TYPE_Bullet1;
          p->list.compact = HtmlMarkupArg(p,"compact",0)!=0;
          TestPoint(0);
        }else if( p->list.pPrev->list.pPrev==0 ){
          p->list.type = LI_TYPE_Bullet2;
          p->list.compact = 1;
          TestPoint(0);
        }else{
          p->list.type = LI_TYPE_Bullet3;
          p->list.compact = 1;
          TestPoint(0);
        }
        p->list.type = GetUnorderedListType(p,p->list.type);
        break;
      case Html_EndDL:
        inDt = 0;
        TestPoint(0);
        /* Fall thru into the next case */
      case Html_EndDIR:
      case Html_EndMENU:
      case Html_EndOL:
      case Html_EndUL:
        p->ref.pOther = htmlPtr->innerList;
        if( htmlPtr->innerList ){
          htmlPtr->innerList = htmlPtr->innerList->list.pPrev;
          TestPoint(0);
        }else{
          TestPoint(0);
        }
        break;
      case Html_DIV:
        paraAlign = ALIGN_None;
        style.align = GetAlignment(p, style.align);
        PushStyleStack(htmlPtr, Html_EndDIV, style);
        TestPoint(0);
        break;
      case Html_DT:
        if( htmlPtr->innerList && htmlPtr->innerList->base.type==Html_DL ){
          p->ref.pOther = htmlPtr->innerList;
          TestPoint(0);
        }else{
          p->ref.pOther = 0;
          TestPoint(0);
        }
        inDt = STY_DT;
        break;
      case Html_EndDD:
      case Html_EndDT:
        inDt = 0;
        TestPoint(0);
        break;
      case Html_DL:
        p->list.pPrev = htmlPtr->innerList;
        p->list.cnt = 0;
        htmlPtr->innerList = p;
        p->list.compact = HtmlMarkupArg(p,"compact",0)!=0;
        inDt = 0;
        TestPoint(0);
        break;
      case Html_EM:
        style.font = ItalicFont( FontSize(style.font) );
        PushStyleStack(htmlPtr, Html_EndEM, style);
        TestPoint(0);
        break;
      case Html_EMBED:
        break;
      case Html_BASEFONT:
      case Html_FONT:
        z = HtmlMarkupArg(p,"size",0);
        if( z ){
          if( *z=='-' ){
            size = FontSize(style.font) - atoi(&z[1]);
          }else if( *z=='+' ){
            size = FontSize(style.font) + atoi(&z[1]);
          }else{
            size = atoi(z);
          }
          if( size <= 0 ){
            size = 1;
          }
          if( size >= N_FONT_SIZE ){
            size = N_FONT_SIZE - 1;
          }
          style.font = FontFamily(style.font) + size - 1;
        }
        z = HtmlMarkupArg(p,"color",0);
        if( z ){
          style.color = HtmlGetColorByName(htmlPtr, z);
        }
        PushStyleStack(htmlPtr,
            p->base.type==Html_FONT ? Html_EndFONT : Html_EndBASEFONT, style);
        break;
      case Html_FORM: {
        char *zUrl;
        char *zMethod;
        Tcl_DString cmd;      /* -formcommand callback */
        int result;
        char zToken[50];

        htmlPtr->formStart = 0;
        p->form.id = 0;
        if( htmlPtr->zFormCommand==0 || htmlPtr->zFormCommand[0]==0 ){
          TestPoint(0);
          break;
        }
        zUrl = HtmlMarkupArg(p,"action",0);
        if( zUrl==0 ){
          TestPoint(0);
          break;
        }
        HtmlLock(htmlPtr);
        zUrl = HtmlResolveUri(htmlPtr, zUrl);
        if( HtmlUnlock(htmlPtr) ) return;
        if( zUrl==0 ) break;
        zMethod = HtmlMarkupArg(p,"method","GET");
        sprintf(zToken," %d form ", ++htmlPtr->nForm);
        Tcl_DStringInit(&cmd);
        Tcl_DStringAppend(&cmd, htmlPtr->zFormCommand, -1);
        Tcl_DStringAppend(&cmd, zToken, -1);
        Tcl_DStringAppendElement(&cmd, zUrl);
        HtmlFree(zUrl);
        Tcl_DStringAppendElement(&cmd, zMethod);
        Tcl_DStringStartSublist(&cmd);
        HtmlAppendArglist(&cmd, p);
        Tcl_DStringEndSublist(&cmd);
        HtmlLock(htmlPtr);
        result = Tcl_GlobalEval(htmlPtr->interp, Tcl_DStringValue(&cmd));
        Tcl_DStringFree(&cmd);
        if( HtmlUnlock(htmlPtr) ) return;
        if( result==TCL_OK ){
          htmlPtr->formStart = p;
          p->form.id = htmlPtr->nForm;
        }
        Tcl_ResetResult(htmlPtr->interp);
        break;
      }
      case Html_EndFORM:
        p->ref.pOther = htmlPtr->formStart;
        htmlPtr->formStart = 0;
        TestPoint(0);
        break;
      case Html_H1:
      case Html_H2:
      case Html_H3:
      case Html_H4:
      case Html_H5:
      case Html_H6:
        paraAlign = ALIGN_None;
        i = (p->base.type - Html_H1)/2 + 1;
        if( i>=1 && i<=6 ){
          ScaleFont(&style,header_sizes[i-1]);
        }
        style.font = BoldFont( FontSize(style.font) );
        style.align = GetAlignment(p, style.align);
        PushStyleStack(htmlPtr, Html_EndH1, style);
        break;
      case Html_EndH1:
      case Html_EndH2:
      case Html_EndH3:
      case Html_EndH4:
      case Html_EndH5:
      case Html_EndH6:
        paraAlign = ALIGN_None;
        style = HtmlPopStyleStack(htmlPtr, Html_EndH1);
        TestPoint(0);
        break;
      case Html_HR:
        nextStyle = style;
        style.align = GetAlignment(p, ALIGN_None);
        useNextStyle = 1;
        break;
      case Html_I:
        style.font = ItalicFont( FontSize(style.font) );
        PushStyleStack(htmlPtr, Html_EndI, style);
        TestPoint(0);
        break;
      case Html_IMG:
        HtmlLock(htmlPtr);
        p->image.pImage = HtmlGetImage(htmlPtr, p);
        if( HtmlUnlock(htmlPtr) ) return;
        TestPoint(0);
        break;
      case Html_INPUT:
        p->input.pForm = htmlPtr->formStart;
        TestPoint(0);
        break;
      case Html_KBD:
        style.font = CWFont( FontSize(style.font) );
        PushStyleStack(htmlPtr, Html_EndKBD, style);
        TestPoint(0);
        break;
      case Html_LI:
        if( htmlPtr->innerList ){
          p->li.type = htmlPtr->innerList->list.type;
          if( htmlPtr->innerList->base.type==Html_OL ){
            z = HtmlMarkupArg(p, "value", 0);
            if( z ){
              int n = atoi(z);
              if( n>0 ){
                p->li.cnt = n;
                htmlPtr->innerList->list.cnt = n+1;
                TestPoint(0);
              }else{
                TestPoint(0);
              }
            }else{
              p->li.cnt = htmlPtr->innerList->list.cnt++;
              TestPoint(0);
            }
            p->li.type = GetOrderedListType(p,p->li.type);
          }else{
            p->li.type = GetUnorderedListType(p,p->li.type);
            TestPoint(0);
          }
        }else{
          p->base.flags &= ~HTML_Visible;
          TestPoint(0);
        }
        break;
      case Html_MARQUEE:
        style.flags |= STY_Invisible;
        PushStyleStack(htmlPtr, Html_EndMARQUEE, style);
        TestPoint(0);
        break;
      case Html_NOBR:
        style.flags |= STY_NoBreak;
        PushStyleStack(htmlPtr, Html_EndNOBR, style);
        TestPoint(0);
        break;
      case Html_NOFRAME:
        if( htmlPtr->zFrameCommand && *htmlPtr->zFrameCommand ){
          nextStyle = style;
          nextStyle.flags |= STY_Invisible;
          PushStyleStack(htmlPtr, Html_EndNOFRAME, nextStyle);
          useNextStyle = 1;
        }else{
          PushStyleStack(htmlPtr, Html_EndNOFRAME, style);
        }
        TestPoint(0);
        break;
      case Html_NOSCRIPT:
        if( htmlPtr->zScriptCommand && *htmlPtr->zScriptCommand ){
          nextStyle = style;
          nextStyle.flags |= STY_Invisible;
          PushStyleStack(htmlPtr, Html_EndNOSCRIPT, nextStyle);
          useNextStyle = 1;
        }else{
          PushStyleStack(htmlPtr, Html_EndNOSCRIPT, style);
        }
        TestPoint(0);
        break;
      case Html_OL:
        p->list.pPrev = htmlPtr->innerList;
        p->list.type = GetOrderedListType(p,LI_TYPE_Enum_1);
        p->list.cnt = 1;
        z = HtmlMarkupArg(p,"start",0);
        if( z ){
          int n = atoi(z);
          if( n>0 ){
            p->list.cnt = n;
            TestPoint(0);
          }else{
            TestPoint(0);
          }
        }else{
          TestPoint(0);
        }
        p->list.compact = htmlPtr->innerList!=0 ||
                          HtmlMarkupArg(p,"compact",0)!=0;
        htmlPtr->innerList = p;
        break;
      case Html_P:
        paraAlign = GetAlignment(p, ALIGN_None);
        TestPoint(0);
        break;
      case Html_EndP:
        paraAlign = ALIGN_None;
        TestPoint(0);
        break;
      case Html_PRE:
      case Html_LISTING:
      case Html_XMP:
      case Html_PLAINTEXT:
        paraAlign = ALIGN_None;
        style.font = CWFont( FontSize(style.font) );
        style.flags |= STY_Preformatted;
        PushStyleStack(htmlPtr, Html_EndPRE, style);
        TestPoint(0);
        break;
      case Html_EndPRE:
      case Html_EndLISTING:
      case Html_EndXMP:
        style = HtmlPopStyleStack(htmlPtr, Html_EndPRE);
        TestPoint(0);
        break;
      case Html_S:
        style.flags |= STY_StrikeThru;
        PushStyleStack(htmlPtr, Html_EndS, style);
        TestPoint(0);
        break;
      case Html_SCRIPT:
        if( htmlPtr->zScriptCommand && *htmlPtr->zScriptCommand ){
          Tcl_DString cmd;
          int result;
          Tcl_DStringInit(&cmd);
          Tcl_DStringAppend(&cmd, htmlPtr->zScriptCommand, -1);
          Tcl_DStringStartSublist(&cmd);
          HtmlAppendArglist(&cmd, p);
          Tcl_DStringEndSublist(&cmd);
          Tcl_DStringStartSublist(&cmd);
          Tcl_DStringAppend(&cmd, p->script.zScript, p->script.nScript);
          Tcl_DStringEndSublist(&cmd);
          HtmlLock(htmlPtr);
          result = Tcl_GlobalEval(htmlPtr->interp, Tcl_DStringValue(&cmd));
          Tcl_DStringFree(&cmd);
          if( HtmlUnlock(htmlPtr) ) return;
          Tcl_ResetResult(htmlPtr->interp);
        }
        nextStyle = style;
        style.flags |= STY_Invisible;
        useNextStyle = 1;
        break;
      case Html_SELECT:
        p->input.pForm = htmlPtr->formStart;
        nextStyle.flags |= STY_Invisible;
        useNextStyle = 1;
        PushStyleStack(htmlPtr, Html_EndSELECT, style);
        htmlPtr->formElemStart = p;
        break;
      case Html_EndSELECT:
        style = HtmlPopStyleStack(htmlPtr, Html_EndSELECT);
        if( htmlPtr->formElemStart
        && htmlPtr->formElemStart->base.type==Html_SELECT ){
          p->ref.pOther = htmlPtr->formElemStart;
          MakeInvisible(p->ref.pOther, p);
        }else{
          p->ref.pOther = 0;
        }
        htmlPtr->formElemStart = 0;
        break;
      case Html_STRIKE:
        style.flags |= STY_StrikeThru;
        PushStyleStack(htmlPtr, Html_EndSTRIKE, style);
        TestPoint(0);
        break;
      case Html_STYLE:
        /* Ignore style sheets */
        break;
      case Html_SAMP:
        style.font = CWFont( FontSize(style.font) );
        PushStyleStack(htmlPtr, Html_EndSAMP, style);
        TestPoint(0);
        break;
      case Html_SMALL:
        ScaleFont(&style,-1);
        PushStyleStack(htmlPtr, Html_EndSMALL, style);
        TestPoint(0);
        break;
      case Html_STRONG:
        style.font = BoldFont( FontSize(style.font) );
        PushStyleStack(htmlPtr, Html_EndSTRONG, style);
        TestPoint(0);
        break;
      case Html_SUB:
        ScaleFont(&style,-1);
        if( style.subscript > -6 ){
          style.subscript--;
          TestPoint(0);
        }else{
          TestPoint(0);
        }
        PushStyleStack(htmlPtr, Html_EndSUB, style);
        break;
      case Html_SUP:
        ScaleFont(&style,-1);
        if( style.subscript < 6 ){
          style.subscript++;
          TestPoint(0);
        }else{
          TestPoint(0);
        }
        PushStyleStack(htmlPtr, Html_EndSUP, style);
        break;
      case Html_TABLE:
        paraAlign = ALIGN_None;
        nextStyle = style;
        nextStyle.align = ALIGN_Left;
        z = HtmlMarkupArg(p, "bgcolor", 0);
        if( z ){
          nextStyle.bgcolor = HtmlGetColorByName(htmlPtr, z);
          style.bgcolor = nextStyle.bgcolor;
/*        }else{
          nextStyle.bgcolor = COLOR_Background; */
        }
        PushStyleStack(htmlPtr, Html_EndTABLE, nextStyle);
        useNextStyle = 1;
        htmlPtr->inTd = 0;
        htmlPtr->inTr = 0;
        TestPoint(0);
        break;
      case Html_EndTABLE:
        paraAlign = ALIGN_None;
        if( htmlPtr->inTd ){
          style = HtmlPopStyleStack(htmlPtr, Html_EndTD);
          htmlPtr->inTd = 0;
        }
        if( htmlPtr->inTr ){
          style = HtmlPopStyleStack(htmlPtr, Html_EndTR);
          htmlPtr->inTr = 0;
        }
        style = HtmlPopStyleStack(htmlPtr, p->base.type);
        TestPoint(0);
        break;
      case Html_TD:
        if( htmlPtr->inTd ){
          style = HtmlPopStyleStack(htmlPtr, Html_EndTD);
        }
        htmlPtr->inTd = 1;
        paraAlign = GetAlignment(p, rowAlign);
        if( (z = HtmlMarkupArg(p, "bgcolor", 0))!=0 ){
          style.bgcolor = HtmlGetColorByName(htmlPtr, z);
        }
        PushStyleStack(htmlPtr, Html_EndTD, style);
        TestPoint(0);
        break;
      case Html_TEXTAREA:
        p->input.pForm = htmlPtr->formStart;
        nextStyle = style;
        nextStyle.flags |= STY_Invisible;
        PushStyleStack(htmlPtr, Html_EndTEXTAREA, nextStyle);
        htmlPtr->formElemStart = p;
        useNextStyle = 1;
        TestPoint(0);
        break;
      case Html_EndTEXTAREA:
        style = HtmlPopStyleStack(htmlPtr, Html_EndTEXTAREA);
        if( htmlPtr->formElemStart
        && htmlPtr->formElemStart->base.type==Html_TEXTAREA ){
          p->ref.pOther = htmlPtr->formElemStart;
        }else{
          p->ref.pOther = 0;
        }
        htmlPtr->formElemStart = 0;
        break;
      case Html_TH:
        /* paraAlign = GetAlignment(p, rowAlign); */
        if( htmlPtr->inTd ){
          style = HtmlPopStyleStack(htmlPtr, Html_EndTD);
        }
        paraAlign = GetAlignment(p, ALIGN_Center);
        style.font = BoldFont( FontSize(style.font) );
        if( (z = HtmlMarkupArg(p, "bgcolor", 0))!=0 ){
          style.bgcolor = HtmlGetColorByName(htmlPtr, z);
        }
        PushStyleStack(htmlPtr, Html_EndTD, style);
        htmlPtr->inTd = 1;
        TestPoint(0);
        break;
      case Html_TR:
        if( htmlPtr->inTd ){
          style = HtmlPopStyleStack(htmlPtr, Html_EndTD);
          htmlPtr->inTd = 0;
        }
        if( htmlPtr->inTr ){
          style = HtmlPopStyleStack(htmlPtr, Html_EndTR);
        }
        rowAlign = GetAlignment(p, ALIGN_None);
        if( (z = HtmlMarkupArg(p, "bgcolor", 0))!=0 ){
          style.bgcolor = HtmlGetColorByName(htmlPtr, z);
        }
        PushStyleStack(htmlPtr, Html_EndTR, style);
        htmlPtr->inTr = 1;
        TestPoint(0);
        break;
      case Html_EndTR:
        if( htmlPtr->inTd ){
          style = HtmlPopStyleStack(htmlPtr, Html_EndTD);
          htmlPtr->inTd = 0;
        }
        style = HtmlPopStyleStack(htmlPtr, Html_EndTR);
        htmlPtr->inTr = 0;
        paraAlign = ALIGN_None;
        rowAlign = ALIGN_None;
        TestPoint(0);
        break;
      case Html_EndTD:
      case Html_EndTH:
        style = HtmlPopStyleStack(htmlPtr, Html_EndTD);
        htmlPtr->inTd = 0;
        paraAlign = ALIGN_None;
        rowAlign = ALIGN_None;
        TestPoint(0);
        break;
      case Html_TITLE:
        style.flags |= STY_Invisible;
        PushStyleStack(htmlPtr, Html_EndTITLE, style);
        TestPoint(0);
        break;
      case Html_TT:
        style.font = CWFont( FontSize(style.font) );
        PushStyleStack(htmlPtr, Html_EndTT, style);
        TestPoint(0);
        break;
      case Html_U:
        style.flags |= STY_Underline;
        PushStyleStack(htmlPtr, Html_EndU, style);
        break;
      case Html_VAR:
        style.font = ItalicFont( FontSize(style.font) );
        PushStyleStack(htmlPtr, Html_EndVAR, style);
        TestPoint(0);
        break;
      default:
        TestPoint(0);
        break;
    }
    p->base.style = style;
    p->base.style.flags |= anchorFlags | inDt;
    if( paraAlign!=ALIGN_None ){
      p->base.style.align = paraAlign;
    }
    if( useNextStyle ){
      style = nextStyle;
      useNextStyle = 0;
    }
    TRACE(HtmlTrace_Style,
      ("Style of 0x%08x font=%02d color=%02d bg=%02d "
       "align=%d flags=0x%04x token=%s\n",
      (int)p, p->base.style.font, p->base.style.color, p->base.style.bgcolor,
      p->base.style.align, p->base.style.flags, HtmlTokenName(p)));
    p = p->pNext;
  }

  /* Copy state information back into the htmlPtr structure for
  ** safe keeping. */
  htmlPtr->paraAlignment = paraAlign;
  htmlPtr->rowAlignment = rowAlign;
  htmlPtr->anchorFlags = anchorFlags;
  htmlPtr->inDt = inDt;
  htmlPtr->flags &= ~STYLER_RUNNING;
}

/*
** Compute the size of all elements in the widget.  Assume that a
** style has already been assigned to all elements.
**
** Some of the elements might have already been sized.  Refer to the
** htmlPtr->lastSized and only compute sizes for elements that follow
** this one.  If htmlPtr->lastSized==0, then size everything.
**
** This routine only computes the sizes of individual elements.  The
** size of aggregate elements (like tables) are computed separately.
**
** The HTML_Visible flag is also set on every element that results
** in ink on the page.
**
** This routine may invoke a callback procedure which could delete
** the HTML widget.
*/
void HtmlSizer(HtmlWidget *htmlPtr){
  HtmlElement *p;
  int iFont = -1;
  Tk_Font font;
  int spaceWidth = 0;
  Tk_FontMetrics fontMetrics;
  char *z;
  int stop = 0;

  if( htmlPtr->pFirst==0 ){ TestPoint(0); return; }
  if( htmlPtr->lastSized==0 ){
    p = htmlPtr->pFirst;
    TestPoint(0);
  }else{
    p = htmlPtr->lastSized->pNext;
    TestPoint(0);
  }
  for(; !stop && p; p=p->pNext){
    if( p->base.style.flags & STY_Invisible ){
      p->base.flags &= ~HTML_Visible;
      TestPoint(0);
      continue;
    }
    if( iFont != p->base.style.font ){
      iFont = p->base.style.font;
      HtmlLock(htmlPtr);
      font = HtmlGetFont(htmlPtr, iFont);
      if( HtmlUnlock(htmlPtr) ) break;
      Tk_GetFontMetrics(font, &fontMetrics);
      spaceWidth = 0;
    }
    switch( p->base.type ){
      case Html_Text:
        p->text.w = Tk_TextWidth(font, p->text.zText, p->base.count);
        p->base.flags |= HTML_Visible;
        p->text.descent = fontMetrics.descent;
        p->text.ascent = fontMetrics.ascent;
        if( spaceWidth==0 ){
          spaceWidth = Tk_TextWidth(font, " ", 1);
          TestPoint(0);
        }else{
          TestPoint(0);
        }
        p->text.spaceWidth = spaceWidth;
        break;
      case Html_Space:
        if( spaceWidth==0 ){
          spaceWidth = Tk_TextWidth(font, " ", 1);
        }
        p->space.w = spaceWidth;
        p->space.descent = fontMetrics.descent;
        p->space.ascent = fontMetrics.ascent;
        p->base.flags &= ~HTML_Visible;
        break;
      case Html_TD:
      case Html_TH:
        z = HtmlMarkupArg(p, "rowspan","1");
        p->cell.rowspan = atoi(z);
        z = HtmlMarkupArg(p, "colspan","1");
        p->cell.colspan = atoi(z);
        p->base.flags |= HTML_Visible;
        TestPoint(0);
        break;
      case Html_LI:
        p->li.descent = fontMetrics.descent;
        p->li.ascent = fontMetrics.ascent;
        p->base.flags |= HTML_Visible;
        TestPoint(0);
        break;
      case Html_IMG:
        p->base.flags |= HTML_Visible;
        p->image.redrawNeeded = 0;
        p->image.textAscent = fontMetrics.ascent;
        p->image.textDescent = fontMetrics.descent;
        p->image.align = HtmlGetImageAlignment(p);
        if( p->image.pImage==0 ){
          p->image.ascent = fontMetrics.ascent;
          p->image.descent = fontMetrics.descent;
          p->image.zAlt = HtmlMarkupArg(p, "alt", "<image>");
          p->image.w = Tk_TextWidth(font, p->image.zAlt, strlen(p->image.zAlt));
        }else{
          int w, h;
          p->image.pNext = p->image.pImage->pList;
          p->image.pImage->pList = p;
          Tk_SizeOfImage(p->image.pImage->image, &w, &h);
          p->image.h = h;
          p->image.w = w;
          p->image.ascent = h/2;
          p->image.descent = h - p->image.ascent;
        }
        if( (z = HtmlMarkupArg(p, "width", 0))!=0 ){
          int w = atoi(z);
          if( w>0 ) p->image.w = w;
        }
        if( (z = HtmlMarkupArg(p, "height", 0))!=0 ){
          int h = atoi(z);
          if( h>0 ) p->image.h = h;
        }
        break;
      case Html_HR:
      case Html_TABLE:
        p->base.flags |= HTML_Visible;
        TestPoint(0);
        break;
      case Html_APPLET:
      case Html_EMBED:
      case Html_INPUT:
        p->input.textAscent = fontMetrics.ascent;
        p->input.textDescent = fontMetrics.descent;
        stop = HtmlControlSize(htmlPtr, p);
        break;
      case Html_SELECT:
      case Html_TEXTAREA:
        p->input.textAscent = fontMetrics.ascent;
        p->input.textDescent = fontMetrics.descent;
        break;
      case Html_EndSELECT:
      case Html_EndTEXTAREA:
        if( p->ref.pOther ){
          p->ref.pOther->input.pEnd = p;
          stop = HtmlControlSize(htmlPtr, p->ref.pOther);
        }
        break;
      default:
        p->base.flags &= ~HTML_Visible;
        break;
    }
  }
  if( p ){
    htmlPtr->lastSized = p;
  }else{
    htmlPtr->lastSized = htmlPtr->pLast;
  }
}
