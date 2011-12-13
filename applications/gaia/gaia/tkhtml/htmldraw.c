static char const rcsid[] = "@(#) $Id$";
/*
** Routines used to render HTML onto the screen for the Tk HTML widget.
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
#include "htmldraw.h"
#ifdef USE_TK_STUBS
# include <tkIntXlibDecls.h>
#endif

#define USE_TK_DRAWCHARS 1

/*
** Allocate a new HtmlBlock structure.
*/
static HtmlBlock *AllocBlock(void){
  HtmlBlock *pNew;

  pNew = HtmlAlloc( sizeof(HtmlBlock) );
  if( pNew ){
    memset(pNew, 0, sizeof(*pNew));
    pNew->base.type = Html_Block;
  }
  return pNew;
}

/*
** Free an HtmlBlock structure.  Assume that it is already unlinked
** from the element list and the block list.
*/
static void FreeBlock(HtmlBlock *pBlock){
  if( pBlock ){
    if( pBlock->z ){
      HtmlFree(pBlock->z);
    }
    HtmlFree(pBlock);
  }
}

/*
** Destroy the given Block after first unlinking it from the
** element list.  Note that this unlinks the block from the
** element list only -- not from the block list.
*/
static void UnlinkAndFreeBlock(HtmlWidget *htmlPtr, HtmlBlock *pBlock){
  if( pBlock->base.pNext ){
    pBlock->base.pNext->base.pPrev = pBlock->base.pPrev;
    TestPoint(0);
  }else{
    htmlPtr->pLast = pBlock->base.pPrev;
    TestPoint(0);
  }
  if( pBlock->base.pPrev ){
    pBlock->base.pPrev->base.pNext = pBlock->base.pNext;
    TestPoint(0);
  }else{
    htmlPtr->pFirst = pBlock->base.pNext;
    TestPoint(0);
  }
  pBlock->base.pPrev = pBlock->base.pNext = 0;
  FreeBlock(pBlock);
}

/*
** Append a block to the block list and insert the block into the
** element list immediately prior to the element given.
*/
static void AppendBlock(
  HtmlWidget *htmlPtr,     /* The HTML widget */
  HtmlElement *pToken,     /* The token that comes after pBlock */
  HtmlBlock *pBlock        /* The block to be appended */
){
  pBlock->base.pPrev = pToken->base.pPrev;
  pBlock->base.pNext = pToken;
  pBlock->pPrev = htmlPtr->lastBlock;
  pBlock->pNext = 0;
  if( htmlPtr->lastBlock ){
    htmlPtr->lastBlock->pNext = pBlock;
    TestPoint(0);
  }else{
    htmlPtr->firstBlock = pBlock;
    TestPoint(0);
  }
  htmlPtr->lastBlock = pBlock;
  if( pToken->base.pPrev ){
    pToken->base.pPrev->base.pNext = (HtmlElement*)pBlock;
    TestPoint(0);
  }else{
    htmlPtr->pFirst = (HtmlElement*)pBlock;
    TestPoint(0);
  }
  pToken->base.pPrev = (HtmlElement*)pBlock;
}

/*
** Print an ordered list index into the given buffer.  Use numbering
** like this:
**
**     A  B  C ... Y Z AA BB CC ... ZZ
**
** Revert to decimal for indices greater than 52.
*/
static void GetLetterIndex(char *zBuf, int index, int isUpper){
  int seed;
  if( index<1 || index>52 ){
    sprintf(zBuf,"%d",index);
    TestPoint(0);
    return;
  }
  if( isUpper ){
    seed = 'A';
    TestPoint(0);
  }else{
    seed = 'a';
    TestPoint(0);
  }
  index--;
  if( index<26 ){
    zBuf[0] = seed + index;
    zBuf[1] = 0;
    TestPoint(0);
  }else{
    index -= 26;
    zBuf[0] = seed + index;
    zBuf[1] = seed + index;
    zBuf[2] = 0;
    TestPoint(0);
  }
  strcat(zBuf,".");
}

/*
** Print an ordered list index into the given buffer.  Use roman
** numerals.  For indices greater than a few thousand, revert to
** decimal.
*/
static void GetRomanIndex(char *zBuf, int index, int isUpper){
  int i = 0;
  int j;
  static struct {
    int value;
    char *name;
  } values[] = {
    { 1000, "m"  },
    {  999, "im" },
    {  990, "xm" },
    {  900, "cm" },
    {  500, "d"  },
    {  499, "id" },
    {  490, "xd" },
    {  400, "cd" },
    {  100, "c"  },
    {   99, "ic" },
    {   90, "xc" },
    {   50, "l"  },
    {   49, "il" },
    {   40, "xl" },
    {   10, "x"  },
    {    9, "ix" },
    {    5, "v"  },
    {    4, "iv" },
    {    1, "i"  },
  };
  if( index<1 || index>=5000 ){
    sprintf(zBuf,"%d",index);
    TestPoint(0);
    return;
  }
  for(j=0; index>0 && j<sizeof(values)/sizeof(values[0]); j++){
    int k;
    while( index >= values[j].value ){
      for(k=0; values[j].name[k]; k++){
        zBuf[i++] = values[j].name[k];
        TestPoint(0);
      }
      index -= values[j].value;
      TestPoint(0);
    }
  }
  zBuf[i] = 0;
  if( isUpper ){
    for(i=0; zBuf[i]; i++){
      zBuf[i] += 'A' - 'a';
      TestPoint(0);
    }
  }else{
    TestPoint(0);
  }
  strcat(zBuf,".");
}

/* Draw the selection background for the given block
*/
static void DrawSelectionBackground(
  HtmlWidget *htmlPtr,      /* The HTML widget */
  HtmlBlock *pBlock,        /* The block whose background is drawn */
  Drawable drawable,        /* Draw the background on this drawable */
  int x, int y              /* Virtual coords of top-left of drawable */
){
  int xLeft, xRight;        /* Left and right bounds of box to draw */
  int yTop, yBottom;        /* Top and bottom of box */
  HtmlElement *p = 0;       /* First element of the block */
  Tk_Font font;             /* Font */
  GC gc;                    /* GC for drawing */
  XRectangle xrec;          /* Size of a filled rectangle to be drawn */

  if( pBlock==0 || (pBlock->base.flags & HTML_Selected)==0 ){
    TestPoint(0);
    return;
  }
  xLeft = pBlock->left - x;
  if( pBlock==htmlPtr->pSelStartBlock && htmlPtr->selStartIndex>0 ){
    if( htmlPtr->selStartIndex >= pBlock->n ){ TestPoint(0); return; }
    p = pBlock->base.pNext;
    font = HtmlGetFont(htmlPtr, p->base.style.font);
    if( font==0 ) return;
    if( p->base.type==Html_Text ){
      xLeft = p->text.x - x + Tk_TextWidth(font, pBlock->z,
                                           htmlPtr->selStartIndex);
    }
  }
  xRight = pBlock->right - x;
  if( pBlock==htmlPtr->pSelEndBlock && htmlPtr->selEndIndex<pBlock->n ){
    if( p==0 ){
      p = pBlock->base.pNext;
      font = HtmlGetFont(htmlPtr, p->base.style.font);
      if( font==0 ) return;
    }
    if( p->base.type==Html_Text ){
      xRight = p->text.x - x + Tk_TextWidth(font, pBlock->z,
                                            htmlPtr->selEndIndex);
    }
  }
  yTop = pBlock->top - y;
  yBottom = pBlock->bottom - y;
  gc = HtmlGetGC(htmlPtr, COLOR_Selection, FONT_Any);
  xrec.x = xLeft;
  xrec.y = yTop;
  xrec.width = xRight - xLeft;
  xrec.height = yBottom - yTop;
  XFillRectangles(htmlPtr->display, drawable, gc, &xrec, 1);
}

/*
** Draw a rectangle.  The rectangle will have a 3-D appearance if
** flat==0 and a flat appearance if flat==1.
**
** We don't use Tk_Draw3DRectangle() because it doesn't work well
** when the background color is close to pure white or pure black.
*/
static void HtmlDrawRect(
  HtmlWidget *htmlPtr,              /* The HTML widget */
  Drawable drawable,                /* Draw it here */
  HtmlElement *src,                 /* Element associated with drawing */
  int x, int y, int w, int h,       /* Coordinates of the rectangle */
  int depth,                        /* Width of the relief, or the flat line */
  int relief                        /* The relief.  TK_RELIEF_FLAT omits 3d */
){
  if( depth>0 ){
    int i;
    GC gcLight, gcDark;
    XRectangle xrec[1];
    if( relief!=TK_RELIEF_FLAT ){
      int iLight, iDark;
      iLight = HtmlGetLightShadowColor(htmlPtr, src->base.style.bgcolor);
      gcLight = HtmlGetGC(htmlPtr, iLight, FONT_Any);
      iDark = HtmlGetDarkShadowColor(htmlPtr, src->base.style.bgcolor);
      gcDark = HtmlGetGC(htmlPtr, iDark, FONT_Any);
      if( relief==TK_RELIEF_SUNKEN ){
        GC gcTemp = gcLight;
        gcLight = gcDark;
        gcDark = gcTemp;
      }
    }else{
      gcLight = HtmlGetGC(htmlPtr, src->base.style.color, FONT_Any);
      gcDark = gcLight;
    }
    xrec[0].x = x;
    xrec[0].y = y;
    xrec[0].width = depth;
    xrec[0].height = h;
    XFillRectangles(htmlPtr->display, drawable, gcLight, xrec, 1);
    xrec[0].x = x+w-depth;
    XFillRectangles(htmlPtr->display, drawable, gcDark, xrec, 1);
    for(i=0; i<depth && i<h/2; i++){
      XDrawLine(htmlPtr->display, drawable, gcLight, x+i, y+i, x+w-i-1, y+i);
      XDrawLine(htmlPtr->display, drawable, gcDark, x+i, y+h-i-1,
                 x+w-i-1, y+h-i-1);
    }
  }
  if( h>depth*2 && w>depth*2 ){
    GC gcBg;
    XRectangle xrec[1];
    gcBg = HtmlGetGC(htmlPtr, src->base.style.bgcolor, FONT_Any);
    xrec[0].x = x + depth;
    xrec[0].y = y + depth;
    xrec[0].width = w - depth*2;
    xrec[0].height = h - depth*2;
    XFillRectangles(htmlPtr->display, drawable, gcBg, xrec, 1);
  }
}

/*
** Display a single HtmlBlock.  This is where all the drawing
** happens.
*/
void HtmlBlockDraw(
  HtmlWidget *htmlPtr,   /* The main HTML widget */
  HtmlBlock *pBlock,     /* Block which needs to be drawn */
  Drawable drawable,     /* Draw the line on this */
  int drawableLeft,      /* Virtual coordinate of left edge of drawable */
  int drawableTop,       /* Virtual coordinate of top edge of drawable */
  int drawableWidth,     /* Width of the drawable */
  int drawableHeight     /* Height of the drawable */
){
  Tk_Font font;           /* Font to use to render text */
  GC gc;                  /* A graphics context */
  HtmlElement *src;       /* HtmlElement holding style information */
  HtmlElement *pTable;    /* The table (when drawing part of a table) */
  int x, y;               /* Where to draw */

  if( pBlock==0 ){ TestPoint(0); return; }
  src = pBlock->base.pNext;
  while( src && (src->base.flags & HTML_Visible)==0 ){
    src = src->base.pNext;
    TestPoint(0);
  }
  if( src==0 ){ TestPoint(0); return; }
  if( pBlock->n>0 ){
    /* We must be dealing with plain old text */
    if( src->base.type==Html_Text ){
      x = src->text.x;
      y = src->text.y;
      TestPoint(0);
    }else{
      CANT_HAPPEN;
      return;
    }
    if( pBlock->base.flags & HTML_Selected ){
      HtmlLock(htmlPtr);
      DrawSelectionBackground(htmlPtr, pBlock, drawable,
                              drawableLeft, drawableTop);
      if( HtmlUnlock(htmlPtr) ) return;
    }
    gc = HtmlGetGC(htmlPtr, src->base.style.color, src->base.style.font);
    font = HtmlGetFont(htmlPtr, src->base.style.font);
    if( font==0 ) return;
    Tk_DrawChars(htmlPtr->display,
                 drawable,
                 gc, font,
                 pBlock->z, pBlock->n,
                 x - drawableLeft, y - drawableTop);
    if( src->base.style.flags & STY_Underline ){
      Tk_UnderlineChars(htmlPtr->display, drawable, gc, font, pBlock->z,
                        x - drawableLeft, y-drawableTop, 0, pBlock->n);
    }
    if( src->base.style.flags & STY_StrikeThru ){
      XRectangle xrec;
      xrec.x = pBlock->left - drawableLeft;
      xrec.y = (pBlock->top + pBlock->bottom)/2 - drawableTop;
      xrec.width = pBlock->right - pBlock->left;
      xrec.height = 1 + (pBlock->bottom - pBlock->top > 15);
      XFillRectangles(htmlPtr->display, drawable, gc, &xrec, 1);
    }
    if( pBlock==htmlPtr->pInsBlock && htmlPtr->insStatus>0 ){
      int x;
      XRectangle xrec;
      if( htmlPtr->insIndex < pBlock->n ){
        x = src->text.x - drawableLeft;
        x += Tk_TextWidth(font, pBlock->z, htmlPtr->insIndex);
      }else{
        x = pBlock->right - drawableLeft;
      }
      if( x>0 ){ TestPoint(0); x--; }
      xrec.x = x;
      xrec.y = pBlock->top - drawableTop;
      xrec.width =  2;
      xrec.height = pBlock->bottom - pBlock->top;
      XFillRectangles(htmlPtr->display, drawable, gc, &xrec, 1);
    }
  }else{
    /* We are dealing with a single HtmlElement which contains something
    ** other than plain text. */
    int cnt, w;
    char zBuf[30];
    switch( src->base.type ){
      case Html_LI:
        x = src->li.x;
        y = src->li.y;
        switch( src->li.type ){
          case LI_TYPE_Enum_1:
            sprintf(zBuf,"%d.",src->li.cnt);
            TestPoint(0);
            break;
          case LI_TYPE_Enum_A:
            GetLetterIndex(zBuf,src->li.cnt,1);
            TestPoint(0);
            break;
          case LI_TYPE_Enum_a:
            GetLetterIndex(zBuf,src->li.cnt,0);
            TestPoint(0);
            break;
          case LI_TYPE_Enum_I:
            GetRomanIndex(zBuf,src->li.cnt,1);
            TestPoint(0);
            break;
          case LI_TYPE_Enum_i:
            GetRomanIndex(zBuf,src->li.cnt,0);
            TestPoint(0);
            break;
          default:
            zBuf[0] = 0;
            TestPoint(0);
            break;
        }
        gc = HtmlGetGC(htmlPtr, src->base.style.color, src->base.style.font);
        switch( src->li.type ){
          case LI_TYPE_Undefined:
          case LI_TYPE_Bullet1:
            XFillArc(htmlPtr->display,
                     drawable,
                     gc,
                     x - 7 - drawableLeft, y - 8 - drawableTop, 7, 7,
                     0, 360*64);
            TestPoint(0);
            break;

          case LI_TYPE_Bullet2:
            XDrawArc(htmlPtr->display,
                     drawable,
                     gc,
                     x - 7 - drawableLeft, y - 8 - drawableTop, 7, 7,
                     0, 360*64);
            TestPoint(0);
            break;

          case LI_TYPE_Bullet3:
            XDrawRectangle(htmlPtr->display,
                     drawable,
                     gc,
                     x - 7 - drawableLeft, y - 8 - drawableTop, 7, 7);
            TestPoint(0);
            break;

          case LI_TYPE_Enum_1:
          case LI_TYPE_Enum_A:
          case LI_TYPE_Enum_a:
          case LI_TYPE_Enum_I:
          case LI_TYPE_Enum_i:
            cnt = strlen(zBuf);
            font = HtmlGetFont(htmlPtr, src->base.style.font);
            if( font==0 ) return;
            w = Tk_TextWidth(font, zBuf, cnt);
            Tk_DrawChars(htmlPtr->display,
                 drawable,
                 gc, font,
                 zBuf, cnt,
                 x - w - drawableLeft, y - drawableTop);
            TestPoint(0);
            break;
        }
        break;
      case Html_HR: {
        int relief = htmlPtr->ruleRelief;
        switch( relief ){
          case TK_RELIEF_RAISED:
          case TK_RELIEF_SUNKEN:
            break;
          default:
            relief = TK_RELIEF_FLAT;
            break;
        }
        HtmlDrawRect(htmlPtr, drawable, src,
            src->hr.x - drawableLeft,
            src->hr.y - drawableTop,
            src->hr.w,
            src->hr.h,
            1, relief);
        break;
      }
      case Html_TABLE: {
        int relief = htmlPtr->tableRelief;
        switch( relief ){
          case TK_RELIEF_RAISED:
          case TK_RELIEF_SUNKEN:
            break;
          default:
            relief = TK_RELIEF_FLAT;
            break;
        }
        HtmlDrawRect(htmlPtr, drawable, src,
                           src->table.x - drawableLeft,
                           src->table.y - drawableTop,
                           src->table.w,
                           src->table.h,
                           src->table.borderWidth,
                           relief);
        break;
      }
      case Html_TH:
      case Html_TD: {
        int depth, relief;
        pTable = src->cell.pTable;
        depth = pTable && pTable->table.borderWidth>0;
        switch( htmlPtr->tableRelief ){
          case TK_RELIEF_RAISED:  relief = TK_RELIEF_SUNKEN; break;
          case TK_RELIEF_SUNKEN:  relief = TK_RELIEF_RAISED; break;
          default:                relief = TK_RELIEF_FLAT;   break;
        }
        HtmlDrawRect(htmlPtr, drawable, src,
                         src->cell.x - drawableLeft,
                         src->cell.y - drawableTop,
                         src->cell.w,
                         src->cell.h,
                         depth,
                         relief);
        break;
      }
      case Html_IMG:
        if( src->image.pImage ){
          HtmlDrawImage(src, drawable, drawableLeft, drawableTop,
                        drawableLeft + drawableWidth,
                        drawableTop + drawableHeight);
        }else if( src->image.zAlt ){
          gc = HtmlGetGC(htmlPtr, src->base.style.color, src->base.style.font);
          font = HtmlGetFont(htmlPtr, src->base.style.font);
          if( font==0 ) return;
          Tk_DrawChars(htmlPtr->display,
                 drawable,
                 gc, font,
                 src->image.zAlt, strlen(src->image.zAlt),
                 src->image.x - drawableLeft,
                 src->image.y - drawableTop);
          TestPoint(0);
        }
        break;
      default:
        TestPoint(0);
        break;
    }
  }
}

/*
** Draw all or part of an image.
*/
void HtmlDrawImage(
  HtmlElement *pElem,    /* The <IMG> to be drawn */
  Drawable drawable,     /* Draw it here */
  int drawableLeft,      /* left edge of the drawable */
  int drawableTop,       /* Virtual canvas coordinate for top of drawable */
  int drawableRight,     /* right edge of the drawable */
  int drawableBottom     /* bottom edge of the drawable */
){
  int imageTop;          /* virtual canvas coordinate for top of image */
  int x, y;              /* where to place image on the drawable */
  int imageX, imageY;    /* \__  Subset of image that fits    */
  int imageW, imageH;    /* /    on the drawable              */

  imageTop = pElem->image.y - pElem->image.ascent;
  y = imageTop - drawableTop;
  if( imageTop + pElem->image.h > drawableBottom ){
    imageH = drawableBottom - imageTop;
    TestPoint(0);
  }else{
    imageH = pElem->image.h;
    TestPoint(0);
  }
  if( y<0 ){
    imageY = -y;
    imageH += y;
    y = 0;
    TestPoint(0);
  }else{
    imageY = 0;
    TestPoint(0);
  }
  x = pElem->image.x - drawableLeft;
  if( pElem->image.x + pElem->image.w > drawableRight ){
    imageW = drawableRight - pElem->image.x;
    TestPoint(0);
  }else{
    imageW = pElem->image.w;
    TestPoint(0);
  }
  if( x<0 ){
    imageX = -x;
    imageW += x;
    x = 0;
    TestPoint(0);
  }else{
    imageX = 0;
    TestPoint(0);
  }
  Tk_RedrawImage(pElem->image.pImage->image, imageX, imageY, imageW, imageH,
                 drawable, x, y);
  pElem->image.redrawNeeded = 0;
}

/*
** Recompute the following fields of the given block structure:
**
**    base.count         The number of elements described by this
**                       block structure.
**
**    n                  The number of characters of text output
**                       associated with this block.  If the block
**                       renders something other than text (ex: <IMG>)
**                       then set n to 0.
**
**    z                  Pointer to malloced memory containing the
**                       text associated with this block.  NULL if
**                       n is 0.
**
** Return a pointer to the first HtmlElement not covered by the
** block.
*/
static HtmlElement *FillOutBlock(HtmlWidget *htmlPtr, HtmlBlock *p){
  HtmlElement *pElem;
  int go;
  int n;
  int x, y;
  int i;
  HtmlStyle style;
  int firstSelected;      /* First selected character in this block */
  int lastSelected;       /* Last selected character in this block */
  char zBuf[400];

  /*
  ** Reset n and z
  */
  if( p->n ){
    p->n = 0;
  }
  if( p->z ){
    HtmlFree(p->z);
  }
  firstSelected = 1000000;
  lastSelected = -1;

  /*
  ** Skip over HtmlElements that aren't directly displayed.
  */
  pElem = p->base.pNext;
  p->base.count = 0;
  while( pElem && (pElem->base.flags & HTML_Visible)==0 ){
    HtmlElement *pNext = pElem->pNext;
    if( pElem->base.type==Html_Block ){
      UnlinkAndFreeBlock(htmlPtr, &pElem->block);
      TestPoint(0);
    }else{
      p->base.count++;
      TestPoint(0);
    }
    pElem = pNext;
  }
  if( pElem==0 ){ TestPoint(0); return 0; }

  /*
  ** Handle "special" elements.
  */
  if( pElem->base.type!=Html_Text ){
    switch( pElem->base.type ){
      case Html_HR:
        p->top = pElem->hr.y - pElem->hr.h;
        p->bottom = pElem->hr.y;
        p->left = pElem->hr.x;
        p->right = pElem->hr.x + pElem->hr.w;
        TestPoint(0);
        break;
      case Html_LI:
        p->top = pElem->li.y - pElem->li.ascent;
        p->bottom = pElem->li.y + pElem->li.descent;
        p->left = pElem->li.x - 10;
        p->right = pElem->li.x + 10;
        TestPoint(0);
        break;
      case Html_TD:
      case Html_TH:
        p->top = pElem->cell.y;
        p->bottom = pElem->cell.y + pElem->cell.h;
        p->left = pElem->cell.x;
        p->right = pElem->cell.x + pElem->cell.w;
        TestPoint(0);
        break;
      case Html_TABLE:
        p->top = pElem->table.y;
        p->bottom = pElem->table.y + pElem->table.h;
        p->left = pElem->table.x;
        p->right = pElem->table.x + pElem->table.w;
        TestPoint(0);
        break;
      case Html_IMG:
        p->top = pElem->image.y - pElem->image.ascent;
        p->bottom = pElem->image.y + pElem->image.descent;
        p->left = pElem->image.x;
        p->right = pElem->image.x + pElem->image.w;
        TestPoint(0);
        break;
    }
    p->base.count++;
    TestPoint(0);
    return pElem->pNext;
  }

  /*
  ** If we get this far, we must be dealing with text.
  */
  n = 0;
  x = pElem->text.x;
  y = pElem->text.y;
  p->top = y - pElem->text.ascent;
  p->bottom = y + pElem->text.descent;
  p->left = x;
  style = pElem->base.style;
  go = 1;
  while( pElem ){
    HtmlElement *pNext = pElem->pNext;
    switch( pElem->base.type ){
      case Html_Text:
        if( pElem->base.style.flags & STY_Invisible ){
          TestPoint(0);
          break;
        }
        if( pElem->text.spaceWidth <=0 ){
          CANT_HAPPEN;
          break;
        }
        if( y != pElem->text.y
        ||  style.font != pElem->base.style.font
        ||  style.color != pElem->base.style.color
        ||  (style.flags & STY_FontMask)
              != (pElem->base.style.flags & STY_FontMask)
        ){
          go = 0;
          TestPoint(0);
        }else{
          int sw = pElem->text.spaceWidth;
          int nSpace = (pElem->text.x - x) / sw;
          if( nSpace * sw + x != pElem->text.x ){
            go = 0;
            TestPoint(0);
          }else if( n + nSpace + pElem->base.count >= sizeof(zBuf) ){
            go = 0;
            TestPoint(0);
          }else{
            for(i=0; i<nSpace; i++){
              zBuf[n++] = ' ';
              TestPoint(0);
            }
            strcpy(&zBuf[n], pElem->text.zText);
            n += pElem->base.count;
            x = pElem->text.x + pElem->text.w;
          }
        }
        break;

      case Html_Space:
        if( pElem->base.style.font != style.font ){
          pElem = pElem->pNext;
          go = 0;
        }else if( (style.flags & STY_Preformatted)!=0
                  && (pElem->base.flags & HTML_NewLine)!=0 ){
          pElem = pElem->pNext;
          go = 0;
        }
        break;

      case Html_Block:
        UnlinkAndFreeBlock(htmlPtr,&pElem->block);
        break;

      case Html_A:
      case Html_EndA:
        go = 0;
        break;

      default:
        if( pElem->base.flags & HTML_Visible ) go = 0;
        TestPoint(0);
        break;
    }
    if( go==0 ) break;
    p->base.count++;
    pElem = pNext;
  }
  p->right = x;

  while( n>0 && zBuf[n-1]==' ' ){ TestPoint(0); n--; }
  p->z = HtmlAlloc( n );
  strncpy(p->z, zBuf, n);
  p->n = n;
  return pElem;
}

/*
** Scan ahead looking for a place to put a block.  Return a pointer
** to the element which should come immediately after the block.
**
** if pCnt!=0, then put the number of elements skipped in *pCnt.
*/
static HtmlElement *FindStartOfNextBlock(
  HtmlWidget *htmlPtr,     /* The HTML widget */
  HtmlElement *p,          /* First candid for the start of a block */
  int *pCnt                /* Write number of elements skipped here */
){
  int cnt = 0;

  while( p && (p->base.flags & HTML_Visible)==0 ){
    HtmlElement *pNext = p->pNext;
    if( p->base.type==Html_Block ){
      UnlinkAndFreeBlock(htmlPtr, &p->block);
    }else{
      cnt++;
    }
    p = pNext;
  }
  if( pCnt ){ *pCnt = cnt; }
  return p;
}


/*
** Add additional blocks to the block list in order to cover
** all elements on the element list.
**
** If any old blocks are found on the element list, they must
** be left over from a prior rendering.  Unlink and delete them.
*/
void HtmlFormBlocks(HtmlWidget *htmlPtr){
  HtmlElement *pElem;

  if( htmlPtr->lastBlock ){
    pElem = FillOutBlock(htmlPtr, htmlPtr->lastBlock);
  }else{
    pElem = htmlPtr->pFirst;
  }
  while( pElem ){
    int cnt;
    pElem = FindStartOfNextBlock(htmlPtr, pElem, &cnt);
    if( pElem ){
      HtmlBlock *pNew = AllocBlock();
      if( htmlPtr->lastBlock ){
        htmlPtr->lastBlock->base.count += cnt;
      }
      AppendBlock(htmlPtr, pElem, pNew);
      pElem = FillOutBlock(htmlPtr, pNew);
    }
  }
}
