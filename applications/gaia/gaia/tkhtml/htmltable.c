static char const rcsid[] = "@(#) $Id$";
/*
** Routines for doing layout of HTML tables
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
#include <ctype.h>
#include <math.h>
#include "htmltable.h"

/*
** Default values for various table style parameters
*/
#define DFLT_BORDER             0
#define DFLT_CELLSPACING_3D     5
#define DFLT_CELLSPACING_FLAT   0
#define DFLT_CELLPADDING        2
#define DFLT_HSPACE             0
#define DFLT_VSPACE             0

#if INTERFACE
/*
** Set parameter A to the maximum of A and B.
*/
#define SETMAX(A,B)  if( (A)<(B) ){ (A) = (B); }
#define MAX(A,B)     ((A)<(B)?(B):(A))
#endif

/*
** Return the appropriate cell spacing for the given table.
*/
static int CellSpacing(HtmlWidget *htmlPtr, HtmlElement *pTable){
  char *z;
  int relief;
  int cellSpacing;

  z = HtmlMarkupArg(pTable, "cellspacing", 0);
  if( z==0 ){
     relief = htmlPtr->tableRelief;
     if( relief==TK_RELIEF_RAISED || relief==TK_RELIEF_SUNKEN ){
       cellSpacing = DFLT_CELLSPACING_3D;
     }else{
       cellSpacing = DFLT_CELLSPACING_FLAT;
     }
  }else{
    cellSpacing = atoi(z);
  }
  return cellSpacing;
}

/* Forward declaration */
static HtmlElement *MinMax(HtmlWidget*, HtmlElement *, int *, int *, int);

/* pStart points to a <table>.  Compute the number of columns, the
** minimum and maximum size for each column and the overall minimum
** and maximum size for this table and store these value in the
** pStart structure.  Return a pointer to the </table> element,
** or to NULL if there is no </table>.
**
** The min and max size for column N (where the leftmost column has
** N==1) is pStart->minW[1] and pStart->maxW[1].  The pStart->minW[0]
** and pStart->maxW[0] entries contain the minimum and maximum widths
** of the whole table, including any cell padding, cell spacing,
** border width and "hspace".  The values of pStart->minW[I] for I>=1
** do not contain any cell padding, cell spacing or border width.
** Only pStart->minW[0] contains these extra spaces.
**
** The back references from </table>, </tr>, </td> and </th> back to
** the <table> markup are also filled in.  And for each <td> and <th>
** markup, the pTable and pEnd fields are set to their proper values.
*/
static HtmlElement *TableDimensions(
  HtmlWidget *htmlPtr,               /* The HTML widget */
  HtmlElement *pStart,               /* The <table> markup */
  int lineWidth                      /* Total widget available to the table */
){
  HtmlElement *p;                    /* Element being processed */
  HtmlElement *pNext;                /* Next element to process */
  int iCol = 0;                      /* Current column number.  1..N */
  int iRow = 0;                      /* Current row number */
  int inRow = 0;                     /* True if in between <TR> and </TR> */
  int i, j;                          /* Loop counters */
  int n;                             /* Number of columns */
  int minW, maxW, requestedW;        /* min, max, requested width for a cell */
  int noWrap;                        /* true for NOWRAP cells */
  int colspan;                       /* Column span for the current cell */
  int rowspan;                       /* Row span for the current cell */
  char *z;                           /* Value of a <table> parameter */
  int cellSpacing;                   /* Value of CELLSPACING parameter */
  int cellPadding;                   /* Value of CELLPADDING parameter */
  int tbw;                           /* Width of border around whole table */
  int cbw;                           /* Width of border around one cell */
  int hspace;                        /* Value of HSPACE parameter */
  int separation;                    /* Space between columns */
  int margin;                        /* Space between left margin and 1st col */
  int availWidth;                    /* Part of lineWidth still available */
  int maxTableWidth;                 /* Amount of lineWidth available to table*/
  int fromAbove[HTML_MAX_COLUMNS+1]; /* Cell above extends thru this row */
  int min0span[HTML_MAX_COLUMNS+1];  /* Min for colspan=0 cells */
  int max0span[HTML_MAX_COLUMNS+1];  /* Max for colspan=0 cells */
  int reqW[HTML_MAX_COLUMNS+1];      /* Requested width for each column */

  /* colMin[A][B] is the absolute minimum width of all columns between
  ** A+1 and B+1.  colMin[B][A] is the requested width of columns between
  ** A+1 and B+1.  This information is used to add in the constraints imposed
  ** by <TD COLSPAN=N> markup where N>=2.
  */
  int colMin[HTML_MAX_COLUMNS+1][HTML_MAX_COLUMNS+1];
# define ColMin(A,B) colMin[(A)-1][(B)-1]
# define ColReq(A,B) colMin[(B)-1][(A)-1]

  if( pStart==0 || pStart->base.type!=Html_TABLE ){
    TestPoint(0);
    return pStart;
  }
  TRACE_PUSH(HtmlTrace_Table1);
  TRACE(HtmlTrace_Table1, ("Starting TableDimensions..\n"));
  pStart->table.nCol = 0;
  pStart->table.nRow = 0;
  z = HtmlMarkupArg(pStart, "border", 0);
  if( z && *z==0 ) z = "2";
  tbw = pStart->table.borderWidth = z ? atoi(z) : DFLT_BORDER;
  cbw = tbw>0;
  z = HtmlMarkupArg(pStart, "cellpadding", 0);
  cellPadding = z ? atoi(z) : DFLT_CELLPADDING;
  cellSpacing = CellSpacing(htmlPtr, pStart);
#ifdef DEBUG
  /* The HtmlTrace_Table4 flag causes tables to be draw with borders
  ** of 2, cellPadding of 5 and cell spacing of 2.  This makes the
  ** table clearly visible.  Useful for debugging. */
  if( HtmlTraceMask & HtmlTrace_Table4 ){
    tbw = pStart->table.borderWidth = 2;
    cbw = 1;
    cellPadding = 5;
    cellSpacing = 2;
    pStart->base.style.bgcolor = COLOR_Background;
  }
#endif
  separation = cellSpacing + 2*(cellPadding + cbw);
  margin = tbw + cellSpacing + cbw + cellPadding;
  z = HtmlMarkupArg(pStart, "hspace", 0);
  hspace = z ? atoi(z) : DFLT_HSPACE;

  for(p=pStart->pNext; p && p->base.type!=Html_EndTABLE; p=pNext){
    pNext = p->pNext;
    switch( p->base.type ){
      case Html_EndTD:
      case Html_EndTH:
      case Html_EndTABLE:
        p->ref.pOther = pStart;
        TestPoint(0);
        break;
      case Html_EndTR:
        p->ref.pOther = pStart;
        inRow = 0;
        TestPoint(0);
        break;
      case Html_TR:
        p->ref.pOther = pStart;
        iRow++;
        pStart->table.nRow++;
        iCol = 0;
        inRow = 1;
        maxTableWidth = availWidth = lineWidth - 2*margin;
        TestPoint(0);
        break;
      case Html_CAPTION:
        while( p && p->base.type!=Html_EndTABLE
               && p->base.type!=Html_EndCAPTION ){
          p = p->pNext;
          TestPoint(0);
        }
        break;
      case Html_TD:
      case Html_TH:
        if( !inRow ){
          /* If the <TR> markup is omitted, insert it. */
          HtmlElement *pNew = HtmlAlloc( sizeof(HtmlRef) );
          if( pNew==0 ) break;
          memset(pNew, 0, sizeof(HtmlRef));
          pNew->base = p->base;
          pNew->base.pNext = p;
          pNew->base.type = Html_TR;
          pNew->base.count = 0;
          p->base.pPrev->base.pNext = pNew;
          p->base.pPrev = pNew;
          pNext = pNew;
          break;
        }
        do{
          iCol++;
        }while( iCol <= pStart->table.nCol && fromAbove[iCol] > iRow );
        p->cell.pTable = pStart;
        colspan = p->cell.colspan;
        if( colspan==0 ){
          colspan = 1;
        }
        if( iCol + colspan - 1 > pStart->table.nCol ){
          int nCol = iCol + colspan - 1;
          if( nCol > HTML_MAX_COLUMNS ){
            nCol = HTML_MAX_COLUMNS;
          }
          for(i=pStart->table.nCol+1; i<=nCol; i++){
            fromAbove[i] = 0;
            pStart->table.minW[i] = 0;
            pStart->table.maxW[i] = 0;
            min0span[i] = 0;
            max0span[i] = 0;
            reqW[i] = 0;
            for(j=1; j<i; j++){
              ColMin(j,i) = 0;
              ColReq(j,i) = 0;
            }
          }
          pStart->table.nCol = nCol;
        }
        noWrap = HtmlMarkupArg(p, "nowrap", 0)!=0;
        pNext = MinMax(htmlPtr, p, &minW, &maxW, availWidth);
        p->cell.pEnd = pNext;
        if( (z = HtmlMarkupArg(p, "width", 0))!=0 ){
          for(i=0; isdigit(z[i]); i++){}
          if( strcmp(z,"*")==0 ){
            requestedW = availWidth;
          }else if( z[i]==0 ){
            requestedW = atoi(z);
          }else if( z[i]=='%' ){
            /* requestedW = (atoi(z)*availWidth + 99)/100; */
            requestedW = (atoi(z)*maxTableWidth + 99)/100;
          }
        }else{
          requestedW = 0;
        }
        TRACE(HtmlTrace_Table1,
          ("Row %d Column %d: min=%d max=%d req=%d stop at %s\n",
            iRow,iCol,minW,maxW,requestedW, HtmlTokenName(p->cell.pEnd)));
        if( noWrap ){
          minW = maxW;
        }
        if( iCol + p->cell.colspan <= HTML_MAX_COLUMNS ){
          int min = 0;
          if( p->cell.colspan==0 ){
            SETMAX( min0span[iCol], minW );
            SETMAX( max0span[iCol], maxW );
            min = min0span[iCol] + separation;
          }else if( colspan==1 ){
            SETMAX( pStart->table.minW[iCol], minW );
            SETMAX( pStart->table.maxW[iCol], maxW );
            SETMAX( reqW[iCol], requestedW );
            min = pStart->table.minW[iCol] + separation;
          }else{
            int n = p->cell.colspan;
            SETMAX( ColMin(iCol,iCol+n-1), minW);
            SETMAX( ColReq(iCol,iCol+n-1), requestedW);
            min = minW + separation;
#if 0
            maxW = (maxW + (n - 1)*(1-separation))/n;
            for(i=iCol; i<iCol + n && i<HTML_MAX_COLUMNS; i++){
              SETMAX( pStart->table.maxW[i], maxW );
            }
#endif
          }
          availWidth -= min;
        }
        rowspan = p->cell.rowspan;
        if( rowspan==0 ){
          rowspan = LARGE_NUMBER;
        }
        if( rowspan>1 ){
          for(i=iCol; i<iCol + p->cell.colspan && i<HTML_MAX_COLUMNS; i++){
            fromAbove[i] = iRow + rowspan;
          }
        }
        if( p->cell.colspan > 1 ){
          iCol += p->cell.colspan - 1;
        }else if( p->cell.colspan==0 ){
          iCol = HTML_MAX_COLUMNS + 1;
        }
        break;
    }
  }

#ifdef DEBUG
  if( HtmlTraceMask & HtmlTrace_Table6 ){
    char *zSpace = "";
    TRACE_INDENT;
    for(i=1; i<=pStart->table.nCol; i++){
      printf("%s%d:%d..%d",zSpace,i,
         pStart->table.minW[i],pStart->table.maxW[i]);
      if( reqW[i]>0 ){
        printf("(w=%d)",reqW[i]);
      }
      zSpace = "  ";
    }
    printf("\n");
    for(i=1; i<pStart->table.nCol; i++){
      for(j=i+1; j<=pStart->table.nCol; j++){
        if( ColMin(i,j)>0 ){
          TRACE_INDENT;
          printf("ColMin(%d,%d) = %d\n", i, j, ColMin(i,j));
        }
        if( ColReq(i,j)>0 ){
          TRACE_INDENT;
          printf("ColReq(%d,%d) = %d\n", i, j, ColReq(i,j));
        }
      }
    }
  }
#endif

  /* Compute the min and max width of each column
  */
  for(i=1; i<=pStart->table.nCol; i++){
    int sumMin, sumReq, sumMax;

    /* Reduce the max[] field to N for columns that have "width=N" */
    if( reqW[i]>0 ){
      pStart->table.maxW[i] = MAX(pStart->table.minW[i],reqW[i]);
    }

    /* Expand the width of columns marked with "colspan=0".
    */
    if( min0span[i]>0 || max0span[i]>0 ){
      int n = pStart->table.nCol - i + 1;
      minW = (min0span[i] + (n - 1)*(1-separation))/n;
      maxW = (max0span[i] + (n - 1)*(1-separation))/n;
      for(j=i; j<=pStart->table.nCol; j++){
        SETMAX( pStart->table.minW[j], minW );
        SETMAX( pStart->table.maxW[j], maxW );
      }
    }

    /* Expand the minW[] of columns to accomodate "colspan=N" constraints.
    ** The minW[] is expanded up to the maxW[] first.  Then all the maxW[]s
    ** are expanded in proportion to their sizes.  The same thing occurs
    ** for reqW[]s.
    */
    sumReq = reqW[i];
    sumMin = pStart->table.minW[i];
    sumMax = pStart->table.maxW[i];
    for(j=i-1; j>=1; j--){
      int cmin, creq;
      sumMin += pStart->table.minW[j];
      sumMax += pStart->table.maxW[j];
      sumReq += reqW[i];
      cmin = ColMin(j,i);
      if( cmin>sumMin ){
        int k;
        double scale;
        int *tminW = pStart->table.minW;
        int *tmaxW = pStart->table.maxW;
        if( sumMin<sumMax ){
          scale = (double)(cmin - sumMin)/(double)(sumMax - sumMin);
          for(k=j; k<=i; k++){
            sumMin -= tminW[k];
            tminW[k] = (tmaxW[k] - tminW[k])*scale + tminW[k];
            sumMin += tminW[k];
          }
        }else if( sumMin>0 ){
          scale = (double)cmin/(double)sumMin;
          for(k=j; k<=i; k++){
            sumMin -= tminW[k];
            tminW[k] = tmaxW[k] = tminW[k]*scale;
            sumMin += tminW[k];
          }
        }else{
          int unit = cmin/(i-j+1);
          for(k=j; k<=i; k++){
            tminW[k] = tmaxW[k] = unit;
            sumMin += tminW[k];
          }
        }
      }
      creq = ColReq(j,i);
      if( creq>sumReq ){
        int k;
        double scale;
        int *tmaxW = pStart->table.maxW;
        if( sumReq<sumMax ){
          scale = (double)(creq - sumReq)/(double)(sumMax - sumReq);
          for(k=j; k<=i; k++){
            sumReq -= reqW[k];
            reqW[k] = (tmaxW[k] - reqW[k])*scale + reqW[k];
            sumReq += reqW[k];
          }
        }else if( sumReq>0 ){
          scale = (double)creq/(double)sumReq;
          for(k=j; k<=i; k++){
            sumReq -= reqW[k];
            reqW[k] = reqW[k]*scale;
            sumReq += reqW[k];
          }
        }else{
          int unit = creq/(i-j+1);
          for(k=j; k<=i; k++){
            reqW[k] = unit;
            sumReq += reqW[k];
          }
        }
      }
    }
  }

#ifdef DEBUG
  if( HtmlTraceMask & HtmlTrace_Table6 ){
    char *zSpace = "";
    TRACE_INDENT;
    for(i=1; i<=pStart->table.nCol; i++){
      printf("%s%d:%d..%d",zSpace,i,
         pStart->table.minW[i],pStart->table.maxW[i]);
      if( reqW[i]>0 ){
        printf("(w=%d)",reqW[i]);
      }
      zSpace = "  ";
    }
    printf("\n");
  }
#endif

  /* Compute the min and max width of the whole table
  */
  n = pStart->table.nCol;
  requestedW = tbw*2 + (n+1)*cellSpacing + n*2*(cellPadding + cbw);
  pStart->table.minW[0] = requestedW;
  pStart->table.maxW[0] = requestedW;
  for(i=1; i<=pStart->table.nCol; i++){
    pStart->table.minW[0] += pStart->table.minW[i];
    pStart->table.maxW[0] += pStart->table.maxW[i];
    requestedW += MAX(reqW[i], pStart->table.minW[i]);
  }

  /* Figure out how wide to draw the table */
  z = HtmlMarkupArg(pStart, "width", 0);
  if( z ){
    int len = strlen(z);
    int totalWidth;
    if( len>0 && z[len-1]=='%' ){
      totalWidth = (atoi(z) * lineWidth)/100;
    }else{
      totalWidth = atoi(z);
    }
    SETMAX( requestedW, totalWidth );
  }
  if( lineWidth && (requestedW > lineWidth) ){
    TRACE(HtmlTrace_Table5,("RequestedW reduced to lineWidth: %d -> %d\n",
       requestedW, lineWidth));
    requestedW = lineWidth;
  }
  if( requestedW > pStart->table.minW[0] ){
    float scale;
    int *tminW = pStart->table.minW;
    int *tmaxW = pStart->table.maxW;
    TRACE(HtmlTrace_Table5,
        ("Expanding table minW from %d to %d.  (reqW=%d width=%s)\n",
          tminW[0], requestedW, requestedW, z));
    if( tmaxW[0] > tminW[0] ){
      scale = (double)(requestedW - tminW[0]) / (double)(tmaxW[0] - tminW[0]);
      for(i=1; i<=pStart->table.nCol; i++){
        tminW[i] += (tmaxW[i] - tminW[i]) * scale;
        SETMAX(tmaxW[i], tminW[i]);
      }
    }else if( tminW[0]>0 ){
      scale = requestedW/(double)tminW[0];
      for(i=1; i<=pStart->table.nCol; i++){
        tminW[i] *= scale;
        tmaxW[i] *= scale;
      }
    }else if( pStart->table.nCol>0 ){
      int unit = (requestedW - margin)/pStart->table.nCol - separation;
      if( unit<0 ) unit = 0;
      for(i=1; i<=pStart->table.nCol; i++){
        tminW[i] = tmaxW[i] = unit;
      }
    }else{
      tminW[0] = tmaxW[0] = requestedW;
    }
    pStart->table.minW[0] = requestedW;
    SETMAX( pStart->table.maxW[0], requestedW );
  }

#ifdef DEBUG
  if( HtmlTraceMask & HtmlTrace_Table5 ){
    TRACE_INDENT;
    printf("Start with %s and ", HtmlTokenName(pStart));
    printf("end with %s\n", HtmlTokenName(p));
    TRACE_INDENT;
    printf("nCol=%d minWidth=%d maxWidth=%d\n",
      pStart->table.nCol, pStart->table.minW[0], pStart->table.maxW[0]);
    for(i=1; i<=pStart->table.nCol; i++){
      TRACE_INDENT;
      printf("Column %d minWidth=%d maxWidth=%d\n",
         i, pStart->table.minW[i], pStart->table.maxW[i]);
    }
  }
#endif

  TRACE(HtmlTrace_Table1,
     ("Result of TableDimensions: min=%d max=%d nCol=%d\n",
     pStart->table.minW[0], pStart->table.maxW[0], pStart->table.nCol));
  TRACE_POP(HtmlTrace_Table1);
  return p;
}

/*
** Given a list of elements, compute the minimum and maximum width needed
** to render the list.  Stop the search at the first element seen that is
** in the following set:
**
**       <tr>  <td>  <th>  </tr>  </td>  </th>  </table>
**
** Return a pointer to the element that stopped the search, or to NULL
** if we ran out of data.
**
** Sometimes the value returned for both min and max will be larger than
** the true minimum and maximum.  This is rare, and only occurs if the
** element string contains figures with flow-around text.
*/
static HtmlElement *MinMax(
  HtmlWidget *htmlPtr,     /* The Html widget */
  HtmlElement *p,          /* Start the search here */
  int *pMin,               /* Return the minimum width here */
  int *pMax,               /* Return the maximum width here */
  int lineWidth            /* Total width available */
){
  int min = 0;             /* Minimum width so far */
  int max = 0;             /* Maximum width so far */
  int indent = 0;          /* Amount of indentation (minimum) */
  int obstacle = 0;        /* Possible obstacles in the margin */
  int x1 = 0;              /* Length of current line assuming maximum length */
  int x2 = 0;              /* Length of current line assuming minimum length */
  int go = 1;              /* Change to 0 to stop the loop */
  HtmlElement *pNext;      /* Next element in the list */

  for(p=p->pNext; go && p; p = pNext){
    pNext = p->pNext;
    switch( p->base.type ){
      case Html_Text:
        x1 += p->text.w;
        x2 += p->text.w;
        if( p->base.style.flags & STY_Preformatted ){
          SETMAX( min, x1 );
          SETMAX( max, x1 );
        }else{
          SETMAX( min, x2 );
          SETMAX( max, x1 );
        }
        break;
      case Html_Space:
        if( p->base.style.flags & STY_Preformatted ){
          if( p->base.flags & HTML_NewLine ){
            x1 = x2 = indent;
          }else{
            x1 += p->space.w * p->base.count;
            x2 += p->space.w * p->base.count;
          }
        }else if( p->base.style.flags & STY_NoBreak ){
          if( x1>indent ){ x1 += p->space.w; TestPoint(0);}
          if( x2>indent ){ x2 += p->space.w; TestPoint(0);}
        }else{
          if( x1>indent ){ x1 += p->space.w; TestPoint(0);}
          x2 = indent;
        }
        break;
      case Html_IMG:
        switch( p->image.align ){
          case IMAGE_ALIGN_Left:
          case IMAGE_ALIGN_Right:
            obstacle += p->image.w;
            x1 = obstacle + indent;
            x2 = indent;
            SETMAX( min, x2 );
            SETMAX( min, p->image.w );
            SETMAX( max, x1 );
            break;
          default:
            x1 += p->image.w;
            x2 += p->image.w;
            if( p->base.style.flags & STY_Preformatted ){
              SETMAX( min, x1 );
              SETMAX( max, x1 );
            }else{
              SETMAX( min, x2 );
              SETMAX( max, x1 );
            }
            break;
        }
        break;
      case Html_TABLE:
        /* pNext = TableDimensions(htmlPtr, p, lineWidth-indent); */
        pNext = TableDimensions(htmlPtr, p, 0);
        x1 = p->table.maxW[0] + indent + obstacle;
        x2 = p->table.minW[0] + indent;
        SETMAX( max, x1 );
        SETMAX( min, x2 );
        x1 = indent + obstacle;
        x2 = indent;
        if( pNext && pNext->base.type==Html_EndTABLE ){
          pNext = pNext->pNext;
        }
        break;
      case Html_UL:
      case Html_OL:
        indent += HTML_INDENT;
        x1 = indent + obstacle;
        x2 = indent;
        break;
      case Html_EndUL:
      case Html_EndOL:
        indent -= HTML_INDENT;
        if( indent < 0 ){ indent = 0; }
        x1 = indent + obstacle;
        x2 = indent;
        break;
      case Html_BLOCKQUOTE:
        indent += 2*HTML_INDENT;
        x1 = indent + obstacle;
        x2 = indent;
        break;
      case Html_EndBLOCKQUOTE:
        indent -= 2*HTML_INDENT;
        if( indent < 0 ){ indent = 0; }
        x1 = indent + obstacle;
        x2 = indent;
        break;
      case Html_APPLET:
      case Html_INPUT:
      case Html_SELECT:
      case Html_EMBED:
      case Html_TEXTAREA:
        x1 += p->input.w + p->input.padLeft;
        if( p->base.style.flags & STY_Preformatted ){
          SETMAX( min, x1 );
          SETMAX( max, x1 );
          x2 += p->input.w + p->input.padLeft;
        }else{
          SETMAX( min, indent + p->input.w );
          SETMAX( max, x1 );
          x2 = indent;
        }
        break;
      case Html_BR:
      case Html_P:
      case Html_EndP:
      case Html_DIV:
      case Html_EndDIV:
      case Html_H1:
      case Html_EndH1:
      case Html_H2:
      case Html_EndH2:
      case Html_H3:
      case Html_EndH3:
      case Html_H4:
      case Html_EndH4:
      case Html_H5:
      case Html_H6:
        x1 = indent + obstacle;
        x2 = indent;
        break;
      case Html_EndTD:
      case Html_EndTH:
      case Html_CAPTION:
      case Html_EndTABLE:
      case Html_TD:
      case Html_TR:
      case Html_TH:
      case Html_EndTR:
        go = 0;
        break;
      default:
        break;
    }
    if( !go ){ break; }
  }
  *pMin = min;
  *pMax = max;
  return p;
}

/* Vertical alignments:
*/
#define VAlign_Unknown    0
#define VAlign_Top        1
#define VAlign_Bottom     2
#define VAlign_Center     3
#define VAlign_Baseline   4

/*
** Return the vertical alignment specified by the given element.
*/
static int GetVerticalAlignment(HtmlElement *p, int dflt){
  char *z;
  int rc;
  if( p==0 ) return dflt;
  z = HtmlMarkupArg(p, "valign", 0);
  if( z==0 ){
    rc = dflt;
    TestPoint(0);
  }else if( stricmp(z,"top")==0 ){
    rc = VAlign_Top;
    TestPoint(0);
  }else if( stricmp(z,"bottom")==0 ){
    rc = VAlign_Bottom;
    TestPoint(0);
  }else if( stricmp(z,"center")==0 ){
    rc = VAlign_Center;
    TestPoint(0);
  }else if( stricmp(z,"baseline")==0 ){
    rc = VAlign_Baseline;
    TestPoint(0);
  }else{
    rc = dflt;
    TestPoint(0);
  }
  return rc;
}

/* Do all layout for a single table.  Return the </table> element or
** NULL if the table is unterminated.
*/
HtmlElement *HtmlTableLayout(
  HtmlLayoutContext *pLC,      /* The layout context */
  HtmlElement *pTable          /* The <table> element */
){
  HtmlElement *pEnd;      /* The </table> element */
  HtmlElement *p;         /* For looping thru elements of the table */
  HtmlElement *pNext;     /* Next element in the loop */
  HtmlElement *pCaption;  /* Start of the caption text.  The <caption> */
  HtmlElement *pEndCaption; /* End of the caption.  The </caption> */
  int width;              /* Width of the table as drawn */
  int cellSpacing;        /* Value of cellspacing= parameter to <table> */
  int cellPadding;        /* Value of cellpadding= parameter to <table> */
  int tbw;                /* Width of the 3D border around the whole table */
  int cbw;                /* Width of the 3D border around a cell */
  int pad;                /* cellPadding + borderwidth */
  char *z;                /* A string */
  int leftMargin;         /* The left edge of space available for drawing */
  int lineWidth;          /* Total horizontal space available for drawing */
  int separation;         /* Distance between content of columns (or rows) */
  int i;                  /* Loop counter */
  int n;                  /* Number of columns */
  int btm;                /* Bottom edge of previous row */
  int iRow;               /* Current row number */
  int iCol;               /* Current column number */
  int colspan;            /* Number of columns spanned by current cell */
  int vspace;             /* Value of the vspace= parameter to <table> */
  int hspace;             /* Value of the hspace= parameter to <table> */
  int rowBottom;          /* Bottom edge of content in the current row */
  int defaultVAlign;      /* Default vertical alignment for the current row */
  char *zAlign;           /* Value of the ALIGN= attribute of the <TABLE> */
#define N HTML_MAX_COLUMNS+1
  int y[N];               /* Top edge of each cell's content */
  int x[N];               /* Left edge of each cell's content */
  int w[N];               /* Width of each cell's content */
  int ymax[N];            /* Bottom edge of cell's content if valign=top */
  HtmlElement *apElem[N]; /* The <td> or <th> for each cell in a row */
  int firstRow[N];        /* First row on which a cell appears */
  int lastRow[N];         /* Row to which each cell span's */
  int valign[N];          /* Vertical alignment for each cell */
  HtmlLayoutContext savedContext;  /* Saved copy of the original pLC */
  HtmlLayoutContext cellContext;   /* Used to render a single cell */
#ifdef TABLE_TRIM_BLANK
	extern int HtmlLineWasBlank;
#endif /* TABLE_TRIM_BLANK */

  if( pTable==0 || pTable->base.type!=Html_TABLE ){
    TestPoint(0);
    return pTable;
  }
  TRACE_PUSH(HtmlTrace_Table2);
  TRACE(HtmlTrace_Table2, ("Starting TableLayout() at %s\n",
                          HtmlTokenName(pTable)));

  /* Figure how much horizontal space is available for rendering
  ** this table.  Store the answer in lineWidth.  leftMargin is
  ** the left-most X coordinate of the table.  btm stores the top-most
  ** Y coordinate.
  */
  HtmlComputeMargins(pLC, &leftMargin, &btm, &lineWidth);
  TRACE(HtmlTrace_Table2, ("...btm=%d left=%d width=%d\n",
                           btm, leftMargin, lineWidth));

  /* figure out how much space the table wants for each column,
  ** and in total.. */
  pEnd = TableDimensions(pLC->htmlPtr, pTable, lineWidth);

  /* If we don't have enough horizontal space to accomodate the minimum table
  ** width, then try to move down past some obstruction (such as an
  ** <IMG ALIGN=LEFT>) to give us more room.
  */
  if( lineWidth < pTable->table.minW[0] ){
    HtmlWidenLine(pLC, pTable->table.minW[0], &leftMargin, &btm, &lineWidth);
    TRACE(HtmlTrace_Table2, ("Widen to btm=%d left=%d width=%d\n",
                             btm, leftMargin, lineWidth));
  }
  savedContext = *pLC;

  /* Figure out how wide to draw the table
  */
  if( lineWidth < pTable->table.minW[0] ){
    width = pTable->table.minW[0];
  }else if( lineWidth <= pTable->table.maxW[0] ){
    width = lineWidth;
  }else{
    width = pTable->table.maxW[0];
  }


  /* Compute the width and left edge position of every column in
  ** the table */
  z = HtmlMarkupArg(pTable, "cellpadding", 0);
  cellPadding = z ? atoi(z) : DFLT_CELLPADDING;
  cellSpacing = CellSpacing(pLC->htmlPtr, pTable);
  z = HtmlMarkupArg(pTable, "vspace", 0);
  vspace = z ? atoi(z) : DFLT_VSPACE;
  z = HtmlMarkupArg(pTable, "hspace", 0);
  hspace = z ? atoi(z) : DFLT_HSPACE;
#ifdef DEBUG
  if( HtmlTraceMask & HtmlTrace_Table4 ){
    cellPadding = 5;
    cellSpacing = 2;
    if( vspace<2 ) vspace = 2;
    if( hspace<2 ) hspace = 2;
  }
#endif
  tbw = pTable->table.borderWidth;
  cbw = (tbw>0);
  pad = cellPadding + cbw;
  separation = cellSpacing + 2*pad;
  x[1] = leftMargin + tbw + cellSpacing + pad;
  n = pTable->table.nCol;
  if( n<=0 || pTable->table.maxW[0]<=0 ){
    /* Abort if the table has no columns at all or if the total width
    ** of the table is zero or less. */
    return pEnd;
  }
  zAlign = HtmlMarkupArg(pTable, "align", "");
  if( width < lineWidth ){
    int align = pTable->base.style.align;
    if( align==ALIGN_Right || stricmp(zAlign,"right")==0 ){
      x[1] += lineWidth - width;
    }else if( align==ALIGN_Center && stricmp(zAlign,"left")!=0 ){
      x[1] += (lineWidth - width)/2;
    }
  }
  if( width==pTable->table.maxW[0] ){
    w[1] = pTable->table.maxW[1];
    for(i=2; i<=n; i++){
      w[i] = pTable->table.maxW[i];
      x[i] = x[i-1] + w[i-1] + separation;
      TestPoint(0);
    }
  }else if( width > pTable->table.maxW[0] ){
    int *tmaxW = pTable->table.maxW;
    double scale = ((double)width)/ (double)tmaxW[0];
    w[1] = tmaxW[1] * scale;
    for(i=2; i<=n; i++){
      w[i] = tmaxW[i] * scale;
      x[i] = x[i-1] + w[i-1] + separation;
      TestPoint(0);
    }
  }else if( width > pTable->table.minW[0] ){
    float scale;
    int *tminW = pTable->table.minW;
    int *tmaxW = pTable->table.maxW;
    scale = (double)(width - tminW[0]) / (double)(tmaxW[0] - tminW[0]);
    w[1] = tminW[1] + (tmaxW[1] - tminW[1]) * scale;
    for(i=2; i<=n; i++){
      w[i] = tminW[i] + (tmaxW[i] - tminW[i]) * scale;
      x[i] = x[i-1] + w[i-1] + separation;
      TestPoint(0);
    }
  }else{
    w[1] = pTable->table.minW[1];
    for(i=2; i<=n; i++){
      w[i] = pTable->table.minW[i];
      x[i] = x[i-1] + w[i-1] + separation;
      TestPoint(0);
    }
  }
  w[n] = width - ((x[n] - x[1]) + 2*(tbw + pad + cellSpacing));

  /* Add notation to the pTable structure so that we will know where
  ** to draw the outer box around the outside of the table.
  */
  btm += vspace;
  pTable->table.y = btm;
  pTable->table.x = x[1] - (tbw + cellSpacing + pad);
  pTable->table.w = width;
  SETMAX(pLC->maxX, pTable->table.x + pTable->table.w);
  btm += tbw + cellSpacing;

  /* Begin rendering rows of the table */
  for(i=1; i<=n; i++){
    firstRow[i] = 0;
    lastRow[i] = 0;
    apElem[i] = 0;
  }
  p = pTable->pNext;
  rowBottom = btm;
  for(iRow=1; iRow<=pTable->table.nRow; iRow++){
    TRACE(HtmlTrace_Table2, ("Row %d: btm=%d\n",iRow,btm));
    /* Find the start of the next row.  Keep an eye out for the caption
    ** while we search */
    while( p && p->base.type!=Html_TR ){
      if( p->base.type==Html_CAPTION ){
        pCaption = p;
        while( p && p!=pEnd && p->base.type!=Html_EndCAPTION ){
          p = p->pNext;
        }
        pEndCaption = p;
      }
      TRACE(HtmlTrace_Table3, ("Skipping token %s\n", HtmlTokenName(p)));
      p = p->pNext;
    }
    if( p==0 ){ TestPoint(0); break; }

    /* Record default vertical alignment flag for this row */
    defaultVAlign = GetVerticalAlignment(p, VAlign_Center);

    /* Find every new cell on this row */
    for(iCol=1; iCol<=pTable->table.nCol && iCol<HTML_MAX_COLUMNS; iCol++){
      if( lastRow[iCol]<iRow ) ymax[iCol] = 0;
    }
    iCol = 0;
    for(p=p->pNext; p && p->base.type!=Html_TR && p!=pEnd; p=pNext){
      pNext = p->pNext;
      TRACE(HtmlTrace_Table3, ("Processing token %s\n", HtmlTokenName(p)));
      switch( p->base.type ){
        case Html_TD:
        case Html_TH:
          /* Find the column number for this cell.  Be careful to skip
          ** columns which extend down to this row from prior rows */
          do{
            iCol++;
          }while( iCol <= HTML_MAX_COLUMNS && lastRow[iCol] >= iRow );
          TRACE(HtmlTrace_Table2,
            ("Column %d: x=%d w=%d\n",iCol,x[iCol],w[iCol]));
          /* Process the new cell.  (Cells beyond the maximum number of
          ** cells are simply ignored.) */
          if( iCol <= HTML_MAX_COLUMNS ){
            apElem[iCol] = p;
            pNext = p->cell.pEnd;
            if( p->cell.rowspan==0 ){
              lastRow[iCol] = pTable->table.nRow;
            }else{
              lastRow[iCol] = iRow + p->cell.rowspan - 1;
            }
            firstRow[iCol] = iRow;

            /* Set vertical alignment flag for this cell */
            valign[iCol] = GetVerticalAlignment(p, defaultVAlign);

            /* Render cell contents and record the height */
            y[iCol] = btm + pad;
            cellContext.htmlPtr = pLC->htmlPtr;
            cellContext.pStart = p->pNext;
            cellContext.pEnd = pNext;
            cellContext.headRoom = 0;
            cellContext.top = y[iCol];
            cellContext.bottom = y[iCol];
            cellContext.left = x[iCol];
            cellContext.right = 0;
            cellContext.pageWidth = x[iCol]+w[iCol];
            colspan = p->cell.colspan;
            if( colspan==0 ){
              for(i=iCol+1; i<=pTable->table.nCol; i++){
                cellContext.pageWidth += w[i] + separation;
                lastRow[i] = lastRow[iCol];
              }
            }else if( colspan>1 ){
              for(i=iCol+1; i<iCol+colspan; i++){
                cellContext.pageWidth += w[i] + separation;
                lastRow[i] = lastRow[iCol];
              }
            }
            cellContext.maxX = 0;
            cellContext.maxY = 0;
            cellContext.leftMargin = 0;
            cellContext.rightMargin = 0;
            HtmlLock(cellContext.htmlPtr);
            HtmlLayoutBlock(&cellContext);
            if( HtmlUnlock(cellContext.htmlPtr) ) return 0;
#ifdef TABLE_TRIM_BLANK
			/*
			 * Cancel any trailing vertical whitespace caused
			 * by break markup
			 */
			if (HtmlLineWasBlank)
				cellContext.maxY -= cellContext.headRoom;
#endif /* TABLE_TRIM_BLANK */
            ymax[iCol] = cellContext.maxY;
            SETMAX(ymax[iCol], y[iCol]);
            HtmlClearMarginStack(&cellContext.leftMargin);
            HtmlClearMarginStack(&cellContext.rightMargin);

            /* Set coordinates of the cell border */
            p->cell.x = x[iCol] - pad;
            p->cell.y = btm;
            p->cell.w = cellContext.pageWidth + 2*pad - x[iCol];
            TRACE(HtmlTrace_Table2,
              ("Column %d top=%d bottom=%d h=%d left=%d w=%d\n",
              iCol, y[iCol], ymax[iCol], ymax[iCol]-y[iCol],
              p->cell.x, p->cell.w));

            /* Advance the column counter for cells spaning multiple columns */
            if( colspan > 1 ){
              iCol += colspan - 1;
            }else if( colspan==0 ){
              iCol = HTML_MAX_COLUMNS + 1;
            }
          }
          break;

        case Html_CAPTION:
          /* Gotta remember where the caption is so we can render it
          ** at the end */
          pCaption = p;
          while( pNext && pNext!=pEnd && pNext->base.type!=Html_EndCAPTION ){
            pNext = pNext->pNext;
          }
          pEndCaption = pNext;
          break;
      }
    }

    /* Figure out how high to make this row. */
    for(iCol=1; iCol<=pTable->table.nCol; iCol++){
      if( lastRow[iCol] == iRow || iRow==pTable->table.nRow ){
        SETMAX( rowBottom, ymax[iCol] );
      }
    }
    TRACE(HtmlTrace_Table2, ("Total row height: %d..%d -> %d\n",
                             btm,rowBottom,rowBottom-btm));

    /* Position every cell whose bottom edge ends on this row */
    for(iCol=1; iCol<=pTable->table.nCol; iCol++){
      int dy;    /* Extra space at top of cell used for vertical alignment */

      /* Skip any unused cells or cells that extend down thru
      ** subsequent rows */
      if( apElem[iCol]==0
      || (iRow!=pTable->table.nRow && lastRow[iCol]>iRow) ){  continue; }

      /* Align the contents of the cell vertically. */
      switch( valign[iCol] ){
        case VAlign_Unknown:
        case VAlign_Center:
          dy = (rowBottom - ymax[iCol])/2;
          break;
        case VAlign_Top:
        case VAlign_Baseline:
          dy = 0;
          break;
        case VAlign_Bottom:
          dy = rowBottom - ymax[iCol];
          break;
      }
      if( dy ){
        HtmlElement *pLast = apElem[iCol]->cell.pEnd;
        TRACE(HtmlTrace_Table3, ("Delta column %d by %d\n",iCol,dy));
        HtmlMoveVertically(apElem[iCol]->pNext, pLast, dy);
      }

      /* Record the height of the cell so that the border can be drawn */
      apElem[iCol]->cell.h = rowBottom + pad - apElem[iCol]->cell.y;
      apElem[iCol] = 0;
    }

    /* Update btm to the height of the row we just finished setting */
    btm = rowBottom + pad + cellSpacing;
  }

  btm += tbw;
  pTable->table.h = btm - pTable->table.y;
  SETMAX( pLC->maxY, btm );
  pLC->bottom = btm + vspace;

  /* Render the caption, if there is one */
  if( pCaption ){
  }

  /* Whenever we do any table layout, we need to recompute all the
  ** HtmlBlocks.  The following statement forces this. */
  pLC->htmlPtr->firstBlock = pLC->htmlPtr->lastBlock = 0;

  /* Adjust the context for text that wraps around the table, if
  ** requested by an ALIGN=RIGHT or ALIGN=LEFT attribute.
  */
  if( stricmp(zAlign,"left")==0 ){
    savedContext.maxX = pLC->maxX;
    savedContext.maxY = pLC->maxY;
    *pLC = savedContext;
    HtmlPushMargin(&pLC->leftMargin, pTable->table.w + 2,
        pTable->table.y + pTable->table.h + 2, 0);
  }else if( stricmp(zAlign,"right")==0 ){
    savedContext.maxX = pLC->maxX;
    savedContext.maxY = pLC->maxY;
    *pLC = savedContext;
    HtmlPushMargin(&pLC->rightMargin, pTable->table.w + 2,
        pTable->table.y + pTable->table.h + 2, 0);
  }

  /* All done */
  TRACE(HtmlTrace_Table2, (
     "Done with TableLayout().  x=%d y=%d w=%d h=%d Return %s\n",
     pTable->table.x, pTable->table.y, pTable->table.w, pTable->table.h,
     HtmlTokenName(pEnd)));
  TRACE_POP(HtmlTrace_Table2);
  return pEnd;
}


/*
** Move all elements in the given list vertically by the amount dy
*/
void HtmlMoveVertically(
  HtmlElement *p,         /* First element to move */
  HtmlElement *pLast,     /* Last element.  Do move this one */
  int dy                  /* Amount by which to move */
){
  if( dy==0 ){ TestPoint(0); return; }
  while( p && p!=pLast ){
    switch( p->base.type ){
      case Html_A:
        p->anchor.y += dy;
        break;
      case Html_Text:
        p->text.y += dy;
        break;
      case Html_LI:
        p->li.y += dy;
        break;
      case Html_TD:
      case Html_TH:
        p->cell.y += dy;
        break;
      case Html_TABLE:
        p->table.y += dy;
        break;
      case Html_IMG:
        p->image.y += dy;
        break;
      case Html_INPUT:
      case Html_SELECT:
      case Html_APPLET:
      case Html_EMBED:
      case Html_TEXTAREA:
        p->input.y += dy;
        break;
      default:
        break;
    }
    p = p->pNext;
  }
}
