/*
** The program coded by this file reads a single SGML file and
** splits it up into many smaller HTML files.  Each of the
** smaller HTML files contains a single heading of the
** original SGML source file.
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#define stricmp strcasecmp

/******************* Begin SGML parser code *******************/
/*
** The following code implements a system for parsing SGML.
**
*/
/* These three pointers define certain special handlers.  All whitespace
** is sent to xSpaceHandler.  Non-whitespace is given to xWordHandler.
** Any markup that isn't specifically directed elsewhere is given
** to xDefaultMarkupHandlers.
*/
static void (*xSpaceHandler)(const char*,void*);
static void (*xWordHandler)(const char*,void*);
static void (*xDefaultMarkupHandler)(int, const char**, void*);

/* Each handler is stored in a hash table as an instance of the
** following structure.
*/
typedef struct sgHandler Handler;
struct sgHandler {
  char *zName;                                /* Name of markup to handle */
  void (*xHandler)(int, const char**, void*); /* Routine to do the work */
  Handler *pCollide;                          /* Next handler with same hash */
};

/* The size of the handler hash table. 
** For best results, this should be a prime number which is larger than
** the number of markups in the hash table.
*/
#define SGML_HASH_SIZE 203

/* The handler hash table */
static Handler *apHandler[SGML_HASH_SIZE];

/* Hash a handler name */
static int SgmlHash(const char *zName){
  int h = 0;
  char c;
  while( (c=*zName)!=0 ){
    if( isupper(c) ) c = tolower(c);
    h = h<<5 ^ h ^ c;
    zName++;
  }
  if( h<0 ) h = -h;
  return h % SGML_HASH_SIZE;
}

/* Given a pointer to an input file, read and parse that file
** as if it were SGML.
**
** This is not a true SGML parser because it handles some unusual
** cases differently, and ignores the & operator completely.
*/
void SgmlParse(FILE *in, void *pArg){
  int c;
  int i, j;
  int argc;
  Handler *pHandler;
  char *argv[100];
  char zBuf[1000];

  c = getc(in);
  while( c!=EOF ){
    if( isspace(c) ){
      /* Case 1: spaces */
      zBuf[0] = c;
      i = 1;
      while( i<sizeof(zBuf)-2 && (c=getc(in))!=EOF && isspace(c) ){
        zBuf[i++] = c;
      }
      zBuf[i] = 0;
      /* Dispose of space */
      if( xSpaceHandler ){
        (*xSpaceHandler)(zBuf,pArg);
      }
    }else if( c=='<' ){
      int cQuote = 0;
      i = 0;
      while( (c=getc(in))!=EOF && (cQuote || c!='>') ){
        if( i<sizeof(zBuf)-2 ) zBuf[i++] = c;
        if( cQuote ){
          if( cQuote==c ) cQuote = 0;
        }else if( c=='"' || c=='\'' ){
          cQuote = c;
        }
      }
      if( c=='>' ) c = getc(in);
      zBuf[i] = 0;
      argc = 0;
      argv[0] = &zBuf[0];
      for(j=0; zBuf[j] && !isspace(zBuf[j]); j++){}
      if( zBuf[j] ){
        zBuf[j++] = 0;
        while( argc<(sizeof(argv)/sizeof(argv[0])) - 4 && zBuf[j] ){
          while( isspace(zBuf[j]) ) j++;
          argv[++argc] = &zBuf[j];
          while( zBuf[j] && !isspace(zBuf[j]) && zBuf[j]!='=' ) j++;
          if( zBuf[j]!='=' ){
            argv[argc+1] = argv[argc];
            argc++;
            if( zBuf[j] ) zBuf[j++] = 0;
            continue;
          }
          zBuf[j++] = 0;
          if( zBuf[j]=='"' || zBuf[j]=='\'' ){
            cQuote = zBuf[j++];
          }else{
            cQuote = 0;
          }
          argv[++argc] = &zBuf[j];
          if( cQuote ){
            while( zBuf[j] && zBuf[j]!=cQuote ) j++;
          }else{
            while( zBuf[j] && !isspace(zBuf[j]) ) j++;
          }
          if( zBuf[j] ) zBuf[j++] = 0;
        }
      }
      argv[++argc] = 0;
      /* Despose of a markup */
      pHandler = apHandler[SgmlHash(argv[0])];
      while( pHandler && stricmp(pHandler->zName,argv[0])!=0 ){
        pHandler = pHandler->pCollide;
      }
      if( pHandler ){
        if( pHandler->xHandler ){
          (*pHandler->xHandler)(argc,(const char**)argv,pArg);
        }
      }else if( xDefaultMarkupHandler ){
        (*xDefaultMarkupHandler)(argc,(const char**)argv,pArg);
      }
    }else{
      zBuf[0] = c;
      i = 1;
      while( i<sizeof(zBuf)-2 && (c=getc(in))!=EOF && c!='<' && !isspace(c) ){
        zBuf[i++] = c;
      }
      zBuf[i] = 0;
      /* Dispose of a word */
      if( xWordHandler ){
        (*xWordHandler)(zBuf,pArg);
      }
    }
  }
}

/*
** Clear out the handler hash table
*/
void SgmlHandlerReset(void){
  Handler *pHandler;
  int i;

  for(i=0; i<SGML_HASH_SIZE; i++){
    Handler *pNext;
    for(pHandler=apHandler[i]; pHandler; pHandler=pNext){
      pNext = pHandler->pCollide;
      free(pHandler);
    }
    apHandler[i] = 0;
  }
}

/* Install a new handler
*/
void SgmlHandler(const char *zName, void (*xFunc)(int,const char**,void*)){
  int h = SgmlHash(zName);
  extern void *malloc();
  Handler *pNew = malloc( sizeof(Handler) + strlen(zName) + 1 );
  if( pNew==0 ) return;
  pNew->zName = (char*)&pNew[1];
  strcpy(pNew->zName,zName);
  pNew->pCollide = apHandler[h];
  pNew->xHandler = xFunc;
  apHandler[h] = pNew;  
}

/* Install the default handlers
*/
void SgmlWordHandler(void (*xWord)(const char*,void*)){
  xWordHandler = xWord;
}
void SgmlSpaceHandler(void (*xSpace)(const char*,void*)){
  xSpaceHandler = xSpace;
}
void SgmlDefaultMarkupHandler(void (*xMarkup)(int,const char**,void*)){
  xDefaultMarkupHandler = xMarkup;
}
/****************** End of SGML Parser Code **********************/

/* Information about each heading in the original HTML file
** is stored in an instance of the following structure.  Each
** such structure is kept on a linked list in the same order
** as the headings appear in the HTML file.
*/
typedef struct sgHeading Heading;
struct sgHeading {
  int level;                /* What level. <h1>==0, <h2>==1,... */
  int page;                 /* Page number for this header */
  char *zTag;               /* A tag for this heading.  May be NULL */
  Heading *pCollide;        /* Next heading with same hash on zTag */
  char *zValue;             /* The value.  ex: 1.2.7 */
  char *zFile;              /* The file into which this is written */
  char *zTitle;             /* The complete title text (w/o the value) */
  char *zURL;               /* A URL for this heading used in hyperlinks */
  Heading *pNext;           /* The next heading in sequence */
  Heading *pPrev;           /* The previous heading in sequence */
};

/* Each index entry is stored as an instance of the following
** structure.
*/
typedef struct sgIndexEntry IndexEntry;
struct sgIndexEntry {
  char *zText;         /* The text of this index entry */
  char *zLabel;        /* A label to mark this index entry */
  IndexEntry *pNext;   /* The next entry in the index */
};

/* All information gathered about the input HTML file, and all state
** information while parsing, is stored in the following global
** variables.
*/
static int HeadingIndex[6];           /* Indices for the most recent heading */
static int FigureIndex;               /* The figure index */
static char zBuffer[1000];            /* Collect title and heading text here */
static int nBuffer;                   /* Index of next free slot of zBuffer[] */
static char *zBasename;               /* Base name of the input file */
static char *zTitle = "Untitled";     /* Title of the document */
static Heading *pFirstHeading = 0;    /* First heading */
static Heading *pLastHeading = 0;     /* Last heading */
static Heading *pFirstFigure = 0;     /* First figure */
static Heading *pLastFigure = 0;      /* Last figure */
static FILE *pOut;                    /* All output goes to this file */
#define HASH_SIZE 1009                /* Number of hash table slots */
static Heading *apTagHash[HASH_SIZE]; /* The hash table */
static Heading *pCurrentHeading = 0;  /* The current heading */
static Heading *pCurrentFigure = 0;   /* Last figure seen in pass 2 */
static IndexEntry *pIndexList;        /* List of all index entries */
static int IndexSeen = 0;             /* True if there are any index entires */
static Heading *pIndexHeading;        /* A Heading structure for the index */
static int nPage = 1;                 /* Number of headers */
static int hasBody = 0;               /* True if the input has a <body> */
static int oneFile = 0;        /* True to write output into a single file */
static int noTitle = 0;        /* True to suppress the <h1>title</h1> at the
                               ** beginning of every page */
static int detailedScope = 0;  /* True to cause extra detail about the position
                               ** of the current page relative to the whole
                               ** document to be printed at the top and bottom
                               ** of each page.  Only used if oneFile==0 */
static int noScope = 0;        /* True to prevent any scope information.
                               ** This overrides detailedScope */

static int graphicalButton = 0; /* True to use GIFs for movement buttons in
                                ** noScope mode */

/* Duplicate a string.
*/
static char *StrDup(const char *zSrc){
  char *zDest;
  extern void *malloc();

  zDest = malloc( strlen(zSrc) + 1 );
  if( zDest==0 ){
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }
  strcpy(zDest,zSrc);
  return zDest;
}

/* Construct a new filename based on the base name of the input file.
** The basename is stored in the global variable zBasename.
*/
static char *MakeNewFilename(void){
  char zName[100];
  if( oneFile ){
    sprintf(zName,"%.*s.html",(int)sizeof(zName)-20,zBasename);
  }else{
    static int nFile = 0;
    sprintf(zName,"%.*s%04d.html",(int)sizeof(zName)-20,zBasename,nFile);
    nFile++;
  }
  return StrDup(zName);
}

/* Make a new heading at the level given.  Add it to the heading
** list.  Initialize most fields to be unused.
*/
static void MakeEmptyHeading(int level){
  Heading *pNew;            /* The new heading */
  int i;                    /* Loop counter */
  char zLabel[400];         /* For constructing labels */
  extern void *malloc();

  HeadingIndex[level]++;
  for(i=level+1; i<6; i++) HeadingIndex[i] = 0;
  if( level==0 ) FigureIndex = 0;
  sprintf(zLabel,"%d",HeadingIndex[0]);
  for(i=1; i<=level; i++){
    sprintf(&zLabel[strlen(zLabel)],".%d",HeadingIndex[i]);
  }
  pNew = malloc( sizeof(Heading) );
  if( pNew==0 ){
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }
  pNew->level = level;
  pNew->page = ++nPage;
  pNew->zTag = 0;
  pNew->zValue = StrDup(zLabel);
  pNew->zFile = MakeNewFilename();
  pNew->zTitle = 0;
  if( oneFile ){
    sprintf(zLabel,"%.*s#H%04d",(int)sizeof(zLabel)-40,pNew->zFile,pNew->page);
  }else{
    sprintf(zLabel,"%.*s",(int)sizeof(zLabel)-40,pNew->zFile);
  }
  pNew->zURL = StrDup(zLabel);
  pNew->pNext = 0;
  if( pLastHeading ) pLastHeading->pNext = pNew;
  else               pFirstHeading = pNew;
  pNew->pPrev = pLastHeading;
  pLastHeading = pNew;
}

/* Hash a tag for the purpose of adding a Heading record to the
** apTagHash[] hash table.
*/
static int hash(const char *zName){
  int h = 0;
  while( *zName ){
    h = h<<4 ^ h ^ *zName++;
  }
  if( h<0 ) h = -h;
  return h % HASH_SIZE;
}

/* Processing all <hN> marks on the first pass.
**
** There is no output during the first pass.  The only thing that
** happens is the construction of data structures.
*/
static void Pass1Heading(int argc, const char **argv, void *NotUsed){
  int level;
  int i;

  if( argv[0][1]<'1' || argv[0][1]>'6' ) return;
  level = argv[0][1] - '1';
  MakeEmptyHeading(level);
  for(i=1; argv[i]; i+=2){
    if( stricmp(argv[i],"tag")==0 ){
      int h = hash(argv[i+1]);
      pLastHeading->zTag = StrDup(argv[i+1]);
      pLastHeading->pCollide = apTagHash[h];
      apTagHash[h] = pLastHeading;
    }
  }
  nBuffer = 0;
}

/* Process the <title> directive during pass 1.
*/
static void Pass1Title(int argc, const char **argv, void *NotUsed){
  nBuffer = 0;
}
/* Process theh </title> directive during pass 1.
*/
static void Pass1EndTitle(int argc, const char **argv, void *NotUsed){
  zBuffer[nBuffer] = 0;
  zTitle = StrDup(zBuffer);
}

/* This routine handles all space and word text on the first pass.
** Its job is to put information into zBuffer[].
*/
static void Pass1Word(const char *zText, void *NotUsed){
  int len;
  len = strlen(zText);
  if( nBuffer+len < sizeof(zBuffer)-2 ){
    strcpy(&zBuffer[nBuffer],zText);
    nBuffer += len;
  }
}
static void Pass1Space(const char *zNotUsed, void *NotUsed){
  if( nBuffer<sizeof(zBuffer)-2 ){
    zBuffer[nBuffer++] = ' ';
  }
}

/* Handle the end-of-header marks during Pass1
*/
static void Pass1EndHeading(int argc, const char **argv, void *NotUsed){
  if( pLastHeading==0 ) return;
  zBuffer[nBuffer] = 0;
  pLastHeading->zTitle = StrDup(zBuffer);
}

/* During pass 1, just indicate whether or not there is an index
** file.
*/
static void Pass1Index(int argc, const char **argv, void *NotUsed){
  IndexSeen = 1;
}

/* Process the <body> command during pass 1.  This just sets
** the "hasBody" flag
*/
static void Pass1Body(int argc, const char **argv, void *NotUsed){
  hasBody = 1;
}

/* Process the <image> command on pass 1.
**
** If the <image> lacks a caption= field, then ignore it on this pass.
** The caption= fields means that we are dealing with a numbered image.
** Allocate a new image number and put it into a Header structure.  This
** header structure will be used to store the image number only -- it
** will not be on the header list.
*/
static void Pass1Image(int argc, const char **argv, void *NotUsed){
  const char *zCaption = 0;
  const char *zTag = 0;
  int i;
  Heading *pNew;
  char zLabel[400];

  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"caption")==0 ){
      zCaption = argv[i+1];
    }else if( stricmp(argv[i],"tag")==0 ){
      zTag = argv[i+1];
    }
  }
  if( zCaption==0 ) return;
  FigureIndex++;
  sprintf(zLabel,"%d.%d",HeadingIndex[0],FigureIndex);
  pNew = malloc( sizeof(Heading) );
  if( pNew==0 ){
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }
  pNew->level = 0;
  pNew->page = nPage;
  pNew->zTag = 0;
  pNew->zValue = StrDup(zLabel);
  if( pLastHeading ){
    pNew->zFile = pLastHeading->zFile;
  }else if( oneFile ){
    sprintf(zLabel,"%.*s.html",(int)sizeof(zLabel)-40,zBasename);
    pNew->zFile = StrDup(zLabel);
  }else{
    sprintf(zLabel,"%.*s0000.html",(int)sizeof(zLabel)-40,zBasename);
    pNew->zFile = StrDup(zLabel);
  }
  pNew->zTitle = StrDup(zCaption);
  sprintf(zLabel,"%.*s#F%dx%d",(int)sizeof(zLabel)-100,pNew->zFile,
    HeadingIndex[0],FigureIndex);
  pNew->zURL = StrDup(zLabel);
  pNew->pNext = 0;
  if( pLastFigure ) pLastFigure->pNext = pNew;
  else              pFirstFigure = pNew;
  pNew->pPrev = pLastFigure;
  pLastFigure = pNew;
  if( zTag ){
    int h = hash(zTag);
    pNew->zTag = StrDup(zTag);
    pNew->pCollide = apTagHash[h];
    apTagHash[h] = pNew;
  }
}

/* This function will output one or more <ul>s or </ul>s in order
** to make ul_depth equal to its argument.
*/
static void SetUlDepth(int new_depth){
  /* The following variable keeps track of how deeply nested into a
  ** <ul>..</ul> we are currently.  0 means we are not within any
  ** <ul>..</ul>.  1 means we are within 1 <ul>..</ul>.  And so forth.
  */
  static int ul_depth = 0;
  while( ul_depth < new_depth ){
    fprintf(pOut,"<ul>");
    ul_depth++;
  }
  while( ul_depth > new_depth ){
    fprintf(pOut,"</ul>");
    ul_depth--;
  }
}

/* Write a link to the given heading.
*/
static void WriteLinkTo(const Heading *pHeading){
  if( pOut==0 ) return;
  SetUlDepth(pHeading->level+1);
  if( pHeading->zTitle ){
    fprintf(pOut,"\n<li><a href=%s>%s %s</a>\n",
      pHeading->zURL,
      pHeading->zValue,
      pHeading->zTitle
    );
  }else{
    fprintf(pOut,"\n<li>%s<br>\n",
      pHeading->zValue
    );
  }
}

/* Write links to all parents of the heading "pHeading", then write
** a link to itself.
*/
static void WriteLinkToParentAndSelf(const Heading *pHeading){
  Heading *pParent;
  int targetLevel;

  if( pHeading==0 ) return;
  pParent = pHeading->pPrev;
  targetLevel = pHeading->level;
  if( !detailedScope ) targetLevel--;
  while( pParent && pParent->level>targetLevel ){
    pParent = pParent->pPrev;
  }
  if( pParent ) WriteLinkToParentAndSelf(pParent);
  WriteLinkTo(pHeading);
}

/* Write a comment which should appear at the beginning of every file
** warning the reader not to edit that file.
*/
static void WriteDoNotEditComment(void){
  if( pOut==0 ) return;
  fprintf(pOut,"<!-- DO NOT EDIT!\n");
  fprintf(pOut,"    Automatically generated code\n");
  fprintf(pOut,"    To make changes, edit the source file: %s.doc -->\n",
    zBasename);
}

/* Write the document title.  This is called at the beginning of
** every output file, except for 0000.html.
*/
static void WriteHtmlTitle(int currentPage){
  if( pOut==0 || zTitle==0 ) return;
  fprintf(pOut,"<title>%s (page %d of %d)</title>\n",
    zTitle,currentPage,nPage);
  if( !hasBody && !noTitle ){
    fprintf(pOut,"<h1>%s (page %d of %d)</h1>",
      zTitle,currentPage,nPage);
  }
}

/* Write a link back to the table of contents.  This will only be
** called if oneFile==FALSE;
*/
static void WriteLinkToTableOfContents(void){
  if( pOut==0 ) return;
  assert( oneFile==0 );
  SetUlDepth(1);
  fprintf(pOut,
    "<li><a href=%s0000.html>Table Of Contents</a>\n",
    zBasename);
}

/* Write a link back to the index
*/
static void WriteLinkToIndex(void){
  if( pOut==0 || pIndexHeading==0 ) return;
  SetUlDepth(1);
  fprintf(pOut,"\n<li><a href=%s>Index</a>\n",
    pIndexHeading->zURL
  );
}

/* Write a button that creates a link to the given page.  Make
** the text of the button as indicated.  The button is NOT written
** if the page doesn't exist.
**
** This code is called only in "noScope" mode with oneFile==0.
*/
static void WriteButton(
  const char *zGif,         /* Name of a GIF for this button */
  const char *zPageName,    /* Text name of this button */
  Heading *pHeading         /* The heading to jump to */
){
  assert( noScope && !oneFile );
  if( pOut==0 || pHeading==0 ) return;
  if( graphicalButton && zGif ){
    fprintf(pOut,"<a href=%s><img src=%s alt=\"[%s]\"></a>\n",
      pHeading->zURL,zGif,zPageName);
  }else{
    fprintf(pOut,"[<a href=%s>%s</a>]",pHeading->zURL,zPageName);
  }
  
}

/* This procedure writes a special button (similar in appearance to
** a "WriteButton()" button) that targets the table of contents.
**
** This code is called only in noScope mode with oneFile==0.
*/
static void WriteTOCButton(void){
  assert( noScope && !oneFile );
  if( pOut==0 ) return;
  if( graphicalButton ){
    fprintf(pOut,
      "<a href=%s0000.html#toc%04d>\n"
      "<img src=contents.gif alt=\"[Table Of Contents]\">\n" 
      "</a>",
      zBasename,pCurrentHeading->page);
  }else{
    fprintf(pOut,"[<a href=%s0000.html#toc%04d>Table of Contents</a>]",
      zBasename,pCurrentHeading->page);
  }
}

static void CopyToOutput(const char *zText, void *NotUsed){
  if( pOut ) fprintf(pOut,"%s",zText);
}

/* Processing all <hN> marks on the second pass.  This is where
** the output occurs.
**
**  1.  In the current file, write links to all children of the
**      previous heading, and a link to the first heading after
**      the last decendent of this heading.
**
**  2.  Close the current file.  Open a new file.
**
**  3.  Into the new file, write links to :
**        A.  all parents
**        B.  the previous sibling
**
**  4.  Write the beginning of this heading.
*/
static void Pass2Heading(int argc, const char **argv, void *NotUsed){
  int i;
  int level;

  if( argv[0][1]<'1' || argv[0][1]>'6' ) return;
  level = argv[0][1] - '1';

  /* Add links to the end of the current file
  */
  if( pCurrentHeading==0 ){
    Heading *p =  pCurrentHeading = pFirstHeading;
    if( pOut ){
      fprintf(pOut,"\n<h2>Table Of Contents</h2>\n\n");
      while( p ){
        if( noScope ){
          fprintf(pOut,"<a name=toc%04d>\n",p->page);
        }
        WriteLinkTo(p);
        p = p->pNext;
      }
    }
    WriteLinkToIndex();
    SetUlDepth(0);
    if( pOut && pFirstFigure ){
      fprintf(pOut,"\n<h2>List Of Figures</h2>\n\n");
      SetUlDepth(1);
      for(p=pFirstFigure; p; p=p->pNext){
        WriteLinkTo(p);
      }
    }
    SetUlDepth(0);
  }else if( oneFile ){
    pCurrentHeading = pCurrentHeading->pNext;
  }else if( noScope ){
    if( pOut ){
      fprintf(pOut,"\n<p>");
      /* WriteButton("previous.gif","Previous Page",pCurrentHeading->pPrev); */
      WriteButton("next.gif","Next Page",pCurrentHeading->pNext);
      WriteTOCButton();
      WriteButton("index.gif","Index",pIndexHeading);
      fprintf(pOut,"\n");
    }
    pCurrentHeading = pCurrentHeading->pNext;
  }else{
    Heading *p = pCurrentHeading->pNext;
    int currentLevel = pCurrentHeading->level;
    int once = 1;
    if( pOut ) fprintf(pOut,"<p>\n");
    while( p ){
      if( p->level==currentLevel+1 ){
        if( once ) WriteLinkTo(p);
      }else if( p->level<=currentLevel ){
        WriteLinkTo(p);
        currentLevel = p->level;
        once = 0;
        if( !detailedScope ) currentLevel--;
      }
      p = p->pNext;
    }
    pCurrentHeading = pCurrentHeading->pNext;
    WriteLinkToIndex();
    SetUlDepth(0);
  }

  /* Open a new file and add links to its beginning
  */
  if( oneFile ){
    /* Do nothing */
  }else{
    if( pOut ) fclose(pOut);
    if( pCurrentHeading ){
      pOut = fopen(pCurrentHeading->zFile,"w");
      if( pOut==0 ){
        char zErr[100];
        sprintf(zErr,"Can't open \"%.*s\"",(int)sizeof(zErr)-30,
          pCurrentHeading->zFile);
        perror(zErr);
      }else{
        WriteDoNotEditComment();
        WriteHtmlTitle(pCurrentHeading->page);
        if( !noScope ) WriteLinkToTableOfContents();
      }
    }else{
      pOut = 0;
    }
    if( noScope ){
      if( pOut ){
        WriteButton("previous.gif","Previous Page",pCurrentHeading->pPrev);
        WriteButton("next.gif","Next Page",pCurrentHeading->pNext);
        WriteTOCButton();
        WriteButton("index.gif","Index",pIndexHeading);
        fprintf(pOut,"\n<p>\n");
      }
    }else{
      WriteLinkToParentAndSelf(pCurrentHeading->pPrev);
    }
  }

  /* Write out the header information
  */
  if( pOut && pCurrentHeading ){
    if( oneFile ){
      fprintf(pOut,"<a name=H%04d>\n",pCurrentHeading->page);
      fprintf(pOut,"<h%d>%s ",level>5?6:level+2,pCurrentHeading->zValue);
    }else if( noScope ){
      fprintf(pOut,"<h%d>%s ",level>5?6:level+2,pCurrentHeading->zValue);
    }else{
      SetUlDepth(pCurrentHeading->level+1);
      if( pCurrentHeading->zTitle ){
        fprintf(pOut,"\n<li><strong>%s %s</strong>\n",
          pCurrentHeading->zValue,
          pCurrentHeading->zTitle
        );
      }else{
        fprintf(pOut,"\n<li><strong>%s</strong>\n",
          pCurrentHeading->zValue
        );
      }
      SgmlWordHandler(0);
    }
  }
  SetUlDepth(0);
}

/* In pass 2, downshift all heading levels by 1.  h1 becomes h2,
** h2 becomes h3, and so forth.  h6 stays at h6.
*/
static void Pass2EndHeading(int argc, const char **argv, void *NotUsed){
  int i;
  int level;

  if( argv[0][2]<'1' || argv[0][2]>'6' ) return;
  level = argv[0][2] - '0' + 1;
  if( level==7 ) level = 6;

  if( oneFile || noScope ){
    if( pOut ) fprintf(pOut,"</h%d>",level);
  }else{
    SgmlWordHandler(CopyToOutput);
  }
}

/* In pass 2, write the title as an h1 heading right after the
** <body> statement.
*/
static void Pass2Body(int argc, const char **argv, void *NotUsed){
  if( pOut==0 ) return;
  if( oneFile ){
    fprintf(pOut,"\n<h1>%s</h1>\n",zTitle);
  }else{
    fprintf(pOut,"\n<h1>%s (page 1 of %d)</h1>\n",zTitle,nPage);
  }
}

/* In pass 2, write the title of the document as an h1 heading
** right after the </title> if there is no <body> command.
*/
static void Pass2EndTitle(int argc, const char **argv, void *NotUsed){
  if( pOut==0 ) return;
  fprintf(pOut,"</title>");
  if( !hasBody && !noTitle ){
    Pass2Body(argc,argv,NotUsed);
  }
}

/* Find a heading structure given a tag
*/
static Heading *FindHeadingFromTag(const char *zTag){
  int h;
  Heading *p;

  h = hash(zTag);
  p = apTagHash[h];
  while( p && stricmp(p->zTag,zTag)!=0 ){
    p = p->pCollide;
  }
  return p;
}

/* Deal with a <link tag=TAG> directive.  This translates into
** an anchor.
*/
static void Pass2Link(int argc, const char **argv, void *NotUsed){
  int i;
  Heading *pHeading;

  for(i=1; argv[i]; i+=2){
    if( stricmp(argv[i],"tag")==0 ) break;
  }
  if( argv[i]==0 ) return;
  pHeading = FindHeadingFromTag(argv[i+1]);
  if( pOut==0 || pHeading==0 || pHeading->zFile==0 ) return;
  fprintf(pOut,"<a href=%s>",pHeading->zURL);
}

/* Deal with a </link> directive
*/
static void Pass2EndLink(int argc, const char **argv, void *NotUsed){
  if( pOut==0 ) return;
  fprintf(pOut,"</a>");
}

/* Deal with a <ref tag=TAG> directive.  This is translated into
** pure text.
*/
static void Pass2Ref(int argc, const char **argv, void *NotUsed){
  int i;
  Heading *pHeading;

  for(i=1; argv[i]; i+=2){
    if( stricmp(argv[i],"tag")==0 ) break;
  }
  if( argv[i]==0 ) return;
  pHeading = FindHeadingFromTag(argv[i+1]);
  if( pOut==0 ) return;
  if( pHeading==0 || pHeading->zValue==0 ){
    fprintf(pOut,"???");
  }else{
    fprintf(pOut,"%s",pHeading->zValue);
  }
}

/* Convert an <image> markup into <img>.  The "eps=" and "width=" fields
** are ignored.  "gif=" is converted into "src=" and "alt=" is copied thru.
** "tag=" is used to find the figure number and "caption=" is used
** to generate text for the caption.
*/
static void Pass2Image(int argc, const char **argv, void *NotUsed){
  const char *zAlt = "**Image**";
  const char *zFile = 0;
  const char *zTag = 0;
  const char *zCaption = 0;
  Heading *pHeading = 0;
  int i;

  if( pOut==0 ) return;
  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"gif")==0 ){
      zFile = argv[i+1];
    }else if( stricmp(argv[i],"alt")==0 ){
      zAlt = argv[i+1];
    }else if( stricmp(argv[i],"tag")==0 ){
      zTag = argv[i+1];
    }else if( stricmp(argv[i],"caption")==0 ){
      zCaption = argv[i+1];
    }
  }
  if( zCaption ){
    int a,b;
    if( pCurrentFigure==0 ){
      pHeading = pFirstFigure;
    }else{
      pHeading = pCurrentFigure->pNext;
    }
    assert( pHeading!=0 );
    pCurrentFigure = pHeading;
    assert( zTag==0 || strcmp(zTag,pHeading->zTag)==0 );
    assert( strcmp(zCaption,pHeading->zTitle)==0 );
    sscanf(pHeading->zValue,"%d.%d",&a,&b);
    fprintf(pOut,"\n<a name=F%dx%d><hr>",a,b);
  }
  if( zFile ){
    fprintf(pOut,"\n<p align=center>\n<img src=\"%s\" alt=\"%s\">\n",
       zFile,zAlt);
  }
  if( pHeading ){
    fprintf(pOut,"<h4 align=center>Figure %s: %s</h4><hr>\n",
      pHeading->zValue,zCaption);
  }else{
    fprintf(pOut,"<p>\n");
  }
}

/* Ignore all text until we call On().  (This is actually used
** in order to ignore the text contained in <latex>...</latex>)
*/
void Off(int argc, const char **argv, void *pDummy){
  SgmlWordHandler(0);
  SgmlSpaceHandler(0);
}

/* Stop ignoring text
*/
void On(int argc, const char **argv, void *pDummy){
  SgmlWordHandler(CopyToOutput);
  SgmlSpaceHandler(CopyToOutput);
}

static void Pass2Index(int argc, const char **argv, void *NotUsed){
  int i;
  IndexEntry *pNew;
  char zMark[10];
  char zLabel[100];
  static int cnt = 0;
  extern void *malloc();

  for(i=1; argv[i]; i+=2){
    if( stricmp(argv[i],"text")==0 ) break;
  }
  if( argv[i]==0 ) return;
  pNew = malloc( sizeof(IndexEntry) );
  if( pNew==0 ) return;
  pNew->zText = StrDup(argv[i+1]);
  sprintf(zLabel,"%.*s#X%05d",
    (int)sizeof(zLabel)-30,
    oneFile ? "" : pCurrentHeading->zFile,
    cnt
  );
  sprintf(zMark,"X%05d",cnt);
  cnt++;
  pNew->zLabel = StrDup(zLabel);
  pNew->pNext = pIndexList;
  pIndexList = pNew;
  if( pOut && pCurrentHeading ){
    fprintf(pOut,"<a name=%s>",zMark);
  }
}

/* Process the <quote> and </quote> markups
*/
void Quote(int argc, const char **argv, void *NotUsed){
  if( pOut ) fprintf(pOut,"``");
}
void EndQuote(int argc, const char **argv, void *NotUsed){
  if( pOut ) fprintf(pOut,"''");
}

/* Translate <cmd>..</cmd> into <kbd>..</kbd>
*/
void Cmd(int argc, const char **argv, void *NotUsed){
  if( pOut ) fprintf(pOut,"<kbd>");
}
void EndCmd(int argc, const char **argv, void *NotUsed){
  if( pOut ) fprintf(pOut,"</kbd>");
}

/* Translate <func>..</func> into <code>..()</code>
*/
void Func(int argc, const char **argv, void *NotUsed){
  if( pOut ) fprintf(pOut,"<code>");
}
void EndFunc(int argc, const char **argv, void *NotUsed){
  if( pOut ) fprintf(pOut,"()</code>");
}

/* Compare to IndexEntries
*/
static int IndexCompare(IndexEntry *pLeft, IndexEntry *pRight){
  int x;
  if( pLeft==0 ) return 1;
  if( pRight==0 ) return -1;
  x = stricmp(pLeft->zText,pRight->zText);
  return x;
}

/* Merge two lists of IndexEntrys
*/
static IndexEntry *Merge(IndexEntry *pLeft, IndexEntry *pRight){
  IndexEntry *pFirst;
  IndexEntry *pLast;

  if( pLeft==0 ) return pRight;
  if( pRight==0 ) return pLeft;
  if( IndexCompare(pLeft,pRight)<0 ){
    pFirst = pLast = pLeft;
    pLeft = pLeft->pNext;
  }else{
    pFirst = pLast = pRight;
    pRight = pRight->pNext;
  }
  while( pLeft && pRight ){
    if( IndexCompare(pLeft,pRight)<0 ){
      pLast->pNext = pLeft;
      pLast = pLast->pNext;
      pLeft = pLeft->pNext;
    }else{
      pLast->pNext = pRight;
      pLast = pLast->pNext;
      pRight = pRight->pNext;
    }
  }
  if( pLeft ){
    pLast->pNext = pLeft;
  }else if( pRight ){
    pLast->pNext = pRight;
  }else{
    pLast->pNext = 0;
  }
  return pFirst;
}

/* Sort a list of index entries
*/
#define NSLOT 20
static IndexEntry *SortIndex(IndexEntry *pFirst){
  int i;
  IndexEntry *set[NSLOT];

  for(i=0; i<NSLOT; i++){ set[i] = 0; }
  while( pFirst ){
    IndexEntry *pNext = pFirst->pNext;
    pFirst->pNext = 0;
    for(i=0; i<NSLOT-1 && set[i]!=0; i++){
      pFirst = Merge(set[i],pFirst);
      set[i] = 0;
    }
    set[i] = Merge(set[i],pFirst);
    pFirst = pNext;
  }
  for(i=0; i<NSLOT; i++){
    pFirst = Merge(pFirst,set[i]);
  }
  return pFirst;
}

/* Print the index
*/
static void PrintIndex(void){
  IndexEntry *p;
  if( pOut==0 || pIndexList==0 || pIndexHeading==0 ) return;
 
  p = pIndexList = SortIndex(pIndexList);
  if( !oneFile ){
    WriteDoNotEditComment();
    WriteHtmlTitle(pIndexHeading->page);
    if( noScope ){
      WriteTOCButton();
    }else{
      WriteLinkToTableOfContents();
    }
  }
  fprintf(pOut,"<h2>Index</h2>\n",pIndexHeading->zValue);
  while( p ){
    fprintf(pOut,"<a href=%s>%s</a><br>\n",
      p->zLabel,
      p->zText
    );
    p = p->pNext;
  }
}

static void PrintMarkup(int argc, const char **argv, void *NotUsed){
  int i, j;
  const char *z;
  if( pOut==0 ) return;
  fprintf(pOut,"<%s",argv[0]);
  for(i=1; i<argc; i+=2){
    fprintf(pOut," %s=",argv[i]);
    for(z=argv[i+1]; *z; z++){
      if( isspace(*z) ){
        fprintf(pOut,"'%s'",argv[i+1]);
      }
    }
    if( *z==0 ){
      fprintf(pOut,"%s",argv[i+1]);
    }
  }
  fprintf(pOut,">");
}

static void Usage(char **argv){
  fprintf(stderr,"Usage: %s [-flat] [-noscope] [-notitle] base.doc\n",argv[0]);
  exit(1);
}

void main(int argc, char **argv){
  char *zBase;
  char *zTail;
  char *zOutput;
  FILE *pIn;
  int i;
  int endSwitch = 0;
  char *zInputFilename = 0;

  for(i=1; i<argc; i++){
    if( endSwitch || argv[i][0]!='-' ){
      if( zInputFilename ) Usage(argv);
      zInputFilename = argv[i];
    }else{
      if( strcmp(argv[i],"-flat")==0 ){
        oneFile = 1;
      }else if( strcmp(argv[i],"-notitle")==0 ){
        noTitle = 1;
      }else if( strcmp(argv[i],"-detailedscope")==0 ){
        detailedScope = 1;
      }else if( strcmp(argv[i],"-noscope")==0 ){
        noScope = 1;
      }else if( strcmp(argv[i],"-buttons")==0 ){
        noScope = 1; graphicalButton = 1;
      }else if( strcmp(argv[i],"--")==0 ){
        endSwitch = 1;
      }
    }
  }
  zTail = strrchr(zInputFilename,'.');
  if( zTail==0 || strcmp(zTail,".doc")!=0 ) Usage(argv);
  *zTail = 0;
  zBase = strrchr(zInputFilename,'/');
  if( zBase==0 ) zBase = zInputFilename;
  else           zBase = &zBase[1];
  zBasename = StrDup(zBase);
  *zTail = '.';
  zOutput = MakeNewFilename();
  pIn = fopen(zInputFilename,"r");
  if( pIn==0 ){
    char zErr[100];
    sprintf(zErr,"Can't open \"%.*s\"",(int)sizeof(zErr)-30,zInputFilename);
    perror(zErr);
    exit(1);
  }
  pOut = 0;

  SgmlWordHandler(Pass1Word);
  SgmlSpaceHandler(Pass1Space);
  SgmlDefaultMarkupHandler(0);
  SgmlHandler("h1",Pass1Heading);
  SgmlHandler("h2",Pass1Heading);
  SgmlHandler("h3",Pass1Heading);
  SgmlHandler("h4",Pass1Heading);
  SgmlHandler("h5",Pass1Heading);
  SgmlHandler("h6",Pass1Heading);
  SgmlHandler("/h1",Pass1EndHeading);
  SgmlHandler("/h2",Pass1EndHeading);
  SgmlHandler("/h3",Pass1EndHeading);
  SgmlHandler("/h4",Pass1EndHeading);
  SgmlHandler("/h5",Pass1EndHeading);
  SgmlHandler("/h6",Pass1EndHeading);
  SgmlHandler("index",Pass1Index);
  SgmlHandler("title",Pass1Title);
  SgmlHandler("/title",Pass1EndTitle);
  SgmlHandler("body",Pass1Body);
  SgmlHandler("image",Pass1Image);
  SgmlParse(pIn,0);

  if( IndexSeen ){
    MakeEmptyHeading(0);
    pLastHeading->zTitle = "Index";
    pIndexHeading = pLastHeading;
    if( pLastHeading->pPrev ){
      pLastHeading->pPrev->pNext = 0;
    }else{
      pFirstHeading = 0;
    }
    pLastHeading = pIndexHeading->pPrev;
    pIndexHeading->pPrev = 0;
  }

  SgmlHandlerReset();
  SgmlWordHandler(CopyToOutput);
  SgmlSpaceHandler(CopyToOutput);
  SgmlDefaultMarkupHandler(PrintMarkup);
  SgmlHandler("h1",Pass2Heading);
  SgmlHandler("h2",Pass2Heading);
  SgmlHandler("h3",Pass2Heading);
  SgmlHandler("h4",Pass2Heading);
  SgmlHandler("h5",Pass2Heading);
  SgmlHandler("h6",Pass2Heading);
  SgmlHandler("/h1",Pass2EndHeading);
  SgmlHandler("/h2",Pass2EndHeading);
  SgmlHandler("/h3",Pass2EndHeading);
  SgmlHandler("/h4",Pass2EndHeading);
  SgmlHandler("/h5",Pass2EndHeading);
  SgmlHandler("/h6",Pass2EndHeading);
  SgmlHandler("link",Pass2Link);
  SgmlHandler("/link",Pass2EndLink);
  SgmlHandler("ref",Pass2Ref);
  SgmlHandler("index",Pass2Index);
  SgmlHandler("/title",Pass2EndTitle);
  SgmlHandler("body",Pass2Body);
  SgmlHandler("image",Pass2Image);
  SgmlHandler("latex",Off);
  SgmlHandler("/latex",On);
  SgmlHandler("quote",Quote);
  SgmlHandler("/quote",EndQuote);
  SgmlHandler("cmd",Cmd);
  SgmlHandler("/cmd",EndCmd);
  SgmlHandler("func",Func);
  SgmlHandler("/func",EndFunc);
  pOut = fopen(zOutput,"w");
  if( pOut==0 ){
    char zErr[100];
    sprintf(zErr,"Can't open \"%s\" for writing",
      (int)sizeof(zErr)-40,zOutput);
    perror(zErr);
  }  
  rewind(pIn);
  SgmlParse(pIn,0);
  if( pIndexHeading ){
    if( !oneFile ){
      if( noScope ){
        if( pOut ) fprintf(pOut,"\n<p>\n");
        WriteTOCButton();
        WriteButton("index.gif","Index",pIndexHeading);
        if( pOut ) fprintf(pOut,"\n");
      }else{
        WriteLinkToIndex();
      }
      if( pOut ) fclose(pOut);
      pOut = fopen(pIndexHeading->zFile,"w");
      if( pOut==0 ){
        char zErr[100];
        sprintf(zErr,"Can't open \"%s\" for writing",
          (int)sizeof(zErr)-40,zOutput);
        perror(zErr);
      }
    }
    PrintIndex();
  }  
  fclose(pIn);
  if( pOut ) fclose(pOut);
  exit(0);
}
