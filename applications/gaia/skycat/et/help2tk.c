/*
** The program coded by this file reads a single SGML file and
** emits Tcl/Tk code for hypertext help.  The following SGML is
** understood:
** 
**
** <p>..</p>                       A paragraph
**
** <br>                            A line break
**
** <page tag=X title=Y button=Z>   Defines a single help page.  tag= is
**                                 required.
**
** <ul>..<li>..<li>..</ul>         An unnumbered list.
**
** <ol>..<li>..<li>..</ol>         An ordered list.
**
** <em>..</em>                     Emphasized text.
**
** <center>..</center>             Centered text.
**
** <strong>..</strong>             Strong emphasis.
**
** <code>..</code>                 Code. (Constant width font)
**
** <pre>..</pre>                   Preformatted text.
**
** <link tag=X>..</link>           A hyperlink to another page.
*/
#include <stdio.h>
#include <ctype.h>
#define stricmp strcasecmp

/****************** Begin Escape Sequence Translator *************/
/*
** The next section of code implements routines used to translate
** the '&' escape sequences of SGML to individual characters
*/

/* Each escape sequence is recorded as an instance of the following
** structure
*/
typedef struct sgEsc Esc;
struct sgEsc {
  char *zName;     /* The name of this escape sequence.  ex:  "amp" */
  int value;       /* The value for this sequence.       ex:  '&' */
  Esc *pNext;      /* Next sequence with the same hash on zName */
};


/* The size of the handler hash table. */
#define ESC_HASH_SIZE 203

/* The following flag is TRUE if the escape sequence has table needs
** to be initialized.
*/
static int bEscNeedsInit = 1;

/* The handler hash table */
static Esc *apEscHash[ESC_HASH_SIZE];

/* Hash a handler name */
static int EscHash(const char *zName){
  int h = 0;
  char c;
  while( (c=*zName)!=0 ){
    if( isupper(c) ) c = tolower(c);
    h = h<<5 ^ h ^ c;
    zName++;
  }
  if( h<0 ) h = -h;
  return h % ESC_HASH_SIZE;
}

/* The following is a table of all escape sequences
*/
static Esc esc_sequences[] = {
  { "amp",       '&',        0 },
  { "lt",        '<',        0 },
  { "gt",        '>',        0 },
};

/* Initialize the escape sequence hash table
*/
static void EscInit(void){
  int i;  /* For looping thru the list of escape sequences */
  int h;  /* The hash on a sequence */

  for(i=0; i<sizeof(esc_sequences)/sizeof(esc_sequences[0]); i++){
    h = EscHash(esc_sequences[i].zName);
    esc_sequences[i].pNext = apEscHash[h];
    apEscHash[h] = &esc_sequences[i];
  }
}

/* Translate escape sequences in the string "z".  "z" is overwritten
** with the translated sequence.
**
** Example:
**
**      input =    "AT&amp;T &gt MCI"
**      output =   "AT&T > MCI"
*/
void TranslateEscapes(char *z){
  int from;   /* Read characters from this position in z[] */
  int to;     /* Write characters into this position in z[] */
  int h;      /* A hash on the escape sequence */
  Esc *p;     /* For looping down the escape sequence collision chain */

  from = to = 0;
  if( bEscNeedsInit ){
    EscInit();
    bEscNeedsInit = 0;
  }
  while( z[from] ){
    if( z[from]=='&' ){
      int i = from+1;
      int c;
      while( z[i] && isalpha(z[i]) ){ i++; }
      c = z[i];
      z[i] = 0;
      h = EscHash(&z[from+1]);
      p = apEscHash[h];
      while( p && strcmp(p->zName,&z[from+1])!=0 ){ p = p->pNext; }
      z[i] = c;
      if( p ){
        z[to++] = p->value;
        from = i;
        if( c==';' ) from++;
      }else{
        z[to++] = z[from++];
      }
    }else{
      z[to++] = z[from++];
    }
  }
  z[to] = 0;
}

/******************* End Escape Sequence Translator ***************/

/******************* Begin SGML parser code *******************/
/*
** The following code implements several subroutines that are
** used to parse HTML.  This code is pasted into from a separate
** file, in order to make the htmlsplit.c sources self-contained.
** Logically, these routines belong in a separate file.
*/
/* These three pointers define certain special handlers.
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

/* The size of the handler hash table. */
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

/* Given a pointer to an input file, read the and parse that file
** as if it were SGML.
**
** This is not a true SGML parser, in that it handles some unusual
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
      TranslateEscapes(argv[0]);
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
          TranslateEscapes(argv[argc]);
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
      TranslateEscapes(zBuf);
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
extern void *malloc();
void SgmlHandler(const char *zName, void (*xFunc)(int,const char**,void*)){
  int h = SgmlHash(zName);
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

/* A tag is stored as an instance of the following structure
*/
typedef struct sgTag Tag;
struct sgTag {
  char zName[20];       /* Name of this tag */
  Tag *pNext;           /* Next tag on the list */
  Tag *pPrev;           /* Previous tag on the list */
};

/* Page titles are stored in a linked list of the following
** structure.  This information is eventually used to build
** an index page.
*/
typedef struct sgTitle Title;
struct sgTitle {
  char *zTitle;        /* A single title */
  char *zTag;          /* The tag */
  Title *pNext;        /* Next title in the list */
};

static Tag *pTagList = 0;     /* All current tags */
static Tag *pFreeTags = 0;    /* Tags available for reuse */

static char zBuf[1000];       /* Text waiting for output */
static int nBuf = 0;          /* First free slot in zBuf[] */

static char *zWidget = "$w";  /* Name of the text widget */

static int bCollapseSpace = 1;     /* When true, all whitespace in the
                                   ** HTML is converted to a single space
                                   ** in the output. */
static int bIgnoreSpace = 1;       /* Discard all space characters up to the
                                   ** next non-space */
static int bDiscard = 0;           /* Discard all input when TRUE */

#define mxListDepth 6               /* Maximum nesting depth of <ul> or <ol> */
static int listDepth = -1;          /* Current nesting depth */
static int listCount[mxListDepth];  /* Counter setting.  -1 for <ul> */

static int inPageProc = 0;          /* Currently withing a HelpXXXX proc */

static int priorNewlines = 3;       /* Number of newlines at the end of the
                                    ** previous zBuf[] */

static Title *pTitleList = 0;       /* List of all titles */

/* Return a pointer to an unused Tag structure
*/
static Tag *GetTag(void){
  Tag *p;
  if( pFreeTags==0 ){
    p = malloc( sizeof(Tag) );
    if( p==0 ){
      fprintf(stderr,"Out of memory\n");
      exit(1);
    }
  }else{
    p = pFreeTags;
    pFreeTags = p->pNext;
  }
  return p;
}

/* The Tag structure "p" is no longer being used.  Return it
** to the free-list for possible reuse.
*/
static void FreeTag(Tag *p){
  p->pNext = pFreeTags;
  pFreeTags = p;
}

/* Flush the contents of zBuf[] into the text widget
*/
static void Flush(void){
  Tag *p;
  char *zSpace = "";
  int i,j;
  char zBuf2[sizeof(zBuf)*2];

  if( nBuf==0 ) return;
  zBuf[nBuf] = 0;
  i = j = priorNewlines = 0;
  while( zBuf[i] ){
    switch( zBuf[i] ){
      case '\n':
        zBuf2[j++] = '\\';
        zBuf2[j++] = 'n';
        priorNewlines++;
        i++;
        break;

      case '[':
      case ']':
      case '$':
      case '\"':
      case '\\':
        zBuf2[j++] = '\\';
        /* Fall thru */
      default:
        zBuf2[j++] = zBuf[i++];
        priorNewlines = 0;
    }
  }
  zBuf2[j] = 0;
  printf("%s insert end \"%s\" {",zWidget,zBuf2);
  for(p=pTagList; p; p=p->pNext){
    printf("%s%s",zSpace,p->zName);
    zSpace = " ";
  }
  printf("}\n");
  nBuf = 0;
}

/* Push a tag onto the tag list.  The buffer will be flushed
** automatically prior to the push.
*/
static void Push(char *zTag){
  Tag *p;

  Flush();  
  p = GetTag();
  sprintf(p->zName,"%.15s",zTag);
  p->pNext = pTagList;
  p->pPrev = 0;
  if( pTagList ){
    pTagList->pPrev = p;
  }
  pTagList = p;
}

/* Pop the tag named in the argument from the tag list.  The buffer
** will be flushed prior to the pop.
*/
static void Pop(char *zTag){
  Tag *p;

  for(p=pTagList; p; p=p->pNext){
    if( strcmp(p->zName,zTag)==0 ) break;
  }
  if( p==0 ) return;
  Flush();
  if( p->pPrev ){
    p->pPrev->pNext = p->pNext;
  }else{
    pTagList = p->pNext;
  }
  if( p->pNext ){
    p->pNext->pPrev = p->pPrev;
  }
  FreeTag(p);
}

/* Add text to the output buffer
*/
static void Out(const char *zText){
  while( *zText ){
    if( nBuf>=sizeof(zBuf)-1 ) Flush();
    zBuf[nBuf++] = *zText++;
  }
}

/* Make sure there are at least N newlines at the end of the buffer.
*/
static void NeedNewlines(int N){
  int i;
  i = nBuf-1;
  while( i>=0 && N ){
    if( zBuf[i]!='\n' ) break;
    i--;
    N--;
  }
  if( i<0 ) N -= priorNewlines;
  while( N>0 ){
    Out("\n");
    N--;
  }
  bIgnoreSpace = 1;
}

/* This routine handles all non-whitespace in the original HTML.
*/
static void HandleWord(const char *zText, void *NotUsed){
  if( bDiscard ) return;
  bIgnoreSpace = 0;
  Out(zText);
}

/* This routine handles all white-space in the original HTML.
*/
static void  HandleSpace(const char *zText, void *NotUsed){
  if( bDiscard ) return;
  if( bCollapseSpace && bIgnoreSpace ) return;
  if( bCollapseSpace ){
    if( nBuf==0 || zBuf[nBuf-1]!=' ' ) Out(" ");
  }else{
    Out(zText);
  }
}

/* Handle all of the following: em code
*/
static void Style(int argc, const char **argv, void *NotUsed){
  int i;
  char zTag[10];
  for(i=0; argv[0][i]; i++){
    if( isupper(argv[0][i]) ){
      zTag[i] = tolower(argv[0][i]);
    }else{
      zTag[i] = argv[0][i];
    }
  }
  zTag[i] = 0;
  Push(zTag);
}

/* Handle all of the following: /em /code
*/
static void EndStyle(int argc, const char **argv, void *NotUsed){
  int i;
  char zTag[10];
  for(i=0; argv[0][i+1]; i++){
    if( isupper(argv[0][i+1]) ){
      zTag[i] = tolower(argv[0][i+1]);
    }else{
      zTag[i] = argv[0][i+1];
    }
  }
  zTag[i] = 0;
  Pop(zTag);
}

/* Handle the <pre> and </pre> directives
*/
static void Preformat(int argc, const char **argv, void *NotUsed){
  NeedNewlines(1);
  Push("pre");
  bCollapseSpace = 0;
}
static void EndPreformat(int argc, const char **argv, void *NotUsed){
  NeedNewlines(2);
  Pop("pre");
  bCollapseSpace = 1;
}

/* Handle the <p>
*/
static void Paragraph(int argc, const char **argv, void *NotUsed){
  NeedNewlines(2);
}

/* Handle the </p>
*/
static void EndParagraph(int argc, const char **argv, void *NotUsed){
}

/* Handle <br>
*/
static void Break(int argc, const char **argv, void *NotUsed){
  NeedNewlines(1);
}

/* Handle <link> and </link>
*/
static int nAnchor = 0;
static void Anchor(int argc, const char **argv, void *NotUsed){
  int i;
  char zTag[10];

  if( argv[0][0]=='/' ){
    sprintf(zTag,"a%d",nAnchor);
    Pop(zTag);
    Pop("a");
    return;
  }
  for(i=1; i<argc; i+=2){ 
    if( stricmp(argv[i],"tag")==0 ){
      nAnchor++;
      printf("%s tag bind a%d <1> {Help %s}\n",
        zWidget, nAnchor, argv[i+1]);
      Push("a");
      sprintf(zTag,"a%d",nAnchor);
      Push(zTag);
      break;
    }
  }
}

/* Handle <ul> and <ol>
*/
static void List(int argc, const char **argv, void *NotUsed){
  char zTag[10];

  listDepth++;
  if( listDepth>=mxListDepth ) return;
  if( argv[0][0]=='u' || argv[0][0]=='U' ){
    listCount[listDepth] = -1;
  }else{
    listCount[listDepth] = 0;
  }
  NeedNewlines(1);
  if( listDepth>0 ){
    sprintf(zTag,"i%d",listDepth-1);
    Pop(zTag);
  }
  sprintf(zTag,"i%d",listDepth);
  Push(zTag);
}

/* Handle </ul> and </ol>
*/
static void EndList(int argc, const char **argv, void *NotUsed){
  char zTag[10];

  if( listDepth<0 ) return;
  NeedNewlines(1);
  listDepth--;
  if( listDepth+1>=mxListDepth ){
    return;
  }
  sprintf(zTag,"i%d",listDepth+1);
  Pop(zTag);
  if( listDepth>=0 ){
    sprintf(zTag,"i%d",listDepth);
    Push(zTag);
  }
}

/* Handle the <li> directive
*/
static void ListItem(int argc, const char **argv, void *NotUsed){
  char zTag1[10], zTag2[10];
  if( listDepth<0 || listDepth>=mxListDepth ) return;
  NeedNewlines(1);
  sprintf(zTag1,"i%d",listDepth);
  Pop(zTag1);
  sprintf(zTag2,"l%d",listDepth);
  Push(zTag2);
  if( listCount[listDepth]<0 ){
    /* A Bullet list */
    Out("  \001\t");
  }else{
    /* A numbered list */
    char zNum[20];
    sprintf(zNum,"  %2d.\t",++listCount[listDepth]);
    Out(zNum);
  }
  Pop(zTag2);
  Push(zTag1);
}

/* Handle the <page tag=xxx> directive
*/
static void Page(int argc, const char **argv, void *NotUsed){
  int i;
  const char *zTag = 0;
  const char *zTitle = 0;
  const char *zButton = 0;
  Title *pNew, *p;

  Flush();
  nAnchor = 0;
  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"tag")==0 ){
      zTag = argv[i+1];
    }
    if( stricmp(argv[i],"title")==0 ){
      zTitle = argv[i+1];
    }
    if( stricmp(argv[i],"button")==0 ){
      zButton = argv[i+1];
    }
  }
  if( zTag==0 ){
    fprintf(stderr,"Missing \"tag\" argument on <page>\n");
    return;
  }
  if( inPageProc ){
    printf("};# End of script\n");
  }
  if( zButton ){
    printf("set __HelpButton(%s) %s\n",zTag,zButton);
  }
  printf("set __HelpScript(%s) {\n",zTag);
  inPageProc = 1;
  if( zTitle ){
    Push("title");
    HandleWord(zTitle,0);
    Pop("title");
    NeedNewlines(2);
    pNew = malloc( sizeof(Title) + strlen(zTitle)+1 + strlen(zTag)+1 );
    if( pNew ){
      pNew->zTitle = (char*)&pNew[1];
      strcpy(pNew->zTitle,zTitle);
      pNew->zTag = &pNew->zTitle[strlen(zTitle)+1];
      strcpy(pNew->zTag,zTag);
      pNew->pNext = 0;
      if( pTitleList==0 ){
        pTitleList = pNew;
      }else{
        for(p=pTitleList; p->pNext; p = p->pNext){};
        p->pNext = pNew;
      } 
    }
  }
}

/* Generate a index for the <indexpage> directive
*/
static void IndexPage(int argc, const char **argv, void *NotUsed){
  int i;
  const char *zTag = 0;
  const char *zTitle = 0;
  const char *zButton = 0;
  Title *p;
  static const char *anchor[] = { "link", "tag", 0, 0 };
  static const char *endanchor[] = { "/link", 0 };

  Flush();
  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"tag")==0 ){
      zTag = argv[i+1];
    }
    if( stricmp(argv[i],"title")==0 ){
      zTitle = argv[i+1];
    }
    if( stricmp(argv[i],"button")==0 ){
      zButton = argv[i+1];
    }
  }
  if( zTag==0 ){
    fprintf(stderr,"Missing \"tag\" argument on <indexpage>\n");
    return;
  }
  if( inPageProc ){
    printf("};# End of script\n");
  }
  if( zButton ){
    printf("set __HelpButton(%s) %s\n",zTag,zButton);
  }
  printf("set __HelpScript(%s) {\n",zTag);
  if( zTitle ){
    Push("title");
    HandleWord(zTitle,0);
    Pop("title");
    NeedNewlines(2);
  }
  for(p=pTitleList; p; p=p->pNext){
    anchor[2] = p->zTag;
    Anchor(3,anchor,0);
    HandleWord(p->zTitle,0);
    Anchor(1,endanchor,0);
    NeedNewlines(1);
  }
  Flush();
  printf("}; # End of script\n");
  inPageProc = 0;
}

static void Usage(char **argv){
  fprintf(stderr,"Usage: %s [-widget WIDGET] [-callback CALLBACK] filename\n",
    argv[0]);
  exit(1);
}

/* A table of markups and the routines to handle them
*/
static struct {
  char *zMark;
  void (*xHandler)(int, const char**, void*);
} markups[] = {
  { "em",      Style },
  { "/em",     EndStyle },
  { "strong",  Style },
  { "/strong", EndStyle },
  { "code",    Style },
  { "/code",   EndStyle },
  { "center",  Style },
  { "/center", EndStyle },
  { "br",      Break },
  { "p",       Paragraph },
  { "/p",      EndParagraph },
  { "link",    Anchor },
  { "/link",   Anchor },
  { "ol",      List },
  { "ul",      List },
  { "/ol",     EndList },
  { "/ul",     EndList },
  { "li",      ListItem },
  { "pre",     Preformat },
  { "/pre",    EndPreformat },
  { "page",    Page },
  { "indexpage", IndexPage },
};

void main(int argc, char **argv){
  FILE *pIn;
  int i;
  int endSwitch = 0;
  char *zInputFilename = 0;

  for(i=1; i<argc; i++){
    if( endSwitch || argv[i][0]!='-' || i==argc-1){
      if( zInputFilename ) Usage(argv);
      zInputFilename = argv[i];
    }else{
      if( strcmp(argv[i],"--")==0 ){
        endSwitch = 1;
      }
    }
  }
  pIn = fopen(zInputFilename,"r");
  if( pIn==0 ){
    char zErr[100];
    sprintf(zErr,"Can't open \"%.*s\"",(int)sizeof(zErr)-30,zInputFilename);
    perror(zErr);
    exit(1);
  }

  SgmlWordHandler(HandleWord);
  SgmlSpaceHandler(HandleSpace);
  SgmlDefaultMarkupHandler(0);
  for(i=0; i<sizeof(markups)/sizeof(markups[0]); i++){
    SgmlHandler(markups[i].zMark,markups[i].xHandler);
  }
  SgmlParse(pIn,0);
  Flush();
  if( inPageProc ){
    printf("};# End of script\n");
  }
  exit(0);
}
