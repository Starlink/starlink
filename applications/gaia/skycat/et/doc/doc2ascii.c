/*
** The program coded by this file reads a single SGML document
** and converts it into plain ascii, which is emitted on standard
** output.
**
** Summary of SGML markup supported by this program:
**
** <hN>..</hN>                  Heading (N between 1 and 6)
** <br>                         Line break
** <p>..</p>                    Paragraph
** <pre>...</pre>               Preformatted text
** <ol>..<li>..</ol>            Ordered list
** <ul>..<li>..</ul>            Unordered list (a bullet list)
** <title>..</title>            Document title
** <html>..</html>              HTML code
** <latex>..</latex>            LaTeX code
** <dl><dt><dd></dl>            Definition list
** <em>..</em>                  Emphasize
** <strong>..</strong>          Strong emphasis
** <image>                      An image or figure
** <index>                      An entry for the index
** <ref>                        A reference to a heading or figure
** <blockquote>..</blockquote>  A block quoted paragraph
** <quote>..</quote>            Quotation marks
** <cmd>..</cmd>                Text of a computer command
** <func>..</func>              Name of a function or procedure
** <code>..</code>              An excerpt of programming language
** <file>..</file>              The name of a file
** <var>..</var>                Meta-language text
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#define stricmp strcasecmp

/****************** Begin Escape Sequence Translator *************/
/*
** The next section of code implements routines used to translate
** the '&' escape sequences of SGML to individual characters.
** Examples:
**
**         &amp;          &
**         &lt;           <
**         &gt;           >
**         &nbsp;         Hard-Space
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

/* The size of the handler hash table.  For best results this should
** be a prime number which is about the same size as the number of
** escape sequences known to the system. */
#define ESC_HASH_SIZE 7

/* The following flag is TRUE if the escape sequence has table needs
** to be initialized.
*/
static int bEscNeedsInit = 1;

/* The hash table 
**
** If the name of an escape sequences hashes to the value H, then
** apEscHash[H] will point to a linked list of Esc structures, one of
** which will be the Esc structure for that escape sequence.
*/
static Esc *apEscHash[ESC_HASH_SIZE];

/* Hash a escape sequence name.  The value returned is an integer
** between 0 and ESC_HASH_SIZE-1, inclusive.
*/
static int EscHash(const char *zName){
  int h = 0;      /* The hash value to be returned */
  char c;         /* The next character in the name being hashed */

  while( (c=*zName)!=0 ){
    if( isupper(c) ) c = tolower(c);
    h = h<<5 ^ h ^ c;
    zName++;
  }
  if( h<0 ) h = -h;
  return h % ESC_HASH_SIZE;
}

/* The following is a table of all escape sequences.  Add new sequences
** by adding entries to this table.
*/
static Esc esc_sequences[] = {
  { "amp",       '&',        0 },
  { "lt",        '<',        0 },
  { "gt",        '>',        0 },
  { "nbsp",      ' ',        0 },
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
** Unrecognized escape sequences are unaltered.
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
** used to parse HTML.
*/
/* These three pointers define certain special handlers.  xSpaceHandler
** is called to deal with all white space.  xWordHandler is called to
** deal with every sequence of characters that is not white-space.
** xDefaultMarkupHandler is called for every SGML directive that
** doesn't have its own handler.
*/
static void (*xSpaceHandler)(const char*,void*);
static void (*xWordHandler)(const char*,void*);
static void (*xDefaultMarkupHandler)(int, const char**, void*);

/* Each specific SGML markup handler is stored in a hash table as an 
** instance of the following structure.
*/
typedef struct sgHandler Handler;
struct sgHandler {
  char *zName;                                /* Name of markup to handle */
  void (*xHandler)(int, const char**, void*); /* Routine to do the work */
  Handler *pCollide;                          /* Next handler with same hash */
};

/* The size of the handler hash table.  For best results, this should
** be a prime number which is at least as large as the number of
** handlers registered with the system 
*/
#define SGML_HASH_SIZE 203

/* The handler hash table 
**
** If an SGML markup name hashes to H, then apHandler[H] will point to
** a linked list of Handler structure, one of which will describe the
** handler for that particular markup (if it exists.)
*/
static Handler *apHandler[SGML_HASH_SIZE];

/* Hash a handler name
**
** SGML markup is case insensitive, so this function will give the
** same hash regardless of the case of the markup name.
**
** The value returned is an integer between 0 and SGML_HASH_SIZE-1,
** inclusive.
*/
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

/* Record the current line number
*/
int linenum = 1;

/* Return a single character from the given input file.  Return
** EOF at end-of-file.  Keep the linenum variable up-to-date.
*/
static int OneChar(FILE *in){
  int c = getc(in);
  if( c=='\n' ) linenum++;
  return c;
}

/* Given a pointer to an input file, read the and parse that file
** as if it were SGML.
**
** This is not a true SGML parser, in that it handles some unusual
** cases differently, and ignores the & operator completely.
*/
void SgmlParse(
  FILE *in,            /* Read input from this stream */
  void *pArg           /* Give this value as the 3rd argument to each handler */
){
  int c;               /* The next character of input */
  int i, j;            /* Loop counters */
  int argc;            /* The number of arguments on a markup */
  Handler *pHandler;   /* The handler used for the markup */
  char *argv[100];     /* Pointers to each markup argument. */
  char zBuf[1000];     /* Space to hold the markup.  (No markup can
                       ** be longer than 1000 characters, therefore.) */

  c = OneChar(in);
  while( c!=EOF ){
    if( isspace(c) ){
      /* Case 1: spaces 
      */
      zBuf[0] = c;
      i = 1;
      while( i<sizeof(zBuf)-2 && (c=OneChar(in))!=EOF && isspace(c) ){
        zBuf[i++] = c;
      }
      zBuf[i] = 0;
      if( xSpaceHandler ){
        (*xSpaceHandler)(zBuf,pArg);
      }
    }else if( c=='<' ){
      /* Case 2: markup
      **
      ** Begin by copying the complete markup into zBuf[]
      */
      int cQuote = 0;
      i = 0;
      while( (c=OneChar(in))!=EOF && (cQuote || c!='>') ){
        if( i<sizeof(zBuf)-2 ) zBuf[i++] = c;
        if( cQuote ){
          if( cQuote==c ) cQuote = 0;
        }else if( c=='"' || c=='\'' ){
          cQuote = c;
        }
      }
      if( c=='>' ) c = OneChar(in);
      zBuf[i] = 0;

      /* Now that the markup is completely in zBuf[], parse it up.
      **
      ** argv[0] will be a pointer to the name.  argv[1] will point to
      ** the first argument and argv[2] will point to the first argument's
      ** value.  argv[N/2+1] will point to the N-th argument and
      ** argv[N/2+2] will point to the N-th argument's value.
      **
      ** For example, in:      <img src="image.gif" alt="figure 1">
      ** we will get:
      **                   argv[0] = "img"
      **                   argv[1] = "src"
      **                   argv[2] = "image.gif"
      **                   argv[3] = "alt"
      **                   argv[4] = "figure 1"
      **                   argv[5] = NULL
      **
      ** Note that the argv[] list is NULL terminated.  Also note that
      ** argument values are stripped of quotation marks.
      */
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

      /* Now process the markup.  If there is a specific handler for
      ** this markup, use it.  Otherwise, call the default markup handler
      */
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
      /* Ordinary text.  This will get passed to the word handler.
      */
      zBuf[0] = c;
      i = 1;
      while( i<sizeof(zBuf)-2 && (c=OneChar(in))!=EOF && c!='<' && !isspace(c)){
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

/* Clear out the handler hash table.  Call this to erase all handlers other
** than the space, word and default markup handlers.
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

/* Install a new handler for specific markup.  zName is the name of the
** markup that is to be handled.  xFunc is a pointer to the routine to
** call whenever a markup named zName appears in the input.
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

/* The number of every heading and figure that has a tag= 
** argument is stored in an instance of the following structure.
** These structures are in turn stored in a hash table keyed
** on the tags.
*/
typedef struct sgTag Tag;
struct sgTag {
  char *zTag;      /* The value of the "tag=" field */
  char *zValue;    /* The heading number.  Ex:  1.5.3 */
  Tag *pCollide;   /* Next tag with the same hash */
};

/* The current heading counters are stored in the following
** array:
*/
static int HeadingIndex[6];
static int FigureIndex;

/* State information for the formatter
*/
static char zLine[1000];        /* Output line buffer */
static int nLine;               /* Next free slot in zLine */
static int lineLength;          /* Length of one line */
static int lineLengthDelta;     /* Change length of current line by this much */
static int pageWidth;           /* Width of the page */
static int indent;              /* Indentation amount */
static int indentDelta;         /* Change indent of current line by this much */
static int blankLinesWanted;    /* We want this many blank lines before the
                                ** beginning of the next line */
static int spacesWanted;        /* Put this many spaces on zLine[] before
                                ** adding more text */
static int preformatted;        /* Don't fill text if this is true */
static int suppressOutput;      /* True to block all output */
static int suppressNewline;     /* Suppress the next newline seen while in
                                ** preformat (<pre>) mode */
static int centerText;          /* If true, then center every output line */
#define HASH_SIZE 1009          /* Number of tag hash table slots */
static Tag *apTagHash[HASH_SIZE]; /* The hash table */
static int listDepth = 0;       /* Nested depth of <ul> or <ol> */
static int listCounter[20];     /* Counters for each nesting level */
static int inDL = 0;            /* We are inside of <dl>...</dl> */
static int inDT = 0;            /* Inside <dl>..</dl> we've seen a <dt> */
static int inDD = 0;            /* Inside <dl>..</dl> we've seen a <dd> */

/* Reset the paragraph and figure counters
*/
static void ResetCounters(void){
  int i;
  for(i=0; i<6; i++) HeadingIndex[i] = 0;
  FigureIndex = 0;
  lineLength = pageWidth;
  listDepth = 0;
  indent = indentDelta = lineLengthDelta = 0;
  nLine = 0;
  spacesWanted = 0;
  blankLinesWanted = 0;
}

/* Advance the heading counter due to the appearance of a new
** level N heading.
*/
static void NextHeading(int N){
  int i;
  if( N<1 ) N = 1;
  if( N>6 ) N = 6;
  N--;
  HeadingIndex[N]++;
  for(i=N+1; i<6; i++){
    HeadingIndex[i] = 0;
  }
  if( N==0 ){
    FigureIndex = 0;
  }
}

/* Fill the given string with the current heading number.
*/
static void HeadingNumber(char *zValue){
  int i;
  char *zSep = "";
  zValue[0] = 0;
  for(i=0; i<6 && HeadingIndex[i]>0; i++){
    sprintf(&zValue[strlen(zValue)],"%s%d",zSep,HeadingIndex[i]);
    zSep = ".";
  }
  if( HeadingIndex[1]==0 ){
    strcat(zValue,".");
  }
}

/* Advance the figure counter.
*/
static void NextFigure(void){
  FigureIndex++;
}

/* Fill this given string with the current figure number.
*/
static void FigureNumber(char *zValue){
  sprintf(zValue,"%d.%d",HeadingIndex[0],FigureIndex);
}

/* Compute the hash on a tag.
*/
static int Hash(const char *zTag){
  int h = 0;

  while( *zTag ){
    h = (h<<2) ^ h ^ *zTag++;
  }
  if( h<0 ) h = -h;
  h %= HASH_SIZE;
  return h;
}

/* Create a new tag-value pair
*/
static void NewTag(const char *zTag, const char *zValue){
  Tag *pNew = malloc( sizeof(Tag) + strlen(zTag) + strlen(zValue) + 2 );
  int h;
  if( pNew==0 ){
    fprintf(stderr,"Out of memory...\n");
    exit(1);
  }
  pNew->zTag = (char*)&pNew[1];
  pNew->zValue = &pNew->zTag[strlen(zTag)+1];
  strcpy(pNew->zTag,zTag);
  strcpy(pNew->zValue,zValue);
  h = Hash(zTag);
  pNew->pCollide = apTagHash[h];
  apTagHash[h] = pNew;
}

/* Return the value associated with the given tag.  Return "???" if
** there is no such tag.
*/
static char *FindValue(const char *zTag){
  int h;
  Tag *pTag;
  h = Hash(zTag);
  for(pTag=apTagHash[h]; pTag; pTag=pTag->pCollide){
    if( strcmp(pTag->zTag,zTag)==0 ){
      return pTag->zValue;
    }
  }
  return "???";
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
  level = argv[0][1] - '0';
  NextHeading(level);
  for(i=1; argv[i]; i+=2){
    if( stricmp(argv[i],"tag")==0 ){
      char zValue[200];
      HeadingNumber(zValue);
      NewTag(argv[i+1],zValue);
    }
  }
}

/* Processing all <image> marks on the first pass.
**
** There is no output during the first pass.  The only thing that
** happens is the construction of data structures.
*/
static void Pass1Image(int argc, const char **argv, void *NotUsed){
  int i;
  const char *zTag = 0;
  const char *zCaption = 0;

  for(i=1; argv[i]; i+=2){
    if( stricmp(argv[i],"tag")==0 ){
      zTag = argv[i+1];
    }else if( stricmp(argv[i],"caption")==0 ){
      zCaption = argv[i+1];
    }
  }
  if( zCaption ){
    NextFigure();
    if( zTag ){
      char zValue[200];
      FigureNumber(zValue);
      NewTag(zTag,zValue);
    }
  }
}

/* Flush the output line buffer
*/
static void FlushBuffer(void){
  int i;
  if( nLine==0 ) return;
  for(i=0; i<blankLinesWanted; i++){
    printf("\n");
  }
  blankLinesWanted = 0;
  zLine[nLine] = 0;
  if( centerText ){
    i = (lineLength + lineLengthDelta - nLine)/2;
    printf("%*s%.*s\n",indent+indentDelta+i,"",nLine,zLine);
  }else{
    printf("%*s%.*s\n",indent+indentDelta,"",nLine,zLine);
  }
  indentDelta = 0;
  lineLengthDelta = 0;
  nLine = 0;
  spacesWanted = 0;
  suppressNewline = 0;
}

/* Add text to the output buffer.
*/
static void Pass2Word(const char *zText, void *NotUsed){
  int len;
  if( suppressOutput ) return;
  if( preformatted ){
    while( *zText && nLine<sizeof(zLine)-1 ){
      zLine[nLine++] = *zText++;
    }
    suppressNewline = 0;
    return;
  }
  len = strlen(zText);
  if( spacesWanted ){
    if( nLine + spacesWanted + len > lineLength + lineLengthDelta ){
      FlushBuffer();
    }
  }else{
    if( nLine + len > lineLength + lineLengthDelta + 2 ){
      int i = nLine-1;
      while( i>0 && zLine[i]!=' ' && zLine[i]!='-' ){ i--; }
      if( i==0 ){
        FlushBuffer();
      }else{
        int old_nLine = nLine;
        i++;
        nLine = i;
        FlushBuffer();
        while( i<old_nLine ){ zLine[nLine++] = zLine[i++]; }
      }
    }
  }
  while( spacesWanted ){
    if( nLine>0 && nLine < sizeof(zLine)-1 ){
      zLine[nLine++] = ' ';
    }
    spacesWanted--;
  }
  while( *zText && nLine<sizeof(zLine)-1 ){
    zLine[nLine++] = *zText++;
  }
}

/* Add space to the output buffer
*/
static void Pass2Space(const char *zText, void *NotUsed){
  int i, limit;
  if( suppressOutput ) return;
  if( preformatted ){
    while( *zText && nLine<sizeof(zLine)-1 ){
      if( *zText=='\n' ){
        if( suppressNewline ){
          suppressNewline = 0;
          nLine = 0;
        }else if( nLine > 0 ){
          FlushBuffer();
        }else{
          printf("\n");
        }
        zText++;
      }else{
        zLine[nLine++] = *zText++;
      }
    }
    return;
  }
  if( spacesWanted ) return;

  /* The following is a set of heuristics that attempt to determine
  ** if one or two spaces are appropriate.  */
  i = nLine-1;
  if( i>=0 && zLine[i]!='.' ){
    spacesWanted = 1;
    return;
  }
  i--;
  limit = i-2;
  if( limit<0 ) limit = 0;
  while( i>=limit && zLine[i]!=' ' ){
    if( zLine[i]=='.' || isupper(zLine[i]) ){
      spacesWanted = 1;
      return;
    }
    i--;
  }
  spacesWanted = 2;
}

/* Processing all <hN> marks on the second pass.  This is where
** the output occurs.
*/
static void Pass2Heading(int argc, const char **argv, void *NotUsed){
  int level;
  char zValue[100];

  preformatted = suppressOutput = 0;
  listDepth = 0;
  if( argv[0][1]<'1' || argv[0][1]>'6' ) return;
  level = argv[0][1] - '0';
  NextHeading(level);
  HeadingNumber(zValue);
  FlushBuffer();
  if( level==1 ){
    blankLinesWanted = 2;
  }else{
    blankLinesWanted = 1;
  }
  if( level<=2 ){
    indent = strlen(zValue) + 1 + (level==1);
    indentDelta = -indent;
    lineLength = pageWidth - indent;
    lineLengthDelta = indent;
  }else{
    indent = indentDelta = lineLengthDelta = 0;
    lineLength = pageWidth;
  }
  Pass2Word(zValue,0);
  Pass2Space("",0);
}

/* </hN> in pass 2.
*/
static void Pass2EndHeading(int argc, const char **argv, void *NotUsed){
  int level;

  if( argv[0][2]<'1' || argv[0][2]>'6' ) return;
  level = argv[0][2] - '0';
  if( level<=2 ){
    FlushBuffer();
    indent = indentDelta = lineLengthDelta = 0;
    lineLength = pageWidth;
    blankLinesWanted = 1;
  }else{
    while( nLine>=0 && isspace(zLine[nLine-1]) ){ nLine--; }
    if( nLine>=0 && zLine[nLine-1]!='.' ){
      Pass2Word(".",0);
      spacesWanted = 2;
    }
  }
}

/* Turn text suppression on or off
*/
static void SuppressionOn(int argc, const char **argv, void *NotUsed){
  suppressOutput = 1;
}
static void SuppressionOff(int argc, const char **argv, void *NotUsed){
  suppressOutput = 0;
}

/* Deal with a <ref tag=TAG> directive.  This is translated into
** pure text.
*/
static void Ref(int argc, const char **argv, void *NotUsed){
  int i, len;
  char zValue[100];
  for(i=1; argv[i]; i+=2){
    if( stricmp(argv[i],"tag")==0 ){
      sprintf(zValue,"%.*s",(int)sizeof(zValue)-1,FindValue(argv[i+1]));
      len = strlen(zValue);
      if( len>0 && zValue[len-1]=='.' ) zValue[len-1] = 0;
      Pass2Word(zValue,0);
    }
  }
}

/* Deal with the <image> markup.
**
** If there is a caption= field, then we have a figure.  Without caption=, it
** is a simple image.
**
** Because we have ASCII output, use the "alt=" field for the image.
*/
static void Pass2Image(int argc, const char **argv, void *NotUsed){
  const char *zAlt = "**Image**";
  const char *zTag = 0;
  const char *zCaption = 0;
  int i;

  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"alt")==0 ){
      zAlt = argv[i+1];
    }else if( stricmp(argv[i],"tag")==0 ){
      zTag = argv[i+1];
    }else if( stricmp(argv[i],"caption")==0 ){
      zCaption = argv[i+1];
    }
  }
  if( zCaption ){
    char zValue[100];
    NextFigure();
    FigureNumber(zValue);
    FlushBuffer();
    blankLinesWanted = 1;
    centerText = 1;
    Pass2Word(zAlt,0);
    FlushBuffer();
    Pass2Word(zValue,0);
    Pass2Space("",0);
    Pass2Word(zCaption,0);
    FlushBuffer();
    blankLinesWanted = 1;
    centerText = 0;
  }else{
    Pass2Word(zAlt,0);
  }
}

/* Process the <quote> and </quote> markups
*/
void Quote(int argc, const char **argv, void *NotUsed){
  Pass2Word("\"",0);
}

/* Process the <br> markup
*/
void Break(int argc, const char **argv, void *NotUsed){
  FlushBuffer();
}

/* Process the <p> markup
*/
void Paragraph(int argc, const char **argv, void *NotUsed){
  FlushBuffer();
  blankLinesWanted = 1;
}

/* Process the <pre> and </pre> markup
*/
void Preformat(int argc, const char **argv, void *NotUsed){
  Paragraph(argc,argv,0);
  preformatted = 1;
  suppressNewline = 1;
}
void EndPreformat(int argc, const char **argv, void *NotUsed){
  Paragraph(argc,argv,0);
  preformatted = 0;
}

/* Process the <blockquote> and </blockquote> markup
*/
void BlockQuote(int argc, const char **argv, void *NotUsed){
  Paragraph(argc,argv,0);
  indent += 5;
  lineLength -= 10;
}
void EndBlockQuote(int argc, const char **argv, void *NotUsed){
  Paragraph(argc,argv,0);
  indent -= 5;
  if( indent<0 ) indent = 0;
  lineLength += 10;
  if( lineLength>pageWidth ) lineLength = pageWidth;
}

/* Process <ul>..</ul> or <ol>..</ol>
*/
void List(int argc, const char **argv, void *NotUsed){
  listDepth++;
  if( listDepth>sizeof(listCounter) ) return;
  Paragraph(argc,argv,0);
  if( argv[0][0]=='o' || argv[0][0]=='O' ){
    listCounter[listDepth-1] = 0;
  }else{
    listCounter[listDepth-1] = -1;
  }
  indent += 5;
  lineLength -= 5;
}
void EndList(int argc, const char **argv, void *NotUsed){
  if( listDepth<=0 ) return;
  if( listDepth>sizeof(listCounter) ){
    listDepth--;
    return;
  }
  Paragraph(argc,argv,0);
  indent -= 5;
  lineLength += 5;
  listDepth--;
}
void ListItem(int argc, const char **argv, void *NotUsed){
  char zLabel[10];
  if( listDepth==0 || listDepth>sizeof(listCounter) ) return;
  FlushBuffer();
  indentDelta = -5;
  lineLengthDelta = +5;
  if( listCounter[listDepth-1]<0 ){
    sprintf(zLabel,"  * "); 
  }else{
    sprintf(zLabel,"%3d.",++listCounter[listDepth-1]);
  }
  Pass2Word(zLabel,0);
  spacesWanted = 1;
}

void DefList(int argc, const char **argv, void *NotUsed){
  if( inDL ) return;
  inDL = 1;
  inDD = 1;
  Paragraph(argc,argv,0);
  indent += 15;
  lineLength -= 15;
}
void DefListTag(int argc, const char **argv, void *NotUsed){
  if( !inDL ) return;
  FlushBuffer();
  if( inDD ){
    indent -= 10;
    lineLength += 10;
    inDD = 0;
  }
  inDT = 1;
}
void DefListData(int argc, const char **argv, void *NotUsed){
  if( !inDL ) return;
  FlushBuffer();
  if( inDT ){
    indent += 10;
    lineLength -= 10;
    inDT = 0;
  }
  inDD = 1;
}
void EndDefList(int argc, const char **argv, void *NotUsed){
  if( !inDL ) return;
  Paragraph(argc,argv,0);
  if( inDD ){
    indent -= 15;
    lineLength += 15;
  }else{
    indent -= 5;
    lineLength += 5;
  }
  inDL = inDD = inDT = 0;
}

/* The </func> markup adds a "()" to the end of a function name
*/
void EndFunction(int argc, const char **argv, void *NotUsed){
  Pass2Word("()",0);
}

/* Ignore this markup
*/
void Ignore(int argc, const char **argv, void *NotUsed){
  /* Do nothing */
}

static void Usage(char **argv){
  fprintf(stderr,"Usage: %s filename ...\n",argv[0]);
  exit(1);
}

/* This structure records all tags and their associated action routines
** for the second pass thru the file.
*/
static struct sgPass2 {
  char *zTag;
  void (*xFunc)(int,const char**,void*);
} aPass2[] = {
  { "h1", Pass2Heading },
  { "h2", Pass2Heading },
  { "h3", Pass2Heading },
  { "h4", Pass2Heading },
  { "h5", Pass2Heading },
  { "h6", Pass2Heading },
  { "/h1", Pass2EndHeading },
  { "/h2", Pass2EndHeading },
  { "/h3", Pass2EndHeading },
  { "/h4", Pass2EndHeading },
  { "/h5", Pass2EndHeading },
  { "/h6", Pass2EndHeading },
  { "ref", Ref },
  { "title",  SuppressionOn },
  { "/title", SuppressionOff },
  { "image",Pass2Image},
  { "latex",SuppressionOn},
  { "/latex",SuppressionOff},
  { "html",SuppressionOn},
  { "/html",SuppressionOff},
  { "quote",Quote},
  { "/quote",Quote},
  { "blockquote",BlockQuote},
  { "/blockquote",EndBlockQuote},
  { "ul",List},
  { "/ul",EndList},
  { "ol",List},
  { "/ol",EndList},
  { "li",ListItem},
  { "p",Paragraph},
  { "/p",Paragraph},
  { "br",Break},
  { "pre",Preformat},
  { "/pre",EndPreformat},
  { "dl",DefList},
  { "dd",DefListData},
  { "dt",DefListTag},
  { "/dl",EndDefList},
  { "func",Ignore},
  { "/func",EndFunction},
  { "cmd",Ignore},
  { "/cmd",Ignore},
  { "code",Ignore},
  { "/code",Ignore},
  { "em",Ignore},
  { "/em",Ignore},
  { "strong",Ignore},
  { "/strong",Ignore},
  { "file",Ignore},
  { "/file",Ignore},
  { "var",Ignore},
  { "/var",Ignore},
};

void main(int argc, char **argv){
  FILE *pIn;
  int i;

  if( argc<2 ){
    Usage(argv);
  }
  SgmlWordHandler(0);
  SgmlSpaceHandler(0);
  SgmlDefaultMarkupHandler(0);
  SgmlHandler("h1",Pass1Heading);
  SgmlHandler("h2",Pass1Heading);
  SgmlHandler("h3",Pass1Heading);
  SgmlHandler("h4",Pass1Heading);
  SgmlHandler("h5",Pass1Heading);
  SgmlHandler("h6",Pass1Heading);
  SgmlHandler("image",Pass1Image);
  pageWidth = 70;
  ResetCounters();
  for(i=1; i<argc; i++){
    if( sscanf(argv[i],"width=%d",&pageWidth)==1 ) continue;
    pIn = fopen(argv[i],"r");
    if( pIn==0 ){
      char zErr[100];
      sprintf(zErr,"Can't open \"%.*s\"",(int)sizeof(zErr)-40,argv[i]);
      perror(zErr);
      exit(1);
    }
    SgmlParse(pIn,0);
    fclose(pIn);
  }

  ResetCounters();
  SgmlHandlerReset();
  SgmlWordHandler(Pass2Word);
  SgmlSpaceHandler(Pass2Space);
  SgmlDefaultMarkupHandler(0);
  for(i=0; i<sizeof(aPass2)/sizeof(aPass2[0]); i++){
    SgmlHandler(aPass2[i].zTag,aPass2[i].xFunc);
  }
  pageWidth = 70;
  for(i=1; i<argc; i++){
    if( sscanf(argv[i],"width=%d",&pageWidth)==1 ) continue;
    pIn = fopen(argv[i],"r");
    if( pIn==0 ){
      char zErr[100];
      sprintf(zErr,"Can't open \"%.*s\"",(int)sizeof(zErr)-40,argv[i]);
      perror(zErr);
      exit(1);
    }
    SgmlParse(pIn,0);
    fclose(pIn);
  }
  FlushBuffer();
  exit(0);
}
