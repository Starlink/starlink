/*
** The program coded by this file reads a single SGML source document
** in the DRH format and translates it into the O'Reilly format.
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
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
**         &nbsp;         nonbreakable space
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
  { "nbsp",      '~',        0 },
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

/* The number of errors seen
*/
int errcnt = 0;

/* These two routines make sure that every <para> has a corresponding
** </para>.
*/
static int in_para = 0;
void EndPara(void){
  if( in_para ) printf("</para>");
  in_para = 0;
}
void BeginPara(void){
  EndPara();
  printf("<para>");
  in_para = 1;
}

/* This routine makes sure that there are appropriate pairs of
** <chapter> and <sectN> labels.
*/
void EnterSection(int level){
  static int in_sect[6];
  int i;
  level--;
  for(i=6; i>=0 && i>=level; i--){
    if( in_sect[i] ){
      printf("</sect%d>\n",i+1);
    }
    in_sect[i] = 0;
  }
  if( level>=0 ) in_sect[level] = 1;
}

/* Set this variable to TRUE to suppress all output.
*/
static int quiet = 0;

/* This routine is called to handle all white-space
*/
void HandleSpace(const char *zSpace, void *pDummy){
  if( !quiet ) printf("%s",zSpace);
}

/* This routine is called to handle all non-whitespace in the original
** DOC file.
*/
void HandleWord(const char *zWord, void *pDummy){
  if( quiet ) return;
  while( *zWord ){
    switch( *zWord ){
      case '<':  printf("&lt;"); break;
      case '>':  printf("&gt;"); break;
      case '&':  printf("&amp;"); break;
      default:   putchar(*zWord); break;
    }
    zWord++;
  }
}

/* Processing all <h1>, <h2>, ..., <h6> markups.
*/
void Heading(int argc, const char **argv, void *pDummy){
  int level;
  int i;
  
  level = argv[0][1] - '0';
  if( level<1 || level>6 ) return;
  EndPara();
  EnterSection(level);
  printf("<sect%d",level);
  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"tag")==0 ){
      printf(" id=\"%s\"",argv[i+1]);
    }
  }
  printf(">\n<title>");
}

/* Process all </h1>, </h2>, ..., </h6> markups.
*/
void EndHeading(int argc, const char **argv, void *pDummy){
  printf("</title>");
}

/* Process the <br> markup
*/
void Break(int argc, const char **argv, void *pDummy){
  printf("<linebreak>");
}

/* Process the <p> markup
*/
void Paragraph(int argc, const char **argv, void *pDummy){
  BeginPara();
}
void EndParagraph(int argc, const char **argv, void *pDummy){
  EndPara();
}

/* Process the <pre> markup
*/
void Preformat(int argc, const char **argv, void *pDummy){
  printf("<screen>");
}

/* Process the </pre> markup
*/
void EndPreformat(int argc, const char **argv, void *pDummy){
  printf("</screen>");
}

/* The <latex> directive works a lot like <pre>.  Between <latex> and
** </latex> everything is copied verbatim.  But, no code is issued
** to turn on \begin{verbatim}...\end{verbatim} in the output.
*/
void Latex(int argc, const char **argv, void *pDummy){
}
void EndLatex(int argc, const char **argv, void *pDummy){
}


/* Process the <ol> markup
*/
void OrderedList(int argc, const char **argv, void *pDummy){
  printf("<numberedlist>");
}

/* Process the </ol> markup
*/
void EndOrderedList(int argc, const char **argv, void *pDummy){
  printf("</numberedlist>");
}

/* Process the <ul> markup
*/
void BulletList(int argc, const char **argv, void *pDummy){
  printf("<itemizedlist>");
}

/* Process the </ul> markup
*/
void EndBulletList(int argc, const char **argv, void *pDummy){
  printf("</itemizedlist>");
}

/* Process the <li> markup
*/
void ListItem(int argc, const char **argv, void *pDummy){
  printf("<listitem>");
}

/* Ignore all text until we call On().  (This is actually used
** in order to ignore the text contained in <title>...</title>)
*/
void Off(int argc, const char **argv, void *pDummy){
  quiet = 1;
}

/* Stop ignoring text
*/
void On(int argc, const char **argv, void *pDummy){
  quiet = 0;
}

/* Process the <dl> markup
*/
void DefList(int argc, const char **argv, void *pDummy){
}

/* Process the <dt> markup
*/
void DefTag(int argc, const char **argv, void *pDummy){
}

/* Process the <dd> markup
*/
void DefData(int argc, const char **argv, void *pDummy){
}

/* Process the </dl> markup
*/
void EndDefList(int argc, const char **argv, void *pDummy){
}

/* Process the <em> markup
*/
void Emphasize(int argc, const char **argv, void *pDummy){
  printf("<emphasis>");
}

/* Process the </em> markup
*/
void EndEmphasize(int argc, const char **argv, void *pDummy){
  printf("</emphasis>");
}

/* Process the <strong> markup
*/
void Strong(int argc, const char **argv, void *pDummy){
  printf("<emphasis>");
}

/* Process the </strong> markup
*/
void EndStrong(int argc, const char **argv, void *pDummy){
  printf("<emphasis>");
}

/* Process the <code> and </code> markup
*/
void Code(int argc, const char **argv, void *pDummy){
  printf("<literal>");
}
void EndCode(int argc, const char **argv, void *pDummy){
  printf("</literal>");
}

/* Process the <func> and </func> markup
*/
void Func(int argc, const char **argv, void *pDummy){
  /* printf("<emphasis>"); */
  printf("<literal>");
}
void EndFunc(int argc, const char **argv, void *pDummy){
 /*  printf("(<?troff \\|>)</emphasis>"); */
  printf("<<?troff \\|)>)</literal>");
}

/* Process the <var> and </var> markup
*/
void Var(int argc, const char **argv, void *pDummy){
  printf("<replaceable>");
}
void EndVar(int argc, const char **argv, void *pDummy){
  printf("</replaceable>");
}

/* Process the <cmd> and </cmd> markup
*/
void Command(int argc, const char **argv, void *pDummy){
  printf("<command>");
}
void EndCommand(int argc, const char **argv, void *pDummy){
  printf("</command>");
}

/* Process the <file> and </file> markup
*/
void Filename(int argc, const char **argv, void *pDummy){
  printf("<filename>");
}
void EndFilename(int argc, const char **argv, void *pDummy){
  printf("</filename>");
}

/* Process the <blockquote> markup
*/
void BlockQuote(int argc, const char **argv, void *pDummy){
}

/* Process the </blockquote> markup
*/
void EndBlockQuote(int argc, const char **argv, void *pDummy){
}

/* Process the <image eps=FILE width=WIDTH tag=TAG caption=TEXT> markup.
**
** The eps= field is required.  It specifies the name of a file containing
** the postscript image to be imported.  The width= field is optional.
** if present, it specifies that the image should be scaled to the
** given width (the height is change proporationally.)  The WIDTH should
** be an integer or floating-point value followed by a unit.  Units can
** be  "cm" (centimeters), "in" (inches), "em" (ems -- the with of an M 
** character), or "pt" (printer's points).
*/
void Image(int argc, const char **argv, void *pDummy){
  int i;
  const char *zImage = 0;
  const char *zWidth = 0;
  const char *zTag = 0;
  const char *zCaption = 0;

  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"eps")==0 ){
      zImage = argv[i+1];
    }else if( stricmp(argv[i],"width")==0 ){
      zWidth = argv[i+1];
    }else if( stricmp(argv[i],"tag")==0 ){
      zTag = argv[i+1];
    }else if( stricmp(argv[i],"caption")==0 ){
      zCaption = argv[i+1];
    }
  }
  if( zTag ){
    printf("<figure id=\"%s\">\n",zTag);
  }else{
    printf("<figure>\n");
  }
  if( zCaption ){
    printf("<title>%s</title>\n",zCaption);
  }
  if( zImage ){
    printf("<graphic format=\"EPS\" fileref=\"%s\"></graphic>\n",zImage);
  }
}

/* Process the <index text=TEXT> markup
*/
void Index(int argc, const char **argv, void *pDummy){
}

/* Process the <ref tag=TAG> markup
*/
void Ref(int argc, const char **argv, void *pDummy){
  int i;
  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"tag")==0 ){
      printf("<xref id=\"%s\">",argv[i+1]);
      break;
    }
  }
}

/* Process the <quote> and </quote> markups
*/
void Quote(int argc, const char **argv, void *NotUsed){
  printf("``");
}
void EndQuote(int argc, const char **argv, void *NotUsed){
  printf("''");
}

/* Process the <title> and </title> markups
*/
void Title(int argc, const char **argv, void *NotUsed){
  printf("<%s>",argv[0]);
}

/* A table of markups and the routines to handle them
*/
static struct {
  char *zMark;
  void (*xHandler)(int, const char**, void*);
} markups[] = {
  { "h1",      Heading },
  { "h2",      Heading },
  { "h3",      Heading },
  { "h4",      Heading },
  { "h5",      Heading },
  { "h6",      Heading },
  { "/h1",     EndHeading },
  { "/h2",     EndHeading },
  { "/h3",     EndHeading },
  { "/h4",     EndHeading },
  { "/h5",     EndHeading },
  { "/h6",     EndHeading },
  { "br",      Break },
  { "p",       Paragraph },
  { "/p",      EndParagraph },
  { "pre",     Preformat },
  { "/pre",    EndPreformat },
  { "ol",      OrderedList },
  { "ul",      BulletList },
  { "/ol",     EndOrderedList },
  { "/ul",     EndBulletList },
  { "li",      ListItem },
  { "title",   Title },
  { "/title",  Title },
  { "html",    Off },
  { "/html",   On },
  { "latex",   Latex },
  { "/latex",  EndLatex },
  { "dl",      DefList },
  { "/dl",     EndDefList },
  { "dt",      DefTag },
  { "dd",      DefData },
  { "em",      Emphasize },
  { "/em",     EndEmphasize },
  { "image",   Image },
  { "index",   Index },
  { "ref",     Ref },
  { "blockquote",  BlockQuote },
  { "/blockquote", EndBlockQuote },
  { "strong",  Strong },
  { "/strong", EndStrong },
  { "quote",   Quote },
  { "/quote",  EndQuote },
  { "code",    Code },
  { "/code",   EndCode },
  { "cmd",     Command },
  { "/cmd",    EndCommand },
  { "filename",    Filename },
  { "/filename",   EndFilename },
  { "func",    Func },
  { "/func",   EndFunc },
  { "var",     Var },
  { "/var",    EndVar },
  
};

/* Call this function to parse the complete SGML file whose
** input stream is pIn
*/
static void ParseWholeFile(FILE *pIn){
  static int sgmlNeedsInit = 1;
  if( sgmlNeedsInit ){
    int i;
    SgmlWordHandler(HandleWord);
    SgmlSpaceHandler(HandleSpace);
    SgmlDefaultMarkupHandler(0);
    for(i=0; i<sizeof(markups)/sizeof(markups[0]); i++){
      SgmlHandler(markups[i].zMark,markups[i].xHandler);
    }
    sgmlNeedsInit = 0;
  }
  printf("<chapter>\n");
  SgmlParse(pIn,0);
  EnterSection(0);
  printf("</chapter>\n");
}

int main(int argc, char **argv){
  if( argc==1 ){
    ParseWholeFile(stdin);
  }else if( argc==2 ){
    FILE *pIn = fopen(argv[1],"r");
    if( pIn==0 ){
      char zErrMsg[1000];
      sprintf(zErrMsg,"Can't open \"%.*s\"",(int)sizeof(zErrMsg)-30,argv[1]);
      perror(zErrMsg);
      exit(1);
    }
    ParseWholeFile(pIn);
    fclose(pIn);
  }else{
    fprintf(stderr,"Usage: %s [FILENAME]\n",argv[0]);
    exit(1);
  }
  return errcnt;
}
