/*
** The program coded by this file reads a single HTML file and
** prints a single RTF file contain the same information.
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#if !defined(__WIN32__) && !defined(__WIN32)
#define stricmp strcasecmp
#endif

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
  char *value;     /* The value for this sequence.       ex:  "&" */
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
  { "amp",       "&",        0 },
  { "lt",        "<",        0 },
  { "gt",        ">",        0 },
  { "nbsp",      "\\~",      0 },
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
        char *x = p->value;
        while( *x) z[to++] = *x++;
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
static void (*xSpaceHandler)(const char*);
static void (*xWordHandler)(const char*);
static void (*xDefaultMarkupHandler)(int, const char**);

/* Each specific SGML markup handler is stored in a hash table as an 
** instance of the following structure.
*/
typedef struct sgHandler Handler;
struct sgHandler {
  char *zName;                                /* Name of markup to handle */
  void (*xHandler)(int, const char**);        /* Routine to do the work */
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
  FILE *in            /* Read input from this stream */
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
        (*xSpaceHandler)(zBuf);
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
          (*pHandler->xHandler)(argc,(const char**)argv);
        }
      }else if( xDefaultMarkupHandler ){
        (*xDefaultMarkupHandler)(argc,(const char**)argv);
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
        (*xWordHandler)(zBuf);
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
void SgmlHandler(const char *zName, void (*xFunc)(int,const char**)){
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
void SgmlWordHandler(void (*xWord)(const char*)){
  xWordHandler = xWord;
}
void SgmlSpaceHandler(void (*xSpace)(const char*)){
  xSpaceHandler = xSpace;
}
void SgmlDefaultMarkupHandler(void (*xMarkup)(int,const char**)){
  xDefaultMarkupHandler = xMarkup;
}
/****************** End of SGML Parser Code **********************/

/* The number of \par marks that have been emitted since the last
** non-whitespace character was emitted */
static int nPar = 0;

/* The number of spaces that have been emitted since the last
** non-whitespace character was emitted */
static int nSpace = 0;

/* True if we are in the preformatted text mode.  In the preformatted mode,
** all white-space is preserved. */
static int inPreformat = 0;

/* True if we have just entered a <PRE> tag and haven't yet seen an
** end-of-line character. */
static int inPreformatStart = 0;

/* The current depth of nested <UL> or <OL> tags.  If we are not in any
** list, the current depth is 0.  The maximum depth is MAXDEPTH. */
#define MAXDEPTH 100       /* Essentially limitless... */
static int iDepth = 0;

/* For each nested list, record the current count value.  A count of 0
** indicates that we are in an bullet list.  The count is the value to
** be used for the next list item. */
static int aiCount[MAXDEPTH];

/* Number of {'s in the emitted code.  We keep track up this so that we
** can be sure to emit exactly the right number of matching }'s in the
** emitted RTF. */
static int nBrace = 0;

/* This is true if we are currently within a paragraph that has an
** alignment tag.  Ex:   <P ALIGN=CENTER>. */
static int inAlignedParagraph = 0;

/* Size of the current font */
static int szFont = 10;

/* Each level of indention is by this many ``twips''.  There are 1440 twips
** to one inch. */
#define INDENT 720

/* The bullet or number on a list element is indented to the left by
** this amount. */
#define UNINDENT 360

/* Print information that appears at the beginning of every RTF file
*/
static void EmitRtfHeader(void){
  static char zHeader[] = 
    "{\\rtf1\\ansi"      /* This is an RTF document */
    "\\deff1"            /* The default font is f1 */
    "\\deflang1033"      /* The language is U.S. English */
    "{\\fonttbl"         /* List of all fonts used */
       "{\\f1\\froman\\fcharset0\\fprq2 Times New Roman;}"  /* Most text */
       "{\\f2\\froman\\fcharset2\\fprq2 Symbol;}"           /* For bullets */
       "{\\f3\\fmodern\\fcharset0\\fprq1 Pica;}"            /* For TTY font */
    "}"
    "\\f1\\fs20\\widctlpar\n"       /* Begin by using F1 at 10pt */
  ;
  printf("%s",zHeader);
  nPar = 3;
  nSpace = 1;
  inPreformat = 0;
  inAlignedParagraph = 0;
}

/* Print information that appears at the end of every RTF file.  This
** routine also makes sure that all prior { characters get a matching
** } character.
*/
static void EmitRtfTrailer(void){

  /* This is the text to put at the end of the file */
  static char zTrailer[] = "\n}\n";

  /* Before adding the trailer text, output any additional } characters
  ** needed to balance all { characters.  This will only happen if the
  ** original HTML contains unterminated markup, such as a <EM> without
  ** a corresponding </EM>. */
  while( nBrace>0 ){
    putchar('}');
    nBrace--;
  }

  /* Now print the trailer. */
  printf("%s",zTrailer);
}

/* This routine is called for every block of non-whitespace text in the
** original HTML document.
*/
static void Word(const char *zWord){
  if( !inPreformat && nPar>0 && iDepth>0 ){
    printf("\\tab ");
  }
  while( *zWord ){
    switch( *zWord ){
      case '\\':
      case '}':
      case '{':
        putchar('\\');
        break;
      default:
        break;
    }
    putchar(*zWord);
    zWord++;
  }
  nSpace = nPar = 0;
}

/* This routine is called for every block of white-space in the original
** HTML document 
*/
static void Space(const char *zSpace){
  if( inPreformat ){
    while( *zSpace ){
      switch( *zSpace ){
        case ' ':
        case '\t':
          if( !inPreformatStart ){ putchar(' '); }
          break;

        case '\n':
          if( !inPreformatStart ){ printf("\\par\n"); }
          inPreformatStart = 0;
          break;

        default:
          break;
      }
      zSpace++;
    }
    return;
  }
  if( nSpace ) return;
  putchar(' ');
  nPar = 0;
  nSpace = 1;
}

/* This routine is called at the end of a paragraph
*/
static void EndParagraph(int argc, const char **argv){
  while( nPar<2 ){
    printf("\\par\n");
    nPar++;
  }
  nSpace = 1;
  if( inAlignedParagraph ){
    if( nBrace>0 ){
      printf("}");
      nBrace--;
    }
    inAlignedParagraph = 0;
  }
}

/* This routine is called for every P tag
*/
static void Paragraph(int argc, const char **argv){
  char *zAlign = 0;   /* The alignment tag for this paragraph */
  int i;              /* Loop counter */

  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"align")==0 ){
      if( stricmp(argv[i+1],"center")==0 ){
        zAlign = "\\qc";
      }else if( stricmp(argv[i+1],"right")==0 ){
        zAlign = "\\qr";
      }else{
        zAlign = 0;
      }
    }
  }
  EndParagraph(0,0);
  if( zAlign ){
    printf("{%s ",zAlign);
    nBrace++;
    inAlignedParagraph = 1;
  }
}

/* This routine is called for every BR tag
*/
static void LineBreak(int argc, const char **argv){
  while( nPar<1 ){
    printf("\\par\n");
    nPar++;
  }
  nSpace = 1;
}

/* This routine is called for every H1, H2, H3, H4, H5 or H6 tag in the
** original HTML document.
*/
static void Heading(int argc, const char **argv){
  int level = argv[0][1] - '0';
  int fontSize;
  EndParagraph(0,0);
  if( level==1 && nPar<3 ){
    printf("\\par\n");
  }
  switch( level ){
    case 1:  fontSize = 36;  break;
    case 2:  fontSize = 28;  break;
    case 3:  fontSize = 24;  break;
    case 4:  fontSize = 20;  break;
    default: fontSize = 16;  break;
  }
  printf("{\\b\\fs%d ",fontSize);
  nBrace++;
}

/* This routine is called for every /H1, /h2, /h3, /h4, /h5 or /h6 tag in
** the original HTML document
*/
static void EndHeading(int argc, const char **argv){
  int level = argv[0][2] - '0';
  if( nBrace>0 ){
    printf("}");
    nBrace--;
  }
  if( level<4 ){
    Paragraph(0,0);
  }else{
    printf(".  ");
  }
}

/* This routine is called in order to cause the translator to begin
** ignoring the HTML text.
*/
static void Ignore(int argc, const char **argv){
  printf("{\\comment ");
  nBrace++;
}

/* This routine is called in order to end a context.
*/
static void End(int argc, const char **argv){
  if( nBrace>0 ){
    printf("}");
    nBrace--;
  }
}

/* Begin italic text
*/
static void Italic(int argc, const char **argv){
  printf("{\\i ");
  nBrace++;
}

/* Begin bold text
*/
static void Bold(int argc, const char **argv){
  printf("{\\b ");
  nBrace++;
}

/* Begin fixed-width font text
*/
static void FixedFont(int argc, const char **argv){
  printf("{\\f3\\fs%d ",(szFont-1)*2);
  nBrace++;
}

/* Begin centering text
*/
static void Center(int argc, const char **argv){
  printf("{\\qc\n");
  nBrace++;
}

/* Begin superscripted text
*/
static void Superscript(int argc, const char **argv){
  printf("{\\super ");
  nBrace++;
}

/* Begin subscripted text
*/
static void Subscript(int argc, const char **argv){
  printf("{\\sub ");
  nBrace++;
}

/* Make all subsequent text a little larger */
static void Big(int argc, const char **argv){
  szFont += 2;
  printf("{\\fs%d ",szFont*2);
  nBrace++;
}

/* This routine undoes the work of Big() */
static void EndBig(int argc, const char **argv){
  szFont -= 2;
  End(0,0);
}

/* Make all subsequent text a little smaller */
static void Small(int argc, const char **argv){
  if( szFont>4 ){
    szFont -= 2;
  }
  printf("{\\fs%d ",szFont*2);
  nBrace++;
}

/* This routine undoes the work of Small() */
static void EndSmall(int argc, const char **argv){
  szFont += 2;
  End(0,0);
}

/* This routine is called to handle every <PRE> tag in the
** original HTML
*/
static void Preformat(int argc, const char **argv){
  int indent;
  Paragraph(0,0);
  indent = iDepth*INDENT + INDENT/2;
  printf("{\\f3\\fs%d\\li%d\\fi0\\ri-14400\n",(szFont-1)*2,indent);
  inPreformatStart = 1;
  inPreformat = 1;
  nBrace++;
}

/* This routine is called to handle every </PRE> tag in the original HTML */
static void EndPreformat(int argc, const char **argv){
  inPreformat = 0;
  End(0,0);
  Paragraph(0,0);
}

/* This routine is called for every OL or UL tag
*/
static void BeginList(int argc, const char **argv){
  iDepth++;
  if( iDepth<=MAXDEPTH ){
    aiCount[iDepth-1] = (argc>1 && (argv[0][0]=='o' || argv[0][0]=='O'));
  }
  LineBreak(0,0);
  printf("{\\fi-%d\\li%d\n",UNINDENT,iDepth*INDENT);
  nBrace++;
}

/* This routine is called for every LI tag
*/
static void ListItem(int argc, const char **argv){
  LineBreak(0,0);
  if( iDepth>MAXDEPTH || iDepth<1 || aiCount[iDepth-1]==0 ){
    printf("{\\f2\\'b7}\\tab ");
  }else{
    printf("%d.\\tab ",aiCount[iDepth-1]++);
  }
  nSpace = 1;
  nPar = 0;
}

/* This routine is called for every /OL or /UL tag
*/
static void EndList(int argc, const char **argv){
  iDepth--;
  LineBreak(0,0);
  End(0,0);
}

/* The <BLOCKQUOTE> tag is simulated using this combination: <UL> */
static void BlockQuote(int argc, const char **argv){
  BeginList(0,0);
}

/* In the same way, </BLOCKQUOTE> is the same as </UL> */
static void EndBlockQuote(int argc, const char **argv){
  EndList(0,0);
}

/* This routine is called whenever the <DL> tag is seen in the HTML
*/
static void BeginDefinitionList(int argc, const char **argv){
  iDepth++;
  if( iDepth<=MAXDEPTH ){
    aiCount[iDepth-1] = 0;
  }
  LineBreak(0,0);
  printf("{\\fi-%d\\li%d\n",INDENT,iDepth*INDENT);
  nBrace++;
}

/* This routine is called for every <DT> tag seen
*/
static void DefinitionTag(int argc, const char **argv){
  Paragraph(0,0);
  nPar = 0;
  nSpace = 1;
}

/* This routine is called for every <DD> tag seen
*/
static void DefinitionData(int argc, const char **argv){
  LineBreak(0,0);
}

/* This routine is called for every </DL> tag seen
*/
static void EndDefinitionList(int argc, const char **argv){
  EndList(0,0);
  Paragraph(0,0);
}

static void Usage(char **argv){
  fprintf(stderr,"Usage: %s doc.html >doc.rtf\n",argv[0]);
  exit(1);
}

/* 
** The following data structure provides a mapping between HTML tags
** and routines used to process those tags.
*/
static struct sgTags {
  char *zTag;                       /* The HTML Tag */
  void (*xFunc)(int,const char**);  /* A function to process this tag */
} aTags[] = {
  { "b",           Bold                 },
  { "/b",          End                  },
  { "big",         Big                  },
  { "/big",        EndBig               },
  { "blockquote",  BlockQuote           },
  { "/blockquote", EndBlockQuote        },
  { "br",          LineBreak            },
  { "center",      Center               },
  { "/center",     End                  },
  { "cite",        Italic               },
  { "/cite",       End                  },
  { "dd",          DefinitionData       },
  { "dir",         BeginList            },
  { "/dir",        EndList              },
  { "dl",          BeginDefinitionList  },
  { "/dl",         EndDefinitionList    },
  { "dt",          DefinitionTag        },
  { "em",          Italic               },
  { "/em",         End                  },
  { "h1",          Heading              },
  { "/h1",         EndHeading           },
  { "h2",          Heading              },
  { "/h2",         EndHeading           },
  { "h3",          Heading              },
  { "/h3",         EndHeading           },
  { "h4",          Heading              },
  { "/h4",         EndHeading           },
  { "h5",          Heading              },
  { "/h5",         EndHeading           },
  { "h6",          Heading              },
  { "/h6",         EndHeading           },
  { "i",           Italic               },
  { "/i",          End                  },
  { "kbd",         FixedFont            },
  { "/kbd",        End                  },
  { "li",          ListItem             },
  { "menu",        BeginList            },
  { "/menu",       EndList              },
  { "ol",          BeginList            },
  { "/ol",         EndList              },
  { "p",           Paragraph            },
  { "/p",          EndParagraph         },
  { "pre",         Preformat            },
  { "/pre",        EndPreformat         },
  { "samp",        FixedFont            },
  { "/samp",       End                  },
  { "small",       Small                },
  { "/small",      EndSmall             },
  { "strong",      Bold                 },
  { "/strong",     End                  },
  { "sub",         Subscript            },
  { "/sub",        End                  },
  { "sup",         Superscript          },
  { "/sup",        End                  },
  { "title",       Ignore               },
  { "/title",      End                  },
  { "tt",          FixedFont            },
  { "/tt",         End                  },
  { "ul",          BeginList            },
  { "/ul",         EndList              },
  { "var",         Italic               },
  { "/var",        End                  },
};

void main(int argc, char **argv){
  char *zTail;
  FILE *pIn;
  int i;
  int endSwitch = 0;
  char *zInputFilename = 0;

  for(i=1; i<argc; i++){
    if( endSwitch || argv[i][0]!='-' ){
      if( zInputFilename ) Usage(argv);
      zInputFilename = argv[i];
    }else{
      if( strcmp(argv[i],"--")==0 ){
        endSwitch = 1;
      }
    }
  }
  if( zInputFilename ){
#if !defined(__WIN32__) && !defined(__WIN32)
    zTail = strrchr(zInputFilename,'.');
    if( zTail==0 || strcmp(zTail,".html")!=0 ) Usage(argv);
#endif
    pIn = fopen(zInputFilename,"r");
    if( pIn==0 ){
      char zErr[100];
      sprintf(zErr,"%s: Can't open \"%.*s\"",
        argv[0],(int)sizeof(zErr)-30,zInputFilename);
      perror(zErr);
      exit(1);
    }
  }else{
    pIn = stdin;
  }

  SgmlWordHandler(Word);
  SgmlSpaceHandler(Space);
  SgmlDefaultMarkupHandler(0);
  for(i=0; i<sizeof(aTags)/sizeof(aTags[0]); i++){
    SgmlHandler(aTags[i].zTag,aTags[i].xFunc);
  }
  EmitRtfHeader();
  SgmlParse(pIn);
  EmitRtfTrailer();
  exit(0);
}
