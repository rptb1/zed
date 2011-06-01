/*  ZED -- text editor
 *
 * "ZED is a text editor that processes sequential files line by
 *  line under the control of EDITING COMMANDS."
 *
 *  Section numbers in brackets "[3.4.2.1]" refer to SPEC.ZED.
 *
 *  This source is formatted to 66 columns, in the Phoenix
 *  tradition, so that it can be printed in two columns on a
 *  132-column line printer.  It also means I can view the spec.
 *  and the source side-by-side on my screen.  Here is a ruler:
 *------|-------|-------|-------|-------|-------|-------|-------|-
 *	9	17	25	33	41	49	57	65
 */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <assert.h>
#include <stdarg.h>
#include <limits.h>


/* CONFIGURATION */

#define MAXLINELEN	400	/* max. line length [3.1] */
#define MAXARGS		3	/* max. arguments to a command */
#define STRDELIM "/!.+-,?'\":*"	/* string delimeters [3.2.2.1] */
#define MAXNAMELEN	4	/* significant chars in a name */
#define SCCS        "<!$_%>?#'" /* single-char commands */


/* LANGUAGE EXTENSION */

#define please	    do {	/* start macro */
#define thanks	    } while(0)	/* end macro */
#define forever	    for(;;)	/* loop indefinitely */
#define elif(c)     else if(c)
#define ul(c)	    if(!(c))
#define until(c)    while(!(c))
#define elul(c)     else ul(c)
#define notreached  please assert(0); thanks
#define must(c)     please ul(c) notreached; thanks
#define mustnot(c)  please if(c) notreached; thanks
#define unused(var) please (void)(var); thanks

#define throw(msg) \
  please \
    errmsg = (msg); longjmp(errbuf, __LINE__); \
  thanks



/* TYPES */

typedef int bool_t;		/* boolean */
#define FALSE		0
#define TRUE		1

typedef unsigned long n_t;	/* (line) number [3.2.2.7] */
#define N_MAX		ULONG_MAX

typedef struct line_s *line_t;	/* line of text */
typedef struct buf_s *buf_t;	/* input or output buffer */

typedef unsigned char lf_t;	/* line flags */
#define LF_END	(1<<0)		/* "end of file" sentinel */
#define LF_BEG	(1<<1)		/* "beginning of file" sentinel */
#define LF_CHG	(1<<2)		/* changed or new */
#define LF_NOR  (1<<3)		/* non-original line [3.3.5] */
#define LF_PDM	(1<<4)		/* pred. deleted or moved */

typedef struct line_s {		/* line structure */
  line_t prev, next;		/* links to surrounding lines */
  lf_t flags;			/* flags, see lf_t */
  buf_t buf;			/* buf from which line came */
  n_t num;			/* line number in that buf */
  size_t len;			/* length of line in chars */
  char *str;			/* null terminated line text */
} line_s;

typedef struct buf_s {		/* buffer structure */
  char *name;			/* text identification */
  n_t num;			/* number of last line read */
  FILE *f;			/* C stream if dataset */
  line_s sen;			/* sentinel line (BEG/END) */
} buf_s;

typedef size_t cc_t;		/* command code (comtab index) */

typedef struct q_s *q_t;	/* qualified string [3.2.2.3] */

typedef unsigned int qf_t;	/* qualifier flags [3.3.6] */
#define QF_B	(1<<0)		/* beginning of line */
#define QF_E	(1<<1)		/* end of line */
#define QF_C	(1<<2)		/* control char */
#define QF_L	(1<<3)		/* leftwards search */
#define QF_P	(1<<4)		/* precise match */
#define QF_W	(1<<5)		/* word match */
#define QF_U	(1<<6)		/* uppercased match */
#define QF_S	(1<<7)		/* start at first non-space */
#define QF_N	(1<<8)		/* negate match */
#define QF_R	(1<<9)		/* regular expression */
#define QF_wn	(1<<10)		/* a window was specified */
#define QF_ct	(1<<11)		/* a count was specified */

typedef struct q_s {		/* qualified string structure */
  qf_t flags;			/* flags, see qf_t */
  size_t n, m;			/* window, if QF_wn [3.3.6] */
  size_t r;			/* count, if QF_ct [3.3.6] */
  size_t len;			/* length of string */
  char *str;			/* null terminated string */
} q_s;

typedef struct se_s *se_t;	/* search expression [3.2.2.4] */

enum {				/* search expression kind */
  SE_CONJ,			/* (se & se) */
  SE_DISJ,			/* (se | se) */
  SE_Q				/* q */
};
typedef int sek_t;

typedef struct se_s {		/* search expression structure */
  sek_t kind;			/* kind, see sek_t */
  union {
    struct {
      se_t l, r;		/* left and right operands */
    } cj;			/* (se & se) and (se | se) */
    q_t q;			/* qualified string */
  } u;
} se_s;

typedef struct com_s *com_t;	/* command */

typedef struct cg_s *cg_t;	/* command group [3.2.3] */
typedef struct cg_s
{
  cg_t before;			/* previous commands, or NULL */
  com_t com;			/* next command */
} cg_s;

typedef union arg_u		/* argument [3.2.2] */
{
  q_t q;			/* qualified string */
  se_t se;			/* search expression */
  n_t n;			/* number */
  bool_t sw;			/* switch value */
  cg_t cg;			/* command group */
} arg_u, arg_t;
  
typedef struct com_s		/* command structure */
{
  cc_t cc;			/* command code */
  size_t rep;			/* repeat count */
  size_t args;			/* number of args */
  arg_t arg[MAXARGS];		/* the arguments */
} com_s;

typedef struct ch_s *ch_t;	/* chain of lines */
typedef struct ch_s {		/* chain structure */
  bool_t lazy;			/* lazy first line reading */
  buf_t to;			/* TO buffer */
  line_t line;			/* current line */
  char *col;			/* current char (for parsing) */
  buf_t from;			/* FROM buffer */
  bool_t ver;			/* verify line at next prompt */
} ch_s;


/* GLOBAL DATA */

static jmp_buf errbuf;		/* jump dest on error */
static char *errmsg;		/* see throw */
static bool_t inter;		/* interactive flag */
static q_t empty;		/* empty string */

/* Switches, with their default values as far as these can be */
/* determined from SPEC.ZED and fading memory. */

static bool_t sw_LCE   = FALSE;	/* force commands to lower case */
static bool_t sw_UCE   = FALSE;	/* force commands to upper case */
static bool_t sw_J     = FALSE;	/* justification mode */
static bool_t sw_V     = TRUE;	/* automatic verification */
static bool_t sw_VI    = FALSE;	/* verify with indicators */
static bool_t sw_VJ    = TRUE;	/* justification verification */
static bool_t sw_VG    = TRUE;	/* global change verification */
static bool_t sw_VN    = TRUE;	/* verify line numbers */
static bool_t sw_VT    = TRUE;	/* verify line texts */
static bool_t sw_VWR   = TRUE;	/* verify right window */
static bool_t sw_VWL   = TRUE;	/* verify left window */
static bool_t sw_VX    = FALSE;	/* verify in hex for indicators */
static bool_t sw_VE    = FALSE;	/* verify edits lines */
static bool_t sw_X     = FALSE;	/* hex mode */
static bool_t sw_FN    = FALSE;	/* find next mode */
static bool_t sw_TR    = FALSE;	/* trailing spaces */
static bool_t sw_SQ    = FALSE;	/* sequence numbers in output */
static bool_t sw_SQS   = FALSE;	/* source sequence numbers */
static bool_t sw_BRK   = TRUE;	/* break-in */
static bool_t sw_CS    = FALSE;	/* compress spaces */
static bool_t sw_WARN  = TRUE;	/* warnings mode */
static bool_t sw_ERRSTOP=TRUE;	/* non-interactive error action */
static bool_t sw_SCC   = FALSE;	/* supress control characters */
static bool_t sw_SO    = FALSE;	/* supress overflow errors */
static bool_t sw_MS    = FALSE;	/* monitor store usage */


/* COMMAND TABLE */

#define RP(id) \
  static void (run_##id)(com_t com, ch_t ch)
#define PP(id) \
  static bool_t (pa_##id)(ch_t ch, com_t com)

RP(N); RP(P); RP(M); RP(IS); RP(IC); RP(D); RP(F);
RP(BF); RP(E); RP(B); RP(A); RP(T); RP(TL); RP(STOP);
RP(sw); RP(VW); RP(SHS); RP(veri); RP(ver); RP(lt);
RP(do); RP(bl); RP(pc); RP(gt); RP(ha); RP(qu);

PP(sw); PP(n); PP(s); PP(se); PP(2n); PP(1n); PP(qs);

#undef RP
#undef PP

static struct {
  const char *str;		/* command name string */
  void (*run)(com_t com, ch_t ch); /* perform command */
  bool_t (*pa)(ch_t ch, com_t com); /* argument parser */
  bool_t rf;			/* command reads first line */
  bool_t *swp;			/* switch pointer (see run_sw) */
} comtab[] = {
  {"N",       run_N,    NULL,   TRUE},
  {"P",       run_P,    NULL,   TRUE},
  {"M",       run_M,    pa_n,   TRUE},
  {"IS",      run_IS,	pa_s,	TRUE},
  {"IC",      run_IC,	NULL,	TRUE},
  {"D",       run_D,	pa_2n,	TRUE},
  {"F",       run_F,	pa_se,	TRUE},
  {"BF",      run_BF,	pa_se,	TRUE},
  {"E",       run_E,	pa_qs,	TRUE},
  {"B",       run_B,	pa_qs,	TRUE},
  {"A",       run_A,	pa_qs,	TRUE},
  {"T",       run_T,	pa_1n,	TRUE},
  {"TL",      run_TL,	pa_1n,	TRUE},
  {"!",       run_veri, NULL,	TRUE},
  {"?",       run_ver,  NULL,	TRUE},
  {"<",       run_lt,   NULL,	TRUE},
  {"$",       run_do,   NULL,	TRUE},
  {"_",       run_bl,   NULL,	TRUE},
  {"%",       run_pc,   NULL,	TRUE},
  {">",       run_gt,   NULL,	TRUE},
  {"#",       run_ha,   NULL,	TRUE},
  {"'",       run_qu,   NULL,	TRUE},
  {"STOP",    run_STOP, NULL,   FALSE},
  {"SHS",     run_SHS,  NULL,   FALSE},
  {"LCE",     run_sw,   pa_sw,	FALSE, &sw_LCE},
  {"UCE",     run_sw,   pa_sw,	FALSE, &sw_UCE},
  {"J",       run_sw,   pa_sw,	FALSE, &sw_J},
  {"V",       run_sw,   pa_sw,	FALSE, &sw_V},
  {"VI",      run_sw,   pa_sw,	FALSE, &sw_VI},
  {"VJ",      run_sw,   pa_sw,	FALSE, &sw_VJ},
  {"VG",      run_sw,   pa_sw,	FALSE, &sw_VG},
  {"VN",      run_sw,   pa_sw,	FALSE, &sw_VN},
  {"VT",      run_sw,   pa_sw,	FALSE, &sw_VT},
  {"VWR",     run_sw,   pa_sw,	FALSE, &sw_VWR},
  {"VWL",     run_sw,   pa_sw,	FALSE, &sw_VWL},
  {"VW",      run_VW,   pa_sw,	FALSE},
  {"VX",      run_sw,   pa_sw,	FALSE, &sw_VX},
  {"VE",      run_sw,   pa_sw,	FALSE, &sw_VE},
  {"X",       run_sw,   pa_sw,	FALSE, &sw_X},
  {"FN",      run_sw,   pa_sw,	FALSE, &sw_FN},
  {"TR",      run_sw,   pa_sw,	FALSE, &sw_TR},
  {"SQ",      run_sw,   pa_sw,	FALSE, &sw_SQ},
  {"SQS",     run_sw,   pa_sw,	FALSE, &sw_SQS},
  {"BRK",     run_sw,   pa_sw,	FALSE, &sw_BRK},
  {"CS",      run_sw,   pa_sw,	FALSE, &sw_CS},
  {"WARN",    run_sw,   pa_sw,	FALSE, &sw_WARN},
  {"ERRSTOP", run_sw,   pa_sw,	FALSE, &sw_ERRSTOP},
  {"SCC",     run_sw,   pa_sw,	FALSE, &sw_SCC},
  {"SO",      run_sw,   pa_sw,	FALSE, &sw_SO},
  {"MS",      run_sw,   pa_sw,	FALSE, &sw_MS}
};

/* Look up a name in the command table. */

static bool_t cclook(char *name, cc_t *ccp)
{
  cc_t i;
  for(i=0; i<sizeof(comtab)/sizeof(comtab[0]); ++i)
    if(strcmp(name, comtab[i].str) == 0) {
      *ccp = i;
      return TRUE;
    }
  return FALSE;
}


/* CONSTRUCTORS AND PROJECTIONS */

/* Allocate without fail. */
/* @@ This function should cause buffers to be flushed on */
/* low memory conditions instead of summarily exiting. */

static void *alloc(size_t size)
{
  void *p = malloc(size);
  if(p == NULL) {
    fprintf(stderr, "Out of memory.\n");
    exit(1);
  }
  return p;
}

/* Allocate and initialize a line. */

static line_t linenew(buf_t from, n_t num, size_t len, char *str)
{
  line_t line = alloc(sizeof(line_s));
  line->prev = line->next = NULL;
  line->flags = 0;
  line->buf = from;
  line->num = num;
  line->len = len;
  line->str = memcpy(alloc(len+1), str, len+1);
  return line;
}

static bool_t isend(line_t line)
{
  return (line->flags & LF_END) != 0;
}

/* Allocate and initialize a buffer. */

static buf_t bufnew(char *name, FILE *f, lf_t flags)
{
  buf_t buf = alloc(sizeof(buf_s));
  buf->name = name;
  buf->num = 0;
  buf->f = f;
  buf->sen.prev = NULL;
  buf->sen.next = NULL;
  buf->sen.flags = flags;
  buf->sen.buf = buf;
  buf->sen.num = 0;
  buf->sen.len = 0;
  buf->sen.str = NULL;
  return buf;
}

/* Allocate and initialize a chain. */

static ch_t chnew(buf_t from, buf_t to)
{
  ch_t ch = alloc(sizeof(ch_s));
  ch->to = to;
  ch->from = from;
  ch->lazy = TRUE;
  ch->col = NULL;
  ch->line = &ch->to->sen;
  ch->ver = FALSE;
  return ch;
}

/* Allocate and intialize a qualified string. */
/* Note that this function allocates a new copy of the len */
/* characters pointed to by str, so it can be used to make */
/* a string by extracting characters from another, or from */
/* a line. */

static q_t qnew(qf_t flags, size_t n, size_t m, size_t r,
                size_t len, char *str)
{
  q_t q = alloc(sizeof(q_s));
  q->flags = flags;
  q->n = n;
  q->m = m;
  q->r = r;
  q->len = len;
  q->str = alloc(len+1);
  memcpy(q->str, str, len);
  q->str[len] = '\0';
  return q;
}

/* Allocate and initialize a search expression. */

static se_t senew(sek_t kind, ...)
{
  se_t se = alloc(sizeof(se_s));
  va_list va;
  va_start(va, kind);
  se->kind = kind;
  switch(kind) {
    case SE_CONJ: case SE_DISJ:
    se->u.cj.l = va_arg(va, se_t);
    se->u.cj.r = va_arg(va, se_t);
    break;
    case SE_Q:
    se->u.q = va_arg(va, q_t);
    break;
    default: notreached;
  }
  return se;
}

/* Allocate and initialize a command group. */

static cg_t cgnew(cg_t before, com_t com)
{
  cg_t cg = alloc(sizeof(cg_s));
  cg->before = before;
  cg->com = com;
  return cg;
}


/* INPUT/OUTPUT */

/* Read the next line from the stream of a buffer, returning */
/* false if the end of the stream is reached. */
/* @@ buffer should be allocated by MXLL command, not here */

static bool_t readline(buf_t buf, line_t *linep)
{
  size_t len;
  int c;
  char b[MAXLINELEN+1];
  if(fgets(b, MAXLINELEN, buf->f) == NULL)
    return FALSE;
  ++buf->num;
  /* If a whole line was read, including the newline, then */
  /* chop off the newline.  Otherwise there might be a newline */
  /* lurking as the next character on the stream, in which case */
  /* it should be read and ignored.  Otherwise, the line is */
  /* effectively split here [3.4.20]. */
  len = strlen(b);
  if(b[len-1] == '\n') {	/* got whole line? */
    b[len-1] = '\0';
    --len;
  } else {
    c = fgetc(buf->f);
    if(c != '\n') ungetc(c, buf->f);
  }
  *linep = linenew(buf, buf->num, len, b);
  return TRUE;
}

/* Advance the current line of a chain to the next line, */
/* reading the FROM buffer as necessary.  Returns false */
/* at end of chain. */

static bool_t next(ch_t ch)
{
  line_t new;
  /* Just return false if at the end of the chain. */
  if(isend(ch->line)) {
    assert(ch->col == NULL);
    return FALSE;
  }
  /* If the next line is NULL then read the next line from the */
  /* from buffer stream.  If the stream is at the end then */
  /* use the buffer sentinel as the next line. */
  if(ch->line->next == NULL) {
    if(readline(ch->from, &new)) {
      ch->line->next = new;
      new->prev = ch->line;
    } else {
      assert(isend(&ch->from->sen));
      ++ch->from->num;
      ch->from->sen.num = ch->from->num;
      ch->line->next = &ch->from->sen;
      ch->from->sen.prev = ch->line;
    }
  }
  assert(ch->line->next != NULL);
  ch->line = ch->line->next;
  ch->col = ch->line->str;
  ch->ver = TRUE;
  /* @@ apply global edits here */
  return TRUE;
}

/* Advance to the next line if the lazy flag of a chain is set. */
/* This is a special state which occurs when ZED is started, */
/* after REWI, or after changing the FROM buffer when at the */
/* end of a file.  It allows commands which affect the way */
/* lines are read to be issued before the first is read. */
/* See [3.4.21]. */

static bool_t force(ch_t ch)
{
  if(ch->lazy) {
    ch->lazy = FALSE;
    return next(ch);
  }
  return TRUE;
}

/* Back up the current line of a chain to the previous line. */
/* Returns false at the beginning of the chain. */

static bool_t prev(ch_t ch)
{
  force(ch);
  assert(ch->line->prev != NULL);
  if(ch->line->prev->flags & LF_BEG)
    return FALSE;
  ch->line = ch->line->prev;
  ch->col = ch->line->str;
  ch->ver = TRUE;
  return TRUE;
}

/* Advance the current character of a chain to the next */
/* character, reading a new line if necessary.  Returns false */
/* if there are no more characters. */

static bool_t nextc(ch_t ch)
{
  ul(force(ch))
    return FALSE;
  if(ch->col == NULL)		/* at end of file? */
    return FALSE;
  if(*ch->col == '\0')		/* at end of line? */
    ch->lazy = TRUE;		/* advance to next line, later */
  ++ch->col;
  return TRUE;
}

/* Peek at the next character of a chain.  Returns EOF at there */
/* are no more characters in order to be more useful with C */
/* library functions. */

static int peekc(ch_t ch)
{
  ul(force(ch))
    return EOF;
  if(ch->col == NULL)		/* end of file */
    return EOF;
  return *ch->col;
}

/* Advance to the next non-blank character from stream.  If the */
/* current character is non-blank, nothing happens. */

static int peeknbc(ch_t ch)
{
  int c;
  forever {
    c = peekc(ch);
    ul(isspace(c))
      return c;
    nextc(ch);
  }
}


/* COMMAND PARSER */

/* Parsing functions are of the form: */
/*   bool_t par<type>(ch_t ch, <type> *rp); */
/* They attempt to parse an object of <type> from the chain ch, */
/* and iff successful, return true and set *rp to the result. */
/* Otherwise they return false and do not update *rp. */

/* Parse an number. */
/* @@ Should check for overflow. */

static bool_t parn(ch_t ch, n_t *np)
{
  n_t n = 0;
  int c = peeknbc(ch);
  ul(isdigit(c)) return FALSE;
  do {
    n = (n*10) + c-'0';
    nextc(ch);
    c = peekc(ch);
  } while(isdigit(c));
  *np = n;
  return TRUE;
}

/* Parse a name from stream, converting to upper case and */
/* dropping excess characters beyond MAXNAMELEN. */

static bool_t parname(ch_t ch, char *name)
{
  char b[MAXNAMELEN];
  size_t i = 0;
  int c = peekc(ch);
  ul(isalpha(c)) return FALSE;
  do {
    b[i] = toupper(c);
    ++i;
    nextc(ch);
    c = peekc(ch);
  } while(i < MAXNAMELEN && isalpha(c));
  while(isalpha(peekc(ch))) nextc(ch);
  memcpy(name, b, i);
  name[i] = '\0';
  return TRUE;
}

/* Parse a command. */

static bool_t parcom(ch_t ch, com_t *comp)
{
  unsigned long rep;
  char name[MAXNAMELEN+1];
  cc_t cc;
  com_t com;
  int c;
  ul(parn(ch, &rep))		/* repeat count */
    rep = 1;			/* (defaults to 1) */
  c = peeknbc(ch);		/* non-alpha commands */
  if(c != '\0' && strchr(SCCS, c) != NULL) {
    name[0] = c;
    name[1] = '\0';
    must(nextc(ch));
  } elul(parname(ch, name))	/* multi-char commands */
    return FALSE;
  ul(cclook(name, &cc))		/* look up command in table */
    return FALSE;
  com = alloc(sizeof(com_s));
  com->cc = cc;
  com->rep = rep;
  /* If there is an argument parsing function for this */
  /* command, call it to add the arguments to the command */
  /* structure. */
  if(comtab[cc].pa != NULL) {
    ul((*comtab[cc].pa)(ch, com))
      return FALSE;
  } else
    com->args = 0;
  *comp = com;
  return TRUE;
}

/* Parse a command line. */
/* @@ Could return a command group for a line of commands */
/* with semicolons, or do something cleverer when in a command */
/* group. */

static bool_t parcl(ch_t ch, cg_t *cgp)
{
  cg_t cg = NULL;
  com_t com;
  while(peeknbc(ch) != '\0') {
    ul(parcom(ch, &com))
      return FALSE;		/* @@ free cg? */
    cg = cgnew(cg, com);
  }
  ch->lazy = TRUE;		/* read next line, but later */
  *cgp = cg;
  return TRUE;
}

/* Parse the first part of a plain string, delimited by one of */
/* the characters in STRDELIM.  This function does not consume */
/* the end delimiter, so a second string can be run together */
/* as in "E/foo/bar/" [?]. */

static bool_t pars1(ch_t ch, q_t *sp)
{
  char *b, *e;			/* beginning and end pointers */
  int c = peeknbc(ch);
  if(c == '\0' || strchr(STRDELIM, c) == NULL)
    return FALSE;
  b = ch->col + 1;		/* remember start column */
  e = b;
  while(*e != c && *e != '\0')	/* search for end delim */
    ++e;			/* (or line end [3.2.2.1]) */
  ch->col = e;			/* leave col at end delim */
  *sp = qnew(0, 0, 0, 0, e - b, b);
  return TRUE;
}

/* Parse a plain string as above, consuming the end delimiter. */

static bool_t pars(ch_t ch, q_t *sp)
{
  ul(pars1(ch, sp))
    return FALSE;
  if(peekc(ch) != '\0')
    must(nextc(ch));
  return TRUE;
}

/* Parse qualifiers before a qualified string [3.3.6] */

static bool_t parquals(ch_t ch, qf_t *qfp,
                       size_t *np, size_t *mp, size_t *rp)
{
  qf_t qf = 0;			/* default flags */
  size_t m, n, r;
  int c;
  forever {
    c = peeknbc(ch);
    switch(toupper(c)) {		/* @@ repeats? */
      case 'B': qf |= QF_B; break;
      case 'E': qf |= QF_E; break;
      case 'C': qf |= QF_C; break;
      case 'L': qf |= QF_L; break;
      case 'P': qf |= QF_P; break;
      case 'W': qf |= QF_W; break;
      case 'U': qf |= QF_U; break;
      case 'S': qf |= QF_S; break;
      case 'N': qf |= QF_N; break;
      case 'R': qf |= QF_R; break;
      case '[': case '_': {
        n_t nn, mm;
        if(c == '[')
          c = ']';
        must(nextc(ch));
        ul(parn(ch, &nn))
          nn = 1;
        if(peekc(ch) != ',')
          return FALSE;
        must(nextc(ch));
        ul(parn(ch, &mm))
          mm = 32767;		/* [3.3.6] */
        if(peekc(ch) != c)
          return FALSE;
        n = nn;			/* @@ range check? */
        m = mm;
        qf |= QF_wn;
        break;
      }
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9': {
        n_t rr;
        must(parn(ch, &rr));
        r = rr;			/* @@ range check? */
        qf |= QF_ct;
        break;
      }
      default: {
        *qfp = qf;
        *np = n;
        *mp = m;
        *rp = r;
        return TRUE;
      }
    }
    must(nextc(ch));
  }
}

/* Parse the first part of a qualified string. */

static bool_t parq1(ch_t ch, q_t *qp)
{
  qf_t qf;
  size_t n, m, r;
  q_t q;
  ul(parquals(ch, &qf, &n, &m, &r))
    return FALSE;
  ul(pars1(ch, &q))
    return FALSE;
  q->flags = qf;
  q->n = n;
  q->m = m;
  q->r = r;
  *qp = q;
  return TRUE;
}

/* Parse a qualified string. */

static bool_t parq(ch_t ch, q_t *qp)
{
  ul(parq1(ch, qp))
    return FALSE;
  if(peekc(ch) != '\0')
    must(nextc(ch));
  return TRUE;
}

/* Parse a search expression [3.2.2.4] */
/* @@ incomplete */

static bool_t parse(ch_t ch, se_t *sep);

static bool_t parse_conj(ch_t ch, se_t *sep)
{
  se_t l, r;
  ul(parse(ch, &l))
    return FALSE;
  while(peeknbc(ch) == '&') {
    nextc(ch);
    ul(parse(ch, &r))
      return FALSE;		/* @@ free l */
    l = senew(SE_CONJ, l, r);
  }
  *sep = l;
  return TRUE;
}

static bool_t parse_disj(ch_t ch, se_t *sep)
{
  se_t l, r;
  ul(parse_conj(ch, &l))
    return FALSE;
  while(peeknbc(ch) == '|') {
    nextc(ch);
    ul(parse_conj(ch, &r))
      return FALSE;		/* @@ free l */
    l = senew(SE_DISJ, l, r);
  }
  *sep = l;
  return TRUE;
}

static bool_t parse(ch_t ch, se_t *sep)
{
  se_t se;
  q_t q;
  if(peeknbc(ch) == '(') {
    nextc(ch);
    ul(parse_disj(ch, &se))
      return FALSE;
    if(peeknbc(ch) != ')')
      return FALSE;
    nextc(ch);
  } else {
    ul(parq(ch, &q))
      return FALSE;
    se = senew(SE_Q, q);
  }
  *sep = se;
  return TRUE;
}

/* Argument parsing functions are of the form: */
/*   bool_t pa_<argmne>(ch_t ch, com_t com); */
/* They attempt to parse a particular pattern of arguments from */
/* the chain ch, and iff successful, update the command com to */
/* contain the argument objects.  Otherwise they return false */
/* and do not update com. */

/* Parse "sw" argument list. */

static bool_t pa_sw(ch_t ch, com_t com)
{
  int c = peeknbc(ch);
  if(c == '+')
    com->arg[0].sw = TRUE;
  elif(c == '-')
    com->arg[0].sw = FALSE;
  else
    return FALSE;
  com->args = 1;
  nextc(ch);
  return TRUE;
}

/* Parse "n" argument list. */

static bool_t pa_n(ch_t ch, com_t com)
{
  n_t n;
  ul(parn(ch, &n))
    return FALSE;
  com->arg[0].n = n;
  com->args = 1;
  return TRUE;
}

/* Parse "s" argument list. */

static bool_t pa_s(ch_t ch, com_t com)
{
  q_t q;
  ul(pars(ch, &q))
    return FALSE;
  com->arg[0].q = q;
  com->args = 1;
  return TRUE;
}

/* Parse "se" argument list. */

static bool_t pa_se(ch_t ch, com_t com)
{
  se_t se;
  ul(parse(ch, &se))
    return FALSE;
  com->arg[0].se = se;
  com->args = 1;
  return TRUE;
}

/* Parse zero or one number argument list. */

static bool_t pa_1n(ch_t ch, com_t com)
{
  n_t n;
  size_t i;
  for(i=0; i<1; ++i) {
    (void)peeknbc(ch);
    if(parn(ch, &n))
      com->arg[i].n = n;
    else
      break;
  }
  com->args = i;
  return TRUE;
}

/* Parse zero, one, or two number argument list. */

static bool_t pa_2n(ch_t ch, com_t com)
{
  n_t n;
  size_t i;
  for(i=0; i<2; ++i) {
    (void)peeknbc(ch);
    if(parn(ch, &n))
      com->arg[i].n = n;
    else
      break;
  }
  com->args = i;
  return TRUE;
}

/* Parse "q s" argument list. */
/* This parser accepts a single central delimiter to allow */
/* commands of the form "E/foo/bar/" [2.4].  The second */
/* argument may also be omitted at the end of a line, in which */
/* case it assumed to be the empty string [3.2.2.2]. */

static bool_t pa_qs(ch_t ch, com_t com)
{
  q_t q, s;
  ul(parq1(ch, &q))
    return FALSE;
  if(peekc(ch) == '\0')
    s = empty;
  elul(pars(ch, &s))
    return FALSE;
  com->arg[0].q = q;
  com->arg[1].q = s;
  com->args = 2;
  return TRUE;
}


/* COMMAND IMPLEMENTATION */

/* Go to a line in the chain by number.  If the line number */
/* requested is smaller than the current line, this function */
/* repeatedly calls prev().  If it is larger, it calls next(). */
/* Only original lines are compared, so this leaves the */
/* current line at the first original line with a number not */
/* smaller than the number requested. */

static void go(ch_t ch, n_t n)
{
  while((ch->line->flags & LF_NOR) || n < ch->line->num)
    ul(prev(ch)) break;
  while((ch->line->flags & LF_NOR) || n > ch->line->num)
    ul(next(ch)) break;
}

/* Attempt to match a qualified string against a line. */
/* If a match is found, *bp and *ep are updated to contain */
/* the offsets of the first character and one past the last */
/* character of the match respectively. */
/* @@ incomplete */

static bool_t maq(q_t q, line_t line, size_t *bp, size_t *ep)
{
  char *p;
  if(line->str == NULL)		/* fail on sentinel lines */
    return FALSE;
  p = strstr(line->str, q->str);
  if(p == NULL)
    return FALSE;
  *bp = p - line->str;
  *ep = *bp + strlen(q->str);
  return TRUE;
}

/* Attempt to match a search expression against a line. */

static bool_t mase(se_t se, line_t line)
{
  size_t b, e;			/* dummy variables */
  switch(se->kind) {
    case SE_Q:
    return maq(se->u.q, line, &b, &e);
    case SE_CONJ:
    return mase(se->u.cj.l, line) && mase(se->u.cj.r, line);
    case SE_DISJ:
    return mase(se->u.cj.l, line) || mase(se->u.cj.r, line);
  }
  assert(0);
  return FALSE;
}

/* N -- next line [3.4.2] */

static void run_N(com_t com, ch_t ch)
{
  unused(com);
  assert(com->args == 0);
  ul(next(ch))
    throw("next past end of file");
}

/* P -- previous line [3.4.2] */

static void run_P(com_t com, ch_t ch)
{
  unused(com);
  assert(com->args == 0);
  ul(prev(ch))
    throw("prev past beginning of store");
}

/* M -- move to line by number [3.4.2] */
/* @@ M. needs to reset the operational window [3.4.2] */
/* @@ M- and M+ unimplemened [3.4.2] */

static void run_M(com_t com, ch_t ch)
{
  n_t n;
  assert(com->args == 1);
  n = com->arg[0].n;
  if(n == 0)			/* @@ Mark's recollection */
    throw("bad number");
  if(n <= ch->to->num)		/* line has been flushed */
    throw("M past beginning of store");
  go(ch, n);
}      

/* STOP -- stop processing immediately [3.4.15] */

static void run_STOP(com_t com, ch_t ch)
{
  unused(com);
  unused(ch);
  assert(com->args == 0);
  exit(1);
}

/* IS -- insert string before current line [3.4.3] */

static void run_IS(com_t com, ch_t ch)
{
  line_t new;
  assert(com->args == 1);
  new = linenew(NULL, 0,
                com->arg[0].q->len, com->arg[0].q->str);
  new->flags |= LF_NOR;		/* non-original line */
  new->next = ch->line;
  new->prev = ch->line->prev;
  ch->line->prev->next = new;
  ch->line->prev = new;
}

/* IC -- insert copy of current line [3.4.3] */
/* SPEC.ZED doesn't say what happens if the current line is */
/* the end of file.  Mark says we should raise an error. */

static void run_IC(com_t com, ch_t ch)
{
  line_t new;
  unused(com);
  assert(com->args == 0);
  if(isend(ch->line))
    throw("IC at end of source");
  new = linenew(NULL, 0, ch->line->len, ch->line->str);
  new->flags |= LF_NOR;		/* non-original line */
  new->next = ch->line;
  new->prev = ch->line->prev;
  ch->line->prev->next = new;
  ch->line->prev = new;
}

/* D -- delete range of lines [3.4.3] */

static void run_D(com_t com, ch_t ch)
{
  n_t a, b;
  line_t al, bl;		/* first and last line to del */
  assert(com->args <= 2);
  switch(com->args) {
    case 0: a = b = ch->line->num; break;
    case 1: a = b = com->arg[0].n; break;
    case 2: a = com->arg[0].n; b = com->arg[1].n; break;
  }
  if(a <= ch->to->sen.num)
    throw("D before beginning of store");
  go(ch, a);
  al = ch->line;
  go(ch, b);			/* @@ will execute globals */
  if(isend(ch->line))		/* avoid deleting end sentinel */
    must(prev(ch));
  bl = ch->line;
  must(next(ch));		/* leave current line after del */
  al->prev->next = bl->next;
  bl->next->prev = al->prev;
  /* @@ free lines */
}

/* F -- forward search [3.4.2] */

static void run_F(com_t com, ch_t ch)
{
  se_t se;
  assert(com->args == 1);
  se = com->arg[0].se;
  if(sw_FN) (void)next(ch);
  until(mase(se, ch->line))
    ul(next(ch))
      throw("SOURCE EXHAUSTED");	/* [2.3.2] */
  ch->ver = TRUE;
}

/* BF -- backward search [3.4.2] */

static void run_BF(com_t com, ch_t ch)
{
  se_t se;
  assert(com->args == 1);
  se = com->arg[0].se;
  if(sw_FN) (void)prev(ch);
  until(mase(se, ch->line))
    ul(prev(ch))
      throw("NO MORE PREVIOUS LINES"); /* [2.3.2] */
  ch->ver = TRUE;
}

/* E -- exchange [3.4.5.1] */

static void run_E(com_t com, ch_t ch)
{
  size_t b, e, l, nl;
  char *n;
  q_t q, s;
  assert(com->args == 2);
  q = com->arg[0].q;
  s = com->arg[1].q;
  ul(maq(q, ch->line, &b, &e))
    throw("**No match");		/* [2.4] */
  ch->ver = TRUE;		/* force verification of line */
  if(e - b == s->len) {		/* is replacement same length? */
    memcpy(ch->line->str + b, s->str, s->len);
    return;
  }
  l = ch->line->len;
  nl = l - (e - b) + s->len;	/* new line length */
  n = alloc(nl + 1);
  memcpy(n, ch->line->str, b);	/* copy up to match */
  memcpy(n + b, s->str, s->len);/* copy replacement */
  strcpy(n + b + s->len, ch->line->str + e); /* copy after */
  free(ch->line->str);		/* free old line string */
  ch->line->len = nl;		/* replace it with new */
  ch->line->str = n;
}

/* B -- insert before [3.4.5.1] */

static void run_B(com_t com, ch_t ch)
{
  size_t b, e, l, nl;
  char *n;
  q_t q, s;
  assert(com->args == 2);
  q = com->arg[0].q;
  s = com->arg[1].q;
  ul(maq(q, ch->line, &b, &e))
    throw("**No match");		/* [2.4] */
  ch->ver = TRUE;		/* force verification of line */
  if(s->len == 0)		/* stupid optimization */
    return;
  l = ch->line->len;
  nl = l + s->len;
  n = alloc(nl + 1);
  memcpy(n, ch->line->str, b);	/* copy up to match */
  memcpy(n + b, s->str, s->len);/* add new string */
  strcpy(n + b + s->len, ch->line->str + b); /* copy rest */
  free(ch->line->str);		/* free old line */
  ch->line->len = nl;		/* replace it with new */
  ch->line->str = n;
}

/* A -- insert after [3.4.5.1] */

static void run_A(com_t com, ch_t ch)
{
  size_t b, e, l, nl;
  char *n;
  q_t q, s;
  assert(com->args == 2);
  q = com->arg[0].q;
  s = com->arg[1].q;
  ul(maq(q, ch->line, &b, &e))
    throw("**No match");		/* [2.4] */
  ch->ver = TRUE;		/* force verification of line */
  if(s->len == 0)		/* stupid optimization */
    return;
  l = ch->line->len;
  nl = l + s->len;
  n = alloc(nl + 1);
  memcpy(n, ch->line->str, e);	/* copy up to end of match */
  memcpy(n + e, s->str, s->len);/* add new string */
  strcpy(n + e + s->len, ch->line->str + e); /* copy rest */
  free(ch->line->str);		/* free old line */
  ch->line->len = nl;		/* replace it with new */
  ch->line->str = n;
}

/* T -- type lines [3.4.7] */

static void run_T(com_t com, ch_t ch)
{
  n_t a;
  assert(com->args <= 1);
  if(com->args == 1) {
    a = com->arg[0].n;
    if(a == 0)
      throw("T passed zero, naughty");
  } else
    a = N_MAX;
  do {
    ul(isend(ch->line))
      puts(ch->line->str);
    --a;
  } while(a > 0 && next(ch));
}

/* TL -- type lines in LIST format [3.4.7] [3.3.9] */

static void run_TL(com_t com, ch_t ch)
{
  n_t a;
  assert(com->args <= 1);
  if(com->args == 1) {
    a = com->arg[0].n;
    if(a == 0)
      throw("TL passed zero, naughty");
  } else
    a = N_MAX;
  do {
    lf_t flags = ch->line->flags;
    n_t n = ch->line->num;
    ul((flags & LF_END)) {
      if(flags & LF_NOR) {
        if(ch->line->buf != NULL)
          printf("(%5lu)", (unsigned long)n);
        else
          printf("****   ");
      } else
        printf("%5lu%c%c", (unsigned long)n,
               flags & LF_CHG ? '+' : ' ',
               flags & LF_PDM ? '-' : ' ');
      puts(ch->line->str);
    }
    --a;
  } while(a > 0 && next(ch));
}

static void run_veri(com_t com, ch_t ch)
{
  unused(com);
  unused(ch);
}

static void run_ver(com_t com, ch_t ch)
{
  unused(com);
  unused(ch);
  ch->ver = TRUE;
}

static void run_lt(com_t com, ch_t ch)
{
  unused(com);
  assert(ch->col != NULL);
  if(ch->col == ch->line->str)
    throw("< past beginning of line");
  --ch->col;
}

static void run_do(com_t com, ch_t ch)
{
  unused(com);
  assert(ch->col != NULL);
  if(*ch->col != '\0') {
    *ch->col = tolower(*ch->col);
    ++ch->col;
    ch->ver = TRUE;
  }
}

static void run_bl(com_t com, ch_t ch)
{
  unused(com);
  assert(ch->col != NULL);
  if(*ch->col != '\0') {
    *ch->col = ' ';
    ++ch->col;
    ch->ver = TRUE;
  }
}

static void run_pc(com_t com, ch_t ch)
{
  unused(com);
  assert(ch->col != NULL);
  if(*ch->col != '\0') {
    *ch->col = toupper(*ch->col);
    ++ch->col;
    ch->ver = TRUE;
  }
}

static void run_gt(com_t com, ch_t ch)
{
  unused(com);
  assert(ch->col != NULL);
  if(*ch->col != '\0')		/* **** error on edge of win? */
    ++ch->col;
}

static void run_ha(com_t com, ch_t ch)
{
  unused(com);
  unused(ch);
}

static void run_qu(com_t com, ch_t ch)
{
  unused(com);
  unused(ch);
}

/* switches */
/* All the switch commands share this run function, and use the */
/* "p" field in the command table to twiddle the right switch */
/* variable. */

static void run_sw(com_t com, ch_t ch)
{
  bool_t *swp;
  unused(ch);
  assert(com->args == 1);
  assert(comtab[com->cc].swp != NULL);
  swp = (bool_t *)comtab[com->cc].swp;
  *swp = com->arg[0].sw;
}

/* VW -- set both VWL and VWR [3.4.16] */

static void run_VW(com_t com, ch_t ch)
{
  unused(ch);
  assert(com->args == 1);
  sw_VWL = sw_VWR = com->arg[0].sw;
}

/* SHS -- show switches [3.4.14] */

static void run_SHS(com_t com, ch_t ch)
{
  cc_t i;
  unused(ch);
  unused(com);
  for(i = 0; i < sizeof(comtab)/sizeof(comtab[0]); ++i)
    if(comtab[i].swp != NULL)
      printf("%s%c\n", comtab[i].str,
             *comtab[i].swp ? '+' : '-');
}

/* Run a command group. */

static void run(cg_t cg, ch_t ch)
{
  size_t i;
  if(cg != NULL) {
    run(cg->before, ch);
    if(comtab[cg->com->cc].rf)
      force(ch);
    for(i=0; i<cg->com->rep; ++i)
      (*comtab[cg->com->cc].run)(cg->com, ch);
  }
}


/* TOP LEVEL */

/* Verify the current line or prompt, maybe [3.3.1] [3.4.16] */

static void ver(ch_t ch)
{
  bool_t vf = FALSE;
  if(sw_V && (sw_VN || sw_VT)) {
    vf = ch->ver;
    ch->ver = FALSE;
  }
  if(vf) {
    if(sw_VN) {
      if(ch->line->flags & LF_NOR) {
        if(ch->line->buf == NULL)
          fputs("*.", stdout);
        else
          printf("(%lu)", (unsigned long)ch->line->num);
      } else {
        printf("%lu", (unsigned long)ch->line->num);
        if(isend(ch->line))
          putchar('*');
        else
          putchar('.');
      }
      putchar('\n');
    }
    if(sw_VT && !isend(ch->line)) {
      fputs(ch->line->str, stdout);
      putchar('\n');
      if(sw_VWL && ch->col != ch->line->str) {
        size_t i = ch->col - ch->line->str - 1;
        while(i--) putchar(' ');
        putchar('>');
      }
    }
  }
  else
    fputs(": ", stdout);
  fflush(stdout);
}

/* Initialization and main loop. */

int main(void)
{
  int line;
  FILE *f, *g;
  cg_t cg;
  ch_t ech, cch;	/* edit and command chains */

  /* trace = TRUE; */

  empty = qnew(0, 0, 0, 0, 0, "");
  
  f = fopen("zed.c", "r");
  if(f == NULL) abort();
  g = fopen("test.out", "w");
  if(g == NULL) abort();

  ech = chnew(bufnew("test.in", f, LF_END | LF_CHG),
              bufnew("test.out", g, LF_BEG));

  cch = chnew(bufnew("stdin", stdin, LF_END | LF_CHG),
              bufnew("stdout", stdout, LF_BEG));

  inter = TRUE;		/* @@ [3.4.16] */
  sw_V = inter;		/* depends on interactive [3.4.16] */
  sw_VN = TRUE;		/* default is VN+ [3.4.16] */
  sw_VT = TRUE;		/* default is VT+ [3.4.16] */

  line = setjmp(errbuf);
  if(line) {
    fprintf(stderr, "%d:%s?\n", line, errmsg);
  }

  until(feof(stdin)) {
    ver(ech);
    ul(parcl(cch, &cg)) {
      fprintf(stderr, "parse failure\n");
      cch->lazy = TRUE;	/* flush to next line */
    } else
      run(cg, ech);
  }
  
  {
    line_t l;
    for(l = cch->to->sen.next;
        l != &cch->from->sen;
        l = l->next) {
      printf("%6lu %s\n", (unsigned long)l->num, l->str);
    }
  }

  return 0;
}
