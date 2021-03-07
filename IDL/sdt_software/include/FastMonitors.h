#ifndef FastMonitors_h_def
#define FastMonitors_h_def
static char SccsId_FastMonitors_h[] = "@(#)FastMonitors.h	1.8, 03/01/00 UCB SSL" ;

#define MONALLOC 400

enum nasatype {BIT, UB, UI085, SI085, ULI085, ULI320, DFP085, TIME12} ;

struct mon_struct { 
  unsigned int 	apid;
  unsigned int 	apidofs;
  unsigned int 	inx; /* for the gse */
  float 	scl;
  float 	ofs;
  char		unit;
  char 		unit_str[5];
  char 		name[20];
  float		rh, rl, yh, yl;
  float		p5, p4, p3, p2;
  char		poly;
  unsigned char mask;
  unsigned short bit;
  unsigned int	trig_inx ;
  unsigned char trig_mask ;
  char 		trig_op ;
  unsigned char trig_val ;
  enum nasatype type ;
} ;

extern struct mon_struct mon[MONALLOC];

extern int monMax;

extern struct mon_struct SCmon[MONALLOC];

extern int SCmonMax;

int read_mongains(char *mon_path) ;
int read_SCmongains(char *mon_path) ;

float DecodeMonValue(unsigned char *val, int index) ;
float DecodeSCMonValue(unsigned char *val, int index) ;

int CheckMonLimit(float val, int index, unsigned char *buf) ;
int CheckSCMonLimit(float val, int index, unsigned char *buf) ;

int FindMonIndex(char *monName) ;
int FindSCMonIndex(char *monName) ;

#endif
