/*
 ***********************************************************************
 *
 *     sunnadir.h
 *
 *     Author: Winston Teitler.
 *
 ***********************************************************************
 *
 *     Header for the sunnadir program.
 *
 ***********************************************************************
 *
 *     @(#)sunnadir.h	1.7    08/22/97    UCB SSL
 *
 ***********************************************************************
 */





/*
 *======================================================================
 *======================================================================
 */





#ifndef  SUNNADIR_H
#define  SUNNADIR_H





/*
 *======================================================================
 *======================================================================
 */





#ifdef  __cplusplus
   extern  "C"
   {
#endif





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Constants.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     Dates and times.
 *
 *----------------------------------------------------------------------
 */



#define  WT_FAST_YEAR0         (1968)  /* epoch */
#define  WT_FAST_MONTH0        (5)
#define  WT_FAST_DAY0          (24)
#define  WT_FAST_HR0           (0)
#define  WT_FAST_MIN0          (0)
#define  WT_FAST_SEC0          (0.0)



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft packet format.
 *
 *----------------------------------------------------------------------
 */



#define  WT_FAST_PMAXB         (242)   /* max no bytes per packet */

#define  WT_FAST_PMAXBL        (16)    /* max no bytes per line */
#define  WT_FAST_PBLANKS       (1)     /* no of blanks betw bytes */

#define  WT_FAST_PBCCSDSA      (4)     /* bytes for CCSDS Address */

#define  WT_FAST_PBLTL         (2)     /* bytes for Lgth Tab Load */

#define  WT_FAST_PBSECH        (2)     /* bytes for secondary hdr */

#define  WT_FAST_PBMLA         (4)     /* bytes for Mem Load Addr */

#define  WT_FAST_PBESTI        (4)     /* bytes for Eph ST, int */
#define  WT_FAST_PBPERI        (2)     /* bytes for Period, int */
#define  WT_FAST_PBSHST        (2)     /* bytes for Shadow ST */
#define  WT_FAST_PBSHET        (2)     /* bytes for Shadow ET */
#define  WT_FAST_PBSHOBJ       (1)     /* bytes for Shadow Object */
#define  WT_FAST_PBTBLG        (1)     /* bytes for Table Length */
#define  WT_FAST_PBSTEP        (2)     /* bytes for Tab Step Time */
#define  WT_FAST_PBESTF        (2)     /* bytes for Eph ST, frac */
#define  WT_FAST_PBPERF        (2)     /* bytes for Period, frac */

#define  WT_FAST_PBGAM         (2)     /* bytes per Gamma */
#define  WT_FAST_PBDGAM        (2)     /* bytes per Delta Gamma */


#define  WT_FAST_PCCSDSA       (0X1C00C000UL)  /* CSSDS Address */

#define  WT_FAST_PSECH         (0X00000001UL)  /* secondary hdr */

#define  WT_FAST_PMLA          (0X00009200UL)  /* Mem Load Addr */


#define  WT_FAST_PNULL         (WT_TRUE)  /* indic for null pack */

#define  WT_FAST_PNBLINES      (1)     /* no blank lines bef null */

#define  WT_FAST_PNBCCSDSA     (4)     /* bytes null CCSDS Address */

#define  WT_FAST_PNBLTL        (2)     /* bytes null Lgth Tab Load */

#define  WT_FAST_PNBSECH       (2)     /* bytes null secondary hdr */

#define  WT_FAST_PNBMLA        (4)     /* bytes null Mem Load Addr */

#define  WT_FAST_PNBFILL       (1)     /* bytes for null filler */


#define  WT_FAST_PNCCSDSA      (0X1C00C000UL)  /* null CSSDS Addr */

#define  WT_FAST_PNLTL         (0X000000F1UL)  /* null Lgth Tab L */

#define  WT_FAST_PNSECH        (0X00000000UL)  /* null sec hdr */

#define  WT_FAST_PNMLA         (0X000000E0UL)  /* null Mem L Addr */

#define  WT_FAST_PNFILL        (0X00000000UL)  /* null filler */





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Type definitions.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     Set of orbit data values.
 *
 *----------------------------------------------------------------------
 */



   struct sn_orb_dat_struct
     {
       double orbGamma ;       /* Sun-Nadir proj angle */
       double orbIncGamma ;    /* increment in tab val */
       double orbShadowVal ;   /* > 0 implies shadow */
     } ;

   typedef
       struct sn_orb_dat_struct
       snOrbDat ;





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Function prototypes.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     Attitude.
 *
 *----------------------------------------------------------------------
 */



   int
       SnDetAtt(FILE *fpUsr, FILE *fpArc, const char *fname,
           int fileCode, long int jdn, double time,
           double *attv) ;



/*
 *----------------------------------------------------------------------
 *
 *     Command line arguments.
 *
 *----------------------------------------------------------------------
 */



   int
       SnPrdCmdArgs(int argc, char **argv,
           int *foundStartTime_ptr, int *year_ptr, int *month_ptr,
           int *day_ptr, int *yearday_ptr, long int *jdn_ptr,
           long int *mjn_ptr, int *weekday_ptr, int *hr_ptr,
           int *min_ptr, double *sec_ptr, double *time_ptr,
           int *foundCfgFile_ptr, int *fcCfg_ptr, char *fnCfg,
           int *foundOrbFile_ptr, int *fcOrb_ptr, char *fnOrb,
           int *foundAttFile_ptr, int *fcAtt_ptr, char *fnAtt,
           int *foundScpFile_ptr, int *fcScp_ptr, char *fnScp,
           int *foundArcFile_ptr, int *fcArc_ptr, char *fnArc,
           int *foundLogFile_ptr, int *fcLog_ptr, char *fnLog,
           int *foundUsrFile_ptr, int *fcUsr_ptr, char *fnUsr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Configuration file values.
 *
 *----------------------------------------------------------------------
 */



   int
       SnPrdCfgVals(FILE *fpUsr, FILE *fpArc, int foundCfgFile,
           int fcCfg, const char *fnCfg, const char *titleStdin,
           double *duration_ptr,
           double *timeResOrbit_ptr,
           double *timeResTable_ptr,
           long int *nElemReq_ptr,
           double *gamma0_ptr,
           double *effRe_ptr,
           double *offShST_ptr,
           double *offShET_ptr,
           int *nUplWin_ptr,
           double *offUwST_ptr,
           double *offUwET_ptr,
           unsigned long int *shObj1_ptr,
           unsigned long int *shObj2_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Ephemeris data table.
 *
 *----------------------------------------------------------------------
 */



   int
       SnPrdEphTab(FILE *fpUsr, FILE *fpArc, long int nOrb,
           const double *tOrbTab, const snOrbDat *orbTab, double ephST,
           double orbPer, long int nPer, double timeRes, long int nElem,
           double ***ephGamma_ptr, double **ephDeltaGamma_ptr) ;


   int
       SnRptEph(FILE *fpUsr, FILE *fpArc, long int nOrbPer,
           long int nElem, double **ephGamma,
           const double *ephDeltaGamma) ;



/*
 *----------------------------------------------------------------------
 *
 *     Freeing allocated memory.
 *
 *----------------------------------------------------------------------
 */



   int
       SnFreeMem(long int nOrb, long int nOrbPer, long int nElem,
           double *tOrbTab, snOrbDat *orbTab, double **ephGamma,
           double *ephDeltaGamma) ;



/*
 *----------------------------------------------------------------------
 *
 *     Orbit data.
 *
 *----------------------------------------------------------------------
 */



   int
       SnCloseOrbFile(void) ;


   int
       SnGetOrbPos(long int *jdn_ptr, double *time_ptr, double *scr,
           double *scv, int *eof_ptr) ;


   int
       SnOpenOrbFile(char *fname, int fileCode) ;



/*
 *----------------------------------------------------------------------
 *
 *     Orbit data table.
 *
 *----------------------------------------------------------------------
 */



   int
       SnDetPeriod(FILE *fpUsr, FILE *fpArc, long int jdn0,
           double userStartTime, double duration, double timeRes,
           long int nElemReq, double gamma0, long int nOrb,
           const double *tOrbTab, const snOrbDat *orbTab,
           double *ephST_ptr, long int *ephSTJdn2_ptr,
           double *ephSTTime2_ptr, double *orbPer_ptr,
           long int *nPer_ptr, long int *nElem_ptr) ;


   int
       SnDetShTime(FILE *fpUsr, FILE *fpArc, long int nOrb,
           const double *tOrbTab, const snOrbDat *orbTab, double ephST,
           double orbPer, long int nPer, double timeRes, long int nElem,
           double offShST, double offShET,
           int *shEnter_ptr, double *shST_ptr, double *shET_ptr) ;


   int
       SnPrdOrbTab(FILE *fpUsr, FILE *fpArc, int jdn0,
           double userStartTime, double duration, double timeRes,
           double effRe, int fcOrb, char *fnOrb, int fcAtt,
           const char *fnAtt,
           long int *nOrb_ptr, double **tOrbTab_ptr,
           snOrbDat **orbTab_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Position of the Sun.
 *
 *----------------------------------------------------------------------
 */



   int
       SnDetSunPos(long int jdn, double time, double nut[3][3],
           double *sunr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft Packet file.
 *
 *----------------------------------------------------------------------
 */



   int
       SnPrdScpFile(FILE *fpUsr, FILE *fpArc, int fcScp,
           const char *fnScp, int nUplWin, double offUwST,
           double offUwET, long int jdn0, double ephST,
           long int ephSTJdn2, double ephSTTime2, double orbPer,
           long int nOrbPer, double shST, double shET, int shEnter,
           unsigned long int shObj1, unsigned long int shObj2,
           double timeRes, long int nElem, double **ephGamma,
           const double *ephDeltaGamma) ;





/*
 *======================================================================
 *======================================================================
 */





#ifdef  __cplusplus
   }
#endif





/*
 *======================================================================
 *======================================================================
 */





#endif   /* SUNNADIR_H */
