/* PolarDecomConfig.h */

#ifndef POLARDECOMCONFIG_H
#define POLARDECOMCONFIG_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS: */
#define SccsId_PolarDecomConfig_h  "@(#)PolarDecomConfig.h	1.20, 07/22/04"

#define MAX_POLAR_DATA_DIRECTORIES    200
#define MAX_POLAR_DIRECTORY_NAME_LEN  256
#define MAX_POLAR_COUNT_DIRECTORIES   10
#define MAX_POLAR_BURST_HISTORY_DIRECTORIES   10
#define MAX_POLAR_UCLA_MFE_TIMING_DIRECTORIES  4

/* This structure is set up to gather the Polar Decommutator
 * Configuration file information.  It serves as source code
 * communication between the lex/yacc parser and the rest of
 * the decommutator:
 */
struct CFGPolarInfo_struct
    {
    int     default_memory_type ;
    int     number_data_directories ;
    int     number_count_directories ;
    int     number_cmp_directories ;
    int     number_aprj_directories ;
    int     number_bhist_directories ;
    int     number_ucla_mfe_tdirs ;
    int     number_filtered_v56l_dirs ;
    int     sdt7_timing_mode ;
    int     mburst_timing_mode ;
    int     bburst_timing_mode ;
    int     nrt_pword_idx ;
    int     VerifyEFILZ ;
    int     Use54HzMFE ;
    int     Use_UCLA_MFE_Timing ;
    int     Use_Iowa_Mode2_Data ;
    int     Use_Filtered_V12L ;
    int     Use_Filtered_V34L ;
    int     Use_Filtered_V56L ;
    int     Use_Filtered_BBV12L ;
    int     Use_Filtered_BBV34L ;
    int     Use_Filtered_BBV56L ;
    int     Use_Filtered_MBV12L ;
    int     Use_Filtered_MBV34L ;
    int     Use_Filtered_MBV56L ;
    int     ScanForBackMFEOffsets ;
    double  nrt_default_tspan_hours ;
    char    *nrt_default_ofile ;
    char    *nrt_server ;
    char    *dnames[MAX_POLAR_DATA_DIRECTORIES] ;
    char    *cntnames[MAX_POLAR_COUNT_DIRECTORIES] ;
    char    *cmpnames[MAX_POLAR_DATA_DIRECTORIES] ;
    char    *aprjnames[MAX_POLAR_DATA_DIRECTORIES] ;
    char    *bhistnames[MAX_POLAR_BURST_HISTORY_DIRECTORIES] ;
    char    *ucla_mfe_names[MAX_POLAR_UCLA_MFE_TIMING_DIRECTORIES] ;
    char    *filtered_v56l_dirs[MAX_POLAR_COUNT_DIRECTORIES] ;
    char    *DQDFname ;
    char    *PQDFname ;
    } ;
typedef struct  CFGPolarInfo_struct  CFGPolarInfo ;

/* CFGPolarInfo function declarations (all of these are defined in
 * PolarEFIDcom.cc):
 */
extern CFGPolarInfo  *ConstructCFGPolarInfo (void) ;
extern int  DestructCFGPolarInfo (CFGPolarInfo *cptr) ;
extern int  PrintCFGPolarInfo (CFGPolarInfo *cptr, FILE *ofp) ;

/* Defined in ParseConfig.l: */
extern FILE *InitConfigFile (char *fname) ;

/* Defined in ParseConfig.y: */
extern int cfgparse (void) ;
extern CFGPolarInfo *EndPolarCFGParsing (void) ;

extern CFGPolarInfo *ParsePolarEfiConfigurationFromStandardLocations (
    int *ecode, char **location_found) ;

extern CFGPolarInfo *ParsePolarEfiConfiguration (
    char *cfg_filename, int  *ecode) ;

extern int  DoesPolarConfigFileExist (const char *basedir,
    const char *subdir, const char *fname, char **pathname) ;

extern char *PolarCfgParseEnvironmentVariablesInString (char *istr,
     int *err_code) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* POLARDECOMCONFIG_H */
