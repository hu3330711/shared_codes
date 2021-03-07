// ---------------------------------------------------------------------
//
// cluster_lib.h
//
// These are the declarations that should be relevant to all
// Cluster data as defined by ESA:
//
//

#ifndef CLUSTER_LIB_H
#define CLUSTER_LIB_H

/* For SCCS */
#define SccsId_cluster_lib_h "@(#)cluster_lib.h	1.9, 05/21/04"

#include <stdio.h>
#include <stdlib.h>

#ifdef OLD_STYLE_STR_HDRS
#include <iostream.h>
#include <fstream.h>
#else
#include <iostream>
#include <fstream>
using std::cin ;
using std::cout ;
using std::cerr ;
using std::flush ;
using std::endl ;
#endif

#include <ctype.h>
#include <time.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "cluster_const.h"

// ---------------------------------------------------------------------
// Typedefs:

// ---------------------------------------------------------------------
// Constants.

//  Cluster project data source types:
extern const int  CLUSTER_DATA_SRC_TYPE_ASPOC ;
extern const int  CLUSTER_DATA_SRC_TYPE_EDI ;
extern const int  CLUSTER_DATA_SRC_TYPE_FGM ;
extern const int  CLUSTER_DATA_SRC_TYPE_CIS ;
extern const int  CLUSTER_DATA_SRC_TYPE_PEACE ;
extern const int  CLUSTER_DATA_SRC_TYPE_RAPID ;
extern const int  CLUSTER_DATA_SRC_TYPE_WEC ;
extern const int  CLUSTER_DATA_SRC_TYPE_SPC_PLATFORM ;
extern const int  CLUSTER_DATA_SRC_TYPE_SHORT_TERM_ORBIT ;
extern const int  CLUSTER_DATA_SRC_TYPE_SHORT_TERM_EVENT ;
extern const int  CLUSTER_DATA_SRC_TYPE_ATTITUDE_SPIN_RATE ;
extern const int  CLUSTER_DATA_SRC_TYPE_TIME_CALIBRATION ;
extern const int  CLUSTER_DATA_SRC_TYPE_COMMAND_HISTORY ;
extern const int  CLUSTER_DATA_SRC_TYPE_COVARIANT_MATRIX ;

//  Cluster project data file types:
extern const int  CLUSTER_DATA_FILE_TYPE_NORMAL_SCIENCE ;
extern const int  CLUSTER_DATA_FILE_TYPE_BURST_SCIENCE ;
extern const int  CLUSTER_DATA_FILE_TYPE_HOUSE_KEEPING ;
extern const int  CLUSTER_DATA_FILE_TYPE_HOUSE_KEEPING_PARAMETER ;
extern const int  CLUSTER_DATA_FILE_TYPE_AUXILIARY ;

//  Cluster project data file spacecraft ids:
extern const int  CLUSTER_DATA_FILE_SPACECRAFT_X ;
extern const int  CLUSTER_DATA_FILE_SPACECRAFT_1 ;
extern const int  CLUSTER_DATA_FILE_SPACECRAFT_2 ;
extern const int  CLUSTER_DATA_FILE_SPACECRAFT_3 ;
extern const int  CLUSTER_DATA_FILE_SPACECRAFT_4 ;

extern  const int    ClusterCDDirNameNmb ;
extern  const char   *ClusterCDDirName[16] ;

extern  const int    ClusterCDDirSubsNmb ;
extern  const char   *ClusterCDDirSubs[40] ;

extern  const int  voldesc_name_nmb ;
extern  const char *voldesc_name[2] ;

extern  const int  cluster_cnt_file_suffix_nmb ;
extern  const char *CLUSTER_CNT_FILE_SUFFIX[2] ;

extern  const int CLUSTER_SFDU_VOLUME_PROD ;
extern  const int CLUSTER_SFDU_HDR_LVO ;
extern  const int CLUSTER_SFDU_DDUS ;
extern  const int CLUSTER_SFDU_CATALOGUE ;

extern  const int  CLUSTER_DATA_SET_NAME_LENGTH ;

#ifdef FROM_POLAR

// The list of prefixes that the EFD files will use:
extern  const int    EFD_prefix_number ;
extern  const char   *EFD_prefixes[4] ;

// The list of prefixes that the MGF files will use:
extern  const int    MGF_prefix_number ;
extern  const char   *MGF_prefixes[2] ;

// The list of prefixes that the SCR (housekeeping) files will use:
extern  const int    SCR_prefix_number ;
extern  const char   *SCR_prefixes[4] ;

// The list of prefixes that the SFR (Spin fit) files will use:
extern  const int    SFR_prefix_number ;
extern  const char   *SFR_prefixes[2] ;

extern  const int  ISTP_GEOTAIL_ID_number ;
extern  const char *ISTP_GEOTAIL_ID[2] ;

extern  const int  ISTP_WIND_ID_number ;
extern  const char *ISTP_WIND_ID[2] ;

extern  const int  ISTP_POLAR_ID_number ;
extern  const char *ISTP_POLAR_ID[2] ;

// The list of prefixes for EFI, EFD, MGF, SCR, and other ISTP LZ files:
extern  const int    ISTP_LZ_prefix_number ;
extern  const char   *ISTP_LZ_prefixes[] ;

// The list of suffixes for EFI, EFD, MGF, SCR, and other ISTP LZ files:
extern  const int    ISTP_LZ_suffix_number ;
extern  const char   *ISTP_LZ_suffixes[2] ;

// The list of prefixes for the attitude files:
extern  const int    att_prefix_number ;
extern  const char   *att_prefixes[4] ;

// The list of prefixes for the orbit files:
extern  const int    orb_prefix_number ;
extern  const char   *orb_prefixes[4] ;

// The list of prefixes for the spin-phase files:
extern const int    spn_prefix_number ;
extern const char   *spn_prefixes[2] ;

// The list of suffixes for attitude, orbit, and other ISTP CDF files:
extern const int    ISTP_CDF_suffix_number ;
extern const char   *ISTP_CDF_suffixes[2] ;

// The location of the date part of ISTP data files:
extern const int    ISTP_date_location ;

// The location of the version number part of ISTP data files:
extern const int    ISTP_version_location ;

extern  const int    ISTP_EFI_prefix_number ;
extern  const int    ISTP_MFE_prefix_number ;
extern  const int    ISTP_SCR_prefix_number ;
extern  const int    ISTP_EFD_prefix_number ;
extern  const int    ISTP_MGF_prefix_number ;

extern  const char   **ISTP_EFI_prefixes ;
extern  const char   **ISTP_MFE_prefixes ;
extern  const char   **ISTP_SCR_prefixes ;
extern  const char   **ISTP_EFD_prefixes ;
extern  const char   **ISTP_MGR_prefixes ;

extern  const int    IstpEfiLzLocNameNmb ;
extern  const int    IstpMfeLzLocNameNmb ;
extern  const char   *IstpEfiLzLocName[2] ;
extern  const char   *IstpMfeLzLocName[2] ;

extern  const int    IstpAttLocNumber ;
extern  const int    IstpOrbLocNumber ;
extern  const int    IstpSphLocNumber ;

extern  const char   *IstpAttLocName[4] ;
extern  const char   *IstpOrbLocName[4] ;
extern  const char   *IstpSphLocName[2] ;

#endif // FROM_POLAR

// ---------------------------------------------------------------------
// Typedefs:

struct ClusterDataFileNameInfo_struct
    {
    int    year ;
    int    month ;
    int    day_of_month ;
    int    day_of_year ;
    int    julian_day ;
    int    file_type ;
    int    source_type ;
    int    sub_disk ;
    int    spacecraft ;
    char   file_version ;
    } ;
typedef  struct ClusterDataFileNameInfo_struct ClusterDataFileNameInfo ;

struct ClusterDataSetNameInfo_struct
    {
    // Expect a name like:
    //
    //    950609_1_1a
    //
    // The year/month/day_of_month are in the first 6 digits
    //
    // sub_disk is the 8'th character (the character following the
    //    first '_').  In the above example, it is '1'.
    //
    // "field2" is the 10'th character (the character following the
    //    second '_').  It indicates the total number of CDs for
    //    the date indicated in the first six digits.
    //
    // "set_version" is the last character.  In the example above, it
    //    is 'a'.   Version 'b' supercedes 'a', version 'c' supercedes
    //    'b', etc.  This field should be treated as case-insensitive.
    //
    int    year ;
    int    month ;
    int    day_of_month ;
    int    day_of_year ;
    int    julian_day ;
    int    sub_disk ;
    int    field2 ;
    char   set_version ;
    } ;
typedef  struct ClusterDataSetNameInfo_struct ClusterDataSetNameInfo ;

struct ClusterVolumeProduction_struct
    {
    char  label[100] ;
    char  mission[20] ;
    char  preparation_info[200] ;
    int   total_data_qty ;
    char  computer[40] ;
    char  operating_system[40] ;
    char  dds_software_version[40] ;

    // These are in cluster epoch time (Jan 1, 1958):
    double  earliest_packet ;
    double  latest_packet ;
    double  production_time ;
    char    earliest_packet_text[30] ;
    char    latest_packet_text[30] ;
    char    production_time_text[30] ;
    } ;
typedef   ClusterVolumeProduction_struct  ClusterVolumeProduction ;

struct ClusterHeaderReference_struct
    {
    char  ccsd_label[100] ;
    char  reference_type[40] ;
    char  label[100] ;
    char  reference[100] ;
    int   HdrRefType ;  /* 0 -> idx, 1 -> lsc, 2 -> lst */
    } ;
typedef   ClusterHeaderReference_struct  ClusterHeaderReference ;

struct ClusterCatalogueEntry_struct
    {
    int     Spacecraft ;        /*  1, 2, 3, 4 */
    char    DataSource[8] ;     /* WEC, CIS, FGM, etc. */
    char    DataType[8] ;       /* NSD, BSD, HKD, etc. */
    char    ADID[10] ;
    char    Filename[50] ;      /* pathname of file on the CD */
    int     NumberPackets ;     /* number of packets in the file */

    // These are in ClusterEpochTime (secs since 58/01/01).
    // If set to:  -1   then time is not applicable to this file:
    double  earliest_packet ;
    double  latest_packet ;
    } ;
typedef   ClusterCatalogueEntry_struct  ClusterCatalogueEntry ;

struct ClusterCDSfdu_struct
    {
    char                       Id[40] ;
    ClusterVolumeProduction    VProd ;
    ClusterHeaderReference     IDX ;
    ClusterHeaderReference     LSC ;
    ClusterHeaderReference     LST ;

    int                        NumberCatalogueEntries ;
    ClusterCatalogueEntry      Catalogue[MAX_CLS_CD_CATALOGUE_ENTRIES] ;
    } ;
typedef   ClusterCDSfdu_struct  ClusterCDSfdu ;


// Contains information (SFDU and otherwise) regarding a Cluster
// project data set.
//
// This will usually be filled by a call to the routine:
//
//   IsThisClusterDataSet
//
struct ClusterDataSetScanInfo_struct
    {
    // Indicates if this is an existing, proper, Cluster data set:
    int     is_data_set ;

    // Indicates if the data set name conforms to the project
    // CD root naming convention:
    int     pathname_is_cluster_like ;

    // Indicates if the data set is counted:
    int     data_is_counted ;

    // Indicates if this data set is on a CD:
    int     data_is_cd ;

    // Indicates if the time span in "TSpan" is meaningful:
    int     timespan_available ;

    // Contains the yyyy/mm/dd for the date of the start of the
    // data in this set:
    char    Date[20] ;

    // If "data_is_counted" is "1", this contains the full-pathname
    // of the count file for the data set.  "CountFileName" is the
    // structure's own copy, so it should be deleted whenever the
    // structure is deleted.
    char    *CountFileName ;

    // If "is_data_set", then this is set to the full pathname of
    // the data set.  We cannot rely on "filename" in "sfdu->Catalogue"
    // because it may not have the correct full name, it is a copy
    // of an original CD to hard disk.  "FullPathName" is the
    // structure's own copy, so it should be deleted whenever the
    // structure is deleted.
    char    *FullPathName ;

    // If "pathname_is_cluster_like" is "1", then this contains
    // the pathname information:
    ClusterDataSetNameInfo  name_info ;

    // The TimeSpan represented by this data set:
    TimeSpanDescription   TSpan ;

    // Contains the information from the volume descriptor in
    // the data set.  Note that "sfdu" is the structure's own
    // copy of the "ClusterCDSfdu", so any destruction of this
    // structure should destruct "sfdu" as well.
    ClusterCDSfdu           *sfdu ;
    } ;
typedef   ClusterDataSetScanInfo_struct  ClusterDataSetScanInfo ;


// Contains information (SFDU and otherwise) regarding a list of Cluster
// data sets.  It is important to encapsulate a list of these, because,
// like FAST, users may be interested in timespans encompassing more
// than one data set.  In particular, an orbit, or a calendar day may
// require multiple data sets.
//
// This will usually be filled by a call to the routine:
//
//   IsThisClusterDataSetList
//
struct ClusterDataSetScanInfoList_struct
    {
    int   nlist ;
    ClusterDataSetScanInfo *list ;

    // Indicates if the time span in "TSpan" is meaningful:
    int     timespan_available ;

    // The TimeSpan represented by this data set list:
    TimeSpanDescription   TSpan ;
    } ;
typedef   ClusterDataSetScanInfoList_struct  ClusterDataSetScanInfoList ;

#ifdef FROM_POLAR

// This is the sfdu label structure:
struct  istp_sfdu_label_struct
    {
    char    caid[8] ;
    int     version ;
    int     sfdu_class ;
    int     delim_type ;
    int     spares ;
    char    ddid[8] ;
    char    adid[12] ;
    int     delim_param ;
    } ;
typedef  struct  istp_sfdu_label_struct  istp_sfdu_label ;

// This is the edit file structure in FLR's:
struct  istp_edit_file_struct
    {
    char   file_key[28] ;
    char   file_rerun_number[8] ;
    char   file_program_version[12] ;
    char   file_run_date_time[20] ;
    char   file_data_type[8] ;
    } ;
typedef  struct  istp_edit_file_struct  istp_edit_file ;


// The class for the Level-0 data file FLR (file label record):
class  LZeroFlr
    {
    private:
    public:
	int32         spacecraft_id ;
	int32         instrument_number ;
	char          instrument_name[8] ;
	int32         physical_record_count ;
	int32         physical_records_per_major_frame ;
	int32         max_physical_records_per_major_frame ;
	int32         major_frame_count_start ;
	int32         major_frame_count_end ;
	char          spacecraft_clock_start[8] ;
	char          spacecraft_clock_end[8] ;
	int32         atc_year_start ;
	int32         atc_day_start ;
	int32         atc_millisecond_start ;
	int32         atc_microsecond_start ;
	int32         atc_year_end ;
	int32         atc_day_end ;
	int32         atc_millisecond_end ;
	int32         atc_microsecond_end ;
	int32         number_expected_major_frames ;
	int32         number_major_frames ;
	int32         number_major_frame_gaps ;
	char          data_coverage_type[8] ;
	char          data_rerun_number[8] ;
	char          decom_program_version[12] ;
	char          decom_database_version[12] ;
	char          decom_run_date_time[20] ;
	char          instrument_file_name[48] ;
	int32         physical_record_size ;
	char          spares[20] ;
	char          merge_rerun_number[8] ;
	char          merge_program_version[12] ;
	char          merge_run_date_time[20] ;
	int32         number_edit_files ;
	istp_edit_file  edit_files[20] ;

	// Constructors:
	LZeroFlr ()
	    {
	    }
	LZeroFlr (char *cbuf) ;

        // Other functions:
	print (FILE *ofp) ;
	int  DeterminePhysicalRecordSize () ;
    } ;

// ---------------------------------------------------------------------
// The class for the Level-0 data record header.  There is one of
// these for each major frame (i.e. one physical record) of all
// ISTP intruments:
class  LZeroDrh
    {
    private:
    public:
	int32   instrument_number ;
	int32   physical_record_number ;
	int32   major_frame_count ;
	char    spacecraft_clock_major[8] ;
	int32   atc_year_start ;
	int32   atc_day_start ;
	int32   atc_millisecond_start ;
	int32   atc_microsecond_start ;
	int32   minor_frames_with_fill ;
	int32   minor_frames_with_sync_error ;
	int32   telemetry_mode ;
	char    minor_frame_quality[POLAR_DRH_MINOR_FRAME_QUALITY_SIZE] ;

	// Constructors:
	LZeroDrh ()
	    {
	    }
	LZeroDrh (char *cbuf) ;

	// Print out the values:
	virtual void print (FILE *ofp)
	   {
	   fprintf (ofp, "unspecified data record header type\n") ;
	   fflush (ofp) ;
	   }
	void set_from_buffer (char *cbuf) ;
    } ;

// ----------------------------------------------------------------------
struct  dbl_vec_polar_str
    {
    double  ra ;
    double  decl ;
    } ;
typedef  struct  dbl_vec_polar_str  dbl_vec_polar ;

// ----------------------------------------------------------------------
struct  dbl_vec_cart_str
    {
    double  x ;
    double  y ;
    double  z ;
    } ;
typedef  struct  dbl_vec_cart_str  dbl_vec_cart ;

// ----------------------------------------------------------------------
class ISTPAttitudeData
   {
   private:
       CDFstatus  cstatus ;
       CDFid      cid ;
   public:
       int       number_recs ;
       double    *ttag ;
       double    *spin_rate ;

       // Note that right ascension and declination are stored in radians,
       // cartesian are in the units defined in the CDF file.
       dbl_vec_polar   *gci_ra_dec ;
       dbl_vec_cart    *gci_cartesian ;
       dbl_vec_polar   *gse_ra_dec ;
       dbl_vec_cart    *gse_cartesian ;
       dbl_vec_polar   *gsm_ra_dec ;
       dbl_vec_cart    *gsm_cartesian ;

       // Constructor(s):
       ISTPAttitudeData (const char *path) ;

       // This transforms a RA/DEC type to a Cartesian type:
       void ra_declination_to_cartesian (dbl_vec_polar *pvec,
           dbl_vec_cart *xyz_vec) ;

       // This interpolates to a specific time:
       void interpolate_data_to_time (double in_time, double *srate,
	   dbl_vec_polar *ra_dec_gci, dbl_vec_cart *cartesian_gci,
	   dbl_vec_polar *ra_dec_gse, dbl_vec_cart *cartesian_gse,
	   dbl_vec_polar *ra_dec_gsm, dbl_vec_cart *cartesian_gsm) ;

       // Destructor:
       ~ISTPAttitudeData () ;
   } ;

// ----------------------------------------------------------------------
class ISTPEphemerisData
   {
   private:
       CDFstatus  cstatus ;
       CDFid      cid ;
   public:
       int       number_recs ;
       double    *ttag ;

       // Note that these in the units defined in the CDF file.
       dbl_vec_cart    *gci_position ;
       dbl_vec_cart    *gci_velocity ;
       dbl_vec_cart    *gse_position ;
       dbl_vec_cart    *gse_velocity ;
       dbl_vec_cart    *gsm_position ;
       dbl_vec_cart    *gsm_velocity ;
       dbl_vec_cart    *sun_vector ;
       double          *distance_er ;
       float           *mlt ;          
       float           *maglat ;          
       float           *lshell ;          

       // Constructor(s):
       ISTPEphemerisData (const char *path) ;

       // This interpolates to a specific time:
       void interpolate_data_to_time (double in_time,
	   dbl_vec_cart *pos_gci,
	   dbl_vec_cart *vel_gci, dbl_vec_cart *pos_gse,
	   dbl_vec_cart *vel_gse, dbl_vec_cart *pos_gsm,
	   dbl_vec_cart *vel_gsm, dbl_vec_cart *sun_vec,
	   double *dist_er, float *i_mlt,
	   float *i_maglat, float *i_lshell) ;

       // Destructor:
       ~ISTPEphemerisData () ;
   } ;

// ----------------------------------------------------------------------
class ISTPSpinPhaseData
   {
   private:
       CDFstatus  cstatus ;
       CDFid      cid ;
   public:
       int       number_recs ;
       int       NPulse ;
       double    *ttag ;
       float     *srate ;

       // Constructor(s):
       ISTPSpinPhaseData (const char *path) ;

       // Destructor:
       ~ISTPSpinPhaseData () ;
   } ;

// -------------------------------------------------------------
// This class is here for convenience - the ISTP files do not
// contain BModel information:
class ISTPModelBField
   {
   private:
   public:
       int       number_recs ;
       double          *ttag ;
       dbl_vec_cart    *gci_bmodel ;
       dbl_vec_cart    *gse_bmodel ;
       dbl_vec_cart    *gsm_bmodel ;

       // Constructor(s):
       ISTPModelBField () ;
       ISTPModelBField (int npts, double *ittags,
	   float *bxgei, float *bygei, float *bzgei,
	   float *bxgse, float *bygse, float *bzgse,
	   float *bxgsm, float *bygsm, float *bzgsm) ;

       // Destructor:
       ~ISTPModelBField () ;
   } ;

#endif // FROM_POLAR

// ---------------------------------------------------------------------
// Utility functions:

extern  int16     istp_get_int16 (char *buf) ;
extern  int32     istp_get_int32 (char *buf) ;

extern int IsThisClusterDataFileName (char *pathname, char *Date,
   ClusterDataFileNameInfo *info) ;

extern int IsThisClusterDataSetName (char *data_set,
    ClusterDataSetNameInfo *info) ;

extern int IsThisClusterDataSet (char *pathname, StringList *CountDirs,
    ClusterDataSetScanInfo *info) ;

extern char *GetClusterDataFileInDataSet (ClusterDataSetScanInfo *info,
    int scraft, int source_type, int file_type, int *ecode) ;

extern char *FindClusterDataSetInDirectory (char *directory,
    char *datep, StringList *CountDirs, ClusterDataSetScanInfo *info) ;

extern StringList *FindClusterDataSetListInDirectory (char *directory,
    char *datep, StringList *CountDirs,
    ClusterDataSetScanInfoList *LInfo) ;

extern StringList *FindClusterDataSetListInDirectories (int ndirs,
    char **idirs, char *datep, StringList *CountDirs,
    ClusterDataSetScanInfoList *LInfo) ;

extern int MatchClusterSourceTypeToInteger (char *name) ;

extern int MatchClusterFileTypeToInteger (char *name) ;

extern int IsThisClusterDataSetList (char *fpath, StringList *CountDirs,
    ClusterDataSetScanInfoList *ilist) ;

int ClusterCountFileExists (char *pathname, char *data_set,
    char **cnt_file) ;

extern int PathIsToClusterCD (char *path, ClusterCDSfdu **sfdu) ;

extern ClusterCDSfdu *ReadClusterCDSfduFile (char *path, int *ecode) ;

double DecodeClusterSfduTime (char *tstring) ;

extern int SDTTimeStampToClusterTime (int JulianDay, double DayTime,
    double *PTime) ;

extern int ClusterTimeToSDTTimeStamp (double PTime,
    int *JulianDay, double *DayTime)  ;

extern char *GoToClusterSFDUValue (char *sptr, char *lptr, int omit_end) ;

extern int PathIsToCD (char *path) ;

extern int PrintClusterVolumeProduction (ClusterVolumeProduction *cp,
    FILE *ofp) ;

extern int InitClusterVolumeProduction (ClusterVolumeProduction *cv) ;
extern int CopyClusterVolumeProduction (ClusterVolumeProduction *icv,
    ClusterVolumeProduction *ocv) ;
extern int ClearClusterVolumeProduction (ClusterVolumeProduction *cv) ;

extern int InitClusterHeaderReference (ClusterHeaderReference *ch) ;
extern int CopyClusterHeaderReference (ClusterHeaderReference *ich,
    ClusterHeaderReference *och) ;
extern int ClearClusterHeaderReference (ClusterHeaderReference *ch) ;

extern int InitClusterCatalogueEntry (ClusterCatalogueEntry *ce) ;
extern int CopyClusterCatalogueEntry (ClusterCatalogueEntry *ice,
    ClusterCatalogueEntry *oce) ;
extern int ClearClusterCatalogueEntry (ClusterCatalogueEntry *ce) ;

extern int InitClusterCDSfdu (ClusterCDSfdu *cp) ;
extern int CopyClusterCDSfdu (ClusterCDSfdu *icp, ClusterCDSfdu *ocp) ;
extern int ClearClusterCDSfdu (ClusterCDSfdu *cp) ;
extern int PrintClusterCDSfdu (ClusterCDSfdu *cp, FILE *ofp) ;

extern int PrintClusterDataSetNameInfo (ClusterDataSetNameInfo *cp,
    FILE *ofp) ;

extern int InitClusterDataSetScanInfo (ClusterDataSetScanInfo *cp) ;
extern int CopyClusterDataSetScanInfo (ClusterDataSetScanInfo *icp,
    ClusterDataSetScanInfo *ocp) ;
extern int ClearClusterDataSetScanInfo (ClusterDataSetScanInfo *cp) ;
extern int PrintClusterDataSetScanInfo (ClusterDataSetScanInfo *cp,
    FILE *ofp) ;

extern int InitClusterDataSetScanInfoList (
    ClusterDataSetScanInfoList *cp) ;
extern int CopyClusterDataSetScanInfoList (
    ClusterDataSetScanInfoList *icpl,
    ClusterDataSetScanInfoList *ocpl) ;
extern int ClearClusterDataSetScanInfoList (
    ClusterDataSetScanInfoList *cp) ;
extern int PrintClusterDataSetScanInfoList (
    ClusterDataSetScanInfoList *cp, FILE *ofp) ;

extern int ClusterChangeAlphbeticCaseInProjectFilename (
    char *fname) ;

extern int  DoesConfigFileExist (const char *basedir,
    const char *subdir, const char *fname, char **pathname) ;

#endif  // CLUSTER_LIB_H

// ---------------------------------------------------------------------
//  end:  cluster_lib.h

