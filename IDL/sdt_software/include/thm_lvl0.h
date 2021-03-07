/* ------------------------------------------------------------- */
/*
 * thm_lvl0.h
 *
 * Declarations required for the handling of THEMIS LZ files.
 * Note that LZ files consist of packets, not frames.
 *
 * There is also some support for "GSE" files (disk files consisting
 * of THEMIS frames, created during during testing before launch).
 */

#ifndef THM_LVL0_H
#define THM_LVL0_H

/* SCCS ID: */
#define SccsId_thm_lvl0_h "@(#)thm_lvl0.h	1.1, 04/21/07"

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>
#include <dirent.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>

/* Minimum and maximum spacecraft IDs: */
#define   THM_FIRST_SPC_ID                   0x151
#define   THM_LAST_SPC_ID                    0x155

/* ------------------------------------------------------------- */
/* Description of THEMIS Level Zero Apid Data Files (2005/09/28):
 *
 * It is assumed (2005/09/28) that each Themis Level-0 Apid file
 * will consist of raw packets of exactly one Apid and that these
 * packets will be time-ordered, and with no duplicate packets.
 *
 * The file format (2005/09/28) is as follows:
 *
 *   1024-byte Header section, defined as follows:
 *
 *      Bytes 00-05:   Themis apid 700 packet 6-byte hdr code:
 *                        17 00 C0 00 03 F9
 *                     This will allow GSE software to read
 *                     the LZ file and, particularly, treat
 *                     the LZ file header as a special apid 700
 *                     packet.
 *
 *      Bytes 06-25:   ASCII Identification string indicating that
 *                     this is a Themis Level-0 Apid file.   As of
 *                     2006/04/18, this string is:
 *
 *                        THEMIS L0 APID File
 *
 *      Bytes 26-29:   Four byte binary MSB unsigned integer,
 *                     containing the SCID.
 *
 *      Bytes 30-33:   Four byte binary MSB unsigned integer,
 *                     containing the APID.
 *
 *      Bytes 34-37:   Four byte binary MSB unsigned integer,
 *                     containing the number of packets in the file.
 *
 *      Bytes 38-41:   Four byte binary MSB unsigned integer,
 *                     containing the full, four-digit Year
 *                     corresponding to the Date of the file.
 *
 *      Bytes 42-45:   Four byte binary MSB unsigned integer,
 *                     containing the Month (1-12),
 *                     corresponding to the Date of the file.
 *
 *      Bytes 46-49:   Four byte binary MSB unsigned integer,
 *                     containing the Day-of-Month (1-31),
 *                     corresponding to the Date of the file.
 *
 *      Bytes 50-53:   Four byte binary MSB unsigned integer,
 *                     containing the Version Number of the file.
 *
 *   Immediately following the file Header are the packets, with
 *   no bytes in residence between consecutive packets.
 *
 *   As 0f 2005/09/28, there are no sections of the file after
 *   the packets.
 *
 */

/* The number of bytes in an LZ APid File Header. */
#define  THM_LZ_FILE_HDR_LENGTH             1024

/* Byte locations of the various header fields: */
#define  THM_LZ_FILE_HDR_PKT_HDR_LOC        0
#define  THM_LZ_FILE_HDR_PKT_HDR_LENGTH     6
#define  THM_LZ_FILE_HDR_ID_STR_LOC         6
#define  THM_LZ_FILE_HDR_ID_STR_LENGTH      20
#define  THM_LZ_FILE_HDR_SCID_LOC           26
#define  THM_LZ_FILE_HDR_APID_LOC           30
#define  THM_LZ_FILE_HDR_NPKT_LOC           34
#define  THM_LZ_FILE_HDR_YEAR_LOC           38
#define  THM_LZ_FILE_HDR_MNTH_LOC           42
#define  THM_LZ_FILE_HDR_MDAY_LOC           46
#define  THM_LZ_FILE_HDR_VRSN_LOC           50

/* ------------------------------------------------------------- */
/* Structures: */

/* Structure containing the header information from an LZ file
 * header.  Here is the structure of the
 */
struct ThmLZFileHdr_struct
    {
    /* The apid 700 packet header bytes: */
    unsigned char  PktHdr[6] ;

    /* The ID string in the file header: */
    char   FileId[THM_LZ_FILE_HDR_ID_STR_LENGTH + 1] ;

    int    Spacecraft ;

    int    Apid ;

    int    NPackets ;

    /* The date of the file: */
    int      Year ;
    int      DayOfYear ;
    int      Month ;
    int      DayOfMonth ;
    int      JulianDay ;

    /* The version number of the file: */
    int      Version ;
    } ;
typedef   struct ThmLZFileHdr_struct  ThmLZFileHdr ;

/* Structure for holding a list of names of LZ Apid files: */
struct ThmLZFileInfo_struct
    {
    /* The full pathname of the LZ file: */
    char           *filename ;

    /* Information read from the header of "filename": */
    ThmLZFileHdr   Hdr ;

    /* Current FILE ptr open for reading: */
    FILE           *ifp ;

    /* The time of the packet which is currently being processed,
     * in seconds after 00:00 UT of the Date stored in "Hdr".
     * If less than -100, no packets have been read.
     */
    double         LastTime ;
    } ;
typedef   struct ThmLZFileInfo_struct  ThmLZFileInfo ;

/* List-Structure for representing a list of LZ files for a given Apid,
 * and Spacecraft, where each such file, itself, is represented by a
 * "ThmLZFileInfo".
 *
 * This list is useful because it allows for the processing of
 * LZ files for TimeSpans which cross day boundaries (i.e. more than
 * one LZ file for the Apid).
 *
 * It is assumed that the order of the files, represented by the "NLZ"
 * entries in "LZInfoList", will be from the earliest represented date
 * to the latest represented date, and will contain only one entry for
 * each represented date.
 */
struct ThmLZAFList_struct
    {
    int    Spacecraft ;         /* 0x151 - 0x155 */
    int    Apid ;
    int    NLZ ;
    ThmLZFileInfo *LZInfoList ;
    } ;
typedef   struct ThmLZAFList_struct  ThmLZAFList ;

/* List-Structure holding a group of Apid LZ file lists ("ThmLZAFList"'s),
 * for a list of Apids (no more than one list per Apid), for one
 * spacecraft.  The array of "ThmLZAFList"'s are expected to be in
 * Apid order and no Apid will be represented by more than one
 * "ThmLZAFList".
 */
struct ThmLZFGroup_struct
    {
    int    Spacecraft ;         /* 0x151 - 0x155 */
    int    NApid ;
    ThmLZAFList *ApidList ;
    } ;
typedef   struct ThmLZFGroup_struct  ThmLZFGroup ;


/* Structure containing information about a THEMIS, GSE file: */
struct ThmGSEInfo_struct
    {
    /* The timespan of the file: */
    /* To separate this module from SDT, we don't use the
     * "sdtlib" "TimeSpanDescription".  We just list the fields
     * separately.
     */
    int      Year ;
    int      DayOfYear ;
    int      Month ;
    int      DayOfMonth ;
    long     StartJulianDay ;
    double   StartTime ;
    long     EndJulianDay ;
    double   EndTime ;

    /* Unix start,end times: */
    long     UnixStartSeconds ;
    long     UnixStartUSeconds ;
    long     UnixEndSeconds ;
    long     UnixEndUSeconds ;
    } ;
typedef   struct ThmGSEInfo_struct  ThmGSEInfo ;

/* ------------------------------------------------------------- */
/* Functions: */

extern int InitThmLZFileHdr (ThmLZFileHdr *lzhdr) ;
extern int FillThmLZFileHdr (ThmLZFileHdr *lzhdr,
    unsigned char *ibuf) ;
extern int CopyThmLZFileHdr (ThmLZFileHdr *ilzhdr,
    ThmLZFileHdr *olzhdr) ;
extern int ClearThmLZFileHdr (ThmLZFileHdr *lzhdr) ;
extern int PrintThmLZFileHdr (ThmLZFileHdr *lzhdr, FILE *ofp) ;
extern int Read_FP_LZFileHeader (FILE *ifp, ThmLZFileHdr *LZHdr) ;

extern int InitThmLZFileInfo (ThmLZFileInfo *lzinfo) ;
extern int FillThmLZFileInfo (ThmLZFileInfo *lzinfo, char *i_fname,
    int RHFlg, ThmLZFileHdr *i_hdr, int LeaveOpenFlg) ;
extern int CopyThmLZFileInfo (ThmLZFileInfo *ilzinfo,
    ThmLZFileInfo *olzinfo) ;
extern int ClearThmLZFileInfo (ThmLZFileInfo *lzinfo) ;
extern int PrintThmLZFileInfo (ThmLZFileInfo *lzinfo, FILE *ofp) ;

extern int InitThmLZAFList (ThmLZAFList *lzal) ;
extern int FillThmLZAFList (ThmLZAFList *lzal, int i_apid,
    int i_spc, int i_nlz, ThmLZFileInfo **i_lzlist) ;
extern int CopyThmLZAFList (ThmLZAFList *ilzal, ThmLZAFList *olzal) ;
extern int ClearThmLZAFList (ThmLZAFList *lzal) ;
extern int PrintThmLZAFList (ThmLZAFList *lzal,
    FILE *ofp) ;

extern int InitThmLZFGroup (ThmLZFGroup *lzg) ;
extern int FillThmLZFGroup (ThmLZFGroup *lzg, int NApids, int *ApidList,
    int NDirs, char **DirList, int SpcId,
    int syear, int smonth, int smday, double ssec,
    int eyear, int emonth, int emday, double esec,
    int *ecode) ;
extern int CopyThmLZFGroup (ThmLZFGroup *ilzg, ThmLZFGroup *olzg) ;
extern int ClearThmLZFGroup (ThmLZFGroup *lzg) ;
extern int PrintThmLZFGroup (ThmLZFGroup *lzg, FILE *ofp) ;

extern int ThmLZCheckDateTime (int year, int month, int mday,
    double sec, int *RJDay, double *RTime) ;

extern int ThmFindLZFilesInDirectory (char *directory, int NApids,
    int *ApidList, int SpcId, int SJDay, int EJDay, int CheckFNames,
    ThmLZFGroup *lzg, int *ecode) ;

extern int ThmThisIsLZFName (char *fname) ;

extern long THMConvertCalendarToJulianDay (int year, int month,
    int day) ;
extern int THMConvertJulianDayToCalendar (long julian_day,
    int *year, int *month, int *day, int *day_of_year) ;
extern int THMConvertDayOfYearToMonthDay (int year, int day_of_year,
    int *month, int *day_of_month) ;
extern int THMConvertMonthDayToDayOfYear(int year, int month, int day,
    int *day_of_year) ;
extern int THMConvertSecondsToClockTime (double in_time, int *ohour,
    int *ominute, int *osecond, int *omillisec) ;
extern int THMNormalizeJulianDayTime (long int *julian_day,
    double *time) ;

extern int InitThmGSEInfo (ThmGSEInfo *ginf) ;
extern int CopyThmGSEInfo (ThmGSEInfo *iginf, ThmGSEInfo *oginf) ;
extern int ClearThmGSEInfo (ThmGSEInfo *ginf) ;
extern int PrintThmGSEInfo (ThmGSEInfo *ginf, FILE *ofp) ;

extern int IsThisThemisGSEDataSet (char *fpath, ThmGSEInfo *ginfo) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif  /* THM_LVL0_H */

/* ------------------------------------------------------------- */
/*  end:  thm_lvl0.h  */
