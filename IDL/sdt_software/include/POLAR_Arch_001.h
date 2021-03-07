/*
 ***********************************************************************
 *
 *     POLAR_Arch_001.h
 *
 *     Author: Winston Teitler.
 *
 ***********************************************************************
 *
 *     C header for POLAR_Arch_001.
 *
 ***********************************************************************
 *
 *     @(#)POLAR_Arch_001.h	1.2    09/10/07    UCB SSL
 *
 ***********************************************************************
 */





/*
 *======================================================================
 *======================================================================
 */





#ifndef  POLAR_ARCH_001_H
#define  POLAR_ARCH_001_H





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
 *     First date for POL_Arch_001.
 */

#define  WT_POL_ARCH_001_DATE_1                "1996/03/16"



/*
 *     Last date for POL_Arch_001.
 */

#define  WT_POL_ARCH_001_DATE_2                "2008/12/31"



/*
 *     Last date for good electric field V12.
 *
 *     After this date, data for Instantaneous electric field and for
 *     Burst electric field is not to be included in the archive.
 */

#define  WT_POL_ARCH_001_LAST_DATE_V12_OK      "2006/02/28"



/*
 *     Codes for the file types.
 *
 *     This refers to the archive files.
 *
 *     Possible file types are: ASCII, Binary, CDF.
 */

#define  WT_POL_ARCH_001_TYPE_ASCII            (1)

#define  WT_POL_ARCH_001_TYPE_BIN              (2)

#define  WT_POL_ARCH_001_TYPE_CDF              (3)



/*
 *     SDT output file name prefix for POL_Arch_001.
 */

#define  WT_POL_ARCH_001_SDT_FILE_PREFIX       "POL_Arch_001_"



/*
 *     Limit for archive file version numbers.
 *
 *     Version numbers must be  >= 0  and  < this limit .
 */

#define  WT_POL_ARCH_001_VERSION_LIM           (100)



/*
 *     Max. number of data components.
 *
 *     Note that this does not include the time tag.
 */

#define  WT_POL_ARCH_001_MAX_DATA_COMPS        (7)



/*
 *     Max. number of "filler" values for a data component.
 */

#define  WT_POL_ARCH_001_MAX_FILLERS           (2)



/*
 *     Number of bytes for an ASCII header line
 *     (excluding the terminating "newline").
 */

#define  WT_POL_ARCH_001_N_BYT_HDR_LINE_1      (63)



/*
 *     Number of bytes for an ASCII header line
 *     (inluding the terminating "newline").
 */

#define  WT_POL_ARCH_001_N_BYT_HDR_LINE_2                              \
           (WT_POL_ARCH_001_N_BYT_HDR_LINE_1 + 1)



/*
 *     Number of ASCII header lines for basic information.
 */

#define  WT_POL_ARCH_001_N_HDR_BASIC           (4)



/*
 *     Number of ASCII header lines for the time tag component.
 */

#define  WT_POL_ARCH_001_N_HDR_T_TAG           (1)



/*
 *     Number of ASCII header lines for the data components.
 */

#define  WT_POL_ARCH_001_N_HDR_DATA                                    \
           WT_POL_ARCH_001_MAX_DATA_COMPS



/*
 *     Number of ASCII header lines for the messages.
 */

#define  WT_POL_ARCH_001_N_HDR_MSGS            (4)



/*
 *     Total number of ASCII header lines.
 */

#define  WT_POL_ARCH_001_N_HDR_LINES                                   \
           (WT_POL_ARCH_001_N_HDR_BASIC +                              \
            WT_POL_ARCH_001_N_HDR_T_TAG +                              \
            WT_POL_ARCH_001_N_HDR_DATA +                               \
            WT_POL_ARCH_001_N_HDR_MSGS)



/*
 *     Total number of bytes for the ASCII header.
 */

#define  WT_POL_ARCH_001_N_BYT_HDR                                     \
           (WT_POL_ARCH_001_N_HDR_LINES *                              \
            WT_POL_ARCH_001_N_BYT_HDR_LINE_2)



/*
 *     Name for time tag component.
 */

#define  WT_POL_ARCH_001_T_TAG_NAME            "Time"



/*
 *     Units for time tag component.
 */

#define  WT_POL_ARCH_001_T_TAG_UNITS           "sec"



/*
 *     Format for ASCII time tag.
 */

#define  WT_POL_ARCH_001_FMT_ASCII_T_TAG       "%12.6f"



/*
 *     Number of bytes for ASCII time tag.
 */

#define  WT_POL_ARCH_001_N_BYTES_ASCII_T_TAG   (12)



/*
 *     Format for ASCII data value.
 */

#define  WT_POL_ARCH_001_FMT_ASCII_D_VAL       " %12.5e"



/*
 *     Number of bytes for ASCII data value.
 */

#define  WT_POL_ARCH_001_N_BYTES_ASCII_D_VAL   (13)



/*
 *     Min. absolute value for a non-zero data value.
 */

#define  WT_POL_ARCH_001_MIN_ABS               (1.001e-37)



/*
 *     Max. absolute value for a data value.
 */

#define  WT_POL_ARCH_001_MAX_ABS               (0.999e+37)



/*
 *     Buffer size.
 *
 *     This is an excessive size, more than large enough for any input
 *     or output data lines.
 */

#define  WT_POL_ARCH_001_BUFF_LEN              (1000)



/*
 *     Auxiliary dummy data component structure.
 */

#define  WT_DUMMY_POL_ARCH_001_COMP                                    \
           {                                                           \
             WT_EMPTY_STRING,                                          \
             WT_EMPTY_STRING,                                          \
             0,                                                        \
             { 0.0, 0.0, },                                            \
             WT_EMPTY_STRING,                                          \
           }





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
 *     This structure contains values for processing an ASCII archive
 *     file.
 */


   struct WT_T_POL_Arch_001_ASCII_File_struct


     {


/*
 *     File mode.
 */

       char *mode ;


/*
 *     File info.
 */

       WT_T_File_Info file_info ;


     } ;


   typedef
       struct WT_T_POL_Arch_001_ASCII_File_struct
       WT_T_POL_Arch_001_ASCII_File ;



/*
 *======================================================================
 *======================================================================
 */



/*
 *     This structure contains values for processing a binary archive
 *     file.
 */


   struct WT_T_POL_Arch_001_Bin_File_struct


     {


/*
 *     File mode.
 */

       char *mode ;


/*
 *     File info.
 */

       WT_T_File_Info file_info ;


     } ;


   typedef
       struct WT_T_POL_Arch_001_Bin_File_struct
       WT_T_POL_Arch_001_Bin_File ;



/*
 *======================================================================
 *======================================================================
 */



/*
 *     This structure contains values for processing a CDF archive file.
 */


   struct WT_T_POL_Arch_001_CDF_File_struct


     {


/*
 *     CDF identifier.
 */

       CDFid id ;


/*
 *     Variable number for the time tag.
 */

       long int t_tag_var_num ;


/*
 *     Variable number for the data values.
 */

       long int value_var_num ;


/*
 *     Record number to write data.
 */

       long int var_rec_num ;


     } ;


   typedef
       struct WT_T_POL_Arch_001_CDF_File_struct
       WT_T_POL_Arch_001_CDF_File ;



/*
 *======================================================================
 *======================================================================
 */



/*
 *     This structure contains values for processing an archive file.
 */


   struct WT_T_POL_Arch_001_File_Access_Info_struct


     {


/*
 *     File name.
 *
 *     Must include directory path information, if needed.
 *
 *     However, note that for a CDF file, the file name should not
 *     include the .cdf suffix.
 */

       char *file_name ;


/*
 *     File creation flag.
 *
 *     Must be one of the following:
 *
 *       WT_TRUE   =  Create a new file.
 *
 *       WT_FALSE  =  Open an existing file.
 */

       int create_new_file ;


/*
 *     File type.
 *
 *     Must be one of the following:
 *
 *       WT_POL_ARCH_001_TYPE_ASCII  =  ASCII file.
 *
 *       WT_POL_ARCH_001_TYPE_BIN    =  Binary file.
 *
 *       WT_POL_ARCH_001_TYPE_CDF    =  CDF file.
 */

       int file_type ;


/*
 *     Values for an ASCII file.
 */

       WT_T_POL_Arch_001_ASCII_File ASCII_file_values ;


/*
 *     Values for a binary file.
 */

       WT_T_POL_Arch_001_Bin_File Bin_file_values ;


/*
 *     Values for a CDF file.
 */

       WT_T_POL_Arch_001_CDF_File CDF_file_values ;


     } ;


   typedef
       struct WT_T_POL_Arch_001_File_Access_Info_struct
       WT_T_POL_Arch_001_File_Access_Info ;



/*
 *======================================================================
 *======================================================================
 */



/*
 *     This structure contains values that characterize a data component
 *     of an archive quantity.
 *
 *     The time tag is not considered a data component.
 *
 *     Each data component corresponds to 1 SDT input quantity.
 *
 *     For example, there may be 3 SDT input quantities, corresponding
 *     to the X, Y, Z components of a vector; the archive quantity will
 *     have a time tag, plus 3 data components; each data component
 *     corresponds to one of the vector components.
 */


   struct WT_T_POL_Arch_001_Comp_struct


     {


/*
 *     Name of the data component.
 */

       char *name ;


/*
 *     Units for the data component.
 */

       char *units ;


/*
 *     Number of "filler" values.
 *
 *     Must be >= 0 and <= WT_POL_ARCH_001_MAX_FILLERS.
 */

       int n_fillers ;


/*
 *     "Filler" values for the data component.
 */

       double filler[WT_POL_ARCH_001_MAX_FILLERS] ;


/*
 *     Name of the SDT input quantity.
 */

       char *i_q_name ;


     } ;


   typedef
       struct WT_T_POL_Arch_001_Comp_struct
           WT_T_POL_Arch_001_Comp ;



/*
 *======================================================================
 *======================================================================
 */



/*
 *     This structure contains values that characterize an archive
 *     quantity.
 *
 *     Each archive quantity corresponds to 1 output file.
 */


   struct WT_T_POL_Arch_001_Quant_struct


     {


/*
 *     Name of the archive quantity.
 */

       char *name ;


/*
 *     Number of data components.
 *
 *     This does not include the time tag.
 *
 *     Must be >= 1 and <= WT_POL_ARCH_001_MAX_DATA_COMPS.
 */

       int n_data_comps ;


/*
 *     Data components.
 */

       WT_T_POL_Arch_001_Comp comp[WT_POL_ARCH_001_MAX_DATA_COMPS] ;


     } ;


   typedef
       struct WT_T_POL_Arch_001_Quant_struct
           WT_T_POL_Arch_001_Quant ;



/*
 *======================================================================
 *======================================================================
 */



/*
 *     This structure contains values for an archive file header.
 */



/*
 *     The archive file header information is included in the archive
 *     file.
 *
 *
 *
 *     The manner in which the header information is included in the
 *     archive file depends on the type of the file.
 *
 *
 *
 *----------------------------------------------------------------------
 *
 *
 *
 *     For an ASCII file, the header information is included in the form
 *     of text lines that appear at the beginning of the file.
 *
 *
 *
 *     Each header line has a "newline" at the end.
 *
 *     Fields within header lines are separated from one another by 1
 *     or more spaces (blanks).
 *
 *     There can be 1 or more spaces (blanks) before the first field of
 *     a line, but this is not required; similarly for 1 or more spaces
 *     after the last field of a line.
 *
 *     Normally there are no spaces embedded within fields.  An
 *     exception occurs for the messages containing further information
 *     about the file; each message corresponds to 1 message line of the
 *     header, and each message is considered to be one single data
 *     field, even though normally there will be embedded blanks.
 *
 *
 *
 *     The header consists of 16 lines, of 64 characters each, including
 *     the "newline" at the end of the line; so, each line of the header
 *     has 63 characters before the "newline".
 *
 *     The last field of a header line is followed by a sufficient
 *     number of spaces, to assure that there are 63 characters before
 *     the "newline".
 *
 *     Some header lines may be "empty", i.e., they have no fields with
 *     any values.  An "empty" header line consists of 63 spaces,
 *     followed by the "newline".
 *
 *
 *
 *     The header lines are as follows, and contain the indicated
 *     fields:
 *
 *
 *
 *      1) Line 1.
 *
 *
 *          1) Quantity name.
 *
 *             This is the name of the data quantity contained in the
 *             file.
 *
 *
 *
 *      2) Line 2.
 *
 *
 *          1) Date created.
 *
 *             This is the local date when the file was created.
 *
 *             The format is:
 *
 *               YYYY/MM/DD
 *
 *
 *          2) Time created.
 *
 *             This is the local time of day when the file was created.
 *
 *             The format is:
 *
 *               HH:MM:SS
 *
 *
 *          3) Version number.
 *
 *             The version number of the file is an integer  >= 0
 *             and  <= 99 .
 *
 *
 *
 *      3) Line 3.
 *
 *
 *          1) Date of the data, as JDN.
 *
 *             This is the UT date of the data, as a Julian Day
 *             Number.
 *
 *
 *          2) Date of the data, as year/month/day.
 *
 *             This is the UT date of the data, in the format:
 *
 *               YYYY/MM/DD
 *
 *
 *          3) "Unix time" for the date of the data.
 *
 *             This is the time elapsed from the "Unix time" epoch
 *             (1970/01/01 00:00:00 UT)
 *             to the date of the data at 00:00:00 UT.
 *
 *             Units are seconds.
 *
 *
 *          4) "1900 time" for the date of the data.
 *
 *             This is the time elapsed from the "1900 time" epoch
 *             (1900/01/01 00:00:00 UT)
 *             to the date of the data at 00:00:00 UT.
 *
 *             Units are seconds.
 *
 *
 *      4) Line 4.
 *
 *
 *          1) Number of data values per data point.
 *
 *             This number does not include the time tag.
 *
 *             The number of data values per data point is  >= 1 .
 *
 *
 *          2) Number of data points.
 *
 *             The number of data points is  >= 0 .
 *
 *
 *          3) Time tag of the first data point in the file.
 *
 *             This is the number of seconds since 00:00:00 UT of the
 *             date of the data.
 *
 *
 *          4) Time tag of the last data point in the file.
 *
 *             This is the number of seconds since 00:00:00 UT of the
 *             date of the data.
 *
 *         If the file has 0 data points, the first and last time tags
 *         are each set to 0.
 *
 *         If the file has 1 data point, the first and last time tags
 *         are each set to the time tag for the only data point.
 *
 *
 *          5) Number of messages.
 *
 *             The number of messages is  >= 0 .
 *
 *
 *
 *      5) Line 5.
 *
 *
 *          1) Component name for the time tag of each data point.
 *
 *
 *          2) Units for the time tag.
 *
 *
 *
 *      6) Line 6.
 *
 *
 *          1) Component name for the 1st. data value of each data
 *             point.
 *
 *
 *          2) Units for the 1st. data value.
 *
 *
 *          3) Number of "filler" values for the 1st. data value.
 *
 *
 *          4) 1st. "filler" value for the 1st. data value, if it
 *             exists.
 *
 *
 *          5) 2nd. "filler" value for the 1st. data value, if it
 *             exists.
 *
 *
 *
 *      7) Lines 7 - 12.
 *
 *
 *         Similar to Line 6, for the 2nd. through the 7th. data value
 *         for each data point.
 *
 *
 *         If there are fewer than 7 data values for each data point,
 *         the unused last of these lines are left empty.
 *
 *
 *
 *      8) Lines 13 - 16.
 *
 *
 *         Each of these lines may contain a message with further
 *         information about the file.
 *
 *
 *         If there are fewer than 4 lines of messages required, the
 *         unused last of these lines are left empty.
 *
 *
 *
 *----------------------------------------------------------------------
 *
 *
 *
 *     For a binary file, the header information is included in the form
 *     of text lines that appear at the beginning of the file.
 *
 *
 *
 *     The header for a binary file has the same format as the header
 *     for an ASCII file.
 *
 *
 *
 *----------------------------------------------------------------------
 *
 *
 *
 *     For a CDF file, the header information is included in the form
 *     of global attributes.
 *
 *
 *
 *     The items of header information are described in the section for
 *     an ASCII file.
 *
 *
 *
 *     It should be noted that each attribute entry that is a character
 *     string contains only the characters in the text of the string;
 *     there is no "newline" terminator and no "null character"
 *     terminator.
 *
 *     For example, the text
 *
 *       abc
 *
 *     would appear as an attribute entry of character type, consisting
 *     of exactly 3 data elements (3 characters).
 *
 *
 *
 *     The attributes are as follows, and contain the indicated entries;
 *     some numeric (not character string) entries (e.g., Date created)
 *     contain more than 1 data element:
 *
 *
 *
 *      1) QUANTITY_NAME
 *
 *
 *          1) Quantity name.
 *
 *             CDF_CHAR.
 *
 *
 *
 *      2) CREATION_DATE
 *
 *
 *          1) Date created.
 *
 *             CDF_INT4.
 *
 *
 *              1) Date created, year.
 *
 *                 E.g., 2007.
 *
 *
 *              2) Date created, month.
 *
 *                 E.g., 12.
 *
 *
 *              3) Date created, day of month.
 *
 *                 E.g., 31.
 *
 *
 *
 *      3) CREATION_TIME
 *
 *
 *          1) Time created.
 *
 *             CDF_INT4.
 *
 *
 *              1) Time created, hours.
 *
 *                 E.g., 23.
 *
 *
 *              2) Time created, minutes.
 *
 *                 E.g., 5.
 *
 *
 *              3) Time created, seconds.
 *
 *                 E.g., 59.
 *
 *
 *
 *      4) VERSION_NUMBER
 *
 *
 *          1) Version number.
 *
 *             CDF_INT4.
 *
 *
 *
 *      5) DATA_DATE_JDN
 *
 *
 *          1) Date of the data, as JDN.
 *
 *             CDF_INT4.
 *
 *
 *
 *      6) DATA_DATE_YMD
 *
 *
 *          1) Date of the data, as year/month/day.
 *
 *             CDF_INT4.
 *
 *
 *              1) Date of the data, year.
 *
 *                 E.g., 2000.
 *
 *
 *              2) Date of the data, month.
 *
 *                 E.g., 7.
 *
 *
 *              3) Date of the data, day of month.
 *
 *                 E.g., 4.
 *
 *
 *
 *      7) DATA_DATE_UNIX_TIME
 *
 *
 *          1) "Unix time" for the date of the data.
 *
 *             CDF_REAL8.
 *
 *
 *
 *      8) DATA_DATE_1900_TIME
 *
 *
 *          1) "1900 time" for the date of the data.
 *
 *             CDF_REAL8.
 *
 *
 *
 *      9) N_DATA_VALUES_PER_POINT
 *
 *
 *          1) Number of data values per data point.
 *
 *             CDF_INT4.
 *
 *
 *
 *     10) N_DATA_POINTS
 *
 *
 *          1) Number of data points.
 *
 *             CDF_INT4.
 *
 *
 *
 *     11) FIRST_TIME_TAG
 *
 *
 *          1) Time tag of the first data point in the file.
 *
 *             CDF_REAL8.
 *
 *
 *
 *     12) LAST_TIME_TAG
 *
 *
 *          1) Time tag of the last data point in the file.
 *
 *             CDF_REAL8.
 *
 *
 *
 *     13) N_MESSAGES
 *
 *
 *          1) Number of messages.
 *
 *             CDF_INT4.
 *
 *
 *
 *     14) TIME_TAG_COMP_INFO
 *
 *
 *          1) Information for the time tag.
 *
 *
 *              1) Component name for the time tag.
 *
 *                 CDF_CHAR.
 *
 *
 *              2) Units for the time tag.
 *
 *                 CDF_CHAR.
 *
 *
 *
 *     15) DATA_VALUE_COMP_INFO
 *
 *
 *          1) Information for the data values.
 *
 *             The group of attribute entries that follows occurs once
 *             for each of the data values.
 *
 *
 *              1) Component name for the data value.
 *
 *                 CDF_CHAR.
 *
 *
 *              2) Units for the data value.
 *
 *                 CDF_CHAR.
 *
 *
 *              3) Number of "filler" values for the data value.
 *
 *                 CDF_INT4.
 *
 *
 *                 The group of attribute entries that follows occurs
 *                 once for each of the "filler" values.
 *
 *
 *                  1) "Filler" value.
 *
 *                     CDF_REAL8.
 *
 *
 *
 *     16) MESSAGES
 *
 *
 *          1) Information for the data values.
 *
 *             The group of attribute entries that follows occurs once
 *             for each of the messages.
 *
 *
 *              1) Message.
 *
 *                 CDF_CHAR.
 *
 *
 *
 *----------------------------------------------------------------------
 */



   struct WT_T_POL_Arch_001_File_Header_struct


     {


/*
 *     Archive quantity.
 */

       WT_T_POL_Arch_001_Quant *quant ;


/*
 *     Date created.
 *
 *     This is the date when the run was made to create the file.
 *
 *     Year, month, day of month.
 */

       int cre_date_year ;

       int cre_date_month ;

       int cre_date_d_o_m ;


/*
 *     Time created.
 *
 *     This is the time of day when the run was made to create the file.
 *
 *     Hours, minutes, seconds.
 *
 *     Note that the seconds are integer.
 *
 *     The date and time should correspond to a valid, normalized date
 *     and time.
 */

       int cre_time_hour ;

       int cre_time_min ;

       int cre_time_sec ;


/*
 *     File version number.
 *
 *     Must be >= 0 and < WT_POL_ARCH_001_VERSION_LIM.
 */

       int version_n ;


/*
 *     Date of the data.
 *
 *     JDN.
 */

       long int data_date_jdn ;


/*
 *     Date of the data.
 *
 *     Year, month, day of month.
 */

       int data_date_year ;

       int data_date_month ;

       int data_date_d_o_m ;


/*
 *     "Unix time" for the date of the data.
 *
 *     This is the time elapsed from the "Unix time" epoch
 *     (1970/01/01 00:00:00 UT)
 *     to the date of the data at 00:00:00 UT.
 *
 *     Units are seconds.
 */

       double data_date_unix_time ;


/*
 *     "1900 time" for the date of the data.
 *
 *     This is the time elapsed from the "1900 time" epoch
 *     (1900/01/01 00:00:00 UT)
 *     to the date of the data at 00:00:00 UT.
 *
 *     Units are seconds.
 */

       double data_date_1900_time ;


/*
 *     Number of data values per data point.
 *
 *     This is the same as the number of data components in the archive
 *     quantity.
 *
 *     Must be >= 1 and <= WT_POL_ARCH_001_MAX_DATA_COMPS.
 */

       int n_val_per_pt ;


/*
 *     Number of data points.
 *
 *     Must be >= 0.
 */

       int n_pts ;


/*
 *     Time tags for the first and for the last data points.
 *
 *     See the structure for a data point, for the definition of the
 *     time tags.
 *
 *     If the file has 0 data points, the first and last time tags are
 *     each set to 0.
 *
 *     If the file has 1 data point, the first and last time tags are
 *     each set to the time tag for the only data point.
 */

       double first_t_tag ;

       double last_t_tag ;


/*
 *     Number of messages.
 *
 *     Must be >= 0 and <= WT_POL_ARCH_001_N_HDR_MSGS.
 */

       int n_msgs ;


/*
 *     Messages.
 */

       char *msg[WT_POL_ARCH_001_N_HDR_MSGS] ;


     } ;


   typedef
       struct WT_T_POL_Arch_001_File_Header_struct
       WT_T_POL_Arch_001_File_Header ;



/*
 *======================================================================
 *======================================================================
 */



/*
 *     This structure contains values for an archive file data point.
 */



/*
 *     The manner in which the data points are included in the archive
 *     file depends on the type of the file.
 *
 *
 *
 *----------------------------------------------------------------------
 *
 *
 *
 *
 *     For an ASCII file, the data points are included in the form of
 *     text lines that appear following the file header.
 *
 *
 *
 *     There is 1 data line for each data point in the file.
 *
 *
 *
 *     The data lines are written in chronological sequence; the
 *     time tags are in increasing sequence, with no duplicates.
 *
 *
 *
 *     Each data line has a "newline" at the end.
 *
 *     Fields within data lines are separated from one another by 1
 *     or more spaces (blanks).
 *
 *     There can be 1 or more spaces (blanks) before the first field of
 *     a line, but this is not required; similarly for 1 or more spaces
 *     after the last field of a line.
 *
 *
 *
 *     Each data line contains the following fields:
 *
 *
 *
 *      1) Time tag.
 *
 *
 *
 *      2) A number of data values for that time tag.
 *
 *
 *
 *     The time tag values are written with "f" format; the data
 *     values are written with "e" format.
 *
 *
 *
 *     The time tag values mean seconds after 00:00:00 UT for the UT
 *     date of the data, indicated in the file header.
 *
 *
 *
 *     The time tag values are between 0 (incl.) and 86400.  The
 *     value of 86400 can be reached only for the last day of the
 *     archive.
 *
 *
 *
 *     The number of data values is included in the file header.
 *
 *
 *
 *     Each data value is a separate field.
 *
 *
 *
 *----------------------------------------------------------------------
 *
 *
 *     For a binary file, the data points are included in the form of
 *     binary records that appear following the file header.
 *
 *
 *
 *     There is 1 data record for each data point in the file.
 *
 *
 *
 *     The data records are written in chronological sequence; the
 *     time tags are in increasing sequence, with no duplicates.
 *
 *
 *
 *     Each data record contains the following:
 *
 *
 *
 *      1) Time tag.
 *
 *         Double.
 *
 *
 *
 *      2) A number of data values for that time tag.
 *
 *         Float.
 *
 *
 *
 *     The time tag is described above (ASCII file).
 *
 *
 *
 *     The number of data values is included in the file header.
 *
 *
 *
 *----------------------------------------------------------------------
 *
 *
 *     For a CDF file, the data points are included in the form of data
 *     records consisting of zVariables.
 *
 *
 *
 *     There is 1 data record for each data point in the file.
 *
 *
 *
 *     The data records are written in chronological sequence; the
 *     time tags are in increasing sequence, with no duplicates.
 *
 *
 *
 *     The zVariables in each data record are as follows:
 *
 *
 *
 *      1) TIME_TAG
 *
 *
 *          1) Time tag.
 *
 *             CDF_REAL8.
 *
 *             Number of dimensions: 0.
 *
 *
 *
 *      2) DATA_VALUES
 *
 *
 *          1) Data values.
 *
 *             CDF_REAL4.
 *
 *             Number of dimensions: 1.
 *
 *             Dimension size: Number of data values per data point.
 *
 *
 *
 *     The time tag is described above (ASCII file).
 *
 *
 *
 *----------------------------------------------------------------------
 */



   struct WT_T_POL_Arch_001_File_Data_Point_struct


     {


/*
 *     Time tag.
 *
 *     Referred to the date of the data in the file header
 *     (seconds since 00:00:00 UT).
 */

       double time_tag ;


/*
 *     Number of data values.
 *
 *     This is the same as the number of data values per data point in
 *     the file header.
 *
 *     Must be >= 1 and <= WT_POL_ARCH_001_MAX_DATA_COMPS.
 */

       int n_data_vals ;


/*
 *     Data point values.
 */


       double value[WT_POL_ARCH_001_MAX_DATA_COMPS] ;


     } ;


   typedef
       struct WT_T_POL_Arch_001_File_Data_Point_struct
       WT_T_POL_Arch_001_File_Data_Point ;





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Arrays.
 *
 ***********************************************************************
 */



/*
 *     Archive quantities for POL_Arch_001.
 */


   static WT_T_POL_Arch_001_Quant
       WT_A_POL_Arch_001_quant[] =


     {


/*
 *     Attitude.
 */


       {
         "POLAR_S_C_Attitude_GEI",
         3,
         {
           {
             "Att_GEI_X",
             "-",
             0,
             { 0.0, 0.0, },
             "SpinAxisECI_X",
           },
           {
             "Att_GEI_Y",
             "-",
             0,
             { 0.0, 0.0, },
             "SpinAxisECI_Y",
           },
           {
             "Att_GEI_Z",
             "-",
             0,
             { 0.0, 0.0, },
             "SpinAxisECI_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_S_C_Attitude_GSE",
         3,
         {
           {
             "Att_GSE_X",
             "-",
             0,
             { 0.0, 0.0, },
             "SpinAxisGSE_X",
           },
           {
             "Att_GSE_Y",
             "-",
             0,
             { 0.0, 0.0, },
             "SpinAxisGSE_Y",
           },
           {
             "Att_GSE_Z",
             "-",
             0,
             { 0.0, 0.0, },
             "SpinAxisGSE_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_S_C_Attitude_GSM",
         3,
         {
           {
             "Att_GSM_X",
             "-",
             0,
             { 0.0, 0.0, },
             "SpinAxisGSM_X",
           },
           {
             "Att_GSM_Y",
             "-",
             0,
             { 0.0, 0.0, },
             "SpinAxisGSM_Y",
           },
           {
             "Att_GSM_Z",
             "-",
             0,
             { 0.0, 0.0, },
             "SpinAxisGSM_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


/*
 *     Orbit.
 */


       {
         "POLAR_S_C_Position_GEI",
         3,
         {
           {
             "Pos_GEI_X",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarPositionECI_X",
           },
           {
             "Pos_GEI_Y",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarPositionECI_Y",
           },
           {
             "Pos_GEI_Z",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarPositionECI_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_S_C_Position_GSE",
         3,
         {
           {
             "Pos_GSE_X",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarPositionGSE_X",
           },
           {
             "Pos_GSE_Y",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarPositionGSE_Y",
           },
           {
             "Pos_GSE_Z",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarPositionGSE_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_S_C_Position_GSM",
         3,
         {
           {
             "Pos_GSM_X",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarPositionGSM_X",
           },
           {
             "Pos_GSM_Y",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarPositionGSM_Y",
           },
           {
             "Pos_GSM_Z",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarPositionGSM_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_S_C_Velocity_GEI",
         3,
         {
           {
             "Vel_GEI_X",
             "Km/sec",
             0,
             { 0.0, 0.0, },
             "PolarVelocityECI_X",
           },
           {
             "Vel_GEI_Y",
             "Km/sec",
             0,
             { 0.0, 0.0, },
             "PolarVelocityECI_Y",
           },
           {
             "Vel_GEI_Z",
             "Km/sec",
             0,
             { 0.0, 0.0, },
             "PolarVelocityECI_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_S_C_Velocity_GSE",
         3,
         {
           {
             "Vel_GSE_X",
             "Km/sec",
             0,
             { 0.0, 0.0, },
             "PolarVelocityGSE_X",
           },
           {
             "Vel_GSE_Y",
             "Km/sec",
             0,
             { 0.0, 0.0, },
             "PolarVelocityGSE_Y",
           },
           {
             "Vel_GSE_Z",
             "Km/sec",
             0,
             { 0.0, 0.0, },
             "PolarVelocityGSE_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_S_C_Velocity_GSM",
         3,
         {
           {
             "Vel_GSM_X",
             "Km/sec",
             0,
             { 0.0, 0.0, },
             "PolarVelocityGSM_X",
           },
           {
             "Vel_GSM_Y",
             "Km/sec",
             0,
             { 0.0, 0.0, },
             "PolarVelocityGSM_Y",
           },
           {
             "Vel_GSM_Z",
             "Km/sec",
             0,
             { 0.0, 0.0, },
             "PolarVelocityGSM_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_S_C_Orbit_Other",
         7,
         {
           {
             "Dist",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarSPcraftDist",
           },
           {
             "MLT",
             "Hours",
             0,
             { 0.0, 0.0, },
             "PolarMLT",
           },
           {
             "Mag_Lat",
             "Deg",
             0,
             { 0.0, 0.0, },
             "PolarMagLat",
           },
           {
             "L_Shell",
             "Re",
             0,
             { 0.0, 0.0, },
             "PolarLShell",
           },
           {
             "Geo_Lat",
             "Deg",
             0,
             { 0.0, 0.0, },
             "PolarLatitude",
           },
           {
             "Geo_Long",
             "Deg",
             0,
             { 0.0, 0.0, },
             "PolarLongitude",
           },
           {
             "I_Lat",
             "Deg",
             0,
             { 0.0, 0.0, },
             "PolarInvariantLatitude",
           },
         },
       },


/*
 *     Spacecraft potential and plasma density, Instantaneous.
 */


       {
         "POLAR_S_C_Pot_P_Den",
         2,
         {
           {
             "S_C_Pot",
             "Volt",
             0,
             { 0.0, 0.0, },
             "POLAR_SPPD_S_C_Pot1234",
           },
           {
             "n",
             "cm^(-3)",
             2,
             { 1.0e-20, 1.0e+20, },
             "POLAR_SPPD_n_1234",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


/*
 *     Spacecraft potential and plasma density, Spin Period.
 */


       {
         "POLAR_S_C_Pot_P_Den_A",
         2,
         {
           {
             "S_C_Pot_A",
             "Volt",
             0,
             { 0.0, 0.0, },
             "POLAR_SPPD_S_C_Pot1234_A",
           },
           {
             "n_A",
             "cm^(-3)",
             2,
             { 1.0e-20, 1.0e+20, },
             "POLAR_SPPD_n_1234_A",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


/*
 *     Spacecraft potential and plasma density, Burst.
 */


       {
         "POLAR_S_C_Pot_P_Den_B",
         2,
         {
           {
             "S_C_Pot_B",
             "Volt",
             0,
             { 0.0, 0.0, },
             "POLAR_SPPD_S_C_Pot1234_B",
           },
           {
             "n_B",
             "cm^(-3)",
             2,
             { 1.0e-20, 1.0e+20, },
             "POLAR_SPPD_n_1234_B",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


/*
 *     Electric field E_0, Instantaneous.
 */


       {
         "POLAR_E_0_GSE",
         3,
         {
           {
             "E_0_GSE_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_GSE_X",
           },
           {
             "E_0_GSE_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_GSE_Y",
           },
           {
             "E_0_GSE_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_GSE_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_E_0_GSM",
         3,
         {
           {
             "E_0_GSM_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_GSM_X",
           },
           {
             "E_0_GSM_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_GSM_Y",
           },
           {
             "E_0_GSM_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_GSM_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


/*
 *     Electric field E_0, Spin Period.
 */


       {
         "POLAR_E_0_A_GSE",
         3,
         {
           {
             "E_0_A_GSE_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_A_34_GSE_X",
           },
           {
             "E_0_A_GSE_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_A_34_GSE_Y",
           },
           {
             "E_0_A_GSE_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_A_34_GSE_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_E_0_A_GSM",
         3,
         {
           {
             "E_0_A_GSM_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_A_34_GSM_X",
           },
           {
             "E_0_A_GSM_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_A_34_GSM_Y",
           },
           {
             "E_0_A_GSM_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_A_34_GSM_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


/*
 *     Electric field E_0, Burst.
 */


       {
         "POLAR_E_0_B_GSE",
         3,
         {
           {
             "E_0_B_GSE_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_B_GSE_X",
           },
           {
             "E_0_B_GSE_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_B_GSE_Y",
           },
           {
             "E_0_B_GSE_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_B_GSE_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_E_0_B_GSM",
         3,
         {
           {
             "E_0_B_GSM_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_B_GSM_X",
           },
           {
             "E_0_B_GSM_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_B_GSM_Y",
           },
           {
             "E_0_B_GSM_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_0_B_GSM_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


/*
 *     Electric field E_Dot0, Instantaneous.
 */


       {
         "POLAR_E_Dot0_GSE",
         3,
         {
           {
             "E_Dot0_GSE_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_GSE_X",
           },
           {
             "E_Dot0_GSE_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_GSE_Y",
           },
           {
             "E_Dot0_GSE_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_GSE_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_E_Dot0_GSM",
         3,
         {
           {
             "E_Dot0_GSM_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_GSM_X",
           },
           {
             "E_Dot0_GSM_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_GSM_Y",
           },
           {
             "E_Dot0_GSM_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_GSM_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


/*
 *     Electric field E_Dot0, Spin Period.
 */


       {
         "POLAR_E_Dot0_A_GSE",
         3,
         {
           {
             "E_Dot0_A_GSE_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_A_34_GSE_X",
           },
           {
             "E_Dot0_A_GSE_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_A_34_GSE_Y",
           },
           {
             "E_Dot0_A_GSE_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_A_34_GSE_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_E_Dot0_A_GSM",
         3,
         {
           {
             "E_Dot0_A_GSM_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_A_34_GSM_X",
           },
           {
             "E_Dot0_A_GSM_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_A_34_GSM_Y",
           },
           {
             "E_Dot0_A_GSM_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_A_34_GSM_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


/*
 *     Electric field E_Dot0, Burst.
 */


       {
         "POLAR_E_Dot0_B_GSE",
         3,
         {
           {
             "E_Dot0_B_GSE_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_B_GSE_X",
           },
           {
             "E_Dot0_B_GSE_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_B_GSE_Y",
           },
           {
             "E_Dot0_B_GSE_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_B_GSE_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


       {
         "POLAR_E_Dot0_B_GSM",
         3,
         {
           {
             "E_Dot0_B_GSM_X",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_B_GSM_X",
           },
           {
             "E_Dot0_B_GSM_Y",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_B_GSM_Y",
           },
           {
             "E_Dot0_B_GSM_Z",
             "mV/m",
             0,
             { 0.0, 0.0, },
             "POLAR_E_Dot0_B_GSM_Z",
           },
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
           WT_DUMMY_POL_ARCH_001_COMP,
         },
       },


     } ;



/*
 *     Number of archive quantities.
 */

#define  WT_POL_ARCH_001_N_QUANTS              (25)



/*
 *----------------------------------------------------------------------
 */



/*
 *     Names of CDF file attributes for POL_Arch_001.
 *
 *     The order of the array elements is important.
 */


   static char*
       WT_A_POL_Arch_001_cdf_attr_name[] =


     {


       "QUANTITY_NAME",

       "CREATION_DATE",

       "CREATION_TIME",

       "VERSION_NUMBER",

       "DATA_DATE_JDN",

       "DATA_DATE_YMD",

       "DATA_DATE_UNIX_TIME",

       "DATA_DATE_1900_TIME",

       "N_DATA_VALUES_PER_POINT",

       "N_DATA_POINTS",

       "FIRST_TIME_TAG",

       "LAST_TIME_TAG",

       "N_MESSAGES",

       "TIME_TAG_COMP_INFO",

       "DATA_VALUE_COMP_INFO",

       "MESSAGES",


     } ;



/*
 *     Number of attributes for CDF file.
 */

#define  WT_POL_ARCH_001_N_CDF_ATTRS           (16)



/*
 *----------------------------------------------------------------------
 */



/*
 *     Names of CDF file variables for POL_Arch_001.
 *
 *     The order of the array elements is important.
 */


   static char*
       WT_A_POL_Arch_001_cdf_var_name[] =


     {


       "TIME_TAG",

       "DATA_VALUES",


     } ;



/*
 *     Number of variables for CDF file.
 */

#define  WT_POL_ARCH_001_N_CDF_VARS            (2)





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Macros.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     CDF.
 *
 *----------------------------------------------------------------------
 */



/*
 *     CDF status text, any stream.
 *
 *     Writes the source file name and line number, and the specific
 *     text.
 *
 *     "status" must be a CDF status code (whose text will be written).
 *
 *     "stream" must be a stream for the output.
 *
 *     "text" must be a character pointer.
 */


#define                                                                \
                                                                       \
   WT_M_Write_CDF_Status_Text(status, stream, text)                    \
                                                                       \
       do                                                              \
                                                                       \
         {                                                             \
                                                                       \
           (void) CDFgetStatusText(status, text) ;                     \
                                                                       \
           (void) fprintf((stream),                                    \
                      "\n"                                             \
                      "\n"                                             \
                      "CDF STATUS TEXT\n"                              \
                      "    File: <%s>\n"                               \
                      "    Line: %d\n"                                 \
                      "%s\n",                                          \
                      __FILE__,                                        \
                      __LINE__,                                        \
                      (text)) ;                                        \
                                                                       \
           (void) fflush(stream) ;                                     \
                                                                       \
         }   while  (WT_FALSE)



/*
 *----------------------------------------------------------------------
 */



/*
 *     CDF status text, set stream.
 *
 *     "status" must be a CDF status code (whose text will be written).
 *
 *     "text" must be a character pointer.
 *
 *     Functions related to error messages control whether any error
 *     messages are written at all or not; and if they are written, to
 *     which stream.
 */


#define                                                                \
                                                                       \
   WT_M_Put_CDF_Status_Text(status, text)                              \
                                                                       \
       do                                                              \
                                                                       \
         {                                                             \
                                                                       \
           FILE *wt_m_stream ;                                         \
           int wt_m_write_switch ;                                     \
                                                                       \
           (void) CDFgetStatusText(status, text) ;                     \
                                                                       \
           GetErrMsgIO(&wt_m_stream, &wt_m_write_switch) ;             \
                                                                       \
           if  (wt_m_write_switch)                                     \
             {                                                         \
               WT_M_Write_CDF_Status_Text(status, wt_m_stream,         \
                   text) ;                                             \
             }                                                         \
                                                                       \
         }   while  (WT_FALSE)





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



   int
       Open_POL_Arch_001_Archive_File(
           WT_T_POL_Arch_001_File_Access_Info
             *file_access_info) ;


   int
       Close_POL_Arch_001_Archive_File(
           WT_T_POL_Arch_001_File_Access_Info
             *file_access_info) ;


   int
       Print_WT_T_POL_Arch_001_Comp(
           const WT_T_POL_Arch_001_Comp *p,
           FILE *o_fp) ;


   int
       Check_WT_T_POL_Arch_001_Comp(
           const WT_T_POL_Arch_001_Comp *p) ;


   int
       Print_WT_T_POL_Arch_001_Quant(
           const WT_T_POL_Arch_001_Quant *p,
           FILE *o_fp) ;


   int
       Check_WT_T_POL_Arch_001_Quant(
           const WT_T_POL_Arch_001_Quant *p) ;


   int
       Print_WT_T_POL_Arch_001_File_Header(
           const WT_T_POL_Arch_001_File_Header *p,
           FILE *o_fp) ;


   int
       Check_WT_T_POL_Arch_001_File_Header(
           const WT_T_POL_Arch_001_File_Header *p) ;


   int
       Write_WT_T_POL_Arch_001_File_Header(
           const WT_T_POL_Arch_001_File_Header *p,
           WT_T_POL_Arch_001_File_Access_Info
             *file_access_info) ;


   int
       Print_WT_T_POL_Arch_001_File_Data_Point(
           const WT_T_POL_Arch_001_File_Data_Point *p,
           FILE *o_fp) ;


   int
       Write_WT_T_POL_Arch_001_File_Data_Point(
           const WT_T_POL_Arch_001_File_Data_Point *p,
           WT_T_POL_Arch_001_File_Access_Info
             *file_access_info) ;





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





#endif   /* POLAR_ARCH_001_H */
