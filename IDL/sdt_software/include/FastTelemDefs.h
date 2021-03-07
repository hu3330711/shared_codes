/* FastTelem.h */
#ifndef FASTTELEMDEFS_H
#define FASTTELEMDEFS_H

#ifdef __cplusplus
extern "C" {
#endif

/* For SCCS: */
#define SccsId_FastTelemDefs_h  "@(#)FastTelemDefs.h	1.9, 12/03/06"

#include <stdio.h>
#include <memory.h>

/* Files to be included from the main integration area: */
#include <telemParams.h>

#include <math.h>
#define SP 5.0  /* 5.0 sec spin period */

/* Size (in bytes) of an AnnotationHeader: */
#define  AnnHdrSize    12

/* Size (in bytes) of a PrimaryCCSDSHeader: */
#define  PriHdrSize    6

/* Size (in bytes) of a SecondaryCCSDSHeader: */
#define  SecHdrSize    8

/* Size (in bytes) of a Packet Data Unit: */
#define  PDUSize       1048

/* Size (in bytes) of a DataHeader: */
#define  DataHdrSize   14

/* Size (in bytes) of a ErrorStatus Block: */
#define  ErrStatSize   2

/* Error return meaning that the selected structure was NULL: */
#define  ERR_PTR_NULL  -999

/* Short int within an AnnotationHeader containing the VirtualChannel ID:
 *
 */
#define  VCID_word      0

/* Location of VCID bits in short:  VCID_word */
#define VCID_bits       0x000e

/* Number of bits to shift the VCID to get it into a true integer: */
#define  VCID_shift     1

/* Short int within an AnnotationHeader containing the SpaceCraft ID: */
#define  SCID_word      0

/* Location of SCID bits in short:  SCID_word */
#define SCID_bits       0x3ff0

/* Number of bits to shift the SCID to get it into a true integer: */
#define  SCID_shift     4

/* Short int within an AnnotationHeader containing the Version ID: */
#define  Vers_word      0

/* Location of version bits in short:  Vers_word */
#define Vers_bits       0xc000 

/* Number of bits to shift the Version to get it into a true integer: */
#define  Vers_shift     14

/* Starting Byte within an AnnotationHeader of the time code (6 bytes): */
#define  AnnotationTimeCodeByte   6

/* Various word and bit locations, and shifts: */
#define  FrameHeaderError_word   1
#define  VCSequenceDiscontinuity_word   1
#define  IncompletePacket_word   1
#define  FrameErrorChecking_word   1
#define  FrameErrorDetected_word   1
#define  PacketSequenceDiscontinuity_word   1
#define  DataDirection_word   1
#define  PossibleHeaderError_word   1

#define FrameHeaderError_bits   0x0001
#define  VCSequenceDiscontinuity_bits   0x0002
#define  IncompletePacket_bits   0x0004
#define  FrameErrorChecking_bits   0x0008
#define  FrameErrorDetected_bits   0x0010
#define  PacketSequenceDiscontinuity_bits   0x0020
#define  DataDirection_bits   0x0040
#define  PossibleHeaderError_bits   0x0080

#define  FrameHeaderError_shift   0
#define  VCSequenceDiscontinuity_shift   1
#define  IncompletePacket_shift   2
#define  FrameErrorChecking_shift   3
#define  FrameErrorDetected_shift   4
#define  PacketSequenceDiscontinuity_shift   5
#define  DataDirection_shift   6
#define  PossibleHeaderError_shift   7

/* Access information for ground time format: */
#define  GroundTimeFormat_word   1
#define  GroundTimeFormat_bits   0x0f00
#define  GroundTimeFormat_shift   8

/* Access information for Fill Location: */
#define  FirstFillByteLocation_word   2

/* Primary CCSDS Header constants: */
#define  PH_VNum_word   0
#define  PH_VNum_bits   0xe000
#define  PH_VNum_shift   13
#define  PH_Type_word   0
#define  PH_Type_bits   0x1000
#define  PH_Type_shift   12
#define  PH_SHF_word   0
#define  PH_SHF_bits   0x0800
#define  PH_SHF_shift   11
#define  PH_APID_word   0
#define  PH_APID_bits   0x07ff
#define  PH_APID_shift   0
#define  PH_SegF_word   1
#define  PH_SegF_bits   0xc000
#define  PH_SegF_shift   14
#define  PH_SSqC_word   1
#define  PH_SSqC_bits   0x3fff
#define  PH_SSqC_shift   0
#define  PH_PLen_word   2

/* Secondary CCSDS Header constants: */
#define  SH_Type_byte   0
#define  SH_Type_bits   0x80
#define  SH_Type_shift   7
#define  SH_Len_byte   0
#define  SH_Len_bits   0x7f
#define  SH_Len_shift   0
#define  SH_TCEx_byte   1
#define  SH_TCEx_bits   0x80
#define  SH_TCEx_shift   7
#define  SH_TCID_byte   1
#define  SH_TCID_bits   0x70
#define  SH_TCID_shift   4
#define  SH_TCR1_byte   1
#define  SH_TCR1_bits   0x0c
#define  SH_TCR1_shift   2
#define  SH_TCR2_byte   1
#define  SH_TCR2_bits   0x03
#define  SH_TCR2_shift   0
#define  SH_TCSeconds_byte   2

/* ----------------------------------------------------------------- */

#ifndef int32
typedef int     int32 ;
#endif

#ifndef uint32
typedef unsigned int     uint32 ;
#endif

/* ----------------------------------------------------------------- */
/* Structure to handle AnnotationHeader's: */
struct  AnnotationHeader_struct
    {
    /* Holds the bytes for the header: */
    unsigned short  mem_store[6] ;

    /* Extracted flags and ID's: */
    int    pv_VirtualChannel ;
    int    pv_Spacecraft ;
    int    pv_Version ;
    int    pv_FrameHeaderError ;
    int    pv_VirtualChannelDiscontinuity ;
    int    pv_IncompletePacket ;
    int    pv_FrameErrorChecking ;
    int    pv_FrameErrorDetected ;
    int    pv_PacketSequenceDiscontinuity ;
    int    pv_DataDirection ;
    int    pv_PossibleHeaderError ;
    int    pv_GroundTimeFormat ;
    int    pv_FirstFillByteLocation ;

    /* Time Code values: */
    int    pv_Year ;
    int    pv_DayOfYear ;
    int    pv_Millisecs ;
    int    pv_Microsecs ;
    } ;
typedef  struct  AnnotationHeader_struct  AnnotationHeader ;

/* Constructor(s): */
AnnotationHeader *ConstructAnnotationHeader (char *packet_start) ;
int  FillAnnotationHeader (AnnotationHeader *anh, char *packet_start) ;

/* Destructor: */
int DestructAnnotationHeader(AnnotationHeader *anh) ;
int ClearAnnotationHeader(AnnotationHeader *anh) ;

/* Annotation Hdr Functions to extract information from the raw bytes: */
int AnnHdrExtractVCID (AnnotationHeader *anh) ;
int AnnHdrExtractSCID (AnnotationHeader *anh) ;
int AnnHdrExtractVersion (AnnotationHeader *anh) ;
int AnnHdrExtractFlags (AnnotationHeader *anh) ;
int AnnHdrExtractGroundTimeFormat (AnnotationHeader *anh) ;
int AnnHdrExtractFirstFillByteLocation (AnnotationHeader *anh) ;

/* Annotation Hdr Function to process the time code bytes: */
int AnnHdrExtractTimeCode(AnnotationHeader *anh) ;

/* Annotation Hdr Functions to return previously extracted information: */
int AnnHdrVirtualChannel (AnnotationHeader *anh) ;
int AnnHdrSpacecraft (AnnotationHeader *anh) ;
int AnnHdrVersion (AnnotationHeader *anh) ;
int AnnHdrFrameHeaderError(AnnotationHeader *anh) ;
int AnnHdrVirtualChannelDiscontinuity(AnnotationHeader *anh) ;
int AnnHdrIncompletePacket(AnnotationHeader *anh) ;
int AnnHdrFrameErrorChecking(AnnotationHeader *anh) ;
int AnnHdrFrameErrorDetected(AnnotationHeader *anh) ;
int AnnHdrPacketSequenceDiscontinuity(AnnotationHeader *anh) ;
int AnnHdrDataDirection(AnnotationHeader *anh) ;
int AnnHdrPossibleHeaderError(AnnotationHeader *anh) ;
int AnnHdrGroundTimeFormat(AnnotationHeader *anh) ;
int AnnHdrFirstFillByteLocation (AnnotationHeader *anh) ;

/* Debug "print" function: */
int AnnHdrPrint(AnnotationHeader *anh, FILE *ofp) ;

/* ----------------------------------------------------------------- */
/* Structure to handle PrimaryCCSDSHeader's: */
struct  PrimaryCCSDSHeader_struct
    {
    /* Holds the bytes for the header: */
    unsigned char  mem_store[6] ;

    /* Extracted flags, ID's, and values: */
    int    pv_VersionNumber ;
    int    pv_Type ;
    int    pv_SecondaryHeaderFlag ;
    int    pv_APID ;
    int    pv_SegmentFlags ;
    int    pv_SourceSequenceCount ;
    int    pv_PacketLength ;
    } ;
typedef  struct  PrimaryCCSDSHeader_struct  PrimaryCCSDSHeader ;

/* Constructor(s): */
PrimaryCCSDSHeader *ConstructPrimaryCCSDSHeader(char *start) ;
int FillPrimaryCCSDSHeader(PrimaryCCSDSHeader *pptr, char *start) ;

/* Destructor(s): */
int DestructPrimaryCCSDSHeader(PrimaryCCSDSHeader *pptr) ;
int ClearPrimaryCCSDSHeader(PrimaryCCSDSHeader *pptr) ;

/* Member Functions to extract information from the raw bytes: */
int PriHdrExtractVersion (PrimaryCCSDSHeader *pptr, unsigned short *swrd) ;
int PriHdrExtractType (PrimaryCCSDSHeader *pptr, unsigned short *swrd) ;
int PriHdrExtractSecondaryHeaderFlag (PrimaryCCSDSHeader *pptr,
    unsigned short *swrd) ;
int PriHdrExtractAPID (PrimaryCCSDSHeader *pptr,
    unsigned short *swrd) ;
int PriHdrExtractSegmentFlags (PrimaryCCSDSHeader *pptr,
    unsigned short *swrd) ;
int PriHdrExtractSourceSequenceCount (PrimaryCCSDSHeader *pptr,
    unsigned short *swrd) ;
int PriHdrExtractPacketLength (PrimaryCCSDSHeader *pptr,
    unsigned short *swrd) ;

/* Member Functions to return previously extracted information: */
int PriHdrVersionNumber (PrimaryCCSDSHeader *pptr) ;
int PriHdrType (PrimaryCCSDSHeader *pptr) ;
int PriHdrSecondaryHeaderFlag (PrimaryCCSDSHeader *pptr) ;
int PriHdrAPID (PrimaryCCSDSHeader *pptr) ;
int PriHdrSegmentFlags (PrimaryCCSDSHeader *pptr) ;
int PriHdrSourceSequenceCount (PrimaryCCSDSHeader *pptr) ;
int PriHdrPacketLength (PrimaryCCSDSHeader *pptr) ;

/* Debug "print" function: */
int PriHdrPrint(PrimaryCCSDSHeader *pch, FILE *ofp) ;

/* ----------------------------------------------------------------- */
/* Structure to handle SecondaryCCSDSHeader's: */
struct  SecondaryCCSDSHeader_struct
    {
    int    pv_Type ;
    int    pv_Length ;

    int    pv_TimeCodeExtensionFlag ;
    int    pv_TimeCodeID ;
    int    pv_TimeCodeSecondsByteCount ;
    int    pv_TimeCodeSubSecondsByteCount ;
    int    pv_TimeCodeSecondsSinceEpoch ;
    int    pv_TimeCodeSubSecondsSinceEpoch ;
    } ;
typedef  struct  SecondaryCCSDSHeader_struct  SecondaryCCSDSHeader ;

/* Constructor(s): */
SecondaryCCSDSHeader *ConstructSecondaryCCSDSHeader(char *start) ;
int FillSecondaryCCSDSHeader(SecondaryCCSDSHeader *shdr, char *start) ;

/* Note that no copy constructor and assignment= operator
 * are defined since (as of 93/10/07) there are no allocated
 * sub-fields:
 */

/* Destructor(s): */
int DestructSecondaryCCSDSHeader(SecondaryCCSDSHeader *shdr) ;
int ClearSecondaryCCSDSHeader(SecondaryCCSDSHeader *shdr) ;

/* Member Functions to extract information from the raw bytes: */
int SecHdrExtractType (SecondaryCCSDSHeader *shdr,
    unsigned char *bptr) ;
int SecHdrExtractSecondaryHeaderLength (SecondaryCCSDSHeader *shdr,
    unsigned char *bptr) ;
int SecHdrExtractPacketTimeCode (SecondaryCCSDSHeader *shdr,
    unsigned char *bptr) ;

/* Member Functions to return previously extracted information: */
int SecHdrType (SecondaryCCSDSHeader *shdr) ;
int SecHdrLength (SecondaryCCSDSHeader *shdr) ;
int SecHdrTimeCodeID (SecondaryCCSDSHeader *shdr) ;
int SecHdrTimeCodeSecondsByteCount (SecondaryCCSDSHeader *shdr) ;
int SecHdrTimeCodeSubSecondsByteCount (SecondaryCCSDSHeader *shdr) ;
int SecHdrTimeCodeSecondsSinceEpoch (SecondaryCCSDSHeader *shdr) ;
int SecHdrTimeCodeSubSecondsSinceEpoch (SecondaryCCSDSHeader *shdr) ;

/* Debug "print" function: */
int SecHdrPrint(SecondaryCCSDSHeader *shdr, FILE *ofp) ;

/* ------------------------------------------------------------------ */
/* This structure holds spin and magnetometer phase information from
 * a packet data header:
 */
struct  phaseHdr_struct
    {
    int  SpinNumber ;
    int  SpinPhase ;
    int  MagPhase ;
    } ;
typedef struct  phaseHdr_struct  phaseHdr ;

/* Extract Spin and Magnetometer phase information from a packet
 * data header:
 */
extern int  extractSMPhase (unsigned char *dptr, phaseHdr *phptr) ;

extern double Bphase (double SpinNumber, double SpinPhase, double MagPhase) ;

extern double Bphase_monotonic (phaseHdr *phptr, double time, 
				int32 *lastBphase, double *lastTime) ;

#ifdef __cplusplus
}
#endif

#endif /* FASTTELEMDEFS_H */
