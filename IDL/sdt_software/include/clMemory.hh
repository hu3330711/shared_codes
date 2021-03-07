// clMemory.hh
// This file contains C++ declarations for Memory widgits (SHM and Mmaped):

#ifndef CLMEMORY_HH
#define CLMEMORY_HH

// SCCS ID string:
#define SccsId_clMemory_hh "@(#)clMemory.hh	1.2, 12/03/06"

#include <SDTMemory.h>

class clMemoryUnit
    {
    private:
    public:
        AddrT          Size ;       // The total size of the memory unit,
				    // in bytes.
        char           *Address ;   // The local, mapped address of the start
				    // of the memory unit, in bytes.
        int            DQICount ;   // The number of DQI's whose data buffers
			            // currently reside inside the MemoryUnit.
			            // This is for use only by DQH client
			            // usage of the DQH Interface, so that it
			            // will know when it no longer needs a
			            // MemoryUnit attachment.

       // Constructor(s):
       clMemoryUnit () ;
       clMemoryUnit (AddrT in_size, char *in_address) ;
       clMemoryUnit (const clMemoryUnit &in_instance) ;

       // "Assignment" operator:
       clMemoryUnit& operator = (const clMemoryUnit&) ;

       // Destructor:
       virtual ~clMemoryUnit () ;

       // Clear internal storage:
       virtual void clear () ;

       // Print:
       virtual void print (FILE *ofp) ;

       // Special function to back-generate an SDT MemoryUnit:
       virtual MemoryUnit *ConvertToSdt () ;

       // Returns the type (Shared, Mapped, or Unknown) of Memory Unit:
       virtual int type () ;

       // Generate this MemoryUnit (as part of the OS):
       virtual int GenerateUnit () ;

       // Sets the local Address for the buffer space:
       virtual int SetMemoryUnitAddress () ;

       // Detach this MemoryUnit (as part of the OS):
       virtual int DetachUnit () ;

       // Remove this MemoryUnit (as part of the OS):
       virtual int RemoveUnit () ;
    } ;

class clSharedMemoryUnit : public clMemoryUnit
    {
    private:
    public:
        int            ShmKey ;     // The "key" of the Shared-Memory segment.
        int            ShmId ;      // The "Id" of the Shared-Memory segment.

       // Constructor(s):
       clSharedMemoryUnit () ;
       clSharedMemoryUnit (int in_ShmKey, int in_ShmId, AddrT in_Size,
	   char *in_Address) ;
       clSharedMemoryUnit (const clSharedMemoryUnit &in_instance) ;

       // "Assignment" operator:
       clSharedMemoryUnit& operator = (const clSharedMemoryUnit&) ;

       // Destructor:
       ~clSharedMemoryUnit () ;

       // Clear internal storage:
       void clear () ;

       // Print:
       void print (FILE *ofp) ;

       // Special function to back-generate an SDT MemoryUnit:
       MemoryUnit *ConvertToSdt () ;

       // Returns the type (Shared, Mapped, or Unknown) of Memory Unit:
       int type () ;

       // Generate this MemoryUnit (as part of the OS):
       int GenerateUnit () ;

       // Sets the local Address for the buffer space:
       int SetMemoryUnitAddress () ;

       // Detach this MemoryUnit (as part of the OS):
       int DetachUnit () ;

       // Remove this MemoryUnit (as part of the OS):
       int RemoveUnit () ;
    } ;

class clMappedMemoryUnit : public clMemoryUnit
    {
    private:
    public:
        clString       MmapName ;   // The pathname of the memory-mapped file.
        AddrT          MmapStart ;  // The starting byte, in the file, for
				    // this memory mapping.

       // Constructor(s):
       clMappedMemoryUnit () ;
       clMappedMemoryUnit (char *in_MmapName, AddrT in_MmapStart,
           AddrT in_Size, char *in_Address) ;
       clMappedMemoryUnit (const clMappedMemoryUnit &in_instance) ;

       // "Assignment" operator:
       clMappedMemoryUnit& operator = (const clMappedMemoryUnit&) ;

       // Destructor:
       ~clMappedMemoryUnit () ;

       // Clear internal storage:
       void clear () ;

       // Print:
       void print (FILE *ofp) ;

       // Special function to back-generate an SDT MemoryUnit:
       MemoryUnit *ConvertToSdt () ;

       // Returns the type (Shared, Mapped, or Unknown) of Memory Unit:
       int type () ;

       // Generate this MemoryUnit (as part of the OS):
       int GenerateUnit () ;

       // Sets the local Address for the buffer space:
       int SetMemoryUnitAddress () ;

       // Detach this MemoryUnit (as part of the OS):
       int DetachUnit () ;

       // Remove this MemoryUnit (as part of the OS):
       int RemoveUnit () ;
    } ;

class clMemorySegment
    {
    private:
    public:
	MemorySegment  cs ;

       // Constructor(s):
       clMemorySegment () ;
       clMemorySegment (MemorySegment *mseg) ;
       clMemorySegment (AddrT in_Size, AddrT in_Offset) ;
       clMemorySegment (const clMemorySegment &in_instance) ;

       // "Assignment" operator:
       clMemorySegment& operator = (const clMemorySegment&) ;

       // Destructor:
       ~clMemorySegment () ;

       // Clear internal storage:
       void clear () ;

       // Print:
       void print (FILE *ofp) ;
    } ;

#endif // CLMEMORY_HH

