// clParceString.h

// C++ SSL Data Tools Decriptions (Widgits) Declaration File:

// SCCS ID string:
#define SccsId_clStringParse_h "@(#)clParceString.h	1.2, 11/03/95"

#include <stdio.h>
#include <string.h>
#include <SDTDescription.h>

#define ASSIGNMENT_INDICATOR "="
#define SEPERATOR " "

// "StringParse" Class:
class clParceString
    {
    private:
    public:
	char *CurrentString ;
        int UniqueMarkers ;

        char *Value ;

	clParceString() ;
	clParceString( char *buffer, int unique_markers) ;
	clParceString( int unique_markers) ;
	clParceString( const clParceString &cls) ;
	~clParceString() ;
        clParceString& operator = (const clParceString &cls) ;

        int FindStringIndex( char *string, int start_index) ;
        int FindStringQuoted( char *string, int start_index) ;
        int FindMarker( char *marker, int version) ;
        char *FindValue( char *marker, int version) ;

        int AddParameter( char *marker, char *value) ;
        char *GetParameter( char *marker, int version) ;

        char *GetCurrentString( void) ;
    } ;
