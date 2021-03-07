; $Id: little_endian.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $


function little_endian

   ; grabbed from the newsgroup 02 Jul 1998 by Robert Mallozzi

   return,(BYTE (1, 0, 1))[0]

end

