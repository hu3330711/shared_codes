;;-Abstract
;;
;;   CK segment object.
;;
;;-Disclaimer
;;
;;   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
;;   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
;;   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
;;   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
;;   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
;;   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
;;   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
;;   PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
;;   SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
;;   SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
;;
;;   IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
;;   BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
;;   LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
;;   INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
;;   REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
;;   REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
;;
;;   RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
;;   THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
;;   CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
;;   ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
;;
;;-I/O
;;
;;   Constructor:
;;
;;      ck = obj_new( 'CK', ckname )
;;
;;         ckname   string path name, ralative or absolute, of a CK.
;;
;;   Functions:
;;
;;      value( item )
;;
;;         item
;;
;;            KERNEL
;;            CK_ID
;;            REFERENCE
;;            TYPE
;;            RATES
;;            SEG_START
;;            SEG_END
;;            SCLK_START
;;            SCLK_END
;;            TAGS
;;
;;   Procedures:
;;
;;      kernel_summary   output a formatted table of the kenerel segment
;;                       header.
;;
;;      help             display object format and field information.
;;
;;-Examples
;;
;;    IDL> ck = obj_new( 'CK', '/kernels/genesis/gnssc_r_020506_020512_v1.bc')
;;
;;    IDL> ck->kernel_summary
;;    CK summary for /kernels/genesis/gnssc_r_020506_020512_v1.bc
;;       CK ID     Ref  Type Rates    Seg start/end          SCLK start/end
;;      -47000       1     3     1             1153   180508239160.00000000
;;                                           802154   180646286780.00000000
;;
;;      -47000       1     3     1           802155   180646286780.00000000
;;                                           899486   180663075877.00000000
;;
;;    IDL> print, ck->value( 'KERNEL' )
;;    /kernels/genesis/gnssc_r_020506_020512_v1.bc
;;
;;    IDL> print, ck->value( 'CK_ID' )
;;          -47000      -47000
;;
;;    IDL> print, ck->value( 'REFERENCE' )
;;               1           1
;;
;;    IDL> print, ck->value( 'TYPE' )
;;               3           3
;;
;;    IDL> print, ck->value( 'RATES' )
;;               1           1
;;
;;    IDL> print, ck->value( 'SEG_START' )
;;            1153      802155
;;
;;    IDL> print, ck->value( 'SEG_END' )
;;          802154      899486
;;
;;    IDL> print, ck->value( 'SCLK_START' )
;;       1.8050824e+11   1.8064629e+11
;;
;;    IDL> print, ck->value( 'SCLK_END' )
;;       1.8064629e+11   1.8066308e+11
;;
;;    IDL> print, ck->value( 'TAGS' )
;;    KERNEL CK_ID REFERENCE TYPE RATES SEG_START SEG_END SCLK_START SCLK_END
;;
;;-Particulars
;;
;;   Create an object containing segment header data for a specific CK.
;;
;;-Required Reading
;;
;;   ICY.REQ
;;   CK.REQ
;;
;;-Version
;;
;;   -Icy Version 1.0.1, 01-APR-2015, EDW (JPL)
;;
;;       Implemented proper header to conform to NAIF standard for
;;       Icy documentation.
;;
;;   -Icy Version 1.0.0, 24-AUG-2004, EDW (JPL)
;;
;;-Index_Entries
;;
;;   SCK object
;;
;;-&

;;
;; Define the structure for the object.
;;
PRO CK__define

   struct = { CK,                     $
              KERNEL    : '',         $
              CK_ID     : ptr_new(),  $
              REFERENCE : ptr_new(),  $
              TYPE      : ptr_new(),  $
              RATES     : ptr_new(),  $
              SEG_START : ptr_new(),  $
              SEG_END   : ptr_new(),  $
              SCLK_START: ptr_new(),  $
              SCLK_END  : ptr_new(),  $
              TAGS      : ptr_new()   $
            }

END


;;
;; Initialize the object with the CK kernel of interest.
;;
FUNCTION CK::init, kernel

   tag  = [ 'KERNEL',     $
            'CK_ID',      $
            'REFERENCE',  $
            'TYPE',       $
            'RATES',      $
            'SEG_START',  $
            'SEG_END',    $
            'SCLK_START', $
            'SCLK_END'    ]

   ;;
   ;; Open the CK file for DAF access.
   ;;
   Catch, err
      if err eq 0 then cspice_dafopr, kernel, handle
   Catch, /cancel
   if ( err NE 0 ) then begin
      err_out
      RETURN, 0
   endif

   ;;
   ;; Begin a forward search.
   ;;
   Catch, err
      if err eq 0 then cspice_dafbfs, handle
   Catch, /cancel
   if ( err NE 0 ) then begin
      err_out
      RETURN, 0
   endif

   ;;
   ;; Find the next data array.
   ;;
   Catch, err
      if err eq 0 then cspice_daffna, found
   Catch, /cancel
   if ( err NE 0 ) then begin
      err_out
      RETURN, 0
   endif

   BLOCK       = 5
   i           = 0
   ck_id  = lonarr(BLOCK)
   reference   = lonarr(BLOCK)
   type        = lonarr(BLOCK)
   rates       = lonarr(BLOCK)
   seg_start   = lonarr(BLOCK)
   seg_end     = lonarr(BLOCK)
   sclk_start  = dblarr(BLOCK)
   sclk_end    = dblarr(BLOCK)

   self.kernel = kernel
   self.tags   = ptr_new(tag)

   while ( found ) do begin

      ;;
      ;; Retrieve the current segment.
      ;;
      Catch, err
         if err eq 0 then cspice_dafgs, 2, 6, dc, ic
      Catch, /cancel
      if ( err NE 0 ) then begin
         err_out
         RETURN, 0
      endif

      ;;
      ;; Fill the data arrays with the segment information.
      ;;
      ck_id[i] = ic[0]
      reference [i] = ic[1]
      type      [i] = ic[2]
      rates     [i] = ic[3]
      seg_start [i] = ic[4]
      seg_end   [i] = ic[5]

      sclk_start[i] = dc[0]
      sclk_end  [i] = dc[1]

      Catch, err
         if err eq 0 then cspice_daffna, found
      Catch, /cancel
      if ( err NE 0 ) then begin
         err_out
         RETURN, 0
      endif

      if ( found ) then begin
         i      = i + 1

         ;;
         ;; Allocate memory for the data arrays in 5 element increment.
         ;; Track 'i's value, at each multiple of 5, add 5 elements to
         ;; the arrays.
         ;;
         if i mod BLOCK EQ 0 then begin
             ck_id  = [ temporary(ck_id), intarr(BLOCK) ]
             reference   = [ temporary(reference ), intarr(BLOCK) ]
             type        = [ temporary(type      ), intarr(BLOCK) ]
             rates       = [ temporary(rates     ), intarr(BLOCK) ]
             seg_start   = [ temporary(seg_start ), intarr(BLOCK) ]
             seg_end     = [ temporary(seg_end   ), intarr(BLOCK) ]
             sclk_start  = [ temporary(sclk_start), intarr(BLOCK) ]
             sclk_end    = [ temporary(sclk_end  ), intarr(BLOCK) ]
         endif

      endif

   endwhile

   ;;
   ;; Close the DAF file in the proper way.
   ;;
   Catch, err
      if err eq 0 then cspice_dafcls, handle
   Catch, /cancel
   if ( err NE 0 ) then begin
      err_out
      RETURN, 0
   endif

   ;;
   ;; Assign the data arrays to object variables for return.
   ;;
   self.ck_id  = ptr_new(ck_id[0:i] )
   self.reference   = ptr_new(reference [0:i] )
   self.type        = ptr_new(type      [0:i] )
   self.rates       = ptr_new(rates     [0:i] )
   self.seg_start   = ptr_new(seg_start [0:i] )
   self.seg_end     = ptr_new(seg_end   [0:i] )
   self.sclk_start  = ptr_new(sclk_start[0:i] )
   self.sclk_end    = ptr_new(sclk_end  [0:i] )

   RETURN, 1

END


;;
;; Output a summary of the kernel for this object.
;;
PRO CK::kernel_summary

   ;;
   ;; Ouput a formatted table for human inspection.
   ;;
   print, "CK summary for ", self.kernel

   print, FORMAT='( A8,A8,A6,A6,A17,A24)', 'CK ID', 'Ref'  , $
                                            'Type', 'Rates', $
                                            'Seg start/end', 'SCLK start/end'

   count =  n_elements(*self.ck_id) -1

   for i=0, count  do begin

      print, FORMAT='(I8,I8,I6,I6,I17,F24.8)', $
                     (*self.ck_id       )[i] , $
                     (*self.reference   )[i] , $
                     (*self.type        )[i] , $
                     (*self.rates       )[i] , $
                     (*self.seg_start   )[i],  $
                     (*self.sclk_start  )[i]

      print, FORMAT='(A28,I17,F24.8)', ' ', $
                     (*self.seg_end )[i],   $
                     (*self.sclk_end)[i]

      print, ' '

   endfor

END


;;
;; Output a particular value from the kernel.
;;
FUNCTION CK::value, value

   CASE value OF

      'KERNEL'    : RETURN, self.kernel
      'CK_ID'     : RETURN, *self.ck_id
      'REFERENCE' : RETURN, *self.reference
      'TYPE'      : RETURN, *self.type
      'RATES'     : RETURN, *self.rates
      'SEG_START' : RETURN, *self.seg_start
      'SEG_END'   : RETURN, *self.seg_end
      'SCLK_START': RETURN, *self.sclk_start
      'SCLK_END'  : RETURN, *self.sclk_end
      'TAGS'      : RETURN, *self.tags
      ELSE        : RETURN, 0

   ENDCASE

END


;;
;; A trivial help procedure.
;;
PRO CK::help

   help, self, /obj

END


;;
;; Error output.
;;
PRO ERR_OUT
   print, 'Unexpected error detected.'
   print, !Error_State.Name
   print, !Error_State.Msg
END
