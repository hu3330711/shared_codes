;;-Abstract
;;
;;   SPK segment object.
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
;;      spk = obj_new( 'SPK', spkname )
;;
;;         spkname   string path name, relative or absolute, of an SPK.
;;
;;   Functions:
;;
;;      value( item )    return the value corresponding to 'item,'
;;
;;         item   name of segement data to return. Allowed data item
;;                names:
;;
;;                KERNEL
;;                SPK_ID
;;                CENTER
;;                FRAME
;;                TYPE
;;                SEG_START
;;                SEG_END
;;                ET_START
;;                ET_END
;;                TAGS         list of allowed item names, excluding "TAG."
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
;; IDL> spk1 = obj_new( 'SPK', '/kernels/ULY-Ulysess/uly_spk_merge2.bsp')
;;
;; IDL> spk1->kernel_summary
;; SPK summary for /kernels/ULY-Ulysess/uly_spk_merge2.bsp
;;   SPK ID  Center Frame  Type    Seg start/end            ET start/end
;;      -55     399    11     1              641     -291488100.00000000
;;                                          4241     -291268927.71930838
;;
;;      -55      10    11     1             4242     -291268927.71930838
;;                                         53712     -252654101.19489717
;;
;;      -55       5    11     1            53713     -252654101.19489717
;;                                         60553     -245695692.58680630
;;
;;      -55      10    11     1            60554     -245695692.58680630
;;                                       1743787      189345900.00000000
;;
;;        3       0    11     2          1743788     -291643200.00000000
;;                                       1760194      189432000.00000000
;;
;;        5       0    11     2          1760195     -291643200.00000000
;;                                       1765273      189432000.00000000
;;
;;       10       0    11     2          1765274     -291643200.00000000
;;                                       1773502      189432000.00000000
;;
;;      301       3    11     2          1773503     -291643200.00000000
;;                                       1826440      189432000.00000000
;;
;;      399       3    11     2          1826441     -291643200.00000000
;;                                       1879378      189432000.00000000
;;
;; IDL> print, spk1->value( 'KERNEL' )
;; /kernels/ULY-Ulysess/uly_spk_merge2.bsp
;;
;; IDL> print, spk1->value( 'SPK_ID' )
;;          -55         -55         -55         -55           3           5
;;           10         301         399
;;
;; IDL> print, spk1->value( 'CENTER' )
;;          399          10           5          10           0           0
;;            0           3           3
;;
;; IDL> print, spk1->value( 'FRAME' )
;;           11          11          11          11          11          11
;;           11          11          11
;;
;; IDL> print, spk1->value( 'SEG_START' )
;;          641        4242       53713       60554     1743788     1760195
;;      1765274     1773503     1826441
;;
;; IDL> print, spk1->value( 'SEG_END' )
;;         4241       53712       60553     1743787     1760194     1765273
;;      1773502     1826440     1879378
;;
;; IDL> print, spk1->value( 'ET_START' )
;;   -2.9148810e+08  -2.9126893e+08  -2.5265410e+08  -2.4569569e+08
;;   -2.9164320e+08  -2.9164320e+08  -2.9164320e+08  -2.9164320e+08
;;   -2.9164320e+08
;;
;; IDL> print, spk1->value( 'ET_END' )
;;   -2.9126893e+08  -2.5265410e+08  -2.4569569e+08   1.8934590e+08
;;    1.8943200e+08   1.8943200e+08   1.8943200e+08   1.8943200e+08
;;    1.8943200e+08
;;
;; IDL> print, spk1->value( 'TAGS' )
;; KERNEL SPK_ID CENTER FRAME TYPE SEG_START SEG_END ET_START ET_END
;;
;;-Particulars
;;
;;   Create an object containing segment header data for a specific SPK.
;;
;;-Required Reading
;;
;;   ICY.REQ
;;   SPK.REQ
;;
;;-Version
;;
;;   -Icy Version 1.0.1, 01-APR-2015, EDW (JPL)
;;
;;       Implemented proper header to conform to NAIF standard for
;;       Icy documentation.
;;
;;   -Icy Version 1.0.0, 02-MAR-2005, EDW (JPL)
;;
;;-Index_Entries
;;
;;   SPK object
;;
;;-&

;;
;; Define the object's structure, in this case each
;; field except "KERNEL" contains an unassigned pointer.
;;
PRO SPK__define

   struct = { SPK,                   $
              KERNEL   : '',         $
              SPK_ID   : ptr_new(),  $
              CENTER   : ptr_new(),  $
              FRAME    : ptr_new(),  $
              TYPE     : ptr_new(),  $
              SEG_START: ptr_new(),  $
              SEG_END  : ptr_new(),  $
              ET_START : ptr_new(),  $
              ET_END   : ptr_new(),  $
              TAGS     : ptr_new()   $
            }

END



;;
;; Initialize the data arrays from the data in the SPK.
;; Note, 'init' returns only 0 or 1 depending on
;; whether an error occurred (0) or not (1);
;;
FUNCTION SPK::init, kernel

   tag  = [ 'KERNEL',    $
            'SPK_ID',    $
            'CENTER',    $
            'FRAME',     $
            'TYPE',      $
            'SEG_START', $
            'SEG_END',   $
            'ET_START',  $
            'ET_END' ]

   ;;
   ;; Open the kernel, catch any error signal.
   ;;
   Catch, err
      if err eq 0 then cspice_dafopr, kernel, handle
   Catch, /cancel
   if ( err NE 0 ) then begin
      err_out
      RETURN, 0
   endif


   ;;
   ;; Begin a foreward search, catch any error signal.
   ;;
   Catch, err
      if err eq 0 then cspice_dafbfs, handle
   Catch, /cancel
   if ( err NE 0 ) then begin
      err_out
      RETURN, 0
   endif

   ;;
   ;; Find the next array, catch any error signal.
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
   spk_id      = lonarr(BLOCK)
   center      = lonarr(BLOCK)
   frame       = lonarr(BLOCK)
   type        = lonarr(BLOCK)
   seg_start   = lonarr(BLOCK)
   seg_end     = lonarr(BLOCK)
   et_start    = dblarr(BLOCK)
   et_end      = dblarr(BLOCK)

   self.kernel = kernel
   self.tags   = ptr_new(tag)

   while ( found ) do begin

      Catch, err
         if err eq 0 then cspice_dafgs, 2, 6, dc, ic
      Catch, /cancel
      if ( err NE 0 ) then begin
         err_out
         RETURN, 0
      endif

      spk_id   [i] = ic[0]
      center   [i] = ic[1]
      frame    [i] = ic[2]
      type     [i] = ic[3]
      seg_start[i] = ic[4]
      seg_end  [i] = ic[5]

      et_start [i] = dc[0]
      et_end   [i] = dc[1]

      Catch, err
         if err eq 0 then cspice_daffna, found
      Catch, /cancel
      if ( err NE 0 ) then begin
         err_out
         RETURN, 0
      endif

      if ( found ) then begin
         i      = i + 1

         if i mod BLOCK EQ 0 then begin
             spk_id    = [ temporary(spk_id)   , lonarr(BLOCK) ]
             center    = [ temporary(center)   , lonarr(BLOCK) ]
             frame     = [ temporary(frame)    , lonarr(BLOCK) ]
             type      = [ temporary(type)     , lonarr(BLOCK) ]
             seg_start = [ temporary(seg_start), lonarr(BLOCK) ]
             seg_end   = [ temporary(seg_end  ), lonarr(BLOCK) ]
             et_start  = [ temporary(et_start) , dblarr(BLOCK) ]
             et_end    = [ temporary(et_end  ) , dblarr(BLOCK) ]
         endif

      endif

   endwhile

   ;;
   ;; Close the kernel, catch any error signal.
   ;;
   Catch, err
      if err eq 0 then cspice_dafcls, handle
   Catch, /cancel
   if ( err NE 0 ) then begin
      err_out
      RETURN, 0
   endif

   ;;
   ;; Set pointers to the various data arrays. These arrays
   ;; should have equal length.
   ;;
   self.spk_id    = ptr_new(spk_id   [0:i])
   self.center    = ptr_new(center   [0:i])
   self.frame     = ptr_new(frame    [0:i])
   self.type      = ptr_new(type     [0:i])
   self.seg_start = ptr_new(seg_start[0:i])
   self.seg_end   = ptr_new(seg_end  [0:i])
   self.et_start  = ptr_new(et_start [0:i])
   self.et_end    = ptr_new(et_end   [0:i])

   ;;
   ;; No errors, return a success... required for the init
   ;; function.
   ;;
   RETURN, 1
END


;;
;; A procedure to output the SPK summary data in a tabular format.
;;
PRO SPK::kernel_summary

   ;;
   ;; Ouput a formatted table for human inspection.
   ;;
   print, "SPK summary for ", self.kernel

   print, FORMAT='( A8,A8,A6,A6,A17,A24)', 'SPK ID'       , 'Center', $
                                           'Frame'        , 'Type'  , $
                                           'Seg start/end', 'ET start/end'

   count =  n_elements(*self.spk_id) -1

   ;;
   ;; Loop over the number of bodies, printing the values
   ;; corresponding to each spk_id index.
   ;;
   for i=0, count  do begin

      print, FORMAT='(I8,I8,I6,I6,I17,F24.8)', $
                          (*self.spk_id  )[i]  , $
                          (*self.center)[i]  , $
                          (*self.frame )[i]  , $
                          (*self.type  )[i]  , $
                         (*self.seg_start)[i], $
                         (*self.et_start) [i]

      print, FORMAT='(A28,I17,F24.8)', ' ', $
                        (*self.seg_end)[i], $
                        (*self.et_end) [i]

      print, ' '

   endfor

END


FUNCTION SPK::value, value

   ;;
   ;; Given that 'value' has the value of one of the
   ;; defined 'tags', return the data array corresponding to
   ;; that tag.
   ;;
   ;; Return 0B (FALSE) for an unknown tag.
   ;;

   CASE value OF

      'KERNEL'    : RETURN, self.kernel
      'SPK_ID'    : RETURN, *self.spk_id
      'CENTER'    : RETURN, *self.center
      'FRAME'     : RETURN, *self.frame
      'TYPE'      : RETURN, *self.type
      'SEG_START' : RETURN, *self.seg_start
      'SEG_END'   : RETURN, *self.seg_end
      'ET_START'  : RETURN, *self.et_start
      'ET_END'    : RETURN, *self.et_end
      'TAGS'      : RETURN, *self.tags
      ELSE        : RETURN, 0

   ENDCASE

END


;;
;; A trivial help procedure.
;;
PRO SPK::help

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
