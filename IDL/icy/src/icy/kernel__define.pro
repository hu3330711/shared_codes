;;-Abstract
;;
;;   Kernel object, kernel database information.
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
;;      ker = obj_new( 'kernel', kind)
;;
;;         kind   the scalar string list of identifiers indicating the type
;;                of kernels of interest. Recognized value:
;;
;;                SPK  --- All SPK files are counted in the total.
;;                CK   --- All CK files are counted in the total.
;;                PCK  --- All binary PCK files are counted in the
;;                         total.
;;                EK   --- All EK files are counted in the total.
;;                TEXT --- All text kernels that are not meta-text.
;;                         kernels are included in the total.
;;                META --- All meta-text kernels are counted in the
;;                         total.
;;                ALL  --- Every type of kernel is counted in the
;;                         total.
;;
;;               'kind' lack case sensitivity. If a word appears in kind
;;               that is not one of those listed above, it is ignored.
;;
;;   Functions:
;;
;;      files()     returns an array containing the file names of the loaded
;;                  kernels of type 'kind'.
;;
;;      sources()   returns an array containing the text strings identifying
;;                  the load source of the loaded kernels of type 'kind'.
;;
;;      count()     retuens the number of loaded kernels.
;;
;;      types()     returns an array containing the text strings identifying
;;                  the type (kind) of the loaded kernels.
;;
;;   Procedures:
;;
;;      list             output a formatted list all kernel names.
;;
;;      help             display object format and field information.
;;
;;-Examples
;;
;;      IDL> .compile kernel__define.pro
;;
;;      IDL> cspice_furnsh, '/kernels/standard.tm'
;;
;;      IDL> ker = obj_new( 'kernel', 'ALL')
;;
;;   How many kernels loaded in the IDL instance.
;;
;;      IDL> print, ker->count()
;;                10
;;
;;   Loop over the elements of the 'FILES' tag.
;;
;;      IDL> files  = ker->files()
;;      IDL> count  = ker->count()
;;      IDL> source = ker->sources()
;;
;;      IDL> for i=0, count-1 do print, i, ' ', files[i]
;;             0 /kernels/standard.ker
;;             1 /kernels/gen/lsk/naif0008.tls
;;             2 /kernels/gen/spk/de414.bsp
;;             3 /kernels/gen/spk/earthstns_itrf93_050714.bsp
;;             4 /kernels/gen/fk/moon_060721.tf
;;             5 /kernels/gen/pck/moon_pa_de403_1950-2198.bpc
;;             6 /kernels/gen/fk/moon_assoc_me.tf
;;             7 /kernels/gen/fk/moon_assoc_pa.tf
;;             8 /kernels/gen/pck/earth_000101_080120_071029.bpc
;;             9 /kernels/gen/pck/pck00008.tpc
;;
;;   Now the sources for each of the elements in 'files'.
;;
;;      IDL> for i=0, count-1 do print, i, ' ', source[i]
;;             0
;;             1 /kernels/standard.ker
;;             2 /kernels/standard.ker
;;             3 /kernels/standard.ker
;;             4 /kernels/standard.ker
;;             5 /kernels/standard.ker
;;             6 /kernels/standard.ker
;;             7 /kernels/standard.ker
;;             8 /kernels/standard.ker
;;             9 /kernels/standard.ker
;;
;;   The empty string from the first element, [0], of the 'source' array
;;   indicates the corresponding element in the 'files' array,
;;   standard.ker', as a meta kernel.
;;
;;   To simply output the kernels stored in 'ker' of type "ALL".
;;
;;      IDL> ker->list
;;      /kernels/standard.ker
;;      /kernels/gen/lsk/naif0008.tls
;;      /kernels/gen/spk/de414.bsp
;;      /kernels/gen/spk/earthstns_itrf93_050714.bsp
;;      /kernels/gen/fk/moon_060721.tf
;;      /kernels/gen/pck/moon_pa_de403_1950-2198.bpc
;;      /kernels/gen/fk/moon_assoc_me.tf
;;      /kernels/gen/fk/moon_assoc_pa.tf
;;      /kernels/gen/pck/earth_000101_080120_071029.bpc
;;      /kernels/gen/pck/pck00008.tpc
;;
;;-Particulars
;;
;;   Create an object containing information on the contents on the
;;   kernel database for a specific kernel type.
;;
;;-Required Reading
;;
;;   ICY.REQ
;;   KERNEL.REQ
;;
;;-Version
;;
;;   -Icy Version 1.0.1, 01-APR-2015, EDW (JPL)
;;
;;       Implemented proper header to conform to NAIF standard for
;;       Icy documentation.
;;
;;   -Icy Version 1.0.0, 29-AUG-2006, EDW (JPL)
;;
;;-Index_Entries
;;
;;   Kernel object
;;
;;-&


;;
;; Define the object's fields.
;;
PRO kernel__define

   struct = { kernel,              $
              COUNT  : 0L       ,  $
              FILE   : ptr_new(),  $
              TYPE   : ptr_new(),  $
              SOURCE : ptr_new()   $
            }

END


;;
;; The intializer called on invoking the constructor.
;; 'type_of-interest' directly passes to the cspice_kdata call.
;;
FUNCTION kernel::init, kind


   Catch, err
      if err eq 0 then cspice_ktotal, kind, count
   Catch, /cancel
   if ( err NE 0 ) then begin
      err_out
      RETURN, 0
   endif


   self.count = count

   if ( count NE 0 ) then begin

      BLOCK      = 5
      file_arr   = strarr(BLOCK)
      type_arr   = strarr(BLOCK)
      source_arr = strarr(BLOCK)

      for i = 0, (count-1) do begin

         if  (i+1) mod BLOCK EQ 0 then begin

            ;;
            ;; Allocate more memory for the file, type, and source
            ;; array every BLOCK iteration.
            ;;
            file_arr   = [ temporary(file_arr)  , strarr(BLOCK) ]
            type_arr   = [ temporary(type_arr)  , strarr(BLOCK) ]
            source_arr = [ temporary(source_arr), strarr(BLOCK) ]

         endif


         Catch, err
            if err eq 0 then cspice_kdata, i,      $
                                           kind,   $
                                           file,   $
                                           type,   $
                                           source, $
                                           handle, $
                                           found
         Catch, /cancel
         if ( err NE 0 ) then begin
            err_out
            RETURN, 0
         endif


         ;;
         ;; If cspice_kdata returned values, copy those values to the
         ;; work arrays.
         ;;
         if ( found ) then begin
            file_arr  [i] = file
            type_arr  [i] = type
            source_arr[i] = source
         endif

      endfor

      ;;
      ;; Set the object fields for the file list, source list, and type
      ;; list to pointers to the corresponding arrays.
      ;;
      self.file   = ptr_new( file_arr  [0:i] )
      self.type   = ptr_new( type_arr  [0:i] )
      self.source = ptr_new( source_arr[0:i] )

   endif else begin


      ;;
      ;; No loaded kernels. Assign the field points to
      ;; empty strings. This prevents any possibility of a NULL
      ;; pointer.
      ;;
      self.file   = ptr_new( '' )
      self.type   = ptr_new( '' )
      self.source = ptr_new( '' )

   endelse

   RETURN, 1
END



;;
;; Field accessor functions.
;;

;;
;; List of loaded kernel files.
;;
FUNCTION kernel::files

   RETURN, *self.file

END


;;
;; List of kernel types corresponding (1-to-1) to
;; the 'files' list.
;;
FUNCTION kernel::types

   RETURN, *self.type

END


;;
;; List of kernel file load sources corresponding (1-to-1)
;; to the 'files' list. Note a file loaded by name, i.e. not
;; using a meta-kernel will show an empty string as a 'source'.
;;
FUNCTION kernel::sources

   RETURN, *self.source

END


;;
;; The number of loaded kernels. This value also represents
;; the number of elements in the 'files', 'sources', and
;; 'types' lists.
;;
FUNCTION kernel::count

   RETURN, self.count

END


;;
;; A simple procedure to list all kernel names
;; stored in 'self.file'.
;;
PRO kernel::list

   count = self.count
   files = *self.file

   for i=0,count-1 do begin
      print, files[i]
   endfor

END


;;
;; A trivial help procedure.
;;
PRO KERNEL::help

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
