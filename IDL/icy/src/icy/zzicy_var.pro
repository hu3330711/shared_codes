;;-Procedure zzicy_var ( Convert arbitrary array to N Array )
;;
;;-Abstract
;;
;;   Create a copy of an input as an N Array passable to Icy.
;;   Store the original shape of the input for further 
;;   shape gymnastics.
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
;;   Given:
;;
;;      var   an arbitrary variable of some shape.
;;
;;   the call:
;;
;;      var_x = zzicy_var( var_in )
;;
;;   returns:
;;
;;      var_x   a structure containing the fields:
;;
;;         datum   the size array for 'var_in', i.e the
;;                 array returned by size( var_in ).
;;
;;         n       the number of elements in 'var_in'.
;;
;;         var     an N array form of 'var_in'.
;;
;;-Examples
;;
;;   ;;
;;   ;; Create the structure describing the original shape of 'str'.
;;   ;;
;;   var_str = zzicy_var( str )
;;
;;   datum   = *var_str.datum
;;   in_str  = *var_str.var
;;
;;   cspice_str2et, in_str, et
;;
;;   if ( datum[0] eq 0 ) then begin
;;
;;      ;;
;;      ;; Scalar output.
;;      ;;
;;      out_et = et
;;
;;   endif else begin
;;
;;      ;;
;;      ;; Reshape 'et' to the same shape as the input 'str'.
;;      ;;
;;      out_et = reform( et, datum[ 1:datum[0] ] )
;;
;;   endelse
;;
;;-Particulars
;;
;;   This function converts an array of any type and shape to an IDL
;;   N Array passable to a vectorized Icy interface. A field element,
;;   datum, stores the shape of the original array.
;;   
;;-Required Reading
;;
;;   ICY.REQ
;;
;;-Version
;;
;;   -Icy Version 1.0.0, 29-JUN-2010, EDW (JPL)
;;
;;-Index_Entries
;;
;;-&

FUNCTION zzicy_var, var

   ;;
   ;; Create return structure.
   ;;
   var_x = { datum:ptr_new(), $
             n:0L,            $
             var:ptr_new() }

   ;;
   ;; Populate structure fields.
   ;;
   
   ;;
   ;; Store the original size array for 'var'. The size array
   ;; contains:
   ;;
   ;; [k_dimensions  dim_1  dim_2 ... dim_k  type  num_elements]
   ;;
   ;;    k_dimensions   number of dimensions of variable:
   ;;                   0 - scalar
   ;;                   1 - N array
   ;;                   2 - NxM array
   ;;                   etc.
   ;;
   ;;    dim_1          value of dimension 1
   ;;
   ;;    dim_2          value of dimension 2
   ;; 
   ;;       ...
   ;;
   ;;    dim_k          value of dimension k
   ;;
   ;;    type           data type
   ;;
   ;;    num_elements   total number of elements in array, equal to
   ;;                   dim_1 * dim_2 * ... * dim_k
   ;;
   var_x.datum = ptr_new( size(var) )

   ;;
   ;; Store the number of elements in 'var'.
   ;;
   var_x.n     = n_elements( var )

   if ( (*var_x.datum)[0]  eq 0 ) then begin

      ;;
      ;; Scalar input, no modification of shape required.
      ;;
      var_x.var = ptr_new( var )

   endif else begin

      ;;
      ;; Array input of some shape. Reshape to format
      ;; acceptable to Icy spec, an N array.
      ;;
      var_x.var = ptr_new( reform( var, var_x.n ) )

   endelse

   return, var_x

END
