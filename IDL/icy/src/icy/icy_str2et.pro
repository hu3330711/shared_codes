;;-Abstract
;;
;;   ICY__STR2ET converts a string representing an epoch to a double 
;;   precision value representing the number of TDB seconds past the 
;;   J2000 epoch corresponding to the input epoch.
;;
;;   For important details concerning this module's function, please 
;;   refer to the CSPICE routine str2et_c.
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
;;      str   any scalar or array of strings of any shape recognized 
;;            by the SPICE Time subsystem as an epoch.
;;
;;   the call:
;;
;;      et = icy_str2et( str )
;;
;;   returns:
;;
;;      et   the set of double precision TDB seconds past the J2000 epoch
;;           that corresponds to the input 'str'.
;;
;;           'et' returns with the same shape and dimension as 'str'.
;;
;;   Note: Reference the function cspice_tsetyr for information concerning
;;   the translation of two digit representations of the century count.
;;
;;-Examples
;;
;;   Any numerical results shown for this example may differ between
;;   platforms as the results depend on the SPICE kernels used as input
;;   and the machine specific arithmetic implementation.
;;
;;      ;;
;;      ;; Load a leapseconds kernel.
;;      ;;
;;      cspice_furnsh, 'standard.tm'
;;
;;      ;;
;;      ;; Define the epoch as a string.
;;      ;;
;;      date = 'Thu Mar 20 12:53:29 PST 1997'
;;
;;      ;;
;;      ;; Convert a string to ephemeris time (ET).
;;      ;;
;;      et = icy_str2et( date )
;;      print, 'Scalar:'
;;      print, FORMAT='(F20.8)', et
;;
;;      ;;
;;      ;; Define a vector of time strings:
;;      ;;
;;      time = strarr(5)
;;
;;      ;;
;;      ;; Allocate an array of ints, initial value
;;      ;; 2454000, increment by 10000 for each array
;;      ;; element. Use longs, 'L', for the Julian data
;;      ;; value
;;      ;;
;;      jd = 10000L * lindgen(5,2) + 2454000L
;;
;;      ;;
;;      ;; Fill the time array with string representations
;;      ;; of the 'jd' array in the format of a Julian date
;;      ;; string. Also strip whitespace.
;;      ;;
;;      time = string(jd)
;;      time = strcompress( 'JD' + time, /REMOVE_ALL )
;;
;;      et = icy_str2et( time )
;;      print, 'Array:'
;;      print, et
;;
;;   IDL outputs:
;;
;;      Scalar:
;;      -87836728.81438904
;;
;;      Array:
;;      2.1211207e+08 1.0761121e+09 1.9401121e+09 2.8041121e+09 3.6681121e+09
;;      4.5321121e+09 5.3961121e+09 6.2601121e+09 7.1241121e+09 7.9881121e+09
;;
;;-Particulars
;;
;;   None.
;;
;;-Required Reading
;;
;;   ICY.REQ
;;   TIME.REQ
;;
;;-Version
;;
;;   -Icy Version 1.0.0, 29-JUN-2010, EDW (JPL)
;;
;;-Index_Entries
;;
;;   Convert a string to TDB seconds past the J2000 epoch
;;
;;-&

FUNCTION ICY_STR2ET, str

   ;;
   ;; Shape gymnastics. Take it on faith that zzicy_var does something useful.
   ;; Some addition comments would be spiffy...
   ;;
   var_str = zzicy_var( str )

   datum   = *var_str.datum
   in_str  = *var_str.var

   cspice_str2et, in_str, et

   if ( datum[0] eq 0 ) then begin

      ;;
      ;; Scalar output, return.
      ;;
      return, et

   endif else begin

      ;;
      ;; Reshape 'et' to the same shape as the input 'str' then return.
      ;;
      return, reform( et, datum[ 1:datum[0] ] )

   endelse

END
