;;-Abstract
;;
;;   ICY_SPKEZR returns the state (position and velocity) of a target body 
;;   relative to an observing body, optionally corrected for light  time 
;;   (planetary aberration) and stellar aberration.
;;
;;   For important details concerning this module's function, please refer to
;;   the CSPICE routine spkezr_c.
;;
;;-I/O
;;
;;   Given:
;;
;;      targ      the scalar string name of a target body.
;;                Optionally, you may supply the integer ID code 
;;                for the object as an integer string, i.e. both 
;;                'MOON' and '301' are legitimate strings that 
;;                indicate the Moon is the target body.
;;   
;;                The target and observer define a state vector 
;;                whose position component points from the observer 
;;                to the target.
;;   
;;      et        the scalar or N array of double precision ephemeris time,
;;                expressed as seconds past J2000 TDB, at which the state 
;;                of the target body relative to the observer is to be 
;;                computed, 'et' refers to time at the observer's location
;;   
;;      ref       the scalar string name of the reference frame relative
;;                to which the output state vector should be
;;                expressed. This may be any frame supported by the SPICE
;;                system, including built-in frames (documented in the
;;                Frames Required Reading) and frames defined by a loaded
;;                frame kernel (FK). 
;; 
;;                When 'ref' designates a non-inertial frame, the 
;;                orientation of the frame is evaluated at an epoch  
;;   
;;                dependent on the selected aberration correction. 
;;      abcorr    a scalar string that indicates the aberration corrections
;;                to apply to the state of the target body to account  
;;                for one-way light time and stellar aberration.
;;                  
;;                'abcorr' may be any of the following: 
;;  
;;                   'NONE'     Apply no correction. Return the  
;;                              geometric state of the target body  
;;                              relative to the observer.  
;;  
;;                The following values of 'abcorr' apply to the
;;                "reception" case in which photons depart from the
;;                target's location at the light-time corrected epoch
;;                et-lt and *arrive* at the observer's location at
;;                'et':
;;  
;;                   'LT'       Correct for one-way light time (also 
;;                              called "planetary aberration") using a 
;;                              Newtonian formulation. This correction 
;;                              yields the state of the target at the 
;;                              moment it emitted photons arriving at 
;;                              the observer at 'et'. 
;;  
;;                              The light time correction uses an
;;                              iterative solution of the light time 
;;                              equation (see Particulars for details). 
;;                              The solution invoked by the "LT" option 
;;                              uses one iteration. 
;;  
;;                   'LT+S'     Correct for one-way light time and 
;;                              stellar aberration using a Newtonian 
;;                              formulation. This option modifies the 
;;                              state obtained with the "LT" option to 
;;                              account for the observer's velocity 
;;                              relative to the solar system 
;;                              barycenter. The result is the apparent 
;;                              state of the target---the position and 
;;                              velocity of the target as seen by the 
;;                              observer. 
;;  
;;                   'CN'       Converged Newtonian light time
;;                              correction. In solving the light time
;;                              equation, the 'CN' correction iterates
;;                              until the solution converges (three
;;                              iterations on all supported platforms).
;;                              Whether the 'CN+S' solution is
;;                              substantially more accurate than the
;;                              'LT' solution depends on the geometry
;;                              of the participating objects and on the
;;                              accuracy of the input data. In all
;;                              cases this routine will execute more
;;                              slowly when a converged solution is
;;                              computed. See the Particulars section
;;                              below for a discussion of precision of
;;                              light time corrections.
;;
;;                   'CN+S'     Converged Newtonian light time
;;                              correction and stellar aberration
;;                              correction.
;;  
;;  
;;                The following values of 'abcorr' apply to the 
;;                "transmission" case in which photons *depart* from 
;;                the observer's location at 'et' and arrive at the 
;;                target's location at the light-time corrected epoch 
;;                et+lt: 
;;  
;;                   'XLT'      "Transmission" case:  correct for 
;;                              one-way light time using a Newtonian 
;;                              formulation. This correction yields the 
;;                              state of the target at the moment it 
;;                              receives photons emitted from the 
;;                              observer's location at 'et'. 
;;  
;;                   'XLT+S'    "Transmission" case:  correct for 
;;                              one-way light time and stellar 
;;                              aberration using a Newtonian 
;;                              formulation  This option modifies the 
;;                              state obtained with the "XLT" option to 
;;                              account for the observer's velocity 
;;                              relative to the solar system 
;;                              barycenter. The position component of 
;;                              the computed target state indicates the 
;;                              direction that photons emitted from the 
;;                              observer's location must be "aimed" to 
;;                              hit the target. 
;;  
;;                   'XCN'      "Transmission" case:  converged  
;;                              Newtonian light time correction. 
;;  
;; 
;;                   'XCN+S'    "Transmission" case: converged Newtonian
;;                              light time correction and stellar
;;                              aberration correction.
;;  
;;  
;;                Neither special nor general relativistic effects are 
;;                accounted for in the aberration corrections applied 
;;                by this routine. 
;;  
;;                Both letter case and embedded blanks are not significant 
;;                in the 'abcorr' string. 
;;   
;;      obs       the scalar string name of a observing body. 
;;                Optionally, you may supply the integer ID code 
;;                for the object as an integer string, i.e. both 
;;                'MOON' and '301' are legitimate strings that 
;;                indicate the Moon is the observing body.
;;
;;   the call:
;;
;;      starg = icy_spkezr( targ, et, ref, abcorr, obs )
;;
;;   returns:
;;
;;      starg   an anonymous structure consisting of two fields:
;;
;;              state  
;;
;;              The double precision, Cartesian state vector(s) representing 
;;              the position and velocity of the target body relative to the 
;;              specified observer at 'et'. 'starg' is corrected for the 
;;              specified aberrations 'abcorr'. 
;;
;;              'starg.state' returns with shape of either a 6 array or a 
;;              6 x (dimension of 'et') array, e.g. if the dimension 
;;              of et equals 5 x 2, the dimension of 'starg.state'
;;              equals 6 x 5 x 2.
;;
;;              For non 6 array shapes, retrieve the i,j state vector via:
;;              
;;                 state_k = state[*,i,j]
;;                
;;              The first three components of a 6-array 'starg' represent 
;;              the x-, y- and z-components of the target's position as
;;              measure in kilometers; the last three components form the 
;;              corresponding velocity vector as measured in kilometers-per-
;;              second.
;;
;;              The position component of 'starg' points from the 
;;              observer's location at 'et' to the aberration-corrected 
;;              location of the target. Note that the sense of the 
;;              position vector is independent of the direction of 
;;              radiation travel implied by the aberration 
;;              correction. 
;;
;;              The velocity component of 'starg' is the derivative with 
;;              respect to time of the position component of 'starg.'
;;
;;              Non-inertial frames are treated as follows: letting 
;;              'ltcent' be the one-way light time between the observer 
;;              and the central body associated with the frame, the 
;;              orientation of the frame is evaluated at et-ltcent, 
;;              et+ltcent, or 'et' depending on whether the requested
;;              aberration correction is, respectively, for received
;;              radiation, transmitted radiation, or is omitted. 'ltcent'
;;              is computed using the method indicated by 'abcorr'.
;;
;;              ltime
;;
;;              the double precision scalar one-way light time between 
;;              the observer and target in seconds; if the target state 
;;              is corrected for aberrations, then 'ltime' is the one-way 
;;              light time between the observer and the light time 
;;              corrected target location.
;;
;;              'starg:ltime' returns with the same shape and dimension
;;              as 'et'.
;;
;;              Please note, CSPICE documentation and source code uniformly 
;;              uses the variable name 'lt' to designate the light-time 
;;              between an observer and target. IDL uses 'lt' as the 
;;              less-than numeric comparison operator and so does not 
;;              allow 'lt' as a variable  name. Therefore, Icy documentation 
;;              uses the name 'ltime' for the light-time value. 
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
;;      ;; Define parameters for a state lookup:
;;      ;;
;;      ;; Return the state vector of Mars (499) as seen from
;;      ;; Earth (399) in the J2000 frame
;;      ;; using aberration correction LT+S (light time plus
;;      ;; stellar aberration) at the epoch 
;;      ;; July 4, 2003 11:00 AM PST.
;;      ;;
;;      target   = 'Mars'
;;      frame    = 'J2000'
;;      abcorr   = 'LT+S'
;;      observer = 'Earth'
;;
;;      ;;
;;      ;; Define the epoch as a string.
;;      ;;
;;      date = 'Jan 1, 2000'
;;
;;      ;;
;;      ;; Convert a string to ephemeris time (ET).
;;      ;;
;;      et = icy_str2et( date )
;;      print, 'Scalar:'
;;
;;      ;;
;;      ;; Perform a state lookup with the scalaer 'et'. Display
;;      ;; the sized and shape of the 'starg' fields. We expect a scalar
;;      ;; for 'starg.ltime' and a 6 array for 'starg.state'.
;;      ;;
;;
;;      starg = icy_spkezr( target, et, frame, abcorr, observer )
;;      help, starg.state
;;      help, starg.ltime
;;
;;      print, 'TDB      ', et
;;      print, 'Position ', starg.state[0:2]
;;      print, 'Velocity ', starg.state[3:5]
;;      print, ' '
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
;;      jd = 1000L * lindgen(5,2) + 2454000L
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
;;
;;      ;;
;;      ;; Perform a state lookup using the 5x2 dimensioned 'et.'
;;      ;; Display the sized and shape of the 'starg' fields. We expect a
;;      ;; 5x2 array for 'starg.ltime' and a 6x5x2 array for 'starg.state'.
;;
;;      starg = icy_spkezr( target, et, frame, abcorr, observer )
;;      help, starg.state
;;      help, starg.ltime
;;
;;      ;;
;;      ;; Loop over the 5x2 elements of 'starg.state' and 
;;      ;; 'starg.ltime'. Output each element of 'et' and the
;;      ;; 'starg' fields.
;;      ;;
;;
;;      for i = 0, 1 do begin
;;        
;;         for j = 0, 4 do begin
;;           
;;            print, j, i
;;            print, 'TDB      ', et[j,i]
;;            print, 'Position ', starg.state[0:2,j,i]
;;            print, 'Velocity ', starg.state[3:5,j,i]
;;           
;;         endfor
;;        
;;      endfor
;;
;;   IDL outputs:
;;      
;;      Scalar:
;;      <Expression>    DOUBLE    = Array[6]
;;      <Expression>    DOUBLE    =        921.65030
;;
;;      TDB            -43135.816
;;      Position    2.3319669e+08  -1.3382726e+08      -63666011.
;;      Velocity        31.138979       28.693018       13.005541
;;       
;;      Array:
;;      <Expression>    DOUBLE    = Array[6, 5, 2]
;;      <Expression>    DOUBLE    = Array[5, 2]
;;
;;             0       0
;;      TDB         2.1211207e+08
;;      Position   -3.8563766e+08      -54984382.      -18961038.
;;      Velocity        6.7070631      -46.565050      -20.867426
;;             1       0
;;      TDB         2.9851207e+08
;;      Position    2.1530690e+08   1.8055477e+08       73767285.
;;      Velocity       -33.313928       25.274733       11.655807
;;             2       0
;;      TDB         3.8491207e+08
;;      Position       -95261095.       29428045.       20468028.
;;      Velocity      -0.19731095       7.5478630       2.8931499
;;             3       0
;;      TDB         4.7131207e+08
;;      Position    1.4962829e+08  -2.1288050e+08      -99336561.
;;      Velocity        41.082261       15.268251       6.8498476
;;             4       0
;;      TDB         5.5771207e+08
;;      Position   -3.3568069e+08   1.8518449e+08       89013761.
;;      Velocity       -23.553751      -41.116210      -17.826324
;;             0       1
;;      TDB         6.4411207e+08
;;      Position    1.4444178e+08      -42070026.      -25020001.
;;      Velocity       -4.5848786       21.362342       8.9377437
;;             1       1
;;      TDB         7.3051207e+08
;;      Position        36846272.   1.4415942e+08       70187333.
;;      Velocity       -8.3288294       17.740793       8.0839654
;;             2       1
;;      TDB         8.1691207e+08
;;      Position   -1.1680845e+08  -3.1327043e+08  -1.3974989e+08
;;      Velocity        50.544563      -16.075675      -7.6802378
;;             3       1
;;      TDB         9.0331207e+08
;;      Position   -1.0012511e+08   2.9308911e+08   1.3177028e+08
;;      Velocity       -40.352298      -18.206625      -7.1734776
;;             4       1
;;      TDB         9.8971207e+08
;;      Position       -63314650.      -49045382.      -21045092.
;;      Velocity       -4.1380184       4.1907253      0.98762511
;;
;;-Particulars
;;
;;   Please refer to the Aberation Corrections Required Reading (ABCORR.REQ)
;;   for detailed information describing the nature and calculation of the
;;   applied corrections.
;;
;;-Required Reading
;;
;;   ICY.REQ
;;   ABCORR.REQ
;;   SPK.REQ
;;   NAIF_IDS.REQ 
;;   FRAMES.REQ
;;   TIME.REQ
;;
;;-Version
;;
;;   -Icy Version 1.0.1, 01-APR-2015, EDW (JPL)
;;
;;       Implemented proper header to conform to NAIF standard for 
;;       Icy documentation.
;;
;;   -Icy Version 1.0.0, 22-NOV-2004, EDW (JPL)
;;
;;&

FUNCTION ICY_SPKEZR, targ, et, ref, abcorr, obs

   ;;
   ;; Shape gymnastics. Take it on faith that zzicy_var does something useful.
   ;; Some addition comments would be spiffy...
   ;;
   var_str = zzicy_var( et )

   datum   = *var_str.datum
   in_et   = *var_str.var

   ;;
   ;; The conventional Icy cspice_spkezr call.
   ;;
   cspice_spkezr, targ, in_et, ref, abcorr, obs, state, ltime

   ;;
   ;; Return the state information and light time value in
   ;; an anonymous structure.
   ;;

   if ( datum[0] eq 0 ) then begin

      ;;
      ;; Scalar output, return.
      ;;
      return, { state:state, ltime:ltime }

   endif else begin

      ;;
      ;; Reshape 'state' and 'ltime' to the same shape as the input 'et'
      ;; then return.
      ;;
      return, { state:reform( state, [6, datum[ 1:datum[0] ] ] ), $
                ltime:reform( ltime,     datum[ 1:datum[0] ]   ) }

   endelse

END

