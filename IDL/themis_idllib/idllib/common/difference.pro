;+
;FUNCTION: difference.pro
;
;PURPOSE: Finds difference of array elements.
;           (Returns array is one element shorter than original)
;
;ARGUMENTS:
;       INPUT_ARRAY  -> Array to find element differences of
;
;KEYWORDS: None
;
;RETURNS: Difference array
;         0 - If INPUT_ARRAY has less than 2 elements
;
;CALLING SEQUENCE: diff=difference(array)
;
;NOTES: Returned array is one (1) element shorter than INPUT_ARRAY
;
;CREATED BY:  John Dombeck  1/11/01
;
;MODIFICATION HISTORY:
;       01/11/01-J. Dombeck     created
;-
;INCLUDED MODULES:
;   difference
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   None
;
;-



;*** MAIN *** : * DIFFERENCE *

function difference,input_array

  count=n_elements(input_array)
  if count lt 2 then return,0

return,input_array(1:count-1)-input_array(0:count-2)
end        ;*** MAIN *** : * DIFFERENCE *

