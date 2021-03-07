; $Id: mean.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $
function mean,x,dim,_EXTRA=e

   ; multidimensional version from Kevin Ivory (04/03/1997)

   on_error,2

   if (n_elements(dim) eq 0) then dim = 0
   return,total(x,dim,_EXTRA=e)/(total(finite(x),dim)>1)

end
