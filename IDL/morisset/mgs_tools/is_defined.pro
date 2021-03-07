; $Id: is_defined.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $
function is_defined,arg

    ; from David Fanning, got it from newsgroup on 2 Jul 1998

    return,keyword_set(n_elements(arg))

end

