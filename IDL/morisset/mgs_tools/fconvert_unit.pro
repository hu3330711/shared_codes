; $Id: fconvert_unit.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $


function fconvert_unit,data,unit,toparam,result=result,_EXTRA=e

    ; simply take data, call convert_unit procedure and
    ; return data

    convert_unit,data,unit,toparam,result=result,_EXTRA=e

    return,data

end

