; $Id: strsize.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $



function strsize,strarg,width


     xch = float(!d.x_ch_size)/!d.x_size
     if (strlen(strarg) gt 0) then $
       return,width/(xch*strlen(strarg)) $
     else $
       return,1.

end

