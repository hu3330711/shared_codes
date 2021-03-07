pro subsphcar,TIMES,DATA_in,DATA_out



; get array sizes
count=SIZE(DATA_in[*,0],/N_ELEMENTS)
;MESSAGE,/CONTINUE,'number of records: ' + string(count)

DATA_out=dblarr(count,3)

tsphcar_vect,DATA_in[*,0],DATA_in[*,1],DATA_in[*,2],x,y,z

;for i=0L,count-1L do begin
		;ctimpar,iyear,imonth,iday,ih,im,is
		;This has to be changed to be faster!!!!!!!!!!!!!!!
;		ctimpar,TIMES[i].year,TIMES[i].month,TIMES[i].date,TIMES[i].hour,TIMES[i].min,double(TIMES[i].sec)+TIMES[i].fsec
;		tgeigse,DATA_in[i,0],DATA_in[i,1],DATA_in[i,2],xgse,ygse,zgse
		DATA_out[*,0]=x
		DATA_out[*,1]=y
		DATA_out[*,2]=z
;endfor


;return,DATA_out
end

      pro tsphcar_vect,R,THETA,PHI,X,Y,Z
;
;   CONVERTS SPHERICAL COORDS INTO CARTESIAN ONES AND VICA VERSA
;    (THETA AND PHI IN RADIANS).
;
;                  J>0            J<0
;-----INPUT:   J,R,THETA,PHI     J,X,Y,Z
;----OUTPUT:      X,Y,Z        R,THETA,PHI
;
;  NOTE: AT THE POLES (X=0 AND Y=0) WE ASSUME PHI=0 (WHEN CONVERTING
;        FROM CARTESIAN TO SPHERICAL COORDS, I.E., FOR J<0)
;
;   LAST MOFIFICATION:  APRIL 1, 2003 (ONLY SOME NOTATION CHANGES AND MORE
;                         COMMENTS ADDED)
;
;   AUTHOR:  N. A. TSYGANENKO
;

      SQ=R*SIN(THETA)
      X=SQ*COS(PHI)
      Y=SQ*SIN(PHI)
      Z=R*COS(THETA)
      RETURN
END
