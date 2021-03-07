;;“ñŸŠÖ”‚É‚æ‚é•âŠÔ
pro calc_2nd_order_curve,x,x0,y0,x1,y1,x2,y2,result=result
result=(x-x1)*(x-x2)/(x0-x1)/(x0-x2)*y0 $
      +(x-x0)*(x-x2)/(x1-x0)/(x1-x2)*y1 $
      +(x-x0)*(x-x1)/(x2-x0)/(x2-x1)*y2
end