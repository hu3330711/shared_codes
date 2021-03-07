@compile_inventory.pro
;
; TJK - 11/6/2002 - split sp_phys catalog out since it takes so long to run.
;
; Read the metadata file...
a = ingest_database('/home/cdaweb/metadata/sp_phys_cdfmetafile.txt',DEBUG=DEBUG)
; Draw the inventory graph...
;s=draw_inventory(a,TITLE='SPACE PHYSICS',GIF='/home/cdaweb/metadata/sp_phys_cdfmetafile.gif', $
;START_TIME='1970/01/01 00:00:00',STOP_TIME='2002/12/31 23:59:59',/debug)

aa=a

s=draw_inventory(a,TITLE='SPACE PHYSICS', $
GIF='/home/cdaweb/metadata/sp_phys_cdfmetafile.gif', $
START_TIME='1969/01/01 00:00:00',STOP_TIME='2009/12/31 23:59:59',debug=debug)
a=aa
s=draw_inventory(a,TITLE='SPACE PHYSICS 1969-1974', $
GIF='/home/cdaweb/website/htdocs/sp_phys/avail_1969-1974.gif', $
START_TIME='1969/01/01 00:00:00',STOP_TIME='1974/12/31 23:59:59',debug=debug)
a=aa
s=draw_inventory(a,TITLE='SPACE PHYSICS 1975-1979', $
GIF='/home/cdaweb/website/htdocs/sp_phys/avail_1975-1979.gif', $
START_TIME='1975/01/01 00:00:00',STOP_TIME='1979/12/31 23:59:59',debug=debug)
a=aa
s=draw_inventory(a,TITLE='SPACE PHYSICS 1980-1984', $
GIF='/home/cdaweb/website/htdocs/sp_phys/avail_1980-1984.gif', $
START_TIME='1980/01/01 00:00:00',STOP_TIME='1984/12/31 23:59:59',debug=debug)
a=aa
s=draw_inventory(a,TITLE='SPACE PHYSICS 1985-1989', $
GIF='/home/cdaweb/website/htdocs/sp_phys/avail_1985-1989.gif', $
START_TIME='1985/01/01 00:00:00',STOP_TIME='1989/12/31 23:59:59',debug=debug)
a=aa
s=draw_inventory(a,TITLE='SPACE PHYSICS 1990-1994', $
GIF='/home/cdaweb/website/htdocs/sp_phys/avail_1990-1994.gif', $
START_TIME='1990/01/01 00:00:00',STOP_TIME='1994/12/31 23:59:59',debug=debug)
a=aa
s=draw_inventory(a,TITLE='SPACE PHYSICS 1995-1999', $
GIF='/home/cdaweb/website/htdocs/sp_phys/avail_1995-1999.gif', $
START_TIME='1995/01/01 00:00:00',STOP_TIME='1999/12/31 23:59:59',debug=debug)
a=aa
s=draw_inventory(a,TITLE='SPACE PHYSICS 2000-2004', $
GIF='/home/cdaweb/website/htdocs/sp_phys/avail_2000-2004.gif', $
START_TIME='2000/01/01 00:00:00',STOP_TIME='2004/12/31 23:59:59',debug=debug)
a=aa
s=draw_inventory(a,TITLE='SPACE PHYSICS 2005-2009', $
GIF='/home/cdaweb/website/htdocs/sp_phys/avail_2005-2009.gif', $
START_TIME='2005/01/01 00:00:00',STOP_TIME='2009/12/31 23:59:59',debug=debug)
;
delvar, a,aa

exit




