;-------------------------------------------------------------
; $Id: install.pro,v 1.1 1999/03/24 04:30:05 mgs Exp $
;+
; NAME:
;        INSTALL
;
; PURPOSE:
;        Generic installation routine. Installs files from a
;        listfile or by filemask. Creates target directory 
;        and makes automatic backup of old versions. With the
;        RCS keyword set, files will be extracted from the
;        version control system instead of being copied from the
;        master directory.
;
; CATEGORY:
;        Installation
;
; CALLING SEQUENCE:
;        INSTALL ,targetdir,masterdir [,options]
;
; INPUTS:
;        TARGETDIR -> Name of the directory where the files shall
;            be located. Default is '.'.
;            Default is ~/IDL/gamap
;
;        MASTERDIR -> Name of the directory from which files tchall
;            be copied. If the RCS keyword is set, this is the directory
;            that contains the master repository, not the RCS directory
;            itself. Default is '.'. Note: Target and master directory
;            must differ.
;
; KEYWORD PARAMETERS:
;        LISTFILE -> Name of a file that contains the names of all
;            files to be copied. The files listed in LISTFILE will 
;            be searched in MASTERDIR+'/'+filename, i.e. they should
;            be specified with relative path information. Empty lines
;            or lines that start with '#' are ignored. Tip: Use
;            ls -1 > listfile   to create a listfile, then edit manually.
;            LISTFILE must be given as fully qualified path.
;
;        FILEMASK -> As an alternative to providing a listfile, one
;            can specify a filemask. Then all files matching this
;            filemask will be copied/extracted. Again: filemask is 
;            attached to MASTERDIR.
;
;        /RCS -> Set this flag to extract files from the version 
;            control system rather than copying them over.
;
;        R_ATTRIBUTE -> AN optional string that contains extra parameters
;            for the RCS co command (e.g. '-sStab' to extract the latest
;            stable version of a file).
;
;        /NO_BACKUP -> Set this flag if you do not want to create a 
;            backup copy of existing files with the same name.
;
;        LOGFILE -> Optional filename of an IDL journal file. Default
;            is '~/install.log'. Install will open (and close) a 
;            journal file unless one has already been opened before 
;            (i.e. by a caller routine that installs a larger package).
;
; OUTPUTS:
;        The selected files will be installed or updated in TARGETDIR.
;        A backup copy of older versions is made in TARGETDIR/BACKUP<date>.
;        A journal file is created.
;
; SUBROUTINES:
;
; REQUIREMENTS:
;        Uses  strdate, strwhere, strrepl, yesno, mfindfile,
;           extract_filename from the tools suite.
;
; NOTES:
;        The IDL search path is temporarily extended (TOOLS_HOME is
;        added to ensure proper working of install).
;
;        The working directory is temporarily changed to the TARGETDIR
;        and changed back at the end.
;
; EXAMPLE:
;        Install,'./newprog','~master/IDL/progs',listfile='progs.list'
;
;        ; Copies all files from ~master/IDL/progs/progs.list into
;        ; ./newprog
;
;        DFrom = './newprog'
;        DTo   = '~master/IDL/progs'
;        Install,DFrom,DTo,filemask='*.pro',/RCS,R_Attr='-sStab'  
;
;        ; Extracts the latest stable version of all IDL pro files
;        ; from ~master/IDL/progs/RCS and copies them into ./newprog
;
; MODIFICATION HISTORY:
;        mgs, 22 Jan 1999: VERSION 1.00
;        mgs, 25 Jan 1999: - Install now generic. See install_gamap.pro
;               for the installation of the GAMAP package.
;
;-
; Copyright (C) 1999, Martin Schultz, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author to arrange payment.
; Bugs and comments should be directed to mgs@io.harvard.edu
; with subject "IDL routine install"
;-------------------------------------------------------------


pro install,targetdir,masterdir,listfile=listfile,filemask=filemask, $
        RCS=RCS,R_Attribute=R_Attribute,No_Backup=No_Backup, $
        LogFile=LogFile
 

   FORWARD_FUNCTION strdate,strwhere,strrepl,yesno,mfindfile,  $
            extract_filename


   ; =========================================================
   ; Since there is so much undefined during install, let's
   ; 'link' to a master tool directory that contains the 
   ; necessary functions.   ***!!!!  HARDWIRING HERE !!!!***
   ; =========================================================

   TOOLS_HOME = '~mgs/IDL/tools'

   cd,current=old_dir
   cd,TOOLS_HOME
   resolve_routine,'strdate',/IS_FUNCTION
   resolve_routine,'strwhere',/IS_FUNCTION
   resolve_routine,'strrepl',/IS_FUNCTION
   resolve_routine,'yesno',/IS_FUNCTION
   resolve_routine,'mfindfile',/IS_FUNCTION
   resolve_routine,'extract_filename',/IS_FUNCTION
   cd,old_dir

   ; add TOOLS_HOME to IDL !PATH variable 
   Old_Path = !Path
   !Path = TOOLS_HOME + ':' + !Path
 

   ; =========================================================
   ; Variable defaults
   ; =========================================================

   RCS = keyword_set(RCS)
   if (n_elements(R_Attribute) eq 0) then R_Attribute = ''
   rcsdir = ''
   rcsext = ''
   if (RCS) then begin
      rcsdir = '/RCS'      ; will be appended to masterdir
      rcsext = ',v'        ; will be appended to filename
   endif

   No_Backup = keyword_set(No_Backup)

   state = 0   ; just the very beginning

   ; =========================================================
   ; Open a journal file for the installation log
   ; Default is 'install.log' in the user's home directory
   ; because we don't know whether the target dir exists.
   ; =========================================================
   if (n_elements(LogFile) eq 0) then LogFile = '~/install.log'

   ; open journal file only if not opened previously
   if (!JOURNAL le 0) then begin
      journal,LogFile
      journal_close = 1
   endif else journal_close = 0

   ; =========================================================
   ; Get default for targetdir and masterdir
   ; =========================================================

   if (n_elements(targetdir) eq 0) then begin
      message,'No target directory specified. Will use .',/Cont
      targetdir = '.'
   endif
   if (n_elements(masterdir) eq 0) then begin
      message,'No master directory specified. Will use .',/Cont
      masterdir = '.'
   endif

   if (targetdir eq masterdir) then begin
      print,'Master and target directory identical! Installation aborted!', $
           /Cont
      goto,finished
   endif

   journal,'Master directory : '+masterdir
   journal,'Target directory : '+targetdir
   journal,''
    

   ; =========================================================
   ; Test if masterdir and targetdir exist
   ; =========================================================

   spawn,'ls '+masterdir+'/*',result
   ; result will be empty string if no files found
   if (result[0] eq '') then begin
      message,'Cannot find Master directory ! Installation aborted.',/Cont
      goto,finished
   endif

 
   spawn,'ls '+targetdir+'/* ',result


   if (result[0] eq '') then begin
   ; --- NEW DIRECTORY ---
   ; NOTE: Recursive creation not possible at this stage! *****
 
      print
      print,'Target directory '+targetdir+' does not exist '+ $
             '(or is empty).'
      ans = yesno('Shall I create it?',def=1)
 
      if (ans) then begin
         spawn,'rm -rf '+targetdir,result ; just in case it exists but is empty
         spawn,'mkdir '+targetdir,result
      endif
 
      No_Backup = 1
 
   endif else if (not No_Backup) then begin
   ; --- OLD DIRECTORY : UPDATE ---
 
      ; create a backup copy of everything in targetdir
      ; make a nice date_time string:
      dt = strdate()
      dt = strcompress(strrepl(dt,strwhere(dt,' '),'_'),/remove)
      dt = strrepl(dt,strwhere(dt,'/'),'-')
 
      backupdir = targetdir+'/BACKUP'+dt
      print
      print,'I will backup all current files in '+targetdir+ $
            ' to '+backupdir+'...'
 
      spawn,'mkdir '+backupdir
      spawn,'cp -pf '+targetdir+'/* '+backupdir  
      print,'Done with backup.'
      print,'In case the installation terminates abnormally, you may '
      print,'find a non-functional mix of old and new files.'
      print
 
   endif
 
   ; ======================================
   ; Set current directory to targetdir
   ; ======================================

   cd,current=old_dir
   cd,targetdir
   State = 1   
   
   ; ======================================
   ; Create list of files to install or 
   ; update
   ; ======================================

   if (n_elements(listfile) gt 0) then begin
   ; --- FILENAMES STORED IN LISTFILE ---
      full = listfile[0]
      print,'Read files from listfile '+full
      open_file,full,ilun,/NO_PICKFILE
      if (ilun le 0) then begin
         message,'Cannot find list of required tools !',/Cont
         message,'Installation terminated.',/INFO,/NoName
         goto,finished
      endif
      State = 2

      LStr = ''
      FileList = ''
      while (not eof(ilun)) do begin

          readf,ilun,LStr
          ; skip empty lines and comment lines
          LStr = strtrim(LStr,2)
          if (LStr eq '') then goto,read_next
          if (strmid(LStr,0,1) eq '#') then goto,read_next

          FileList = [ FileList, LStr ]

read_next:
      endwhile

      if (n_elements(FileList) eq 1) then begin
          message,'No filenames in Listfile !',/Cont
          goto,finished
      endif

      FileList = masterdir + rcsdir + '/' + Filelist[1:*] + rcsext


   endif else begin
   ; --- GET FILELIST FROM FILEMASK ---
     
      ilun = 0    ; no listfile - logical unit necessary for termination

      if (n_elements(filemask) eq 0) then begin
         message,'Neither LISTFILE nor FILEMASK specified! Will use *', $
              /INFO
         filemask = '*'
      endif

      fullmask = masterdir + rcsdir + '/' + filemask

      filelist = mfindfile(fullmask+rcsext)
      if (filelist[0] eq '') then begin
          message,'No files match specification '+fullmask+' !',/Cont
          goto,finished
      endif
 
   endelse

print,'DEBUG:',filelist


   ; ======================================
   ; For RCS files, link master RCS directory
   ; to target directory
   ; ======================================
 
   ; create symbolic link to RCS directory in GAMAP_HOME
   if (RCS) then spawn,'ln -s '+masterdir+rcsdir+' ./RCS'
   State = 3
   

   ; ======================================
   ; Loop through filelist and install or 
   ; update the files
   ; ======================================

   for i=0,n_elements(FileList)-1 do begin

      ; test for install or upgrade 
      thisfilename = extract_filename(FileList[i])
      if (RCS) then thisfilename =  $
           strmid(thisfilename,0,strpos(thisfilename,rcsext))
print,thisfilename
      test = mfindfile(thisfilename)
      if ( strtrim(test[0],2) eq '' ) then infostr = 'Installing ' $
      else infostr = 'Updating ' 

      ; print progress message
      print,infostr,thisfilename,' ...'

      ; extract RCS file or copy file
      if (RCS) then $
         spawn,'co '+R_Attribute+' '+FileList[i]  $
      else  $
         spawn,'cp -f '+FileList[i]+' .'
 
   endfor


   State=4
 
 
   ; ======================================
   ; That's all. Clean up now ...
   ; ======================================

Finished:

   ; restore old PATH variable
   if (n_elements(Old_Path) gt 0) then !Path = Old_Path

   ; remove symbolic link
   if (State gt 2 AND RCS) then spawn,'rm -rf ./RCS'

   ; close listfile 
   if (State gt 1 AND ilun gt 0) then free_lun,ilun

   ; restore old working directory
   if (State gt 0) then cd,old_dir

   ; reset error status
   if (State gt 3) then message,/Reset

   ; close journal file
   if (journal_close) then journal

 
   return
end
 
