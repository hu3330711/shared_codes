# AWK Program File
#
# modeperiods.awk
# 
# PURPOSE
#
# Receives output from sql stored procedure adj_modeperiods_orbit
# to create a one-line, formatted description of instrument turn-ons. 
#
# INPUTS
#
# Standard input from do_isql.
# Command line option -v assigns value to ORBIT variable.
#
# USAGE
#
# printf "%s %s\ngo\n" "adj_modeperiods_orbit" "$orbit" |
#	 do_isql |
#	 nawk -f modeperiods.awk -v "ORBIT=$orbit" -
#

# Function converts month string (length 3) to number

function monthnum(mon)
{
  m["Jan"] = "01";
  m["Feb"] = "02";
  m["Mar"] = "03";
  m["Apr"] = "04";
  m["May"] = "05";
  m["Jun"] = "06";
  m["Jul"] = "07";
  m["Aug"] = "08";
  m["Sep"] = "09";
  m["Oct"] = "10";
  m["Nov"] = "11";
  m["Dec"] = "12";
  return m[mon]
}

# Function gives number of minutes accumulated in all prevous months

function priormin(mon)
{
  m["Jan"] = 0;
  m["Feb"] = 44640;
  m["Mar"] = 84960;
  m["Apr"] = 129600;
  m["May"] = 172800;
  m["Jun"] = 217440;
  m["Jul"] = 260640;
  m["Aug"] = 305280;
  m["Sep"] = 349920;
  m["Oct"] = 393120;
  m["Nov"] = 437760;
  m["Dec"] = 480960;
  return m[mon]
} 
 
BEGIN {

# Initialization

# ORBIT is defined on command line call to nawk
  inst_stat = 0; # Remembers whether instruments on or off
  pmode_old = -1;
  fmode_old = -1;
  survey_old = -1;
  start_old = "00-00-00/00:00:00";
  finish_old = "00-00-00/00:00:00";
  abs_old = 0;
  refugee = 0;

# Open the output line

  printf("%s ", ORBIT)
}

# Main

{
# Disregard all input lines satisfying the following criteria:
#   Less than 11 fields
#   Particle or Field mode equal to zero
#   Backorbit with Particle or Field mode not equal to 255
#   Any configuration with negligible duration, except double 255
  
  if (NF != 11) next ;
  if ($1 == 0 || $2 == 0) next ;
  if ($3 == 2 && ($1 != 255 || $2 != 255)) next ;
  if ($7 == $11 && ($1 != 255 || $2 != 255)) next ;
  refugee++ # Counter tracks number of lines let through
  #print;
  #next;

# Instrument Configuration Variables

  pmode = $1;
  fmode = $2;
  survey = $3;

# Start Time

  month_s = $4; 
  monum_s = monthnum(month_s);
  day_s = $5;
  year_s = substr($6,3,2);
  time_s = $7;
  dummy = split(time_s, hh_mm_s, ":");
  hours_s = hh_mm_s[1];
  merid_s = substr(hh_mm_s[2], 3, 2);
  if (merid_s == "PM" && hours_s < 12) hours_s = hours_s + 12;
  if (merid_s == "AM" && hours_s == 12) hours_s = "00";
  minutes_s = substr(hh_mm_s[2], 1, 2);
  sec_s = "00";
  abs_s = priormin(month_s) + day_s*24*60 + hours_s*60 + minutes_s;
  start = sprintf("%s-%s-%s/%s:%s:%s",year_s,monum_s,day_s,hours_s,minutes_s,sec_s);

# Finish Time

  month_e = $8; 
  monum_e = monthnum(month_e);
  day_e = $9;
  year_e = substr($10,3,2);
  time_e = $11;
  dummy = split(time_e, hh_mm_e, ":");
  hours_e = hh_mm_e[1];
  merid_e = substr(hh_mm_e[2], 3, 2);
  if (merid_e == "PM" && hours_e < 12) hours_e = hours_e + 12;
  if (merid_e == "AM" && hours_e ==12) hours_e = "00";
  minutes_e = substr(hh_mm_e[2], 1, 2);
  sec_e = "00";
  abs_e = priormin(month_e) + day_e*24*60 + hours_e*60 + minutes_e;
  finish = sprintf("%s-%s-%s/%s:%s:%s",year_e,monum_e,day_e,hours_e,minutes_e,sec_e);

# Now print the formatted output.  Make sure no new ON periods begin
# before or at the same time the previous one ends.
# Never turn instruments on unless they were off, or off unless on.

# If first given record not in backorbit, assume instruments turn on this entry

  if (refugee == 1 && survey != 2 && inst_stat == 0) {
    printf("%s ", finish);
    inst_stat = 1;
    abs_old = abs_e;
  }

# If neither mode equals 255 then start

  if ((inst_stat == 0) && (pmode != 255 && fmode != 255) && (abs_s > abs_old)) {
    printf("%s ", start);
    inst_stat = 1;
    abs_old = abs_s;
  }

# If Survey equals 0 or 1 and one mode not equal to 255 then start

  if ((inst_stat == 0) && (survey == 0 || survey == 1) && (pmode != 255 || fmode != 255) && (abs_s > abs_old)) {
    printf("%s ", start);
    inst_stat = 1;
    abs_old = abs_s;
  }

## If Survey changes from 2 (backorbit) to 0 or 1 (fast, slow) then start
#
#  if ((survey == 0 || survey == 1) && (survey_old == 2) && (inst_stat == 0)) {
#    printf("%s ", start);
#    inst_stat = 1;
#  }
#
## If modes were both 255 and now one is not, then start
#
#  if ((pmode_old == 255 && fmode_old == 255) && (pmode != 255 || fmode != 255)  && inst_stat == 0) {
#    printf("%s ", start);
#    inst_stat = 1;
#  }

# If both particle and Field modes equal 255, then finish

  if (inst_stat == 1 && (pmode == 255 && fmode == 255) && (abs_s > abs_old)) {
    printf("%s ", start);
    inst_stat = 0;
    abs_old = abs_s;
  }

# If Survey equals 2 and one mode equals 255 then finish

  if (inst_stat == 1 && survey == 2 && (pmode == 255 || fmode == 255) && (abs_s > abs_old)) {
    printf("%s ", start);
    inst_stat = 0;
    abs_old = abs_s;
  }

## If Survey changes from 0 or 1 to 2, then finish
#  
#  if ((survey_old == 0 || survey_old == 1) && survey==2 && inst_stat==1) {
#    printf("%s ", start);
#    inst_stat = 0;
#  }


# The following assignments occur after all processing of current line

  pmode_old = pmode; 
  fmode_old = fmode;
  survey_old = survey;
  start_old = start;
  finish_old = finish;
}

END {

# If instruments still ON, but no more entries this orbit, assume turn-off

  if (inst_stat == 1) {
    printf("%s", finish);
    inst_stat = 0;
  }

# Close the output line

printf("\n")

}
