# AWK Program File
#
# inst_sched.awk
# 
# PURPOSE
#
# Receives output from sql stored procedure adj_modeperiods_orbit
# to create a instument mode configuration schedule
#
# INPUTS
#
# Standard input from do_isql.
# Command line option -v assigns value to ORBIT variable.
#
# USAGE
#
# printf "%s %s\ngo\n" "adj_modeperiods_orbit" "$orbit" |
#        do_isql |
#        nawk -f inst_sched.awk -v "ORBIT=$orbit" -
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

BEGIN {

# Initialization

# ORBIT is defined on command line call to nawk
  inst_stat = 0; # Remembers whether instruments on or off
  pmode_old = -1;
  fmode_old = -1;
  survey_old = -1;
  start_old = "00-00-00/00:00:00";
  finish_old = "00-00-00/00:00:00";
  refugee = 0;

# Print the Header

  printf("%-10s %-10s %-17s %-17s\n", "PARTICLE", "FIELD", "BEGIN", "END")
  printf("%-10s %-10s %-17s %-17s\n", "--------", "-----", "-----", "---")
}

# Main

{
# Disregard all input lines satisfying the following criteria:
#   Less than 11 fields
#   Particle or Field mode equal to zero
#   Backorbit with Particle or Field mode not equal to 255
#   Old particle and field modes same as new
#   Any configuration with negligible duration (commented)

  if (NF != 11) next ;
  if ($1 == 0 || $2 == 0) {next ;}
  if ($3 == 2 && ($1 != 255 || $2 != 255)) {next ;}
  # Next line removed 2001.02.01 - Jeremy Huddleston.
  # It causes data to be dropped.  See orbit 10079 for an example.
  #if ($1 == pmode_old && $2 == fmode_old) {next}
  #if ($7 == $11) {next ;}
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
  finish = sprintf("%s-%s-%s/%s:%s:%s",year_e,monum_e,day_e,hours_e,minutes_e,sec_e);


# Print output line

  printf("%-10s %-10s %-17s %-17s\n", pmode, fmode, start, finish)

# The following assignments occur after all processing of current line

  pmode_old = pmode; 
  fmode_old = fmode;
  survey_old = survey;
  start_old = start;
}
