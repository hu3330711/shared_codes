BEGIN {
  printed_orb = 0;
  orbit = 0;
}
{
  if ($4 == "255" || $5 == "255") next;
  if ($2 == $3) next;
  orbit = $1;
  split($2, ep1, "/");
  split($3, ep2, "/");
  date1 = ep1[1];
  time1 = ep1[2];
  time2 = ep2[2];
  # ORBIT  DATE  TIME1-TIME2   P=255 F=255
  if (orbit == printed_orb) {
    printf("%-5s\t%-8s\t%8s - %8s\tP=%-3s\tF=%-3s\n", \
	   "", date1, time1, time2, $4, $5);
  } else {
    printf("%-5s\t%-8s\t%8s - %8s\tP=%-3s\tF=%-3s\n", \
	   orbit, date1, time1, time2, $4, $5);
    printed_orb = orbit;
  }
}
  

