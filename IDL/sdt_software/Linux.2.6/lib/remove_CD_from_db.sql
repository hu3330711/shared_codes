use xxARCH_DBxx
go

/* find all contacts corresponding to counts files on this CD 
 * save start and end times for deleting from operational_events
 */
select contact_num, start, finish into #contact_del from files, contacts
 where volname = "xxVOLNAMExx" and counts_file = ident

if (select count(*) from #contact_del) = 0 
	begin
/* find the contacts by the orbit number taken from the counts file 
 * pathnames in the files table 
 */
	select convert(numeric(6), 
	   substring(pathname, patindex("%_orbit%", pathname) + 6,
	   patindex("%[_]_/%",pathname) - patindex("%_orbit%", pathname) -6)) 
	as orb
	into #condel
	from files where volname = "xxVOLNAMExx" 
	             and pathname like "%.cnt"
	insert #contact_del
	select contact_num, start, finish from contacts, #condel 
	where orb = orbit_num
	end

go

print "deleting all entries related to contacts on CD xxVOLNAMExx: "
go

select c.start, c.finish, station_ident, orbit_num, session_type from contacts c, #contact_del cd
 where c.contact_num = cd.contact_num

go

/* find engineering periods associated with contacts
 * save start and end times for deleting from operational events
 */
select eng_period, e.start, e.finish into #eng_del from engineering_periods e, #contact_del cd 
 where e.contact_num = cd.contact_num

/* find HSBM data associated with contacts
 * save start and end times for deleting from operational events
 */
select HSBM_num, h.start, h.finish into #HSBM_del from HSBM_data h, #contact_del cd
 where h.contact_num = cd.contact_num

/* find burst data associated with contacts
 * save start and end times for deleting from operational events
 */
select b.burst_num, b.start, b.finish into #burst_del 
 from burst_APID ba, burst_data b, #contact_del cd
 where ba.contact_num = cd.contact_num and ba.burst_num = b.burst_num

/* find mode periods associated with contacts
 * save start and end times for deleting from operational events
 */
select mp.mode_period, mp.start, mp.finish into #mode_del 
 from mode_APID ma, mode_periods mp, #contact_del cd
 where ma.contact_num = cd.contact_num and mp.mode_period = ma.mode_period

/* find operational events which had been referenced by deleted entries. 
 */
print "looking for operational events related to contact"
go
select time, data into #op_del from operational_events, #contact_del
 where time = start 
insert #op_del
select time, data from operational_events, #contact_del
 where time = finish
print "looking for operational events related to engineering periods"
go
insert #op_del
select time, data from operational_events, #eng_del
 where time = start
insert #op_del
select time, data from operational_events, #eng_del
 where time = finish
print "looking for operational events related to HSBM data"
go
insert #op_del
select time, data from operational_events, #HSBM_del
 where time = start
insert #op_del
select time, data from operational_events, #HSBM_del
 where time = finish
print "looking for operational events related to burst data"
go
insert #op_del
select time, data from operational_events, #burst_del
 where time = start
insert #op_del
select time, data from operational_events, #burst_del
 where time = finish
print "looking for operational events related to mode periods"
go
insert #op_del 
select time, data from operational_events, #mode_del
 where time = start 
insert #op_del 
select time, data from operational_events, #mode_del
 where time = finish

go

begin tran
go

print "deleting from engineering periods"
go
delete engineering_periods from engineering_periods e, #eng_del ed 
 where e.eng_period = ed.eng_period

go

print "deleting from HSBM_data"
go
delete HSBM_data from HSBM_data h, #HSBM_del hd
 where h.HSBM_num = hd.HSBM_num
go

print "deleting from burst_APID"
go
delete burst_APID from burst_APID ba, #burst_del bd
 where ba.burst_num = bd.burst_num
go

print "deleting from burst_data"
go
delete burst_data from burst_data b, #burst_del bd
 where b.burst_num = bd.burst_num

go
print "deleting from mode_APID"
go
delete mode_APID from mode_APID ma, #mode_del md
 where ma.mode_period = md.mode_period

print "deleting from mode_periods"
go
delete mode_periods from mode_periods mp, #mode_del md
 where mp.mode_period = md.mode_period

go
/* delete contact_APID associated with contacts
 */
print "deleting from contact_APID"
go
delete contact_APID from contact_APID ca, #contact_del cd
 where ca.contact_num  = cd.contact_num

go
/* delete the contacts which have counts files on this CD.
 */
print "deleting from contacts"
go
delete contacts from contacts c, #contact_del cd
 where c.contact_num = cd.contact_num

go
/* delete operational events, then delete events data
 */
print "deleting from operational_events"
go
delete operational_events from operational_events oe, #op_del od
 where oe.time = od.time

go
print "deleting from events_data"
go
delete events_data from events_data, #op_del
 where ident = data

go
/* find entries in contact_APID or contacts which refer to files on this CD
 * which correspond to a contact which has its counts file on a different CD
 * and set the references to NULL
 */
print "updating remaining contacts entries referring to files on this CD to NULL"
go
update contacts set reject_file = NULL from contacts, files 
 where volname = "xxVOLNAMExx" and reject_file = ident
print "updating remaining contact_APID entries referring to files on this CD to NULL"
go
update contact_APID set contact_file = NULL from contact_APID, files 
 where volname = "xxVOLNAMExx" and contact_file = ident
update contact_APID set error_file = NULL from contact_APID, files 
 where volname = "xxVOLNAMExx" and error_file = ident
 
go

/* delete all files on this CD from the files table. 
 */
print "deleting from files"
go
delete files 
 where volname = "xxVOLNAMExx" 

go

commit tran
go

print "CD removal complete"
go
