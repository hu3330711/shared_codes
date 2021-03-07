create table iug_ionospheric_cond (
       sigma_0 real,
       sigma_1 real,
       sigma_2 real,
       sigma_xx real,
       sigma_yy real,
       sigma_xy real,
       height real,
       glat real,
       glon real,
       yyyy int,
       mmdd int,
       ltut int,
       atime int,
       algorithm int,
       primary key(height, glat, glon, yyyy, mmdd, ltut, atime, algorithm)
);