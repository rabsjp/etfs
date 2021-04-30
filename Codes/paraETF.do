clear
cd "~/Desktop/jotarepos/etfs/data"
insheet using "etfmarketdata.csv", clear

drop v1 sessioncode

destring groupnav_a groupnav_b groupnav groupp_a groupp_b groupp_c groupq_c playernbidc playernaskc active_c group1, replace force

tabulate tre, generate (dum)

rename (dum1 dum2 dum3 dum4 playerboxes_collected) (N2 N3 Z2 Z3 bomb)

g dum_z = 0
replace dum_z = 1 if Z2==1
replace dum_z = 1 if Z3==1

g dum_3 = 0 
replace dum_3 = 1 if Z3==1 
replace dum_3 = 1 if N3==1

g dum_m = dum_z*dum_3

g late = 0
replace late = 1 if subsessionronda>8

g dum_zl = dum_z*late
g dum_3l = dum_3*late
g dum_ml = dum_m*late

g dum_zp = dum_z*subsessionronda
g dum_3p = dum_3*subsessionronda
g dum_mp = dum_m*subsessionronda


gen rad_a=abs((groupp_a/fva)-1)

gen rad_b=abs((groupp_b/fvb)-1)

gen rad_c=abs((groupp_c/fvc)-1)

gen rad_cnav=abs((groupp_c/groupnav)-1)

gen rad_cnav_l= abs(groupp_c-groupnav)

gen rd_cnav = groupp_c/groupnav-1

gen rd_cnav_l = groupp_c-groupnav


gen rad_ab=abs(((groupp_b/fvb)/(groupp_a/fva))-1)

regress rad_a N3 subsessionround_number if Z2==0 & Z3==0


regress rad_ab N3 subsessionround_number if Z2==0 & Z3==0

regress rad_ab Z3 subsessionround_number if N2==0 & N3==0

regress rad_c N3 subsessionround_number if Z2==0 & N2==0

regress rad_cnav N3 subsessionround_number if Z2==0 & N2==0

regress rad_b N3 subsessionround_number if Z2==0 & Z3==0

regress rad_a N3 subsessionround_number if Z2==0 & Z3==0 & subsessionround_number>15, vce(cluster session)

regress rad_b N3 subsessionround_number if Z2==0 & Z3==0 & subsessionround_number>15, vce(cluster session)

regress rad_ab N3 subsessionround_number if Z2==0 & Z3==0 & subsessionround_number>15, vce(cluster session)

regress rad_c N3 subsessionround_number if Z2==0 & N2==0 & subsessionround_number>15, vce(cluster session)


xtset uses
preserve
keep if subsessionround_number>15
bootstrap, cluster(session) idcluster(new) group(uses) seed(350): xtreg rad_a dum_z dum_3 dum_m, re 
bootstrap, cluster(session) idcluster(new) group(uses) seed(350): xtreg rad_a dum_z dum_3 dum_m, re 

bootstrap, cluster(session) idcluster(new) group(uses) seed(350): xtreg rad_b dum_z dum_3 dum_m, re 

restore


preserve
keep if subsessionround_number>15
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_a dum_z dum_3 dum_m
test dum_3+dum_m=0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_a dum_z dum_3 dum_m subsessionronda
test dum_3+dum_m=0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_a dum_z dum_3 dum_m dum_zl dum_3l dum_ml late
test dum_3+dum_m=0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_a dum_z dum_3 dum_m dum_zp dum_3p dum_mp subsessionronda
test dum_3+dum_m=0

restore

preserve
keep if subsessionround_number>15
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_b dum_z dum_3 dum_m
test dum_3+dum_m=0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_b dum_z dum_3 dum_m subsessionronda
test dum_3+dum_m=0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_b dum_z dum_3 dum_m dum_zl dum_3l dum_ml late
test dum_3+dum_m=0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_b dum_z dum_3 dum_m subsessionronda
test dum_3+dum_m=0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_b dum_z dum_3 dum_m dum_zp dum_3p dum_mp subsessionronda
test dum_3+dum_m=0


restore

preserve
keep if subsessionround_number>15
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_ab dum_z dum_3 dum_m
test dum_3+dum_m=0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_ab dum_z dum_3 dum_m subsessionronda
test dum_3+dum_m=0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_ab dum_z dum_3 dum_m dum_zl dum_3l dum_ml late
test dum_3+dum_m=0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_ab dum_z dum_3 dum_m dum_zp dum_3p dum_mp subsessionronda
test dum_3+dum_m=0


*keep if  N2==0 & N3==0
*bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_ab dum_3 
*bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200):  reg rad_c dum_z 
*bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200):  reg rad_c dum_z subsessionronda

restore


g trend_dummy = subsessionronda*dum_z
preserve
keep if subsessionround_number>15
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_cnav dum_z subsessionronda trend_dummy
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rad_cnav_l dum_z subsessionronda trend_dummy
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd_cnav_l dum_z subsessionronda trend_dummy
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd_cnav_l dum_z subsessionronda 

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd_cnav dum_z subsessionronda trend_dummy
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd_cnav dum_z subsessionronda 
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd_cnav dum_z 


restore








xtset uses
preserve
keep if  Z2==0 & Z3==0
bootstrap, cluster(session) idcluster(new) group(uses) seed(350): xtreg rad_a N3 subsessionronda, re 
restore

xtset uses
preserve
keep if  Z2==0 & Z3==0
bootstrap, cluster(session) idcluster(new) group(uses) seed(350): xtreg rad_b N3 subsessionronda, re 
restore

xtset uses
preserve
keep if  Z2==0 & Z3==0
bootstrap, cluster(session) idcluster(new) group(uses) seed(350): xtreg rad_ab N3 subsessionronda, re 
restore

xtset uses
preserve
keep if  Z2==0 & Z3==0 & subsessionround_number>15
xtreg rad_ab N3 subsessionronda, re 
restore

preserve
keep if  Z2==0 & Z3==0 & subsessionround_number>15
bootstrap, cluster(session) idcluster(nuevo) seed(200): reg rad_b N3 subsessionronda
restore


xtset uses
preserve
keep if  N2==0 & Z2==0
bootstrap, cluster(session) idcluster(new) group(uses) seed(350): xtreg rad_c N3 subsessionronda, re 
restore



