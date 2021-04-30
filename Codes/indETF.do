clear
cd "~/Desktop/jotarepos/etfs/data"
insheet using "etfalldata.csv", clear

drop v1 sessioncode

destring groupnav_a groupnav_b groupnav groupp_a groupp_b groupp_c groupq_c playernbidc playerbidc playeraskc playernaskc active_c, replace force

tabulate tre, generate (dum)

g idplayer = session*10 + participantid_in_session

rename (dum1 dum2 dum3 dum4 playerboxes_collected) (N2 N3 Z2 Z3 bomb)
replace bomb = 100 - bomb 

g dum_z = 0
replace dum_z = 1 if Z2==1
replace dum_z = 1 if Z3==1

g dum_3 = 0 
replace dum_3 = 1 if Z3==1 
replace dum_3 = 1 if N3==1

g dum_m = dum_z*dum_3

g bz = dum_z*bomb
gen rad_a=abs((groupp_a/fva)-1)

gen rad_b=abs((groupp_b/fvb)-1)

gen rad_c=abs((groupp_c/fvc)-1)

gen rad_cnav=abs((groupp_c/groupnav)-1)

gen rad_cnav_l= abs(groupp_c-groupnav)

gen rad_ab=abs(((groupp_b/fvb)/(groupp_a/fva))-1)

gen hold_c = playern_c+playert_c
gen hold_b = playern_b+playert_b
gen hold_a = playern_a+playert_a
gen total_hold = hold_a + hold_b + hold_c
gen total_hold2 = hold_a + hold_b + 2*hold_c
g vendiotodo = 0 
replace vendiotodo =1 if total_hold2==0

*replace total_hold2=1 if total_hold2==0
replace total_hold2=. if total_hold2==0



g balan_ab = abs(hold_a-hold_b)
egen balan1_ab = rowmin(hold_a hold_b) 
replace balan1_ab = balan1_ab + hold_c
g balan_ab2 = balan_ab/total_hold2



preserve
keep if subsessionround_number==30
reg bomb vendiotodo
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg balan_ab2 bomb 
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg balan_ab2 bomb dum_z 
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg balan_ab2 bomb dum_z bz
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg playercash bomb
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg playercash bomb dum_z
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg playercash bomb dum_z bz



keep if dum_3==0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg balan_ab2 bomb 
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg balan_ab2 bomb dum_z bz
restore


reg balan_ab bomb dum_z dum_3 dum_m
reg balan_ab bomb dum_z dum_3 dum_m if total_hold>0
reg balan_ab bomb dum_z dum_3 dum_m, vce(cluster session) 
reg hold_c bomb dum_z bz
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg balan_ab bomb dum_z dum_3 dum_m
keep if total_hold>0
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg balan_ab bomb dum_z dum_3 dum_m
restore


reg balan1_ab bomb  dum_z dum_3 dum_m subsessionround_number if subsessionround_number>15



**check whether they have unique 
preserve
keep if subsessionround_number==30 
sort idplayer
keep bomb tre idplayer vendiotodo
quietly by idplayer:  gen dup = cond(_N==1,0,_n)
drop if dup>1

