clear
cd "~/Desktop/jotarepos/etfs/data"
insheet using "etfalldata.csv", clear














destring, replace ignore()

bysort session round groupid: egen groupy1 = min(globalid)
bysort session round groupid: egen groupy2 = median(globalid)
bysort session round groupid: egen groupy3 = max(globalid)


sort id round period
by id round: gen q = choice[_n+1] - choice[_n]
gen enter = 0 
replace enter = 1 if q<0 
replace q =0 if q<0 | q==.
bysort session round: egen lowxround = min(x)

g xlow = 0
replace xlow = 1 if lowxround<186

g xlow2 = 0
replace xlow2 = 1 if lowxround<91


g xtre = tre*x


g xlowtre = tre*xlow
xtset id
bootstrap, cluster(ids) idcluster(new) group(id) seed(200): xtlogit q tre, re if choice<1
bootstrap, cluster(ids) idcluster(new) group(id) seed(200): xtlogit q tre x xtre, re 

bootstrap, cluster(ids) idcluster(new) group(id) seed(350): xtreg q tre xlow xlowtre, re 



***Survival Analysis***
g idr = session*10000+globalid*100+round

bysort session round choice: egen lowvalue = min(x)

bysort idr: egen everq = max(choice)

egen maxvalue= max(x)
gen xflip = maxvalue - x
preserve

keep if q==1
save quitdata.dta, replace
restore
keep if period==80 & everq==0
append using quitdata


**clean out periods** only quit decisions!
gen censored = 1-q
gen xlow_flip = maxvalue - lowvalue
gen t = xflip if q==1
replace t = xlow_flip if everq==0
stset t , failure(censored==1)
sts graph, by(tre)















tsset uidr  period 
tsspell choice

g dura = _end * choice * _seq
tab dura if tre<1 & dura>0

xtlogit q tre, i(uid)




