clear
cd "~/Desktop/jotarepos/etfs/data"
insheet using "etfalldata.csv", clear

drop v1

destring playerbidc playernbidc playernaskc playeraskc groupnav_a groupnav_b groupnav groupp_a groupp_b groupp_c groupq_c active_c , replace force




