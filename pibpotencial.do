cap log close
set more off
version 11.2
clear all
set linesize 90
set scheme sj

*Productivity*

******************************** Working Directory ***********************************
global source "D:\CARLOS_DIAZ\MIS_DOC\CRECIMIENTO&DESARROLLO\PIB_POTENCIAL\PIBPOT\"
global logoutput "D:\CARLOS_DIAZ\MIS_DOC\CRECIMIENTO&DESARROLLO\PIB_POTENCIAL\PIBPOT\"
**************************************************************************************

log using ${logoutput}PIBPOT.log, replace
use ${source}basepibpot.dta
save ${source}basepibpotout.dta,replace

generate fecha = yq(year, qrt) // Define variable fecha

tsset fecha, quarterly // e dice a STATA que va a trabajar con series temporales

*** 1. PIB POTENCIAL POR HODRICK-PRESCOTT ***
hprescott pib, stub(HP) //¡¡¡DEBE ASEGURARSE QUE STATA TIEEN INSTALADO EL COMANDO!!! *(findit hprescott)*

tsline pib HP_pib_1 HP_pib_sm_1 // LAS SERIES GENERADAS HP_pib_1 (SERIE ciclo); HP_pib_sm_1 (pib smooted)


*** FILTRO BAXTER & KING ***

gen DTpib=(pib/L1.pib)-1
gen DTpibq=(pib/L4.pib)-1

bking pib, plo(6) phi(32) stub(BK) k(12)
gen bkpib=pib-BK_pib_1
bking DTpib, plo(6) phi(32) stub(BK1) k(12) 
bking DTpibq, plo(6) phi(32) stub(BK2) k(12)



*** Christiano-Fitzgerald Filter ***
cfitzrw pib, plo(6) phi(32) stub(cf)
gen cfpib=pib-cf_pib_1


/*
gen prueba1=pib-HP_pib_1
tsline prueba1 HP_pib_sm_1
*/
*** Butterwotrth Filter ***

butterworth pib, freq(16) stub(bt)
gen btpib = pib-bt_pib_1

tsline pib cfpib bkpib HP_pib_sm_1 btpib

log close		
exit
