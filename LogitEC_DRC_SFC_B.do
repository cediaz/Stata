cap log close
set more off
version 13.1
clear all
*set linesize 200
*set matsize 11000
*set mem 2g, perm
*set scheme sj

*Modelo de clasificación probabilidad de riesgo crédito*
*|Autor: Carlos Edison Díaz Villarreal|

******************************** Working Directory ***********************************
global source "D:\MODELO_SCORING_EC\"
global logoutput "D:\MODELO_SCORING_EC\"
**************************************************************************************

log using ${logoutput}logit2002.log, replace // Archivo de resultados
use ${source}INPUTS\Base2002.dta			//Base entidades desde enero 2002
save ${source}OUTPUT\base2002out.dta,replace	//Archivo de resultados

cls

rename icc icns

*Se eliminan IOEs:
drop if nombre_entidad == "CAJA PROMOTORA DE VIVIENDA MILITAR Y DE POLICÍA"
drop if nombre_entidad == "FONDO DE GARANTIAS DE COOPERATIVAS FOGACOOP"
drop if nombre_entidad == "INST. COL. DE CRÉDITO EDUCATIVO Y ESTUDIOS TÉCNICOS EN EL EXTERIOR - ICETEX"
drop if nombre_entidad == "FONDO NACIONAL DE GARANTIAS S.A."
drop if nombre_entidad == "FONDO FINANCIERO DE PROYECTOS DE DESARROLLO - FONADE"
drop if nombre_entidad == "Financiera De Desarrollo Nacional S.A."
drop if nombre_entidad == "FONDO PARA EL FINANCIAMIENTO DEL SECTOR AGROPECUARIO - FINAGRO"
drop if nombre_entidad == "FINANCIERA DE DESARROLLO TERRITORIAL S.A. - FINDETER S.A."
drop if nombre_entidad == "FONDO DE GARANTIAS DE INSTITUCIONES FINANCIERAS - FOGAFIN"
drop if nombre_entidad == "CAJA PROMOTORA DE VIVIENDA MILITAR Y DE POLICÍA"
drop if nombre_entidad == "FONDO DE GARANTIAS DE COOPERATIVAS FOGACOOP"
drop if nombre_entidad == "Bancoldex"




*Se codifican nombres de entidades:
encode  nombre_entidad, generate(id_EC)

*Formato de fecha stata:
format fecha_stata %tm

*Exploración de datos:
summarize
describe

*Transformar algunas variables para que todas queden en porcentaje (/100)  //Capricho del autor

replace var_provind = var_provind/100
replace pnr_ixc = pnr_ixc/100
replace pnr_mfb_ajustado = pnr_mfb_ajustado/100
replace var_cb = var_cb / 100
replace icns = icns/100
replace ici = ici/100

*Reporte de duplicados:
duplicates report fecha_stata nombre_entidad

*Se eliminan duplicados (Base original parece unir entidades fusionadas y quedan repetidas)
duplicates drop fecha_stata nombre_entidad, force
duplicates report fecha_stata nombre_entidad

****SE DEFINE PANEL DE DATOS***  //El modelo no va a ser panel de datos inicialmente debido a limitaciones de tiempo, pero queda organizado para un desarrollo futuro.
xtset id_EC fecha_stata
xtdescribe



*Genera ICV y algunos rezagos:		//Se encuentra información faltante en cartera vencida
gen ICV = cartera_vencida/cb
gen icns12 = L12.icns
gen ici12 = L12.ici
gen dtfr12 = L12.dtf_r
gen var_cb12 = L12.var_cb
gen tendcblp_2sdlow_12 = L12.tendcblp_2sdlow
gen tendcblp_1sdlow_12 = L12.tendcblp_1sdlow
gen Dici = D.ici
gen Dcns = D.icns

*sort fecha_stata


*Panorama total sistema de variables norma:
gen Limcb = 0.23
gen Limprvind = 0.09
tsline var_provind var_cb Limcb Limprvind if nombre_entidad == "TOTAL SISTEMA"
graph save ${source}OUTPUT\Var_Sirven, asis replace

gen LimPxI = 0.17
gen LimSPxM = 0.42
gen LimLPxM = 0
gen pnr_mfb_ajustado2 = pnr_acum_trimestral/mfb_ajustado_acum_trimestral
tsline pnr_ixc pnr_mfb_ajustado2  LimPxI LimSPxM if nombre_entidad == "TOTAL SISTEMA"
graph save ${source}OUTPUT\Var_NOsirven, asis replace

**SE GENERAN SERIES CON CONDICIONES DE VARIABLE DEPENDIENTE INICIAL**

*1 condición:
gen C1A = 0
replace C1A = 1 if var_provind >= Limprvind
replace C1A = . if missing(var_provind)

*2 condición:
gen C2A = 0
replace C2A = 1 if pnr_ixc >= LimPxI
replace C2A = . if missing(pnr_ixc)

*3 condición:
gen C3A = 0
replace C3A = 1 if pnr_mfb_ajustado >= LimSPxM | pnr_mfb_ajustado <= LimLPxM
replace C3A = . if missing(pnr_mfb_ajustado)

*4 condición:
gen C4A = 0
replace C4A = 1 if var_cb < Limcb
replace C4A = . if missing(var_cb)

*SE GENERA LA VARIABLE DEPENDIENTE INICIAL:
gen y_a=0
replace y_a = 1 if C1A == 1 & C2A == 1 & C3A == 1 & C4A == 1
replace y_a = . if missing(C1A)|missing(C2A)|missing(C3A)|missing(C4A)

tsline y_a if nombre_entidad == "TOTAL SISTEMA" 
graph save ${source}OUTPUT\YTOTALSISTEMA, asis replace

summarize y_a

tsline y_a if nombre_entidad != "TOTAL SISTEMA" 
graph save ${source}OUTPUT\Y_TS, asis replace

gen L1y_a = L1.y_a
gen L2y_a = L2.y_a
gen L3y_a = L3.y_a
gen L4y_a = L4.y_a
gen L5y_a = L5.y_a
gen L6y_a = L6.y_a


xtline y_a if nombre_entidad != "TOTAL SISTEMA" 
graph save ${source}OUTPUT\Y_xt, asis replace


*Número de entidades activadas en el mes:
bysort fecha_stata: egen sumy_a = sum(y_a)
tsline sumy_a, mlabel(sumy_a)
graph save ${source}OUTPUT\SUMA_ACTIVADAS, asis replace


**SE GENERAN DUMMIES TEMPORALES EN CASO DE SER NECESARIAS:

*Dummy quiebre estructural:

*Quiebre estructural CB (2004m1):
generate dsb_cb = 0
replace dsb_cb = 1 if fecha_stata >= 528

*Quiebres estructurales CV (2006m01, 2009m06)

gen dsb1_ven = 0
replace dsb1_ven = 1 if fecha_stata >= 552

gen dsb2_ven = 0
replace dsb2_ven = 1 if fecha_stata >= 593


*Y dummies mes/año... por si acaso

gen mes = month(dofm(fecha_stata))
gen m1 = (mes==1)
gen m2 = (mes==2)
gen m3 = (mes==3)
gen m4 = (mes==4)
gen m5 = (mes==5)
gen m6 = (mes==6)
gen m7 = (mes==7)
gen m8 = (mes==8)
gen m9 = (mes==9)
gen m10 = (mes==10)
gen m11 = (mes==11)
gen m12 = (mes==12)
local mes "m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11"

*gen fecha2=fecha_stata
*format fecha2 %tm
gen yr=year(dofm(fecha_stata))
gen y1 = (yr == 2002)
gen y2 = (yr == 2003)
gen y3 = (yr == 2004)
gen y4 = (yr == 2005)
gen y5 = (yr == 2006)
gen y6 = (yr == 2007)
gen y7 = (yr == 2008)
gen y8 = (yr == 2009)
gen y9 = (yr == 2010)
gen y10 = (yr == 2011)
gen y11 = (yr == 2012)
gen y12 = (yr == 2013)
gen y13 = (yr == 2014)
gen y14 = (yr == 2015)
gen y15 = (yr == 2016)
local year "y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14"






***Dummies 2SD***

*1 condición:
gen C1B2 = 0
replace C1B2 = 1 if var_cb <= tendcblp_2sdlow //| var_cb12 >= tendcblp_2sdlow_12
replace C1B2 = . if missing(var_cb)

*2 condición:
gen C2B2 = 0
replace C2B2 = 1 if var_provind >= tendpilp_2sdup
replace C2B2 = . if missing(var_provind)

*3 condición:
gen C3B2 = 0
replace C3B2 = 1 if tcr >= tendcvlp_2sdup
replace C3B2 = . if missing(tcr)

*4 condición
gen C4B2 = 0
replace C4B2 = 1 if pnr_ixc >= LimPxI | pnr_mfb_ajustado >= LimSPxM | pnr_mfb_ajustado < LimLPxM
replace C4B2 = . if missing(pnr_ixc) | missing(pnr_mfb_ajustado)


***MODELO1SD***

*1 condición:
gen C1B1 = 0
replace C1B1 = 1 if var_cb <= tendcblp_1sdlow    //| cb <= tendcblp_1sdlow_12
replace C1B1 = . if missing(var_cb)

*2 condición:
gen C2B1 = 0
replace C2B1 = 1 if var_provind >= tendpilp_1sdup
replace C2B1 = . if missing(var_provind)

*3 condición:
gen C3B1 = 0
replace C3B1 = 1 if tcr >= tendcvlp_1sdup
replace C3B1 = . if missing(tcr)

*4 condición
gen C4B1 = 0
replace C4B1 = 1 if pnr_ixc >= LimPxI | pnr_mfb_ajustado >= LimSPxM | pnr_mfb_ajustado < LimLPxM
replace C4B1 = . if missing(pnr_ixc) | missing(pnr_mfb_ajustado)

*Gráficos variables presentación
bysort fecha_stata: egen sumcb = sum(cb)
bysort fecha_stata: egen sumcv = sum(cartera_vencida)
bysort fecha_stata: egen sumprvi = sum(prov_deflac_t_3)
gen cbpnd = cb/sumcb
gen prvipnd =  prov_deflac_t_3/sumprvi
gen cvpnd = cartera_vencida/sumcv

gen var_cbp = var_cb*cbpnd
gen var_provindp = var_provind*prvipnd
gen tcr_msp = tcr_ms * cvpnd

bysort id_EC: egen sdcb = sd(var_cbp)
bysort id_EC: egen sdprv = sd(var_provindp)
bysort id_EC: egen sdtcr = sd(tcr_msp)

twoway (scatter var_cbp fecha_stata if var_cbp <= 2*sdcb & var_cbp >= -2*sdcb & nombre_entidad != "TOTAL SISTEMA", msize(small)) (tsline var_cb if nombre_entidad == "TOTAL SISTEMA"), ytitle(C. Bruta) name(V4, replace) 
graph save ${source}OUTPUT\Variable_var_cb, asis replace
twoway (scatter var_provindp fecha_stata if var_provindp <= 2*sdprv & var_provindp >= -2*sdprv & nombre_entidad != "TOTAL SISTEMA", msize(small)) (tsline var_provind if nombre_entidad == "TOTAL SISTEMA"), ytitle(C. Provisiones BCDE) name(V5, replace)
graph save ${source}OUTPUT\Variable_var_provind, asis replace
bysort fecha_stata: egen tcr_av = mean(tcr_ms)
twoway (scatter tcr_msp fecha_stata if tcr_msp <= 2*sdtcr & tcr_msp >= -2*sdtcr & nombre_entidad != "TOTAL SISTEMA", msize(small)) (tsline tcr_ms if nombre_entidad == "TOTAL SISTEMA"), ytitle(C. Vencida) name(V6, replace)
graph save ${source}OUTPUT\Variable_tcr_ms, asis replace


drop if nombre_entidad == "TOTAL SISTEMA" 
**SE DESCARTA TOTAL SISTEMA PARA NO SESGAR CÁLCULOS (ES EL PROMEDIO DE TODO)**


*Instalación algunas herramientas postestimación:
*ssc install fitstat
*ssc install outreg2
*ssc install estout
*ssc install boxtid

*################MODELO FINAl############*

*MODELO A: 2 SD
logistic y_a L1y_a C1B2 C2B2 C3B2  dtf_r icns ici icns12 ici12 var_cb var_provind tcr_ms dsb_cb, r
logit y_a L1y_a C1B2 C2B2 C3B2  dtf_r icns ici icns12 ici12 var_cb var_provind tcr_ms dsb_cb, r
estat ic
predict XbD
estat gof, group(10) table
fitstat, sav(1M)
lsens, genprob(cut1D) gensens(sen1D) genspec(spec1D) xline(0.06360536)
graph save ${source}OUTPUT\lsensM1D, asis replace 
lroc
graph save ${source}OUTPUT\ROCM1D, asis replace

**ACCURACY RATIO**
sca AUC = r(area)
sca AR = (2*AUC)-1
di AR
gen DsenSpec = abs(sen1D - spec1D)
egen MinSS = min(DsenSpec)
gen CutXs = cut1 if DsenSpec==MinSS
mkmat CutXs, matrix(cut) nomis
sca cut_prb = cut[1, 1]
estat classification
estat classification, cut(0.06360536) // Toca traerla manualmente desde di cut_prb


*ModeloB: 1SD
logistic y_a L1y_a C1B2 C2B2 C3B2  dtf_r icns ici icns12 ici12 var_cb var_provind tcr_ms dsb_cb, r
logit y_a L1y_a C1B1 C2B1 C3B1  dtf_r ici icns12 ici12 var_cb var_provind tcr_ms dsb_cb, r
estat ic
predict XbD2
estat gof, group(10) table
fitstat, sav(2M) using(1M)
lsens, genprob(cut2D) gensens(sen2D) genspec(spec2D) xline(0.06305191)
graph save ${source}OUTPUT\lsensM2D, asis replace 
lroc
graph save ${source}OUTPUT\ROCM2D, asis replace

**ACCURACY RATIO**
sca AUC2 = r(area)
sca AR2 = (2*AUC)-1
di AR2
gen DsenSpec2 = abs(sen2D - spec2D)
egen MinSS2 = min(DsenSpec2)
gen CutXs2 = cut2 if DsenSpec2==MinSS2
mkmat CutXs2, matrix(cut2) nomis
sca cut_prb2 = cut2[1, 1]
estat classification
estat classification, cut(0.06305191) // Toca traerla manualmente desde di cut_prb


***ModeloC: Elimina no significativas***
logistic y_a L1y_a C2B1 C3B1 dtf_r ici icns12 ici12 var_cb var_provind tcr_ms, r
logit y_a L1y_a C2B1 C3B1 dtf_r ici icns12 ici12 var_cb var_provind tcr_ms, r
estat ic
predict XbD3
estat gof, group(10)
estat gof, group(20) table
fitstat, sav(3M) using(2M)

lsens, genprob(cut3D) gensens(sen3D) genspec(spec3D) xline(0.0631087)
graph save ${source}OUTPUT\lsensM3, asis replace 
lroc
graph save ${source}OUTPUT\ROCM3D, asis replace

**ACCURACY RATIO**
sca AUC3 = r(area)
sca AR3 = (2*AUC)-1
di AR3
gen DsenSpec3 = abs(sen3D - spec3D)
egen MinSS3 = min(DsenSpec3)
gen CutXs3 = cut3 if DsenSpec3==MinSS3
mkmat CutXs3, matrix(cut3) nomis
sca cut_prb3 = cut3[1, 1]
estat classification
estat classification, cut(0.0631087) // Toca traerla manualmente desde di cut_prb

/*
*Histogramas

*kdensity r
twoway histogram XbD3 || kdensity XbD, name(grht1, replace)
twoway histogram y_a || kdensity y_a, bi name(grht2, replace)
graph combine grht2 grht1
graph save ${source}OUTPUT\histogramas2, asis replace

***DIAGNOSTICOS:

*1- Specification error:

linktest

*Matriz de confusión M1A:

estat classification, cut(0.0631087)

*xtline XbD3 y_a if nombre_entidad != "TOTAL SISTEMA"
*graph save ${source}OUTPUT\Resd_Finales, asis replace
 
 

* 3- Multicolinearidad:

*se instalan paquetes:
*findit collin
*net install collin.pkg

corr L1y_a C2B1 C3B1 dtf_r ici icns12 ici12 var_cb var_provind tcr_ms
*Matriz de correlación: Correlaciones son bajas

* Factores de inflación de la varianza: Son bajos VIF < 10 no hay multicol

collin L1y_a C2B1 C3B1 dtf_r ici icns12 ici12 var_cb var_provind tcr_ms

* Tolerancia: 1-R^2 R^2 son bajos



* 3- Influencia
gen idn = _n
gen idN = _N
*scatter stdres XbD1, mlabel(idn) ylab(-4(2) 16) yline(0)
*scatter stdres idn, mlab(idn) ylab(-4(2) 16) yline(0)

predict db1, dbeta
scatter db1 XbD3, mlab(idn) yline(0)

predict hat1, hat
scatter hat1 XbD3, mlab(idn) yline(0)

 
*2- Bondad de ajuste
estat gof
estat gof, group(10)
estat gof, group(20)
estat gof, group(20) table



* 4- Influencia:


predict hat2, hat
egen hatmean = mean(hat2)
scatter hat2 idn, mlab(idn)

*leverage (hat value)	>2 or 3 times of the average of leverage

***Parece no tener problemas de influencia (datos extremos)

sort fecha_stata
sort XbD3

*/

export excel fecha fecha_stata id_EC y_a XbD3  using ${source}OUTPUT\OUTPUT_FINAL.xlsx , sheet("Resultados_stata_M1") sheetreplace cell(B2) firstrow(varlabels)

/*
xtline y_a XbD2 if nombre_entidad == "BANCO FALABELLA" | nombre_entidad == "CREDIFAMILIA S.A." | nombre_entidad == "CREDIFINANCIERA S.A" | nombre_entidad == "GIROS & FINANZAS COMPAÑIA DE FINANCIAMIENTO S.A." ///
 | nombre_entidad == "LA HIPOTECARIA COMPAÑÍA DE FINANCIAMIENTO S.A." | nombre_entidad == "FINANCIERA JURISCOOP S.A. COMPAÑIA DE FINANCIAMIENTO" ///
 | nombre_entidad == "BANCO MULTIBANK S.A." | nombre_entidad == "BANCO PICHINCHA S.A." | nombre_entidad == "DANN REGIONAL COMPAÑÍA DE FINANCIAMIENTO S.A." ///
 | nombre_entidad == "BANCO FINANDINA S.A." | nombre_entidad == "OPPORTUNITY INTERNATIONAL COLOMBIA S.A. COMPAÑIA DE FINANCIAMIENTO" ///
 | nombre_entidad == "BANCOLOMBIA S.A." | nombre_entidad == "COLTEFINANCIERA S.A." | nombre_entidad == "BANCO COOPCENTRAL" | nombre_entidad == "BANCO MUNDO MUJER S.A.", yline(0.05441891)
graph save ${source}OUTPUT\Malas_agosto, asis replace
*/

tsline y_a XbD3, yline(0.0631087)
graph save ${source}OUTPUT\probHisto, replace

tsline XbD3, yline(0.0631087)
graph save ${source}OUTPUT\probHistO_xb, replace



**********************************************************************************************************************
****MODELO CON VARIABLES ADICIONALES***
gen act_brt = activo + provision
gen CBA = cartera_bruta/act_brt
gen USA = utilidad/act_brt
gen CICB = cartera_improductiva/cartera_bruta
gen GPU = (gasto_provisiones-recup_provisiones)/utilidad
gen ICR = cartera_riesgo/cartera_bruta
gen Pc = provision/cartera_bruta
gen Rcde = (cartera_riesgo_c + cartera_riesgo_d + cartera_riesgo_e)/cartera_bruta
gen Rde = (cartera_riesgo_d + cartera_riesgo_e)/cartera_bruta

*Part cartera bruta sistema
bysort fecha_stata: egen cb_totS=sum(cartera_bruta)
gen pcb_stm = cartera_bruta/cb_totS

*Participación en activo:
bysort fecha_stata: egen  act_tot= sum(act_brt)
gen part_act = act_brt / act_tot
gen PACB = part_ac*var_cb

*gen Pdcb=part_act*var_cb

gen icns12_tf = icns12^-1.22
gen icco_tf = icco^2.68
gen CBA2 = CBA^2
drop if CBA2 > 1
gen ICR2 = ICR^2
gen Rde2 = Rde^2
gen ici2 = ici^2
gen var_cb2 = var_cb^2
gen dtf2 = dtf_r^2

*Original:
*logit y_a L1y_a C2B1 C3B1 dtf_r ici CBA2 ICR Rde2 var_cb var_provind tcr_ms dsb1_ven, r

*Nueva var

logistic y_a L1y_a C2B1 C3B1 dtf_r ici CBA ICR Rde2 var_cb var_provind tcr_ms, r
logit y_a L1y_a C2B1 C3B1 dtf_r ici CBA ICR Rde2 var_cb var_provind tcr_ms, r
estat ic
predict Prob_F
estat gof, group(10)
estat gof, group(20) table
fitstat, sav(4M) using(3M) force


*GRAFICOS PUNTO DE CORTE Y ROC
lsens, genprob(cut_F) gensens(sen_F) genspec(spec_F) xline(0.0638193)
graph save ${source}OUTPUT\lsensM_F, asis replace 
lroc
graph save ${source}OUTPUT\ROCM_F, asis replace

**ACCURACY RATIO**
sca AUC_F = r(area)
sca AR_F = (2*AUC_F)-1
di AR_F

*PUNTO DE CORTE
gen DsenSpec_F = abs(sen_F - spec_F)
egen MinS_F = min(DsenSpec_F)
gen Cut_F = cut_F if DsenSpec_F == MinS_F
mkmat Cut_F, matrix(cut_F) nomis
sca cut_prb_F = cut_F[1, 1]





*Matriz de confusión
estat classification
estat classification, cut(0.0638193) // Toca traerla manualmente desde di cut_prb

*Histogramas
*kdensity r
twoway histogram Prob_F || kdensity Prob_F, name(grht1, replace)
twoway histogram y_a || kdensity y_a, bi name(grht2, replace)
graph combine grht2 grht1
graph save ${source}OUTPUT\histogramas2, asis replace


*1- Specification error:

linktest
*boxtid logit y_a var_cb var_provind tcr_ms


* 3- Multicolinearidad:

*se instalan paquetes:
*findit collin
*net install collin.pkg

corr L1y_a C2B1 C3B1 dtf_r ici CBA ICR Rde2 var_cb var_provind tcr_ms
*Matriz de correlación: Correlaciones son bajas

* Factores de inflación de la varianza: Son bajos VIF < 10 no hay multicol

collin L1y_a C2B1 C3B1 dtf_r ici CBA ICR Rde2 var_cb var_provind tcr_ms

* Tolerancia: 1-R^2 R^2 son bajos

* 3- Influencia
gen idn = _n
gen idN = _N

predict db1, dbeta
scatter db1 Prob_F, mlab(idn) yline(0)

predict hat1, hat
scatter hat1 Prob_F, mlab(idn) yline(0)

 
*2- Bondad de ajuste
estat gof
estat gof, group(10) table



* 4- Influencia:


predict hat2, hat
egen hatmean = mean(hat2)
scatter hat2 idn, mlab(idn)

*leverage (hat value)	>2 or 3 times of the average of leverage

***Parece no tener problemas de influencia (datos extremos)

export excel fecha fecha_stata id_EC y_a Prob_F  using ${source}OUTPUT\OUTPUT_FINAL.xlsx , sheet("Resultados_stata_MFinal") sheetreplace cell(B2) firstrow(varlabels)




*###Gráficos variables balance
bysort fecha_stata: egen CBA_AV = mean(CBA)
bysort fecha_stata: egen ICR_AV = mean(ICR)
bysort fecha_stata: egen Rde_AV = mean(Rde2)


twoway (scatter CBA fecha_stata, msize(small))(tsline CBA_AV), ytitle((CBruta/Activos)) name(VF1, replace)
graph save ${source}OUTPUT\V1, asis replace
twoway (scatter ICR fecha_stata, msize(small))(tsline ICR_AV), ytitle(ICR) name(VF2, replace)
graph save ${source}OUTPUT\V2, asis replace
twoway (scatter Rde2 fecha_stata, msize(small))(tsline Rde_AV), ytitle((ICR_de^2)) name(VF3, replace)
graph save ${source}OUTPUT\V3, asis replace
graph combine VF1 VF2 VF3 V4 V5 V6
graph save ${source}OUTPUT\CombinaVariablesFin, asis replace


*PRESENTACIÓN MENSUAL ¿CÓMO IRÍA? ¿ARAÑAS?
/*
logit y_a L1y_a C2B1 C3B1 dtf_r ici CBA ICR Rde2 var_cb var_provind tcr_ms, r
fitstat, sav(5M) using(4M) force
estat gof, group(10)
*/


*#####INDICE RIESGO SISTEMA#######


*Probabilidad ponderada por participación en activo:
gen P_pond = pcb_stm * Prob_F

*Índice sistema:
bysort fecha_stata: egen  Indice_sist = sum(P_pond)
bysort fecha_stata: egen  Indice_sist_mean = mean(P_pond)
tsline Indice_sist, mlabel(Indice_sist)
graph save ${source}OUTPUT\Indice_Sistema, asis replace

twoway (scatter P_pond fecha_stata, msize(small))(tsline Indice_sist), ytitle(Prob. pond. por activos e indicador) name(Ind1, replace)
graph save ${source}OUTPUT\Graf_Indice_ppond, asis replace
twoway (scatter Prob_F fecha_stata, msize(small))(tsline Indice_sist), ytitle(Probabilidad e indicador) name(Ind2, replace)
graph save ${source}OUTPUT\Graf_Indice_prb, asis replace
graph combine Ind1 Ind2, rows(2)
graph save ${source}OUTPUT\2Graf_Indice_Sistema, asis replace

*################MODELO PANEL ####################################################
/*
xtlogit y_a L1y_a C2B1 C3B1 dtf_r ici CBA ICR Rde2 var_cb, i(id_EC) fe
estimates store fixed
xtlogit y_a L1y_a C2B1 C3B1 dtf_r ici CBA ICR Rde2 var_cb, i(id_EC) re
estimates store rand
hausman fixed
*/


xtlogit y_a L1y_a C2B1 C3B1 dtf_r ici CBA ICR Rde2 var_cb, i(id_EC) re
predict xbA1, xb
gen ProbXT = 1/(1 + exp(-xbA))
roctab y_a ProbXT
roctab y_a ProbXT, graph
*roctab y_a ProbXT, detail
local AUC_A `r(area)'
gen PXT_pond = ProbXT * pcb_stm




**ACCURACY RATIO**
sca AUC_A = r(area)
sca AR_XT = (2*AUC_A)-1
di AR_XT





bysort fecha_stata: egen  Indice_sist_XT = sum(PXT_pond)
*fitstat, sav(5M_XT) using(4M) force
*estat gof, group(10)
twoway (scatter PXT_pond fecha_stata, msize(small))(tsline Indice_sist_XT), ytitle(Prob. pond. %C.bruta e indicador) name(Ind3, replace)
graph save ${source}OUTPUT\Ind3_XT, asis replace



export excel fecha fecha_stata id_EC y_a XbD3 Prob_F ProbXT P_pond Indice_sist Indice_sist_XT using ${source}OUTPUT\OUTPUT_FINAL.xlsx , sheet("Resultados_stata_Total") sheetreplace cell(B2) firstrow(varlabels)



log close
exit
