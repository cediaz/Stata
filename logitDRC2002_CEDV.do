cap log close
set more off
*version 13
clear all
set linesize 82
*set scheme sj

*Modelo de clasificación probabilidad de riesgo crédito*
*|Autor: Carlos Edison Díaz Villarreal|

******************************** Working Directory ***********************************
global source "E:\MODELO_SCORING_EC\"
global logoutput "E:\MODELO_SCORING_EC\"
**************************************************************************************

log using ${logoutput}logit2002.log, replace // Archivo de resultados
use ${source}INPUTS\Base2002.dta			//Base entidades desde enero 2002
save ${source}OUTPUT\base2002out.dta,replace	//Archivo de resultados

cls

*rename icc icns

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
*drop if nombre_entidad == "TOTAL SISTEMA"


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

*Reporte de duplicados:
duplicates report fecha_stata nombre_entidad

*Se eliminan duplicados (Base original parece unir entidades fusionadas y quedan repetidas)
duplicates drop fecha_stata nombre_entidad, force
duplicates report fecha_stata nombre_entidad

****SE DEFINE PANEL DE DATOS***  //El modelo no va a ser panel de datos inicialmente debido a limitaciones de tiempo, pero queda organizado para un desarrollo futuro.
xtset id_EC fecha_stata
xtdescribe



*Genera ICV:
gen ICV = cartera_vencida/cb

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

*Componentes total sistema de variables norma:

tsline pnr_acum_trimestral mfb_ajustado_acum_trimestral ixc_acum_trimestral if nombre_entidad == "TOTAL SISTEMA"
graph save ${source}OUTPUT\Estacionales, asis replace


tsline pnr_ixc pnr_mfb_ajustado  LimPxI LimSPxM if nombre_entidad == "TOTAL SISTEMA", saving(Cond2_3, replace)
tsline pnr_acum_trimestral mfb_ajustado_acum_trimestral ixc_acum_trimestral if nombre_entidad == "TOTAL SISTEMA", saving(Lev1, replace)

gr combine Cond2_3.gph Lev1.gph, iscale(*0.5)
graph save ${source}OUTPUT\cond_combinado1, asis replace


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
replace C3A = 1 if pnr_mfb_ajustado >= LimSPxM | pnr_mfb_ajustado < LimLPxM
replace C3A = . if missing(pnr_mfb_ajustado)

*4 condición:
gen C4A = 0
replace C4A = 1 if var_cb >= Limcb
replace C4A = . if missing(var_cb)

*SE GENERA LA VARIABLE DEPENDIENTE INICIAL:
gen y_a=0
replace y_a = 1 if C1A == 1 & C2A == 1 & C3A == 1 & C4A == 1
replace y_a = . if missing(C1A)|missing(C2A)|missing(C3A)|missing(C4A)

*xtline y_a
*graph save ${source}OUTPUT\y1, asis replace

*Instalación algunas herramientas:
*ssc install fitstat
*ssc install outreg2
*ssc install estout

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

gen fecha2=fecha_stata
format fecha2 %d
gen yr=year(fecha2)
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

*drop if nombre_entidad == "TOTAL SISTEMA"

***Primer Logit***
gen icns12 = L12.icns
gen ici12 = L12.ici
gen dtfr12 = L12.dtf_r

logit y_a var_provind pnr_ixc pnr_mfb_ajustado var_cb dtf_r icns12 ici12 tcr acelaracion_sem_promedio ICV if nombre_entidad != "TOTAL SISTEMA", r nolog
estat ic
estat gof, group(10) table
fitstat, sav(L1)


logit y_a var_provind pnr_mfb_ajustado var_cb dtf_r icns12 ici12 tcr acelaracion_sem_promedio ICV if nombre_entidad != "TOTAL SISTEMA", r nolog
estat ic
estat gof, group(10) table
fitstat, sav(L1A)
fitstat, using(L1) force


logit y_a var_provind pnr_mfb_ajustado var_cb dtf_r icns12 ici12 tcr acelaracion_sem_promedio `year' if nombre_entidad != "TOTAL SISTEMA", r nolog //Va ganando
estat ic
estat gof, group(10) table
fitstat, sav(L1B)
fitstat, using(L1A) force
lsens, genprob(cut1x) gensens(sen1x) genspec(spec1x)
sort cut1x
graph save ${source}OUTPUT\lsensM1x, asis replace
lroc
graph save ${source}OUTPUT\ROCM1x, asis replace
*Matriz de confusión M1A:
estat classification, cut(0.0403)
predict ProbM1x

logit y_a var_provind pnr_mfb_ajustado var_cb dtf_r icns12 ici12 tcr if nombre_entidad != "TOTAL SISTEMA", r nolog
estat ic
estat gof, group(10) table
fitstat, sav(L1C)
fitstat, using(L1B) force

logit y_a var_provind var_cb dtf_r icns12 ici12 if nombre_entidad != "TOTAL SISTEMA", r nolog   // Tiene más sentido los coef.
estat ic
predict Prob1
*gen Prob1 = 1/(1+ exp(lnOdss_M1xt))
estat gof, group(10) table
fitstat, sav(L1D)
fitstat, using(L1B) force
lsens, genprob(cut1) gensens(sen1) genspec(spec1)
graph save ${source}OUTPUT\lsensM1, asis replace
lroc
graph save ${source}OUTPUT\ROCM1, asis replace
*Matriz de confusión M1A:
*list sen1 spec1 cut1
estat classification, cut(0.044372)



***Modelo 1 XT***
xtlogit y_a var_provind pnr_mfb_ajustado var_cb dtf_r icns12 ici12 tcr acelaracion_sem_promedio if nombre_entidad != "TOTAL SISTEMA", nolog //Va ganando
predict xbA if e(sample), xb
roctab y_a xbA
local AUC_A `r(area)'
gen ProbXBA = 1/(1 + exp(-xbA))

xtlogit y_a var_provind var_cb dtf_r icns12 ici12 if nombre_entidad != "TOTAL SISTEMA", nolog
predict xbB if e(sample), xb
roctab y_a xbB
local AUC_B `r(area)'
gen ProbXBB = 1/(1 + exp(-xbB))



***Modelo nuevo y***



***MODELO2***

*1 condición:
gen C1B = 0
replace C1B = 1 if var_cb >= tendcblp_2sdup | cb <= tendcblp_2sdlow
replace C1B = . if missing(var_cb)

*2 condición:
gen C2B = 0
replace C2B = 1 if var_provind >= tendpilp_2sdup
replace C2B = . if missing(var_provind)

*3 condición:
gen C3B = 0
replace C3B = 1 if tcr >= tendcvlp_2sdup
replace C3B = . if missing(tcr)

*4 condición
gen C4B = 0
replace C4B = 1 if pnr_ixc >= LimPxI | pnr_mfb_ajustado >= LimSPxM | pnr_mfb_ajustado < LimLPxM
replace C4B = . if missing(pnr_ixc) | missing(pnr_mfb_ajustado)



*SE GENERA LA VARIABLE DEPENDIENTE MODIFICADA:
gen y_b=0
replace y_b = 1 if C2B == 1 & C2A == 1 & C3A == 1 & C1B == 1
replace y_a = . if missing(C2B)|missing(C2A)|missing(C3A)|missing(C1B)


xtline y_b
graph save ${source}OUTPUT\Ymodelo2, asis replace


*Modelo todas las var:
logit y_b var_provind pnr_ixc pnr_mfb_ajustado var_cb icns ici tcr acelaracion_sem_promedio if nombre_entidad != "TOTAL SISTEMA", r nolog
estat ic
predict Prob1YB
estat gof, group(10) table
fitstat, sav(L2)
*fitstat, using(L1B) force
lsens, genprob(cut2) gensens(sen2) genspec(spec2)
graph save ${source}OUTPUT\Logit2Yb, asis replace
lroc
graph save ${source}OUTPUT\ROCML2, asis replace
*Matriz de confusión M1A:
*list sen1 spec1 cut1
estat classification

logit y_b var_provin var_cb icns12 ici12 tcr if nombre_entidad != "TOTAL SISTEMA", r nolog
estat ic
predict Prob2YB
estat gof, group(10) table
fitstat, sav(L2B)
fitstat, using(L2) force
lsens, genprob(cut2B) gensens(sen2B) genspec(spec2B)
graph save ${source}OUTPUT\Logit2Yb, asis replace
lroc
graph save ${source}OUTPUT\ROCML2, asis replace
*Matriz de confusión M1A:
*list sen1 spec1 cut1
estat classification
*/

***PANEL DATA XTLOGIT MODEL:***

eststo mod1: xtlogit y_b var_provind pnr_ixc pnr_mfb_ajustado var_cb icns ici tcr acelaracion_sem_promedio if nombre_entidad != "TOTAL SISTEMA", nolog // Modelo principal
predict xb1 if e(sample), xb
roctab y_b xb1
local AUC `r(area)'
gen ProbXB1 = 1/(1 + exp(-xb1))


eststo mod2: xtlogit y_b var_cb icns ici if nombre_entidad != "TOTAL SISTEMA", nolog // Modelo principal
predict xb2 if e(sample), xb
roctab y_b xb2
local AUC2 `r(area)'
gen ProbXB2 = 1/(1 + exp(-xb2))

xtline ProbXB1


*################MODELO FINAl############*

* Y_a Usando Dummies:

logit y_a C1B C2B C3B dtf_r icns ici icns12 ici12 var_cb var_provind if nombre_entidad != "TOTAL SISTEMA", r nolog   // Tiene más sentido los coef.
estat ic
predict XbD
estat gof, group(10) table
fitstat, sav(FD)
fitstat, using(L1D) force


*Matriz de confusión M1A:

estat classification, cut(0.128654)

****REVISAR MULTICOL****
logit y_a C1B C2B C3B icns ici icns12 var_provind if nombre_entidad != "TOTAL SISTEMA", r nolog   // Tiene más sentido los coef.
estat ic
predict XbD1
estat gof, group(10) table
fitstat, sav(FD1)
fitstat, using(FD) force
lsens, genprob(cut1D) gensens(sen1D) genspec(spec1D)
graph save ${source}OUTPUT\lsensM1D, asis replace
lroc
graph save ${source}OUTPUT\ROCM1D, asis replace
estat classification, cut(0.072282)

xtline XbD1 y_a if nombre_entidad != "TOTAL SISTEMA"
graph save ${source}OUTPUT\Resd_Finales, asis replace
 
 
twoway scatter
 
*PRESENTACIÓN MENSUAL ¿CÓMO IRÍA? ARAÑAS
 
log close
exit
