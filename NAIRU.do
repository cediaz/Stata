***NAIRU COLOMBIA******************************************************
***CARLOS E. DÍAZ V.***************************************************
***ECONOMICANA.COM***

cap log close
set more off
version 11.2
clear all
set linesize 90
set scheme sj

*NAIRU*

******************************** Working Directory *******************************
global source "D:\CARLOS_DIAZ\MIS_DOC\Laboral\Nairu\"
global logoutput "D:\CARLOS_DIAZ\MIS_DOC\Laboral\Nairu\"
**********************************************************************************

log using ${logoutput}NAIRU.log, replace
use ${source}NAIRUDATA.dta
save ${source}NAIRUOUTPUT.dta,replace

tsset fecha, monthly

/* Primero calculamos la NAIRU invariable en el tiempo, partiendo de Dp=aU^*-aU+v
Asumimos que U^* es constante y que U no está correlacionada con v, entonces el valor 
de U^* puede ser estimado regresando el cambio en la inflación (Dp) contra la tasa de desempleo (U) 
y una constante. */

gen inf = ipc / L12.ipc -1
gen Dinf = D.inf

*1- con toda la ventana de datos
	reg Dinf tdn_sa, r
		gen NAIRU_TconALL = abs(_b[_cons]/_b[tdn_sa])
			di NAIRU_TconALL
			
			tsline tdn tdn_sa NAIRU_TconALL, name(NAIRU_TconALL, replace)
/* Ya conocemos "a", se relaja el supuesto de inflexibilidad en el tiempo y se halla la NAIRU 
variable en el tiempo, sabiento que U_t+(Dp/a)=U^*+(v_t/a)*/

*1- Se construye el lado izquierto de la ecualión:

gen IzqALL = tdn_sa + (Dinf/abs(_b[tdn_sa]))

hprescott IzqALL, stub(HP)

gen NAIRU_movALL = HP_IzqALL_sm

/*La nairu usando toda la ventana no da un resultado confiable,
debido a que la serie de empleo presenta un quiebre metodológico (aunque el DANE dice que 
es comparable y no hay problema), procedemos a usar la NAIRU desde el segundo semestre de 2007 para
tomar la serie nueva: */
			
*2- Teniendo en cuenta cambio metodológico a GEIH
	reg Dinf tdn_sa if fecha >= tm(2007m6), r
		gen NAIRU_Tcon = abs(_b[_cons]/_b[tdn_sa])
			di NAIRU_Tcon
			tsline tdn tdn_sa NAIRU_Tcon, name(NAIRU_Tcon, replace)

/* Ya conocemos "a", se relaja el supuesto de inflexibilidad en el tiempo y se halla la NAIRU 
variable en el tiempo, sabiento que U_t+(Dp/a)=U^*+(v_t/a)*/

*1- Se construye el lado izquierto de la ecualión:

gen Izq = tdn_sa + (Dinf/abs(_b[tdn_sa]))

hprescott Izq, stub(HP)

gen NAIRU_mov = HP_Izq_sm

tsline Izq NAIRU_mov, name(ladoizhp, replace)

tsline tdn tdn_sa NAIRU_movALL NAIRU_mov, name(NAIRU_final, replace)
tsline tdn NAIRU_mov, name(Graf_final, replace)

log close		
exit
