***MODELO DE PROYECCIÓN DE CARTERA - DRC - SFC***
*****DELEGATURA DE RIESGO DE CRÉDITO****
*|Carlos Díaz|cediaz@superfinanciera.gov.co|


capture log close
set more off
version 11
clear all
set linesize 82
set scheme s2gcolor  /* Graphics scheme */
set matsize 5000
*set mem 400m

*Rutas:*

********************Directorio de Trabajo*************************************
global fuente "D:\CarlosEDV\Econometrics\Proyecciones_cartera\INPUTS\"
global resultado "D:\CarlosEDV\Econometrics\Proyecciones_cartera\OUTPUTS\"
******************************************************************************

log using ${fuente}logcartera.log, replace
use ${fuente}BASE_CART.dta
save ${resultado}BASE_CARTout.dta,replace

cls

sort fecha_stata


tsset fecha_stata, monthly 	//Se configura base de serie de tiempo

*Grafico de las series a proyectar en niveles:
tsline brutatot ventotal
graph save ${resultado}BV_niveles, asis replace

*Descripción de las variables (niveles):
sum brutatot ventotal

**PROYECCIONES UNIVARIADAS: RESULTADOS USANDO FILTRO (dexponential):

/*Holt-Winters smoothers: Holt (1957) y Winters(1960)propusieron filtros
adaptativos para modelar los componentes de tendencia/ciclo/estacionalidad de 
una series de tiempo mediante el uso de EWMAs (Exponetially weighted moving average)
Dado que las series no están desestacionalizadas, se usa el comando que
supone un componente estacional: Se usó d exponential pues HW no encontró solución*/

tssmooth dexponential xpcb = brutatot_r, forecast(60) 

tssmooth dexponential xpcv = ventotal_r, forecast(60)

gen cb_va = brutatot_r/l12.brutatot_r - 1
gen cv_va = ventotal_r/L12.ventotal_r -1
gen xpcb_va = xpcb/L12.xpcb - 1
gen xpcv_va =xpcv/L12.xpcv -1

tsline brutatot_r ventotal_r xpcb xpcv
graph save ${resultado}forecast1_xp, asis replace

tsline cb_va cv_va xpcb_va xpcv_va
graph save ${resultado}forecast1_xpva, asis replace

****backtest de la proyección filtro exponencial - El backtest se hace inicialmente en niveles:

tssmooth dexponential xpcb_bcktest = brutatot_r if fecha_stata <= tm(2014m11), forecast(24) 

tssmooth dexponential xpcv_bcktest = ventotal_r if fecha_stata <= tm(2014m11), forecast(24)

label variable xpcb_bcktest "Back test Filtro CB"
label variable xpcv_bcktest "Back test Filtro CV"
local xl = tm(2014m11)

tsline brutatot_r xpcb_bcktest, xline(`xl') name(BTcbfiltro, replace)
tsline ventotal_r xpcv_bcktest, xline(`xl') name(BTcvfiltro, replace)
graph combine BTcbfiltro BTcvfiltro


***DESESTACIONALIZACIÓN DE LAS SERIES***
*Stata no tiene un comando automático de desestacionalización por lo que a 
*continuación se hace "a mano":

*ESTACIONALIDAD: 
**El modelo supone que la serie tiene:
*Tendencia + Estacionalidad + Ciclo; El modelo de reg implícito es:

***Yt = sum(a_i*D_it + &*t + B_1*Y_t-1 + ... +B_p*Y_t-p + e_t

**El anterior es un modelo estimable por mínimos cuadrados ordinarios (MCO) y 
** también es un modelo de proyección:

**Generación de dummies mensuales:

gen m = month(dofm(fecha_stata)) //número de mes

*Dummies mensuales:
gen m1 = (m==1) 
gen m2 = (m==2) 
gen m3 = (m==3) 
gen m4 = (m==4) 
gen m5 = (m==5) 
gen m6 = (m==6) 
gen m7 = (m==7) 
gen m8 = (m==8) 
gen m9 = (m==9) 
gen m10 = (m==10) 
gen m11 = (m==11) 
gen m12 = (m==12) 

**Desestacionalización cartera bruta:

reg brutatot_r b12.m L(1/12)brutatot_r
predict brt_sa, xb
tsline brutatot_r brt_sa
graph save ${resultado}bruta_SA, replace

**Desestacionalización cartera vencida:

reg ventotal_r b12.m L(1/12)ventotal_r
predict ven_sa, xb
tsline ventotal_r ven_sa
graph save ${resultado}venc_SA, replace

***NOTA: SIN EMBARGO, LAS VARIABLES DESESTACIONALIZADAS CON X13 EVIEWS SON:
** brutatot_r_d11 
** ventotal_r_d11
** Las cuales se usaran de aquí en adelante.


*ARIMAS (Box-Jenkins meth):

** I - ESTACIONARIEDAD - Cartera bruta y vencida total y ajustadas por estacionalidad:

*Cartera bruta: Series en diferencias logarítmicas mensuales:
gen lbrutaT = log(brutatot)
gen dlBrutaT = D.lbrutaT
tsline dlBrutaT
graph save ${resultado}B_I1, asis replace

*Prueba de raíz unitaria (dfuller, dfgls, pperron)
dfuller dlBrutaT, reg lags(12) drift

*The null hypothesis is that the variable contains a unit root. El p-valor 
*es 0.0068 por lo que se rechaza la hipótesis nula

*Cartera bruta ajustada estacionalmente: Series en diferencias logarítmicas mensuales:
gen lbrtSA = log(brutatot_r_d11)
gen dlbrtSA = D.lbrtSA
tsline dlbrtSA
graph save ${resultado}BSA_I1, asis replace

*Prueba de raíz unitaria (dfuller, dfgls, pperron)
dfuller dlbrtSA, reg lags(12) drift

*The null hypothesis is that the variable contains a unit root. El p-valor 
*es 0.018 por lo que se rechaza la hipótesis nula


****II-Estimación****
***Se sacan las gráficas de AC y PAC y se estiman los ARIMAS (solo para variables
*ajustadas estacionalmente como se acordó en reuniones.

*Cartera vencida: Series en diferencias logarítmicas mensuales:
gen lvencT = log(ventotal)
gen dlVencT = D.lvencT
tsline dlVencT
graph save ${resultado}Venc_I1, asis replace

*Prueba de raíz unitaria (dfuller, dfgls, pperron)
dfuller dlVencT, reg lags(12) drift

*The null hypothesis is that the variable contains a unit root. El p-valor 
*es 0.0030 por lo que se rechaza la hipótesis nula

*Cartera vencida ajustada estacionalmente: Series en diferencias logarítmicas mensuales:
gen lvenSA = log(ventotal_r_d11)
gen dlVencSA = D.lvenSA
tsline dlVencSA
graph save ${resultado}VencSA_I1, asis replace

*Prueba de raíz unitaria (dfuller, dfgls, pperron)
dfuller dlVencSA, reg lags(12) drift

*The null hypothesis is that the variable contains a unit root. El p-valor 
*es 0.0003 por lo que se rechaza la hipótesis nula.


***Gráficos de autocorrelación parcial y total:

****Cartera bruta:
ac dlBrutaT, name(ac_bruta, replace) lags(36) note("")
pac dlBrutaT, name(pac_bruta, replace) lags(36) note("")
graph combine ac_bruta pac_bruta, note("banda de confianza del 95%")
graph save ${resultado}acpac_bruta, asis replace
corrgram dlBrutaT

********Arimas sospechosos: ar 3-5 ... ma 3?


****Cartera bruta ajustada estacionalmente:
ac dlbrtSA, name(ac_brutaSA, replace) lags(36) note("")
pac dlbrtSA, name(pac_brutaSA, replace) lags(36) note("")
graph combine ac_brutaSA pac_brutaSA, note("banda de confianza del 95%")
graph save ${resultado}acpac_brutaSA, asis replace

corrgram dlbrtSA

********Arimas sospechosos: ar 3-5 ma (0-3?)

arima dlbrtSA, arima(3, 0, 2) nolog
estat ic

*ARIMA seleccionado:
arima dlbrtSA, arima(2, 0, 2) nolog
estat ic
predict resbrt, res
predict arima_outsmpl_cb, dynamic(tm(2016m11))
predict fvar_cb, mse
gen upper_cb = arima_outsmpl_cb + 1.96*sqrt(fvar_cb)
gen lower_cb = arima_outsmpl_cb - 1.96*sqrt(fvar_cb)


**Por criterios de información se escoge una forma (2,0,2). Recordemos que ya
**está diferenciado.


****Cartera vencida ajustada estacionalmente:
ac dlVencSA, name(ac_vencSA) lags(36) note("")
pac dlVencSA, name(pac_vencSA) lags(36) note("")
graph combine ac_vencSA pac_vencSA, note("banda de confianza del 95%")
graph save ${resultado}acpac_venc, asis replace

corrgram dlVencSA

arima dlVencSA, arima(3, 0, 4) nolog
estat ic
predict resvenc, res
predict arima_outsmpl_cv, dynamic(tm(2016m11))
predict fvar_cv, mse
gen upper_cv = arima_outsmpl_cb + 1.96*sqrt(fvar_cv)
gen lower_cv = arima_outsmpl_cb - 1.96*sqrt(fvar_cv)

*ARIMA seleccionado:
arima dlVencSA, arima(3, 0, 3) nolog
estat ic

***Diagnósticos de los modelos:

****1-Q-Statistic:

*a-Cartera bruta
wntestq resbrt
**El test no arroja evidencia de que los errores se desvían del ruido blanco

*b-Cartera vencida:
wntestq resvenc
**El test no arroja evidencia de que los errores se desvían del ruido blanco

**** 2- Cum Periodogram WN test: El test Q puede no detectar periodicidad no-aleatoria
*en los errores (p.e. series no desestacionalizadas), este test las encuentra:

*C.Bruta:
wntestb resbrt
*En el caso de la bruta, el periodograma se mantiene cerca de la línea de 45 grados
* y dentro de las bandas de confianza, por lo que los residuos no muestran evidencias de 
* periodicidad no aleatoria.

*C.Vencida:
wntestb resvenc

*En el caso de la vencida, el periodograma se mantiene cerca de la línea de 45 grados
* y dentro de las bandas de confianza, por lo que los residuos no muestran evidencias de 
* periodicidad no aleatoria.

***Proyecciones bruta y vencida con ARIMAS seleccionados:

*Proyección cartera bruta (mensual):
tsline dlbrtSA arima_outsmpl_cb upper_cb lower_cb
graph save ${resultado}Pry_ARIMA_bruta, asis replace

*Proyección de cartera vencida (mensual):
tsline dlVencSA arima_outsmpl_cv upper_cv lower_cv
graph save ${resultado}Pry_ARIMA_venc, asis replace

******PROYECCIÓN VAR. ANUAL BRUTA Y VENCIDA (EXCEL)****

************Insertar excel*******************


***************MODELO DE VECTORES AUTOREGRESIVOS (SERIES DE TIEMPO MÚLTIPLES)****

****VAR DE FORMA REDUCIDA - Modelo 1 (Todas las variables - interacción bruta y vencida)****
/*A continuación se hacen los modelos establecidos hasta el momento pero con las
variables ajustadas estacionalmente*/

gen lise = log(ise_sa)
gen dlise = D.lise
gen ddtfr = D.dtf_r

*Ya se sabe que bruta y vencida ajustadas estacionalmente son estacionarias, ahora vemos si
*el ISE y la dtf en diferencia son estacionarios y poder llevar a cabo el var en forma reducida.
*Nota: Una vez estimado el modelo se verificará la estacionariedad del vector

*Estacionaridad del dlise (ISE):
*Prueba de raíz unitaria (dfuller, dfgls, pperron)
dfuller dlise, reg lags(12) drift
*El p-value for Z(t) = 0.0015 permite rechazar la hipótesis nula de raíz unitaria.
*La serie es estacionaria

*Estacionaridad de ddtfr (DTF real):
*Prueba de raíz unitaria (dfuller, dfgls, pperron)
dfuller ddtfr, reg lags(12) drift
*El p-value for Z(t) = 0.0000 permite rechazar la hipótesis nula de raíz unitaria.
*La serie es estacionaria 



**** Definición de rezagos:

varsoc dlbrtSA dlVencSA dlise ddtfr, maxlag(18)
varsoc dlbrtSA dlVencSA dlise ddtfr, maxlag(24)

* Los criterios FPE y AIC señalan 4 rezagos óptimos; HQIC 3 rezagos y SBIC 1 rezago
*a diferencia del modelo inicial (sin desestacionalizar) que señalaba 12 rezagos.

*Según Lütkepohl (2005), los criterios SBIC y HQIC brindan estimaciones consistentes 
*del verdadero orden de los rezagos, mientras los criterios FPE y AIC sobreestiman
*los rezagos. En cualquier caso, se compararán los modelos con diferentes rezagos señalados.

***ESTIMACIÓN:

*1- VAR con rezagos óptimos señalados por FPE y AIC (4 rezagos) ***MODELO SELECCIONADO DESPUÉS DE TEST***:

varbasic dlbrtSA dlVencSA dlise ddtfr, lags(1 2 3 4) step(24) nograph
estat ic

*2- VAR con rezagos óptimos señalados por SBIC (1 rezagos):

varbasic dlbrtSA dlVencSA dlise ddtfr, lags(1) step(24) nograph
estat ic

*El modelo supera al anterior en SBIC (1 de 3 criterios), pero no en los demás 
*criterios ytiene una caída importante en R^2

*3- VAR con rezagos óptimos señalados por HQIC (3 rezagos): (MODELO SELECCIONADO inicialmente)

varbasic dlbrtSA dlVencSA dlise ddtfr, lags(1 2 3) step(24) nograph
estat ic

*El modelo supera al primero en HQIC y SBIC (2 de 3 criterios) y la caída en R^2
*no es notable. Adicionalmente, como se mencionó anteriormente, el criterio de 
*selección de rezagos HQIC no tiende a sobreestimar los rezagos como AIC Y FPE.

***DIAGNOSTICOS DEL VAR SELECCIONADO INICIALMENTE (3 REZAGOS):

* 1- ESTACIONARIDAD DEL VAR:

varstable, graph

*Los 12 eigenvalores caen dentro del circulo unitario, son menores a 1 y no hay 
*valores muy cercanos a 1 (Choques mueren relativamente rápido). El VAR es estable.

*2-Autocorrelación de residuos:
varlmar
*El modelo presenta autocorrelación de primer orden. 

*****SELECCIONAMOS MODELO CON 4 REZAGOS (FPE AIC):
varbasic dlbrtSA dlVencSA dlise ddtfr, lags(1 2 3 4) step(24) nograph
estat ic

***DIAGNOSTICOS DEL VAR SELECCIONADO FINALMENTE (4 REZAGOS):

* 1- ESTACIONARIDAD DEL VAR:

varstable, graph

*Los 16 eigenvalores caen dentro del circulo unitario, son menores a 1 y no hay 
*valores muy cercanos a 1 (Choques mueren relativamente rápido). El VAR es estable.

*2-Autocorrelación de residuos:
varlmar
*El modelo NO presenta autocorrelación. 

*3-Normalidad:
varnorm

*Las series que nos interesan no presentan perturbaciones normales, solo la DTF pasa el test.


*Proyección (Var mensuales):
fcast compute F_, step(40)
fcast graph F_dlbrtSA F_dlVencSA F_dlise F_ddtfr

********************************************************************************

****VEC (en proceso) - pry Largo plazo.
*** Anteriormente se mostró que las series son integtradas de orden 1 I(1)

*Rezagos
varsoc lbrtSA lvenSA lise dtf_r, maxlag(24)

**4 rezagos por FPE AIC

*Contegración
vecrank lbrtSA lvenSA lise dtf_r, max ic
**SBIC Y HQIC sugieren 3 relaciones de cointegración

*Vec inicial:
qui vec lbrtSA lvenSA lise dtf_r, rank(3) lags(4) trend(trend)
estimates store trendlags4
qui vec lbrtSA lvenSA lise dtf_r, rank(3) lags(4) trend(rtrend)
estimates store rtrendlags4

lrtest trendlags4 rtrendlags4

qui vec lbrtSA lvenSA lise dtf_r, rank(3) lags(4) trend(constant)
estimates store constantlags4

lrtest rtrendlags4 constantlags4

qui vec lbrtSA lvenSA lise dtf_r, rank(3) lags(4) trend(rconstant)
estimates store rconstantlags4

lrtest rtrendlags4 rconstantlags4

qui vec lbrtSA lvenSA lise dtf_r, rank(3) lags(4) trend(none)
estimates store nonelags4

lrtest rtrendlags4 nonelags4

vec lbrtSA lvenSA lise dtf_r, rank(3) lags(4) trend(rtrend)

**Proyecciones VEC
fcast compute Vc_, step(24)
fcast graph Vc_lbrtSA Vc_lvenSA Vc_lise Vc_dtf_r

export excel fecha fecha_stata arima_outsmpl_cb upper_cb lower_cb arima_outsmpl_cv upper_cv lower_cv F_dlbrtSA F_dlVencSA F_dlise F_ddtfr F_dlbrtSA_LB F_dlbrtSA_UB F_dlbrtSA_SE F_dlVencSA_LB F_dlVencSA_UB F_dlVencSA_SE F_dlise_LB F_dlise_UB F_dlise_SE ///
 Vc_lbrtSA Vc_lvenSA Vc_lise Vc_dtf_r Vc_lbrtSA_LB Vc_lbrtSA_UB Vc_lvenSA_UB Vc_lvenSA_LB using ${resultado}OUTPUT_FINAL.xlsx, sheet("Forec_stata_ARIMA_VAR_VEC") sheetreplace cell(B2) firstrow(varlabels)



log close
exit, clear


*5878797 op 5 op 0
